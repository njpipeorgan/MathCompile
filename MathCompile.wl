(* ::Package:: *)

Needs["CCompilerDriver`"];


BeginPackage["MathCompile`"];


CompileToCode::usage="\!\(\*RowBox[{\"CompileToCode\", \"[\", StyleBox[\"func\", \"TI\"], \"]\"}]\) rewrites a Wolfram Language function in C++.";
CompileToBinary::usage="\!\(\*RowBox[{\"CompileToBinary\", \"[\", StyleBox[\"func\", \"TI\"], \"]\"}]\) generates a function compiled as C++.";


CompileToCode[Function[func___]]:=If[#===$Failed,$Failed,#["output"]]&@compile[Hold[Function[func]]]
CompileToBinary[Function[func___],opts:OptionsPattern[]]:=compilelink[compile[Hold[Function[func]]],opts]


$CppSource="";
$CompilerOutput="";


Begin["`Private`"];


$packagepath=DirectoryName[$InputFileName];
$slotmaximum=16;
$rankmaximum=16;


parse::unknown="`1` cannot be parsed.";
syntax::iter="`1` does not have a correct syntax for an iterator.";
syntax::bad="`1` does not have a correct syntax for `2`.";
syntax::badtype="`1` does not specify a correct type.";
syntax::farg="`1` does not have a correct syntax for a function argument.";
syntax::slotmax="#`1` exceeds the maximum value of slot number allowed.";
syntax::fpure="`1` does not have a correct syntax for a pure function.";
syntax::scopevar="`1` does not have a correct syntax for a local variable.";
syntax::badbreak="Break[] cannot be called in `1`.";
syntax::breakloc="Break[] in `1` is not correctly enclosed by a loop.";
semantics::bad="`1` does not have correct semantics.";
semantics::undef="Identifier `1` is not found.";
semantics::noinit="Variable `1` is declared but not initialized.";
semantics::badinit="Variable `1` is initialized in a nested scope.";
semantics::badref="Variable `1` is referenced before initialization.";
codegen::bad="Cannot generate code for `1`.";
codegen::notype="One or more arguments of the main function is declared without types.";
link::rettype="Failed to retrieve the return type of the compiled function.";
link::bigrank="The rank of an argument exceeds the maximum rank.";
link::badtype="Cannot infer the type id \"`1`\"returned by the library.";
link::workdir="The working directory does not exist.";
link::libdir="The library directory does not exist.";
link::genfail="Failed to generate the library.";
link::noheader="The header file \"math_compile.h\" cannot be found.";


compile[code_]:=
  Module[{precodegen,output,types,error},
    $variabletable=Association[];
    error=Catch[
        precodegen=semantics@allsyntax@parse[code];
        types=getargtypes[precodegen];
        If[MemberQ[types,nil],Message[codegen::notype];Throw["codegen"]];
        output=maincodegen@precodegen;
      ];
    Clear[$variabletable];
    If[error===Null,<|"output"->output,"types"->types|>,$Failed]
  ]


newid:=SymbolName@Unique["id"]
newvar:=SymbolName@Unique["v"]
newvarpack:=SymbolName@Unique["vp"]


parse[Hold[head_[args___]]]:=parse[Hold[head]]@@(parse/@Hold/@Hold[args])

parse[Hold[s_Symbol]]:=id[SymbolName[Unevaluated@s]];
parse[Hold[I]]:=id["I"];

parse[Hold[i_Integer]]:=literal[i];
parse[Hold[i_Real]]:=literal[i];
parse[Hold[i_String]]:=literal[i];

parse[Hold[any_]]:=(Message[parse::unknown,ToString[Unevaluated[any]]];Throw["lexical"])


$typenames={
  (*informal names*)
  {"int64_t","MachineInteger"},
  (*formal names*)
  {"void", "Void"},
  {"wl::boolean", "Boolean"},
  {"wl::string", "String"},
  {"int64_t", "Integer"},    {"uint64_t", "UnsignedInteger"},
  {"int8_t",  "Integer8"},   {"uint8_t",  "UnsignedInteger8"},
  {"int16_t", "Integer16"},  {"uint16_t", "UnsignedInteger16"},
  {"int32_t", "Integer32"},  {"uint32_t", "UnsignedInteger32"},
  {"int64_t", "Integer64"},  {"uint64_t", "UnsignedInteger64"},
  {"double",  "Real"},       {"wl::complex<double>", "Complex"},
  {"float",   "Real32"},     {"wl::complex<float>",  "ComplexReal32"},
  {"double",  "Real64"},     {"wl::complex<double>", "ComplexReal64"}
};

Apply[(totypename[#1]:=#2)&,$typenames,{1}];
Apply[(totypespec[#2]:=#1)&,$typenames,{1}];
totypename["array"[type:Except["void"|"array"[___]],rank_Integer/;rank>=1]]:={totypename[type],rank}
totypespec[{type_/;(type!="Void"&&istypename[type]),rank_Integer/;rank>=1}]:="array"[totypespec[type],rank]

totypename[any___]:=Missing[]
totypespec[any___]:=Missing[]

istypename[name_]:=!MissingQ@totypespec[name]
istypespec[spec_]:=!MissingQ@totypename[spec]


syntax[list][code_]:=code//.{
    id["List"][exprs___]:>list[exprs]
  }

syntax[type][code_]:=code//.{
    id["Typed"][id[arg_],(literal|id)[type_]/;istypename[type]]:>typed[id[arg],totypespec[type]],
    id["Typed"][id[arg_],list[(literal|id)[t_],literal[r_]]/;istypename[{t,r}]]:>typed[id[arg],totypespec[{t,r}]],
    id["Typed"][(literal|id)[type_]/;istypename[type]]:>typed[totypespec[type]],
    id["Typed"][list[(literal|id)[t_],literal[r_]]/;istypename[{t,r}]]:>typed[totypespec[{t,r}]]
  }/.{
    id["Typed"][any___]:>(Message[syntax::badtype,tostring@id["Typed"][any]];Throw["syntax"])
  }

syntax[clause][code_]:=code//.{
    id[type:("Table"|"Do"|"Sum"|"Product")][expr_,iters__]:>(
      clause[type][id["Function"][list@@DeleteCases[#[[;;,1]],nil],expr],#[[;;,2]]]&@
        Replace[{iters},{
            e:Except[list][___]:>{nil,iter[e]},
            list[e_]:>{nil,iter[e]},
            list[id[var_],spec:Repeated[_,3]]:>{id[var],variter[spec]},
            any_:>(Message[syntax::iter,tostring[any]];Throw["syntax"])
          },{1}])
  }/.{
    any:id[type:("Table"|"Do"|"Sum"|"Product")][___]:>
      (Message[syntax::bad,tostring[any],type];Throw["syntax"])
  }

syntax[mutable][code_]:=code//.{
    id["AddTo"][id[var_],expr_]:>native["add_to"][id[var],expr],
    id["SubtractFrom"][id[var_],expr_]:>native["subtract_from"][id[var],expr],
    id["TimesBy"][id[var_],expr_]:>native["times_by"][id[var],expr],
    id["DivideBy"][id[var_],expr_]:>native["divide_by"][id[var],expr],
    id["Increment"][id[var_]]:>native["increment"][id[var]],
    id["Decrement"][id[var_]]:>native["decrement"][id[var]],
    id["PreIncrement"][id[var_]]:>native["pre_increment"][id[var]],
    id["PreDecrement"][id[var_]]:>native["pre_decrement"][id[var]],
    id["AddTo"][target:id["Part"][id[var_],specs___],expr_]:>native["add_to"][target,expr],
    id["SubtractFrom"][target:id["Part"][id[var_],specs___],expr_]:>native["subtract_from"][target,expr],
    id["TimesBy"][target:id["Part"][id[var_],specs___],expr_]:>native["times_by"][target,expr],
    id["DivideBy"][target:id["Part"][id[var_],specs___],expr_]:>native["divide_by"][target,expr],
    id["Increment"][target:id["Part"][id[var_],specs___]]:>native["increment"][target],
    id["Decrement"][target:id["Part"][id[var_],specs___]]:>native["decrement"][target],
    id["PreIncrement"][target:id["Part"][id[var_],specs___]]:>native["pre_increment"][target],
    id["PreDecrement"][target:id["Part"][id[var_],specs___]]:>native["pre_decrement"][target],
    id["AppendTo"][id[var_],expr_]:>native["append_to"][id[var],expr],
    id["PrependTo"][id[var_],expr_]:>native["prepend_to"][id[var],expr]
  }/.{
    any:id[type:("AddTo"|"SubtractFrom"|"TimesBy"|"DivideBy"|"AppendTo"|"PrependTo")][___]:>
      (Message[syntax::bad,tostring[any],type];Throw["syntax"])
  }

syntax[function][code_]:=
  Module[{
    functionrules={
      id["Function"][args_,expr_]:>(
        function[
          id/@#[[;;,1]],   (* argument names *)
          #[[;;,2]],       (* argument types *)
          sequence[expr]    (* function body *)
          ]&@Replace[If[Head[args]===list,List@@args,{args}],{
              id[arg_]:>{arg,nil},
              typed[id[arg_],type_]:>{arg,type},
              any_:>(Message[syntax::farg,tostring[any]];Throw["syntax"])
            },{1}]),
      id["Function"][pure_]:>
        Module[{slots,slotsrule,slotspos},
          slots=Union@Cases[pure,
            s:id[type:("Slot"|"SlotSequence")][literal[i_Integer/;i>0]]:>
              (s->If[type=="Slot",i,slotseq[i]]),
            {0,Infinity},Heads->True];
          slotsrule=Module[{i=newid,nvar,names},
            nvar=Max[slots[[;;,2]]/.slotseq->Sequence,0]+1;
            If[nvar>$slotmaximum,(Message[syntax::slotmax,nvar-1];Throw["syntax"])];
            names=MapAt[pack[#]&,id[i,#]&/@Range[nvar],{-1,2}];
            (#1->Sequence@@Replace[#2,{slotseq[i_]:>names[[i;;]],i_:>names[[i;;i]]}]&)@@@slots];
          slotspos=(#[[2]]/.pack[i_]:>i)->#&/@Union[slotsrule[[;;,2]]];
          function[
            ReplacePart[Table[nil,Max[slotspos[[;;,1]],0]],slotspos],
            Table[nil,Max[slotspos[[;;,1]],0]],
            sequence[pure/.slotsrule]
          ]
        ],
      any:id["Function"][___]:>(Message[syntax::bad,tostring[any],"Function"];Throw["syntax"])
    }},
    Fold[
      If[#2=={},Replace[#,functionrules],ReplacePart[#,#2->Replace[Extract[##],functionrules]]]&,
      code,
      Reverse@SortBy[Length]@Position[code,id["Function"][___]]
    ]
  ]

syntax[scope][code_]:=code//.{
    id["Module"][list[vars___],expr_]:>(
      scope[#[[;;,1]],sequence[sequence@@Cases[#,{var_,init:Except[nil]}:>id["Set"][id[var],init],{1}],expr]]&@
        Replace[{vars},{
          id[var_]:>{var,nil},
          id["Set"][id[var_],init_]:>{var,init},
          any_:>(Message[syntax::scopevar,tostring[any]];Throw["syntax"])
        },{1}])
  }/.{
    any:id["Module"][___]:>(Message[syntax::bad,tostring[any],"Module"];Throw["syntax"])
  }

syntax[branch][code_]:=code//.{
    id["If"][cond_,true_,false_]:>branchif[cond,sequence@true,sequence@false],
    id["If"][cond_,true_]:>branchif[cond,sequence@true,sequence@id["Null"]],
    id["Which"][any__/;EvenQ@Length@{any}]:>
      branchwhich[native["_which_conditions"]@@(sequence/@{any}[[;;;;2]]),
        Sequence@@(sequence/@{any}[[2;;;;2]])]
  }/.{
    any:id["If"][___]:>(Message[syntax::bad,tostring[any],"If"];Throw["syntax"]),
    any:id["Which"][___]:>(Message[syntax::bad,tostring[any],"Which"];Throw["syntax"])
  }

syntax[sequence][code_]:=code//.{
    id["CompoundExpression"][exprs__]:>sequence[exprs]}//.{
    sequence[before___,sequence[exprs___],after___]:>sequence[before,exprs,after]}/.{
    sequence[]:>sequence[id["Null"]]
  }

syntax[assign][code_]:=code//.{
    id["Set"][id[var_],expr_]:>assign[id[var],expr],
    id["Set"][target:id["Part"][id[var_],specs___],expr_]:>assign[target,expr]
  }/.{
    any:id["Set"][___]:>(Message[syntax::bad,tostring[any],"Set"];Throw["syntax"])
  }

syntax[loopbreak][code_]:=Module[{heads,headspos,dopos},
    Do[
      headspos=Table[Append[p[[;;i]],0],{i,Length@p-1}];
      heads=Extract[code,headspos];
      If[Last@heads=!=sequence,
        Message[syntax::badbreak,tostring@Extract[code,Most@p]];Throw["syntax"],
        dopos=Select[
          Extract[headspos,Position[heads,clause["Do"]]],
          ReplacePart[#,-1->1]==p[[;;Length@#]]&];
        If[Length@dopos==0,
          Message[syntax::breakloc,tostring@Extract[code,Most@p]];Throw["syntax"],
          code=ReplacePart[code,{Last[dopos]->clause["BreakDo"],p->break[]}];
      ]]
    ,{p,Position[code,id["Break"][]]}];
    code//.{
        sequence[any___,break[]]:>sequence[any,break[],id["Null"]]
      }
  ]

$syntaxpasses={list,type,clause,mutable,function,scope,branch,sequence,assign,loopbreak};
allsyntax[code_]:=Fold[syntax[#2][#1]&,code,$syntaxpasses];


$builtinconstants=const/@
<|
  "Null"       ->"null",
  "Pi"         ->"pi",
  "E"          ->"e",
  "Degree"     ->"degree",
  "EulerGamma" ->"euler_gamma",
  "I"          ->"i",
  "All"        ->"all",
  "True"       ->"true",
  "False"      ->"false"
|>;

$builtinfunctions=native/@
<|
(* scope *)
    (*"Module"*)
(* control flow *)
  (*"If"              ->"native_if",*)
    (*"Do"*)
    (*"Table"*)
    (*"Sum"*)
    (*"Product"*)
(* arithmetic *)
  "Plus"            ->"plus",
  "Subtract"        ->"subtract",
  "Times"           ->"times",
  "Divide"          ->"divide",
  "Minus"           ->"minus",
    (*"AddTo"*)
    (*"SubtractFrom"*)
    (*"TimesBy"*)
    (*"DivideBy"*)
(* complex numbers *)
  "Complex"         ->"make_complex",
  "Re"              ->"re",
  "Im"              ->"im",
  "ReIm"            ->"re_im",
  "Abs"             ->"abs",
  "Arg"             ->"arg",
  "AbsArg"          ->"abs_arg",
  "Conjugate"       ->"conjugate",
(* numerical functions *)
  "N"               ->"n",
  "Round"           ->"round",
  "Floor"           ->"floor",
  "Ceiling"         ->"ceiling",
  "IntegerPart"     ->"integer_part",
  "FractionalPart"  ->"fractional_part",
  "Mod"             ->"mod",
  "Less"            ->"less",
  "Greater"         ->"greater",
  "LessEqual"       ->"less_equal",
  "GreaterEqual"    ->"greater_equal",
  "Equal"           ->"equal",
  "Unequal"         ->"unequal",
  "Sign"            ->"sign",
  "Clip"            ->"clip",
  "Chop"            ->"chop",
  "Unitize"         ->"unitize",
  "UnitStep"        ->"unit_step",
  "Positive"        ->"positive",
  "Negative"        ->"negative",
  "NonPositive"     ->"non_positive",
  "NonNegative"     ->"non_negative",
  "Min"             ->"min",
  "Max"             ->"max",
  "Ramp"            ->"ramp",
  "LogisticSigmoid" ->"logistic_sigmoid",(*
  "Chop"            ->"chop",
  "Threshold"       ->"threshold",
  "NumericalOrder"  ->"numerical_order",*)
(* integral functions *)
  "EvenQ"           ->"even_q",
  "OddQ"            ->"odd_q",
  "Divisible"       ->"divisible",
  "Fibonacci"       ->"fibonacci",
  "LucasL"          ->"lucas_l",
(* boolean functions *)
  "Boole"           ->"boole",
  "Not"             ->"bool_not",
  "And"             ->"bool_and",
  "Or"              ->"bool_or",
  "Xor"             ->"bool_xor",
  "Nand"            ->"bool_nand",
  "Nor"             ->"bool_nor",
  "Xnor"            ->"bool_xnor",
  "Implies"         ->"implies",
  "BitNot"          ->"bit_not",
  "BitAnd"          ->"bit_and",
  "BitOr"           ->"bit_or",
  "BitXor"          ->"bit_xor",
  "BitLength"       ->"bit_length",
(* elementary functions *)
  "Log"             ->"log",
  "Log10"           ->"log10",
  "Log2"            ->"log2",
  "Exp"             ->"exp",
  "Power"           ->"power",
  "Sqrt"            ->"sqrt",
  "Sin"             ->"sin",
  "Sinc"            ->"sinc",
  "Cos"             ->"cos",
  "Tan"             ->"tan",
  "Csc"             ->"csc",
  "Sec"             ->"sec",
  "Cot"             ->"cot",
  "Sinh"            ->"sinh",
  "Cosh"            ->"cosh",
  "Tanh"            ->"tanh",
  "Csch"            ->"csch",
  "Sech"            ->"sech",
  "Coth"            ->"coth",
  "ArcSin"          ->"arcsin",
  "ArcCos"          ->"arccos",
  "ArcTan"          ->"arctan",
  "ArcCsc"          ->"arccsc",
  "ArcSec"          ->"arcsec",
  "ArcCot"          ->"arccot",
  "ArcSinh"         ->"arcsinh",
  "ArcCosh"         ->"arccosh",
  "ArcTanh"         ->"arctanh",
  "ArcCsch"         ->"arccsch",
  "ArcSech"         ->"arcsech",
  "ArcCoth"         ->"arccoth",
  "Gudermannian"    ->"gudermannian",
  "InverseGudermannian"->"inverse_gudermannian",
  "Haversine"       ->"haversine",
  "InverseHaversine"->"inverse_haversine",
(* special functions *)
  "Gamma"           ->"gamma",
  "LogGamma"        ->"log_gamma",
  "Erf"             ->"erf",
  "Erfc"            ->"erfc",
  "Beta"            ->"beta",
  "Zeta"            ->"zeta",
(* random number *)
    (*"RandomInteger"*)
    (*"RandomReal"*)
    (*"RandomComplex"*)
    (*"RandomVariate"*)
  "RandomChoice"    ->"random_choice",
(* array operation *)
    (*"ConstantArray"*)
  "List"            ->"list",
  "Set"             ->"set",
  "Dimensions"      ->"dimensions",
  "Length"          ->"length",
  "ArrayDepth"      ->"array_depth",
  "Part"            ->"part",
  "VectorQ"         ->"vector_q",
  "MatrixQ"         ->"matrix_q",
  "Span"            ->"make_span",
  "Total"           ->"total",
  "Mean"            ->"mean",
  "Range"           ->"range",
  "Reverse"         ->"reverse",
  "First"           ->"first",
  "Last"            ->"last",
  "Most"            ->"most",
  "Rest"            ->"rest",
  "Join"            ->"join",
  "Transpose"       ->"transpose",
  "Flatten"         ->"flatten",
  "Order"           ->"order",
  "Ordering"        ->"ordering",
  "Sort"            ->"sort",
  "Append"          ->"append",
  "Prepend"         ->"prepend",
(*functional*)
  "Apply"           ->"apply",
  "Select"          ->"select",
  "Count"           ->"count",
  "Map"             ->"map",
  "Scan"            ->"scan",
  "MapThread"       ->"map_thread",
  "Nest"            ->"nest",
  "NestList"        ->"nest_list",
  "Fold"            ->"fold",
  "FoldList"        ->"fold_list",
  "NestWhile"       ->"nest_while",
  "NestWhileList"   ->"nest_while_list",
  "FixedPoint"      ->"fixed_point",
  "FixedPointList"  ->"fixed_point_list",
  "Identity"        ->"identity",
  "Composition"     ->"composition",
  "RightComposition"->"right_composition",
  "AllTrue"         ->"all_true",
  "AnyTrue"         ->"any_true",
  "NoneTrue"        ->"none_true"
    (*"Count"*)
|>;


headerseries[expr_,pos_]:=Table[Extract[expr,ReplacePart[Take[pos,n],-1->0]],{n,Length@pos}]

variablerename[code_]:=
  Module[{renamerules={
      scope[ids_,expr_]:>
        Module[{vars=Table[newvar,Length@ids]},
          AppendTo[$variabletable,AssociationThread[vars->ids]];
          scope[vars,expr/.Thread[(id/@ids)->(var/@vars)]]
        ],
      function[ids_,types_,expr_]:>
        Module[{vars},
          vars=If[MatchQ[#,id[_,pack[_]]],newvarpack,newvar]&/@ids;
          (*AppendTo[$variabletable,AssociationThread[vars->ids]];*)
          function[vars,types,
            expr/.
              MapThread[#1->If[
                (Length[#]==1&&Count[headerseries[expr,#[[1]]],function]==0)&@
                  Position[expr,#1],movvar,var][#2]&,{ids,vars}],
            Sequence@@If[MemberQ[ids,id[_,pack[_]]],
              With[{var=newvar},{variadic[{var},{nil},expr/.{id[_,i_]:>argv[var,i]}]}],
              {}]
          ]
        ],
      any_:>(Message[semantics::bad,tostring@any];Throw["semantics"])
    }},
    Fold[If[#2=={},Replace[#,renamerules],ReplacePart[#,#2->Replace[Extract[##],renamerules]]]&,
      code,Most/@Reverse@SortBy[Length]@Position[code,function|scope]]
  ]

listtoseq[expr_]:=Replace[expr,list[any___]:>Sequence[any]]

functionmacro[code_]:=code//.{
    id["ConstantArray"][val_,dims_]:>native["constant_array"][val,vargtag,listtoseq[dims]],
    id["RandomInteger"][spec_,dims_]:>native["random_integer"][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomInteger"][spec_]:>native["random_integer"][listtoseq[spec],vargtag],
    id["RandomInteger"][]:>native["random_integer"][literal[1],vargtag],
    id["RandomReal"][spec_,dims_]:>native["random_real"][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomReal"][spec_]:>native["random_real"][listtoseq[spec],vargtag],
    id["RandomReal"][]:>native["random_real"][literal[1],vargtag],
    id["RandomComplex"][spec_,dims_]:>native["random_complex"][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomComplex"][spec_]:>native["random_complex"][listtoseq[spec],vargtag],
    id["RandomComplex"][]:>native["random_complex"][id["Complex"][literal[1],literal[1]],vargtag],
    id["RandomVariate"][dist_,dims_]:>native["random_variate"][dist,vargtag,listtoseq[dims]],
    id["RandomVariate"][dist_]:>native["random_variate"][dist,vargtag],
    id["RandomChoice"][array_,dims_]:>native["random_choice"][array,vargtag,listtoseq[dims]],
    id["Count"][array_,id["PatternTest"][id["Blank"][],func_]]:>native["count"][array,vargtag,func],
    id["Count"][array_,id["PatternTest"][id["Blank"][],func_],literal[i_Integer]]:>native["count"][array,vargtag,func,const[i]],
    id["Count"][array_,patt_,literal[i_Integer]]:>native["count"][array,patt,const[i]],
    id["Total"][array_,literal[i_Integer]]:>native["total"][array,const[i]],
    id["Total"][array_,list[literal[i_Integer]]]:>native["total"][array,const[i],const[i]],
    id["Total"][array_,list[literal[i1_Integer],literal[i2_Integer]]]:>native["total"][array,const[i1],const[i2]],
    id["Clip"][any_,list[min_,max_]]:>native["clip"][any,vargtag,min,max],
    id["Clip"][any_,list[min_,max_],list[vmin_,vmax_]]:>native["clip"][any,vargtag,min,max,vmin,vmax],
    id["Map"][func_,array_,list[literal[i_Integer]]]:>native["map"][func,array,const[i]],
    id["Reverse"][array_,literal[i_Integer]]:>native["reverse"][array,const[i]],
    id["ArrayReshape"][array_,dims_]:>native["array_reshape"][array,vargtag,listtoseq[dims]],
    id["ArrayReshape"][array_,dims_,padding_]:>native["array_reshape"][array,padding,vargtag,listtoseq[dims]],
    id["Fold"][func_,x_,id["Reverse"][y_]]:>native["foldr"][func,x,y],
    id["Fold"][func_,id["Reverse"][y_]]:>native["foldr"][func,y],
    id["FoldList"][func_,x_,id["Reverse"][y_]]:>native["foldr_list"][func,x,y],
    id["FoldList"][func_,id["Reverse"][y_]]:>native["foldr_list"][func,y],
    id["Apply"][func_,list[args___]]:>func[args],
    id["Apply"][func_,array_,list[literal[i_Integer]]]:>native["apply"][func,array,const[i]],
    id["MapThread"][func_,list[arrays__]]:>native["map_thread"][func,vargtag,arrays],
    id["MapThread"][func_,list[arrays__],literal[i_Integer]]:>native["map_thread"][func,const[i],vargtag,arrays],
    id["MapThread"][func_,array_,literal[i_Integer]]:>native["map_thread"][func,array,const[i]],
    id["Part"][array_,specs___]:>id["Part"][array,
      Sequence@@Replace[{specs},literal[i_Integer/;i>0]:>native["cidx"][literal[i-1]],{1}]],
    id["Transpose"][array_,list[l:(literal[_Integer]..)]]:>
      native["transpose"][array,Sequence@@(const/@{l}[[;;,1]])],
    id["Flatten"][array_,literal[i_Integer]]:>
      native["flatten"][array,
        If[0<=i<$rankmaximum,consts@@Range[i+1],
          (Message[semantics::bad,tostring@id["Flatten"][array,literal[i]]];Throw["semantics"])]],
    id["Flatten"][array_,l:list[literal[_Integer]..]]:>id["Flatten"][array,list[l]],
    id["Flatten"][array_,l:list[list[literal[_Integer]..]..]]:>
      Module[{levels=l/.{list->List,literal->Identity},ints,maxlevel,out},
        ints=Cases[levels,_Integer,-1];
        If[1<=Min[ints]&&(maxlevel=Max[ints])<=$rankmaximum&&DuplicateFreeQ[ints],
        out=Join[levels,List/@Complement[Range[maxlevel],ints]];
        native[If[#==Range[Length@#]&@Flatten[out],"flatten_copy","flatten"]][
          array,Sequence@@(consts@@@out)],
        (Message[semantics::bad,tostring@id["Flatten"][array,l]];Throw["semantics"])]
      ],
    id["Composition"][funcs__][args___]:>First@Fold[{#2@@#1}&,{args},{funcs}],
    id["RightComposition"][funcs__][args___]:>First@Fold[{#2@@#1}&,{args},Reverse@{funcs}],
    id["Composition"][]:>native["identity"],
    id["RightComposition"][]:>native["identity"],
    id["NestWhile"][func_,expr_,test_,literal[i_Integer],id["Infinity"],any___]:>
      native["nest_while"][func,expr,test,const[i],const["int_infinity"],any],
    id["NestWhileList"][func_,expr_,test_,literal[i_Integer],id["Infinity"],any___]:>
      native["nest_while_list"][func,expr,test,const[i],const["int_infinity"],any],
    id["NestWhile"][func_,expr_,test_,literal[i_Integer],any___]:>
      native["nest_while"][func,expr,test,const[i],any],
    id["NestWhileList"][func_,expr_,test_,literal[i_Integer],any___]:>
      native["nest_while_list"][func,expr,test,const[i],any],
    id["FixedPoint"][any___,id["Rule"][id["SameTest"],pred_]]:>native["fixed_point"][any,vargtag,pred],
    id["FixedPointList"][any___,id["Rule"][id["SameTest"],pred_]]:>native["fixed_point_list"][any,vargtag,pred],
    id["AllTrue"][array_,test_,literal[i_Integer]]:>native["all_true"][array,test,const[i]],
    id["AnyTrue"][array_,test_,literal[i_Integer]]:>native["any_true"][array,test,const[i]],
    id["NoneTrue"][array_,test_,literal[i_Integer]]:>native["none_true"][array,test,const[i]],
    id["Join"][any__,literal[i_Integer]]:>native["join"][const[i],any]
  }

arithmeticmacro[code_]:=code//.{
    id["Plus"][x1_,x2_,xs__]:>id["Plus"][id["Plus"][x1,x2],xs],
    id["Plus"][x1_,id["Times"][literal[-1],x2_]]:>id["Subtract"][x1,x2]
  }//.{
    id["Times"][literal[-1],x_]:>id["Minus"][x]
  }//.{
    id["Times"][x1_,x2_,xs__]:>id["Times"][id["Times"][x1,x2],xs],
    id["Times"][x1_,id["Power"][x2_,literal[-1]]]:>id["Divide"][x1,x2]
  }

resolvesymbols[code_]:=code//.{
    id[func_][args___]:>Lookup[$builtinfunctions,func,id[func]][args]
  }/.{
    id[any_]:>Lookup[$builtinconstants,any,Lookup[$builtinfunctions,any,id[any]]]
  }

lexicalorder[a_,b_]:=
  Module[{la=Length[a],lb=Length[b]},
    If[la>lb,Order[a,PadRight[b,la,-1]],Order[PadRight[a,lb,-1],b]]
  ];

findinit[code_]:=
  Module[{sequencepos,scopevarpos,scopevar,initpos,initscopepos,refpos},
    sequencepos=Most/@Position[code,sequence];
    scopevarpos=<|Catenate[Module[{p=Most[#]},#->Append[p,2]&/@Extract[code,Append[p,1]]]&/@Position[code,scope]]|>;
    scopevar=Keys[scopevarpos];
    initpos=First@Sort[#,lexicalorder]&/@
      GroupBy[{Extract[code,Join[Most[#],{1,1}]],Most[#]}&/@Position[code,assign],First->Last];
    refpos=First@Sort[#,lexicalorder]&/@
      DeleteMissing@AssociationThread[scopevar->#/@scopevar]&@
        GroupBy[Module[{p=Most[#]},Extract[code,Append[p,1]]->p]&/@Position[code,var],First->Last];
    Module[{badinit,badref},
      badinit=Cases[{#,scopevarpos[#],Most@initpos[#]}&/@scopevar,
        {name_,exp_,act_}/;(exp=!=act):>(name-><|"Expected"->exp,"Actual"->act|>)];
      badref=Cases[{#,If[MissingQ[#],#,Append[#,1]]&@initpos[#],refpos[#]}&/@scopevar,
        {name_,init_,use_}/;(init=!=use):>(name-><|"Initialization"->init,"FirstUsage"->use|>)];
      If[MissingQ@#2["Actual"],
        (Message[semantics::noinit,$variabletable[#1]];),
        (Message[semantics::badinit,$variabletable[#1]];Throw["semantics"])]&@@@badinit;
      (Message[semantics::badref,$variabletable[#1]];Throw["semantics"])&@@@badref;
    ];
    ReplacePart[code,Append[#,0]->initialize&/@DeleteMissing[initpos/@scopevar]]
  ]

semantics[code_]:=findinit@resolvesymbols@arithmeticmacro@functionmacro@variablerename[code]


nativename[str_]:=StringRiffle[ToLowerCase@StringCases[str,RegularExpression["[A-Z][a-z]*"]],"_"]
getargtypes[function[_,types_,__]]:=types
expandpack[var_String]:=If[StringTake[var,2]=="vp","...",""]
anyispack[vars_List]:=AnyTrue[vars,StringTake[#,2]=="vp"&]

codegen[args[vars_,types_],___]:=
  MapThread[If[#=="auto&&",#,"const "<>#<>"&"]&@codegen[type[#1]]<>expandpack[#2]<>" "<>#2&,{types/.nil->"auto&&",vars}]
codegen[argv[var_,i_Integer]]:=var<>".get("<>ToString[i-1]<>")"
codegen[argv[var_,pack[i_Integer]]]:=var<>".get_pack("<>ToString[i-1]<>")"

codegen[function[vars_,types_,sequence[exprs___]],any___]:=
  {"[&](",Riffle[If[!anyispack[vars],Append[#,"auto&&..."],#]&@codegen[args[vars,types]],", "],")",
    codegen[sequence[exprs],If[{any}=={"Scope"},"Scope","Return"]]}
codegen[variadic[vars_,types_,sequence[exprs___]],___]:=
  {"[&](",Riffle[codegen[args[vars,types]],", "],")",codegen[sequence[exprs],"Return"]}
codegen[function[vars_,types_,sequence[exprs___],variadic[specs___]],___]:=
  codegen[native["variadic"][function[vars,types,sequence[exprs]],variadic[specs]]]

codegen[scope[_,sequence[exprs___]],any___]:=codegen[sequence[exprs],any]

codegen[initialize[var_,expr_],___]:={"auto ",codegen[var]," = ",codegen[native["val"][expr]]}

codegen[assign[var_,expr_],___]:=codegen[native["set"][var,expr]]

codegen[literal[s_String],___]:="std::string("ToString@CForm[s]<>")"
codegen[literal[i_Integer],___]:="int64_t("<>ToString@CForm[i]<>")"
codegen[literal[r_Real],___]:=ToString@CForm[r]
codegen[const[i_Integer],___]:="wl::const_int<"<>ToString@CForm[i]<>">{}"
codegen[c:consts[(_Integer)..],___]:="wl::const_ints<"<>StringRiffle[ToString@*CForm/@(List@@c),", "]<>">{}"
codegen[const[s_String],___]:="wl::const_"<>s

codegen[native[name_],"Function"]:="wl::"<>name
codegen[native[name_],___]:="WL_FUNCTION(wl::"<>name<>")"

codegen[vargtag,___]:="wl::varg_tag{}"
codegen[leveltag[l_Integer],___]:="wl::level_tag<"<>ToString@CForm[l]<>">{}"

codegen[clause[type_][func_,{iters___}],___]:={
    "wl::clause_"<>nativename[type],"(",
    codegen[func,If[type=="Do"||type=="BreakDo","Scope","Return"]],",",
    Riffle[codegen[#,"Return"]&/@{iters},", "],")"}
codegen[iter[expr___],___]:=codegen[native["iterator"][expr]]
codegen[variter[expr___],___]:=codegen[native["var_iterator"][expr]]

codegen[type[t_String],___]:=t
codegen[type["array"[t_,r_]],___]:="wl::ndarray<"<>t<>", "<>ToString[r]<>">"
codegen[typed[any_],___]:=codegen[type[any]]<>"{}"

codegen[var[name_],___]:=name<>expandpack[name]
codegen[movvar[name_],___]:="WL_PASS("<>name<>")"<>expandpack[name]
codegen[id[name_],___]:=(Message[semantics::undef,name];Throw["semantics"])

codegen[sequence[scope[vars_,expr_]],any___]:=codegen[scope[vars,expr],any]
codegen[sequence[expr___],"Scope"]:={"{",({codegen[#],";"}&/@{expr}),"}"}
codegen[sequence[most___,initialize[var_,expr_]],"Return"]:=codegen[sequence[most,initialize[var,expr],var],"Return"]
codegen[sequence[most___,last_],"Return"]:={"{",({codegen[#],";"}&/@{most}),"return ",codegen[native["val"][last]],";","}"}
codegen[sequence[expr___],"Hold"]:={"[&]",codegen[sequence[expr],"Return"]}
codegen[sequence[expr___],___]:={codegen[sequence[expr],"Hold"],"()"}

codegen[branchif[cond_,expr1_,expr2_],___]:=
  codegen[native["branch_if"][cond,expr1,expr2],"Hold"]
codegen[branchwhich[conds_,cases__],___]:=
  codegen[native["which"][conds,cases],"Hold"]
codegen[break[]]:="throw wl::loop_break{}"

codegen[list[any___],___]:=codegen[native["list"][any]]

codegen[head_[args___],any___]:={codegen[head,"Value"],"(",Riffle[codegen[#,any]&/@{args},", "],")"}
codegen[native[name_][args___],any___]:={codegen[native[name],"Function"],"(",Riffle[codegen[#,any]&/@{args},", "],")"}

codegen[any_,rest___]:=(Message[codegen::bad,tostring[any]];Throw["codegen"])

initcodegen[function[vars_,types_,expr_]]:=
  Flatten@{"auto main_function(",Riffle[codegen[args[vars,types]],", "],")",codegen[expr,"Return"]}
  
codeformat[segments_List]:=
  StringRiffle[#,{"","\n","\n"}]&@
    FoldPairList[
      Module[{pad=#1-Boole[StringTake[#2,1]=="}"]},
       {If[pad<=0,"",StringRepeat[" ",4pad]]<>#2,pad+Boole[StringTake[#2,-1]=="{"]}]&,
      0,StringJoin/@SplitBy[segments/.{"{"->Sequence[" {","\n"],";"->Sequence[";","\n"]},#=="\n"&][[;;;;2]]
    ]
maincodegen[code_]:=codeformat@initcodegen[code]


$numerictypes={"Undefined",
"Integer8","UnsignedInteger8",
"Integer16","UnsignedInteger16",
"Integer32","UnsignedInteger32",
"Integer64","UnsignedInteger64",
"Real32","Real64",
"ComplexReal32","ComplexReal64"};

symboltype[type_]:=Which[StringContainsQ[type,"Integer"],Integer,
  StringContainsQ[type,"Complex"],Complex,StringContainsQ[type,"Real"],Real,True,type];

loadfunction[libpath_String,funcid_String,args_]:=
  Module[{typefunc,libfunc,rank,type,commontype,returntype,argtypes,maxrank=1024,maxtypecount=256},
    typefunc=LibraryFunctionLoad[libpath,funcid<>"_type",{},Integer];
    If[typefunc===$Failed,Message[link::rettype];Return[$Failed]];
    {rank,type}=QuotientRemainder[typefunc[],maxtypecount];
    LibraryFunctionUnload[typefunc];
    If[type==0&&rank==0,returntype="Void",
      If[0<=rank<=maxrank,Null,Message[link::bigrank];Return[$Failed]];
      If[2<=type+1<=Length[$numerictypes],type=$numerictypes[[type+1]],
        Message[link::badtype,type];Return[$Failed]];
      commontype=symboltype[type];
      returntype=If[rank==0,commontype,
        If[MemberQ[{"Integer64","Real64","ComplexReal64"},type],
          {commontype,rank},LibraryDataType[NumericArray,type,rank]]];
    ];
    argtypes=Replace[totypename[#],{
        {t_,r_}:>If[MemberQ[{"Integer64","Real64","ComplexReal64"},t],
          {symboltype[t],r,"Constant"},{LibraryDataType[NumericArray,t,r],"Constant"}],
        t_String:>symboltype[t]}]&/@args;
    LibraryFunctionLoad[libpath,funcid<>"_func",argtypes,returntype]
  ]


$template="
#include \"librarylink.h\"

std::default_random_engine wl::global_random_engine;
WolframLibraryData wl::librarylink::lib_data;

EXTERN_C DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData lib_data) {
#if defined(WL_NO_RANDOM_DEVICE)
    wl::global_random_engine.seed(wl::utils::_get_time());
#else
    std::random_device rd;
    wl::global_random_engine.seed(rd());
#endif
    wl::librarylink::lib_data = lib_data;
    return LIBRARY_NO_ERROR;
}

`funcbody`
EXTERN_C DLLEXPORT int `funcid`_type(WolframLibraryData lib_data,
    mint argc, MArgument *argv, MArgument res) {
    using ReturnType = wl::remove_cvref_t<
        decltype(main_function(`argsv`))>;
    using ValueType = std::conditional_t<
        wl::is_array_v<ReturnType>, wl::value_type_t<ReturnType>, ReturnType>;
    mint rank = mint(wl::array_rank_v<ReturnType>);
    mint type = wl::librarylink::get_numeric_array_type<ValueType>();
    constexpr mint max_type_count = 256;
    MArgument_setInteger(res, rank * max_type_count + type);
    return LIBRARY_NO_ERROR;
}

EXTERN_C DLLEXPORT int `funcid`_func(WolframLibraryData lib_data,
    mint argc, MArgument *argv, MArgument res) {
    try {
        auto val = main_function(
`args`
        );
        wl::librarylink::set(res, val);
    } catch (int library_error) {
        return library_error;
    } catch (const std::logic_error& error) {
        return LIBRARY_FUNCTION_ERROR;
    } catch (const std::bad_alloc& error) {
        return LIBRARY_MEMORY_ERROR;
    } catch (...) {
        return LIBRARY_FUNCTION_ERROR;
    }
    return LIBRARY_NO_ERROR;
}
";


Options[compilelink]={
  "LibraryDirectory"->"TargetDirectory"/.Options[CCompilerDriver`CreateLibrary],
  "WorkingDirectory"->Automatic,
  "Debug"->False, 
  "CompileOptions"->""
};
Options[CompileToBinary]=Options[compilelink];

compilelink[$Failed,___]:=$Failed;

compilelink[f_,OptionsPattern[]]:=
  Module[{output,types,funcid,src,lib,workdir,libdir},
    $CppSource="";
    $CompilerOutput="";
    output=f["output"];
    types=codegen@*type/@f["types"];
    funcid="f"<>ToString@RandomInteger[{10^8,10^9-1}];
    workdir=OptionValue["WorkingDirectory"];
    libdir=OptionValue["LibraryDirectory"];
    If[workdir=!=Automatic&&!(StringQ[workdir]&&DirectoryQ[workdir]),
      Message[link::workdir];Return[$Failed]];
    If[!StringQ[libdir],
      Message[link::libdir];Return[$Failed]];
    MathCompile`$CppSource=
      TemplateApply[$template,<|
        "funcbody"->output,
        "argsv"->StringRiffle[#<>"{}"&/@types,", "],
        "args"->StringRiffle[
          MapThread[StringTemplate["wl::librarylink::get<``>(argv[``])"],
            {types,Range[Length@types]-1}],
          {"            ",",\n            ",""}],
        "funcid"->funcid
        |>];
    If[FileExistsQ[$packagepath<>"/src/math_compile.h"]=!=True,
      Message[link::noheader];Return[$Failed]];
    lib=CCompilerDriver`CreateLibrary[
      MathCompile`$CppSource,funcid,
      "Language"->"C++",
      "CompileOptions"->
        If[OptionValue["Debug"],$debugcompileroptions,$compileroptions][
          CCompilerDriver`DefaultCCompiler[]]<>OptionValue["CompileOptions"],
      "CleanIntermediate"->!OptionValue["Debug"],
      "IncludeDirectories"->{$packagepath<>"/src"},
      "WorkingDirectory"->workdir,
      "TargetDirectory"->libdir,
      "ShellCommandFunction"->((MathCompile`$CompilerCommand=#)&),
      "ShellOutputFunction"->((MathCompile`$CompilerOutput=#)&)
    ];
    If[lib===$Failed,Message[link::genfail];Return[$Failed]];
    loadfunction[lib,funcid,f["types"]]
  ]


$compileroptions=<|
  CCompilerDriver`GCCCompiler`GCCCompiler->
    "-x c++ -std=c++1z -fPIC -O3 -ffast-math -march=native",
  CCompilerDriver`GenericCCompiler`GenericCCompiler->
    If[$SystemID=="Windows-x86-64",(*MinGW*)"-static ",""]<>
      "-x c++ -std=c++1z -fPIC -O3 -ffast-math -march=native",
  CCompilerDriver`IntelCompiler`IntelCompiler->
    "-std=c++17 -Kc++ -O3 -fp-model fast=2 -march=native",
  CCompilerDriver`VisualStudioCompiler`VisualStudioCompiler->
    "/std:c++17 /EHsc /Ox /Oi /Ob2 /Gy /fp:fast /DNDEBUG"
|>;
$debugcompileroptions=<|
  CCompilerDriver`GCCCompiler`GCCCompiler->
    "-x c++ -std=c++1z -fPIC -O0 -g3 -march=native",
  CCompilerDriver`GenericCCompiler`GenericCCompiler->
    If[$SystemID=="Windows-x86-64",(*MinGW*)"-static ",""]<>
      "-x c++ -std=c++1z -fPIC -O0 -g3 -march=native",
  CCompilerDriver`IntelCompiler`IntelCompiler->
    "-std=c++17 -Kc++ -O0 -g  -march=native -debug all -traceback -check-uninit",
  CCompilerDriver`VisualStudioCompiler`VisualStudioCompiler->
    "/std:c++17 /EHsc /Od"
|>


print[id["Set"][x_,y_]]:=RowBox[{print@x,"=",print@y}]
print[id["List"][args___]]:=RowBox[Join[{"{"},Riffle[print/@{args},","],{"}"}]]
print[id["Plus"][args__/;Length@{args}>=2]]:=RowBox[Join[{"("},Riffle[print/@{args},"+"],{")"}]]
print[id["Times"][args__/;Length@{args}>=2]]:=RowBox[Join[{"("},Riffle[print/@{args},"\[Cross]"],{")"}]]
print[id["Power"][x_,y_]]:=SuperscriptBox[print@x,print@y]
print[native[any_][args___]]:=RowBox[Join[{any,"["},Riffle[print/@{args},","],{"]"}]]
print[native[any_]]:=any
print[typed[any_]]:=RowBox[{"Typed","[",ToString[any],"]"}]
print[id["Slot"][x_]]:=RowBox[{"#",print@x}]
print[head_[args___]]:=RowBox[Join[{print@head,"["},Riffle[print/@{args},","],{"]"}]]
print[id[id_String]]:=id
print[var[var_String]]:=var
print[movvar[var_String]]:=RowBox[{"move","(",var,")"}]
print[literal[literal_]]:=ToString@CForm@literal
print[list[args___]]:=RowBox[Join[{"{"},Riffle[print/@{args},","],{"}"}]]
print[iter[spec__]]:=RowBox[Join[{"Iterator","["},Riffle[print/@{spec},","],{"]"}]]
print[variter[spec__]]:=RowBox[Join[{"Iterator","["},Riffle[(print/@{spec}),","],{"]"}]]
print[clause[type_][func_,iters_List]]:=RowBox[{type,"[",print[func],",",print[list@@iters],"]"}]
print[mutable[type_][var_,expr_]]:=print@id[type][var,expr]
print[function[args_,types_,expr_]]:=RowBox[{"Function","[",print[list@@(id/@args)],",",print[expr],"]"}]
print[scope[vars_,expr_]]:=RowBox[{"Module","[",print[list@@(id/@vars)],",",print[expr],"]"}]
print[branch[cond_,true_,false_]]:=print@id["If"][cond,true,false];
print[sequence[exprs__]]:=RowBox[Join[{"("},Riffle[(print/@{exprs}),";"],{")"}]]
print[assign[var_,expr_]]:=RowBox[{print@var,"=",print@expr}]
print[initialize[var_,expr_]]:=RowBox[{print@var,"=",print@expr}]
print[any_]:=any

tostring[any_]:=ToString@DisplayForm[print[any]]


End[];


EndPackage[];
