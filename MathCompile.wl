(* ::Package:: *)

BeginPackage["MathCompile`"];


CompileToCode::usage="\!\(\*RowBox[{\"CompileToCode\", \"[\", StyleBox[\"func\", \"TI\"], \"]\"}]\) rewrites a Wolfram Language function in C++.";


SetAttributes[CompileToCode,HoldFirst];
CompileToCode[Function[func___]]:=compile[Hold[Function[func]]]


Begin["`Private`"];


parse::unknown="`1` cannot be parsed.";
syntax::iter="`1` does not have a correct syntax for an iterator.";
syntax::bad="`1` does not have a correct syntax for `2`.";
syntax::farg="`1` does not have a correct syntax for a function argument.";
syntax::fpure="`1` does not have a correct syntax for a pure function.";
syntax::scopevar="`1` does not have a correct syntax for a local variable.";
semantics::bad="`1` does not have correct semantics.";
semantics::undef="Identifier `1` is not found.";
semantics::noinit="Variable `1` is declared but not initialized.";
semantics::badinit="Variable `1` is initialized in a nested scope.";
semantics::badref="Variable `1` is referenced before initialization.";
codegen::bad="Cannot generate code for `1`."


compile[code_]:=
  Module[{output,error},
    $variabletable=Association[];
    error=Catch[
        output=maincodegen@macro@semantics@allsyntax@parse[code];
      ];
    Clear[$variabletable];
    If[error===Null,output,$Failed]
  ]


newid:=SymbolName@Unique["id"]
newvar:=SymbolName@Unique["v"]


parse[Hold[head_[args___]]]:=parse[Hold[head]]@@(parse/@Hold/@Hold[args])

parse[Hold[s_Symbol]]:=id[SymbolName[s]];
parse[Hold[I]]:=id["I"];

parse[Hold[i_Integer]]:=literal[i];
parse[Hold[i_Real]]:=literal[i];
parse[Hold[i_String]]:=literal[i];

parse[Hold[any_]]:=(Message[parse::unknown,ToString[Unevaluated[any]]];Throw["lexical"])


$typenames={
  {"void", "Void"},
  {"bool", "Boolean"},
  {"std::string", "String"},
  {"int64_t", "Integer"},    {"uint64_t", "UnsignedInteger"},
  {"int8_t",  "Integer8"},   {"uint8_t",  "UnsignedInteger8"},
  {"int16_t", "Integer16"},  {"uint16_t", "UnsignedInteger16"},
  {"int32_t", "Integer32"},  {"uint32_t", "UnsignedInteger32"},
  {"int64_t", "Integer64"},  {"uint64_t", "UnsignedInteger64"},
  {"double",  "Real"},       {"std::complex<double>", "Complex"},
  {"float",   "Real32"},     {"std::complex<float>",  "Complex64"},
  {"double",  "Real64"},     {"std::complex<double>", "Complex128"}
};

Apply[(totypename[#1]:=#2)&,$typenames,{1}];
Apply[(totypespec[#2]:=#1)&,$typenames,{1}];
totypename["array"[type:Except["void"|"array"[___]],rank_Integer/;rank>=1]]:="Array"[totypename[type],rank]
totypespec["Array"[type:Except["Void"|"Array"[___]],rank_Integer/;rank>=1]]:="array"[totypespec[type],rank]

totypename[any___]:=Throw["type"]
totypespec[any___]:=Throw["type"]

istypename[name_]:=Catch[True&@totypespec[name],"type",False&]
istypespec[spec_]:=Catch[True&@totypename[spec],"type",False&]


syntax[list][code_]:=code//.{
    id["List"][exprs___]:>list[exprs]
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
    id["AddTo"][target:id["Part"][id[var_],specs___],expr_]:>native["view_add_to"][target,expr],
    id["SubtractFrom"][target:id["Part"][id[var_],specs___],expr_]:>native["view_subtract_from"][target,expr],
    id["TimesBy"][target:id["Part"][id[var_],specs___],expr_]:>native["view_times_by"][target,expr],
    id["DivideBy"][target:id["Part"][id[var_],specs___],expr_]:>native["view_divide_by"][target,expr],
    id["AppendTo"][id[var_],expr_]:>native["append_to"][id[var],expr],
    id["PrependTo"][id[var_],expr_]:>native["prepend_to"][id[var],expr]
  }/.{
    any:id[type:("AddTo"|"SubtractFrom"|"TimesBy"|"DivideBy"|"AppendTo"|"PrependTo")][___]:>
      (Message[syntax::bad,tostring[any],type];Throw["syntax"])
  }

syntax[function][code_]:=
  Module[{
    functionrules={
      id["Function"][arg:Except[list[___]],expr_]:>id["Function"][list[arg],sequence[expr]],
      id["Function"][list[args___],expr_]:>(
        function[Range[Length@#],#[[;;,1]],#[[;;,2]],sequence[expr]]&@
          Replace[{args},{
              id[arg_]:>{arg,nil},
              id["Typed"][id[arg_],literal[type_]/;istypename[type]]:>{arg,totypespec[type]},
              id["Typed"][id[arg_],literal["Array"][literal[t_],literal[r_]]/;istypename["Array"[t,r]]]:>{arg,totypespec["Array"[t,r]]},
              any_:>(Message[syntax::farg,tostring[any]];Throw["syntax"])
            },{1}]),
      id["Function"][pure_]:>If[FreeQ[pure,id["Function"][___]],
        function[#1,#2[[;;,2,1]],Table[nil,Length@#1],sequence[pure/.#2]]&[#,id["Slot"][literal[#]]->id[newid]&/@#]&@
          Union@Cases[pure,id["Slot"][literal[i_Integer/;i>0]]:>i,{0,Infinity}],
        (Message[syntax::fpure,tostring[id["Function"][pure]]];Throw["syntax"])],
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
          id["Set"][id[var_],init_/;(Head[init]=!=id["Typed"])]:>{var,init},
          id["Set"][id[var_],id["Typed"][literal[type_]/;istypename[type]]]:>{var,typed[totypespec[type]]},
          id["Set"][id[var_],id["Typed"][literal["Array"][literal[t_],literal[r_]]/;istypename["Array"[t,r]]]]:>{var,typed[totypespec["Array"[t,r]]]},
          id["Set"][id[var_],id["Typed"][t___]]:>{var,badsyntax[id["Typed"][t]]},
          any_:>(Message[syntax::scopevar,tostring[any]];Throw["syntax"])
        },{1}])
  }/.{
    any:id["Module"][___]:>(Message[syntax::bad,tostring[any],"Module"];Throw["syntax"])
  }

syntax[branch][code_]:=code//.{
    id["If"][cond_,true_,false_]:>branch[cond,sequence@true,sequence@false],
    id["If"][cond_,true_]:>branch[cond,sequence@true,sequence@id["Null"]]
  }/.{
    any:id["If"][___]:>(Message[syntax::bad,tostring[any],"If"];Throw["syntax"])
  }

syntax[sequence][code_]:=code//.{
    id["CompoundExpression"][exprs__]:>sequence[exprs]}/.{
    id["CompoundExpression"]:>badsyntax[id["CompoundExpression"]]}//.{
    sequence[before___,sequence[exprs___],after___]:>sequence[before,exprs,after]}/.{
    sequence[]:>sequence[id["Null"]]
  }

syntax[assign][code_]:=code//.{
    id["Set"][id[var_],expr_]:>assign[id[var],expr]
  }/.{
    any:id["Set"][___]:>(Message[syntax::bad,tostring[any],"Set"];Throw["syntax"])
  }

$syntaxpasses={list,clause,mutable,function,scope,branch,sequence,assign};
allsyntax[code_]:=Fold[syntax[#2][#1]&,code,$syntaxpasses];


$builtinconstants=native/@
<|
  "Null"       ->"const_null",
  "Pi"         ->"const_pi",
  "E"          ->"const_e",
  "Degree"     ->"const_degree",
  "EulerGamma" ->"const_euler_gamma",
  "I"          ->"const_i"
|>;

$builtinfunctions=native/@
<|
(* scope *)
    (*"Module"*)
(* control flow *)
  "If"              ->"native_if",
    (*"Do"*)
    (*"Table"*)
    (*"Sum"*)
    (*"Product"*)
(* arithmetic *)
  "Plus"            ->"plus",
  "Subtract"        ->"subtract",
  "Times"           ->"times",
  "Divide"          ->"divide",
    (*"AddTo"*)
    (*"SubtractFrom"*)
    (*"TimesBy"*)
    (*"DivideBy"*)
(* complex numbers *)
  "Complex"         ->"make_complex",
  "Re"              ->"re",
  "Im"              ->"im",
  "ReIm"            ->"re_im",
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
  "Min"             ->"min",
  "Max"             ->"max",
  "Abs"             ->"abs",
  "Sign"            ->"sign",
  "Clip"            ->"clip",
  "Rescale"         ->"rescale",
  "Chop"            ->"chop",
  "Threshold"       ->"threshold",
  "LogisticSigmoid" ->"logistic_sigmoid",
  "Unitize"         ->"unitize",
  "UnitStep"        ->"unit_step",
  "Ramp"            ->"ramp",
  "Boole"           ->"boole",
  "Less"            ->"less",
  "Greater"         ->"greater",
  "LessEqual"       ->"less_equal",
  "GreaterEqual"    ->"greater_equal",
  "Equal"           ->"equal",
  "Unequal"         ->"unequal",
  "NumericalOrder"  ->"numerical_order",
(* elementary functions *)
  "Log"             ->"log",
  "Log10"           ->"log10",
  "Log2"            ->"log2",
  "Exp"             ->"exp",
  "Power"           ->"power",
  "Sqrt"            ->"sqrt",
  "CubeRoot"        ->"cube_root",
  "Sin"             ->"sin",
  "Cos"             ->"cos",
  "Tan"             ->"tan",
  "Csc"             ->"csc",
  "Sec"             ->"sec",
  "Cot"             ->"cot",
  "Sinc"            ->"sinc",
  "ArcSin"          ->"arcsin",
  "ArcCos"          ->"arccos",
  "ArcTan"          ->"arctan",
  "ArcCsc"          ->"arccsc",
  "ArcSec"          ->"arcsec",
  "ArcCot"          ->"arccot",
  "ArcSin"          ->"arcsin",
  "ArcCosh"         ->"arccosh",
  "ArcTanh"         ->"arctanh",
  "ArcCsch"         ->"arccsch",
  "ArcSech"         ->"arcsech",
  "ArcCoth"         ->"arccoth",
(* random number *)
    (*"RandomInteger"*)
    (*"RandomReal"*)
    (*"RandomComplex"*)
    (*"RandomVariate"*)
  "RandomChoice"    ->"random_choice",
  "SandomSample"    ->"random_sample",
(* array operation *)
    (*"ConstantArray"*)
  "Set"             ->"set",
  "Part"            ->"part",
  "Span"            ->"make_span",
  "Total"           ->"total",
  "Mean"            ->"mean",
  "Range"           ->"range",
(*functional*)
  "Select"          ->"select"
|>;


variablerename[code_]:=
  Module[{renamerules={
      scope[ids_,expr_]:>
        Module[{vars=Table[newvar,Length@ids]},
          AppendTo[$variabletable,AssociationThread[vars->ids]];
          scope[vars,expr/.Thread[(id/@ids)->(var/@vars)]]
        ],
      function[indices_,ids_,types_,expr_]:>
        Module[{vars=Table[newvar,Length@ids]},
          AppendTo[$variabletable,AssociationThread[vars->ids]];
          function[indices,vars,types,expr/.Thread[(id/@ids)->(var/@vars)]]
        ],
      any_:>(Message[semantics::bad,tostring@any];Throw["semantics"])
    }},
    Fold[If[#2=={},Replace[#,renamerules],ReplacePart[#,#2->Replace[Extract[##],renamerules]]]&,
      code,Most/@Reverse@SortBy[Length]@Position[code,function|scope]]
  ]

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

semantics[code_]:=findinit@resolvesymbols@variablerename[code]


listtoseq[expr_]:=Replace[expr,list[any___]:>Sequence[any]]

macro[code_]:=code//.{
    id["ConstantArray"][val_,dims_]:>native["constant_array"][val,vargtag,listtoseq[dims]],
    id["RandomInteger"][spec_,dims_]:>native["random_integer"][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomInteger"][spec_]:>native["random_integer"][listtoseq[spec],vargtag],
    id["RandomReal"][spec_,dims_]:>native["random_integer"][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomReal"][spec_]:>native["random_integer"][listtoseq[spec],vargtag],
    id["RandomComplex"][spec_,dims_]:>native["random_integer"][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomComplex"][spec_]:>native["random_integer"][listtoseq[spec],vargtag],
    id["RandomVariate"][dist_,dims_]:>native["random_variate"][dist,vargtag,listtoseq[dims]],
    id["RandomVariate"][dist_]:>native["random_variate"][dist,vargtag],
    id["Count"][array_,id["PatternTest"][id["Blank"][],func_]]:>native["count"][array,func]
  }


nativename[str_]:=StringRiffle[ToLowerCase@StringCases[str,RegularExpression["[A-Z][a-z]*"]],"_"]

codegen[args[indices_,vars_,types_],___]:=
  If[Length[indices]==0,{},
    Normal@SparseArray[indices->MapThread[#1<>" "<>#2&,{types/.nil->"auto",vars}],Max[indices],"auto"]]

codegen[function[indices_,vars_,types_,sequence[exprs___]],___]:=
  {"[&](",Riffle[Append[codegen[args[indices,vars,types]],"auto..."],", "],")",codegen[sequence[exprs],"Return"]}

codegen[scope[_,sequence[exprs___]],any___]:=codegen[sequence[exprs],any]

codegen[initialize[var_,expr_],___]:={"auto ",codegen[var]," = ",codegen[native["val"][expr]]}

codegen[assign[var_,expr_],___]:=codegen[native["set"][var,expr]]

codegen[literal[s_String],___]:=ToString@CForm[s]<>"_s"
codegen[literal[i_Integer],___]:=ToString@CForm[i]<>"_i"
codegen[literal[r_Real],___]:=ToString@CForm[r]<>"_r"

codegen[native[name_],___]:="wl::"<>name

codegen[vargtag,___]:="wl::varg_tag{}"
codegen[leveltag[l_Integer],___]:="wl::level_tag<"<>ToString@CForm[l]<>">{}"

codegen[clause[type_][func_,{iters___}],___]:={"wl::clause_"<>nativename[type],"(",codegen[func,"Return"],",",Riffle[codegen[#,"Return"]&/@{iters},", "],")"}
codegen[iter[expr___],___]:=codegen[native["iterator"][expr]]
codegen[variter[expr___],___]:=codegen[native["var_iterator"][expr]]

codegen[typed[type_String],___]:=type<>"{}"
codegen[typed["array"[type_,rank_]],___]:="wl::ndarray<"<>type<>", "<>ToString[rank]<>">{}"

codegen[var[name_],___]:=name
codegen[id[name_],___]:=(Message[semantics::undef,name];Throw["semantics"])

codegen[sequence[scope[vars_,expr_]],any___]:=codegen[scope[vars,expr],any]
codegen[sequence[expr___],"Scope"]:={"{",({codegen[#],";"}&/@{expr}),"}"}
codegen[sequence[most___,initialize[var_,expr_]],"Return"]:=codegen[sequence[most,initialize[var,expr],var],"Return"]
codegen[sequence[most___,last_],"Return"]:={"{",({codegen[#],";"}&/@{most}),"return ",codegen[native["val"][last]],";","}"}
codegen[sequence[expr___],"Hold"]:={"[&]",codegen[sequence[expr],"Return"]}
codegen[sequence[expr___],___]:={codegen[sequence[expr],"Hold"],"()"}

codegen[branch[cond_,expr1_,expr2_],___]:=
  codegen[native["branch_if"][cond,expr1,expr2],"Hold"]

codegen[list[any___],___]:=codegen[native["list"][any]]

codegen[head_[args___],any___]:={codegen[head,"Value"],"(",Riffle[codegen[#,any]&/@{args},", "],")"}

codegen[any_,rest___]:=(Message[codegen::bad,tostring[any]];Throw["codegen"])

initcodegen[function[indices_,vars_,types_,expr_]]:=
  Flatten@{"auto main_function(",Riffle[codegen[args[indices,vars,types]],", "],")",codegen[expr,"Return"]}
  
codeformat[segments_List]:=
  StringRiffle[#,{"","\n","\n"}]&@
    FoldPairList[
      Module[{pad=#1-Boole[StringTake[#2,1]=="}"]},
       {If[pad<=0,"",StringRepeat[" ",4pad]]<>#2,pad+Boole[StringTake[#2,-1]=="{"]}]&,
      0,StringJoin/@SplitBy[segments/.{"{"->Sequence[" {","\n"],";"->Sequence[";","\n"]},#=="\n"&][[;;;;2]]
    ]
maincodegen[code_]:=codeformat@initcodegen[code]


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
print[literal[literal_]]:=ToString@CForm@literal
print[list[args___]]:=RowBox[Join[{"{"},Riffle[print/@{args},","],{"}"}]]
print[iter[spec__]]:=RowBox[Join[{"Iterator","["},Riffle[print/@{spec},","],{"]"}]]
print[variter[spec__]]:=RowBox[Join[{"Iterator","["},Riffle[(print/@{spec}),","],{"]"}]]
print[clause[type_][func_,iters_List]]:=RowBox[{type,"[",print[func],",",print[list@@iters],"]"}]
print[mutable[type_][var_,expr_]]:=print@id[type][var,expr]
print[function[indices_,args_,types_,expr_]]:=RowBox[{"Function","[",print[list@@(id/@args)],",",print[expr],"]"}]
print[scope[vars_,expr_]]:=RowBox[{"Module","[",print[list@@(id/@vars)],",",print[expr],"]"}]
print[branch[cond_,true_,false_]]:=print@id["If"][cond,true,false];
print[sequence[exprs__]]:=RowBox[Join[{"("},Riffle[(print/@{exprs}),";"],{")"}]]
print[assign[var_,expr_]]:=RowBox[{print@var,"=",print@expr}]
print[initialize[var_,expr_]]:=RowBox[{print@var,"=",print@expr}]
print[any_]:=any

tostring[any_]:=ToString@DisplayForm[print[any]]


End[];


EndPackage[];
