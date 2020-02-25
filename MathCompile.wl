(* ::Package:: *)

Needs["CCompilerDriver`"];


BeginPackage["MathCompile`"];


CompileToCode::usage="\!\(\*RowBox[{\"CompileToCode\", \"[\", StyleBox[\"func\", \"TI\"], \"]\"}]\) rewrites a Wolfram Language function in C++.";
CompileToBinary::usage="\!\(\*RowBox[{\"CompileToBinary\", \"[\", StyleBox[\"func\", \"TI\"], \"]\"}]\) generates a function compiled as C++.";


CompileToCode
CompileToBinary
LevelTag
Extern
IndirectReturn
CXX


$CppSource="";
$CompilerOutput="";


MCParse
MCSyntax
MCSemantics
MCOptim
MCCodegen
MCLink
MCCxx
MCRuntime


Begin["`Private`"];


CompileToCode[Function[func___]]:=If[#===$Failed,$Failed,toexportcode@#["output"]]&@compile[Hold[Function[func]],True]
CompileToBinary[Function[func___],opts:OptionsPattern[]]:=compilelink[compile[Hold[Function[func]],False],Function[func],opts]
LevelTag[i_Integer]:=Sequence[]
SetAttributes[Extern,HoldFirst];
Extern[f_,type_]:=Unevaluated[f]
IndirectReturn[f_][any___]:=Block[{linkreturn=$Failed},f[any];linkreturn]
IndirectReturn/:LibraryFunctionUnload[IndirectReturn[f_]]:=LibraryFunctionUnload[f]


$packagepath=DirectoryName[$InputFileName];
$slotmaximum=16;
$rankmaximum=16;
$documentlinkbase="https://github.com/njpipeorgan/MathCompile/wiki/";


getlink[page_]:="\!\(\*TemplateBox[{\"\[RightGuillemet]\",\""<>$documentlinkbase<>page<>"\"},\"HyperlinkURL\"]\)"
MCParse::unknown="`1` cannot be parsed.";
MCSyntax::iter="`1` does not have a correct syntax for an iterator.";
MCSyntax::bad="`1` does not have a correct syntax for `2`.";
MCSyntax::badtype="`1` does not specify a correct type.";
MCSyntax::badextern="`1` does not specify a correct extern function.";
MCSyntax::badembedcxx="`1` does not specify correct embedded C++ code.";
MCSyntax::farg="`1` does not have a correct syntax for a function argument.";
MCSyntax::slotmax="#`1` exceeds the maximum value of slot number allowed.";
MCSyntax::fpure="`1` does not have a correct syntax for a pure function.";
MCSyntax::scopevar="`1` does not have a correct syntax for a local variable.";
MCSyntax::badbreak="Break[] cannot be called in `1`.";
MCSyntax::breakloc="Break[] in `1` is not correctly enclosed by a loop.";
MCSemantics::bad="`1` does not have correct semantics.";
MCSemantics::undef="Identifier `1` is not found.";
MCSemantics::noinit="Variable `1` is declared but not initialized.";
MCSemantics::badinit="Variable `1` is initialized in a nested scope.";
MCSemantics::badref="Variable `1` is referenced before initialization.";
MCOptim::elemtype="The types of the elements in the list `1` are not consistent.";
MCOptim::elemdims="The dimensions of the elements in the list `1` are not consistent.";
MCCodegen::bad="Cannot generate code for `1`.";
MCCodegen::notype="One or more arguments of the main function is declared without types.";
MCLink::rettype="Failed to retrieve the return type of the compiled function.";
MCLink::bigrank="The rank of an argument exceeds the maximum rank.";
MCLink::badtype="Cannot infer the type id \"`1`\"returned by the library.";
MCLink::workdir="The working directory does not exist.";
MCLink::libdir="The library directory does not exist.";
MCLink::includes="The additional include files should be specified as a list of strings.";
MCLink::genfail="Failed to generate the library.";
MCLink::noheader="The header file \"math_compile.h\" cannot be found.";
MCCxx::compilerver="An incompatible version of the C++ compiler is used, see MathCompiler wiki for more information.";
MCCxx::support="The compiler `1` is not supported on the platform `2`.";
MCCxx::error="`1`";
MCRuntime::error="`1`";


compile[code_,tocode_]:=
  Module[{parsed,sourceidx,precodegen,output,types,error},
    $variabletable=Association[];
    $kernelfunctiontable=Association[];
    error=Catch[
        $sourceptr=1;
        parsed=parse[code];
        precodegen=alloptim@semantics@allsyntax@parsed;
        types=getargtypes[precodegen];
        If[MemberQ[types,nil],Message[MCCodegen::notype];Throw["codegen"]];
        output=maincodegen[precodegen,tocode];
      ];
    Clear[$variabletable];
    If[error===Null,<|
        "source"->tostring[parsed],"output"->output,"types"->types
      |>,$Failed]
  ]


newid:=SymbolName@Unique["id"]
newvar:=SymbolName@Unique["v"]
newvarpack:=SymbolName@Unique["vp"]
newtype:=SymbolName@Unique["T"]


parse[Hold[head_[args___]]]:=
  Module[{parsehead,arglist,parseargs={}},
    parsehead=parse[Hold[head]];
    arglist=List@@(Hold/@Hold[args]);
    parseargs=($sourceptr++;parse[#])&/@arglist;
    $sourceptr+=If[Length[arglist]==0,2,1];
    parsehead@@parseargs
  ]
parse[Hold[s_Symbol]]:=
  Module[{context=Context[Unevaluated@s],name=SymbolName[Unevaluated@s],fullname,oldptr=$sourceptr},
    If[context=="Global`"||context=="System`"||context=="MathCompile",context=""];
    fullname=context<>name;
    $sourceptr+=StringLength@fullname;
    id[fullname,oldptr]
  ]
parse[Hold[I]]:=
  Module[{oldptr=sourceptr},
    $sourceptr+=1;
    id["I",oldptr]
  ]
parse[Hold[i:(_Integer|_Real|_String)]]:=
  Module[{oldptr=$sourceptr},
    $sourceptr+=StringLength@ToString@CForm[i];
    literal[i,oldptr]
  ]
parse[Hold[any_]]:=(Message[MCParse::unknown,ToString[Unevaluated[any]]];Throw["lexical"])


$typenames={
  (*informal names*)
  {"int64_t","MachineInteger"},
  {"uint8_t","Byte"},
  {"wl::complex<float>","Complex64"},
  {"wl::complex<double>","Complex128"},
  {"wl::string","TerminatedString"},
  (*formal names*)
  {"wl::void_type", "Void"},
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
totypespec[ndarray[type_/;(type!="Void"&&istypename[type]),rank_Integer/;rank>=1]]:="array"[totypespec[type],rank]
totypespec[({args___}->ret_)/;AllTrue[{args,ret},istypename]]:="func"[(totypespec/@{args}),totypespec@ret]

totypename[any___]:=Missing[]
totypespec[any___]:=Missing[]

istypename[name_]:=!MissingQ@totypespec[name]
istypespec[spec_]:=!MissingQ@totypename[spec]


syntax[list][code_]:=code//.{
    id["List",p_][exprs___]:>list[p][exprs]
  }

syntax[type][code_]:=code//.{
    id["Typed",p0_][id[arg_,p_],type_]:>typed[p0][id[arg,p],typespec[type]],
    id["Typed",p0_][type_]:>typed[p0][typespec[type]]
  }//.{
    typespec[type_]:>typespec[type//.{
        (literal|id)[t_,_]:>t,
        list[_][t_,r_Integer]:>ndarray[t,r],
        id["Rule",_][list[_][args___],ret_]:>({args}->ret),
        id["Rule",_][arg_,ret_]:>({arg}->ret)
      }]
  }//.{
    typespec[type_]:>If[istypename[type],totypespec[type],typespec[type]]
  }/.{
    id["Typed",_][any___]:>(Message[MCSyntax::badtype,tostring@id["Typed",0][any]];Throw["syntax"]),
    typespec[any___]:>(Message[MCSyntax::badtype,tostring@id["Typed",0][any]];Throw["syntax"])
  }

syntax[extern][code_]:=code//.{
    id["MathCompile`Extern",p_][id[f_,fp_],type:typed[_][_]]:>
      Module[{fstr="Hold["<>f<>"]",fidx},
        If[MissingQ[fidx=$kernelfunctiontable[fstr]],
          fidx=Length@$kernelfunctiontable;AppendTo[$kernelfunctiontable,fstr->fidx]];
        native["librarylink::extern_function",p][literal[fidx,fp],type]
      ]
  }/.{
    id["MathCompile`Extern",_][any___]:>(Message[MCSyntax::badextern,tostring@id["Extern",0][any]];Throw["syntax"])
  }

syntax[embedcxx][code_]:=code//.{
    id["MathCompile`CXX",p_][literal[cxx_String,_]]:>
      Module[{split=StringSplit[cxx,"`"~~v:(Except["`"]..)~~"`":>v,All]},
        embedcxx[p][If[Length@split==1,{},id[#,p]&/@split[[2;;-2;;2]]],split[[1;;-1;;2]]]
      ]
  }/.{
    id["MathCompile`CXX",_][any___]:>(Message[MCSyntax::badembedcxx,tostring@id["CXX",0][any]];Throw["syntax"])
  }

syntax[clause][code_]:=code//.{
    id[type:("Table"|"Do"|"Sum"|"Product"),p0_][expr_,iters__]:>(
      clause[type,p0][id["Function",p0][list[0]@@DeleteCases[#[[;;,1]],nil],expr],#[[;;,2]]]&@
        Replace[{iters},{
            e:Except[list[_]][___]:>{nil,native["iterator",0][e]},
            list[p1_][e_]:>{nil,native["iterator",p1][e]},
            list[p1_][id[var_,p2_],spec:Repeated[_,3]]:>{id[var,p2],native["var_iterator",p1][spec]},
            any_:>(Message[MCSyntax::iter,tostring[any]];Throw["syntax"])
          },{1}])
  }/.{
    any:id[type:("Table"|"Do"|"Sum"|"Product"),_][___]:>
      (Message[MCSyntax::bad,tostring[any],type];Throw["syntax"])
  }

syntax[mutable][code_]:=code//.{
    id["AddTo",p0_][target:id[_,_],expr_]:>native["add_to",p0][target,expr],
    id["SubtractFrom",p0_][target:id[_,_],expr_]:>native["subtract_from",p0][target,expr],
    id["TimesBy",p0_][target:id[_,_],expr_]:>native["times_by",p0][target,expr],
    id["DivideBy",p0_][target:id[_,_],expr_]:>native["divide_by",p0][target,expr],
    id["Increment",p0_][target:id[_,_]]:>native["increment",p0][target],
    id["Decrement",p0_][target:id[_,_]]:>native["decrement",p0][target],
    id["PreIncrement",p0_][target:id[_,_]]:>native["pre_increment",p0][target],
    id["PreDecrement",p0_][target:id[_,_]]:>native["pre_decrement",p0][target],
    id["AddTo",p0_][target:id["Part",_][id[_,_],___],expr_]:>native["add_to",p0][target,expr],
    id["SubtractFrom",p0_][target:id["Part",_][id[_,_],___],expr_]:>native["subtract_from",p0][target,expr],
    id["TimesBy",p0_][target:id["Part",_][id[_,_],___],expr_]:>native["times_by",p0][target,expr],
    id["DivideBy",p0_][target:id["Part",_][id[_,_],___],expr_]:>native["divide_by",p0][target,expr],
    id["Increment",p0_][target:id["Part",_][id[_,_],___]]:>native["increment",p0][target],
    id["Decrement",p0_][target:id["Part",_][id[_,_],___]]:>native["decrement",p0][target],
    id["PreIncrement",p0_][target:id["Part",_][id[_,_],___]]:>native["pre_increment",p0][target],
    id["PreDecrement",p0_][target:id["Part",_][id[_,_],___]]:>native["pre_decrement",p0][target],
    id["AppendTo",p0_][target:id[_,_],expr_]:>native["append_to",p0][target,expr],
    id["PrependTo",p0_][target:id[_,_],expr_]:>native["prepend_to",p0][target,expr]
  }/.{
    any:id[type:("AddTo"|"SubtractFrom"|"TimesBy"|"DivideBy"|"AppendTo"|"PrependTo"),_][___]:>
      (Message[MCSyntax::bad,tostring[any],type];Throw["syntax"])
  }

syntax[function][code_]:=
  Module[{
    functionrules={
      id["Function",p0_][args_,expr_]:>(
        function[p0][
          id/@#[[;;,1]],   (* argument names *)
          #[[;;,2]],       (* argument types *)
          sequence[expr]    (* function body *)
          ]&@Replace[If[MatchQ[Head[args],list[_]],List@@args,{args}],{
              id[arg_,_]:>{arg,nil},
              typed[_][id[arg_,_],type_]:>{arg,type},
              any_:>(Message[MCSyntax::farg,tostring[any]];Throw["syntax"])
            },{1}]),
      id["Function",p0_][pure_]:>
        Module[{slots,slotsrule,slotspos},
          slots=Union@Cases[pure,
            s:id[type:("Slot"|"SlotSequence"),_][literal[i_Integer/;i>0,_]]:>
              (s->If[type=="Slot",i,slotseq[i]]),
            {0,Infinity},Heads->True];
          slotsrule=Module[{i=newid,nvar,names},
            nvar=Max[slots[[;;,2]]/.slotseq->Sequence,0]+1;
            If[nvar>$slotmaximum,(Message[MCSyntax::slotmax,nvar-1];Throw["syntax"])];
            names=MapAt[pack[#]&,arg[i,#]&/@Range[nvar],{-1,2}];
            (#1->Sequence@@Replace[#2,{slotseq[i_]:>names[[i;;]],i_:>names[[i;;i]]}]&)@@@slots];
          slotspos=(#[[2]]/.pack[i_]:>i)->#&/@Union[slotsrule[[;;,2]]];
          function[p0][
            ReplacePart[Table[nil,Max[slotspos[[;;,1]],0]],slotspos],
            Table[nil,Max[slotspos[[;;,1]],0]],
            sequence[pure/.slotsrule]
          ]
        ],
      any:id["Function",_][___]:>(Message[MCSyntax::bad,tostring[any],"Function"];Throw["syntax"])
    }},
    Fold[
      If[#2=={},Replace[#,functionrules],ReplacePart[#,#2->Replace[Extract[##],functionrules]]]&,
      code,
      Reverse@SortBy[Length]@Position[code,id["Function",_][___]]
    ]
  ]

syntax[scope][code_]:=code//.{
    id["Module",p0_][list[_][vars___],expr_]:>(
      scope[p0][#[[;;,1]],sequence[sequence@@
        Cases[#,{var_,p1_,p2_,init:Except[nil]}:>id["Set",p1][id[var,p2],init],{1}],expr]]&@
        Replace[{vars},{
          id[var_,p2_]:>{var,0,p2,nil},
          id["Set",p1_][id[var_,p2_],init_]:>{var,p1,p2,init},
          any_:>(Message[MCSyntax::scopevar,tostring[any]];Throw["syntax"])
        },{1}])
  }/.{
    any:id["Module",_][___]:>(Message[MCSyntax::bad,tostring[any],"Module"];Throw["syntax"])
  }

syntax[branch][code_]:=code//.{
    id["If",p0_][cond_,true_,false_]:>branchif[p0][cond,sequence@true,sequence@false],
    id["If",p0_][cond_,true_]:>branchif[p0][cond,sequence@true,sequence@id["Null",0]](*,
    id["Which",p0_][any__/;EvenQ@Length@{any}]:>
      branchwhich[p0][native["_which_conditions"]@@(sequence/@{any}[[;;;;2]]),
        Sequence@@(sequence/@{any}[[2;;;;2]])]*)
  }/.{
    any:id["If",_][___]:>(Message[MCSyntax::bad,tostring[any],"If"];Throw["syntax"]),
    any:id["Which",_][___]:>(Message[MCSyntax::bad,tostring[any],"Which"];Throw["syntax"])
  }
  
syntax[loop][code_]:=code//.{
    id["For",p0_][start_,test_,incr_,body_]:>sequence[start,loopfor[p0][sequence@test,sequence@incr,sequence@body]],
    id["For",p0_][start_,test_,incr_]:>sequence[start,loopfor[p0][sequence@test,sequence@incr,sequence[]]],
    id["While",p0_][test_,body_]:>loopwhile[p0][sequence@test,sequence@body],
    id["While",p0_][test_]:>loopwhile[p0][sequence@test,sequence[]]
  }/.{
    any:id["For",_][___]:>(Message[MCSyntax::bad,tostring[any],"For"];Throw["syntax"]),
    any:id["While",_][___]:>(Message[MCSyntax::bad,tostring[any],"While"];Throw["syntax"])
  }

syntax[sequence][code_]:=code//.{
    id["CompoundExpression",_][exprs__]:>sequence[exprs]}//.{
    sequence[before___,sequence[exprs___],after___]:>sequence[before,exprs,after]}/.{
    sequence[]:>sequence[id["Null",0]]
  }

syntax[assign][code_]:=code//.{
    id["Set",p0_][target:id[_,_],expr_]:>assign[p0][target,expr],
    id["Set",p0_][target:id["Part",_][id[_,_],___],expr_]:>assign[p0][target,expr]
  }/.{
    any:id["Set",_][___]:>(Message[MCSyntax::bad,tostring[any],"Set"];Throw["syntax"])
  }

syntax[loopbreak][code_]:=Module[{heads,headspos,breakpos},
    Do[
      headspos=Table[Append[p[[;;i]],0],{i,Length@p-1}];
      heads=Extract[code,headspos];
      If[Last@heads=!=sequence,
        Message[MCSyntax::badbreak,tostring@Extract[code,Most@p]];Throw["syntax"],
        breakpos=Join[
          Select[Extract[headspos,Position[heads,clause["Do",_]]],
            ReplacePart[#,-1->1(*body of Do*)]==p[[;;Length@#]]&],
          Select[Extract[headspos,Position[heads,loopfor[_]]],
            ReplacePart[#,-1->3(*body of For*)]==p[[;;Length@#]]&],
          Select[Extract[headspos,Position[heads,loopwhile[_]]],
            ReplacePart[#,-1->2(*body of While*)]==p[[;;Length@#]]&]];
        If[Length@breakpos==0,
          Message[MCSyntax::breakloc,tostring@Extract[code,Most@p]];Throw["syntax"]]
      ]
    ,{p,Position[code,id["Break",_][]]}];
    code//.{
        id["Break",_][]:>break[]
      }//.{
        sequence[any___,break[]]:>sequence[any,break[],id["Null",0]]
      }
  ]

$syntaxpasses={list,type,extern,embedcxx,clause,mutable,function,scope,branch,loop,sequence,assign,loopbreak};
allsyntax[code_]:=Fold[syntax[#2][#1]&,code,$syntaxpasses];


$builtinconstants=
"wl::const_"<>#&/@<|
  "Null"       ->"null",
  "I"          ->"i",
  "All"        ->"all",
  "True"       ->"true",
  "False"      ->"false",
  "Integer"    ->"integer",
  "Real"       ->"real",
  "Complex"    ->"complex",
  "String"     ->"string",
  "List"       ->"list",
  "Pi"         ->"pi",
  "E"          ->"e",
  "Degree"     ->"degree",
  "GoldenRatio"->"golden_ratio",
  "GoldenAngle"->"golden_angle",
  "EulerGamma" ->"euler_gamma",
  "Catalan"    ->"calatan",
  "Glaisher"   ->"glaisher",
  "Khinchin"   ->"khinchin",
  (* string patterns *)
  "Whitespace"          ->"whitespace",
  "NumberString"        ->"number_string",
  "WordCharacter"       ->"word_character",
  "LetterCharacter"     ->"letter_character",
  "DigitCharacter"      ->"digit_character",
  "HexadecimalCharacter"->"hexadecimal_character",
  "WhitespaceCharacter" ->"whitespace_character",
  "PunctuationCharacter"->"punctuation_character",
  "WordBoundary"        ->"word_boundary",
  "StartOfLine"         ->"start_of_line",
  "EndOfLine"           ->"end_of_line",
  "StartOfString"       ->"start_of_string",
  "EndOfString"         ->"end_of_string",
  "Byte"                ->"byte",
  "Character"           ->"character",
  "Word"                ->"word",
  "Record"              ->"record"
|>;

$builtinfunctions=
<|
(* scope *)
    (*"Module"*)
(* control flow *)
  (*"If"              ->"native_if",*)
    (*"Do"*)
    (*"Table"*)
    (*"Sum"*)
    (*"Product"*)
  "Pause"           ->"pause",
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
  "Quotient"        ->"quotient",
  "IntegerDigits"   ->"integer_digits",
  "Less"            ->"less",
  "Greater"         ->"greater",
  "LessEqual"       ->"less_equal",
  "GreaterEqual"    ->"greater_equal",
  "Equal"           ->"equal",
  "Unequal"         ->"unequal",
  "SameQ"           ->"same_q",
  "UnsameQ"         ->"unsame_q",
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
  "Threshold"       ->"threshold",
  "NumericalOrder"  ->"numerical_order",*)
(* integral functions *)
  "EvenQ"           ->"even_q",
  "OddQ"            ->"odd_q",
  "Divisible"       ->"divisible",
  "Fibonacci"       ->"fibonacci",
  "LucasL"          ->"lucas_l",
  "Factorial"       ->"factorial",
  "Factorial2"      ->"factorial2",
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
  "BitShiftLeft"    ->"bit_shift_left",
  "BitShiftRight"   ->"bit_shift_right",
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
  "ChiSquareDistribution"->"chi_square_distribution",
  "UniformDistribution"->"uniform_distribution",
  "NormalDistribution"->"normal_distribution",
  "LogNormalDistribution"->"log_normal_distribution",
  "CauchyDistribution"->"cauchy_distribution",
  "StudentTDistribution"->"student_t_distribution",
  "FRatioDistribution"->"f_ratio_distribution",
  "ExponentialDistribution"->"exponential_distribution",
  "PoissonDistribution"->"poisson_distribution",
  "BernoulliDistribution"->"bernoulli_distribution",
  "GammaDistribution"->"gamma_distribution",
  "WeibullDistribution"->"weibull_distribution",
  "ExtremeValueDistribution"->"extreme_value_distribution",
  "GeometricDistribution"->"geometric_distribution",
  "BinomialDistribution"->"binomial_distribution",
  "NegativeBinomialDistribution"->"negative_binomial_distribution",
    (*"RandomInteger"*)
    (*"RandomReal"*)
    (*"RandomComplex"*)
    (*"RandomVariate"*)
  "RandomChoice"    ->"random_choice",
  "RandomSample"    ->"random_sample",
(* array operation *)
    (*"ConstantArray"*)
  "List"            ->"list",
  "Set"             ->"set",
  "Dimensions"      ->"dimensions",
  "Length"          ->"length",
  "ArrayDepth"      ->"array_depth",
  "Part"            ->"part",
  "Partition"       ->"partition",
  "VectorQ"         ->"vector_q",
  "MatrixQ"         ->"matrix_q",
  "Span"            ->"make_span",
  "Range"           ->"range",
  "Reverse"         ->"reverse",
  "RotateLeft"      ->"rotate_left",
  "RotateRight"     ->"rotate_right",
  "First"           ->"first",
  "Last"            ->"last",
  "Most"            ->"most",
  "Rest"            ->"rest",
  "Take"            ->"take",
  "Drop"            ->"drop",
  "Join"            ->"join",
  "Transpose"       ->"transpose",
  "ConjugateTranspose"->"conjugate_transpose",
  "Flatten"         ->"flatten",
  "Order"           ->"order",
  "Ordering"        ->"ordering",
  "OrderedQ"        ->"ordered_q",
  "Sort"            ->"sort",
  "Append"          ->"append",
  "Prepend"         ->"prepend",
  "Insert"          ->"insert",
  "Delete"          ->"delete_",
  "Union"           ->"set_union",
  "Position"        ->"position",
  "Cases"           ->"cases",
  "DeleteCases"     ->"delete_cases",
  "MemberQ"         ->"member_q",
  "FreeQ"           ->"free_q",
  "Normal"          ->"normal",
(*array math operation*)
  "Total"           ->"total",
  "Mean"            ->"mean",
  "Dot"             ->"dot",
  "Inner"           ->"inner",
  "Outer"           ->"outer",
  "Tr"              ->"tr",
  "Accumulate"      ->"accumulate",
  "Differences"     ->"differences",
  "Ratios"          ->"ratios",
  "Norm"            ->"norm",
  "Normalize"       ->"normalize",
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
  "NoneTrue"        ->"none_true",
  "Count"           ->"count",
(* patterns *)
  "Blank"           ->"blank",
  "BlankSequence"   ->"blank_sequence",
  "BlankNullSequence"->"blank_null_sequence",
  "Alternatives"    ->"alternatives",
  "Repeated"        ->"repeated",
  "RepeatedNull"    ->"repeated_null",
  "Except"          ->"except",
  "Longest"         ->"longest",
  "Shortest"        ->"shortest",
  "Rule"            ->"rule",
  "RuleDelayed"     ->"rule_delayed",
  "Condition"       ->"condition",
(* string *)
  "StringJoin"      ->"string_join",
  "StringLength"    ->"string_length",
  "StringTake"      ->"string_take",
  "RegularExpression"->"regular_expression",
  "StringExpression"->"string_expression",
  "StringCases"     ->"string_cases",
  "StringReplace"   ->"string_replace",
  "StringCount"     ->"string_count",
  "StringMatchQ"    ->"string_match_q",
  "StringContainsQ" ->"string_contains_q",
  "StringFreeQ"     ->"string_free_q",
  "StringStartsQ"   ->"string_starts_q",
  "StringEndsQ"     ->"string_ends_q",
  "StringSplit"     ->"string_split",
  "StringPosition"  ->"string_position",
  "StringRiffle"    ->"string_riffle",
  "StringPattern`PatternConvert"->"_pattern_convert",
  "LetterQ"         ->"letter_q",
  "DigitQ"          ->"digit_q",
  "UpperCaseQ"      ->"upper_case_q",
  "LowerCaseQ"      ->"lower_case_q",
  "PrintableASCIIQ" ->"printable_ascii_q",
  "Characters"      ->"characters",
  "CharacterRange"  ->"character_range",
  "ToCharacterCode" ->"to_character_code",
  "FromCharacterCode"->"from_character_code",
  "ToString"        ->"to_string",
(* io *)
  "Directory"       ->"directory",
  "Print"           ->"print",
  "Echo"            ->"echo",
  "EchoFunction"    ->"echo_function",
  "OpenRead"        ->"open_read",
  "OpenWrite"       ->"open_write",
  "OpenAppend"      ->"open_append",
  "StreamPosition"  ->"stream_position",
  "SetStreamPosition"->"set_stream_position",
  "Read"            ->"read",
  "ReadLine"        ->"read_line",
  "ReadString"      ->"read_string",
  "ReadList"        ->"read_list",
  "Write"           ->"write",
  "WriteLine"       ->"write_line",
  "WriteString"     ->"write_string",
  "BinaryWrite"     ->"binary_write"
|>;


headerseries[expr_,pos_]:=Table[Extract[expr,ReplacePart[Take[pos,n],-1->0]],{n,Length@pos}]

variablerename[code_]:=
  Module[{renamerules={
      scope[p0_][ids_,expr_]:>
        Module[{vars=Table[newvar,Length@ids]},
          AppendTo[$variabletable,AssociationThread[vars->ids]];
          scope[p0][vars,expr/.Thread[(id[#,p_]&/@ids)->(var[#,p]&/@vars)]]
        ],
      function[p0_][ids_,types_,expr_]:>
        Module[{vars},
          vars=If[MatchQ[#,arg[_,pack[_]]],newvarpack,newvar]&/@ids;
          (*AppendTo[$variabletable,AssociationThread[vars->ids]];*)
          function[p0][vars,types,
            expr/.MapThread[#1->If[
              (Length[#]==1&&Count[headerseries[expr,#[[1]]],function]==0)&@
                Position[expr,#1],movvar,var][#2]&,{ids/.id[n_]:>id[n,_],vars}],
            Sequence@@If[MemberQ[ids,arg[_,pack[_]]],
              With[{var=newvar},{variadic[{var},{nil},expr/.{arg[_,i_]:>argv[var,i]}]}],
              {}]
          ]
        ],
      any_:>(Message[MCSemantics::bad,tostring@any];Throw["semantics"])
    }},
    Fold[If[#2=={},Replace[#,renamerules],ReplacePart[#,#2->Replace[Extract[##],renamerules]]]&,
      code,Most/@Reverse@SortBy[Length]@Position[code,function[_]|scope[_]]]
  ]

listtoseq[expr_]:=Replace[expr,list[_][any___]:>Sequence[any]]
parsedatatype[literal[type_String,p_]]:=typed[p][totypespec[type]/.Missing[]:>"wl::void_type"]
parsefileformat[literal[format_String,p_]]:=If[MemberQ[{"Table","TSV","CSV"},format],
  const["wl::file_format::"<>format,p],literal[format,p]]

functionmacro[code_]:=code//.{
    id["ConstantArray",p_][val_,dims_]:>native["constant_array",p][val,vargtag,listtoseq[dims]],
    id["RandomInteger",p_][spec_,dims_]:>native["random_integer",p][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomInteger",p_][spec_]:>native["random_integer",p][listtoseq[spec],vargtag],
    id["RandomInteger",p_][]:>native["random_integer",p][literal[1,0],vargtag],
    id["RandomReal",p_][spec_,dims_]:>native["random_real",p][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomReal",p_][spec_]:>native["random_real",p][listtoseq[spec],vargtag],
    id["RandomReal",p_][]:>native["random_real",p][literal[1,0],vargtag],
    id["RandomComplex",p_][spec_,dims_]:>native["random_complex",p][listtoseq[spec],vargtag,listtoseq[dims]],
    id["RandomComplex",p_][spec_]:>native["random_complex",p][listtoseq[spec],vargtag],
    id["RandomComplex",p_][]:>native["random_complex",p][id["Complex"][literal[1,0],literal[1,0]],vargtag],
    id["RandomVariate",p_][dist_,dims_]:>native["random_variate",p][dist,vargtag,listtoseq[dims]],
    id["RandomVariate",p_][dist_]:>native["random_variate",p][dist,vargtag],
    id["RandomChoice",p_][id["Rule",_][weights_,array_],dims_]:>native["random_choice",p][weights,array,vargtag,listtoseq[dims]],
    id["RandomChoice",p_][id["Rule",_][weights_,array_]]:>native["random_choice",p][weights,array,vargtag],
    id["RandomChoice",p_][array_,dims_]:>native["random_choice",p][array,vargtag,listtoseq[dims]],
    id["RandomSample",p_][id["Rule",_][weights_,array_],dims_]:>native["random_sample",p][weights,array,vargtag,dims],
    id["Count",p_][array_,id["PatternTest",_][id["Blank",_][],func_]]:>native["count",p][array,vargtag,func],
    id["Count",p_][array_,id["PatternTest",_][id["Blank",_][],func_],list[_][literal[i_Integer,_]]]:>native["count",p][array,vargtag,func,const[i]],
    id["Count",p_][array_,patt_,list[_][literal[i_Integer,_]]]:>native["count",p][array,patt,const[i]],
    id["Total",p_][array_,literal[i_Integer,_]]:>native["total",p][array,const[i]],
    id["Total",p_][array_,id["Infinity",_]]:>native["total",p][array,const[1],const[-1]],
    id["Total",p_][array_,list[_][literal[i_Integer,_]]]:>native["total",p][array,const[i],const[i]],
    id["Total",p_][array_,list[_][literal[i1_Integer,_],literal[i2_Integer,_]]]:>native["total",p][array,const[i1],const[i2]],
    id["Clip",p_][any_,list[_][min_,max_]]:>native["clip",p][any,vargtag,min,max],
    id["Clip",p_][any_,list[_][min_,max_],list[_][vmin_,vmax_]]:>native["clip",p][any,vargtag,min,max,vmin,vmax],
    id["Map",p_][func_,array_,list[_][literal[i_Integer,_]]]:>native["map",p][func,array,const[i]],
    id["Scan",p_][func_,array_,list[_][literal[i_Integer,_]]]:>native["scan",p][func,array,const[i]],
    id["Reverse",p_][array_,literal[i_Integer,_]]:>native["reverse",p][array,const[i]],
    id["ArrayReshape",p_][array_,dims_]:>native["array_reshape",p][array,vargtag,listtoseq[dims]],
    id["ArrayReshape",p_][array_,dims_,padding_]:>native["array_reshape",p][array,padding,vargtag,listtoseq[dims]],
    id["Fold",p_][func_,x_,id["Reverse",_][y_]]:>native["foldr",p][func,x,y],
    id["Fold",p_][func_,id["Reverse",_][y_]]:>native["foldr",p][func,y],
    id["FoldList",p_][func_,x_,id["Reverse",_][y_]]:>native["foldr_list",p][func,x,y],
    id["FoldList",p_][func_,id["Reverse",_][y_]]:>native["foldr_list",p][func,y],
    id["Apply",p_][func_,list[_][args___]]:>func[args],
    id["Apply",p_][func_,array_,list[_][literal[i_Integer,_]]]:>native["apply",p][func,array,const[i]],
    id["MapThread",p_][func_,list[_][arrays__]]:>native["map_thread",p][func,vargtag,arrays],
    id["MapThread",p_][func_,list[_][arrays__],literal[i_Integer,_]]:>native["map_thread",p][func,const[i],vargtag,arrays],
    id["MapThread",p_][func_,array_,literal[i_Integer,_]]:>native["map_thread",p][func,array,const[i]],
    id["Part",p_][array_,specs___]:>native["part",p][array,
      Sequence@@Replace[{specs},literal[i_Integer/;i>0]:>native["cidx",0][literal[i-1,0]],{1}]],
    id["Transpose",p_][array_,list[_][l:(literal[_Integer,_]..)]]:>native["transpose",p][array,Sequence@@(const/@{l}[[;;,1]])],
    id["Flatten",p_][array_,literal[i_Integer,_]]:>
      native["flatten",p][array,
        If[0<=i<$rankmaximum,consts@@Range[i+1],
          (Message[MCSemantics::bad,tostring@id["Flatten"][array,literal[i,0]]];Throw["semantics"])]],
    id["Flatten",p_][array_,l:list[_][literal[_Integer,_]..]]:>id["Flatten",p][array,list[l]],
    id["Flatten",p_][array_,l:list[_][list[_][literal[_Integer,_]..]..]]:>
      Module[{levels=l/.{list[_]->List,literal->(#1&)},ints,maxlevel,out},
        ints=Cases[levels,_Integer,-1];
        If[1<=Min[ints]&&(maxlevel=Max[ints])<=$rankmaximum&&DuplicateFreeQ[ints],
        out=Join[levels,List/@Complement[Range[maxlevel],ints]];
        native[If[#==Range[Length@#]&@Flatten[out],"flatten_copy","flatten"],p][
          array,Sequence@@(consts@@@out)],
        (Message[MCSemantics::bad,tostring@id["Flatten",p][array,l]];Throw["semantics"])]
      ],
    id["Outer",p_][f_,args___]:>native["outer",p][f,Sequence@@Replace[{args},literal[i_Integer,_]:>const[i],1]],
    id["Composition",p_][funcs__][args___]:>First@Fold[{#2@@#1}&,{args},Reverse@{funcs}],
    id["RightComposition",p_][funcs__][args___]:>First@Fold[{#2@@#1}&,{args},{funcs}],
    id["Composition",p_][]:>native["identity",p],
    id["RightComposition",p_][]:>native["identity",p],
    id["NestWhile",p_][func_,expr_,test_,literal[i_Integer,_],id["Infinity",_],any___]:>
      native["nest_while",p][func,expr,test,const[i],const["int_infinity"],any],
    id["NestWhileList",p_][func_,expr_,test_,literal[i_Integer,_],id["Infinity",_],any___]:>
      native["nest_while_list",p][func,expr,test,const[i],const["int_infinity"],any],
    id["NestWhile",p_][func_,expr_,test_,literal[i_Integer,_],any___]:>
      native["nest_while",p][func,expr,test,const[i],any],
    id["NestWhileList",p_][func_,expr_,test_,literal[i_Integer,_],any___]:>
      native["nest_while_list",p][func,expr,test,const[i],any],
    id["FixedPoint",p_][any___,id["Rule",_][id["SameTest",_],pred_]]:>native["fixed_point",p][any,vargtag,pred],
    id["FixedPointList",p_][any___,id["Rule",_][id["SameTest",_],pred_]]:>native["fixed_point_list",p][any,vargtag,pred],
    id["AllTrue",p_][array_,test_,literal[i_Integer,_]]:>native["all_true",p][array,test,const[i]],
    id["AnyTrue",p_][array_,test_,literal[i_Integer,_]]:>native["any_true",p][array,test,const[i]],
    id["NoneTrue",p_][array_,test_,literal[i_Integer,_]]:>native["none_true",p][array,test,const[i]],
    id["Join",p_][any__,literal[i_Integer,_]]:>native["join",p][const[i],any],
    id["Position",p_][any_,id["PatternTest",_][id["Blank",_][],func_],list[_][literal[i_Integer,_]]]:>
      native["position",p][any,vargtag,func,const[i]],
    id["Position",p_][any_,patt_,list[_][literal[i_Integer,_]]]:>native["position",p][any,patt,const[i]],
    id["Cases",p_][any_,id["PatternTest",_][id["Blank",_][],func_],list[_][literal[i_Integer,_]]]:>
      native["cases",p][any,vargtag,func,const[i]],
    id["Cases",p_][any_,patt_,list[_][literal[i_Integer,_]]]:>native["cases",p][any,patt,const[i]],
    id["DeleteCases",p_][any_,id["PatternTest",_][id["Blank",_][],func_]]:>
      native["delete_cases",p][any,vargtag,func],
    id["DeleteCases",p_][any_,id["PatternTest",_][id["Blank",_][],func_],list[_][literal[i_Integer,_]]]:>
      native["delete_cases",p][any,vargtag,func,const[i]],
    id["DeleteCases",p_][any_,patt_,list[_][literal[i_Integer,_]]]:>native["delete_cases",p][any,patt,const[i]],
    id["MemberQ",p_][any_,id["PatternTest",_][id["Blank",_][],func_],list[_][literal[i_Integer,_]]]:>
      native["member_q",p][any,vargtag,func,const[i]],
    id["MemberQ",p_][any_,patt_,list[_][literal[i_Integer,_]]]:>native["member_q",p][any,patt,const[i]],
    id["FreeQ",p_][any_,id["PatternTest",_][id["Blank",_][],func_],list[_][literal[i_Integer,_]]]:>
      native["free_q",p][any,vargtag,func,const[i]],
    id["FreeQ",p_][any_,patt_,list[_][literal[i_Integer,_]]]:>native["free_q",p][any,patt,const[i]],
    id["Tr",p_][array_,f_,literal[i_Integer,_]]:>native["tr",p][array,f,const[i]],
    id["Norm",p_][array_,literal[i_Integer,_]]:>native["norm",p][array,const[i]],
    id["Partition",p_][array_,id["LevelTag",_][literal[i_Integer,_]],n_,d_]:>native["partition",p][array,const[i],n,d],
    id["ReplacePart",p_][array_,id["Rule",_][pos_,new_]]:>native["replace_part",p][array,pos,new],
    id["StringCount",p_][str_,patt_,id["Rule",_][id["Overlaps",_],id["True",_]]]:>native["string_count<true>",p][str,patt],
    id["StringCount",p_][str_,patt_,id["Rule",_][id["Overlaps",_],id["False",_]]]:>native["string_count<false>",p][str,patt],
    id["StringCases",p_][str_,patt_,id["Rule",_][id["Overlaps",_],id["True",_]]]:>native["string_cases<true>",p][str,patt],
    id["StringCases",p_][str_,patt_,id["Rule",_][id["Overlaps",_],id["False",_]]]:>native["string_cases<false>",p][str,patt],
    id["StringPosition",p_][str_,patt_,id["Rule",_][id["Overlaps",_],id["True",_]]]:>native["string_position<true>",p][str,patt],
    id["StringPosition",p_][str_,patt_,id["Rule",_][id["Overlaps",_],id["False",_]]]:>native["string_position<false>",p][str,patt],
    id["OpenRead",p_][str_,id["Rule",_][id["BinaryFormat",_],id["True",_]]]:>native["open_read<true>",p][str],
    id["OpenRead",p_][str_,id["Rule",_][id["BinaryFormat",_],id["False",_]]]:>native["open_read<false>",p][str],
    id["OpenWrite",p_][str_,id["Rule",_][id["BinaryFormat",_],id["True",_]]]:>native["open_write<true>",p][str],
    id["OpenWrite",p_][str_,id["Rule",_][id["BinaryFormat",_],id["False",_]]]:>native["open_write<false>",p][str],
    id["OpenAppend",p_][str_,id["Rule",_][id["BinaryFormat",_],id["True",_]]]:>native["open_append<true>",p][str],
    id["OpenAppend",p_][str_,id["Rule",_][id["BinaryFormat",_],id["False",_]]]:>native["open_append<false>",p][str],
    id["BinaryRead",p_][str_,type:literal[_String,_]]:>native["binary_read",p][str,parsedatatype[type]],
    id["BinaryReadList",p_][str_,type:literal[_String,_],any___]:>native["binary_read_list",p][str,parsedatatype[type],any],
    id["BinaryWrite",p_][str_,x_,type:literal[_String,_]]:>native["binary_write",p][str,x,parsedatatype[type]],
    id["Import",p_][path_,any_,id["Rule",_][id["Padding",_],val_]]:>id["Import",p][path,any,val],
    id["Import",p_][path_,list[_][literal["Binary",_],type:literal[_String,_]],any:Except[id["Rule",_][___]]]:>
      native["import_binary",p][path,parsedatatype[type],any],
    id["Import",p_][path_,list[_][format:literal[_String,_],type:literal[_String,_]],any:Except[id["Rule",_][___]]]:>
      native["import_text",p][path,parsefileformat[format],parsedatatype[type],any],
    id["Import",p_][path_,list[_][literal["Binary",_],type:literal[_String,_]]]:>
      native["import_binary",p][path,parsedatatype[type]],
    id["Import",p_][path_,list[_][format:literal[_String,_],type:literal[_String,_]]]:>
      native["import_text",p][path,parsefileformat[format],parsedatatype[type]],
    id["Export",p_][path_,x_,list[_][literal["Binary",_],type:literal[_String,_]]]:>
      native["export_binary",p][path,x,parsedatatype[type]],
    id["Export",p_][path_,x_,list[_][format:literal[_String,_],type:literal[_String,_]]]:>
      native["export_text",p][path,x,parsefileformat[format],parsedatatype[type]],
    id["Export",p_][path_,x_,format:literal[_String,fp_]]:>id["Export",p][path,x,list[fp][format,literal["Void",0]]]
  }

arithmeticmacro[code_]:=code//.{
    id["Plus",p_][x1_,x2_,xs__]:>id["Plus",p][id["Plus",0][x1,x2],xs],
    id["Plus",p_][x1_,id["Times",_][literal[-1,_],x2_]]:>id["Subtract",p][x1,x2]
  }//.{
    id["Times",p_][literal[-1,_],x_]:>id["Minus",p][x]
  }//.{
    id["Times",p_][x1_,x2_,xs__]:>id["Times",p][id["Times",0][x1,x2],xs],
    id["Times",p_][x1_,id["Power",_][x2_,literal[-1,_]]]:>id["Divide",p][x1,x2]
  }//.{
    id["Plus",p_][x___,id["Plus",_][y___],z___]:>id["Plus",p][x,y,z],
    id["Times",p_][x___,id["Times",_][y___],z___]:>id["Times",p][x,y,z]
  }//.{
    id["Power",p_][x_,literal[y_Integer,_]]:>native["power",p][x,const[y]]
  }

lexicalorder[a_,b_]:=
  Module[{la=Length[a],lb=Length[b]},
    If[la>lb,Order[a,PadRight[b,la,-1]],Order[PadRight[a,lb,-1],b]]
  ];

regexmacro[inputcode_]:=Module[{code,patternfunctions,sepos,names},
    code=inputcode//.{
      id["PatternTest",p_][patt:(id["Pattern",_][x:id[_,_],_]),test_]:>id["Condition",p][patt,test[x]],
      id["PatternTest",p_][patt_,test_]:>Module[{var=newvar},id["Condition",p][id["Pattern",p][id[var,-p],patt],test[id[var,0]]]]};
    patternfunctions={
      "StringExpression","Pattern","Blank","BlankSequence","BlankNullSequence",
      "Alternatives","Repeated","RepeatedNull","Except","Longest","Shortest",
      "Rule","RuleDelayed","Condition"};
    sepos=NestWhile[
      (First/@Split[#,#1==Take[#2,UpTo[Length@#1]]&])&,
      Sort[Position[code,id[Alternatives@@patternfunctions,_][___]],lexicalorder],
      Length[#1]!=Length[#2]&,2];
    Module[{se,rulepos,condpos,rulereplacements,var},Do[
      se=Extract[code,p];
      names=GroupBy[#,First->Last,Min]&@Cases[se,id["Pattern",p_][id[x_,xi_],_]:>(x->xi),-1,Heads->True];
      rulepos=Reverse@Sort[Append[#,2]&/@Position[se,id["Rule"|"RuleDelayed",_][_,_]],lexicalorder];
      rulereplacements=KeyValueMap[id[#1,p_]:>native["_replacement",p][const[#2]]&,names];
      Do[se=ReplacePart[se,rp->(se[[Sequence@@rp]]/.rulereplacements)];,{rp,rulepos}];
      condpos=Append[#,2]&/@Position[se,id["Condition",_][_,_]];
      Do[var=newvar;se=ReplacePart[se,cp->function[Extract[se,Most[cp]~Join~{0,2}]][{var},{nil},sequence[
        Extract[se,cp]/.KeyValueMap[id[#1,p_]:>argmatch[var,const[#2]]&,names]]]];
      ,{cp,condpos}];
      se=se//.KeyValueMap[id["Pattern",p_][id[#1,_],patt_]:>native["pattern",p][const[#2],patt]&,names];
      code=ReplacePart[code,p->se];
    ,{p,sepos}]];
    code
  ]

builtinlookup[table_,key_,found_,notfound_]:=If[KeyExistsQ[table,key],found[table[key]],notfound[key]];

resolvesymbols[code_]:=code//.{
    id[func_,p_][args___]:>builtinlookup[$builtinfunctions,func,native[#,p]&,id[#,p]&][args]
  }/.{
    id[any_,p_]:>builtinlookup[$builtinconstants,any,const[#,p]&,
      builtinlookup[$builtinfunctions,#,native[#,p]&,id[#,p]&]&]
  }

findinit[code_]:=
  Module[{sequencepos,scopevarpos,scopevar,initpos,initscopepos,refpos},
    sequencepos=Most/@Position[code,sequence];
    scopevarpos=<|Catenate[Module[{p=Most[#]},#->Append[p,2]&/@Extract[code,Append[p,1]]]&/@Position[code,scope[_]]]|>;
    scopevar=Keys[scopevarpos];
    initpos=First@Sort[#,lexicalorder]&/@
      GroupBy[{Extract[code,Join[Most[#],{1,1}]],Most[#]}&/@Position[code,assign[_]],First->Last];
    refpos=First@Sort[#,lexicalorder]&/@
      DeleteMissing@AssociationThread[scopevar->#/@scopevar]&@
        GroupBy[Module[{p=Most[#]},Extract[code,Append[p,1]]->p]&/@Position[code,var],First->Last];
    Module[{badinit,badref},
      badinit=Cases[{#,scopevarpos[#],Most@initpos[#]}&/@scopevar,
        {name_,exp_,act_}/;(exp=!=act):>(name-><|"Expected"->exp,"Actual"->act|>)];
      badref=Cases[{#,If[MissingQ[#],#,Append[#,1]]&@initpos[#],refpos[#]}&/@scopevar,
        {name_,init_,use_}/;(init=!=use):>(name-><|"Initialization"->init,"FirstUsage"->use|>)];
      If[MissingQ@#2["Actual"],
        (Message[MCSemantics::noinit,$variabletable[#1]];),
        (Message[MCSemantics::badinit,$variabletable[#1]];Throw["semantics"])]&@@@badinit;
      (Message[MCSemantics::badref,$variabletable[#1]];Throw["semantics"])&@@@badref;
    ];
    ReplacePart[code,Append[#,0]->initialize&/@DeleteMissing[initpos/@scopevar]]
  ]

semantics[code_]:=findinit@resolvesymbols@regexmacro@arithmeticmacro@functionmacro@variablerename[code]


optim[array][code_]:=code//.{
    list[p_][v:(literal[_,_]..)]:>(If[SameQ@@(Head/@#),
      regularlist[p][Head[#[[1]]],{Length@#},#],
      (Message[MCOptim::elemtype,tostring@list[0][v]];Throw["syntax"])]&@{v}[[;;,1]]),
    list[p_][v:(regularlist[_][__]..)]:>(If[SameQ@@(#[[;;,1]])&&SameQ@@(#[[;;,2]]),
      regularlist[p][#[[1,1]],Prepend[#[[1,2]],Length@#],#[[;;,3]]],
      (Message[MCOptim::elemdims,tostring@list[0][v]];Throw["syntax"])]&@{v})
  };

$optimpasses={array};
alloptim[code_]:=Fold[optim[#2][#1]&,code,$optimpasses];


nativename[str_]:=StringRiffle[ToLowerCase@StringCases[str,RegularExpression["[A-Z][a-z]*"]],"_"]
getargtypes[function[_][_,types_,__]]:=types
expandpack[var_String]:=If[StringTake[var,2]=="vp","...",""]
anyispack[vars_List]:=AnyTrue[vars,StringTake[#,2]=="vp"&]

annotatebegin[p_Integer/;p>0]:="/*\\b"<>ToString[p]<>"*/"
annotateend[p_Integer/;p>0]:="/*\\e"<>ToString[p]<>"*/"
annotatebegin[___]:="/*\\b*/"
annotateend[___]:="/*\\e*/"
toexportcode[code_String]:=StringDelete[code,"/*\\"~~("b"|"e"|"n")~~Shortest[___]~~"*/"]
toexportbinary[code_String]:=StringJoin@Flatten@StringSplit[
  StringTrim/@StringSplit[code,"\n"],x:("/*\\"~~("b"|"e"|"n")~~Shortest[___]~~"*/"):>{"\n",x}]
stringtransform[str_String]:=FromCharacterCode@Flatten[Map[
    If[#<128,#,Join[{92,If[#<65536,117,85]},#+48+39UnitStep[#-10]&@IntegerDigits[#,16,If[#<65536,4,8]]]]&,
    ToCharacterCode[ToString@CForm@str,"Unicode"]
  ]]

codegen[mainargs[vars_,types_,False],___]:=
  Module[{templatetypes={}},
    <|"Code"->MapThread["const "<>If[MatchQ[#1,"array"[___]],
          Last@AppendTo[templatetypes,newtype],codegen[type[#1]]]<>"& "<>#2&,
        {types,vars}],
      "Template"->If[Length@templatetypes==0,
        "",StringRiffle[templatetypes,{"template<typename ",", typename ",">\n"}]]
    |>]
codegen[mainargs[vars_,types_,True],___]:=<|"Code"->codegen[args[vars,types]],"Template"->""|>;
codegen[args[vars_,types_],___]:=
  MapThread[If[#=="auto&&",#,"const "<>#<>"&"]&@codegen[type[#1]]<>expandpack[#2]<>" "<>#2&,{types/.nil->"auto&&",vars}]
codegen[argv[var_,i_Integer]]:=var<>".get("<>ToString@CForm[i-1]<>")"
codegen[argv[var_,pack[i_Integer]]]:=var<>".get_pack("<>ToString@CForm[i-1]<>")"
codegen[argmatch[var_,const[i_]]]:=var<>"["<>codegen[const[i]]<>"]"

codegen[function[p_][vars_,types_,sequence[exprs___]],any___]:=
  {annotatebegin[p],"[&](",
    Riffle[If[!anyispack[vars],Append[#,"auto&&..."],#]&@codegen[args[vars,types]],", "],")",
    codegen[sequence[exprs],If[{any}=={"Scope"},"Scope","Return"]],annotateend[p]}
codegen[variadic[p_][vars_,types_,sequence[exprs___]],___]:=
  {annotatebegin[p],"[&](",Riffle[codegen[args[vars,types]],", "],")",
    codegen[sequence[exprs],"Return"],annotateend[p]}
codegen[function[p_][vars_,types_,sequence[exprs___],variadic[specs___]],___]:=
  codegen[native["variadic",p][function[p][vars,types,sequence[exprs]],variadic[p][specs]]]

codegen[scope[p_][_,sequence[exprs___]],any___]:=
  {annotatebegin[p],codegen[sequence[exprs],any],annotateend[p]}

codegen[initialize[var_,expr_],___]:={"auto ",codegen[var]," = ",codegen[native["val",0][expr]]}

codegen[assign[p_][var_,expr_],___]:=codegen[native["set",p][var,expr]]

codegen[literal[s_String,p_],___]:={annotatebegin[p],"wl::string(u8",stringtransform[s],annotateend[p],")"}
codegen[literal[i_Integer,p_],___]:={annotatebegin[p],"int64_t(",ToString@CForm[i],annotateend[p],")"}
codegen[literal[r_Real,p_],___]:={annotatebegin[p],ToString@CForm[r],annotateend[p]}
codegen[const[i_Integer],___]:={annotatebegin[],"wl::const_int<"<>ToString@CForm[i]<>">",annotateend[],"{}"}
codegen[c:consts[(_Integer)..],___]:=
  {annotatebegin[],"wl::const_ints<"<>StringRiffle[ToString@*CForm/@(List@@c),", "]<>">",annotateend[],"{}"}
codegen[const[s_String,p___],___]:={annotatebegin[p],s,annotateend[p]}

codegen[native[name_,p_],"Function"]:={annotatebegin[p],"wl::"<>name,annotateend[p]}
codegen[native[name_,p_],___]:={annotatebegin[p],"WL_FUNCTION(","wl::"<>name,annotateend[p],")"}
codegen[native[name_,0],"Function"]:={"wl::"<>name}
codegen[native[name_,0],___]:={"WL_FUNCTION(","wl::"<>name,")"}

codegen[vargtag,___]:={annotatebegin[],"wl::varg_tag{}",annotateend[]}
(*codegen[leveltag[l_Integer],___]:="wl::level_tag<"<>ToString@CForm[l]<>">{}"*)

codegen[clause[type_,p_][func_,{iters___}],___]:={
    annotatebegin[p],"wl::clause_"<>nativename[type],"(",
    codegen[func,If[type=="Do"||type=="BreakDo","Scope","Return"]],",",
    Riffle[codegen[#,"Return"]&/@{iters},", "],annotateend[p],")"}

codegen[type[t_String],___]:=t
codegen[type["array"[t_,r_]],___]:="wl::ndarray<"<>t<>", "<>ToString[r]<>">"
codegen[type["func"[{args___},ret_]],___]:="wl::function<"<>codegen[type[ret]]<>"("<>StringRiffle[codegen@*type/@{args},","]<>")>"
codegen[typed[p_][any_],___]:={annotatebegin[p],"(",codegen[type[any]]<>"{}",annotateend[p],")"}

codegen[var[name_,p___],___]:={annotatebegin[p],name<>expandpack[name],annotateend[p]}
codegen[movvar[name_,p___],___]:={annotatebegin[p],"WL_PASS("<>name<>")"<>expandpack[name],annotateend[p]}
codegen[id[name_,___],___]:=(Message[MCSemantics::undef,name];Throw["semantics"])

codegen[sequence[scope[vars_,expr_]],any___]:=codegen[scope[vars,expr],any]
codegen[sequence[expr___],"Scope"]:={"{",({codegen[#],";"}&/@{expr}),"}"}
codegen[sequence[most___,initialize[var_,expr_]],"Return"]:=codegen[sequence[most,initialize[var,expr],var],"Return"]
codegen[sequence[most___,last_],"Return"]:={"{",({codegen[#],";"}&/@{most}),"return ",codegen[native["val",0][last]],";","}"}
codegen[sequence[expr___],"Hold"]:={"[&]",codegen[sequence[expr],"Return"]}
codegen[sequence[expr___],___]:={codegen[sequence[expr],"Hold"],"()"}

codegen[branchif[p_][cond_,expr1_,expr2_],___]:=
  codegen[native["branch_if",p][cond,expr1,expr2],"Hold"]
(*codegen[branchwhich[conds_,cases__],___]:=
  codegen[native["which"][conds,cases],"Hold"]*)

codegen[loopfor[p_][test_,incr_,body_],___]:=
  codegen[native["loop_for",p][test,incr,body],"Hold"]
codegen[loopwhile[p_][test_,body_],___]:=
  codegen[native["loop_while",p][test,body],"Hold"]
codegen[break[]]:="throw wl::loop_break{}"

codegen[list[p_][any___],___]:=codegen[native["list",p][any]]
codegen[regularlist[p_][t_,dims_,array_],___]:={
  annotatebegin[p],
  codegen[type[totypespec@ndarray[ToString@t,Length@dims]]],"(",
  StringRiffle[ToString@*CForm/@dims,{"{",", ","}"}],", ",
  StringRiffle[ToString@*CForm/@Flatten@array,{"{",", ","}"}],
  annotatebegin[p],")"}

codegen[embedcxx[p_][vars_,cxx_],___]:={annotatebegin[p],StringJoin@Riffle[cxx,vars[[;;,1]]],annotateend[p]}

codegen[head_[args___],any___]:={codegen[head,"Value"],"(",Riffle[codegen[#,any]&/@{args},", "],")"}
codegen[native[name_,p_][args___],any___]:=
  {annotatebegin[p],codegen[native[name,0],"Function"],"(",Riffle[codegen[#,any]&/@{args},", "],annotateend[p],")"}

codegen[any_,rest___]:=(Message[MCCodegen::bad,tostring[any]];"<codegen>")

initcodegen[function[p_][vars_,types_,expr_],tocode_]:=
  Module[{typecode=codegen[mainargs[vars,types,tocode]]},
    Flatten@{annotatebegin[p],typecode["Template"]<>"auto main_function(",
      Riffle[typecode["Code"],", "],")",codegen[expr,"Return"],annotateend[p]}
  ]
  
codeformat[segments_List]:=
  StringRiffle[#,{"","\n","\n"}]&@
    FoldPairList[
      Module[{pad=#1-Boole[StringTake[#2,1]=="}"]},
       {If[pad<=0,"",StringRepeat[" ",4pad]]<>#2,pad+Boole[StringTake[#2,-1]=="{"]}]&,
      0,StringJoin/@SplitBy[segments/.{"{"->Sequence[" {","\n"],";"->Sequence[";","\n"]},#=="\n"&][[;;;;2]]
    ]
maincodegen[code_,tocode_]:=codeformat@initcodegen[code,tocode]


$numerictypes={
"MathLink","Void","Boolean",
"Integer8","UnsignedInteger8",
"Integer16","UnsignedInteger16",
"Integer32","UnsignedInteger32",
"Integer64","UnsignedInteger64",
"Real32","Real64",
"ComplexReal32","ComplexReal64"};

symboltype[type_]:=Which[
  StringContainsQ[type,"Integer"],Integer,
  StringContainsQ[type,"Complex"],Complex,
  StringContainsQ[type,"Real"],Real,
  type==="String","UTF8String",
  True,type];

loadfunction[libpath_String,funcid_String,args_]:=
  Module[{typefunc,libfunc,rank,type,commontype,returntype,argtypes,
          maxtypecount=256,retbylink=False},
    typefunc=LibraryFunctionLoad[libpath,funcid<>"_type",{},Integer];
    If[typefunc===$Failed,Message[MCLink::rettype];Return[$Failed]];
    {rank,type}=QuotientRemainder[typefunc[],maxtypecount];
    LibraryFunctionUnload[typefunc];
    If[1<=type<=Length[$numerictypes],
      type=$numerictypes[[type]],Message[MCLink::badtype];Return[$Failed]];
    If[type=="MathLink",returntype="Void";retbylink=True,
      If[Not[0<=rank<=$rankmaximum],Message[MCLink::bigrank];Return[$Failed]];
      commontype=symboltype[type];
      returntype=If[rank==0,commontype,
        If[MemberQ[{"Integer64","Real64","ComplexReal64"},type],
          {commontype,rank},
          LibraryDataType[NumericArray,Replace[type,"Boolean"->"Integer8"],rank]]];
    ];
    argtypes=Replace[totypename[#],{
        {t_,r_}:>If[MemberQ[{"Integer64","Real64","ComplexReal64"},t],
          {symboltype[t],r,"Constant"},
          {LibraryDataType[NumericArray,t,r],"Constant"}],
        t_String:>symboltype[t]}]&/@args;
    <|"Function"->LibraryFunctionLoad[libpath,funcid<>"_func",argtypes,returntype],
      "ReturnByLink"->retbylink|>
  ]


$template=Import[$packagepath<>"/SourceFiles/src_template.cpp","Text"];

Options[compilelink]={
  "TargetDirectory"->Automatic,
  "WorkingDirectory"->Automatic,
  "Debug"->False,
  "MonitorAbort"->True,
  "CompileOptions"->"",
  "Includes"->{},
  "IncludeDirectories"->{},
  "Libraries"->{},
  "LibraryDirectories"->{}
};
Options[CompileToBinary]=Options[compilelink];

compilelink[$Failed,___]:=$Failed;

compilelink[f_,uncompiled_,OptionsPattern[]]:=
  Module[{output,types,funcid,src,lib,workdir,
      libdir,mldir,mllib,compiler,opt,opterror,errorparser,errors},
    $CppSource="";
    $CompilerOutput="";
    output=f["output"];
    types=codegen@*type/@f["types"];
    funcid="f"<>ToString@RandomInteger[{10^12,10^13-1}];
    workdir=OptionValue["WorkingDirectory"];
    libdir=OptionValue["TargetDirectory"];
    If[libdir===Automatic,libdir="TargetDirectory"/.Options[CCompilerDriver`CreateLibrary]];
    If[(workdir=!=Automatic)&&!(StringQ[workdir]&&DirectoryQ[workdir]),
      Message[MCLink::workdir];Return[$Failed]];
    If[!(StringQ[libdir]&&DirectoryQ[libdir]),Message[MCLink::libdir];Return[$Failed]];
    opterror=Null;
    MathCompile`$CppSource=
      TemplateApply[$template,<|
          "includes"->(If[MatchQ[#,{_String...}],
              StringJoin["#include \""<>#<>"\"\n"&/@#],
              Message[MCLink::includes];opterror=True;""
            ]&@Flatten@{OptionValue["Includes"]}),
          "funcbody"->toexportbinary@output,
          "argsv"->StringRiffle[#<>"{}"&/@types,", "],
          "args"->StringRiffle[
            MapThread[StringTemplate["wl::librarylink::get<``>(argv[``])"],
              {types,Range[Length@types]-1}],
            {"            ",",\n            ",""}],
          "funcid"->funcid,
          "kernelfuncnames"->StringJoin[
            ToString@CForm[#]<>",\n        "&/@Keys@$kernelfunctiontable]
        |>];
    If[opterror=!=Null,Return[$Failed]];
    If[FileExistsQ[$packagepath<>"/IncludeFiles/math_compile.h"]=!=True,
      Message[MCLink::noheader];Return[$Failed]];
    mldir=$InstallationDirectory<>
      "/SystemFiles/Links/MathLink/DeveloperKit/"<>$SystemID<>"/CompilerAdditions";
    mllib=If[StringContainsQ[$SystemID,"MacOSX"],"MLi4","ML"<>ToString[$SystemWordLength]<>"i4"];
    compiler=CCompilerDriver`DefaultCCompiler[];
    opterror=Catch[opt=$compileroptions[compiler,$SystemID];];
    If[opterror=!=Null,Return[$Failed]];
    lib=Quiet@CCompilerDriver`CreateLibrary[
      MathCompile`$CppSource,funcid,
      "Language"->"C++",
      "CompileOptions"->$joinoptions@{
          opt["Base"],
          opt["Optimize"][If[TrueQ@OptionValue["Debug"],0,3]],
          opt["Define"][{"WL_USE_MATHLINK",
            If[TrueQ@OptionValue["MonitorAbort"],"WL_CHECK_ABORT",Nothing],
            If[!TrueQ@OptionValue["Debug"],"NDEBUG",Nothing]}],
          OptionValue["CompileOptions"]
        },
      "CleanIntermediate"->!TrueQ@OptionValue["Debug"],
      "IncludeDirectories"->Join[{mldir,$packagepath<>"/IncludeFiles"},
        Flatten@{OptionValue["IncludeDirectories"]}],
      "LibraryDirectories"->Join[{mldir,$packagepath<>"/LibraryResources/"<>$SystemID},
        Flatten@{OptionValue["LibraryDirectories"]}],
      "Libraries"->Join[{mllib,"pcre2-8"},Flatten@{OptionValue["Libraries"]}],
      "WorkingDirectory"->workdir,
      "TargetDirectory"->libdir,
      "ShellCommandFunction"->((MathCompile`$CompilerCommand=#)&),
      "ShellOutputFunction"->((MathCompile`$CompilerOutput=#)&)
    ];
    If[lib===$Failed,
      Message[MCLink::genfail];
      errorparser=Lookup[$compilererrorparser,compiler,Null];
      If[Head[errorparser]===Function,
        errors=errorparser[funcid];
        emitcompilererrors[f["source"],errors];,
        Message[MCCxx::error,"Check $CompilerOutput for the errors."];
      ];
      Return[$Failed]];
    If[#["ReturnByLink"],IndirectReturn[#["Function"]],#["Function"]]&@
      loadfunction[lib,funcid,f["types"]]
  ]


$joinoptions=StringRiffle[Flatten[#]," "]&;
$compileroptionsbase=<|
  "GCC"-><|
    "Base"->"-x c++ -std=c++1z -fPIC -march=native",
    "Optimize"-><|0->"-O0",1->"-O1",2->"-O2",3->"-O3 -ffast-math"|>,
    "Define"->("-D"<>#&/@#&)|>,
  "MinGW"-><|
    "Base"->"-static -x c++ -std=c++1z -fPIC -march=native",
    "Optimize"-><|0->"-O0",1->"-O1",2->"-O2",3->"-O3 -ffast-math"|>,
    "Define"->("-D"<>#&/@#&)|>,
  "ICC"-><|
    "Base"->"-std=c++17 -Kc++ -restrict -march=native",
    "Optimize"-><|0->"-O0",1->"-O1",2->"-O2",3->"-O3 -fp-model fast=2"|>,
    "Define"->("-D"<>#&/@#&)|>,
  "Clang"-><|
    "Base"->"-x c++ -std=c++1z -fPIC -march=native",
    "Optimize"-><|0->"-O0",1->"-O1",2->"-O2",3->"-O3 -ffast-math"|>,
    "Define"->("-D"<>#&/@#&)|>,
  "MSVC"-><|
    "Base"->"/std:c++17 /EHsc /MD",
    "Optimize"-><|0->"/O0",1->"/O1",2->"/O2",3->"/Ox /Gy /fp:fast"|>,
    "Define"->("/D"<>#&/@#&)|>
|>;
$compileroptions[
    CCompilerDriver`GCCCompiler`GCCCompiler,
    _
  ]:=$compileroptionsbase["GCC"]
$compileroptions[
    CCompilerDriver`GenericCCompiler`GenericCCompiler,
    "Windows-x86-64"
  ]:=$compileroptionsbase["MinGW"]
$compileroptions[
    CCompilerDriver`GenericCCompiler`GenericCCompiler,
    s_String/;StringContainsQ[s,"Linux"]
  ]:=$compileroptionsbase["Clang"]
$compileroptions[
    CCompilerDriver`ClangCompiler`ClangCompiler,
    "MacOSX-x86-64"
  ]:=$compileroptionsbase["Clang"]
$compileroptions[
    CCompilerDriver`IntelCompiler`IntelCompiler,
    s_String/;StringContainsQ[s,"Linux"]
  ]:=$compileroptionsbase["ICC"]
$compileroptions[
    CCompilerDriver`VisualStudioCompiler`VisualStudioCompiler,
    s_String/;StringContainsQ[s,"Windows"]
  ]:=$compileroptionsbase["MSVC"]
$compileroptions[compiler_,platform_String
  ]:=(Message[MCCxx::support,SymbolName@Unevaluated@compiler,platform];Throw[0];)

$compilererrorparserbase=<|
  "GCC"->Function[{id},{
      Split[#,Head[#2]=!=Integer&]&,Flatten[{
        StringCases[#,id<>".c:"~~l:(DigitCharacter ..)~~":":>FromDigits@l],
        StringDelete["static assertion failed: "]@StringCases[#,"error: "~~err___:>err]
      }&/@StringSplit[MathCompile`$CompilerOutput,"\n"]]}],
  "ICC"->Function[{id},{
      Reverse/@Split[#,Head[#1]=!=Integer&]&,Flatten[
        StringCases[$CompilerOutput<>"\n",{
          "at line "~~l:(DigitCharacter ..)~~Shortest[m___]~~id<>".c\""
            /;StringFreeQ[m,"at line"]:>FromDigits[l]-1,
          "error: "~~Shortest[err___]~~"\n":>
            StringDelete["static_assert"~~__~~"'"~~Shortest[__]~~"' "]@err}]]}],
  "Clang"->Function[{id},{
      Reverse/@Split[#,Head[#1]=!=Integer&]&,Flatten[{
        StringCases[#,id<>".c:"~~l:(DigitCharacter ..)~~":":>FromDigits@l],
          StringDelete["static_assert"~~__~~"'"~~Shortest[__]~~"' "]@
            StringCases[#,"error: "~~err___:>err]
      }&/@StringSplit[MathCompile`$CompilerOutput,"\n"]]}],
  "MSVC"->Function[{id},{
      Reverse/@Split[#,Head[#1]=!=Integer&]&,Flatten[{
        StringCases[#,id<>".c("~~l:(DigitCharacter ..)~~")":>FromDigits@l],
        StringDelete["C2338: "]@StringCases[#,": error "~~err___:>err]
      }&/@StringSplit[MathCompile`$CompilerOutput,"\n"]]}]
|>;
$compilererrorparser=<|
  CCompilerDriver`GCCCompiler`GCCCompiler->
    $compilererrorparserbase["GCC"],
  CCompilerDriver`GenericCCompiler`GenericCCompiler->
    $compilererrorparserbase[If[$SystemID=="Windows-x86-64","GCC","Clang"]],
  CCompilerDriver`IntelCompiler`IntelCompiler->
    $compilererrorparserbase["ICC"],
  CCompilerDriver`VisualStudioCompiler`VisualStudioCompiler->
    $compilererrorparserbase["MSVC"]
|>;
emitcompilererrors[wlsrc_,{extract_Function,parsed_List}]:=
  Module[{cxxsrc,srcrange,errors,message,position,srcpart},
    cxxsrc=StringSplit[$CppSource,"\n"];
    srcrange=MinMax[Flatten@Position[cxxsrc,_String?(StringTake[#,UpTo[2]]=="/*"&)]];
    errors=extract@DeleteCases[parsed,l_Integer/;!Between[l,srcrange]];
    If[AnyTrue[errors,Head[#]===String&&StringContainsQ[#,"MCCxx::compilerver"]&,2],
      (Message[MCCxx::compilerver];Return[])];
    If[Length@errors===0,
      Message[MCCxx::error,"Check $CompilerOutput for the errors."];Return[];];
    errors=List@@@Flatten[If[Head@First[#]===Integer,Thread[Rest[#]->First[#]],Thread[#->0]]&/@errors];
    If[Length@errors===0,
      Message[MCCxx::error,"Check $CompilerOutput for the errors."];Return[];];
    errors=MapAt[If[#==0,0,FromDigits@First@StringCases[
        cxxsrc[[#]],"/*\\"~~("b"|"e"|"n")~~(l:Shortest[___])~~"*/":>"0"<>l]]&,
      errors,{;;,2}];
    Do[
      message=ToString@CForm@If[StringLength[#]>80,
        StringTake[#,{1,77}]<>"...",#]&@error[[1]];
      position=error[[2]];
      If[TrueQ[position>0],
        srcpart=StringTake[ToString@CForm[#],{2,-2}]&/@{
          StringTake[wlsrc,{Max[1,position-30],position-1}],
          StringTake[wlsrc,{position,position}],
          StringTake[wlsrc,{position+1,Min[StringLength[wlsrc],position+30]}]};
        Message[MCCxx::error,"\!\(\(\"..."<>srcpart[[1]]<>"\"\!\(\""<>srcpart[[2]]<>"\"\+\"\[And]\"\)\""<>srcpart[[3]]<>"...\"\)\+"<>message<>"\)"],
        Message[MCCxx::error,"\!\("<>message<>"\) (cannot be located)"]
      ]
    ,{error,errors}]
  ]


print[id[id_String,___]]:=id
print[literal[literal_,_]]:=ToString@CForm@literal
print[head_[args___]]:={print@head,"[",Riffle[print/@{args},","],"]"}

print[native[any_,_][args___]]:={any,"[",Riffle[print/@{args},","],"]"}
print[native[any_,_]]:=any
print[typed[_][any_]]:={"Typed","[",ToString[any],"]"}
print[var[var_String,_]]:=var
print[movvar[var_String,_]]:={"WL_PASS","(",var,")"}
print[list[_][args___]]:={"{",Riffle[print/@{args},","],"}"}
print[regularlist[_][_,dims_,array_]]:=print@Map[literal[#,0]&,Apply[list[0],array,{0,Length@dims-1}],{-1}]
print[clause[type_,_][func_,iters_List]]:={type,"[",print[func],",",print[list@@iters],"]"}
print[function[_][args_,types_,expr_]]:={"Function","[",print[list@@(id/@args)],",",print[expr],"]"}
print[scope[_][vars_,expr_]]:={"Module","[",print[list@@(id/@vars)],",",print[expr],"]"}
print[branchif[_][cond_,true_,false_]]:=print[id["If"][cond,true,false]];
print[sequence[exprs__]]:={"(",Riffle[(print/@{exprs}),";"],")"}
print[assign[_][var_,expr_]]:={print@var,"=",print@expr}
print[initialize[var_,expr_]]:={print@var,"=",print@expr}
print[embedcxx[_][_,cxx_]]:={"CXX","[",cxx,"]"}
print[any_]:=any

tostring[any_]:=StringJoin@Flatten@print[any]


End[];


EndPackage[];
