(* ::Package:: *)

registertest[test_]:=(AppendTo[$MathCompileTests,test];)

test[name_String,f_,pairs_,cmp_:SameQ]:=(
  Module[{mf,file},
    $CurrentMathCompileTest=name;
    If[!TrueQ@MatchQ[pairs,{(({___})|({___}->_))..}],
      Echo[name<>" contains zero groups of arguments."];Abort[]];
    mf=CompileToBinary[f];
    file=DeleteMissing@{Information[mf]["File"]};
    If[mf===$Failed||(Or@@(!TrueQ[
        If[Head[#]===Rule,($LastReturn=(mf@@#[[1]]))~cmp~#[[2]],
          ($LastReturn=(mf@@#))~cmp~(f@@#)]]&/@pairs)),
      AppendTo[$FailedMathCompileTests,name];
    ];
    Quiet@LibraryFunctionUnload[mf];
    If[FileExistsQ[#],DeleteFile[#]]&/@file;
  ])

cleartests[]:=($MathCompileTests={};)

runtests[]:=
  Block[{$FailedMathCompileTests={},i=0,n=Length@$MathCompileTests},
    PrintTemporary[Column[{ProgressIndicator[Dynamic[i/n]],Dynamic@$CurrentMathCompileTest}]];
    Do[$MathCompileTests[[i]][],{i,1,n}];<|"FailedTests"->$FailedMathCompileTests|>
  ]


$MathCompileTests={};

registertest[test["types:integer,1",          Function[{Typed[x,{Integer,1}]},x],{{Range[10]},{Range[0]},{Range[1000000]}}]&];
registertest[test["types:double,1",           Function[{Typed[x,{Real,1}]},x],{{N@Range[10]},{N@Range[0]},{N@Range[1000000]}}]&];
registertest[test["types:float,1",            Function[{Typed[x,{"Real32",1}]},x],{{NumericArray[N@Range[100],"Real32"]}}]&];
registertest[test["types:complex,1",          Function[{Typed[x,{Complex,1}]},x],{{Range[100](1.+1.I)}}]&];
registertest[test["types:complexfloat32,1",   Function[{Typed[x,{"ComplexReal32",1}]},x],{{NumericArray[Range[100](1.+1.I),"ComplexReal32"]}}]&];
registertest[test["types:bool",               Function[{Typed[x,"Boolean"]},x],{{True},{False}}]&];
registertest[test["types:uint16,1",           Function[{Typed[x,{"UnsignedInteger16",1}]},x],{{NumericArray[Range[100],"UnsignedInteger16"]}}]&];
registertest[test["types:int8",               Function[{Typed[x,"Integer8"]},x],{{-5}}]&];
registertest[test["types:double,8",           Function[{Typed[x,{Real,8}]},x],{{ArrayReshape[N@Range[362880],{2,3,4,5,6,7,8,9}]}}]&];

registertest[test["module:basic",             Function[{Typed[x,Real]},Module[{y=0.0,z},z=4;y=x*x;x+y+z]],{{5.0}}]&];
registertest[test["module:nested",            Function[{Typed[x,Real]},Module[{y},y=x;Module[{z},z=y+1;z]]],{{3.0}}]&];
registertest[test["module:list",              Function[{Typed[x,Real]},Module[{y=Typed[{Complex,1}]},y={x,x,x}]],{{3.0}->{3.0+0.I,3.0+0.I,3.0+0.I}}]&];
registertest[test["if:value",                 Function[{Typed[x,"Boolean"]},If[x,Range[1,100],Range[101,200]]],{{True},{False}}]&];
registertest[test["if:function",              Function[{Typed[x,"Boolean"]},Module[{f},f=If[x,Tan,Cot];f[1.5]+If[x,Sin,Cos][1.5]]],{{False},{True}}]&];
registertest[test["if:nested",                Function[{Typed[x,Integer]},Module[{f},f=If[x>5,#+5&,If[x<-5,#-5&,#&]];f[15]]],{{8},{2},{-8}}]&];
registertest[test["if:capture",               Function[{},Module[{y=3,f},f=If[y>0,#+5&,#-5&];y=-3;f[15]]],{{}}]&];
registertest[test["do:basic",                 Function[{Typed[n,Integer]},Module[{x=0},Do[x+=i,{i,1,n}];x]],{{-10},{0},{1},{10},{10000}}]&];
registertest[test["do:level3",                Function[{Typed[n,Integer]},Module[{x=0},Do[x+=100i+10j+k,{i,n},{j,2n},{k,3n}];x]],{{2},{7},{23}}]&];
registertest[test["for:4args",                Function[{Typed[x0,Real]},Module[{i,x=x0},For[i=0,x<0,++i,x=Log[x]];i]],{{2.},{123456789.}}]&];
registertest[test["for:3args",                Function[{Typed[x0,Real]},Module[{i,x=x0},For[i=0,(x=Log[x])<0,++i];i]],{{2.},{123456789.}}]&];
registertest[test["for:3args",                Function[{},Module[{n},n=1;While[n<17,++n];n]],{{}}]&];
registertest[test["while:2args",              Function[{},Module[{n},n=1;While[++n<17];n]],{{}}]&];
registertest[test["break:while",              Function[{},Module[{n=1},While[True,If[n>10,Break[]];n++];n]],{{}}]&];
registertest[test["break:for",                Function[{},Module[{n},For[n=1,n<=10,n++,If[n>5,Break[]]];n]],{{}}]&];
registertest[test["break:do",                 Function[{Typed[n,Integer]},Module[{t=0},Do[t+=i*i;If[i>n,Break[]],{i,1,100}];t]],{{1},{78},{123}}]&];
registertest[test["scan:simple",              Function[{Typed[n,Integer]},Module[{t=0},Scan[t+=#&,Range[n]];t]],{{0},{112},{1000000}}]&];
registertest[test["scan:level2",              Function[{Typed[n,{Integer,4}]},Module[{t=0.0},Scan[t+=Last[#]&,n,{3}]&];t]],{{ArrayReshape[Range[3*4*5*6],{3,4,5,6}]}}]
registertest[test["sum:array",                Function[{Typed[n,{Integer,3}]},Sum[x+5,{x,n}]],{{RandomInteger[1000,{10,10,10}]}}]&];
registertest[test["sum:index",                Function[{Typed[n,Integer]},Sum[i j,{i,1,n,2},{j,2,n,5}]],{{123}}]&];
registertest[test["product:array",            Function[{Typed[n,{Integer,3}]},Product[x-5,{x,n}]],{{RandomInteger[10,{10,10,10}]}}]&];
registertest[test["product:index",            Function[{Typed[n,Integer]},Product[i+j,{i,1,n,2},{j,2,n,3}]],{{8}}]&];
registertest[test["add_to",                   Function[{},Module[{x={1.,2.,3.}},x+=5;x]],{{}}]&];
registertest[test["subtract_from",            Function[{},Module[{x={1.+1.I,2.+2.I,3.+3.I}},x-=5;x]],{{}}]&];
registertest[test["times_by",                 Function[{},Module[{x=7.5},x*=5;x]],{{}}]&];
registertest[test["divide_by",                Function[{},Module[{x={1.,2.,3.}},x/=5;x]],{{}}]&];
registertest[test["increment",                Function[{},Module[{x={1,2,3}},(x++)*x]],{{}}]&];
registertest[test["decrement",                Function[{},Module[{x={1.+1.I,2.+2.I,3.+3.I}},(x--)+x]],{{}}]&];
registertest[test["pre_increment",            Function[{},Module[{x={1,2,3}},(++x)*x]],{{}}]&];
registertest[test["pre_decrement",            Function[{},Module[{x={1.+1.I,2.+2.I,3.+3.I}},(--x)-x]],{{}}]&];
registertest[test["append_to",                Function[{},Module[{x={1.+1.I,2.+2.I,3.+3.I}},AppendTo[x,5]]],{{}->{1.+1.I,2.+2.I,3.+3.I,5.+0.I}}]&];
registertest[test["prepend_to",               Function[{},Module[{x={{1,2},{3,4},{5,6}}},PrependTo[x,{7,8}]]],{{}}]&];

registertest[test["add:scalar",               Function[{},1+4],{{}}]&];
registertest[test["add:list1",                Function[{},Range[1000]+4.5],{{}}]&];
registertest[test["add:list2",                Function[{},Range[1000]+Range[1001,2000]],{{}}]&];
registertest[test["add:varg",                 Function[{},3+4+Range[10]+(5+6I)+Range[11,20]+7.8],{{}}]&];
registertest[test["add:empty",                Function[{},Plus[]],{{}}]&];
registertest[test["times:scalar",             Function[{},(-2)*4],{{}}]&];
registertest[test["times:varg",               Function[{},3*4*Range[10]*(5+6I)*Range[11,20]*7.8],{{}}]&];
registertest[test["times:empty",              Function[{},Times[]],{{}}]&];
registertest[test["subtract:scalar",          Function[{},Subtract[5,8]],{{}}]&];
registertest[test["subtract:list",            Function[{},5.5-Range[1000]],{{}}]&];
registertest[test["divide:scalar",            Function[{},5/2],{{}->2.5}]&];
registertest[test["divide:list",              Function[{},5.5/Range[1000]],{{}}]&];

registertest[test["function:named",           Function[{Typed[x,Integer]},Function[u,3+u][x]],{{3}}]&];
registertest[test["function:anonymous",       Function[{Typed[x,Integer]},Function[3+#][x]],{{3}}]&];
registertest[test["function:2args",           Function[{Typed[x,Integer]},Function[{u,v},u*u+v*v*v][x,x]],{{3}}]&];
registertest[test["function:0args",           Function[{Typed[x,Integer]},Module[{f=1.I&},f[x,x,x]]],{{3}}]&];
registertest[test["function:nargs",           Function[{},Module[{f=List[##1,#4,##2,#3]&},f[1,2,3,4,5]]],{{}}]&];
registertest[test["function:ignore",          Function[{},Module[{f=#1&},f[1,2,3]]],{{}}]&];
registertest[test["function:generic",         Function[{},Module[{f={1,2,3}+#&},f[{4,5,6}]*f[7]]],{{}}]&];
registertest[test["function:recursive",       Function[{},Module[{f=Typed[{Integer}->Integer]},f=If[#==1,1,# f[#-1]]&;f[10]]],{{}}]&];

registertest[test["apply:default",            Function[{Typed[x,{Integer,4}]},Apply[Plus,x]],{{RandomInteger[10,{9,10,11,12}]}}]&];
registertest[test["apply:level0",             Function[{Typed[x,{Integer,4}]},Apply[Times,x,{0}]],{{RandomInteger[10,{9,10,11,12}]}}]&];
registertest[test["apply:level1",             Function[{Typed[x,{Integer,4}]},Apply[Plus,x,{1}]],{{RandomInteger[10,{9,10,11,12}]}}]&];
registertest[test["apply:level3",             Function[{Typed[x,{Integer,4}]},Apply[Times,x,{3}]],{{RandomInteger[10,{9,10,11,12}]}}]&];
registertest[test["apply:fix",                Function[{Typed[x,{Real,2}]},Apply[Subtract[#1,#2]&,x,{1}]],{{RandomReal[10,{10,2}]}}]&];
registertest[test["map:simple",               Function[{Typed[x,{Integer,1}]},#+1&/@x],{{RandomInteger[10,1000]}}]&];
registertest[test["map:level3",               Function[{Typed[x,{Integer,4}]},Map[#[[1]]&,x,{3}]],{{RandomInteger[10,{10,10,10,10}]}}]&];
registertest[test["map:array1",               Function[{Typed[x,{Integer,3}]},Map[Reverse,x,{1}]],{{RandomInteger[10,{10,10,10}]}}]&];
registertest[test["map:array2",               Function[{Typed[x,{Integer,1}]},Map[Range,x,{1}]],{{{5,5,5,5}}}]&];
registertest[test["mapthread:fixed",          Function[{Typed[x,{Integer,1}]},MapThread[#1+#2*#3&,{x,x,x,x}]],{{RandomInteger[10,{100}]}}]&];
registertest[test["mapthread:func",           Function[{Typed[x,{Integer,2}]},MapThread[#1+#2*#3&,x]],{{RandomInteger[10,{4,100}]}}]&];
registertest[test["mapthread:times",          Function[{Typed[x,{Integer,2}]},MapThread[Times,x]],{{RandomInteger[10,{4,100}]}}]&];
registertest[test["mapthread:list",           Function[{Typed[x,{Integer,2}]},MapThread[List,x]],{{RandomInteger[10,{4,100}]}}]&];
registertest[test["mapthread:mixed",          Function[{Typed[x,{Real,2}],Typed[y,{Integer,1}]},MapThread[Part,{x,y}]],{{RandomReal[10,{55,100}],RandomInteger[{1,100},55]}}]&];
registertest[test["mapthread:level2fixed",    Function[{Typed[x,{Integer,3}],Typed[y,{Integer,3}]},MapThread[Join,{x,y,y},2]],{{RandomInteger[10,{6,3,5}],RandomInteger[10,{6,3,3}]}}]&];
registertest[test["mapthread:level3",         Function[{Typed[x,{Integer,4}]},MapThread[List,x,3]],{{RandomInteger[10,{6,3,5,5}]}}]&];
registertest[test["select:basic",             Function[{Typed[x,{Integer,1}]},Select[x,EvenQ]],{{RandomInteger[10,{100}]}}]&];
registertest[test["select:func",              Function[{Typed[x,{Integer,1}]},Select[x,#>5.5&]],{{RandomInteger[10,{100}]}}]&];
registertest[test["select:array",             Function[{Typed[x,{Integer,2}]},Select[x,MemberQ[#,3]&]],{{RandomInteger[10,{20,10}]}}]&];
registertest[test["count:match",              Function[{Typed[x,{Integer,1}]},Count[x,4]],{{RandomInteger[10,{100}]}}]&];
registertest[test["count:matchlevel2",        Function[{Typed[x,{Integer,3}]},Count[x,{2,1},{2}]],{{RandomInteger[3,{10,10,2}]}}]&];
registertest[test["count:pattern",            Function[{Typed[x,{Integer,2}]},Count[x,_?(#[[2]]==4&)]],{{RandomInteger[10,{100,2}]}}]&];
registertest[test["count:patternlevel2",      Function[{Typed[x,{Integer,3}]},Count[x,_?(#[[2]]==4&),{2}]],{{RandomInteger[10,{10,10,2}]}}]&];
registertest[test["nest:basic",               Function[{},Nest[#+1&,3.5,200]],{{}}]&];
registertest[test["nest:array",               Function[{},Nest[Most,Range[10],5]],{{}}]&];
registertest[test["nest_list:basic",          Function[{},NestList[#+1&,3.5,200]],{{}}]&];
registertest[test["nest_list:array",          Function[{},NestList[RotateRight,Range[10],10]],{{}}]&];
registertest[test["nest_while:basic",         Function[{},NestWhile[#/2&,123456,EvenQ]],{{}->1929.}]&];
registertest[test["nest_while:list",          Function[{},NestWhile[2#&,Range[10],Last[#]>1000&]],{{}}]&];
registertest[test["nest_while:nargs",         Function[{},NestWhile[Floor[#/2]&,10,UnsameQ,2]],{{}}]&];
registertest[test["nest_while:limits",        Function[{Typed[max,Integer],Typed[n,Integer]},NestWhile[#+1&,0,#1<9&,5,max,n]],Flatten[Table[{i,j},{i,{0,1,4,5,6,8,9,10,13,14,15}},{j,Max[-i,-13],5,2}],1]]&];
registertest[test["nest_while_list:basic",    Function[{},NestWhileList[Log,100.,#>0&]],{{}}]&];
registertest[test["nest_while_list:list",     Function[{},NestWhileList[2#&,Range[10],Last[#]<1000&]],{{}}]&];
registertest[test["nest_while_list:nargs",    Function[{},NestWhileList[Floor[#/2]&,10,UnsameQ,2]],{{}}]&];
registertest[test["nest_while_list:limits",   Function[{Typed[max,Integer],Typed[n,Integer]},NestWhileList[#+1&,0,#1<9&,5,max,n]],Flatten[Table[{i,j},{i,{0,1,4,5,6,8,9,10,13,14,15}},{j,Max[-i,-13],5,2}],1]]&];
registertest[test["fold:basic",               Function[{},Fold[Times,2,Range[10]]],{{}}]&];
registertest[test["fold:single",              Function[{},Fold[Times,Range[4,10]]],{{}}]&];
registertest[test["fold:foldr",               Function[{},Fold[Times,2,Reverse@Range[10]]],{{}}]&];
registertest[test["fold:foldr,single",        Function[{},Fold[Times,Reverse@Range[4,10]]],{{}}]&];
registertest[test["fold:empty",               Function[{},Fold[Plus,{12}]],{{}}]&];
registertest[test["fold_list:basic",          Function[{},FoldList[Times,2,Range[10]]],{{}}]&];
registertest[test["fold_list:single",         Function[{},FoldList[Times,Range[4,10]]],{{}}]&];
registertest[test["fold_list:foldr",          Function[{},FoldList[Times,2,Reverse@Range[10]]],{{}}]&];
registertest[test["fold_list:foldr,single",   Function[{},FoldList[Times,Reverse@Range[4,10]]],{{}}]&];
registertest[test["fold_list:empty",          Function[{},FoldList[Plus,{12}]],{{}}]&];
registertest[test["fold_list:cast",           Function[{},FoldList[#1/#2&,5,{2,2,2,2}]],{{}->{5.,2.5,1.25,0.625,0.3125}}]&];
registertest[test["fixed_point:basic",        Function[{},FixedPoint[1+Floor[#/2]&,1000000]],{{}}]&];
registertest[test["fixed_point:max",          Function[{},FixedPoint[1+Floor[#/2]&,1000000,10]],{{}}]&];
registertest[test["fixed_point:test",         Function[{},FixedPoint[1+Floor[#/2]&,1000000,SameTest->(Abs[#1-#2]<100&)]],{{}}]&];
registertest[test["fixed_point_list:basic",   Function[{},FixedPointList[1+Floor[#/2]&,1000000]],{{}}]&];
registertest[test["fixed_point_list:max",     Function[{},FixedPointList[1+Floor[#/2]&,1000000,10]],{{}}]&];
registertest[test["fixed_point_list:test",    Function[{},FixedPointList[1+Floor[#/2]&,1000000,SameTest->(Abs[#1-#2]<100&)]],{{}}]&];
registertest[test["identity:scalar",          Function[{},Identity[5.6+6.7]],{{}}]&];
registertest[test["identity:array",           Function[{},Module[{id=Identity},id@Range[10][[3;;8;;2]]]],{{}}]&];
registertest[test["composition:basic",        Function[{},Module[{f=Composition[Round,#*#&,#+5&]},f[{0.4,-3.3,12.9}]]],{{}}]&];
registertest[test["right_composition:basic",  Function[{},Module[{f=RightComposition[Round,#*#&,#+5&]},f[{0.4,-3.3,12.9}]]],{{}}]&];
registertest[test["composition:expand",       Function[{},Composition[Round,#*#&,#+5&][{0.4,-3.3,12.9}]],{{}}]&];
registertest[test["right_composition:expand", Function[{},RightComposition[Round,#*#&,#+5&][{0.4,-3.3,12.9}]],{{}}]&];
registertest[test["all_true:level1",          Function[{Typed[x,{Integer,1}]},AllTrue[x,Positive]],{{{1,2,3,4,5}},{{1,2,-3,4,5}}}]&];
registertest[test["all_true:level3",          Function[{Typed[x,{Integer,4}]},AllTrue[x,#[[1]]>0&,3]],{{RandomInteger[{1,10},{5,6,7,8}]},{RandomInteger[{-10,10},{5,6,7,8}]}}]&];
registertest[test["any_true:level1",          Function[{Typed[x,{Integer,1}]},AnyTrue[x,Positive]],{{{-1,-2,3,-4,-5}},{{-1,-2,-3,-4,-5}}}]&];
registertest[test["any_true:level3",          Function[{Typed[x,{Integer,4}]},AnyTrue[x,#[[1]]>0&,3]],{{RandomInteger[{-10,-1},{5,6,7,8}]},{RandomInteger[{-10,10},{5,6,7,8}]}}]&];
registertest[test["none_true:level1",         Function[{Typed[x,{Integer,1}]},NoneTrue[x,Positive,1]],{{{-1,-2,3,-4,-5}},{{-1,-2,-3,-4,-5}}}]&];

registertest[test["exp,log:real",             Function[{Typed[x,Real]},{Exp[x],Log[x],Log10[x],Log2[x]}],{{0.001},{0.08},{1.5},{5.3}},Equal]&]
registertest[test["exp,log:complex",          Function[{Typed[x,Complex]},{Exp[x],Log[x],Log10[x],Log2[x]}],{{-1.5+1.5I},{0.-2.2I},{1.5-0.5I},{5.3+1.1I}},Equal]&]
registertest[test["power:real",               Function[{Typed[x,Real],Typed[y,Real]},x^y],{{2.5,2.5},{2.5,-2.5}},Equal]&]
registertest[test["power:complex",            Function[{Typed[x,Complex],Typed[y,Complex]},x^y],{{2.5+1.5I,2.5-1.5I},{2.5-1.5I,-2.5+1.5I}},Equal]&]
registertest[test["sqrt:real",                Function[{Typed[x,Real]},Sqrt[x]],{{0.0},{0.01},{1.3},{135.7}},Equal]&]
registertest[test["sqrt:complex",             Function[{Typed[x,Complex]},Sqrt[x]],{{1.5I},{2.5-1.5I},{2.5-1.5I},{-2.5+1.5I}},Equal]&]
registertest[test["logistic_sigmoid",         Function[{Typed[x,Complex]},LogisticSigmoid[x]],{{1.5I},{2.5-1.5I},{2.5-1.5I},{-2.5+1.5I}},Equal]&]
registertest[test["trig:real",                Function[{Typed[x,Real]},{Sin[x],Cos[x],Tan[x],Cot[x],Sec[x],Csc[x]}],{{0.3},{0.5},{0.8}},Equal]&]
registertest[test["trig:complex",             Function[{Typed[x,Complex]},{Sin[x],Cos[x],Tan[x],Cot[x],Sec[x],Csc[x]}],{{0.3+0.1I},{0.5+0.1I},{0.8+0.1I}},Equal]&]
registertest[test["triginv:real",             Function[{Typed[x,Real]},{ArcSin[x],ArcCos[x],ArcTan[x],ArcCot[x],ArcSec[1.+x],ArcCsc[1.+x]}],{{0.3},{0.5},{0.8}},Equal]&]
registertest[test["triginv:complex",          Function[{Typed[x,Complex]},{ArcSin[x],ArcCos[x],ArcTan[x],ArcCot[x],ArcSec[x],ArcCsc[x]}],{{0.3+0.1I},{0.5+0.1I},{0.8+0.1I}},Equal]&]
registertest[test["hyp:real",                 Function[{Typed[x,Real]},{Sinh[x],Cosh[x],Tanh[x],Coth[x],Sech[x],Csch[x]}],{{0.3},{0.5},{0.8}},Equal]&]
registertest[test["hyp:complex",              Function[{Typed[x,Complex]},{Sinh[x],Cosh[x],Tanh[x],Coth[x],Sech[x],Csch[x]}],{{0.3+0.1I},{0.5+0.1I},{0.8+0.1I}},Equal]&]
registertest[test["hypinv:real",              Function[{Typed[x,Real]},{ArcSinh[x],ArcCosh[1.+x],ArcTanh[x],ArcCoth[1.+x],ArcSech[x],ArcCsch[x]}],{{0.3},{0.5},{0.8}},Equal]&]
registertest[test["hypinv:complex",           Function[{Typed[x,Complex]},{ArcSinh[x],ArcCosh[x],ArcTanh[x],ArcCoth[x],ArcSech[x],ArcCsch[x]}],{{0.3+0.1I},{0.5+0.1I},{0.8+0.1I}},Equal]&]
registertest[test["trigderived:real",         Function[{Typed[x,Real]},{Haversine[x],InverseHaversine[x],Gudermannian[x],InverseGudermannian[x],Sinc[x]}],{{0.3},{0.5},{0.8}},Equal]&]
registertest[test["trigderived:complex",      Function[{Typed[x,Complex]},{Haversine[x],InverseHaversine[x],Gudermannian[x],InverseGudermannian[x],Sinc[x]}],{{0.3+0.1I},{0.5+0.1I},{0.8+0.1I}},Equal]&]

registertest[test["gamma,log_gamma",          Function[{Typed[x,Real]},{Gamma[x],LogGamma[x]}],{{0.001},{0.08},{1.5},{5.3}},Equal]&]
registertest[test["erf,erfc",                 Function[{Typed[x,Real]},{Erf[x],Erfc[x]}],{{0.001},{0.08},{1.5},{5.3}},Equal]&]
registertest[test["beta",                     Function[{Typed[x,Real],Typed[y,Real]},Beta[x,y]],{{5.0,4.0},{2.3,3.2}},Equal]&]
registertest[test["zeta",                     Function[{Typed[x,Real]},Zeta[x]],{{0.001},{0.08},{1.5},{5.3}},Equal]&]

registertest[test["not",                      Function[{Typed[x,Boolean]},Not[x]],{{True},{False}}]&];
registertest[test["and",                      Function[{Typed[x,{Integer,1}]},And@@Map[#>0&,x]],List/@Tuples[{0,1},5]]&];
registertest[test["or",                       Function[{Typed[x,{Integer,1}]},Or@@Map[#>0&,x]],List/@Tuples[{0,1},5]]&];
registertest[test["xor",                      Function[{Typed[x,{Integer,1}]},Xor@@Map[#>0&,x]],List/@Tuples[{0,1},5]]&];
registertest[test["nand",                     Function[{Typed[x,{Integer,1}]},Nand@@Map[#>0&,x]],List/@Tuples[{0,1},5]]&];
registertest[test["nor",                      Function[{Typed[x,{Integer,1}]},Nor@@Map[#>0&,x]],List/@Tuples[{0,1},5]]&];
registertest[test["xnor",                     Function[{Typed[x,{Integer,1}]},Xnor@@Map[#>0&,x]],List/@Tuples[{0,1},5]]&];
registertest[test["implies",                  Function[{Typed[x,Boolean],Typed[y,Boolean]},Implies[x,y]],{{True,True},{True,False},{False,True},{False,False}}]&];
registertest[test["boole",                    Function[{Typed[x,Boolean]},Boole[x]],{{True},{False}}]&];
registertest[test["bit_not",                  Function[{Typed[x,{Integer,1}]},BitNot[x]],{{RandomInteger[{-1000000,1000000},20]}}]&];
registertest[test["bit_and",                  Function[{Typed[x,{Integer,2}]},BitAnd@@x],{{RandomInteger[{-1*^8,1*^8},{5,5}]}}]&];
registertest[test["bit_or",                   Function[{Typed[x,{Integer,2}]},BitOr@@x],{{RandomInteger[{-1*^8,1*^8},{5,5}]}}]&];
registertest[test["bit_xor",                  Function[{Typed[x,{Integer,2}]},BitXor@@x],{{RandomInteger[{-1*^8,1*^8},{5,5}]}}]&];
registertest[test["bit_length",               Function[{Typed[x,{Integer,1}]},BitLength[x]],{{RandomInteger[{-1000000,1000000},20]}}]&];

registertest[test["even_q:int",               Function[{Typed[x,Integer]},EvenQ[x]],List/@Range[-10,10]]&];
registertest[test["even_q:real",              Function[{Typed[x,Real]},EvenQ[x]],({#}->EvenQ@Rationalize[#])&/@N@Range[0,5,0.25]]&];
registertest[test["odd_q:int",                Function[{Typed[x,Integer]},OddQ[x]],List/@Range[-10,10]]&];
registertest[test["odd_q:real",               Function[{Typed[x,Real]},OddQ[x]],({#}->OddQ@Rationalize[#])&/@N@Range[0,5,0.25]]&];
registertest[test["divisible",                Function[{Typed[x,Integer],Typed[y,Integer]},Divisible[x,y]],DeleteCases[RandomInteger[{-20,20},{400,2}],{_,0}]]&];
registertest[test["fibonacci",                Function[{Typed[x,Integer]},Fibonacci[x]],List/@Range[-90,90]]&];
registertest[test["lucas_l",                  Function[{Typed[x,{Integer,1}]},LucasL[x]],{{Range[-90,90]}}]&];
registertest[test["integer_digits:1args",     Function[{Typed[x,Integer]},IntegerDigits[x]],{{-58127},{0},{58127}}]&];
registertest[test["integer_digits:2args",     Function[{Typed[x,Integer],Typed[y,Integer]},IntegerDigits[x,y]],{{-58127,2},{0,2},{12345,8},{58127,17}}]&];
registertest[test["integer_digits:3args",     Function[{Typed[x,Integer],Typed[y,Integer],Typed[n,Integer]},IntegerDigits[x,y,n]],{{58127,2,10},{0,2,5},{12345,8,0},{58127,17,10}}]&];

registertest[test["complex",                  Function[{Typed[x,Real],Typed[y,Real]},Complex[x,y]],RandomReal[{-10,10},{100,2}]]&];
registertest[test["re:real",                  Function[{Typed[x,{Real,1}]},Re[x]],{{RandomReal[{-10,10},100]}}]&];
registertest[test["re:complex",               Function[{Typed[x,{Complex,1}]},Re[x]],{{RandomComplex[{-10-10I,10+10I},100]}}]&];
registertest[test["im:real",                  Function[{Typed[x,{Real,1}]},Im[x]],{{RandomReal[{-10,10},100]}->ConstantArray[0.,100]}]&];
registertest[test["im:complex",               Function[{Typed[x,{Complex,1}]},Im[x]],{{RandomComplex[{-10-10I,10+10I},100]}}]&];
registertest[test["arg:real",                 Function[{Typed[x,{Real,1}]},Arg[x]],{{#}->N@Arg[#]&@RandomReal[{-10,10},100]}]&];
registertest[test["arg:complex",              Function[{Typed[x,{Complex,1}]},Arg[x]],{{RandomComplex[{-10-10I,10+10I},100]}}]&];
registertest[test["conjugate:real",           Function[{Typed[x,{Real,1}]},Conjugate[x]],{{RandomReal[{-10,10},100]}}]&];
registertest[test["conjugate:complex",        Function[{Typed[x,{Complex,1}]},Conjugate[x]],{{RandomComplex[{-10-10I,10+10I},100]}}]&];
registertest[test["re_im:real",               Function[{Typed[x,{Real,1}]},ReIm[x]],{{#}->N@ReIm[#]&@RandomReal[{-10,10},100]}]&];
registertest[test["re_im:complex",            Function[{Typed[x,{Complex,1}]},ReIm[x]],{{RandomComplex[{-10-10I,10+10I},100]}}]&];
registertest[test["abs_arg:real",             Function[{Typed[x,{Real,1}]},AbsArg[x]],{{#}->N@AbsArg[#]&@RandomReal[{-10,10},100]}]&];
registertest[test["abs_arg:complex",          Function[{Typed[x,{Complex,1}]},AbsArg[x]],{{RandomComplex[{-10-10I,10+10I},100]}}]&];

registertest[test["list:basic",               Function[{Typed[x,Integer]},{x,x,x,x,x}],{{3},{-5}}]&];
registertest[test["list:inline",              Function[{},{1,2,3,4,5,6,7}],{{}}]&];
registertest[test["list:level2",              Function[{},{Range[5],Range[5]+1,Range[5]-8}],{{}}]&];
registertest[test["list:mixed",               Function[{},{Range[5],{7,6,5,4,3},Range[5]-8}],{{}}]&];
registertest[test["list:nested",              Function[{},{{{1,2,3,4},{5,6,7,8},{9,10,11,12}}}],{{}}]&];
registertest[test["dimensions:scalar",        Function[{},Dimensions[All]],{{}}]&];
registertest[test["dimensions:array",         Function[{Typed[x,{Integer,4}]},Dimensions[x]],{{RandomInteger[10,{4,5,6,7}]}}]&];
registertest[test["length:scalar",            Function[{},Length[5+6I]],{{}}]&];
registertest[test["length:array",             Function[{Typed[x,{Integer,4}]},Length[x]],{{RandomInteger[10,{4,5,6,7}]}}]&];
registertest[test["array_depth:scalar",       Function[{},ArrayDepth[5+6I]],{{}}]&];
registertest[test["array_depth:array",        Function[{Typed[x,{Integer,4}]},ArrayDepth[x]],{{RandomInteger[10,{4,5,6,7}]}}]&];
registertest[test["vector_q:basic",           Function[{},Boole@{VectorQ[{1,2,3,4,5}],VectorQ[{{1,2},{3,4}}]}],{{}}]&];
registertest[test["vector_q:test,level1",     Function[{Typed[x,{Integer,1}]},VectorQ[x,#>0&]],List/@RandomInteger[{0,5},{20,5}]]&];
registertest[test["vector_q:test,level2",     Function[{Typed[x,{Integer,2}]},VectorQ[x,#[[1]]>0&]],List/@RandomInteger[{0,5},{20,5,5}]]&];
registertest[test["matrix_q:basic",           Function[{},Boole@{MatrixQ[{1,2,3,4,5}],MatrixQ[{{1,2},{3,4}}],MatrixQ[{{{1,2},{3,4}}}]}],{{}}]&];
registertest[test["matrix_q:test,level2",     Function[{Typed[x,{Integer,2}]},MatrixQ[x,#>0&]],List/@RandomInteger[{0,25},{20,5,5}]]&];
registertest[test["matrix_q:test,level3",     Function[{Typed[x,{Integer,3}]},MatrixQ[x,#[[1]]>0&]],List/@RandomInteger[{0,5},{20,2,2,5}]]&];
registertest[test["table:basic",              Function[{},Table[i+j+k+l,{i,4},{j,7,9},2,{k,0.5,1.5,0.5},{l,{-1,108,3412}}]],{{}}]&];
registertest[test["table:list",               Function[{},Table[{Range[j+k+l,j+k+l+4]},{j,7,9},2,{k,0.5,1.5,0.5},{l,{-1,108,3412}}]],{{}}]&];
registertest[test["constant_array",           Function[{},ConstantArray[Sin[1.2],{4,5,6,7}]],{{}}]&];
registertest[test["range:1arg",               Function[{Typed[x,Integer]},Range[x]],{{1},{10},{100},{0},{-10}}]&];
registertest[test["range:2args",              Function[{Typed[x,Real],Typed[y,Real]},Range[x,y]],{{1.0,5.0},{-10.5,10.5},{3.1,3.5},{5.0,5.0},{5.0,4.9}}]&];
registertest[test["range:3args",              Function[{Typed[x,Real],Typed[y,Real],Typed[z,Real]},Range[x,y,z]],{{1.2,2.2,0.15},{10.0,1.0,-1.0},{0.,1.,0.11}}]&];
registertest[test["part:element",             Function[{},Module[{x=ArrayReshape[Range[6*7*8*9],{6,7,8,9}]},x[[1,2,-3,-4]]=-1;x[[-1,-2,3,4]]=-2;x]],{{}}]&];
registertest[test["part:simple",              Function[{},Module[{x=ArrayReshape[Range[6*7*8*9],{6,7,8,9}]},x[[3,4,1,;;4]]=-1;x[[3,4,4;;5,;;]]=-2;x[[1,-2,All,All]]=-3;x]],{{}}]&];
registertest[test["part:regular",             Function[{},Module[{x=ArrayReshape[Range[6*7*8*9],{6,7,8,9}]},x[[1,4,2,;;;;3]]=-1;x[[1,;;,;;,5]]=-2;x]],{{}}]&];
registertest[test["part:general",             Function[{},Module[{x=ArrayReshape[Range[6*7*8*9],{6,7,8,9}]},x[[1,4,{1,2,-8,3,-1,-2,-1},;;;;3]]=-1;x[[6,;;;;2,;;;;2,9;;2;;-2]]=-2;x]],{{}}]&];
registertest[test["part:alias",               Function[{},Module[{x=ArrayReshape[Range[6*7*8*9],{6,7,8,9}]},x[[1;;5,1;;5]]=x[[2;;6,3;;7]];x[[1,2,3;;8,;;]]=x[[1,2,1;;6,;;]];x[[3,1;;4,;;,2]]=x[[3,2;;5,;;,3]];x[[3,;;5,;;5,1]]=x[[3,2;;6,4,2;;6]];x]],{{}}]&];
registertest[test["append:scalar",            Function[{},Append[Range[5],1]],{{}}]&];
registertest[test["append:list",              Function[{},Append[{{1,2},{3,4}},{5,6}]],{{}}]&];
registertest[test["prepend:scalar",           Function[{},Append[Range[5],1]],{{}}]&];
registertest[test["prepend:list",             Function[{},Append[{{1,2},{3,4}},{5,6}]],{{}}]&];
registertest[test["insert:scalar,single",     Function[{},Insert[Range[5],-1,4]],{{}}]&];
registertest[test["insert:scalar,multi",      Function[{},Insert[Range[5],-1,{{1},{6},{2},{2},{4},{-2}}]],{{}}]&];
registertest[test["insert:list,single",       Function[{},Delete[{{1,2},{3,4},{5,6},{7,8}},3]],{{}}]&];
registertest[test["insert:list,multi",        Function[{},Delete[{{1,2},{3,4},{5,6},{7,8}},{{4},{2},{2},{4},{-2}}]],{{}}]&];
registertest[test["join:basic",               Function[{},Join[{1,2,3},{4,5},{6,7,8}]],{{}}]&];
registertest[test["join:rank2",               Function[{},Join[{{5,6},{7,8}}, {{1,2},{3,4}}]],{{}}]&];
registertest[test["join:level2",              Function[{},Join[{{5,6},{7,8}}, {{1,2},{3,4}},2]],{{}}]&];
registertest[test["join:varg",                Function[{},Join[#2,##,#1,2]&@@N@{{{5,6},{7,8}}, {{1,2},{3,4}}}],{{}}]&];
registertest[test["total:basic",              Function[{Typed[x,{Integer,4}]},Total[x]],{{RandomInteger[10,{5,6,7,8}]}}]&];
registertest[test["total:level2",             Function[{Typed[x,{Integer,4}]},Total[x,{2}]],{{RandomInteger[10,{5,6,7,8}]}}]&];
registertest[test["total:level1,2",           Function[{Typed[x,{Integer,4}]},Total[x,2]],{{RandomInteger[10,{5,6,7,8}]}}]&];
registertest[test["total:level-1",            Function[{Typed[x,{Integer,4}]},Total[x,{-1}]],{{RandomInteger[10,{5,6,7,8}]}}]&];
registertest[test["total:level2,-1",          Function[{Typed[x,{Integer,4}]},Total[x,{2,-1}]],{{RandomInteger[10,{5,6,7,8}]}}]&];
registertest[test["total:level1,-2",          Function[{Typed[x,{Integer,4}]},Total[x,-2]],{{RandomInteger[10,{5,6,7,8}]}}]&];
registertest[test["total:levelinf",           Function[{Typed[x,{Integer,4}]},Total[x,Infinity]],{{RandomInteger[10,{5,6,7,8}]}}]&];
registertest[test["mean:scalar",              Function[{Typed[x,{Integer,1}]},N@Mean[x]],{{RandomInteger[10,{100}]}},Equal]&];
registertest[test["mean:list",                Function[{Typed[x,{Integer,2}]},N@Mean[x]],{{RandomInteger[10,{10,10}]}},Equal]&];
registertest[test["dot:vv",                   Function[{Typed[x,{Real,1}],Typed[y,{Real,1}]},x.y],{{RandomReal[1,{10}],RandomReal[1,{10}]}},Equal]&];
registertest[test["dot:vm",                   Function[{Typed[x,{Real,1}],Typed[y,{Real,2}]},x.y],{{RandomReal[1,{10}],RandomReal[1,{10,13}]}},Equal]&];
registertest[test["dot:mv",                   Function[{Typed[x,{Real,2}],Typed[y,{Real,1}]},x.y],{{RandomReal[1,{10,13}],RandomReal[1,{13}]}},Equal]&];
registertest[test["dot:mm",                   Function[{Typed[x,{Real,2}],Typed[y,{Real,2}]},x.y],{{RandomReal[1,{10,13}],RandomReal[1,{13,16}]}},Equal]&];
registertest[test["dot:varg",                 Function[{Typed[x,{Real,3}],Typed[y,{Real,3}]},x.y.x.y],{{RandomReal[1,{4,5,6}],RandomReal[1,{6,5,4}]}},Equal]&];
registertest[test["inner:plus,times",         Function[{Typed[x,{Real,3}],Typed[y,{Real,3}]},Inner[Plus,x,y,Times]],{{RandomReal[1,{4,5,6}],RandomReal[1,{6,5,4}]}},Equal]&];
registertest[test["inner:or,and",             Function[{Typed[x,{Integer,2}],Typed[y,{Integer,2}]},Boole@Inner[Or,Map[#>0&,x,{2}],Map[#>0&,y,{2}],And]],{{RandomInteger[3,{8,10}],RandomInteger[3,{10,12}]}},Equal]&];
registertest[test["tr:rank1",                 Function[{Typed[x,{Integer,1}]},Tr[x]],{{RandomInteger[10,{10}]}}]&];
registertest[test["tr:rank2",                 Function[{Typed[x,{Integer,2}]},Tr[x]],{{RandomInteger[10,{10,10}]}}]&];
registertest[test["tr:rank3",                 Function[{Typed[x,{Integer,3}]},Tr[x]],{{RandomInteger[10,{10,11,12}]}}]&];
registertest[test["tr:list",                  Function[{Typed[x,{Integer,3}]},Tr[x,List]],{{RandomInteger[10,{5,5,5}]}}]&];
registertest[test["tr:times,level2",          Function[{Typed[x,{Integer,3}]},Tr[x,Times,2]],{{RandomInteger[10,{5,5,5}]}}]&];
registertest[test["reverse:basic",            Function[{Typed[x,{Integer,3}]},Reverse[x]],{{RandomInteger[10,{6,7,8}]}}]&];
registertest[test["reverse:level2",           Function[{Typed[x,{Integer,3}]},Reverse[x,2]],{{RandomInteger[10,{6,7,8}]}}]&];
registertest[test["reverse:level3",           Function[{Typed[x,{Integer,3}]},Reverse[x,3]],{{RandomInteger[10,{6,7,8}]}}]&];
registertest[test["partition:basic",          Function[{Typed[x,{Integer,1}]},Partition[x,17]],{{RandomInteger[10,{100}]}}]&];
registertest[test["partition:offset",         Function[{Typed[x,{Integer,1}]},Partition[x,17,7]],{{RandomInteger[10,{100}]}}]&];
registertest[test["partition:rank2",          Function[{Typed[x,{Integer,2}]},Partition[x,{3,3},2]],{{RandomInteger[10,{8,8}]}}]&];
registertest[test["partition:rank3",          Function[{Typed[x,{Integer,3}]},Partition[x,{3,4,5},{5,2,1}]],{{RandomInteger[10,{8,9,10}]}}]&];
registertest[test["array_reshape:basic",      Function[{Typed[x,{Integer,1}]},ArrayReshape[x,{5,6}]],{{Range[23]},{Range[30]},{Range[1234]}}]&];
registertest[test["array_reshape:padding",    Function[{Typed[x,{Integer,3}]},ArrayReshape[x,{2,3,4,5},-10]],{{RandomInteger[10,{4,4,4}]},{RandomInteger[10,{4,6,5}]},{RandomInteger[10,{7,8,9}]}}]&];
registertest[test["first:basic",              Function[{Typed[x,{Integer,1}]},First[x]],{{Range[1]},{Range[10]},{Range[1000]}}]&];
registertest[test["first:rank3",              Function[{Typed[x,{Integer,3}]},First[x]],{{RandomInteger[10,{1,5,5}]},{RandomInteger[10,{6,5,5}]}}]&];
registertest[test["last:basic",               Function[{Typed[x,{Integer,1}]},Last[x]],{{Range[1]},{Range[10]},{Range[1000]}}]&];
registertest[test["last:rank3",               Function[{Typed[x,{Integer,3}]},Last[x]],{{RandomInteger[10,{1,5,5}]},{RandomInteger[10,{6,5,5}]}}]&];
registertest[test["most:basic",               Function[{Typed[x,{Integer,1}]},Most[x]],{{Range[1]},{Range[10]},{Range[1000]}}]&];
registertest[test["most:rank3",               Function[{Typed[x,{Integer,3}]},Most[x]],{{RandomInteger[10,{1,5,5}]},{RandomInteger[10,{6,5,5}]}}]&];
registertest[test["rest:basic",               Function[{Typed[x,{Integer,1}]},Rest[x]],{{Range[1]},{Range[10]},{Range[1000]}}]&];
registertest[test["rest:rank3",               Function[{Typed[x,{Integer,3}]},Rest[x]],{{RandomInteger[10,{1,5,5}]},{RandomInteger[10,{6,5,5}]}}]&];
registertest[test["transpose:basic",          Function[{Typed[x,{Integer,2}]},Transpose[x]],{{RandomInteger[10,{9,16}]}}]&];
registertest[test["transpose:rank3,level3",   Function[{Typed[x,{Integer,3}]},Transpose[x,{2,3,1}]],{{RandomInteger[10,{9,16,25}]}}]&];
registertest[test["transpose:rank3,level2",   Function[{Typed[x,{Integer,3}]},Transpose[x,{2,1}]],{{RandomInteger[10,{9,16,25}]}}]&];
registertest[test["transpose:rank6,level6",   Function[{Typed[x,{Integer,6}]},Transpose[x,{4,3,2,6,5,1}]],{{RandomInteger[10,{2,3,4,2,3,4}]}}]&];
registertest[test["transpose:collapse",       Function[{Typed[x,{Integer,6}]},Transpose[x,{2,3,1,2,3}]],{{RandomInteger[10,{2,3,4,2,3,4}]}}]&];
registertest[test["conjugate_transpose:basic",Function[{Typed[x,{Complex,2}]},ConjugateTranspose[x]],{{RandomComplex[1+I,{5,6}]}}]&];
registertest[test["conjugate_transpose:real", Function[{Typed[x,{Real,2}]},ConjugateTranspose[x]],{{RandomReal[1,{5,6}]}}]&];
registertest[test["flatten:basic",            Function[{Typed[x,{Integer,6}]},Flatten[x]],{{RandomInteger[10,{2,3,4,2,3,4}]}}]&];
registertest[test["flatten:level3",           Function[{Typed[x,{Integer,6}]},Flatten[x,3]],{{RandomInteger[10,{2,3,4,2,3,4}]}}]&];
registertest[test["flatten:arbitrary1",       Function[{Typed[x,{Integer,6}]},Flatten[x,{{1,6},{4,3},{2}}]],{{RandomInteger[10,{2,3,4,2,3,4}]}}]&];
registertest[test["flatten:arbitrary2",       Function[{Typed[x,{Integer,6}]},Flatten[x,{{5,1,2,3},{6,4}}]],{{RandomInteger[10,{2,3,4,2,3,4}]}}]&];
registertest[test["order:scalar",             Function[{},{Order[1,1],Order[0,1],Order[1,0],Order[1.0+1.0I,1.0+1.1I],Order[1.0+1.0I,0.9+1.1I],Order[1.0+1.0I,1.0-1.0I],Order[1.0+1.0I,1.0+1.0I]}],{{}}]&];
registertest[test["order:list",               Function[{},{Order[{1,2,3,4},{1,2,3,5}],Order[{1,2,3,4},{1,2,4,1}],Order[{1,2},{1,2,3}],Order[{{1,2},{3,4}},{{1,2,3},{4,5,6}}]}],{{}}]&];
registertest[test["ordering:basic",           Function[{Typed[x,{Real,2}]},Ordering[x]],{{RandomReal[1.,{100,10}]}}]&];
registertest[test["ordering:first",           Function[{Typed[x,{Real,2}]},Ordering[x,9]],{{RandomReal[1.,{100,10}]}}]&];
registertest[test["ordering:last",            Function[{Typed[x,{Real,2}]},Ordering[x,-9]],{{RandomReal[1.,{100,10}]}}]&];
registertest[test["ordering:func",            Function[{Typed[x,{Real,2}]},Ordering[x,25,Last[#1]<Last[#2]&]],{{RandomReal[1.,{100,10}]}}]&];
registertest[test["sort:basic",               Function[{Typed[x,{Real,2}]},Sort[x]],{{RandomReal[1.,{100,10}]}}]&];
registertest[test["sort:func",                Function[{Typed[x,{Real,2}]},Sort[x,Order[#1[[-3;;]],#2[[-3;;]]]&]],{{RandomReal[1.,{100,10}]}}]&];
registertest[test["union:1arg",               Function[{Typed[x,{Integer,1}]},Union[x]],{{RandomInteger[10,{100}]}}]&];
registertest[test["union:3args",              Function[{Typed[x,{Integer,1}],Typed[y,{Integer,1}]},Union[x,y,x]],{{RandomInteger[10,{10}],RandomInteger[10,{10}]}}]&];
registertest[test["union:vargs",              Function[{Typed[x,{Integer,2}]},Union@@x],{{RandomInteger[100,{10,20}]}}]&];
registertest[test["rotate_left:basic",        Function[{Typed[x,{Integer,2}]},RotateLeft[x]],{{RandomInteger[10,{10,3}]}}]&];
registertest[test["rotate_left:n",            Function[{Typed[x,{Integer,2}],Typed[n,Integer]},RotateLeft[x,n]],Table[{RandomInteger[10,{10,3}],i},{i,-24,24,3}]]&];
registertest[test["rotate_left:empty",        Function[{Typed[x,{Integer,3}]},RotateLeft[x,3]],{{RandomInteger[10,{0,3,4}]}}]&];
registertest[test["rotate_right:basic",       Function[{Typed[x,{Integer,2}]},RotateRight[x]],{{RandomInteger[10,{10,3}]}}]&];
registertest[test["rotate_right:n",           Function[{Typed[x,{Integer,2}],Typed[n,Integer]},RotateRight[x,n]],Table[{RandomInteger[10,{10,3}],i},{i,-24,24,3}]]&];
registertest[test["rotate_right:empty",       Function[{Typed[x,{Integer,3}]},RotateRight[x,3]],{{RandomInteger[10,{0,3,4}]}}]&];
registertest[test["position:basic",           Function[{Typed[x,{Integer,1}]},Position[x,5]],{{RandomInteger[10,100]}}]&];
registertest[test["position:level2",          Function[{Typed[x,{Integer,2}]},Position[x,5]],{{RandomInteger[10,{10,10}]}}]&];
registertest[test["position:list",            Function[{Typed[x,{Integer,3}]},Position[x,{3,3}]],{{RandomInteger[3,{30,30,2}]}}]&];
registertest[test["position:pattern1",        Function[{Typed[x,{Integer,3}]},Position[x,_?(Total[#,{2}][[1]]>=5&),{1}]],{{RandomInteger[3,{30,30,2}]}}]&];
registertest[test["position:pattern2",        Function[{Typed[x,{Integer,3}]},Position[x,_?(Total[#]>=5&),{2}]],{{RandomInteger[3,{30,30,2}]}}]&];
registertest[test["cases:basic",              Function[{Typed[x,{Integer,1}]},Cases[x,5]],{{RandomInteger[10,100]}}]&];
registertest[test["cases:level2",             Function[{Typed[x,{Integer,2}]},Cases[x,5,{2}]],{{RandomInteger[10,{10,10}]}}]&];
registertest[test["cases:list",               Function[{Typed[x,{Integer,3}]},Cases[x,{3,3},{2}]],{{RandomInteger[3,{30,30,2}]}}]&];
registertest[test["cases:pattern1",           Function[{Typed[x,{Integer,3}]},Cases[x,_?(Total[#,{2}][[1]]>=5&),{1}]],{{RandomInteger[3,{30,30,2}]}}]&];
registertest[test["cases:pattern2",           Function[{Typed[x,{Integer,3}]},Cases[x,_?(Total[#]>=5&),{2}]],{{RandomInteger[3,{30,30,2}]}}]&];
registertest[test["delete_cases:basic",       Function[{Typed[x,{Integer,1}]},DeleteCases[x,5]],{{RandomInteger[10,100]}}]&];
registertest[test["delete_cases:list",        Function[{Typed[x,{Integer,2}]},DeleteCases[x,{1,2}]],{{RandomInteger[2,{100,2}]}}]&];
registertest[test["delete_cases:pattern",     Function[{Typed[x,{Integer,2}]},DeleteCases[x,_?(Total[#]>50&)]],{{RandomInteger[10,{50,10}]}}]&];
registertest[test["free_q:scalar",            Function[{Typed[x,{Integer,2}]},Boole[FreeQ[#,5]&/@x]],{{RandomInteger[10,{100,10}]}}]&];
registertest[test["free_q:list",              Function[{Typed[x,{Integer,3}]},Boole[FreeQ[#,{0,1,2}]&/@x]],{{RandomInteger[2,{100,20,3}]}}]&];
registertest[test["member_q:scalar",          Function[{Typed[x,{Integer,2}]},Boole[MemberQ[#,5]&/@x]],{{RandomInteger[10,{100,10}]}}]&];
registertest[test["member_q:list",            Function[{Typed[x,{Integer,3}]},Boole[MemberQ[#,{0,1,2}]&/@x]],{{RandomInteger[2,{100,20,3}]}}]&];
registertest[test["member_q:pattern",         Function[{Typed[x,{Integer,3}]},Boole[MemberQ[#,_?(#[[2]]==8&),{1}]&/@x]],{{RandomInteger[20,{100,20,3}]}}]&];

registertest[test["random_integer",           Function[{},RandomInteger[{-20,20},10000]],{{}},Abs[Mean@N[#]-0.0]<1.0&&Abs[StandardDeviation@N[#]-11.83]<0.5&]&];
registertest[test["random_real",              Function[{},RandomReal[{-20,20},10000]],{{}},Abs[Mean[#]-0.0]<1.0&&Abs[StandardDeviation[#]-11.83]<0.5&]&];
registertest[test["random_complex",           Function[{},RandomComplex[{-2-I,2+I},10000]],{{}},Abs@Mean[#]<0.1&&Abs[Abs@StandardDeviation[#]-1.291]<0.05&]&];
registertest[test["random_choice:scalar,1",   Function[{},Table[RandomChoice[Range[-20,20]],10000]],{{}},Abs[Mean@N[#]-0.0]<1.0&&Abs[StandardDeviation@N[#]-11.83]<0.5&]&];
registertest[test["random_choice:scalar,n",   Function[{},RandomChoice[Range[-20,20],10000]],{{}},Abs[Mean@N[#]-0.0]<1.0&&Abs[StandardDeviation@N[#]-11.83]<0.5&]&];
registertest[test["random_choice:list,1",     Function[{},Module[{x=Flatten[Table[{i,j},{i,0,9},{j,0,9}],1]},Table[RandomChoice[x],10000]]],{{}},Abs[Mean@N[#.{10,1}]-49.5]<2.8&&Abs[StandardDeviation@N[#.{10,1}]-28.86]<1.3&]&];
registertest[test["random_choice:list,n",     Function[{},RandomChoice[Flatten[Table[{i,j},{i,0,9},{j,0,9}],1],10000]],{{}},Abs[Mean@N[#.{10,1}]-49.5]<2.8&&Abs[StandardDeviation@N[#.{10,1}]-28.86]<1.3&]&];
registertest[test["random_choice:weights,n",  Function[{},RandomChoice[{10,0,1,2,3}->{1,2,3,4,5},10000]],{{}},Abs[Mean@N[#]-2.249]<0.17&&Abs[StandardDeviation@N[#]-1.677]<0.07&]&];
registertest[test["random_choice:weights,1",  Function[{},Table[RandomChoice[{10,0,1,2,3}->{1,2,3,4,5}],10000]],{{}},Abs[Mean@N[#]-2.249]<0.17&&Abs[StandardDeviation@N[#]-1.677]<0.07&]&];
registertest[test["random_sample:basic",      Function[{},Table[RandomSample[Range[-20,20]],100]],{{}},Mean@N[Flatten@#]==0.0&&AllTrue[#,DuplicateFreeQ]&]&];
registertest[test["random_sample:short",      Function[{},Table[RandomSample[Range[1,1000],5],1000]],{{}},1<=Min[#]&&Max[#]<=1000&&AllTrue[#,DuplicateFreeQ]&]&];
registertest[test["random_sample:long",       Function[{},Table[RandomSample[Range[1,1000],500],100]],{{}},1<=Min[#]&&Max[#]<=1000&&AllTrue[#,DuplicateFreeQ]&]&];
registertest[test["random_sample:w,short",    Function[{},Table[RandomSample[Range[10]->Range[10],5],10000]],{{}},Max@Abs[Mean@N[#]-{7.003,6.859,6.695,6.486,6.224}]<0.27&&AllTrue[#,DuplicateFreeQ]&]&];
registertest[test["random_sample:w,long",     Function[{},Table[RandomSample[Range[40]*Range[40]->Range[40],8],10000]],{{}},Max@Abs[Mean@N[#]-{30.37,30.21,30.04,29.84,29.66,29.44,29.23,29.01}]<0.8&&AllTrue[#,DuplicateFreeQ]&]&];
registertest[test["uniform_dist:0args",       Function[{},RandomVariate[UniformDistribution[],10000]],{{}},Abs[Mean@N[#]-0.5]<0.01&&Abs[StandardDeviation@N[#]-0.2886]<0.004&]&];
registertest[test["uniform_dist:n",           Function[{},RandomVariate[UniformDistribution[10],1000]],{{}},0.<=Min[#]&&Max[#]<=1.&]&];
registertest[test["uniform_dist:n",           Function[{},RandomVariate[UniformDistribution[Transpose@{Range[0,9],Range[1,10]}],1000]],{{}},And@@Table[AllTrue[#[[;;,n]],Between[{n-1,n}]],{n,10}]&]&];
registertest[test["chi_square_dist",          Function[{},RandomVariate[ChiSquareDistribution[3.5],10000]],{{}},Abs[Mean@N[#]-3.5]<0.08&&Abs[StandardDeviation@N[#]-2.647]<0.10&]&];
registertest[test["normal_dist:0args",        Function[{},RandomVariate[NormalDistribution[],10000]],{{}},Abs[Mean@N[#]-0.0]<0.03&&Abs[StandardDeviation@N[#]-1.0]<0.02&]&];
registertest[test["normal_dist:2args",        Function[{},RandomVariate[NormalDistribution[3.1,5.2],10000]],{{}},Abs[Mean@N[#]-3.1]<0.15&&Abs[StandardDeviation@N[#]-5.2]<0.12&]&];
registertest[test["log_normal_dist",          Function[{},RandomVariate[LogNormalDistribution[3.1,5.2],10000]],{{}},Abs[Median@N[#]-22.3]<6.0&&Abs[MedianDeviation@N[#]-22.2]<6.0&]&];
registertest[test["cauchy_dist",              Function[{},RandomVariate[CauchyDistribution[1.1,3.2],10000]],{{}},Abs[Median@N[#]-1.1]<0.15&&Abs[MedianDeviation@N[#]-3.2]<0.15&]&];
registertest[test["student_t_dist:1arg",      Function[{},RandomVariate[StudentTDistribution[3.5],10000]],{{}},Abs[Mean@N[#]-0.0]<0.05&&Abs[StandardDeviation@N[#]-1.53]<0.2&]&];
registertest[test["student_t_dist:3args",     Function[{},RandomVariate[StudentTDistribution[3.1,5.2,3.7],10000]],{{}},Abs[Mean@N[#]-3.1]<0.3&&Abs[StandardDeviation@N[#]-7.65]<0.6&]&];
registertest[test["f_ratio_dist",             Function[{},RandomVariate[FRatioDistribution[9.5,5.5],10000]],{{}},Abs[Median@N[#]-1.055]<0.03&&Abs[MedianDeviation@N[#]-0.518]<0.03&]&];
registertest[test["exponential_dist",         Function[{},RandomVariate[ExponentialDistribution[0.7],10000]],{{}},Abs[Median@N[#]-0.990]<0.05&&Abs[MedianDeviation@N[#]-0.687]<0.03&]&];
registertest[test["poisson_dist",             Function[{},RandomVariate[PoissonDistribution[35],10000]],{{}},Abs[Median@N[#]-35]<0.001&&Abs[MedianDeviation@N[#]-4]<0.001&]&];
registertest[test["bernoulli_dist",           Function[{},RandomVariate[BernoulliDistribution[0.7],10000]],{{}},Abs[Mean@N[#]-0.7]<0.016&&Abs[StandardDeviation@N[#]-0.4583]<0.008&]&];
registertest[test["gamma_dist",               Function[{},RandomVariate[GammaDistribution[4.5,2.5],10000]],{{}},Abs[Median@N[#]-10.43]<0.25&&Abs[MedianDeviation@N[#]-3.36]<0.12&]&];
registertest[test["weibull_dist",             Function[{},RandomVariate[WeibullDistribution[3.5,5.5],10000]],{{}},Abs[Mean@N[#]-4.948]<0.05&&Abs[StandardDeviation@N[#]-1.566]<0.03&]&];
registertest[test["extreme_value_dist:0args", Function[{},RandomVariate[ExtremeValueDistribution[],10000]],{{}},Abs[Mean@N[#]-0.578]<0.04&&Abs[StandardDeviation@N[#]-1.282]<0.05&]&];
registertest[test["extreme_value_dist:2args", Function[{},RandomVariate[ExtremeValueDistribution[3.5,2.5],10000]],{{}},Abs[Mean@N[#]-4.932]<0.1&&Abs[StandardDeviation@N[#]-3.206]<0.1&]&];
registertest[test["geometric_dist",           Function[{},RandomVariate[GeometricDistribution[0.118],10000]],{{}},Abs[Median@N[#]-5]<0.001&&Abs[MedianDeviation@N[#]-4]<0.001&]&];
registertest[test["binomial_dist",            Function[{},RandomVariate[BinomialDistribution[30,0.3],10000]],{{}},Abs[Mean@N[#]-9.0]<0.08&&Abs[StandardDeviation@N[#]-2.509]<0.06&]&];
registertest[test["negative_binomial_dist",   Function[{},RandomVariate[NegativeBinomialDistribution[30,0.3],10000]],{{}},Abs[Mean@N[#]-70.0]<0.5&&Abs[StandardDeviation@N[#]-15.28]<0.4&]&];

