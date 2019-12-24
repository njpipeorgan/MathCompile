(* ::Package:: *)

registertest[test_]:=(AppendTo[$MathCompileTests,test];)

test[name_String,f_,pairs_]:=(
  Module[{mf,file},
    $CurrentMathCompileTest=name;
    If[Length[pairs]==0,
      Echo[name<>" contains zero groups of arguments."];Abort[]];
    mf=CompileToBinary[f];
    file=DeleteMissing@{Information[mf]["File"]};
    If[mf===$Failed||(Or@@(!TrueQ[
        If[Head[#]===Rule,($LastReturn=mf@@#[[1]])===#[[2]],
          ($LastReturn=mf@@#)===(f@@#)]]&/@pairs)),
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

