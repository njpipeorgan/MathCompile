(* ::Package:: *)

registertest[test_]:=(AppendTo[$MathCompileTests,test];)

test[name_String,f_,pairs_]:=(
  Module[{mf,file},
    $CurrentMathCompileTest=name;
    mf=CompileToBinary[f];
    file=DeleteMissing@{Information[mf]["File"]};
    If[mf===$Failed||(Or@@(!TrueQ[
        If[Head[#]===Rule,(mf@@#[[1]])===#[[2]],(mf@@#)===(f@@#)]]&/@pairs)),
      AppendTo[$FailedMathCompileTests,name];
    ];
    Quiet@LibraryFunctionUnload[mf];
    If[FileExistsQ[#],DeleteFile[#]]&/@file;
  ])

runtests[]:=
  Block[{$FailedMathCompileTests={},i=0,n=Length@$MathCompileTests},
    PrintTemporary[Column[{ProgressIndicator[Dynamic[i/n]],Dynamic@$CurrentMathCompileTest}]];
    Do[$MathCompileTests[[i]][],{i,1,n}];<|"FailedTests"->$FailedMathCompileTests|>
  ];


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
registertest[test["prepend_to",               Function[{},Module[{x={{1,2},{3,4},{5,6}}},PrependTo[x,{7,8}]]],{}]&];
