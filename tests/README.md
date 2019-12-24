The package `LoadTests.wl` includes many tests for MathCompile. The tests are performed by calling `CompileToBinary` on *Wolfram Language* functions and compare the results evaluated by the compiled functions to those by native functions. 

As for now, the tests cover
 - loading and returning objects of various types, and
 - procedual programming functions
 - functional programming functions
 - arithmetic functions

To use the testing package, load both `MathCompile` package and this package, set the C++ compiler if needed, and call `runtests`:
```
<<"./MathCompile.wl";
<<"./tests/LoadTests.wl";
$CCompiler=Automatic;
runtests[]
```
