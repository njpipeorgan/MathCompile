MathCompile is a package that translates *Wolfram Language* functions into C++ code, and generate dynamic libraries that can be called in *Wolfram Language*. It is written mostly in *Wolfram Language* and utilize a C++ library for type deduction and implementation of the supported functions. 

MathCompile is focused on improving the availability of functional programming and covering most of the functionalities provided by the built-in compiler. Currently, MathCompile supports 9 constants and over 150 functions; see the [wiki page](https://github.com/njpipeorgan/MathCompile/wiki/Compilable-Constants-and-Functions) for the full list. 

# Prerequisite

- *Wolfram Mathematica* 11+

  - The support for more array types requires version 12.

- C++ compiler supporting C++17 standard (if `CompileToBinary` is used)

  - See this [wiki page](https://github.com/njpipeorgan/MathCompile/wiki/Prerequisites-for-C-Compiler) for the list of supported C++ compilers and their availabilities.

# In a nutshell

First, load the package:
```
<<MathCompile`
```
Compile a function using `CompileToBinary`:
```
cf=CompileToBinary[
  Function[{Typed[x,{Integer,1}]},Apply[Times,x]];
]
```
Use this compiled function just like a normal Wolfram Language funcion:
```
cf[{1,2,3,4}]    (* gives 24 *)
```

You can also check the intermediate C++ code using `CompileToCode`:
```
CompileToCode[
  Function[{Typed[x,{Integer,1}]},Apply[Times,x]];
]
```
The result is a C++ function named `main_function`:
```c++
auto main_function(const wl::ndarray<int64_t, 1>& v35) {
    return wl::val(wl::apply(WL_FUNCTION(wl::times), WL_PASS(v35)));
}
```
As an example, the disassembly compiled by gcc 7.4 and clang 5.0 is available through [Compiler Explorer](https://godbolt.org/z/tElm9M).

# Supported types

Specifying the argument types of the compiled function is necessary, in the form of `Typed[<argument>,<type>]`. The types of all other variables are deduced by the compiler when possible. 

**Integral types**

`Integer`, `"Integer8"`, `"Integer16"`, `"Integer32"`, `"Integer64"`

`"UnsignedInteger"`, `"UnsignedInteger8"`, `"UnsignedInteger16"`, `"UnsignedInteger32"`, `"UnsignedInteger64"`

**Floating-point types**

`Real`, `"Real32"`, `"Real64"`

`Complex`, `"ComplexReal32"`, `"ComplexReal64"`

**Array types**

`{<type>,<rank>}`, where `<type>` is an integral or floating-point type and rank a positive integer.

**Other types**

`"Boolean"`, `"Void"`
