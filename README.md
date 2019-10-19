# MathCompile

MathCompile is a package that translates *Wolfram Language* functions into C++ code, and generate dynamic libraries that can be called in *Wolfram Language*.

It is written mostly in *Wolfram Language* and utilize a C++ library for type deduction and implementation of the supported functions.

**Note**: The project is in the progress of adding more functions.

## Prerequisite

- *Wolfram Mathematica* 11+

  - The support for more array types requires version 12.

- C++ compiler supporting C++17 features

  - g++ 7+, with `-std=c++1z` flag
  - icc 19+, with `-std=c++17` flag
  - msvc 19.20+ (VS 2019 16.0), with `/std:c++17` flag

## In a nutshell

First, load the package:
```
<<MathCompile`
```
Compile a function using `CompileToBinary`: (make sure you have a C++ compiler installed)
```
cf=CompileToBinary[
  Function[{Typed[x,{Integer,1}]},Apply[Times,x]];
]
```
Use this compiled function just like a normal Wolfram Language funcion:
```
cf[{1,2,3,4}]    (* gives 24 *)
```

You can also check the C++ code using `CompileToCode`:
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
You can see the C++ code compiled from this Compiler Explorer [link](https://godbolt.org/z/7A9O5O).

## Compilable constants and functions

Currently, 9 constants and over 100 functions are supported by MathCompile. See this [wiki page](https://github.com/njpipeorgan/MathCompile/wiki/Compilable-Constants-and-Functions) for the full list. They include common procedual and functional programming function like `Module` and `Map`, and many numerical functions like `Sin` and `Log`. 

## Supported types

| *Wolfram Laguage*       | C++                       |
| ----------------------- | ------------------------- |
| `"Boolean"`             | `bool`                    |
| `"Integer"`             | `int64_t`                 |
| `"Integer8"`            | `int8_t`                  |
| `"Integer16"`           | `int16_t`                 |
| `"Integer32"`           | `int32_t`                 |
| `"Integer64"`           | `int64_t`                 |
| `"UnsignedInteger"`     | `uint64_t`                |
| `"UnsignedInteger8"`    | `uint8_t`                 |
| `"UnsignedInteger16"`   | `uint16_t`                |
| `"UnsignedInteger32"`   | `uint32_t`                |
| `"UnsignedInteger64"`   | `uint64_t`                |
| `"Real"`                | `double`                  |
| `"Real32"`              | `float`                   |
| `"Real64"`              | `double`                  |
| `"Complex"`             | `std::complex<double>`    |
| `"ComplexReal32"`       | `std::complex<float>`     |
| `"ComplexReal64"`       | `std::complex<double>`    |
| `"Array"[type_, rank_]` | `wl::ndarray<type, rank>` |
