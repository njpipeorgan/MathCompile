# MathCompile

MathCompile is a package that translates *Wolfram Language* functions into C++ code. It is written mostly in *Wolfram Language* and utilize a C++ library for type deduction and implementation of the supported functions. 

**Note**: The project is in the progress of adding necessary components to the C++ library. 

## Prerequisite

- *Wolfram Mathematica* 11+
- C++ compiler supporting C++17 features

  - clang 5+, with `-std=c++1z` flag
  - g++ 7+, with `-std=c++1z` flag
  - icc 19+, with `-std=c++1z` flag
  - msvc 19.20+ (VS 2017 16.0), with `/std:c++17` flag

## In a nutshell

Load the package:
```
<<MathCompile`
```
Compile a function using `CompileToCode`:
```
CompileToCode[
  Function[{Typed[p, "Integer"]},
    Module[{f=If[p>0,#+p&,#-p&]},
      f[42]
    ]
  ]
]
```
The output is a C++ function, and you can see it compiles by this Compiler Explorer [link](https://godbolt.org/z/HEMhmS).
```c++
auto main_function(int64_t v49) {
    auto v48 = wl::val(wl::branch_if(wl::greater(v49, 0_i), [&] {
        return wl::val([&](auto v47, auto...) {
            return wl::val(wl::plus(v47, v49));
        });
    }, [&] {
        return wl::val([&](auto v46, auto...) {
            return wl::val(wl::plus(v46, wl::times(-1_i, v49)));
        });
    }));
    return wl::val(v48(42_i));
}
```

## Supported constants and functions

**Constants**
```
Null
All
Pi
E
Degree
EulerGamma
I
```
**Scope functions**
```
Module
```
**Control flow**
```
If
```
**Arithmetic functions**
```
Plus
Subtract
Times
Divide
AddTo
SubtractFrom
TimesBy
DivideBy
```
**Numerical functions**
```
N
Abs
Round
Ceiling
Floor
IntegerPart
FractionalPart
Greater
Less
Equal
Unequal
```
**Complex numbers**
```
Re (partial)
Im (partial)
Arg (partial)
Conjugate (partial)
```
**Random numbers**
```
RandomInteger
RandomReal
RandomComplex
```
**Functions with iterators**
```
Do
Table
Sum
Product
```
**Array generation**
```
List
ConstantArray
Range
```
**Array manipulation**
```
Set
Part
Span
Select
Count
```

## Supported types

| *Wolfram Laguage*       | C++                       |
| ----------------------- | ------------------------- |
| `"Void"`                | `wl::void_type`           |
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
| `"Complex64"`           | `std::complex<float>`     |
| `"Complex128"`          | `std::complex<double>`    |
| `"String"`              | `std::string`             |
| `"Array"[type_, rank_]` | `wl::ndarray<type, rank>` |
