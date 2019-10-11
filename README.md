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
Compile a function using `CompileToLibrary`: (make sure you have a C++ compiler installed)
```
cf=CompileToLibrary[
  Function[{Typed[x,"Integer"],Typed[y,"Integer"]},x+y]
]
```
Use this compiled function just like a normal Wolfram Language funcion:
```
cf[2,3]    (* gives 5 *)
```

You can also check the C++ code using `CompileToCode`:
```
CompileToCode[
  Function[{Typed[x,"Integer"],Typed[y,"Integer"]},x+y]
]
```
The result is a C++ function named `main_function`:
```c++
auto main_function(int64_t v37, int64_t v38) {
    return wl::val(wl::plus(v37, v38));
}
```
You can see the C++ code compiled from this Compiler Explorer [link](https://godbolt.org/z/iT7usM).

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
True
False
```
**Arithmetic functions**
```
Plus
Subtract
Times
Divide
```
**Procedual programming**
```
Set
Module
If
AddTo
SubtractFrom
TimesBy
DivideBy
```
**Functional programming**
```
Select
Count
```
**Loops**
```
Do
Sum
Product
```
**List construction**
```
List
Dimensions
Table
ConstantArray
Range
```
**List manipulation**
```
Part
Span
Total
Mean
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
Mod
Sign
Clip
Unitize
UnitStep
Greater
Less
GreaterEqual
LessEqual
Equal
Unequal
```
**Logical functions**
```
Not
And
Or
Xor
Nand
Nor
Xnor
Implies
Boole
```
**Complex numbers**
```
Complex
Re
Im
Arg
Conjugate
ReIm
AbsArg
```
**Random numbers**
```
RandomInteger
RandomReal
RandomComplex
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
| `"ComplexReal32"`       | `std::complex<float>`     |
| `"ComplexReal64"`       | `std::complex<double>`    |
| `"Array"[type_, rank_]` | `wl::ndarray<type, rank>` |
