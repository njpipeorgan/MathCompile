# MathCompile

MathCompile is a package that translates *Wolfram Language* functions into C++ code. It is written mostly in *Wolfram Language* and utilize a C++ library for type deduction and implementation of the supported functions. 

**Note**: The project is in the progress of adding necessary components to the C++ library. 

## In a nutshell

Load the package:
```
<<MathCompile`
```
Compile a function using `CompileToCode`:
```
CompileToCode[
    Function[{Typed[p,"Integer"]},
        Module[{f=If[p>0,#+1&,#-1&]},f[p]]
    ]
]
```
The output is a C++ function: 
```c++
auto main_function(int64_t v16) {
    auto v15 = [&] {
        const auto v17 = wl::greater(v16, int64_t(0));
        if (v17) {
        } else {
        }
        return [&, v17](auto&&... v18) {
            if (v17) {
                return [&](auto v14, auto...) {
                    return wl::plus(v14, int64_t(1));
                }(std::forward<decltype(v18)>(v18)...);
            } else {
                return [&](auto v13, auto...) {
                    return wl::plus(v13, int64_t(-1));
                }(std::forward<decltype(v18)>(v18)...);
            }
        };
    }();
    return v15(v16);
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
```
**Complex numbers**
```
Complex
Re
Im
Arg
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
```
**Array generation**
```
ConstantArray
Range
```
**Array manipulation**
```
Set
Part
Span
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
