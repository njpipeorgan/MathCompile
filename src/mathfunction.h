// Compiler from Wolfram Language to C++
// 
// Copyright 2019 Tianhuan Lu
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#pragma once

//#include "mkl.h"
#include <immintrin.h>

#include "macros.h"
#include "types.h"
#include "ndarray.h"
#include "utils.h"
#include "listable.h"

namespace wl
{

#define WL_DEFINE_UNARY_MATH_FUNCTION(name, expr)                           \
template<typename X>                                                        \
WL_INLINE auto name(X&& x)                                                  \
{                                                                           \
    WL_TRY_BEGIN()                                                          \
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,                   \
        WL_ERROR_NUMERIC_ONLY);                                             \
    return utils::listable_function([](auto x) {                            \
            using PX = promote_integral_t<decltype(x)>;                     \
            return expr;                                                    \
        }, std::forward<decltype(x)>(x));                                   \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}

#define WL_DEFINE_BINARY_MATH_FUNCTION(name, expr)                          \
template<typename X, typename Y>                                            \
WL_INLINE auto name(X&& x, Y&& y)                                           \
{                                                                           \
    WL_TRY_BEGIN()                                                          \
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&                 \
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);     \
    return utils::listable_function([](auto x, auto y)                      \
        {                                                                   \
            using PX = promote_integral_t<decltype(x)>;                     \
            using PY = promote_integral_t<decltype(y)>;                     \
            using PC = promote_integral_t<                                  \
                common_type_t<decltype(x), decltype(y)>>;                   \
            return expr;                                                    \
        }, std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));     \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}

template<typename X>
WL_INLINE auto _scalar_square(const X x)
{
    return X(x * x);
}

template<typename X, typename Y>
WL_INLINE auto _scalar_power(const X& x, const Y& y)
{
    if constexpr (is_integral_v<Y>)
    {
        auto tmp = x;
        auto count = y < 0 ? uint64_t(0) - uint64_t(y) :  uint64_t(y);

        auto ret = X(1);
        for (;; tmp *= tmp)
        {
            if ((count & uint64_t(1)) != 0u)
                ret *= tmp;
            if ((count >>= 1) == 0u)
                return y < 0 ? X(1) / ret : ret;
        }
    }
    else
    {
        return std::pow(x, y);
    }
}

template<typename X, int64_t y>
WL_INLINE auto _scalar_power(const X& x, const_int<y>)
{
    if constexpr (y < 0)
        return X(1) / _scalar_power(x, const_int<-y>{});
    else if constexpr (y == 0)
        return X(1);
    else if constexpr (y == 1)
        return x;
    else if constexpr (y == 2)
        return X(x * x);
    else if (y < 16)
    {
        X ret = _scalar_power(x, const_int<(y / 2)>{});
        ret *= ret;
        if constexpr ((y & int64_t(1)) != 0)
            ret *= x;
        return ret;
    }
    else
        return _scalar_power(x, y);
}

template<typename X, int64_t I>
WL_INLINE auto power(X&& x, const_int<I>)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    return utils::listable_function([](auto x)
        {
            using PC = promote_integral_t<common_type_t<decltype(x), int64_t>>;
            return _scalar_power(PC(x), const_int<I>{});
        }, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
WL_INLINE X _scalar_gamma(const X& x)
{
    static_assert(is_float_v<X>, WL_ERROR_INTERNAL);
    return X(std::tgamma(x));
}

template<typename X>
complex<X> _scalar_gamma(const complex<X>& x)
{
    constexpr size_t data_size = 9;
    static const std::array<double, data_size> data ={
        0.99999999999980993, 676.5203681218851, -1259.1392167224028,
        771.32342877765313, -176.61502916214059, 12.507343278686905,
        -0.13857109526572012, 9.9843695780195716e-6,
        1.5056327351493116e-7};

    if (std::real(x) < X(0.5))
    {
        return X(const_pi) / (
            std::sin(X(const_pi) * x) * _scalar_gamma(X(1) - x));
    }
    else
    {
        complex<X> z = x - X(1);
        complex<X> y = data[0];
        for (size_t i = 1; i < data_size; ++i)
            y += data[i] / (z + X(i));
        complex<X> t = z + X(7.5);
        return std::sqrt(X(2 * const_pi)) *
            std::pow(t, z + X(0.5)) * std::exp(-t) * y;
    }
}

WL_DEFINE_UNARY_MATH_FUNCTION(log, std::log(x))
WL_DEFINE_BINARY_MATH_FUNCTION(log, std::log(PC(y)) / std::log(PC(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(log2, PX(1.4426950408889634074) * std::log(x))
WL_DEFINE_UNARY_MATH_FUNCTION(log10, PX(0.43429448190325182765) * std::log(x))
WL_DEFINE_UNARY_MATH_FUNCTION(exp, std::exp(x))
WL_DEFINE_UNARY_MATH_FUNCTION(sqrt, std::sqrt(x))
WL_DEFINE_BINARY_MATH_FUNCTION(power, _scalar_power(PC(x), y))

WL_DEFINE_UNARY_MATH_FUNCTION(sin, std::sin(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cos, std::cos(x))
WL_DEFINE_UNARY_MATH_FUNCTION(tan, std::tan(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cot, PX(1) / std::tan(x))
WL_DEFINE_UNARY_MATH_FUNCTION(sec, PX(1) / std::cos(x))
WL_DEFINE_UNARY_MATH_FUNCTION(csc, PX(1) / std::sin(x))

WL_DEFINE_UNARY_MATH_FUNCTION(arcsin, std::asin(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccos, std::acos(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arctan, std::atan(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccot, std::atan(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(arcsec, std::acos(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccsc, std::asin(PX(1) / x))

WL_DEFINE_UNARY_MATH_FUNCTION(sinh, std::sinh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cosh, std::cosh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(tanh, std::tanh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(coth, PX(1) / std::tanh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(sech, PX(1) / std::cosh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(csch, PX(1) / std::sinh(x))

WL_DEFINE_UNARY_MATH_FUNCTION(arcsinh, std::asinh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccosh, std::acosh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arctanh, std::atanh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccoth, std::atanh(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(arcsech, std::acosh(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccsch, std::asinh(PX(1) / x))

WL_DEFINE_UNARY_MATH_FUNCTION(sinc, (x == PX(0) ? PX(1) : std::sin(x) / PX(x)))
WL_DEFINE_BINARY_MATH_FUNCTION(arctan, std::atan2(PC(x), PC(y)))
WL_DEFINE_UNARY_MATH_FUNCTION(haversine,
    _scalar_square(std::sin(PX(0.5) * x)))
WL_DEFINE_UNARY_MATH_FUNCTION(inverse_haversine,
    PX(2) * std::asin(std::sqrt(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(gudermannian,
    PX(2) * std::atan(std::exp(x)) - PX(1.5707963267948966192))
WL_DEFINE_UNARY_MATH_FUNCTION(inverse_gudermannian,
    std::log(std::tan(PX(0.5) * x + PX(0.78539816339744830962))))
WL_DEFINE_UNARY_MATH_FUNCTION(logistic_sigmoid,
    PX(1) / (PX(1) + std::exp(-PX(x))))

WL_DEFINE_UNARY_MATH_FUNCTION(gamma, _scalar_gamma(x))
WL_DEFINE_UNARY_MATH_FUNCTION(log_gamma, std::lgamma(x))
WL_DEFINE_UNARY_MATH_FUNCTION(erf, std::erf(x))
WL_DEFINE_UNARY_MATH_FUNCTION(erfc, std::erfc(x))
WL_DEFINE_BINARY_MATH_FUNCTION(beta, std::beta(x, y))
WL_DEFINE_UNARY_MATH_FUNCTION(zeta, std::riemann_zeta(x))

}
