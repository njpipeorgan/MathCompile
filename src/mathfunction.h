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

#include "types.h"
#include "traits.h"
#include "utils.h"

namespace wl
{

template<typename X>
auto _scalar_inv(const X& x)
{
    static_assert(is_real_v<X> || is_complex_v<X>, "internal");
    using P = promote_integral_t<X>;
    return P(1) / P(x);
}

#define WL_DEFINE_UNARY_MATH_FUNCTION(name, expr)                           \
template<typename X>                                                        \
auto name(X&& x)                                                            \
{                                                                           \
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");    \
    return utils::listable_function([](auto x) { return expr; },            \
        std::forward<decltype(x)>(x));                                      \
}

WL_DEFINE_UNARY_MATH_FUNCTION(log, std::log(x))
WL_DEFINE_UNARY_MATH_FUNCTION(exp, std::exp(x))
WL_DEFINE_UNARY_MATH_FUNCTION(sqrt, std::sqrt(x))

WL_DEFINE_UNARY_MATH_FUNCTION(sin, _wl_sin(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cos, std::cos(x))
WL_DEFINE_UNARY_MATH_FUNCTION(tan, std::tan(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cot, _scalar_inv(std::tan(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(sec, _scalar_inv(std::cos(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(csc, _scalar_inv(std::sin(x)))

WL_DEFINE_UNARY_MATH_FUNCTION(arcsin, std::asin(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccos, std::acos(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arctan, std::atan(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccot, std::atan(_scalar_inv(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(arcsec, std::acos(_scalar_inv(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(arccsc, std::asin(_scalar_inv(x)))

WL_DEFINE_UNARY_MATH_FUNCTION(sinh, std::sinh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cosh, std::cosh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(tanh, std::tanh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(coth, _scalar_inv(std::tanh(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(sech, _scalar_inv(std::cosh(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(csch, _scalar_inv(std::sinh(x)))

WL_DEFINE_UNARY_MATH_FUNCTION(arcsinh, std::asinh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccosh, std::acosh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arctanh, std::atanh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccoth, std::atanh(_scalar_inv(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(arcsech, std::acosh(_scalar_inv(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(arccsch, std::asinh(_scalar_inv(x)))

template<typename Base, typename X>
auto log(Base&& b, X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<Base>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_log = [](const auto& b, const auto& x)
    {
        using P = promote_integral_t<common_type_t<
            remove_cvref_t<decltype(b)>, remove_cvref_t<decltype(x)>>>;
        return std::log(P(x)) / std::log(P(b));
    };
    return utils::listable_function(scalar_log, 
        std::forward<decltype(b)>(b), std::forward<decltype(x)>(x));
}

template<typename X>
auto log2(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_log2 = [=](const auto& x)
    {
        using P = promote_integral_t<remove_cvref_t<decltype(x)>>;
        constexpr auto log2_inv = P(1.4426950408889634074);
        return log2_inv * std::log(x);
    };
    return utils::listable_function(scalar_log2, 
        std::forward<decltype(x)>(x));
}

template<typename X>
auto log10(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_log10 = [=](const auto& x)
    {
        using P = promote_integral_t<remove_cvref_t<decltype(x)>>;
        constexpr auto log10_inv = P(0.43429448190325182765);
        return log10_inv * std::log(x);
    };
    return utils::listable_function(scalar_log10, 
        std::forward<decltype(x)>(x));
}

template<typename X, typename Power>
auto power(X&& x, Power&& p)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Power>>, "badargtype");
    auto scalar_power = [](const auto& x, const auto& p)
    {
        return std::pow(x, p);
    };
    return utils::listable_function(scalar_power,
        std::forward<decltype(x)>(x), std::forward<decltype(p)>(p));
}

template<typename X, typename Y>
auto arctan(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    auto scalar_arctan = [](const auto& x, const auto& y)
    {
        using P = promote_integral_t<common_type_t<
            remove_cvref_t<decltype(b)>, remove_cvref_t<decltype(x)>>>;
        static_assert(is_real_v<P>, "badargtype");
        return std::atan2(P(x), P(y));
    };
    return utils::listable_function(scalar_arctan,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}

template<typename X>
auto sinc(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    constexpr auto log2_inv = double(1.4426950408889634074);
    auto scalar_sinc = [=](const auto& x)
    {
        using P = promote_integral_t<remove_cvref_t<decltype(x)>>;
        if (x == 0)
            return P(1);
        else
            return std::sin(x) / P(x);
    };
    return utils::listable_function(scalar_sinc,
        std::forward<decltype(x)>(x));
}

}
