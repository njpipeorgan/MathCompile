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

namespace wl
{

#define WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(name, expr)                     \
template<typename X>                                                        \
auto name(X&& x)                                                            \
{                                                                           \
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");    \
    return utils::listable_function([](const auto& x) { return expr; },     \
        std::forward<decltype(x)>(x));                                      \
}

WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(log, std::log(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(exp, std::exp(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sqrt, std::sqrt(x))

WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sin, std::sin(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(cos, std::cos(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(tan, std::tan(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(cot, divide(int8_t(1), std::tan(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sec, divide(int8_t(1), std::cos(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(csc, divide(int8_t(1), std::sin(x)))

WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arcsin, std::asin(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccos, std::acos(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arctan, std::atan(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccot, std::atan(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arcsec, std::acos(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccsc, std::asin(divide(int8_t(1), x)))

WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sinh, std::sinh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(cosh, std::cosh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(tanh, std::tanh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(coth, divide(int8_t(1), std::tanh(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sech, divide(int8_t(1), std::cosh(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(csch, divide(int8_t(1), std::sinh(x)))

WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arcsinh, std::asinh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccosh, std::acosh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arctanh, std::atanh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccoth, std::atanh(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arcsech, std::acosh(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccsch, std::asinh(divide(int8_t(1), x)))


template<typename Base, typename X>
auto log(Base&& b, X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<Base>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_log = [](const auto& b, const auto& x)
    {
        return divide(std::log(x), std::log(b));
    };
    return utils::listable_function(scalar_log, 
        std::forward<decltype(b)>(b), std::forward<decltype(x)>(x));
}

template<typename X>
auto log2(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    constexpr auto log2_inv = double(1.4426950408889634074);
    auto scalar_log2 = [=](const auto& x)
    {
        if constexpr (is_complex_v<remove_cvref_t<decltype(x)>>)
            return log2_inv * std::log(x);
        else
            return std::log2(x);
    };
    return utils::listable_function(scalar_log2, 
        std::forward<decltype(x)>(x));
}

template<typename X>
auto log10(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    constexpr auto log10_inv = double(0.43429448190325182765);
    auto scalar_log10 = [=](const auto& x)
    {
        if constexpr (is_complex_v<remove_cvref_t<decltype(x)>>)
            return log10_inv * std::log(x);
        else
            return std::log10(x);
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
        using XT = remove_cvref_t<decltype(x)>;
        using YT = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XT> && is_real_v<YT>, "badargtype");
        return std::atan2(x, y);
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
        using XT = remove_cvref_t<decltype(x)>;
        using RT = decltype(divide(std::sin(x), x));
        if (x == XT(0))
            return RT(1.0);
        else
            return divide(std::sin(x), x);
    };
    return utils::listable_function(scalar_sinc,
        std::forward<decltype(x)>(x));
}

}
