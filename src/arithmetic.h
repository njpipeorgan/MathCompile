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

#include <algorithm>
#include <vector>

#include "types.h"
#include "arrayview.h"
#include "ndarray.h"
#include "numerical.h"
#include "utils.h"

namespace wl
{

auto _scalar_plus = [](const auto& x, const auto& y)
{
    using XT = remove_cvref_t<decltype(x)>;
    using YT = remove_cvref_t<decltype(y)>;
    using XV = value_type_t<XT>;
    using YV = value_type_t<YT>;
    using C = common_type_t<XV, YV>;
    if constexpr (is_complex_v<XT>)
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(std::real(x) + std::real(y),
                std::imag(x) + std::imag(y));
        else
            return complex<C>(std::real(x) + cast<C>(y), std::imag(x));
    }
    else
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(cast<C>(x) + std::real(y), std::imag(y));
        else
            return cast<C>(x) + cast<C>(y);
    }
};

auto _scalar_subtract = [](const auto& x, const auto& y)
{
    using XT = remove_cvref_t<decltype(x)>;
    using YT = remove_cvref_t<decltype(y)>;
    using XV = value_type_t<XT>;
    using YV = value_type_t<YT>;
    using C = common_type_t<XV, YV>;
    if constexpr (is_complex_v<XT>)
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(std::real(x) - std::real(y),
                std::imag(x) - std::imag(y));
        else
            return complex<C>(std::real(x) - cast<C>(y), std::imag(x));
    }
    else
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(cast<C>(x) - std::real(y), -std::imag(y));
        else
            return cast<C>(x) - cast<C>(y);
    }
};

auto _scalar_times = [](const auto& x, const auto& y)
{
    using XT = remove_cvref_t<decltype(x)>;
    using YT = remove_cvref_t<decltype(y)>;
    using XV = value_type_t<XT>;
    using YV = value_type_t<YT>;
    using C = common_type_t<XV, YV>;
    if constexpr (is_complex_v<XT>)
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(x) * complex<C>(y);
        else
            return complex<C>(std::real(x) * cast<C>(y),
                std::imag(x) * cast<C>(y));
    }
    else
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(cast<C>(x) * std::real(y),
                cast<C>(x) * std::imag(y));
        else
            return cast<C>(x) * cast<C>(y);
    }
};

auto _scalar_divide = [](const auto& x, const auto& y)
{
    using XT = remove_cvref_t<decltype(x)>;
    using YT = remove_cvref_t<decltype(y)>;
    using XV = value_type_t<XT>;
    using YV = value_type_t<YT>;
    using C = common_type_t<XV, YV>;
    if constexpr (is_complex_v<XT>)
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(x) / complex<C>(y);
        else
            return complex<C>(x) / cast<C>(y);
    }
    else
    {
        if constexpr (is_complex_v<YT>)
            return cast<C>(x) / complex<C>(y);
        else if constexpr (is_integral_v<XT>&& is_integral_v<YT>)
            return double(x) / double(y);
        else
            return cast<C>(x) / cast<C>(y);
    }
};

template<typename X>
auto minus(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    return utils::listable_function([](const auto& x) { return -x; },
        std::forward<decltype(x)>(x));
}

template<typename Iter>
auto _variadic_plus(const argument_pack<Iter>& args)
{
    using ArgType = remove_cvref_t<decltype(args.get(0))>;
    constexpr auto rank = array_rank_v<ArgType>;
    const auto size = args.size();
    assert(size > 0u);
    auto ret = val(args.get(0));
    for (size_t i = 1u; i < size; ++i)
        ret = plus(ret, args.get(i));
    return ret;
}

template<typename X, typename Y>
auto plus(X&& x, Y&& y)
{
    if constexpr (is_argument_pack_v<remove_cvref_t<Y>>)
        return plus(std::forward<decltype(x)>(x), _variadic_plus(y));
    else if constexpr (is_argument_pack_v<remove_cvref_t<X>>)
        return plus(_variadic_plus(x), std::forward<decltype(y)>(y));
    else
    {
        static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
        static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
        return utils::listable_function(_scalar_plus,
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
}

template<typename X>
auto plus(X&& x)
{
    if constexpr (is_argument_pack_v<remove_cvref_t<X>>)
        return _variadic_plus(x);
    else
        return std::forward<decltype(x)>(x);
}

template<typename X1, typename X2, typename X3, typename... Xs>
auto plus(X1&& x1, X2&& x2, X3&& x3, Xs&&... xs)
{
    return plus(plus(std::forward<decltype(x1)>(x1),
        std::forward<decltype(x2)>(x2)),
        std::forward<decltype(x3)>(x3),
        std::forward<decltype(xs)>(xs)...);
}

template<typename X, typename Y>
auto subtract(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    return utils::listable_function(_scalar_subtract,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}

template<typename Iter>
auto _variadic_times(const argument_pack<Iter>& args)
{
    using ArgType = remove_cvref_t<decltype(args.get(0))>;
    constexpr auto rank = array_rank_v<ArgType>;
    const auto size = args.size();
    assert(size > 0u);
    auto ret = val(args.get(0));
    for (size_t i = 1u; i < size; ++i)
        ret = times(ret, args.get(i));
    return ret;
}

template<typename X, typename Y>
auto times(X&& x, Y&& y)
{
    if constexpr (is_argument_pack_v<remove_cvref_t<Y>>)
        return plus(std::forward<decltype(x)>(x), _variadic_times(y));
    else if constexpr (is_argument_pack_v<remove_cvref_t<X>>)
        return plus(_variadic_times(x), std::forward<decltype(y)>(y));
    else
    {
        static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
        static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
        return utils::listable_function(_scalar_times,
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
}

template<typename X>
auto times(X&& x)
{
    if constexpr (is_argument_pack_v<remove_cvref_t<X>>)
        return _variadic_times(x);
    else
        return std::forward<decltype(x)>(x);
}

template<typename X1, typename X2, typename X3, typename... Xs>
auto times(X1&& x1, X2&& x2, X3&& x3, Xs&&... xs)
{
    return times(times(std::forward<decltype(x1)>(x1),
        std::forward<decltype(x2)>(x2)),
        std::forward<decltype(x3)>(x3),
        std::forward<decltype(xs)>(xs)...);
}

template<typename X, typename Y>
auto divide(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    return utils::listable_function(_scalar_divide,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}


#define WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(name, func)                 \
template<typename X, typename Y>                                        \
auto _scalar_##name(X& x, const Y& y)                                   \
{                                                                       \
    static_assert(is_convertible_v<common_type_t<X, Y>, X>, "badcast"); \
    x = X(_scalar_##func(x, y));                                        \
    return x;                                                           \
}

WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(add_to, plus)
WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(subtract_from, subtract)
WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(times_by, times)
WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(divide_by, divide)


#define WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(name)                         \
template<typename X, typename Y>                                            \
auto name(X&& x, Y&& y) -> decltype(auto)                                   \
{                                                                           \
    using XType = remove_cvref_t<X>;                                        \
    using YType = remove_cvref_t<X>;                                        \
    constexpr auto x_rank = array_rank_v<XType>;                            \
    constexpr auto y_rank = array_rank_v<YType>;                            \
    if constexpr (x_rank > 0)                                               \
    {                                                                       \
        if constexpr (y_rank == 0)                                          \
        {                                                                   \
            x.for_each([y](auto& dst) { _scalar_##name(dst, y); });         \
        }                                                                   \
        else                                                                \
        {                                                                   \
            static_assert(x_rank == y_rank, "badrank");                     \
            if (!utils::check_dims<x_rank>(x.dims_ptr(), y.dims_ptr()))     \
                throw std::logic_error("baddims");                          \
            if constexpr (YType::category != view_category::General)        \
                x.for_each([](auto& dst, const auto& src)                   \
            { _scalar_##name(dst, src); }, y.begin());                      \
            else if constexpr (XType::category != view_category::General)   \
                y.for_each([](const auto& src, auto& dst)                   \
            { _scalar_##name(dst, src); }, x.begin());                      \
            else /* general_view += general_view */                         \
            {                                                               \
                std::vector<typename Y::value_type> buffer(y.size());       \
                y.copy_to(buffer.begin());                                  \
                x.for_each([](auto& dst, const auto& src)                   \
                { _scalar_##name(dst, src); }, buffer.begin());             \
            }                                                               \
        }                                                                   \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        static_assert(y_rank == 0, "badrank");                              \
        _scalar_##name(x, y);                                               \
    }                                                                       \
    return std::forward<decltype(x)>(x);                                    \
}

WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(add_to)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(subtract_from)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(times_by)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(divide_by)

}
