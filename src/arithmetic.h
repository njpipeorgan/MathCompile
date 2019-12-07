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
#include "listable.h"

namespace wl
{

constexpr auto _scalar_plus = [](const auto& x, const auto& y)
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
            return cast<C>(cast<C>(x) + cast<C>(y));
    }
};

constexpr auto _scalar_subtract = [](const auto& x, const auto& y)
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
            return cast<C>(cast<C>(x) - cast<C>(y));
    }
};

constexpr auto _scalar_times = [](const auto& x, const auto& y)
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
            return cast<C>(cast<C>(x) * cast<C>(y));
    }
};

constexpr auto _scalar_divide = [](const auto& x, const auto& y)
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


#define WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(name, func)                 \
template<typename X, typename Y>                                        \
auto _scalar_##name(X& x, const Y& y)                                   \
{                                                                       \
    static_assert(is_convertible_v<common_type_t<X, Y>, X>,             \
        WL_ERROR_MUTABLE_TYPE);                                         \
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
    WL_TRY_BEGIN()                                                          \
    using XType = remove_cvref_t<X>;                                        \
    using YType = remove_cvref_t<Y>;                                        \
    constexpr auto x_rank = array_rank_v<XType>;                            \
    constexpr auto y_rank = array_rank_v<YType>;                            \
    static_assert(std::is_lvalue_reference_v<X&&> ||                        \
        is_array_view_v<XType>, WL_ERROR_MODIFY_TARGET);                    \
    if constexpr (x_rank > 0)                                               \
    {                                                                       \
        if constexpr (y_rank == 0)                                          \
        {                                                                   \
            x.for_each([y](auto& dst) { _scalar_##name(dst, y); });         \
        }                                                                   \
        else                                                                \
        {                                                                   \
            static_assert(x_rank == y_rank, WL_ERROR_OPERAND_RANK);         \
            if (!utils::check_dims<x_rank>(x.dims_ptr(), y.dims_ptr()))     \
                throw std::logic_error(WL_ERROR_ARITHMETIC_DIMS);           \
            if (has_aliasing(x, y))                                         \
            {                                                               \
                std::vector<value_type_t<YType>> buffer(y.size());          \
                y.copy_to(buffer.begin());                                  \
                x.for_each([](auto& dst, const auto& src)                   \
                    { _scalar_##name(dst, src); }, buffer.begin());         \
            }                                                               \
            else if constexpr (YType::category != view_category::General)   \
                x.for_each([](auto& dst, const auto& src)                   \
                    { _scalar_##name(dst, src); }, y.begin());              \
            else if constexpr (XType::category != view_category::General)   \
                y.for_each([](const auto& src, auto& dst)                   \
                    { _scalar_##name(dst, src); }, x.begin());              \
            else /* general_view ?= general_view */                         \
            {                                                               \
                std::vector<value_type_t<YType>> buffer(y.size());          \
                y.copy_to(buffer.begin());                                  \
                x.for_each([](auto& dst, const auto& src)                   \
                    { _scalar_##name(dst, src); }, buffer.begin());         \
            }                                                               \
        }                                                                   \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        static_assert(y_rank == 0, WL_ERROR_MUTABLE_RANK);                  \
        _scalar_##name(x, y);                                               \
    }                                                                       \
    return std::forward<decltype(x)>(x);                                    \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}

WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(add_to)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(subtract_from)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(times_by)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(divide_by)

template<typename X>
auto pre_increment(X&& x) -> decltype(auto)
{
    WL_TRY_BEGIN()
    return add_to(std::forward<decltype(x)>(x), int64_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto pre_decrement(X&& x) -> decltype(auto)
{
    WL_TRY_BEGIN()
    return subtract_from(std::forward<decltype(x)>(x), int64_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto increment(X&& x)
{
    WL_TRY_BEGIN()
    auto valx = val(x);
    pre_increment(std::forward<decltype(x)>(x));
    return valx;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto decrement(X&& x)
{
    WL_TRY_BEGIN()
    auto valx = val(x);
    pre_decrement(std::forward<decltype(x)>(x));
    return valx;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}


template<typename X>
auto minus(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    return utils::listable_function([](const auto& x) { return -x; },
        std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Iter, bool HasStride>
auto _variadic_plus(const argument_pack<Iter, HasStride>& args)
{
    auto ret = val(args.get(0));
    const auto size = args.size();
    if (size == 0u)
        return ret;
    else
    {
        WL_CHECK_ABORT_LOOP_BEGIN(size - 1u)
            for (auto i = _loop_begin; i < _loop_end; ++i)
                add_to(ret, args.get(i, dim_checked{}));
        WL_CHECK_ABORT_LOOP_END()
        return ret;
    }
}
template<typename X, typename Y>
auto plus(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(plus)
    {
        static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
            is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
        return utils::listable_function(_scalar_plus,
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(plus, int64_t(0))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(plus)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(plus)

template<typename X, typename Y>
auto subtract(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    return utils::listable_function(_scalar_subtract,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Iter, bool HasStride>
auto _variadic_times(const argument_pack<Iter, HasStride>& args)
{
    auto ret = val(args.get(0));
    const auto size = args.size();
    if (size == 0u)
        return ret;
    else
    {
        WL_CHECK_ABORT_LOOP_BEGIN(size - 1u)
            for (auto i = _loop_begin; i < _loop_end; ++i)
                times_by(ret, args.get(i, dim_checked{}));
        WL_CHECK_ABORT_LOOP_END()
        return ret;
    }
}
template<typename X, typename Y>
auto times(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(times)
    {
        static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
            is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
        return utils::listable_function(_scalar_times,
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(times, int64_t(1))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(times)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(times)

template<typename X, typename Y>
auto divide(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    return utils::listable_function(_scalar_divide,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}


}
