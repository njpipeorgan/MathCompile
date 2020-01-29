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

#include <cmath>
#include <complex>
#include <limits>

#include "types.h"
#include "ndarray.h"
#include "utils.h"
#include "listable.h"

namespace wl
{

template<typename X>
auto n(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    if constexpr (!is_integral_v<XV>)
        return std::forward<decltype(x)>(x);
    else
        return utils::listable_function(
            [](const auto& a) { return double(a); },
            std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

#define WL_DEFINE_ROUNDING_FUNCTION(name, stdname)                      \
template<typename X>                                                    \
auto name(X&& x)                                                        \
{                                                                       \
    WL_TRY_BEGIN()                                                      \
    using XT = remove_cvref_t<X>;                                       \
    constexpr auto XR = array_rank_v<XT>;                               \
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);      \
    using XV = std::conditional_t<(XR == 0u), XT, value_type_t<XT>>;    \
    if constexpr (is_integral_v<XV>)                                    \
        return std::forward<decltype(x)>(x);                            \
    else                                                                \
    {                                                                   \
        auto pure = [](const auto& x)                                   \
        {                                                               \
            if constexpr (is_float_v<XV>)                               \
                return int64_t(std::stdname(x));                        \
            else                                                        \
                return complex<value_type_t<XV>>(                       \
                    std::stdname(std::real(x)),                         \
                    std::stdname(std::imag(x)));                        \
        };                                                              \
        return utils::listable_function(pure,                           \
            std::forward<decltype(x)>(x));                              \
    }                                                                   \
    WL_TRY_END(__func__, __FILE__, __LINE__)                            \
}

WL_DEFINE_ROUNDING_FUNCTION(round, round)
WL_DEFINE_ROUNDING_FUNCTION(ceiling, ceil)
WL_DEFINE_ROUNDING_FUNCTION(floor, floor)
WL_DEFINE_ROUNDING_FUNCTION(integer_part, trunc)

template<typename X>
auto fractional_part(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    auto pure = [](const auto& x)
    {
        if constexpr (is_integral_v<XV>)
            return double(0);
        else if constexpr (is_float_v<XV>)
            return x - std::trunc(x);
        else
            return XV(std::real(x) - std::trunc(std::real(x)),
                std::imag(x) - std::trunc(std::imag(x)));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto abs(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    if constexpr (std::is_unsigned_v<XV>)
        return std::forward<decltype(x)>(x);
    else
        return utils::listable_function([](auto x) { return std::abs(x); },
            std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto ramp(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    if constexpr (std::is_unsigned_v<XV>)
        return std::forward<decltype(x)>(x);
    else
    {
        auto pure = [](const auto& x)
        {
            return x >= XV(0) ? x : XV(0);
        };
        return utils::listable_function(pure, std::forward<decltype(x)>(x));
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
boolean greater(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, WL_ERROR_BAD_COMPARE);
    return boolean(x > y);
}

template<typename X, typename Y>
boolean less(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, WL_ERROR_BAD_COMPARE);
    return boolean(x < y);
}

template<typename X, typename Y>
boolean greater_equal(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, WL_ERROR_BAD_COMPARE);
    return boolean(x >= y);
}

template<typename X, typename Y>
boolean less_equal(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, WL_ERROR_BAD_COMPARE);
    return boolean(x <= y);
}

template<bool DimChecked, typename X, typename Y>
boolean _equal_impl(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR == YR, WL_ERROR_OPERAND_RANK);

    if constexpr (XR == 0u)
    {
        static_assert((is_arithmetic_v<X> && is_arithmetic_v<X>) ||
            (is_string_view_v<X> && is_string_view_v<Y>) ||
            (is_value_type_v<X> && std::is_same_v<X, Y>),
            WL_ERROR_BAD_COMPARE);
        if constexpr (is_complex_v<X>)
            return boolean(x == cast<X>(y));
        else if constexpr (is_complex_v<Y>)
            return boolean(cast<Y>(x) == y);
        else
            return boolean(x == y);
    }
    else
    {
        if constexpr (!DimChecked)
            if (!utils::check_dims(x.dims(), y.dims()))
                return const_false;
        if constexpr (X::category != view_category::General)
        {
            auto equal_flag = true;
            y.for_each([&](const auto& a, const auto& b)
                {
                    equal_flag = _equal_impl<true>(a, b);
                    return !equal_flag;
                }, x.begin());
            return boolean(equal_flag);
        }
        else if constexpr (Y::category != view_category::General)
        {
            return _equal_impl<true>(y, x);
        }
        else
        {
            if constexpr (sizeof(value_type_t<X>) < sizeof(value_type_t<Y>))
                return _equal_impl<true>(x.to_array(), y);
            else
                return _equal_impl<true>(x, y.to_array());
        }
    }
}

template<typename X, typename Y>
boolean equal(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    return _equal_impl<false>(x, y);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
boolean equal(const X& x, const Y& y, dim_checked)
{
    WL_TRY_BEGIN()
    return _equal_impl<true>(x, y);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
boolean unequal(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    return !equal(x, y);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
boolean unequal(const X& x, const Y& y, dim_checked)
{
    WL_TRY_BEGIN()
    return !equal(x, y, dim_checked{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y, typename... DimChecked>
boolean same_q(const X& x, const Y& y, DimChecked...)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    constexpr auto same_type = (XR == 0u) ? std::is_same_v<X, Y> :
        std::is_same_v<value_type_t<X>, value_type_t<Y>>;

    if constexpr (XR == YR && same_type)
        return equal(x, y, DimChecked{}...);
    else
        return const_false;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y, typename... DimChecked>
boolean unsame_q(const X& x, const Y& y, DimChecked...)
{
    WL_TRY_BEGIN()
    return !same_q(x, y, DimChecked{}...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
auto mod(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, WL_ERROR_REAL_TYPE_ARG);
        if constexpr (is_integral_v<XV> && is_integral_v<YV>)
        {
            using C = std::conditional_t<
                std::is_signed_v<XV> || std::is_signed_v<YV>,
                make_signed_t<common_type_t<XV, YV>>,
                common_type_t<XV, YV>>;
            if (y == 0)
                return C(0);
            else
            {
                auto xi = C(x);
                auto yi = C(y);
                auto rem = xi % yi;
                if constexpr (std::is_unsigned_v<C>)
                    return rem;
                else if (rem == C(0))
                    return rem;
                else
                    return rem + (yi & ((xi ^ yi) >> (8u * sizeof(C) - 1u)));
            }
        }
        else
        {
            using C = common_type_t<XV, YV>;
            auto cx = C(x);
            auto cy = C(y);
            return cx - std::floor(cx / cy) * cy;
        }
    };
    return utils::listable_function(pure,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
auto quotient(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, WL_ERROR_REAL_TYPE_ARG);
        if constexpr (is_integral_v<XV> && is_integral_v<YV>)
        {
            using C = std::conditional_t<
                std::is_signed_v<XV> || std::is_signed_v<YV>,
                make_signed_t<common_type_t<XV, YV>>,
                common_type_t<XV, YV>>;
            if (y == 0)
                return C(0);
            else
            {
                auto xi = C(x);
                auto yi = C(y);
                auto rem = xi % yi;
                auto quot = xi / yi;
                if constexpr (std::is_unsigned_v<C>)
                    return quot;
                else if (rem == C(0))
                    return quot;
                else
                    return quot + ((xi ^ yi) >> (8u * sizeof(C) - 1u));
            }
        }
        else
        {
            using C = common_type_t<XV, YV>;
            auto cx = C(x);
            auto cy = C(y);
            return std::floor(cx / cy);
        }
    };
    return utils::listable_function(pure,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Ret = int64_t, typename X>
auto sign(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x / std::abs(x);
        else if (x == XV(0))
            return Ret(0);
        else if (x > XV(0))
            return Ret(1);
        else
            return Ret(-1);
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Ret = int64_t, typename X, typename Y, typename N>
auto integer_digits(const X& x, const Y& y, const N& n)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<X> && is_integral_v<Y>,
        WL_ERROR_INTEGRAL_TYPE_ARG);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    constexpr auto fixed_length = !std::is_same_v<N, void_type>;
    if (y < Y(0)) throw std::logic_error(WL_ERROR_INTEGER_DIGITS_NEGATIVE);
    auto ux = (x >= X(0)) ? uint64_t(x) : uint64_t(-x);
    auto uy = uint64_t(y);
    ndarray<Ret, 1u> ret;
    if constexpr (fixed_length)
    {
        static_assert(is_integral_v<N>, WL_ERROR_COUNTING_ARG);
        if (n < N(0)) throw std::logic_error(WL_ERROR_INTEGER_DIGITS_NEGATIVE);
        ret.uninitialized_resize(std::array<size_t, 1u>{size_t(n)}, size_t(n));
        auto ret_begin = ret.data();
        auto ret_end = ret_begin + size_t(n);
        auto ret_iter = ret_begin;
        for (;;)
        {
            uint64_t rem = ux % uy;
            ux = ux / uy;
            *ret_iter = Ret(rem);
            ++ret_iter;
            if (ux == 0u)
            {
                for (; ret_iter < ret_end; ++ret_iter)
                    *ret_iter = Ret(0);
                break;
            }
        }
        std::reverse(ret_begin, ret_end);
    }
    else if (ux == 0u)
        ret.append(Ret(0));
    else
    {
        for (;;)
        {
            uint64_t rem = ux % uy;
            ux = ux / uy;
            ret.append(Ret(rem));
            if (ux == 0u)
                break;
        }
        std::reverse(ret.begin(), ret.end());
    }
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Ret = int64_t, typename X, typename Y>
auto integer_digits(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    return integer_digits<Ret>(x, y, const_null);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Ret = int64_t, typename X>
auto integer_digits(const X& x)
{
    WL_TRY_BEGIN()
    return integer_digits<Ret>(x, uint64_t(10), const_null);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto positive(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        return boolean(x > XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto negative(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        return boolean(x < XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto non_positive(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        return boolean(x <= XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto non_negative(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        return boolean(x >= XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename L>
auto clip(X&& x, const L& limit)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<L> == 1u, WL_ERROR_REQUIRE_ARRAY_RANK"one.");
    if (limit.size() != 2u) throw std::logic_error(WL_ERROR_CLIP_LIMIT_SIZE);
    std::array<value_type_t<L>, 2u> limit_pair;
    limit.copy_to(limit_pair.data());
    return clip(std::forward<decltype(x)>(x), varg_tag{},
        limit_pair[0], limit_pair[1]);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename L, typename VL>
auto clip(X&& x, const L& limit, const VL& vlimit)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<L> == 1u, WL_ERROR_REQUIRE_ARRAY_RANK"one.");
    static_assert(array_rank_v<VL> == 1u, WL_ERROR_REQUIRE_ARRAY_RANK"one.");
    if (limit.size() != 2u) throw std::logic_error(WL_ERROR_CLIP_LIMIT_SIZE);
    if (vlimit.size() != 2u) throw std::logic_error(WL_ERROR_CLIP_LIMIT_SIZE);
    std::array<value_type_t<L>, 2u> limit_pair;
    std::array<value_type_t<VL>, 2u> vlimit_pair;
    limit.copy_to(limit_pair.data());
    vlimit.copy_to(vlimit_pair.data());
    return clip(std::forward<decltype(x)>(x), varg_tag{},
        limit_pair[0], limit_pair[1], vlimit_pair[0], vlimit_pair[1]);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto clip(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        if (x < XV(0))
            return XV(0);
        else if (x > XV(1))
            return XV(1);
        else
            return x;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Min, typename Max>
auto clip(X&& x, varg_tag, Min min, Max max)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    static_assert(is_real_v<Min> && is_real_v<Max>, WL_ERROR_REAL_TYPE_ARG);
    using C = common_type_t<value_type<XT>, Min, Max>;

    const auto cmin = cast<C>(min);
    const auto cmax = cast<C>(max);
    auto pure = [=](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        auto cx = cast<C>(x);
        if (cx < cmin)
            return cmin;
        else if (cx > cmax)
            return cmax;
        else
            return cx;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Min, typename Max, typename VMin, typename VMax>
auto clip(X&& x, varg_tag, Min min, Max max, VMin vmin, VMax vmax)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    static_assert(is_real_v<Min> && is_real_v<Max>, WL_ERROR_REAL_TYPE_ARG);
    static_assert(is_real_v<VMin> && is_real_v<VMax>, WL_ERROR_REAL_TYPE_ARG);
    using C = common_type_t<value_type_t<XT>, Min, Max, VMin, VMax>;

    const auto cmin = cast<C>(min);
    const auto cmax = cast<C>(max);
    const auto cvmin = cast<C>(vmin);
    const auto cvmax = cast<C>(vmax);
    auto pure = [=](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        auto cx = cast<C>(x);
        if (cx < cmin)
            return cvmin;
        else if (cx > cmax)
            return cvmax;
        else
            return cx;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Ret = int64_t, typename X>
auto unitize(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        return Ret(int8_t(x != XV(0)));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Ret = int64_t, typename X>
auto unit_step(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        if (x >= XV(0))
            return Ret(1);
        else
            return Ret(0);
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

constexpr auto min()
{
    return const_real_infinity;
}

template<typename X>
auto min(const X& x)
{
    WL_TRY_BEGIN()
    if constexpr (is_argument_pack_v<X>)
    {
        using XV = value_type_t<value_type_t<X>>;
        const auto pack_size = x.size();
        auto ret = std::numeric_limits<XV>::max();
        WL_CHECK_ABORT_LOOP_BEGIN(x.size())
            for (auto i = _loop_begin; i < _loop_end; ++i)
                ret = std::min(ret, min(x.get(i, dim_checked{})));
        WL_CHECK_ABORT_LOOP_END()
        return ret;
    }
    else if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_real_v<X>, WL_ERROR_REAL_TYPE_ARG);
        return x;
    }
    else
    {
        using XV = value_type_t<X>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        auto ret = std::numeric_limits<XV>::max();
        x.for_each([&](const auto& a) { ret = std::min(ret, a); });
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X1, typename X2, typename... Xs>
auto min(const X1& x1, const X2& x2, const Xs&... xs)
{
    WL_TRY_BEGIN()
    using MinType1 = decltype(min(x1));
    using MinType2 = decltype(min(x2));
    using LimitType = common_type_t<MinType1, MinType2, uint64_t>;
    constexpr auto signed1 =
        std::is_signed_v<MinType1> && std::is_unsigned_v<MinType2>;
    constexpr auto signed2 =
        std::is_signed_v<MinType2> && std::is_unsigned_v<MinType1>;
    constexpr auto high1 = LimitType(std::numeric_limits<MinType1>::max());
    constexpr auto high2 = LimitType(std::numeric_limits<MinType2>::max());
    using RT =
        std::conditional_t<signed1, MinType1,
        std::conditional_t<signed2, MinType2,
        std::conditional_t<(high1 > high2), MinType1, MinType2>>>;

    if constexpr (signed1)
    {
        auto min1 = min(x1);
        if (min1 <= MinType1(0))
            return min(RT(min1), xs...);
        else
        {
            auto min2 = min(x2);
            if (LimitType(min2) > LimitType(high1))
                return min(RT(min1), xs...);
            else
                return min((RT(min1) < RT(min2)) ? RT(min1) : RT(min2), xs...);
        }
    }
    else if constexpr (signed2)
        return min(x2, x1, xs...);
    else
    {
        auto min1 = min(x1);
        auto min2 = min(x2);
        return min((RT(min1) < RT(min2)) ? RT(min1) : RT(min2), xs...);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

constexpr auto max()
{
    return -const_real_infinity;
}

template<typename X>
auto max(const X& x)
{
    WL_TRY_BEGIN()
    if constexpr (is_argument_pack_v<X>)
    {
        using XV = value_type_t<value_type_t<X>>;
        auto ret = std::numeric_limits<XV>::min();
        WL_CHECK_ABORT_LOOP_BEGIN(x.size())
            for (auto i = _loop_begin; i < _loop_end; ++i)
                ret = std::max(ret, max(x.get(i, dim_checked{})));
        WL_CHECK_ABORT_LOOP_END()
        return ret;
    }
    else if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_real_v<X>, WL_ERROR_REAL_TYPE_ARG);
        return x;
    }
    else
    {
        using XV = value_type_t<X>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        auto ret = std::numeric_limits<XV>::min();
        x.for_each([&](const auto& a) { ret = std::max(ret, a); });
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X1, typename X2, typename... Xs>
auto max(const X1& x1, const X2& x2, const Xs&... xs)
{
    WL_TRY_BEGIN()
    using MaxType1 = decltype(max(x1));
    using MaxType2 = decltype(max(x2));
    using LimitType = common_type_t<MaxType1, MaxType2, uint64_t>;
    constexpr auto high1 = LimitType(std::numeric_limits<MaxType1>::max());
    constexpr auto high2 = LimitType(std::numeric_limits<MaxType2>::max());
    using RT = std::conditional_t<(high1 > high2), MaxType1, MaxType2>;

    if constexpr (high1 < high2)
    {
        auto max2 = max(x2);
        if (LimitType(max2) > LimitType(high1))
            return max(RT(max2), xs...);
        else
        {
            auto max1 = max(x1);
            if (std::is_signed_v<MaxType1> && max1 <= MaxType1(0))
                return max(RT(max2), xs...);
            else
                return max((RT(max1) > RT(max2)) ? RT(max1) : RT(max2), xs...);
        }
    }
    else if constexpr (high1 > high2)
        return max(x2, x1, xs...);
    else
    {
        auto max1 = max(x1);
        auto max2 = max(x2);
        return max((RT(max1) > RT(max2)) ? RT(max1) : RT(max2), xs...);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
auto chop(X&& x, const Y& y)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Y>, WL_ERROR_REAL_TYPE_ARG);
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    constexpr auto XR = array_rank_v<XT>;
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;

    if constexpr (is_integral_v<XV>)
        return std::forward<decltype(x)>(x);
    else
    {
        auto pure = [lim = cast<value_type_t<XV>>(y)](const auto& x)
        {
            if constexpr (is_real_v<XV>)
                return std::abs(x) < lim ? XV(0) : x;
            else
            {
                using T = value_type_t<XV>;
                T re = std::real(x);
                T im = std::imag(x);
                re = std::abs(re) < lim ? T(0) : re;
                im = std::abs(im) < lim ? T(0) : im;
                return complex<T>(re, im);
            }
        };
        return utils::listable_function(pure, std::forward<decltype(x)>(x));
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
