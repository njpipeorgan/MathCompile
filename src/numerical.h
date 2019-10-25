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
#include "traits.h"
#include "ndarray.h"
#include "utils.h"

namespace wl
{

template<typename X>
auto n(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto x_rank = array_rank_v<XT>;
    if constexpr (x_rank == 0)
    {
        static_assert(is_arithmetic_v<XT>, "badargtype");
        if constexpr (is_integral_v<XT>)
            return double(x);
        else
            return x;
    }
    else
    {
        using XV = value_type_t<XT>;
        if constexpr (is_integral_v<XV>)
        {
            ndarray<decltype(n(XV{})), x_rank> ret(x.dims());
            x.for_each(
                [](const auto& src, auto& dst) { dst = n(src); },
                ret.begin());
            return ret;
        }
        else
            return std::forward<decltype(x)>(x);
    }
}

#define WL_DEFINE_ROUNDING_FUNCTION(name, stdname)                      \
template<typename X>                                                    \
auto name(X&& x)                                                        \
{                                                                       \
    using XT = remove_cvref_t<X>;                                       \
    constexpr auto x_rank = array_rank_v<XT>;                           \
    if constexpr (x_rank == 0)                                          \
    {                                                                   \
        static_assert(is_arithmetic_v<XT>, "badargtype");               \
        if constexpr (is_integral_v<XT>)                                \
            return x;                                                   \
        if constexpr (is_float_v<XT>)                                   \
            return int64_t(std::stdname(x));                            \
        else if constexpr (is_complex_v<XT>)                            \
            return XT(name(std::real(x)), name(std::imag(x)));          \
    }                                                                   \
    else                                                                \
    {                                                                   \
        using XVT = typename XT::value_type;                            \
        if constexpr (is_integral_v<XVT>)                               \
            return std::forward<decltype(x)>(x);                        \
        else                                                            \
        {                                                               \
            ndarray<decltype(name(XVT{})), x_rank> ret(x.dims());       \
            x.for_each(                                                 \
                [](const auto& src, auto& dst) { dst = name(src); },    \
                ret.begin());                                           \
            return ret;                                                 \
        }                                                               \
    }                                                                   \
}

WL_DEFINE_ROUNDING_FUNCTION(round, round)
WL_DEFINE_ROUNDING_FUNCTION(ceiling, ceil)
WL_DEFINE_ROUNDING_FUNCTION(floor, floor)
WL_DEFINE_ROUNDING_FUNCTION(integer_part, trunc)

template<typename X>
auto fractional_part(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto x_rank = array_rank_v<XT>;
    if constexpr (x_rank == 0)
    {
        static_assert(is_arithmetic_v<XT>, "badargtype");
        if constexpr (is_integral_v<XT>)
            return double(0);
        else if constexpr (is_float_v<XT>)
            return x - std::trunc(x);
        else
            return XT(fractional_part(x.real()), fractional_part(x.imag()));
    }
    else
    {
        using XVT = typename XT::value_type;
        if constexpr (is_integral_v<XVT>)
        {
            return ndarray<double, x_rank>(x.dims());
        }
        else if constexpr (is_movable_v<X&&>)
        { // movable
            ndarray<XVT, x_rank> ret(std::move(x));
            ret.for_each([](auto& src) { src = fractional_part(src); });
            return ret;
        }
        else
        {
            ndarray<XVT, x_rank> ret(x.dims());
            x.for_each(
                [](const auto& src, auto& dst) { dst = fractional_part(src); },
                ret.begin());
            return ret;
        }
    }
}

template<typename X>
auto abs(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto x_rank = array_rank_v<XT>;
    if constexpr (x_rank == 0)
    {
        static_assert(is_arithmetic_v<XT>, "badargtype");
        if constexpr (std::is_unsigned_v<XT>)
            return x;
        else
            return std::abs(x);
    }
    else
    {
        using XVT = typename XT::value_type;
        if constexpr (std::is_unsigned_v<XT>)
            return std::forward<decltype(x)>(x);
        else if constexpr (is_real_v<XVT>&& is_movable_v<X&&>)
        { // movable
            ndarray<XVT, x_rank> ret(std::move(x));
            ret.for_each([](auto& src) { src = abs(src); });
            return ret;
        }
        else
        {
            ndarray<decltype(abs(XVT{})), x_rank > ret(x.dims());
            x.for_each(
                [](const auto& src, auto& dst) { dst = abs(src); },
                ret.begin());
            return ret;
        }
    }
}

template<typename X, typename Y>
boolean greater(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return boolean(x > y);
}

template<typename X, typename Y>
boolean less(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return boolean(x < y);
}

template<typename X, typename Y>
boolean greater_equal(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return boolean(x >= y);
}

template<typename X, typename Y>
boolean less_equal(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return boolean(x <= y);
}

template<typename X, typename Y>
boolean equal(const X& x, const Y& y)
{
    constexpr auto x_rank = array_rank_v<X>;
    constexpr auto y_rank = array_rank_v<Y>;
    static_assert(x_rank == y_rank, "badrank");

    if constexpr (x_rank == 0)
    {
        static_assert((is_arithmetic_v<X> && is_arithmetic_v<X>) ||
            (is_value_type_v<X>, std::is_same_v<X, Y>), "badargtype");
        if constexpr (is_complex_v<X>)
            return boolean(x == X(y));
        else if constexpr (is_complex_v<Y>)
            return boolean(Y(x) == y);
        else
            return boolean(x == y);
    }
    else
    {
        if (!utils::check_dims<x_rank>(x.dims_ptr(), y.dims_ptr()))
            return boolean(false);
        if constexpr (X::category != view_category::General)
        {
            auto equal_flag = boolean(true);
            y.for_each([&](const auto& a, const auto& b)
                {
                    equal_flag = equal(a, b);
                    return !equal_flag;
                }, x.begin());
            return equal_flag;
        }
        else if constexpr (Y::category != view_category::General)
        {
            return equal(y, x);
        }
        else
        {
            if constexpr (sizeof(typename X::value_type) <
                sizeof(typename Y::value_type))
                return equal(x.to_array, y);
            else
                return equal(x, y.to_array);
        }
    }
}

template<typename X, typename Y>
boolean unequal(const X& x, const Y& y)
{
    return !equal(x, y);
}

template<typename X, typename Y>
auto mod(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, "badargtype");
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
                C rem = xi % yi;
                if (std::is_signed_v<C> && (rem != 0))
                    rem += yi & ((xi ^ yi) >> (8u * sizeof(C) - 1u));
                return rem;
            }
        }
        else
        {
            using C = common_type_t<XV, YV>;
            if (y == 0)
                return C(0);
            else
            {
                auto xi = C(x);
                auto yi = C(y);
                C rem = std::fmod(xi, yi);
                if (rem != 0 && ((xi > 0) ^ (yi > 0)))
                    rem += yi;
                return rem;
            }
        }
    };
    return utils::listable_function(pure,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}

template<typename Ret, typename X>
auto sign(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
        {
            return x / std::abs(x);
        }
        else
        {
            if (x == XV(0))
                return Ret(0);
            else if (x > XV(0))
                return Ret(1);
            else
                return Ret(-1);
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X>
auto positive(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        return boolean(x > XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X>
auto negative(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        return boolean(x < XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X>
auto non_positive(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        return boolean(x <= XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X>
auto non_negative(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        return boolean(x >= XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X, typename L>
auto clip(X&& x, const L& limit)
{
    static_assert(array_rank_v<L> == 1, "badargtype");
    if (limit.size() != 2u) throw std::logic_error("baddims");
    std::array<value_type_t<L>, 2u> limit_pair;
    limit.copy_to(limit_pair.data());
    return clip(std::forward<decltype(x)>(x), varg_tag{},
        limit_pair[0], limit_pair[1]);
}

template<typename X, typename L, typename VL>
auto clip(X&& x, const L& limit, const VL& vlimit)
{
    static_assert(array_rank_v<L> == 1, "badargtype");
    static_assert(array_rank_v<VL> == 1, "badargtype");
    if (limit.size() != 2u) throw std::logic_error("baddims");
    if (vlimit.size() != 2u) throw std::logic_error("baddims");
    std::array<value_type_t<L>, 2u> limit_pair;
    std::array<value_type_t<VL>, 2u> vlimit_pair;
    limit.copy_to(limit_pair.data());
    vlimit.copy_to(vlimit_pair.data());
    return clip(std::forward<decltype(x)>(x), varg_tag{},
        limit_pair[0], limit_pair[1], vlimit_pair[0], vlimit_pair[1]);
}

template<typename X>
auto clip(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        if (x < XV(0))
            return XV(0);
        else if (x > XV(1))
            return XV(1);
        else
            return x;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X, typename Min, typename Max>
auto clip(X&& x, varg_tag, Min min, Max max)
{
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    static_assert(is_real_v<Min> && is_real_v<Max>, "badargtype");
    using C = common_type_t<value_type<XT>, Min, Max>;

    const auto cmin = cast<C>(min);
    const auto cmax = cast<C>(max);
    auto pure = [=](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        auto cx = cast<C>(x);
        if (cx < cmin)
            return cmin;
        else if (cx > cmax)
            return cmax;
        else
            return cx;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X, typename Min, typename Max, typename VMin, typename VMax>
auto clip(X&& x, varg_tag, Min min, Max max, VMin vmin, VMax vmax)
{
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_real_v<Min> && is_real_v<Max>, "badargtype");
    static_assert(is_real_v<VMin> && is_real_v<VMax>, "badargtype");
    using C = common_type_t<value_type_t<XT>, Min, Max, VMin, VMax>;

    const auto cmin = cast<C>(min);
    const auto cmax = cast<C>(max);
    const auto cvmin = cast<C>(vmin);
    const auto cvmax = cast<C>(vmax);
    auto pure = [=](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        auto cx = cast<C>(x);
        if (cx < cmin)
            return cvmin;
        else if (cx > cmax)
            return cvmax;
        else
            return cx;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename Ret, typename X>
auto unitize(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        return Ret(int8_t(x != XV(0)));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename Ret, typename X>
auto unit_step(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        if (x >= XV(0))
            return Ret(1);
        else
            return Ret(0);
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

constexpr auto min()
{
    return const_real_infinity;
}

template<typename X>
auto min(const X& x)
{
    if constexpr (is_argument_pack_v<X>)
    {
        using XV = value_type_t<value_type_t<X>>;
        const auto pack_size = x.size();
        auto ret = std::numeric_limits<XV>::max();
        for (size_t i = 0u; i < pack_size; ++i)
            ret = std::min(ret, min(x.get(i)));
        return ret;
    }
    else if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_real_v<X>, "badargtype");
        return x;
    }
    else
    {
        static_assert(is_real_v<value_type_t<X>>, "badargtype");
        using XV = value_type_t<X>;
        auto ret = std::numeric_limits<XV>::max();
        x.for_each([&](const auto& a) { ret = std::min(ret, a); });
        return ret;
    }
}

template<typename X1, typename X2, typename... Xs>
auto min(const X1& x1, const X2& x2, const Xs&... xs)
{
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
}

constexpr auto max()
{
    return -const_real_infinity;
}

template<typename X>
auto max(const X& x)
{
    if constexpr (is_argument_pack_v<X>)
    {
        using XV = value_type_t<value_type_t<X>>;
        const auto pack_size = x.size();
        auto ret = std::numeric_limits<XV>::min();
        for (size_t i = 0u; i < pack_size; ++i)
            ret = std::max(ret, max(x.get(i)));
        return ret;
    }
    else if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_real_v<X>, "badargtype");
        return x;
    }
    else
    {
        static_assert(is_real_v<value_type_t<X>>, "badargtype");
        using XV = value_type_t<X>;
        auto ret = std::numeric_limits<XV>::min();
        x.for_each([&](const auto& a) { ret = std::max(ret, a); });
        return ret;
    }
}

template<typename X1, typename X2, typename... Xs>
auto max(const X1& x1, const X2& x2, const Xs&... xs)
{
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
}

}
