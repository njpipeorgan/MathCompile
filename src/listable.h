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

#include "macros.h"
#include "types.h"
#include "arrayview.h"

namespace wl
{

namespace utils
{

template<typename Fn, typename X>
auto listable_function(Fn fn, X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto x_rank = array_rank_v<XT>;

    if constexpr (x_rank == 0)
    {
        return fn(x);
    }
    else
    {
        using XV = typename XT::value_type;
        using RV = decltype(fn(XV{}));
        if constexpr (is_movable_v<X&&> && std::is_same_v<RV, XV>)
        {
            x.for_each([=](auto& a) { a = fn(a); });
            return std::move(x);
        }
        else
        {
            ndarray<RV, x_rank> ret(x.dims());
            x.for_each([=](const auto& a, auto& r) { r = fn(a); },
                ret.begin());
            return ret;
        }
    }
}

template<typename Fn, typename X, typename Y>
auto listable_function(Fn fn, X&& x, Y&& y)
{
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto x_rank = array_rank_v<XT>;
    constexpr auto y_rank = array_rank_v<YT>;

    if constexpr (x_rank == 0 && y_rank == 0)
    {
        return fn(x, y);
    }
    else if constexpr (x_rank == 0 && y_rank >= 1)
    {
        using YV = typename YT::value_type;
        using RV = decltype(fn(x, YV{}));
        if constexpr (is_movable_v<Y&&> && std::is_same_v<RV, YV>)
        {
            y.for_each([=](auto& b) { b = fn(x, b); });
            return std::move(y);
        }
        else
        {
            ndarray<RV, y_rank> ret(y.dims());
            y.for_each([=](const auto& b, auto& r) { r = fn(x, b); },
                ret.begin());
            return ret;
        }
    }
    else if constexpr (x_rank >= 1 && y_rank == 0)
    {
        using XV = typename XT::value_type;
        using RV = decltype(fn(XV{}, y));
        if constexpr (is_movable_v<X&&> && std::is_same_v<RV, XV>)
        {
            x.for_each([=](auto& a) { a = fn(a, y); });
            return std::move(x);
        }
        else
        {
            ndarray<RV, x_rank> ret(x.dims());
            x.for_each([=](const auto& a, auto& r) { r = fn(a, y); },
                ret.begin());
            return ret;
        }
    }
    else
    {
        static_assert(x_rank == y_rank, WL_ERROR_OPERAND_RANK);
        if (!utils::check_dims(x.dims(), y.dims()))
            throw std::logic_error(WL_ERROR_OPERAND_DIMS);
        using XV = typename XT::value_type;
        using YV = typename YT::value_type;
        using RV = decltype(fn(XV{}, YV{}));
        if constexpr (is_movable_v<X&&> && std::is_same_v<XV, RV>)
        {
            y.for_each([=](const auto& b, auto& a) { a = fn(a, b); },
                x.begin());
            return std::move(x);
        }
        else if constexpr (XT::category == view_category::General)
        {
            if constexpr (is_movable_v<Y&&> && std::is_same_v<YV, RV>)
            {
                x.for_each([=](const auto& a, auto& b) { b = fn(a, b); },
                    y.begin());
                return std::move(y);
            }
            else if constexpr (YT::category == view_category::General)
            {
                if constexpr (std::is_same_v<XV, RV>)
                    return _listable_numerical_function(fn,
                        x.to_array(), std::forward<decltype(y)>(y));
                else
                    return _listable_numerical_function(fn,
                        std::forward<decltype(x)>(x), y.to_array());
            }
            else
            {
                ndarray<RV, x_rank> ret(x.dims());
                x.for_each([=](const auto& a, const auto& b, auto& r)
                    { r = fn(a, b); },
                    y.begin(), ret.begin());
                return ret;
            }
        }
        else
        {
            if constexpr (is_movable_v<Y&&>&& std::is_same_v<YV, RV>)
            {
                x.for_each([=](const auto& a, auto& b) { b = fn(a, b); },
                    y.begin());
                return std::move(y);
            }
            else
            {
                ndarray<RV, x_rank> ret(x.dims());
                y.for_each([=](const auto& b, const auto& a, auto& r)
                    { r = fn(a, b); },
                    x.begin(), ret.begin());
                return ret;
            }
        }
    }
}


#define WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_VARIADIC(name)          \
template<typename Iter, bool HasStride>                             \
auto _variadic_##name(const argument_pack<Iter, HasStride>& args)   \
{                                                                   \
    auto ret = val(args.get(0));                                    \
    const auto size = args.size();                                  \
    for (size_t i = 1u; i < size; ++i)                              \
        ret = name(std::move(ret), args.get(i));                    \
    return ret;                                                     \
}

#define WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(name)        \
if constexpr (is_argument_pack_v<remove_cvref_t<Y>>)                \
{                                                                   \
    if (y.size() == 0u)                                             \
        return std::forward<decltype(x)>(x);                        \
    else                                                            \
        return name(std::forward<decltype(x)>(x),                   \
            _variadic_##name(y));                                   \
}                                                                   \
else if constexpr (is_argument_pack_v<remove_cvref_t<X>>)           \
{                                                                   \
    if (x.size() == 0u)                                             \
        return std::forward<decltype(y)>(y);                        \
    else                                                            \
        return name(_variadic_##name(x),                            \
            std::forward<decltype(y)>(y));                          \
}                                                                   \
else

#define WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(name, expr)     \
constexpr auto name()                                               \
{                                                                   \
    return expr;                                                    \
}

#define WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(name)             \
template<typename X>                                                \
auto name(X&& x)                                                    \
{                                                                   \
    if constexpr (is_argument_pack_v<remove_cvref_t<X>>)            \
    {                                                               \
        using ArgType = remove_cvref_t<decltype(x.get(0))>;         \
        if constexpr (is_convertible_v<                             \
            remove_cvref_t<decltype(name())>,                       \
            remove_cvref_t<decltype(_variadic_##name(x))>>)         \
        {                                                           \
            if (x.size() == 0u)                                     \
                return cast<remove_cvref_t<                         \
                    decltype(_variadic_##name(x))>>(name());        \
            else                                                    \
                return _variadic_##name(x);                         \
        }                                                           \
        else                                                        \
        {                                                           \
            if (x.size() == 0u)                                     \
                throw std::logic_error(WL_ERROR_EMPTY_PACK);        \
            else                                                    \
                return _variadic_##name(x);                         \
        }                                                           \
    }                                                               \
    else                                                            \
        return std::forward<decltype(x)>(x);                        \
}

#define WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(name)              \
template<typename X1, typename X2, typename X3, typename... Xs>     \
auto name(X1&& x1, X2&& x2, X3&& x3, Xs&&... xs)                    \
{                                                                   \
    return name(name(std::forward<decltype(x1)>(x1),                \
        std::forward<decltype(x2)>(x2)),                            \
        std::forward<decltype(x3)>(x3),                             \
        std::forward<decltype(xs)>(xs)...);                         \
}

}

}
