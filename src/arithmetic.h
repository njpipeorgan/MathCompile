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

//=============================================================================
// Scalar arithmetic functions
//=============================================================================

#define WL_DEFINE_REAL_SCALAR_OPERATIONS(name, oper)  \
template<typename X, typename Y>                      \
auto _scalar_##name(const X& x, const Y& y)           \
{ return common_type_t<X, Y>(x oper y); }

WL_DEFINE_REAL_SCALAR_OPERATIONS(plus, +)
WL_DEFINE_REAL_SCALAR_OPERATIONS(subtract, -)
WL_DEFINE_REAL_SCALAR_OPERATIONS(times, *)

template<typename X, typename Y>
auto _scalar_divide(const X& x, const Y& y)
{
    using C = common_type_t<common_type_t<X, Y>, float>;
    return C(C(x) / C(y));
}

#define WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(name, oper)             \
template<typename X, typename Y>                                    \
auto _scalar_##name(const complex<X>& x, const Y& y)                \
{  return complex<common_type_t<X, Y>>(                             \
    x.real() oper y, x.imag()); }                                   \
template<typename X, typename Y>                                    \
auto _scalar_##name(const X& x, const complex<Y>& y)                \
{ return complex<common_type_t<X, Y>>(                              \
    y.real() oper x, y.imag()); }                                   \
template<typename X, typename Y>                                    \
auto _scalar_##name(const complex<X>& x, const complex<Y>& y)       \
{ return complex<common_type_t<X, Y>>(                              \
    x.real() oper y.real(), x.imag() oper y.imag()); }

WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(plus, +)
WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(subtract, -)
WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(times, *)
WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(divide, /)

//=============================================================================
// Arithmetic functions
//=============================================================================

#define WL_DEFINE_ARITHMETIC_FUNCTION(name)                                 \
template<typename X, typename Y>                                            \
auto _scalar_or_array_##name(const X& x, const Y& y)                        \
{                                                                           \
    static_assert(!is_string_v<X> && !is_string_v<Y>, "badargtype");        \
    static_assert(!is_bool_v<X> && !is_bool_v<Y>, "badargtype");            \
    constexpr auto x_rank = array_rank_v<X>;                                \
    constexpr auto y_rank = array_rank_v<Y>;                                \
    if constexpr (x_rank > 0)                                               \
    {                                                                       \
        if constexpr (y_rank > 0)                                           \
        {                                                                   \
            static_assert(x_rank == y_rank, "badrank");                     \
            if (!utils::check_dims<x_rank>(x.dims_ptr(), y.dims_ptr()))     \
                throw std::logic_error("baddims");                          \
            ndarray<decltype(_scalar_plus(x[0], y[0])), x_rank> z(x.dims());\
            for (size_t i = 0; i < x.size(); ++i)                           \
                z[i] = _scalar_##name(x[i], y[i]);                          \
            return z;                                                       \
        }                                                                   \
        else                                                                \
        {                                                                   \
            ndarray<decltype(_scalar_plus(x[0], y)), x_rank> z(x.dims());   \
            for (size_t i = 0; i < x.size(); ++i)                           \
                z[i] = _scalar_##name(x[i], y);                             \
            return z;                                                       \
        }                                                                   \
    }                                                                       \
    else if constexpr (y_rank > 0)                                          \
    {                                                                       \
        return name(y, x);                                                  \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        return _scalar_##name(x, y);                                        \
    }                                                                       \
}                                                                           \
template<typename X, typename Y>                                            \
auto name(X&& x, Y&& y)                                                     \
{                                                                           \
    return _scalar_or_array_##name(                                         \
        val(std::forward<decltype(x)>(x)),                                  \
        val(std::forward<decltype(y)>(y)));                                 \
}

WL_DEFINE_ARITHMETIC_FUNCTION(plus)
WL_DEFINE_ARITHMETIC_FUNCTION(subtract)
WL_DEFINE_ARITHMETIC_FUNCTION(times)
WL_DEFINE_ARITHMETIC_FUNCTION(divide)


//=============================================================================
// Mutable scalar arithmetic functions
//=============================================================================

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

//=============================================================================
// Mutable arithmetic functions
//=============================================================================

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
