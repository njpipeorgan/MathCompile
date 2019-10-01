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

#include "types.h"
#include "traits.h"
#include "numerical.h"

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

#define WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(name, oper)                 \
template<typename X, typename Y>                                        \
auto _scalar_##name(const complex<X>& x, const Y& y)                    \
{  return complex<common_type_t<X, Y>>(                                 \
    x.real() oper y, x.imag()); }                                       \
template<typename X, typename Y>                                        \
auto _scalar_##name(const X& x, const complex<Y>& y)                    \
{ return complex<common_type_t<X, Y>>(                                  \
    y.real() oper x, y.imag()); }                                       \
template<typename X, typename Y>                                        \
auto _scalar_##name(const complex<X>& x, const complex<Y>& y)           \
{ return complex<common_type_t<X, Y>>(                                  \
    x.real() oper y.real(), x.imag() oper y.imag()); }

WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(plus, +)
WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(subtract, -)
WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(times, *)
WL_DEFINE_COMPLEX_SCALAR_OPERATIONS(divide, /)

//=============================================================================
// Arithmetic functions
//=============================================================================

#define WL_DEFINE_ARITHMETIC_FUNCTION(name)                             \
template<typename X, typename Y>                                        \
auto name(const X& x, const Y& y)                                       \
{                                                                       \
    static_assert(!is_string_v<X> && !is_string_v<Y>, "badargtype");    \
    static_assert(!is_bool_v<X> && !is_bool_v<Y>, "badargtype");        \
    if constexpr (is_array_v<X>)                                        \
    {                                                                   \
        if constexpr (is_array_v<Y>)                                    \
        {                                                               \
            using ResultType = decltype(_scalar_##name(x[0], y[0]));    \
            if (x.dims() != y.dims())                                   \
                throw std::logic_error("baddims");                      \
            ndarray<ResultType, X::rank> z(x.dims());                   \
            for (size_t i = 0; i < x.size(); ++i)                       \
                z[i] = _scalar_##name(x[i], y[i]);                      \
            return z;                                                   \
        }                                                               \
        else                                                            \
        {                                                               \
            using ResultType = decltype(_scalar_##name(x[0], y));       \
            ndarray<ResultType, X::rank> z(x.dims());                   \
            for (size_t i = 0; i < x.size(); ++i)                       \
                z[i] = _scalar_##name(x[i], y);                         \
            return z;                                                   \
        }                                                               \
    }                                                                   \
    else if constexpr (is_array_v<Y>)                                   \
    {                                                                   \
        return name(y, x);                                              \
    }                                                                   \
    else                                                                \
    {                                                                   \
        return _scalar_##name(x, y);                                    \
    }                                                                   \
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
// Mutable scalar arithmetic functions
//=============================================================================

#define WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(name)                     \
template<typename X, typename Y>                                        \
auto name(X& x, const Y& y)                                             \
{                                                                       \
    static_assert(!is_string_v<X> && !is_string_v<Y>, "badargtype");    \
    static_assert(!is_bool_v<X> && !is_bool_v<Y>, "badargtype");        \
    if constexpr (is_array_v<X>)                                        \
    {                                                                   \
        if constexpr (is_array_v<Y>)                                    \
        {                                                               \
            if (x.dims() != y.dims())                                   \
                throw std::logic_error("baddims");                      \
            for (size_t i = 0; i < x.size(); ++i)                       \
                _scalar_##name(x[i], y[i]);                             \
            return x;                                                   \
        }                                                               \
        else                                                            \
        {                                                               \
            for (size_t i = 0; i < x.size(); ++i)                       \
                _scalar_##name(x[i], y);                                \
            return x;                                                   \
        }                                                               \
    }                                                                   \
    else                                                                \
    {                                                                   \
        static_assert(!is_array_v<Y>, "");                              \
        return _scalar_##name(x, y);                                    \
    }                                                                   \
}

WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(add_to)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(subtract_from)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(times_by)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(divide_by)

}
