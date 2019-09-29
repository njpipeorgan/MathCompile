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

#define WL_NO_STRING(type) static_assert(!is_string_v<type>, "invalid argument type")

#define WL_DEFINE_ARITHMETIC_FUNCTION(name, expr)                       \
template<typename X, typename Y>                                        \
auto name(const X& x, const Y& y)                                       \
{                                                                       \
    WL_NO_STRING(X);                                                    \
    WL_NO_STRING(Y);                                                    \
    if constexpr (is_array_v<X>)                                        \
    {                                                                   \
        if constexpr (is_array_v<Y>)                                    \
        {                                                               \
            if (x.dims() != y.dims())                                   \
                throw std::logic_error("dimension mismatch");           \
            ndarray<decltype(name(x[0], y[0])), X::Rank> z(x.dims());   \
            for (size_t i = 0; i < x.size(); ++i)                       \
                z[i] = name(x[i], y[i]);                                \
            return z;                                                   \
        }                                                               \
        else                                                            \
        {                                                               \
            ndarray<decltype(name(x[0], y)), X::Rank> z(x.dims());      \
            for (size_t i = 0; i < x.size(); ++i)                       \
                z[i] = name(x[i], y);                                   \
            return z;                                                   \
        }                                                               \
    }                                                                   \
    else if constexpr (is_array_v<Y>)                                   \
    {                                                                   \
        return name(y, x);                                              \
    }                                                                   \
    else                                                                \
    {                                                                   \
        return expr;                                                    \
    }                                                                   \
}

WL_DEFINE_ARITHMETIC_FUNCTION(plus, x + y)
WL_DEFINE_ARITHMETIC_FUNCTION(subtract, x - y)
WL_DEFINE_ARITHMETIC_FUNCTION(times, x * y)
WL_DEFINE_ARITHMETIC_FUNCTION(divide, n(x) / y)

#define WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(name, expr)               \
template<typename X, typename Y>                                        \
auto name(X& x, const Y& y)                                             \
{                                                                       \
    WL_NO_STRING(X);                                                    \
    WL_NO_STRING(Y);                                                    \
    if constexpr (is_array_v<X>)                                        \
    {                                                                   \
        if constexpr (is_array_v<Y>)                                    \
        {                                                               \
            if (x.dims() != y.dims())                                   \
                throw std::logic_error("dimension mismatch");           \
            for (size_t i = 0; i < x.size(); ++i)                       \
                name(x[i], y[i]);                                       \
            return x;                                                   \
        }                                                               \
        else                                                            \
        {                                                               \
            for (size_t i = 0; i < x.size(); ++i)                       \
                name(x[i], y);                                          \
            return x;                                                   \
        }                                                               \
    }                                                                   \
    else                                                                \
    {                                                                   \
        static_assert(!is_array_v<Y>, "");                              \
        return expr;                                                    \
    }                                                                   \
}


WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(add_to, x += y)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(subtract_from, x -= y)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(times_by, x *= y)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(divide_by, x /= y)

#undef WL_NO_STRING
}
