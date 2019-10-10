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

#include "types.h"
#include "traits.h"
#include "ndarray.h"
#include "utils.h"

namespace wl
{

#define WL_DEFINE_UNARY_BOOLEAN_FUNCTION(name, expr)                    \
template<typename X>                                                    \
auto name(X&& x)                                                        \
{                                                                       \
    static_assert(is_boolean_type_v<remove_cvref_t<X>>, "badargtype");  \
    return utils::listable_function([](boolean x) { return expr; },     \
        std::forward<decltype(x)>(x));                                  \
}

#define WL_DEFINE_BINARY_BOOLEAN_FUNCTION(name, expr)                   \
template<typename X, typename Y>                                        \
auto name(X&& x, Y&& y)                                                 \
{                                                                       \
    static_assert(is_boolean_type_v<remove_cvref_t<X>> &&               \
        is_boolean_type_v<remove_cvref_t<Y>>, "badargtype");            \
    return utils::listable_function(                                    \
        [](boolean x, boolean y) { return expr; },                      \
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));    \
}

WL_DEFINE_UNARY_BOOLEAN_FUNCTION(bool_not, !x)
WL_DEFINE_UNARY_BOOLEAN_FUNCTION(boole, int64_t(x))
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_and, x && y)
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_or, x || y)
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_xor, x ^ y)
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_nand, !(x && y))
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_nor, !(x || y))
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_xnor, !(x ^ y))
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(implies, !x || y)


}
