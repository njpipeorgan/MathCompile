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
#include "iterator.h"

namespace wl
{

template<typename Fn, typename First, typename Next, typename... Rest>
auto clause_do(Fn fn, 
    const First& first, const Next& next, const Rest&... rest)
{
    for (size_t i = 0; i < first.length(); ++i)
    {
        if constexpr (First::has_variable)
        {
            const auto& arg1 = first[i];
            clause_do([fn, &arg1](auto&&... args) {
                return fn(arg1, std::forward<decltype(args)>(args)...); },
                next, rest...);
        }
        else
            clause_do(fn, next, rest...);
    }
}

template<typename Fn, typename Last>
auto clause_do(Fn fn, const Last& last)
{
    for (size_t i = 0; i < last.length(); ++i)
    {
        if constexpr (Last::has_variable)
            fn(last[i]);
        else
            fn();
    }
}

template<typename Fn, typename... Iters>
auto clause_table(Fn fn, const Iters&... iters)
{
    constexpr auto rank = sizeof...(iters);
    using ValueType = remove_cvref_t<decltype(fn(iters[0]...))>;
    static_assert(rank >= 1u, "internal");
    static_assert(is_arithmetic_v<ValueType>, "internal"); // for now
    auto dims = std::array<size_t, rank>{iters.length()...};
    ndarray<ValueType, rank> ret(dims);
    auto ret_iter = ret.begin();
    clause_do([&](auto&&... args) {
        *ret_iter++ = fn(std::forward<decltype(args)>(args)...); },
        iters...);
    return ret;
}


}
