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
#include "iterator.h"

namespace wl
{

template<typename Fn, typename FirstIter, typename NextIter, typename... RestIters>
auto clause_do(Fn fn, FirstIter&& first_iter, NextIter&& next_iter, RestIters&&... rest_iters)
{
    for (size_t i = 0; i < first_iter.length(); ++i)
    {
        if constexpr (FirstIter::has_variable)
        {
            const auto arg1 = first_iter[i];
            clause_do(
                [fn, arg1](auto&&... args) { return fn(arg1, std::forward<decltype(args)>(args)...); },
                std::forward<decltype(next_iter)>(next_iter),
                std::forward<decltype(rest_iters)>(rest_iters)...);
        }
        else
        {
            clause_do(
                fn,
                std::forward<decltype(next_iter)>(next_iter),
                std::forward<decltype(rest_iters)>(rest_iters)...);
        }
    }
}

template<typename Fn, typename LastIter>
auto clause_do(Fn fn, LastIter&& last_iter)
{
    for (size_t i = 0; i < last_iter.length(); ++i)
    {
        if constexpr (LastIter::has_variable)
        {
            const auto arg1 = last_iter[i];
            fn(arg1);
        }
        else
        {
            fn();
        }
    }
}

}
