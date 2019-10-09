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

#include <type_traits>

#include "traits.h"
#include "types.h"

namespace wl
{

template<typename A, typename B>
auto branch_if(bool cond, A&& a, B&& b)
{
    using AType = remove_cvref_t<decltype(a())>;
    using BType = remove_cvref_t<decltype(b())>;
    if constexpr (is_value_type_v<AType>)
    {
        static_assert(std::is_same_v<AType, BType>, "badargtype");
        if (cond)
            return std::forward<decltype(a)>(a)();
        else
            return std::forward<decltype(b)>(b)();
    }
    else // "if" returns a function
    {
        return
            [cond,
            a = std::forward<decltype(a)>(a)(),
            b = std::forward<decltype(b)>(b)()](auto&&... args)
        {
            if (cond)
                return a(std::forward<decltype(args)>(args)...);
            else
                return b(std::forward<decltype(args)>(args)...);
        };
    }
}

}
