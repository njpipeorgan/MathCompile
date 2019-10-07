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

#include "traits.h"
#include "types.h"
#include "ndarray.h"

namespace wl
{

template<typename T, size_t R, typename Function>
auto select(const ndarray<T, R>& input, Function f)
{
    if constexpr (R == 1)
    {
        std::vector<T> ret;
        input.for_each([&](const auto& x) { if (f(x)) ret.push_back(x); });
        return ndarray<T, 1u>(
            std::array<size_t, 1u>{ret.size()}, std::move(ret));
    }
    else if constexpr (R >= 2)
    {
        auto ret_dims = input.dims();
        ret_dims[0] = 0u;
        ndarray<T, R> ret(ret_dims);
        size_t item_size = ret.template partial_size<1u>();
        size_t item_count = 0;
        auto view_iter = part(input, 1u);
        auto view_end = part(input, -1).step_forward();
        for (; !view_iter.view_pos_equal(view_end); view_iter.step_forward())
        {
            if (f(view_iter))
            {
                ret.resize(++item_count, item_size);
                view_iter.copy_to(ret.begin() + (item_count - 1u) * item_size);
            }
        }
        return ret;
    }
}


}
