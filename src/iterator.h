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
#include <type_traits>

#include "types.h"
#include "traits.h"

namespace wl
{

template<typename T>
struct step_iterator
{
    static constexpr bool has_variable = true;

    T begin_;
    T step_;
    size_t length_;

    step_iterator(T begin, T end, T step)
    {
        begin_ = begin;
        step_ = step;
        if (std::make_signed_t<T>(step) > 0)
        {
            length_ = end > begin ? size_t((end - begin) / step + 1) : size_t(0);
            if (T(begin + step * int64_t(length_ - 1)) > end)
                --length_;
        }
        else
        {
            length_ = begin > end ? size_t((begin - end) / (-step) + 1) : size_t(0);
            if (T(begin + step * int64_t(length_ - 1)) < end)
                --length_;
        }
    }

    size_t length() const
    {
        return length_;
    }

    auto operator[](size_t i) const
    {
        return T(begin_ + step_ * int64_t(i));
    }
};

template<typename T, bool HasVariable>
struct unit_iterator
{
    static constexpr bool has_variable = HasVariable;

    T begin_;
    size_t length_;

    unit_iterator(T begin, T end)
    {
        begin_ = begin;
        length_ = end > begin ? size_t((end - begin) + 1) : size_t(0);
        if (begin + int64_t(length_ - 1) > end)
            --length_;
    }

    size_t length() const
    {
        return length_;
    }

    auto operator[](size_t i) const
    {
        return T(int64_t(i) + begin_);
    }
};

template<typename Begin, typename End>
auto iterator(Begin begin, End end)
{
    return unit_iterator<Begin, false>(begin, Begin(end));
}

template<typename Begin, typename End>
auto var_iterator(Begin begin, End end)
{
    return unit_iterator<Begin, true>(begin, Begin(end));
}

template<typename End>
auto iterator(End end)
{
    return iterator(int64_t(1), end);
}

template<typename End>
auto var_iterator(End end)
{
    return var_iterator(int64_t(1), end);
}

template<typename Begin, typename End, typename Step>
auto var_iterator(Begin begin, End end, Step step)
{
    using IterType = decltype(begin + step * int64_t(1));
    return iterator<IterType>(IterType(begin), IterType(end), IterType(step));
}

}
