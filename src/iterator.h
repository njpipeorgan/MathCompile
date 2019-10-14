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
#include "numerical.h"

namespace wl
{

template<typename T, bool HasVariable>
struct step_iterator
{
    static constexpr bool has_variable = HasVariable;

    T begin_;
    T step_;
    size_t length_;

    step_iterator(T begin, T step, size_t length) :
        begin_{begin}, step_{step}, length_{length}
    {
    }

    size_t length() const
    {
        return length_;
    }

    auto operator[](size_t i) const
    {
        return begin_ + step_ * T(i);
    }
};

template<typename T>
struct list_iterator
{
    static constexpr bool has_variable = true;

    std::vector<T> values_;

    list_iterator(std::vector<T>&& values) : values_{values}
    {
    }

    size_t length() const
    {
        return values_.size();
    }

    auto operator[](size_t i) const
    {
        return values_[i];
    }
};

template<bool HasVariable, typename Begin, typename End, typename Step>
auto make_step_iterator(Begin begin, End end, Step step)
{
    static_assert(is_real_v<Begin> && is_real_v<End> && is_real_v<Step>,
        "badargtype");
    using T = common_type_t<Begin, Step>;
    if (step == Step(0))
        throw std::logic_error("badvalue");
    if constexpr (is_integral_v<T>)
    {
        auto diff = ptrdiff_t(wl::integer_part(end - begin));
        if ((step > Step(0) && diff < 0) || (step < Step(0) && diff > 0))
            return step_iterator<T, HasVariable>(begin, step, 0u);
        else
            return step_iterator<T, HasVariable>(begin, step,
                size_t(diff / ptrdiff_t(step)) + 1u);
    }
    else
    {
        auto diff = T(end - begin);
        if ((step > Step(0) && diff < 0) || (step < Step(0) && diff > 0))
            return step_iterator<T, HasVariable>(begin, step, 0u);
        else
        {
            size_t length = wl::integer_part(diff / step) + 1u;
            auto remain = T(begin + (length - 1u) * step) - T(end);
            if (step * remain > T(0))
                --length;
            return step_iterator<T, HasVariable>(begin, step, length);
        }
    }
}

template<typename Begin, typename End, typename Step>
auto var_iterator(Begin begin, End end, Step step)
{
    return make_step_iterator<true>(begin, end, step);
}

template<typename Begin, typename End>
auto var_iterator(Begin begin, End end)
{
    return make_step_iterator<true>(begin, end, int8_t(1));
}

template<typename Begin, typename End>
auto var_iterator(End end)
{
    return make_step_iterator<true>(int8_t(1), end, int8_t(1));
}

template<typename Any>
auto var_iterator(Any&& any)
{
    using Type = remove_cvref_t<Any>;

    if constexpr (is_real_v<Type>)
    {
        if constexpr (is_integral_v<Type>)
            return make_step_iterator<true>(Type(1), any, Type(1));
        else
            return make_step_iterator<true>(int64_t(1), any, int64_t(1));
    }
    else
    {
        static_assert(array_rank_v<Type> == 1u, "badargtype");
        if constexpr (is_movable_v<Type&&>)
            return list_iterator(std::move(any.data_));
        else
        {
            std::vector<typename Type::value_type> buffer(any.size());
            any.copy_to(buffer.begin());
            return list_iterator(std::move(buffer));
        }
    }
}

template<typename Any>
auto iterator(Any&& any)
{
    static_assert(is_real_v<remove_cvref_t<Any>>, "badargtype");
    return make_step_iterator<false>(int8_t(1), any, int8_t(1));
}

}
