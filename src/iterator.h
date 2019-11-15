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

template<typename Array>
struct list_iterator
{
    static constexpr bool has_variable = true;
    using Iter = remove_cvref_t<decltype(
        std::declval<Array>().template view_begin<1u>())>;

    Array values_;
    Iter iter_;

    list_iterator(Array&& values) :
        values_{std::move(values)}, iter_{values_.template view_begin<1u>()}
    {
    }

    list_iterator(const Array& values) :
        values_{values}, iter_{values_.template view_begin<1u>()}
    {
    }

    size_t length() const
    {
        return values_.dims()[0];
    }

    auto operator[](size_t i) const
    {
        return *(iter_ + i);
    }
};

template<bool HasVariable, typename Begin, typename End, typename Step>
auto make_step_iterator(Begin begin, End end, Step step)
{
    static_assert(is_real_v<Begin> && is_real_v<End> && is_real_v<Step>,
        WL_ERROR_ITERATOR_TYPE);
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

    if constexpr (array_rank_v<Type> == 0u)
    {
        if constexpr (is_integral_v<Type>)
            return make_step_iterator<true>(Type(1), any, Type(1));
        else
            return make_step_iterator<true>(int64_t(1), any, int64_t(1));
    }
    else
    {
        if constexpr (is_movable_v<Any&&>)
            return list_iterator<Type>(std::move(any));
        else
        {
            auto copy = allows<view_category::Array>(
                std::forward<decltype(any)>(any));
            return list_iterator<decltype(copy)>(std::move(copy));
        }
    }
}

template<typename Any>
auto iterator(Any&& any)
{
    return make_step_iterator<false>(int8_t(1), any, int8_t(1));
}

}
