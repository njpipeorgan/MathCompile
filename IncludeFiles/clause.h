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
#include "arithmetic.h"
#include "utils.h"
#include "const.h"

namespace wl
{

template<typename Fn, typename First, typename... Rest>
auto _iterator_apply_first(Fn fn, const First& first, const Rest&... rest)
{
    if constexpr (sizeof...(Rest) == 0u)
    {
        if constexpr (First::has_variable)
            return val(fn(first[0]));
        else
            return val(fn());
    }
    else
    {
        if constexpr (First::has_variable)
        {
            const auto& arg1 = first[0];
            return _iterator_apply_first(
                [&](const auto&... args) { return fn(arg1, args...); },
                rest...);
        }
        else
            return _iterator_apply_first(fn, rest...);
    }
}

template<typename Skip, typename Fn, typename First, typename... Rest>
auto _clause_impl(Skip& skip_flag,
    Fn fn, const First& first, const Rest&... rest)
{
    if constexpr (sizeof...(Rest) == 0u)
    {
        WL_CHECK_ABORT_LOOP_BEGIN(first.length())
            for (auto i = _loop_begin; i < _loop_end; ++i)
            {
                if constexpr (std::is_same_v<Skip, bool>)
                    if (skip_flag)
                    {
                        skip_flag = false;
                        continue;
                    }
                if constexpr (First::has_variable)
                    fn(first[i]);
                else
                    fn();
            }
        WL_CHECK_ABORT_LOOP_END();
    }
    else
    {
        for (size_t i = 0; i < first.length(); ++i)
        {
            if constexpr (First::has_variable)
            {
                const auto& arg1 = first[i];
                _clause_impl(skip_flag,
                    [&](const auto&... args) { return fn(arg1, args...); },
                    rest...);
            }
            else
                _clause_impl(skip_flag, fn, rest...);
        }
    }
}

template<typename Fn, typename... Iters>
auto clause_do(Fn fn, const Iters&... iters)
{
    WL_TRY_BEGIN()
    static_assert(sizeof...(Iters) >= 1u, WL_ERROR_INTERNAL);
    wl::void_type skip_flag;    // skip flag is not used
    try
    {
        _clause_impl(skip_flag, fn, iters...);
    }
    catch (const loop_break&)
    {
    }
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Fn, typename... Iters>
auto clause_table(Fn fn, const Iters&... iters)
{
    WL_TRY_BEGIN()
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, WL_ERROR_INTERNAL);
    using InnerType = remove_cvref_t<
        decltype(_iterator_apply_first(fn, iters...))>;
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);

    if (outer_size == 0u)
    {
        if constexpr (array_rank_v<InnerType> == 0u)
            return ndarray<InnerType, outer_rank>{};
        else
        {
            return ndarray<value_type_t<InnerType>,
                outer_rank + array_rank_v<InnerType>>{};
        }
    }
    else
    {
        if constexpr (array_rank_v<InnerType> == 0u)
        {
            ndarray<InnerType, outer_rank> ret(outer_dims);
            auto ret_iter = ret.begin();
            wl::void_type skip_flag;    // skip flag is not used
            _clause_impl(skip_flag,
                [&](const auto&... args) { *ret_iter++ = fn(args...); },
                iters...);
            return ret;
        }
        else
        {
            using ValueType = typename InnerType::value_type;
            constexpr auto inner_rank = array_rank_v<InnerType>;
            auto first_item = _iterator_apply_first(fn, iters...);
            auto inner_dims = first_item.dims();
            auto all_dims = utils::dims_join(outer_dims, inner_dims);
            ndarray<ValueType, outer_rank + inner_rank> ret(all_dims);

            auto ret_iter = ret.template view_begin<outer_rank>();
            first_item.copy_to(ret_iter.begin());
            ++ret_iter;

            bool skip_flag = true;      // skip flag is used
            _clause_impl(skip_flag,
                [&](const auto&... args)
                {
                    auto item = fn(args...);
                    if (!utils::check_dims(ret_iter.dims(), item.dims()))
                        throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
                    item.copy_to(ret_iter.begin());
                    ++ret_iter;
                },
                iters...);
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Fn, typename... Iters>
auto clause_sum(Fn fn, const Iters&... iters)
{
    WL_TRY_BEGIN()
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, WL_ERROR_INTERNAL);
    using InnerType = remove_cvref_t<
        decltype(_iterator_apply_first(fn, iters...))>;
    static_assert(is_numerical_type_v<InnerType>, WL_ERROR_SUM_ELEMENT);
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);

    if (outer_size == 0u)
    {
        if constexpr (is_arithmetic_v<InnerType>)
            return InnerType{};
        else
            throw std::logic_error(WL_ERROR_SUM_ZERO_SIZE);
    }
    else
    {
        auto ret = _iterator_apply_first(fn, iters...);
        bool skip_flag = true;      // skip flag is not used
        if constexpr (array_rank_v<InnerType> >= 1u)
        {
            _clause_impl(skip_flag,
                [&](const auto&... args)
                {
                    add_to(ret, fn(args...));
                },
                iters...);
        }
        else
        {
            _clause_impl(skip_flag,
                [&](const auto&... args) { add_to(ret, fn(args...)); },
                iters...);
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Fn, typename... Iters>
auto clause_product(Fn fn, const Iters&... iters)
{
    WL_TRY_BEGIN()
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, WL_ERROR_INTERNAL);
    using InnerType = remove_cvref_t<
        decltype(_iterator_apply_first(fn, iters...))>;
    static_assert(is_numerical_type_v<InnerType>, WL_ERROR_SUM_ELEMENT);
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);

    if (outer_size == 0u)
    {
        if constexpr (is_arithmetic_v<InnerType>)
            return InnerType(int8_t(1));
        else
            throw std::logic_error(WL_ERROR_PRODUCT_ZERO_SIZE);
    }
    else
    {
        auto ret = _iterator_apply_first(fn, iters...);
        bool skip_flag = true;      // skip flag is not used
        _clause_impl(skip_flag,
            [&](const auto&... args)
            {
                times_by(ret, fn(args...));
            },
            iters...);
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
