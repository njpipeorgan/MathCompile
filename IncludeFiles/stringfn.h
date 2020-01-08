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

#include <cassert>
#include <cstring>

#include <vector>

#include "types.h"
#include "arrayview.h"
#include "utils.h"
#include "u8string.h"

namespace wl
{

template<typename X>
auto string_length(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_string_type_v<X>, WL_ERROR_STRING_TYPE_ONLY);
    auto pure = [](const string& x)
    {
        return int64_t(x.size());
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Arg>
auto _string_join_sizes_by_args_impl(const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        size_t byte_size = 0;
        for (size_t i = 0; i < arg.size(); ++i)
            byte_size += arg.get(i).byte_size();
        return byte_size;
    }
    else if constexpr (array_rank_v<Arg> >= 1u)
    {
        static_assert(is_string_v<value_type_t<Arg>>, WL_ERROR_STRING_ONLY);
        size_t byte_size = 0;
        arg.for_each([&](const auto& x) { byte_size += x.byte_size(); });
        return byte_size;
    }
    else
    {
        static_assert(is_string_v<Arg>, WL_ERROR_STRING_ONLY);
        return arg.byte_size();
    }
}

template<typename... Args>
auto _string_join_sizes_by_args(const Args&... args)
{
    return (size_t(0) +... + _string_join_sizes_by_args_impl(args));
}

template<typename Char, typename Arg>
void _string_join_copy_by_args_impl(Char*& str, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        for (size_t i = 0; i < arg.size(); ++i)
            _string_join_copy_by_args_impl(str, arg.get(i));
    }
    else if constexpr (array_rank_v<Arg> >= 1u)
    {
        arg.for_each([&](const auto& x)
            { _string_join_copy_by_args_impl(str, x); });
    }
    else
    {
        const size_t byte_size = arg.byte_size();
        utils::restrict_copy_n(arg.byte_data(), byte_size, str);
        str += byte_size;
    }
}

template<typename Char, typename... Args>
auto _string_join_copy_by_args(Char*& str, const Args&... args)
{
    [[maybe_unused]] const auto& _1 = (
        _string_join_copy_by_args_impl(str, args), ..., 0);
}

template<typename... Args>
auto string_join(const Args&... args)
{
    WL_TRY_BEGIN()
    auto ret = string(_string_join_sizes_by_args(args...));
    auto ret_data = ret.byte_data();
    _string_join_copy_by_args(ret_data, args...);
    ret.place_null_character();
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Spec>
auto _string_take_impl_ascii_only()
{
    return string();
}

template<typename Spec>
auto _string_take_impl_not_ascii_only(const string& str, const Spec& spec)
{
    if constexpr (array_rank_v<Spec> == 0u)
    {
        static_assert(is_integral_v<Spec>, WL_ERROR_PART_SPEC_INTEGRAL);
        const auto begin = str.begin();
        const auto end = str.end();
        if (spec == Spec(0))
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        if (spec > 0)
        { // take substring from the beginning
            auto mid = begin;
            mid.apply_offset(ptrdiff_t(spec), end);
            if (mid.pointer() > end.pointer())
                throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
            const auto byte_size = size_t(mid.byte_difference(begin));
            const auto ascii_only = byte_size == size_t(spec);
            return string(begin.pointer(), byte_size, ascii_only);
        }
        else if (spec < 0)
        { // take substring from the end
            auto mid = end;
            mid.apply_offset(ptrdiff_t(spec), begin);
            if (mid.pointer() < begin.pointer())
                throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
            const auto byte_size = size_t(end.byte_difference(mid));
            const auto ascii_only = byte_size == size_t(-ptrdiff_t(spec));
            return string(mid.pointer(), byte_size, ascii_only);
        }
    }
    return string();
}

template<typename Spec>
auto string_take(const string& str, const Spec& spec)
{
    if (str.ascii_only())
        return _string_take_impl_not_ascii_only(str, spec);
    else
        return _string_take_impl_not_ascii_only(str, spec);
}

}
