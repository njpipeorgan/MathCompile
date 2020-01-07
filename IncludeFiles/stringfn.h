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
        auto sizes = utf8::size_properties{};
        for (size_t i = 0; i < arg.size(); ++i)
            sizes += _string_join_sizes_by_args_impl(arg.get(i));
        return sizes;
    }
    else if constexpr (array_rank_v<Arg> >= 1u)
    {
        static_assert(is_string_v<value_type_t<Arg>>, WL_ERROR_STRING_ONLY);
        auto sizes = utf8::size_properties{};
        arg.for_each([&](const auto& x)
            { sizes += _string_join_sizes_by_args_impl(x); });
        return sizes;
    }
    else
    {
        static_assert(is_string_v<Arg>, WL_ERROR_STRING_ONLY);
        return utf8::size_properties{arg.byte_size(), arg.size()};
    }
}

template<typename... Args>
auto _string_join_sizes_by_args(const Args&... args)
{
    return (utf8::size_properties{} +... +
        _string_join_sizes_by_args_impl(args));
}

template<typename Char, typename Arg>
void _string_join_copy_by_args_impl(Char*& str, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        for (size_t i = 0; i < arg.size(); ++i)
            _string_join_copy_by_args_impl(str, args.get(i));
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
    auto ret = string(_string_join_sizes_by_args(args...));
    auto ret_data = ret.byte_data();
    _string_join_copy_by_args(ret_data, args...);
    *ret_data = '\0'; // append null character
    return ret;
}

}
