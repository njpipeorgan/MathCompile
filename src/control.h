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
#include "arrayview.h"

namespace wl
{

struct _returns_function_tag {};
struct _returns_value_tag {};

template<typename A, typename B>
auto _branch_if_impl(bool cond, A&& a, B&& b, _returns_value_tag)
{
    if (cond)
        return std::forward<decltype(a)>(a)();
    else
        return std::forward<decltype(b)>(b)();
}

template<typename A, typename B>
auto _branch_if_impl(bool cond, A&& a, B&& b, _returns_function_tag)
{
    return
        [cond,
        a = std::forward<decltype(a)>(a)(),
        b = std::forward<decltype(b)>(b)()](auto&&... args)
    {
        using AT = remove_cvref_t<decltype(
            val(a(std::forward<decltype(args)>(args)...)))>;
        using BT = remove_cvref_t<decltype(
            val(a(std::forward<decltype(args)>(args)...)))>;
        static_assert(std::is_same_v<AT, BT>, "badfunctype");
        if (cond)
            return val(a(std::forward<decltype(args)>(args)...));
        else
            return val(b(std::forward<decltype(args)>(args)...));
    };
}

template<typename A, typename B>
auto branch_if(bool cond, A&& a, B&& b)
{
    using AType = remove_cvref_t<decltype(a())>;
    using BType = remove_cvref_t<decltype(b())>;
    if constexpr (is_value_type_v<AType>)
    {
        static_assert(std::is_same_v<AType, BType>, "badargtype");
        return _branch_if_impl(cond, 
            std::forward<decltype(a)>(a), std::forward<decltype(b)>(b), 
            _returns_value_tag{});
    }
    else
    {
        static_assert(!is_value_type_v<BType>, "badargtype");
        return _branch_if_impl(cond,
            std::forward<decltype(a)>(a), std::forward<decltype(b)>(b),
            _returns_function_tag{});
    }
}

template<typename X, typename Y>
auto native_if(boolean cond, X&& x, Y&& y)
{
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto YR = array_rank_v<YT>;
    static_assert(XR == YR, "badargrank");
    if constexpr (XR == 0u)
    {
        static_assert(std::is_same_v<XT, YT>, "badargtype");
        if (cond)
            return std::forward<decltype(x)>(x);
        else
            return std::forward<decltype(y)>(y);
    }
    else
    {
        if (cond)
            return std::forward<decltype(x)>(x).to_array();
        else
            return std::forward<decltype(y)>(y).to_array();
    }
}

/*
template<typename... Conds>
auto _which_conditions(Conds&&... conds)
{
    static_assert(std::conjunction_v<std::is_same<
        remove_cvref_t<decltype(conds())>, boolean>...>, "badfunctype");
    size_t n = 0u;
    [[maybe_unused]] auto _1 = ((conds() ? true : (++n, false)) || ...);
    return n;
}


template<typename... Cases>
auto _which_cases(Cases&&... cases)
{
    //using ResultTypes = remove_cvref_t<decltype(cases())>;
    return std::make_tuple(std::forward<decltype(cases)>(cases)...);
}

template<typename Cases>
auto which(const size_t n, Cases&& cases)
{
    
}
*/

}
