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
#include "const.h"
#include "arrayview.h"

namespace wl
{

struct _returns_function_tag {};
struct _returns_value_tag {};

template<typename... Ts>
struct _branch_type_check_impl;

template<typename T1, typename... Ts>
struct _branch_type_check_impl<T1, Ts...>
{
    static constexpr auto value = is_value_type_v<T1> ?
        std::conjunction_v<std::is_same<T1, Ts>...> :
        std::conjunction_v<std::bool_constant<!is_value_type_v<Ts>>...>;
};

template<typename... Ts>
struct _branch_type_check : _branch_type_check_impl<remove_cvref_t<Ts>...> {};

template<typename T1, typename...>
struct _branch_returns_value :
    std::bool_constant<is_value_type_v<remove_cvref_t<T1>>> {};

template<typename T, typename...>
struct _get_first_type
{
    using type = T;
};

template<typename... Ts>
using _get_first_type_t = typename _get_first_type<Ts...>::type;

template<typename A, typename B>
auto branch_if(const boolean cond, A&& a, B&& b)
{
    using AT = decltype(val(std::declval<A&&>()()));
    using BT = decltype(val(std::declval<B&&>()()));
    static_assert(_branch_type_check<AT, BT>::value, WL_ERROR_BRANCH_RETURN);
    if constexpr (_branch_returns_value<AT>::value)
    {
        return cond ? val(std::forward<decltype(a)>(a)()) :
            val(std::forward<decltype(b)>(b)());
    }
    else
    {
        return
            [cond,
            a = std::forward<decltype(a)>(a)(),
            b = std::forward<decltype(b)>(b)()
            ](auto&&... args)
        {
            auto ra = [&, a] {
                return a(std::forward<decltype(args)>(args)...); };
            auto rb = [&, b] {
                return b(std::forward<decltype(args)>(args)...); };
            return branch_if(cond, std::move(ra), std::move(rb));
        };
    }
}

template<typename... Conds>
auto _which_conditions(Conds&&... conds)
{
    static_assert(std::conjunction_v<std::is_same<
        remove_cvref_t<decltype(conds())>, boolean>...>,
        WL_ERROR_BRANCH_RETURN);
    size_t n = 0u;
    [[maybe_unused]] auto _1 = ((conds() ? true : (++n, false)) || ...);
    return n;
}

template<typename Ret, size_t... Is, typename... Cases>
auto _which_impl(const size_t n, _returns_value_tag,
    std::index_sequence<Is...>, Cases&&... cases)
{
    if constexpr (std::is_same_v<Ret, void_type>)
    {
        [[maybe_unused]] auto _1 = ((n == Is ?
            (std::forward<decltype(cases)>(cases)(), true) : false
            ) || ...);
        return const_null;
    }
    else
    {
        Ret ret;
        if (n >= sizeof...(Cases))
            throw std::logic_error("badargc");
        [[maybe_unused]] auto _1 = ((n == Is ?
            (ret = val(std::forward<decltype(cases)>(cases)()), true) : false
            ) || ...);
        return ret;
    }
}

template<size_t... Is, typename... Cases>
auto _which_impl(const size_t n, _returns_function_tag,
    std::index_sequence<Is...>, Cases&&... cases)
{
    return
        [n,
        cases = std::make_tuple(std::forward<decltype(cases)>(cases)()...)
        ](auto&&... args)
    {
        return _which_impl(n, std::get<Is>(cases)...);
    };
}

template<typename... Cases>
auto which(const size_t n, Cases&&... cases)
{
    static_assert(_branch_type_check<
        decltype(val(std::declval<Cases&&>()()))...>::value,
        WL_ERROR_BRANCH_RETURN);
    using FirstType =
        decltype(val(std::declval<_get_first_type_t<Cases&&...>>()()));
    if constexpr (_branch_returns_value<FirstType>::value)
    {
        return _which_impl<remove_cvref_t<FirstType>>(
            n, _returns_value_tag{},
            std::make_index_sequence<sizeof...(Cases)>{},
            std::forward<decltype(cases)>(cases)...);
    }
    else
    {
        return _which_impl(
            n, _returns_function_tag{},
            std::make_index_sequence<sizeof...(Cases)>{},
            std::forward<decltype(cases)>(cases)...);
    }
}

template<typename Test, typename Incr, typename Body>
auto loop_for(Test test, Incr incr, Body body)
{
    static_assert(is_boolean_v<remove_cvref_t<decltype(test())>>,
        WL_ERROR_LOOP_TEST);
    try
    {
        while (test())
        {
            body();
            incr();
        }
    }
    catch (const loop_break&)
    {
    }
    return const_null;
}

template<typename Test, typename Body>
auto loop_while(Test test, Body body)
{
    static_assert(is_boolean_v<remove_cvref_t<decltype(test())>>,
        WL_ERROR_LOOP_TEST);
    try
    {
        while (test())
        {
            body();
        }
    }
    catch (const loop_break&)
    {
    }
    return const_null;
}

}
