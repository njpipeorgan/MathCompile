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

#include <tuple>
#include <type_traits>

#include "types.h"
#include "utils.h"
#include "const.h"

namespace wl
{

template<int64_t Id, typename Pattern>
struct _named_pattern
{
    Pattern pattern;
};

template<int64_t Id>
struct _named_replacement
{
};

template<typename... Head>
struct _pattern_blank
{
    static_assert(sizeof...(Head) <= 1u, WL_ERROR_INTERNAL);
};

template<typename... Head>
struct _pattern_blank_sequence
{
    static_assert(sizeof...(Head) <= 1u, WL_ERROR_INTERNAL);
};

template<typename... Head>
struct _pattern_blank_null_sequence
{
    static_assert(sizeof...(Head) <= 1u, WL_ERROR_INTERNAL);
};

template<typename... Patterns>
struct _pattern_alternatives
{
    static constexpr size_t size = sizeof...(Patterns);
    std::tuple<Patterns...> patterns;

    template<size_t I>
    auto get() const &
    {
        return std::get<I>(patterns);
    }
    template<size_t I>
    auto get() &&
    {
        return std::get<I>(patterns);
    }
};

template<typename Pattern>
struct _pattern_repeated
{
    Pattern pattern;
};

template<typename Pattern>
struct _pattern_repeated_null
{
    Pattern pattern;
};

template<typename Pattern>
struct _pattern_except
{
    Pattern pattern;
};

template<typename Pattern>
struct _pattern_longest
{
    Pattern pattern;
};

template<typename Pattern>
struct _pattern_shortest
{
    Pattern pattern;
};

template<typename Left, typename Right>
struct _pattern_rule
{
    Left left;
    Right right;
};

template<typename Pattern, typename Test>
struct _pattern_test
{
    Pattern pattern;
    Test test;
};

template<typename... Patterns>
struct _string_expression
{
    std::tuple<Patterns...> patterns;

    template<size_t I>
    auto get() const &
    {
        return std::get<I>(patterns);
    }
    template<size_t I>
    auto get() &&
    {
        return std::get<I>(patterns);
    }
};

template<typename Pattern, typename Condition>
struct _condition
{
    Pattern pattern;
    Condition condition;
};

template<int64_t Id, typename Pattern>
auto pattern(const_int<Id>, Pattern&& pattern)
{
    return _named_pattern<Id, remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}

template<typename Left, typename Right>
auto rule(Left&& left, Right&& right)
{
    return _pattern_rule<remove_cvref_t<Left>, remove_cvref_t<Right>>{
        std::forward<decltype(left)>(left),
        std::forward<decltype(right)>(right)};
}

template<typename... Head>
auto blank(const Head&...)
{
    return _pattern_blank<Head...>{};
}

template<typename... Head>
auto blank_sequence(const Head&...)
{
    return _pattern_blank_sequence<Head...>{};
}

template<typename... Head>
auto blank_null_sequence(const Head&...)
{
    return _pattern_blank_null_sequence<Head...>{};
}

template<typename... Patterns>
auto alternatives(Patterns&&... patterns)
{
    return _pattern_alternatives<remove_cvref_t<Patterns>...>{
        std::make_tuple(std::forward<decltype(patterns)>(patterns)...)};
}

template<typename Pattern>
auto repeated(Pattern&& pattern)
{
    return _pattern_repeated<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}

template<typename Pattern>
auto repeated_null(Pattern&& pattern)
{
    return _pattern_repeated_null<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}

template<typename Pattern>
auto except(Pattern&& pattern)
{
    return _pattern_except<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}

template<typename Pattern>
auto longest(Pattern&& pattern)
{
    return _pattern_longest<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}

template<typename Pattern>
auto shortest(Pattern&& pattern)
{
    return _pattern_shortest<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}

template<typename Pattern, typename Test>
auto pattern_test(Pattern&& pattern, Test&& test)
{
    return _pattern_test<remove_cvref_t<Pattern>, remove_cvref_t<Test>>{
        std::forward<decltype(pattern)>(pattern),
        std::forward<decltype(test)>(test)};
}

template<typename Pattern, typename Condition>
auto condition(Pattern&& pattern, Condition&& condition)
{
    return _condition<remove_cvref_t<Pattern>, remove_cvref_t<Condition>>{
        std::forward<decltype(pattern)>(pattern),
        std::forward<decltype(condition)>(condition)};
}

template<int64_t Id>
auto _replacement(const_int<Id>)
{
    return _named_replacement<Id>{};
}

template<typename... Patterns>
auto string_expression(Patterns&&... patterns)
{
    return _string_expression<remove_cvref_t<Patterns>...>{
        std::make_tuple(std::forward<decltype(patterns)>(patterns)...)};
}

}
