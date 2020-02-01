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

#include <array>
#include <memory>
#include <tuple>

#include "types.h"
#include "arrayview.h"
#include "utils.h"
#include "u8string.h"
#include "numerical.h"
#include "pattern.h"

#include "pcre2_wrapper.h"

namespace wl
{

template<typename X>
auto string_length(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_string_type_v<X>, WL_ERROR_STRING_TYPE_ONLY);
    auto pure = [](const auto& str)
    {
        return int64_t(str.size());
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any>
auto _to_string(const Any& any, const size_t min_length = 0)
{
    if constexpr (array_rank_v<Any> == 0u)
    {
        if constexpr (is_integral_v<Any>)
        {
            if (any == Any(0))
                return string("0");

            constexpr size_t buffer_size = 24;
            utf8::char_t buffer[buffer_size];
            auto ptr = buffer + buffer_size;
            size_t length = 0;
            auto value = any;
            while (value || length < min_length)
            {
                auto rem = value % Any(10);
                value = value / Any(10);
                if constexpr (std::is_unsigned_v<Any>)
                    *--ptr = utf8::char_t('0' + rem);
                else
                    *--ptr = utf8::char_t('0' + (rem < 0 ? -rem : rem));
                ++length;
            }
            if (any < Any(0))
                *--ptr = '-';
            return string(ptr, buffer_size - size_t(ptr - buffer));
        }
        else
        {
            assert(false);
            return string();
        }
    }
    else
    {
        assert(false);
        return string();
    }
}

template<typename Arg>
auto _string_join_attributes_by_args_impl(
    size_t& byte_size, bool& ascii_only, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        for (size_t i = 0; i < arg.size(); ++i)
        {
            byte_size += arg.get(i).byte_size();
            if (ascii_only)
                ascii_only = arg.get(i).ascii_only();
        }
        return byte_size;
    }
    else if constexpr (array_rank_v<Arg> >= 1u)
    {
        static_assert(is_string_v<value_type_t<Arg>>, WL_ERROR_STRING_ONLY);
        arg.for_each([&](const auto& x)
            {
                byte_size += x.byte_size();
                if (ascii_only)
                    ascii_only = x.ascii_only();
            });
        return byte_size;
    }
    else
    {
        static_assert(is_string_view_v<Arg>, WL_ERROR_STRING_ONLY);
        byte_size += arg.byte_size();
        if (ascii_only)
            ascii_only = arg.ascii_only();
    }
}

template<typename... Args>
auto _string_join_attributes_by_args(const Args&... args)
{
    size_t total_size = 0u;
    bool ascii_only = true;
    [[maybe_unused]] const auto& _1 = (
        _string_join_attributes_by_args_impl(
            total_size, ascii_only, args),
        ..., 0);
    return std::pair(total_size, ascii_only);
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
    auto [total_size, ascii_only] = _string_join_attributes_by_args(args...);
    auto ret = string(total_size, ascii_only);
    auto ret_data = ret.byte_data();
    WL_THROW_IF_ABORT()
    _string_join_copy_by_args(ret_data, args...);
    assert(ret.check_validity());
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Spec>
auto _string_take_impl_ascii_only()
{
    return string();
}

template<typename Iter, typename Offset>
Iter _string_take_find_offset(const Iter& begin, const Iter& end,
    const Offset offset, const bool adjust_end)
{
    if (std::is_unsigned_v<Offset> || offset > 0)
    {
        auto ret = begin;
        ret.apply_offset(offset - ptrdiff_t(!adjust_end), end);
        if (ret.get_pointer() > end.get_pointer())
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        return ret;
    }
    else if (offset < 0)
    {
        auto ret = end;
        ret.apply_offset(offset + ptrdiff_t(adjust_end), begin);
        if (ret.get_pointer() < begin.get_pointer())
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        return ret;
    }
    else
    {
        throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        return begin;
    }
}

template<typename Iter>
auto _string_take_impl_unicode_1arg(const Iter& begin, const Iter& end,
    ptrdiff_t offset)
{
    if (offset > 0)
    {
        const auto mid = _string_take_find_offset(begin, end, offset, true);
        return string_view(begin, mid, size_t(offset));
    }
    else
    {
        const auto mid = _string_take_find_offset(begin, end, offset, false);
        return string_view(mid, end, size_t(-offset));
    }
}

template<typename Iter>
auto _string_take_impl_unicode_2args(const Iter& begin, const Iter& end,
    ptrdiff_t offset, ptrdiff_t string_size)
{
    if (string_size <= 0)
        return string_view(begin, begin, size_t(0));
    const auto mid1 = _string_take_find_offset(
        begin, end, offset, false);
    const auto mid2 = _string_take_find_offset(
        mid1, end, size_t(string_size), true);
    const auto byte_size = size_t(mid2.byte_difference(mid1));
    return string_view(mid1, mid2, size_t(string_size));
}

template<typename String, typename Spec>
auto _string_take_impl_unicode(const String& str, const Spec& spec,
    ptrdiff_t total_size = -1)
{
    const auto begin = str.begin();
    const auto end = str.end();
    if constexpr (is_integral_v<Spec>)
    {
        if (spec == Spec(0))
            return string_view(begin, begin, size_t(0));
        else
            return _string_take_impl_unicode_1arg(
                begin, end, ptrdiff_t(spec));
    }
    else if constexpr (array_rank_v<Spec> == 1u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto spec_size = spec.size();
        std::array<ptrdiff_t, 2u> offsets{};
        spec.copy_to(offsets.data());

        if (spec_size == 1u)
        {
            if (offsets[0] == 0)
                throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
            return _string_take_impl_unicode_2args(
                begin, end, offsets[0], 1u);
        }
        else if (spec_size == 2u)
        {
            if (offsets[0] == 0 || offsets[1] == 0)
                throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);

            if (offsets[0] * offsets[1] < 0)
            {
                if (total_size < 0)
                    total_size = str.size();
                if (offsets[0] < 0)
                    offsets[0] += ptrdiff_t(total_size + 1u);
                else
                    offsets[1] += ptrdiff_t(total_size + 1u);
            }
            return _string_take_impl_unicode_2args(
                begin, end, offsets[0], offsets[1] - offsets[0] + 1u);
        }
        else
        {
            throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
        }
    }
    else if constexpr (array_rank_v<Spec> == 2u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto& valspec = allows<view_category::Simple>(spec);
        const auto ret_size = valspec.dims()[0];
        const auto spec_size = valspec.dims()[1];
        const auto spec_data = valspec.data();
        ndarray<string, 1u> ret(std::array<size_t, 1u>{ret_size});
        auto ret_data = ret.data();
        if (spec_size == 1u)
        {
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(_string_take_impl_unicode(str,
                    wl::list(spec_data[i])));
        }
        else if (spec_size == 2u)
        {
            if constexpr (std::is_signed_v<value_type_t<Spec>>)
            {
                if ((total_size < 0) && std::any_of(
                    spec_data, spec_data + 2u * ret_size,
                    [](auto i) { return i < 0; }))
                {
                    total_size = str.size();
                }
            }
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(_string_take_impl_unicode(str,
                    wl::list(spec_data[2u * i], spec_data[2u * i + 1u]),
                    total_size));
        }
        else
        {
            throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
        }
        return ret;
    }
    else
    {
        static_assert(always_false_v<Spec>, WL_ERROR_TAKE_SPEC_TYPE);
    }
}

template<typename String, typename Spec>
auto _string_take_impl_ascii(const String& str, const Spec& spec)
{
    const auto begin = str.byte_begin();
    const auto end = str.byte_end();
    const auto total_size = str.byte_size();
    if constexpr (is_integral_v<Spec>)
    {
        if (spec == Spec(0))
            return string_view(begin, begin, size_t(0));
        else if (size_t(wl::abs(spec)) > total_size)
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        else if (spec > Spec(0))
            return string_view(begin, begin + ptrdiff_t(spec), size_t(spec));
        else
            return string_view(end + ptrdiff_t(spec), end,
                size_t(-ptrdiff_t(spec)));

    }
    else if constexpr (array_rank_v<Spec> == 1u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto spec_size = spec.size();
        std::array<ptrdiff_t, 2u> offsets{};
        spec.copy_to(offsets.data());

        if (spec_size == 1u)
        {
            if (offsets[0] == 0 || size_t(wl::abs(offsets[0])) > total_size)
                throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
            auto substr_begin = (offsets[0] > 0) ?
                (begin + offsets[0] - 1) : (end + offsets[0]);
            return string_view(substr_begin, substr_begin + 1, size_t(1));
        }
        else if (spec_size == 2u)
        {
            if (offsets[0] == 0 || offsets[1] == 0)
                throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);

            if (offsets[0] < 0)
                offsets[0] += ptrdiff_t(total_size + 1u);
            if (offsets[1] < 0)
                offsets[1] += ptrdiff_t(total_size + 1u);
            if (offsets[0] > offsets[1])
                return string_view(begin, begin, size_t(0));
            if (size_t(offsets[0]) > total_size ||
                size_t(offsets[1]) > total_size)
                throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
            return string_view(begin + offsets[0] - 1,
                begin + offsets[1], size_t(offsets[1] - offsets[0] + 1));
        }
        else
        {
            throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
        }
    }
    else if constexpr (array_rank_v<Spec> == 2u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto& valspec = allows<view_category::Simple>(spec);
        const auto ret_size = valspec.dims()[0];
        const auto spec_size = valspec.dims()[1];
        const auto spec_data = valspec.data();
        ndarray<string, 1u> ret(std::array<size_t, 1u>{ret_size});
        auto ret_data = ret.data();
        if (spec_size == 1u)
        {
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(_string_take_impl_ascii(str,
                    wl::list(spec_data[i])));
        }
        else if (spec_size == 2u)
        {
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(_string_take_impl_ascii(str,
                    wl::list(spec_data[2u * i], spec_data[2u * i + 1u])));
        }
        else
        {
            throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
        }
        return ret;
    }
    else
    {
        static_assert(always_false_v<Spec>, WL_ERROR_TAKE_SPEC_TYPE);
    }
}

template<typename String, typename Spec>
auto string_take(String&& str, const Spec& spec)
{
    WL_TRY_BEGIN()
    using StringT = remove_cvref_t<String>;
    static_assert(is_string_view_v<StringT>, WL_ERROR_STRING_ONLY);

    WL_THROW_IF_ABORT()
    if constexpr (array_rank_v<Spec> <= 1u)
    {
        auto view = u8string_view{};
        if (str.ascii_only())
            view = _string_take_impl_ascii(str, spec);
        else
            view = _string_take_impl_unicode(str, spec);

        if constexpr (is_string_v<StringT> && std::is_rvalue_reference_v<String&&>)
        { // must return a string
            return string(view);
        }
        else
        { // can return a string view
            return view;
        }
    }
    else
    {
        if (str.ascii_only())
            return _string_take_impl_ascii(str, spec);
        else
            return _string_take_impl_unicode(str, spec);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<int64_t Id, int64_t... List>
struct _pattern_id_list_find_impl;

template<int64_t Id, int64_t First, int64_t... Rest>
struct _pattern_id_list_find_impl<Id, First, Rest...>
{
    static constexpr auto next =
        _pattern_id_list_find_impl<Id, Rest...>::value;
    static constexpr auto value =
        (next == -1) ? int64_t(-1) : next + 1;
};

template<int64_t Id, int64_t... Rest>
struct _pattern_id_list_find_impl<Id, Id, Rest...>
{
    static constexpr auto value = int64_t(1);
};

template<int64_t Id>
struct _pattern_id_list_find_impl<Id>
{
    static constexpr auto value = int64_t(-1);
};

template<int64_t Start, int64_t... List>
struct _pattern_id_list_max;

template<int64_t Start, int64_t First, int64_t... Rest>
struct _pattern_id_list_max<Start, First, Rest...> :
    _pattern_id_list_max<std::max(Start, First), Rest...> {};

template<int64_t Final>
struct _pattern_id_list_max<Final>
{
    static constexpr auto value = Final;
};

template<int64_t... Ids>
struct _pattern_id_list
{
    static constexpr auto size = sizeof...(Ids);

    template<int64_t Id>
    static constexpr auto find(const_int<Id>)
    {
        return _pattern_id_list_find_impl<Id, Ids...>::value;
    }
    template<int64_t Id>
    static constexpr auto append(const_int<Id>)
    {
        return _pattern_id_list<Ids..., Id>{};
    }
};

template<typename... Conditions>
struct _pattern_condition_list
{
    static constexpr auto size = sizeof...(Conditions);
    std::tuple<Conditions...> conditions_;

    template<typename Condition, size_t... Is>
    auto append_impl(Condition&& condition, std::index_sequence<Is...>) &&
    {
        return _pattern_condition_list<Conditions..., Condition>{
            std::make_tuple(std::get<Is>(std::move(conditions_))...,
                std::move(condition))};
    }
    
    template<typename Condition>
    auto append(Condition condition) &&
    {
        return std::move(*this).append_impl(std::move(condition),
            std::make_index_sequence<size>{});
    }

    template<size_t I>
    auto get() const -> const auto&
    {
        return std::get<I>(conditions_);
    }
};

namespace strexp
{

template<typename ConditionList, typename PatternIdList_>
struct compilation_state
{
    using PatternIdList = PatternIdList_;
    ConditionList conditions;

    template<int64_t Id>
    auto append_id(const_int<Id>) &&
    {
        using NewList = decltype(PatternIdList::append(const_int<Id>{}));
        return compilation_state<ConditionList, NewList>{std::move(conditions)};
    }

    template<typename Condition>
    auto append_condition(Condition condition) &&
    {
        auto new_conditions =
            std::move(conditions).append(std::move(condition));
        return compilation_state<decltype(new_conditions), PatternIdList>{
            std::move(new_conditions)};
    }
};

template<typename ConditionList, typename PatternIdList_>
struct compiled_pattern
{
    pcre2::regex_t regex_ptr;
    pcre2::match_context_t match_context_ptr;
    const ConditionList conditions;
    std::unique_ptr<const ConditionList*> conditions_ptr;
    const string regex_text;
    using PatternIdList = PatternIdList_;

    compiled_pattern(pcre2::regex_t in_regex_ptr,
        ConditionList&& in_conditions, string&& in_regex_text) :
        regex_ptr{in_regex_ptr},
        match_context_ptr{pcre2_match_context_create_8(nullptr)},
        conditions{std::move(in_conditions)},
        conditions_ptr{new (const ConditionList*){nullptr}},
        regex_text{std::move(in_regex_text)}
    {
        set_callout();
    }

    compiled_pattern(pcre2::regex_t in_regex_ptr) :
        regex_ptr{in_regex_ptr},
        match_context_ptr{pcre2_match_context_create_8(nullptr)},
        conditions{},
        conditions_ptr{new (const ConditionList*){nullptr}},
        regex_text{""}
    {
        set_callout();
    }

    void set_callout()
    {
        pcre2_set_callout_8(match_context_ptr.get(),
            &pcre2::callout_function<PatternIdList, ConditionList>,
            conditions_ptr.get());
    }
};

template<typename FormatIdList_>
struct compiled_format
{
    using FormatIdList = FormatIdList_;
    const string text;
};

template<typename Pattern, typename Format>
struct compiled_rule
{
    const Pattern pattern;
    const Format format;
};

template<typename T>
struct _is_compiled_pattern : std::false_type {};

template<typename C, typename P>
struct _is_compiled_pattern<compiled_pattern<C, P>> : std::true_type {};

template<typename P, typename R>
struct _is_compiled_pattern<compiled_rule<P, R>> : std::true_type {};

template<typename T>
constexpr auto _is_compiled_pattern_v = _is_compiled_pattern<T>::value;

enum tag_t
{
    WithinGroup = 0,
    Alternation,
    Concatenation,
    MatchShortest,
    Repetition,
    Grouping,
    CharacterSet,
    Exception
};

template<int64_t Id, typename Pattern, typename State>
auto compile(_named_pattern<Id, Pattern> p, string& str, State s, tag_t tag)
{
    constexpr auto group_idx = State::PatternIdList::find(const_int<Id>{});
    if constexpr (group_idx < 0 || Id < 0)
    {
        // group does not exist; capture a new group
        // negative id means that the group is used for PatternTest
        str.join("(");
        auto s1 = std::move(s).append_id(const_int<Id>{});
        auto s2 = compile(std::move(p.pattern), str, std::move(s1),
            tag == MatchShortest ? MatchShortest : WithinGroup);
        str.join(")");
        return std::move(s2);
    }
    else
    { // group already exists; refer to that group
        str.join("\\g{").join(_to_string(group_idx)).join("}");
        return std::move(s);
    }
}

template<typename... Head, typename State>
auto compile(_pattern_blank<Head...>, string& str, State s, tag_t)
{
    str.join(".");
    return std::move(s);
}

template<typename... Head, typename State>
auto compile(_pattern_blank_sequence<Head...>, string& str, State s, tag_t tag)
{
    if (tag == MatchShortest)
        str.join(".+?");
    else if (tag < Repetition)
        str.join(".+");
    else
        str.join("(?:.+)");
    return std::move(s);
}

template<typename... Head, typename State>
auto compile(_pattern_blank_null_sequence<Head...>, string& str, State s,
    tag_t tag)
{
    if (tag == MatchShortest)
        str.join(".*?");
    else if (tag < Repetition)
        str.join(".*");
    else
        str.join("(?:.*)");
    return std::move(s);
}

template<size_t I = 0u, typename... Patterns, typename State>
auto compile_alternatives(_pattern_alternatives<Patterns...> p, string& str,
    State s, tag_t tag)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto&& pattern = std::move(p).template get<I>();
        auto s1 = compile(std::move(pattern), str, std::move(s), tag);
        if ((I + 1u < sizeof...(Patterns)) && tag == Alternation)
            str.join("|");
        auto s2 = compile_alternatives<I + 1u>(std::move(p), str,
            std::move(s1), tag);
        return std::move(s2);
    }
    else
    {
        return std::move(s);
    }
}

template<typename String>
auto is_single_character(const String& s)
{
    return (s.byte_size() == 1u) || (s.byte_size() <= 4u) && (s.size() == 1u);
}

template<size_t I = 0u, typename... Patterns>
auto compile_use_bracket_impl(
    const _pattern_alternatives<Patterns...>& patterns)
{
    if constexpr (I < sizeof...(Patterns))
    {
        const auto& p = patterns.template get<I>();
        using PT = remove_cvref_t<decltype(p)>;
        bool pass = false;
        if constexpr (is_string_view_v<remove_cvref_t<decltype(p)>>)
            pass = is_single_character(p);
        else
            pass = std::is_same_v<PT, _whitespace_character_type> ||
                std::is_same_v<PT, _word_character_type> ||
                std::is_same_v<PT, _letter_character_type> ||
                std::is_same_v<PT, _digit_character_type> ||
                std::is_same_v<PT, _hexadecimal_character_type> ||
                std::is_same_v<PT, _punctuation_character_type>;
        return pass && compile_use_bracket_impl<I + 1u>(patterns);
    }
    else
    {
        return true;
    }
}

template<typename... Patterns>
auto compile_use_bracket(const _pattern_alternatives<Patterns...>& p)
{
    return compile_use_bracket_impl<0u>(p);
}

template<typename Any>
auto compile_use_bracket(const Any& any)
{
    if constexpr (array_rank_v<Any> == 1u && is_string_type_v<Any>)
        return bool(all_true(any, [](const string& s)
            { return boolean(is_single_character(s)); }));
    else
        return false;
}

template<typename... Patterns, typename State>
auto compile(_pattern_alternatives<Patterns...> p, string& str, State s,
    tag_t tag)
{
    auto use_bracket = compile_use_bracket(p);
    if (tag == Exception && !use_bracket)
        throw std::logic_error(WL_ERROR_STRING_EXCEPT);
    if (tag == Exception)
        str.join("[^");
    else if (use_bracket)
        str.join("[");
    else if (tag >= Alternation)
        str.join("(?:");
    auto s1 = compile_alternatives(std::move(p), str, std::move(s),
        use_bracket ? CharacterSet : Alternation);
    if (use_bracket)
        str.join("]");
    else if (tag >= Alternation)
        str.join(")");
    return std::move(s1);
}

template<typename Pattern, typename State>
auto compile(_pattern_repeated<Pattern> p, string& str, State s, tag_t tag)
{
    if (p.min == 0u)
        str.join("(?:");
    auto s1 = compile(p.pattern, str, std::move(s),
        p.min == 0u ? WithinGroup : Concatenation);
    if (p.min == 0u)
    {
        str.join(")?");
        if (tag == MatchShortest)
            str.join("?");
    }
    auto s2 = compile(std::move(p).pattern, str, std::move(s1), Repetition);
    if (p.max_is_infinity())
        str.join("*");
    else
    {
        if (p.min >= 2u)
            str.join("{").join(_to_string(p.min - 1u)).join(",");
        else
            str.join("{0,");
        str.join(_to_string(p.max - 1u)).join("}");
    }
    if (tag == MatchShortest)
        str.join("?");
    return std::move(s2);
}

template<typename Pattern, typename State>
auto compile(_pattern_longest<Pattern> p, string& str, State s, tag_t tag)
{
    return compile(std::move(p.pattern), str, std::move(s), tag);
}

template<typename Pattern, typename State>
auto compile(_pattern_shortest<Pattern> p, string& str, State s, tag_t tag)
{
    return compile(std::move(p.pattern), str, std::move(s), MatchShortest);
}

template<typename Pattern, typename Condition, typename State>
auto compile(_condition<Pattern, Condition> p, string& str, State s, tag_t tag)
{
    auto s1 = std::move(s).append_condition(std::move(p.condition));
    if (tag > Concatenation)
        str.join("(?:");
    auto s2 = compile(std::move(p.pattern), str, std::move(s1), Concatenation);
    str.join("(?C");
    str.join(_to_string(decltype(s.conditions)::size));
    str.join(")");
    if (tag > Concatenation)
        str.join(")");
    return std::move(s2);
}

template<size_t I, typename... Patterns, typename State>
auto compile_impl(_string_expression<Patterns...>&& p, string& str, State s)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto s2 = compile(std::move(p).template get<I>(), str, std::move(s),
            Concatenation);
        return compile_impl<I + 1u>(std::move(p), str, std::move(s2));
    }
    else
    {
        return std::move(s);
    }
}

template<typename... Patterns, typename State>
auto compile(_string_expression<Patterns...> p, string& str, State s,
    tag_t tag)
{
    if (tag > Concatenation)
        str.join("(?:");
    auto s1 = compile_impl<0u>(std::move(p), str, std::move(s));
    if (tag > Concatenation)
        str.join(")");
    return std::move(s1);
}

inline void compile_impl(const utf8::char_t* begin, const utf8::char_t* end,
    string& str, tag_t tag)
{
    static const bool esc_table[256] ={
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,1,0,0,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

    if (begin == end)
    {
        if (tag > WithinGroup)
            str.join("(?:)");
    }
    else
    {
        if (tag > Concatenation && tag != CharacterSet)
            str.join("(?:");
        for (; begin < end; ++begin)
        {
            if (esc_table[*begin])
                str.append<false, false>('\\');
            str.append<false, false>(*begin);
        }
        str.place_null_character();
        if (tag > Concatenation && tag != CharacterSet)
            str.join(")");
    }
}

template<typename State>
auto compile(const ndarray<string, 1u>& patterns, string& str, State s,
    tag_t tag)
{
    auto use_bracket = compile_use_bracket(patterns);
    auto begin = patterns.begin();
    auto end = patterns.end();
    if (use_bracket)
        str.append('[');
    else if (tag >= Alternation)
        str.join("(?:");
    for (; begin != end; ++begin)
    {
        compile_impl(begin->byte_begin(), begin->byte_end(), str, Alternation);
        if (!use_bracket)
            str.append('|');
    }
    if (use_bracket)
        str.append(']');
    else if (tag >= Alternation)
        str.join(")");
    return std::move(s);
}

template<typename Any, typename State>
auto compile(_pattern_except<Any> p, string& str, State s, tag_t)
{
    if constexpr (is_string_view_v<Any>)
    {
        if (!is_single_character(p.pattern))
            throw std::logic_error(WL_ERROR_STRING_EXCEPT);
        str.join("[^");
        str.join(p.pattern);
        str.join("]");
    }
    else if constexpr (array_rank_v<Any> == 1u && is_string_type_v<Any>)
    {
        if (!compile_use_bracket(p.pattern))
            throw std::logic_error(WL_ERROR_STRING_EXCEPT);
        auto begin = p.pattern.begin();
        auto end = p.pattern.end();
        str.join("[^");
        for (; begin != end; ++begin)
            str.join(*begin);
        str.join("]");
    }
    else if constexpr (std::is_same_v<Any, _word_character_type>)
        str.join("[^[:alnum:]]");
    else if constexpr (std::is_same_v<Any, _letter_character_type>)
        str.join("[^[:alpha:]]");
    else if constexpr (std::is_same_v<Any, _digit_character_type>)
        str.join("\\D");
    else if constexpr (std::is_same_v<Any, _whitespace_character_type>)
        str.join("\\S");
    else if constexpr (std::is_same_v<Any, _word_boundary_type>)
        str.join("\\B");
    else if constexpr (std::is_same_v<Any, _hexadecimal_character_type>)
        str.join("[^[:xdigit:]]");
    else if constexpr (std::is_same_v<Any, _punctuation_character_type>)
        str.join("[^[:punct:]]");
    else if constexpr (std::is_same_v<Any, _start_of_line_type>)
        str.join("(?!^)");
    else if constexpr (std::is_same_v<Any, _end_of_line_type>)
        str.join("(?!$)");
    else if constexpr (std::is_same_v<Any, _start_of_string_type>)
        str.join("(?!\\A)");
    else if constexpr (std::is_same_v<Any, _end_of_string_type>)
        str.join("(?!\\z)");
    else
        static_assert(always_false_v<Any>, WL_ERROR_STRING_EXCEPT);
    return std::move(s);
}

template<typename... Patterns, typename State>
auto compile(_pattern_except<_pattern_alternatives<Patterns...>> p,
    string& str, State s, tag_t)
{
    if (!compile_use_bracket(p.pattern))
        throw std::logic_error(WL_ERROR_STRING_EXCEPT);
    str.join("[^");
    auto s2 = compile_alternatives(std::move(p.pattern), str, std::move(s),
        CharacterSet);
    str.join("]");
    return std::move(s2);
}

template<typename Any, typename State>
auto compile(Any&& any, string& str, State s, tag_t tag)
{
    using AT = remove_cvref_t<Any>;
    if constexpr (is_string_view_v<AT>)
    {
        compile_impl(any.byte_begin(), any.byte_end(), str, tag);
        return std::move(s);
    }
    else if constexpr (array_rank_v<AT> == 1u)
    {
        static_assert(is_string_v<value_type_t<AT>>, WL_ERROR_NOT_A_PATTERN);
        const auto& valany = allows<view_category::Array>(
            std::forward<decltype(any)>(any));
        return compile(valany, str, std::move(s), tag);
    }
    else
    {
        static_assert(is_pattern_v<AT>, WL_ERROR_NOT_A_PATTERN);
        if constexpr (std::is_same_v<AT, _whitespace_type>)
        {
            if (tag > Concatenation)
                str.join("(?:\\s+)");
            else
                str.join("\\s+");
        }
        else if constexpr (std::is_same_v<AT, _number_string_type>)
            str.join("(?:(?:\\+|-)?(?:\\d+(?:\\.\\d*)?|\\.\\d+))");
        else if constexpr (std::is_same_v<AT, _word_character_type>)
        {
            if (tag == CharacterSet)
                str.join("[:alnum:]");
            else
                str.join("[[:alnum:]]");
        }
        else if constexpr (std::is_same_v<AT, _letter_character_type>)
        {
            if (tag == CharacterSet)
                str.join("[:alpha:]");
            else
                str.join("[[:alpha:]]");
        }
        else if constexpr (std::is_same_v<AT, _digit_character_type>)
            str.join("\\d");
        else if constexpr (std::is_same_v<AT, _whitespace_character_type>)
            str.join("\\s");
        else if constexpr (std::is_same_v<AT, _hexadecimal_character_type>)
        {
            if (tag == CharacterSet)
                str.join("[:xdigit:]");
            else
                str.join("[[:xdigit:]]");
        }
        else if constexpr (std::is_same_v<AT, _punctuation_character_type>)
        {
            if (tag == CharacterSet)
                str.join("[:punct:]");
            else
                str.join("[[:punct:]]");
        }
        else if constexpr (std::is_same_v<AT, _word_boundary_type>)
            str.join("\\b");
        else if constexpr (std::is_same_v<AT, _start_of_line_type>)
            str.join("^");
        else if constexpr (std::is_same_v<AT, _end_of_line_type>)
            str.join("$");
        else if constexpr (std::is_same_v<AT, _start_of_string_type>)
            str.join("\\A");
        else if constexpr (std::is_same_v<AT, _end_of_string_type>)
            str.join("\\z");
        else
            static_assert(always_false_v<Any>, WL_ERROR_INTERNAL);
        return std::move(s);
    }
}

template<size_t I, typename... Patterns, typename State, typename FormatIdList>
auto format_compile_impl(_string_expression<Patterns...>&& p,
    string& str, const State& s, FormatIdList f)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto f1 = format_compile(std::move(p).template get<I>(), str, s,
            std::move(f));
        return format_compile_impl<I + 1u>(std::move(p), str, s,
            std::move(f1));
    }
    else
    {
        return std::move(f);
    }
}

template<typename... Patterns, typename State, typename FormatIdList>
auto format_compile(_string_expression<Patterns...>&& p, string& str,
    const State& s, FormatIdList f)
{
    return format_compile_impl<0u>(std::move(p), str, s, std::move(f));
}

template<int64_t Id, typename State, typename FormatIdList>
auto format_compile(_named_replacement<Id>, string& str, const State&,
    FormatIdList)
{
    constexpr auto group_idx = State::PatternIdList::find(const_int<Id>{});
    static_assert(0u < group_idx && group_idx <= 99u, WL_ERROR_INTERNAL);
    str.append('$');
    str.join(_to_string(group_idx, 2u));
    return FormatIdList::append(const_int<group_idx>{});
}

inline void format_compile_impl(const utf8::char_t* begin,
    const utf8::char_t* end, string& str)
{
    WL_THROW_IF_ABORT()
    for (; begin < end; ++begin)
    {
        if (*begin == utf8::char_t('$'))
            str.append('$');
        str.append(*begin);
    }
    str.place_null_character();
}

template<typename State, typename FormatIdList>
auto format_compile(const string& s, string& str, const State&,
    FormatIdList)
{
    format_compile_impl(s.byte_begin(), s.byte_end(), str);
    return FormatIdList{};
}

template<typename State, typename FormatIdList>
auto format_compile(const string_view& s, string& str, const State&,
    FormatIdList)
{
    format_compile_impl(s.byte_begin(), s.byte_end(), str);
    return FormatIdList{};
}

template<typename Any, typename State, typename FormatIdList>
auto _string_replacement_compile(
    Any&& any, string& str, const State&, FormatIdList f)
{
    using AT = remove_cvref_t<Any>;
    if constexpr (is_string_view_v<AT>)
    {
        format_compile_impl(any.byte_begin(), any.byte_end(), str);
        return std::move(f);
    }
    else
    {
        static_assert(always_false_v<Any>, WL_ERROR_NOT_A_REPLACEMENT);
        return std::move(f);
    }
}

template<typename Expression>
auto compile(Expression e)
{
    auto str = string("(?ms)");
    using InitialState = compilation_state<
        _pattern_condition_list<>, _pattern_id_list<>>;
    auto s1 = compile(std::move(e), str, InitialState{}, WithinGroup);
    using CP = compiled_pattern<
        decltype(s1.conditions), typename decltype(s1)::PatternIdList>;
    try
    {
        WL_THROW_IF_ABORT()
        return CP{pcre2::new_regex(str), std::move(s1.conditions),
            std::move(str)};
    }
    catch (const std::logic_error& regex_error)
    {
        throw std::logic_error(std::string(WL_ERROR_INTERNAL) +
            WL_ERROR_REGEX + regex_error.what());
    }
}

template<typename C, typename P>
auto compile(compiled_pattern<C, P>&& pattern)
{
    return std::move(pattern);
}

template<typename C, typename P>
const auto& compile(const compiled_pattern<C, P>& pattern)
{
    return pattern;
}

template<typename P, typename R>
auto compile(compiled_rule<P, R>&& rule)
{
    return std::move(rule);
}

template<typename P, typename R>
const auto& compile(const compiled_rule<P, R>& rule)
{
    return rule;
}

template<typename Left, typename Right>
auto compile(_pattern_rule<Left, Right> rule)
{
    auto cp = compile(std::move(rule.left));
    auto s = compilation_state<
        _pattern_condition_list<>, typename decltype(cp)::PatternIdList>{};
    auto format = string();
    auto f = format_compile(std::move(rule.right), format, s,
        _pattern_id_list<>{});
    auto replacement = compiled_format<decltype(f)>{format};
    return compiled_rule<decltype(cp), decltype(replacement)>{
        std::move(cp), std::move(replacement)};
}

template<typename State>
inline void format_text(const State& state, const string& format, string& ret)
{
    auto begin = format.byte_begin();
    auto end = format.byte_end();
    while (begin != end)
    {
        if (*begin != '$')
        {
            ret.template append<false>(*begin++);
        }
        else if (++begin == end)
        {
            ret.template append<false>('$');
        }
        else if (*begin == '$')
        {
            ret.template append<false>('$');
            ++begin;
        }
        else if (utf8::char_t('0') <= *begin && *begin <= utf8::char_t('9'))
        {
            auto group_idx = size_t(*begin++ - utf8::char_t('0'));
            const bool is_two_digit = begin != end &&
                (utf8::char_t('0') <= *begin && *begin <= utf8::char_t('9'));
            if (is_two_digit)
                group_idx = group_idx * 10u +
                size_t(*begin++ - utf8::char_t('0'));
            if (group_idx <= state.capture_count())
            {
                const auto& view = state.match(group_idx);
                ret.template append<false>(view.byte_data(), view.byte_size());
            }
        }
        else
        {
            ret.template append<false>('$');
            ret.template append<false>(*begin++);
        }
    }
    ret.place_null_character();
}

}

template<typename String>
auto regular_expression(const String& str)
{
    WL_TRY_BEGIN()
    using CP = strexp::compiled_pattern<
        _pattern_condition_list<>, _pattern_id_list<>>;
    try
    {
        WL_THROW_IF_ABORT()
        return CP{pcre2::new_regex(str)};
    }
    catch (const std::logic_error& regex_error)
    {
        throw std::logic_error(
            std::string(WL_ERROR_REGEX) + regex_error.what());
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename CharT>
boolean _string_equal(const CharT* a_begin, const CharT* a_end,
    const CharT* b_begin, const CharT* b_end)
{
    if ((a_end - a_begin) != (b_end - b_begin))
        return const_false;
    for (; a_begin < a_end; ++a_begin, ++b_begin)
    {
        if (*a_begin != *b_begin)
            return const_false;
    }
    return const_true;
}

inline boolean operator==(const string& a, const string& b)
{
    return _string_equal(a.byte_begin(), a.byte_end(),
        b.byte_begin(), b.byte_end());
}
inline boolean operator==(const string& a, const string_view& b)
{
    return _string_equal(a.byte_begin(), a.byte_end(),
        b.byte_begin(), b.byte_end());
}
inline boolean operator==(const string_view& a, const string& b)
{
    return _string_equal(a.byte_begin(), a.byte_end(),
        b.byte_begin(), b.byte_end());
}
inline boolean operator==(const string_view& a, const string_view& b)
{
    return _string_equal(a.byte_begin(), a.byte_end(),
        b.byte_begin(), b.byte_end());
}

template<typename C, typename P>
auto _pattern_convert_impl(const strexp::compiled_pattern<C, P>& pattern)
{
    return pattern.regex_text;
}

template<typename P, typename R>
auto _pattern_convert_impl(const strexp::compiled_rule<P, R>& rule)
{
    auto ret = _pattern_convert_impl(rule.pattern);
    ret.join(" -> ");
    ret.join(rule.format.text);
    return ret;
}

template<typename Any>
auto _pattern_convert(Any&& any)
{
    WL_TRY_BEGIN()
    return _pattern_convert_impl(strexp::compile(
        std::forward<decltype(any)>(any)));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

#define WL_DEFINE_STRING_PRED_FUNCTION_IMPL(name, anchors)                  \
template<bool Overlaps = false, typename String, typename C, typename P>    \
auto _string_##name##_q_impl(String&& str,                                  \
    const strexp::compiled_pattern<C, P>& pattern)                          \
{                                                                           \
    static_assert(is_string_view_v<remove_cvref_t<String>>,                 \
        WL_ERROR_STRING_FUNCTION_STRING);                                   \
    auto search = pcre2::regex_search(pattern, str.c_str(),                 \
        str.byte_size());                                                   \
    return boolean(search.find_next(false, anchors));                       \
}

WL_DEFINE_STRING_PRED_FUNCTION_IMPL(match, PCRE2_ANCHORED | PCRE2_ENDANCHORED)
WL_DEFINE_STRING_PRED_FUNCTION_IMPL(starts, PCRE2_ANCHORED)
WL_DEFINE_STRING_PRED_FUNCTION_IMPL(ends, PCRE2_ENDANCHORED)
WL_DEFINE_STRING_PRED_FUNCTION_IMPL(contains, 0)

template<bool Overlaps = false, typename String, typename C, typename P>
auto _string_free_q_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    return !_string_contains_q_impl(std::forward<decltype(str)>(str), pattern);
}

template<bool Overlaps, typename String, typename C, typename P>
auto _string_count_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    int64_t num_matches = 0;
    auto search = pcre2::regex_search(pattern, str.c_str(), str.byte_size());
    while (search.find_next(Overlaps))
    {
        ++num_matches;
    }
    return num_matches;
}

template<bool Overlaps, typename String, typename P, typename R>
auto _string_cases_impl(String&& str,
    const strexp::compiled_rule<P, R>& rule)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<string, 1u> ret;
    auto search = pcre2::regex_search(
        rule.pattern, str.c_str(), str.byte_size());
    while (search.find_next(Overlaps))
    {
        auto str = string();
        strexp::format_text(search, rule.format.text, str);
        ret.append(std::move(str));
    }
    return ret;
}

template<bool Overlaps, typename String, typename C, typename P>
auto _string_cases_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<string, 1u> ret;
    auto search = pcre2::regex_search(pattern, str.c_str(), str.byte_size());
    while (search.find_next(Overlaps))
    {
        ret.append(string(search.match()));
    }
    return ret;
}

template<bool Overlaps = false, typename String, typename P, typename R>
auto _string_replace_impl(String&& str,
    const strexp::compiled_rule<P, R>& rule)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    string ret;
    auto search = pcre2::regex_search(
        rule.pattern, str.c_str(), str.byte_size());
    while (search.find_next())
    {
        ret.join<false>(search.prefix());
        strexp::format_text(search, rule.format.text, ret);
    }
    ret.join<false>(
        string_view{search.prefix().byte_begin(), str.byte_end()});
    ret.place_null_character();
    return ret;
}

template<bool Overlaps = false, typename String, typename C, typename P>
auto _string_split_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<string, 1u> ret;
    auto search = pcre2::regex_search(pattern, str.c_str(), str.byte_size());
    while (search.find_next())
    {
        if (ret.size() > 0u || search.prefix().byte_size() > 0u)
            ret.append(string(search.prefix()));
    }
    const auto& prefix = string_view{
        search.prefix().byte_begin(), str.byte_end()};
    if (prefix.byte_size() > 0u)
        ret.append(string(prefix));
    return ret;
}

template<bool Overlaps = false, typename String, typename P, typename R>
auto _string_split_impl(String&& str,
    const strexp::compiled_rule<P, R>& rule)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<string, 1u> ret;
    auto search = pcre2::regex_search(
        rule.pattern, str.c_str(), str.byte_size());
    while (search.find_next())
    {
        if (ret.size() > 0u || search.prefix().byte_size() > 0u)
            ret.append(string(search.prefix()));
        auto replaced = string();
        strexp::format_text(search, rule.format.text, replaced);
        ret.append(std::move(replaced));
    }
    const auto& prefix = string_view{
        search.prefix().byte_begin(), str.byte_end()};
    if (prefix.byte_size() > 0u)
        ret.append(string(prefix));
    return ret;
}

template<bool Overlaps, typename String, typename C, typename P>
auto _string_position_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<int64_t, 1u> ret;
    size_t ret_size = 0u;
    auto search = pcre2::regex_search(pattern, str.c_str(), str.byte_size());
    while (search.find_next(Overlaps))
    {
        ret.uninitialized_resize(std::array<size_t, 1u>{ret_size + 2u});
        auto* insert_ptr = ret.data() + ret_size;
        insert_ptr[0] = int64_t(search.match_begin_idx() + 1u);
        insert_ptr[1] = int64_t(search.match_end_idx());
        ret_size += 2u;
    }
    return ndarray<int64_t, 2u>(std::array<size_t, 2u>{ret_size / 2u, 2u},
        std::move(ret).data_vector());
}

#define WL_DEFINE_STRING_FUNCTION(name, ret_type, takes_list, can_overlap,  \
    overlap_default)                                                        \
template<bool Overlaps = overlap_default, typename String, typename Any>    \
auto name(String&& str, Any&& any)                                          \
{                                                                           \
    WL_TRY_BEGIN()                                                          \
    static_assert(can_overlap || !Overlaps, WL_ERROR_INTERNAL);             \
    using ST = remove_cvref_t<String>;                                      \
    const auto& pattern = strexp::compile(std::forward<decltype(any)>(any));\
    if constexpr (is_string_view_v<ST>)                                     \
    {                                                                       \
        return _##name##_impl<Overlaps>(str, pattern);                      \
    }                                                                       \
    else if constexpr (takes_list)                                          \
    {                                                                       \
        static_assert(is_string_type_v<ST> && array_rank_v<ST> == 1u,       \
            WL_ERROR_STRING_TYPE_ONLY);                                     \
        if constexpr (is_movable_v<String> &&                               \
            std::is_same_v<ret_type, string>)                               \
        {                                                                   \
            str.for_each([&](string& s)                                     \
                { s =  _##name##_impl<Overlaps>(s, pattern); });            \
            return std::move(str);                                          \
        }                                                                   \
        else                                                                \
        {                                                                   \
            ndarray<ret_type, 1u> ret(std::array<size_t, 1u>{str.size()});  \
            str.for_each([&](const string& s, ret_type& c)                  \
                { c = _##name##_impl<Overlaps>(s, pattern); }, ret.data()); \
            return ret;                                                     \
        }                                                                   \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        static_assert(always_false_v<String>, WL_ERROR_STRING_ONLY);        \
    }                                                                       \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}

WL_DEFINE_STRING_FUNCTION(string_count, int64_t, true, true, false)
WL_DEFINE_STRING_FUNCTION(string_match_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_contains_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_free_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_starts_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_ends_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_replace, string, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_cases, int, false, true, false)
WL_DEFINE_STRING_FUNCTION(string_split, int, false, false, false)
WL_DEFINE_STRING_FUNCTION(string_position, int, false, true, true)

template<typename String>
auto string_split(String&& str)
{
    WL_TRY_BEGIN()
    static auto whitespace = strexp::compile(const_whitespace);
    return string_split(std::forward<decltype(str)>(str), whitespace);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

#define WL_DEFINE_CHAR_TYPE_FUNCTION(name, crit)                    \
template<typename String>                                           \
auto name(const String& str)                                        \
{                                                                   \
    using namespace utf8;                                           \
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);  \
    auto begin = str.byte_begin();                                  \
    auto end = str.byte_end();                                      \
    for (; begin != end; ++begin)                                   \
    {                                                               \
        const auto ch = *begin;                                     \
        if (!(crit))                                                \
            return const_false;                                     \
    }                                                               \
    return const_true;                                              \
}

WL_DEFINE_CHAR_TYPE_FUNCTION(digit_q, '0'_c <= ch && ch <= '9'_c)
WL_DEFINE_CHAR_TYPE_FUNCTION(letter_q,
    ('a'_c <= ch && ch <= 'z'_c) || ('A'_c <= ch && ch <= 'Z'_c))
WL_DEFINE_CHAR_TYPE_FUNCTION(lower_case_q, 'a'_c <= ch && ch <= 'z'_c)
WL_DEFINE_CHAR_TYPE_FUNCTION(upper_case_q, 'A'_c <= ch && ch <= 'Z'_c)
WL_DEFINE_CHAR_TYPE_FUNCTION(printable_ascii_q, ' '_c <= ch && ch <= '~'_c)

template<typename String>
auto characters(const String& s)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    const auto size = s.size();
    ndarray<string, 1u> ret(std::array<size_t, 1u>{size});
    auto ret_data = ret.data();
    if (size == s.byte_size())
    { // ascii only
        auto s_begin = s.byte_data();
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i,
                ++ret_data, ++s_begin)
            {
                *ret_data = string(s_begin, 1u, true);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    else
    { // not ascii only
        auto s_begin = s.begin();
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i, ++ret_data)
            {
                size_t num_bytes = s_begin.num_bytes();
                *ret_data = string(s_begin.get_pointer(), num_bytes,
                    num_bytes == 1u);
                s_begin.apply_pointer_offset(num_bytes);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto _character_range_impl(const X& x)
{
    static_assert(is_string_view_v<X> || is_integral_v<X>,
        WL_ERROR_CHARACTER_RANGE);
    if constexpr (is_string_view_v<X>)
    {
        if (!strexp::is_single_character(x))
            throw std::logic_error(WL_ERROR_CHARACTER_RANGE);
        return *x.begin();
    }
    else
    {
        if (uint64_t(x) > uint64_t(utf8::max_code_point))
            throw std::logic_error(WL_ERROR_INVALID_CODEPOINT);
        return utf8::char21_t(x);
    }
}

template<typename X, typename Y>
auto character_range(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    utf8::char21_t first_point = _character_range_impl(x);
    utf8::char21_t last_point = _character_range_impl(y);

    if (first_point > last_point)
        return ndarray<string, 1u>{};
    else
    {
        const auto size = size_t(last_point - first_point + 1u);
        ndarray<string, 1u> ret(std::array<size_t, 1u>{size});
        auto ret_data = ret.data();
        if (utf8::is_ascii(last_point))
        {
            WL_THROW_IF_ABORT()
            for (; first_point <= last_point; ++first_point, ++ret_data)
            {
                const auto ch = utf8::char_t(first_point);
                *ret_data = string(&ch, 1u, true);
            }
        }
        else
        {
            WL_THROW_IF_ABORT()
            for (; first_point <= utf8::max_ascii_code_point;
                ++first_point, ++ret_data)
            {
                const auto ch = utf8::char_t(first_point);
                *ret_data = string(&ch, 1u, true);
            }
            WL_CHECK_ABORT_LOOP_BEGIN(last_point - first_point + 1u)
                for (auto i = _loop_begin; i < _loop_end; ++i, ++ret_data)
                {
                    auto [num_bytes, units] = 
                        utf8::from_code_point(first_point + i);
                    *ret_data = string(units.data(), num_bytes, false);
                }
            WL_CHECK_ABORT_LOOP_END()
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Ret = int64_t, typename String>
auto to_character_code(const String& s)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    static_assert(is_integral_v<Ret> &&
        (std::numeric_limits<Ret>::max() >= 255), WL_ERROR_BAD_RETURN);
    const auto size = s.size();
    ndarray<Ret, 1u> ret(std::array<size_t, 1u>{size});
    auto ret_data = ret.data();
    if (size == s.byte_size())
    { // ascii only
        auto s_begin = s.byte_data();
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i,
                ++ret_data, ++s_begin)
            {
                *ret_data = int64_t(*s_begin);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    else
    { // not ascii only
        auto s_begin = s.begin();
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i,
                ++ret_data, ++s_begin)
            {
                *ret_data = int64_t(*s_begin);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto from_character_code(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> <= 1u, WL_ERROR_FROM_CHARACTER_CODE_ARG);
    if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_integral_v<X>, WL_ERROR_FROM_CHARACTER_CODE_ARG);
        const auto [num_bytes, units] =
            utf8::from_code_point(utf8::char21_t(x));
        return string(units.data(), num_bytes, num_bytes == 1u);
    }
    else
    {
        static_assert(is_integral_v<value_type_t<X>>,
            WL_ERROR_FROM_CHARACTER_CODE_ARG);
        const auto& valx = allows<view_category::Simple>(x);
        const auto size = valx.size();
        auto x_begin = valx.begin();
        bool ascii_only = true;
        auto ret = string();
        if (size > string::small_string_byte_size)
            ret.set_dynamic_capacity(size);
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i, ++x_begin)
            {
                auto [num_bytes, units] = utf8::from_code_point(*x_begin);
                if (ascii_only)
                    ascii_only = (num_bytes == 1u);
                ret.template append<false>(units.data(), num_bytes);
            }
        WL_CHECK_ABORT_LOOP_END()
        ret.place_null_character();
        ret.set_ascii_only(ascii_only);
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
int64_t _order_string(const X& x, const Y& y)
{
    static_assert(is_string_view_v<X> && is_string_view_v<Y>,
        WL_ERROR_OPERAND_TYPE);
    if (x.ascii_only() && y.ascii_only())
    {
        auto x_begin = x.byte_begin();
        auto x_end = x.byte_end();
        auto y_begin = y.byte_begin();
        auto y_end = y.byte_end();
        for (; true; ++x_begin, ++y_begin)
        {
            if (x_begin == x_end)
                return (y_begin == y_end) ? 0 : 1;
            else if (y_begin == y_end)
                return -1;
            else if (*x_begin < *y_begin)
                return 1;
            else if (*x_begin > *y_begin)
                return -1;
        }
    }
    else
    {
        auto x_begin = x.begin();
        auto x_end = x.end();
        auto y_begin = y.begin();
        auto y_end = y.end();
        for (; true; ++x_begin, ++y_begin)
        {
            if (x_begin == x_end)
                return (y_begin == y_end) ? 0 : 1;
            else if (y_begin == y_end)
                return -1;
            else if (*x_begin < *y_begin)
                return 1;
            else if (*x_begin > *y_begin)
                return -1;
        }
    }
}

int64_t _order_scalar(const string& x, const string& y)
{
    return _order_string(x, y);
}

int64_t _order_scalar(const string& x, const string_view& y)
{
    return _order_string(x, y);
}

int64_t _order_scalar(const string_view& x, const string& y)
{
    return _order_string(x, y);
}

int64_t _order_scalar(const string_view& x, const string_view& y)
{
    return _order_string(x, y);
}

struct _string_riffle_delimiter
{
    std::array<string, 3u> dels_{};
    bool triple_ = false;

    _string_riffle_delimiter() = default;

    _string_riffle_delimiter(const string& str) :
        dels_{string(), str, string()}, triple_{false}
    {
    }
    _string_riffle_delimiter(const string_view& str) :
        dels_{string(), string(str), string()}, triple_{false}
    {
    }

    template<typename X>
    _string_riffle_delimiter(const X& x) : triple_{true}
    {
        static_assert(array_rank_v<X> == 1u && is_string_v<value_type_t<X>>,
            WL_ERROR_STRING_RIFFLE_DELIMITER_TYPES);
        if (x.size() != 3u)
            throw std::logic_error(WL_ERROR_STRING_RIFFLE_DELIMITER_TYPES);
        x.copy_to(dels_.data());
    }

    bool triple() const
    {
        return triple_;
    }
    const string& left() const
    {
        return dels_[0];
    }
    const string& separator() const
    {
        return dels_[1];
    }
    const string& right() const
    {
        return dels_[2];
    }

    size_t extra_byte_size(size_t num_string) const
    {
        size_t size = triple_ ? left().byte_size() + right().byte_size() : 0u;
        return (num_string < 2u) ? size :
            size + (num_string - 1u) * separator().byte_size();
    }
};

template<typename X, size_t XR, size_t ND>
auto _string_riffle_get_byte_size(const X& x,
    const std::array<size_t, XR>& x_dims,
    const std::array<_string_riffle_delimiter, ND>& del_array)
{
    size_t string_size = 0u;
    x.for_each([&](const string& s) { string_size += s.byte_size(); });

    size_t delimiter_size = 0u;
    for (auto i = int64_t(XR) - 1; i >= 0; --i)
    {
        delimiter_size *= x_dims[i];
        if constexpr (ND == 0u)
            delimiter_size += (x_dims[i] > 2u) ? x_dims[i] - 1u : 0u;
        else
            delimiter_size += del_array[i].extra_byte_size(x_dims[i]);
    }
    return string_size + delimiter_size;
}

constexpr char newline_delimiter[] = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
static_assert(MaximumArrayRank == 16u, WL_ERROR_INTERNAL);

template<size_t I, size_t XR, size_t ND>
void _string_riffle_impl(
    const string*& x_ptr, const std::array<size_t, XR>& x_dims,
    const std::array<_string_riffle_delimiter, ND>& del_array, string& ret)
{
    if constexpr (ND != 0u)
        if (del_array[I].triple())
            ret.template join<false>(del_array[I].left());
    if constexpr (I + 1u == XR)
    {
        auto size = x_dims[XR - 1u];
        if (size == 0u)
            return;
        ret.join<false>(*x_ptr++);
        WL_CHECK_ABORT_LOOP_BEGIN(size - 1u)
            for (auto i = _loop_begin; i < _loop_end; ++i, ++x_ptr)
            {
                if constexpr (ND != 0u)
                    ret.template join<false>(del_array[XR - 1u].separator());
                else
                    ret.template join<false>(" ");
                ret.join<false>(*x_ptr);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    else
    {
        const auto size = x_dims[I];
        if (size == 0u)
            return;
        _string_riffle_impl<I + 1u>(x_ptr, x_dims, del_array, ret);
        for (size_t i = 1u; i < size; ++i)
        {
            if constexpr (ND != 0u)
                ret.template join<false>(del_array[I].separator());
            else
                ret.template append<false>(
                    (const utf8::char_t*)newline_delimiter, XR - I - 1u);
            _string_riffle_impl<I + 1u>(x_ptr, x_dims, del_array, ret);
        }
    }
    if constexpr (ND != 0u)
        if (del_array[I].triple())
            ret.template join<false>(del_array[I].right());
    if (I == 0u)
        ret.place_null_character();
}

template<typename X, typename... Dels>
auto string_riffle(const X& x, const Dels&... dels)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto num_dels = sizeof...(Dels);
    static_assert(num_dels == 0u || num_dels == XR,
        WL_ERROR_STRING_RIFFLE_DELIMITERS);
    static_assert(XR >= 1u && is_string_v<value_type_t<X>>,
        WL_ERROR_STRING_RIFFLE_STRINGS);

    const auto& valx = allows<view_category::Simple>(x);
    const auto x_dims = valx.dims();
    auto x_data = valx.data();
    std::array<_string_riffle_delimiter, num_dels> del_array{dels...};
    const auto ret_byte_size = _string_riffle_get_byte_size(
        valx, x_dims, del_array);
    auto ret = string();
    if (ret_byte_size > string::small_string_byte_size)
        ret.set_dynamic_capacity(ret_byte_size);
    _string_riffle_impl<0u>(x_data, x_dims, del_array, ret);
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
