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

#include "types.h"
#include "arrayview.h"
#include "utils.h"
#include "u8string.h"
#include "numerical.h"
#include "pattern.h"

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
        static_assert(is_string_v<Arg>, WL_ERROR_STRING_ONLY);
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
    auto [total_size, ascii_only] = _string_join_sizes_by_args(args...);
    auto ret = string(total_size, ascii_only);
    auto ret_data = ret.byte_data();
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
        auto valspec = cast<ndarray<ptrdiff_t, 2u>>(spec);
        const auto ret_size = valspec.dims()[0];
        const auto spec_size = valspec.dims()[1];
        const auto spec_data = valspec.data();
        ndarray<string, 1u> ret(std::array<size_t, 1u>{ret_size});
        auto ret_data = ret.data();
        if (spec_size == 1u)
        {
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(
                    _string_take_impl_unicode(str, spec_data[i]));
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
        auto valspec = cast<ndarray<ptrdiff_t, 2u>>(spec);
        const auto ret_size = valspec.dims()[0];
        const auto spec_size = valspec.dims()[1];
        const auto spec_data = valspec.data();
        ndarray<string, 1u> ret(std::array<size_t, 1u>{ret_size});
        auto ret_data = ret.data();
        if (spec_size == 1u)
        {
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(
                    _string_take_impl_ascii(str, spec_data[i]));
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
    using StringT = remove_cvref_t<String>;
    static_assert(is_string_view_v<StringT>, WL_ERROR_STRING_ONLY);

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

template<typename String>
auto regular_expression(const String& str)
{
    utf8::setup_global_locale();
    try
    {
        return utf8::regex(str.begin(), str.end());
    }
    catch (const std::regex_error& err)
    {
        throw std::logic_error(std::string(WL_ERROR_REGEX) + err.what());
    }
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
    static constexpr size_t size = sizeof...(Ids);

    template<size_t I>
    static constexpr int64_t get_v = 
        std::tuple_element_t<I - 1,
            std::tuple<std::integral_constant<int64_t, Ids>...>>::value;

    template<int64_t Id>
    static constexpr int64_t find_v =
        _pattern_id_list_find_impl<Id, Ids...>::value;

    static constexpr auto min_available_id = int64_t(1) << 32;
    static constexpr auto next_available_id =
        _pattern_id_list_max<min_available_id, Ids...>::value + 1;

    template<int64_t Id>
    auto append(const_int<Id>) const
    {
        return _pattern_id_list<Ids..., Id>{};
    }
};

template<typename... Conditions>
struct _pattern_condition_list
{
    std::tuple<Conditions...> conditions;

    template<typename Ids>
    struct matches
    {
        const utf8::smatch& match;

        template<int64_t Id>
        auto get_match() const
        {
            constexpr size_t idx = Ids::template find_v<Id>;
            return string_view{match[idx].first, match[idx].second};
        }
    };

    template<typename Condition>
    auto append(Condition condition) &&
    {
        return _pattern_condition_list<Conditions..., Condition>{
            std::make_tuple(
                std::get<Conditions>(std::move(conditions))...,
                std::move(condition))};
    }

    template<size_t I, typename Ids>
    void _test_conditions_impl(bool& pass, const utf8::smatch& smatch,
        const Ids& ids) const
    {
        if constexpr (I < sizeof...(Conditions))
        {
            const auto match = matches<Ids>{smatch};
            using RT = remove_cvref_t<
                decltype(std::get<I>(conditions)(match))>;
            static_assert(is_boolean_type_v<RT>, WL_ERROR_INTERNAL);
            pass = pass && std::get<I>(conditions)(match);
            if (pass)
                _test_conditions_impl<I + 1u>(pass, smatch, ids);
        }
    }

    template<typename Captures, typename Ids>
    bool test_conditions(const Captures& captures, const Ids& ids)
    {
        bool pass = true;
        _test_conditions_impl<0>(pass, captures, ids);
        return pass;
    }
};

template<typename ConditionList, typename IdList>
struct _string_expression_compilation_state
{
    ConditionList conditions;
    IdList ids;

    template<int64_t Id>
    auto append_id(const_int<Id>) &&
    {
        auto new_ids = ids.append(const_int<Id>{});
        return _string_expression_compilation_state<
            ConditionList, decltype(new_ids)>{
            std::move(conditions), new_ids};
    }

    template<typename Condition>
    auto append_condition(Condition condition) &&
    {
        auto new_conditions =
            std::move(conditions).append(std::move(condition));
        return _string_expression_compilation_state<
            decltype(new_conditions), IdList>{
            std::move(new_conditions), ids};
    }
};

template<typename ConditionList, typename IdList>
struct _compiled_pattern
{
    using PatternIdList = IdList;

    const utf8::regex regex;
    const ConditionList conditions;

    inline bool test_match(const utf8::smatch& smatch) const
    {
        bool pass = true;
        conditions.template _test_conditions_impl<0>(pass, smatch, IdList{});
        return pass;
    }
};

template<typename CompiledPattern>
struct _compiled_pattern_rule
{
    const CompiledPattern pattern;
    const string format;
};

template<typename T>
struct _is_compiled_pattern : std::false_type {};

template<typename CL, typename IL>
struct _is_compiled_pattern<_compiled_pattern<CL, IL>> : std::true_type {};

template<typename CP>
struct _is_compiled_pattern<_compiled_pattern_rule<CP>> : std::true_type {};

template<typename T>
constexpr auto _is_compiled_pattern_v = _is_compiled_pattern<T>::value;


struct _string_expression_match_longest_tag {};
struct _string_expression_match_shortest_tag {};

template<int64_t Id, typename Pattern, typename State, typename... Tags>
auto _string_expression_compile(
    _named_pattern<Id, Pattern> p, string& str, State s, Tags...)
{
    constexpr auto group_idx = decltype(s.ids)::template find_v<Id>;
    if constexpr (group_idx < 0)
    { // group does not exist; capture a new group
        str.join(u8"(");
        auto s1 = std::move(s).append_id(const_int<Id>{});
        auto s2 = _string_expression_compile(
            std::move(p.pattern), str, std::move(s1));
        str.join(u8")");
        return std::move(s2);
    }
    else
    { // group already exists; refer to that group
        str.join(u8"(?:\\").join(_to_string(group_idx)).join(u8")");
        return std::move(s);
    }
}

template<typename... Head, typename State, typename... Tags>
auto _string_expression_compile(
    _pattern_blank<Head...>, string& str, State s, Tags...)
{
    str.join(u8".");
    return std::move(s);
}

template<typename... Head, typename State>
auto _string_expression_compile(
    _pattern_blank_sequence<Head...>, string& str, State s,
    _string_expression_match_shortest_tag)
{
    str.join(u8".+?");
    return std::move(s);
}

template<typename... Head, typename State, typename... Tags>
auto _string_expression_compile(
    _pattern_blank_sequence<Head...>, string& str, State s, Tags...)
{
    str.join(u8".+");
    return std::move(s);
}

template<typename... Head, typename State>
auto _string_expression_compile(
    _pattern_blank_null_sequence<Head...>, string& str, State s,
    _string_expression_match_shortest_tag)
{
    str.join(u8".*?");
    return std::move(s);
}

template<typename... Head, typename State, typename... Tags>
auto _string_expression_compile(
    _pattern_blank_null_sequence<Head...>, string& str, State s, Tags...)
{
    str.join(u8".*");
    return std::move(s);
}

template<size_t I, typename... Patterns, typename State, typename... Tags>
auto _string_expression_compile_impl(
    _pattern_alternatives<Patterns...> p, string& str, State s, Tags...)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto s1 = _string_expression_compile(
            std::move(p).template get<I>(), str, std::move(s));
        if constexpr (I + 1u < sizeof...(Patterns))
            str.join(u8"|");
        auto s2 = _string_expression_compile_impl<I + 1u>(
            std::move(p), str, std::move(s1));
        return std::move(s2);
    }
    else
    {
        return std::move(s);
    }
}

template<typename... Patterns, typename State, typename... Tags>
auto _string_expression_compile(
    _pattern_alternatives<Patterns...> p, string& str, State s, Tags...)
{
    str.join(u8"(?:");
    auto s1 = _string_expression_compile_impl<0>(p, str, std::move(s));
    str.join(u8")");
    return std::move(s1);
}

template<typename Pattern, typename State, typename... Tags>
auto _string_expression_compile(
    _pattern_repeated<Pattern> p, string& str, State s, Tags...)
{
    if (p.min == 0u)
        str.join(u8"(?:");
    auto s1 = _string_expression_compile(p.pattern, str, std::move(s));
    if (p.min == 0u)
        str.join(u8")?");
    str.join(u8"(?:");
    auto s2 = _string_expression_compile(p.pattern, str, std::move(s1));
    if (p.max_is_infinity())
        str.join(u8")*");
    else
    {
        if (p.min >= 2u)
            str.join(u8"){").join(_to_string(p.min - 1u)).join(u8",");
        else
            str.join(u8"){0,");
        str.join(_to_string(p.max - 1u)).join(u8"}");
    }
    return std::move(s2);
}


template<typename Pattern, typename State>
auto _string_expression_compile(
    _pattern_repeated<Pattern> p, string& str, State s,
    _string_expression_match_shortest_tag)
{
    if (p.min == 0u)
        str.join(u8"(?:");
    auto s1 = _string_expression_compile(p.pattern, str, std::move(s));
    if (p.min == 0u)
        str.join(u8")??");
    str.join(u8"(?:");
    auto s2 = _string_expression_compile(p.pattern, str, std::move(s1));
    if (p.max_is_infinity())
        str.join(u8")*");
    else
    {
        if (p.min >= 2u)
            str.join(u8"){").join(p.min - 1u).join(u8",");
        else
            str.join(u8"){0,");
        str.join(p.max - 1u).join(u8"}?");
    }
    return std::move(s2);
}

template<typename Pattern, typename State, typename... Tags>
auto _string_expression_compile(
    _pattern_except<Pattern>, string& str, State s, Tags...)
{
    assert(false);
    return std::move(s);
}

template<typename Pattern, typename State, typename... Tags>
auto _string_expression_compile(
    _pattern_longest<Pattern> p, string& str, State s, Tags...)
{
    auto s1 = _string_expression_compile(
        std::move(p.pattern), str, std::move(s),
        _string_expression_match_longest_tag{});
    return std::move(s);
}

template<typename Pattern, typename State, typename... Tags>
auto _string_expression_compile(
    _pattern_shortest<Pattern> p, string& str, State s, Tags...)
{
    auto s1 = _string_expression_compile(
        std::move(p.pattern), str, std::move(s),
        _string_expression_match_shortest_tag{});
    return std::move(s);
}

template<typename Pattern, typename Condition, typename State, typename... Tags>
auto _string_expression_compile(
    _condition<Pattern, Condition> p, string& str, State s, Tags...)
{
    auto s1 = std::move(s).append_condition(std::move(p.condition));
    auto s2 = _string_expression_compile(
        std::move(p.pattern), str, std::move(s1));
    return std::move(s2);
}

template<size_t I, typename... Patterns, typename State, typename... Tags>
auto _string_expression_compile_impl(
    _string_expression<Patterns...>&& p, string& str, State s, Tags...)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto s2 = _string_expression_compile(
            std::move(p).template get<I>(), str, std::move(s));
        return _string_expression_compile_impl<I + 1u>(
            std::move(p), str, std::move(s2));
    }
    else
    {
        return std::move(s);
    }
}

template<typename... Patterns, typename State, typename... Tags>
auto _string_expression_compile(
    _string_expression<Patterns...> p, string& str, State s, Tags...)
{
    return _string_expression_compile_impl<0u>(
        std::move(p), str, std::move(s));
}

inline void _string_expression_compile_impl(
    const utf8::char_t* begin, const utf8::char_t* end, string& str)
{
    static const bool esc_table[256] ={
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,1,0,0,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    
    for (; begin < end; ++begin)
    {
        if (esc_table[*begin])
            str.append('\\');
        str.append(*begin);
    }
    str.place_null_character();
}

template<typename Any, typename State, typename... Tags>
auto _string_expression_compile(Any&& any, string& str, State s, Tags...)
{
    if constexpr (is_string_view_v<remove_cvref_t<Any>>)
    {
        _string_expression_compile_impl(any.byte_begin(), any.byte_end(), str);
        return std::move(s);
    }
    else
    {
        static_assert(always_false_v<Any>, WL_ERROR_INTERNAL);
        return std::move(s);
    }
}

template<size_t I, typename... Patterns, typename State>
auto _string_replacement_compile_impl(
    _string_expression<Patterns...>&& p, string& str, State s)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto s2 = _string_replacement_compile(
            std::move(p).template get<I>(), str, std::move(s));
        return _string_replacement_compile_impl<I + 1u>(
            std::move(p), str, std::move(s2));
    }
    else
    {
        return std::move(s);
    }
}

template<typename... Patterns, typename State>
auto _string_replacement_compile(
    _string_expression<Patterns...>&& p, string& str, State s)
{
    return _string_expression_compile_impl<0u>(
        std::move(p), str, std::move(s));
}

template<int64_t Id, typename State>
auto _string_replacement_compile(const_int<Id>, string& str, State s)
{
    constexpr auto group_idx = decltype(s.ids)::template find_v<Id>;
    static_assert(0u < group_idx && group_idx <= 99u, WL_ERROR_INTERNAL);
    str.append('$');
    str.join(_to_string(group_idx, 2u));
    return std::move(s);
}

inline void _string_replacement_compile_impl(
    const utf8::char_t* begin, const utf8::char_t* end, string& str)
{
    for (; begin < end; ++begin)
    {
        if (*begin == utf8::char_t('$'))
            str.append('$');
        str.append(*begin);
    }
    str.place_null_character();
}

template<typename State>
auto _string_replacement_compile(const string& p, string& str, State s)
{
    _string_replacement_compile_impl(p.byte_begin(), p.byte_end(), str);
    return std::move(s);
}

template<typename State, typename... Tags>
auto _string_replacement_compile(string&& p, string& str, State s)
{
    _string_replacement_compile_impl(p.byte_begin(), p.byte_end(), str);
    return std::move(s);
}

template<typename Any, typename State>
auto _string_replacement_compile(Any&& any, string& str, State s)
{
    if constexpr (is_string_view_v<remove_cvref_t<Any>>)
    {
        _string_replacement_compile_impl(
            any.byte_begin(), any.byte_end(), str);
        return std::move(s);
    }
    else
    {
        static_assert(always_false_v<Any>, WL_ERROR_INTERNAL);
        return std::move(s);
    }
}

template<typename Expression>
auto _string_expression_compile(Expression e)
{
    auto str = string();
    auto s1 = _string_expression_compile(e, str, 
        _string_expression_compilation_state<
            _pattern_condition_list<>,
            _pattern_id_list<>
        >{});
    //std::cout << "regex = " << str._ascii_string() << std::endl;
    auto regex = utf8::regex(str.begin(), str.end());
    return _compiled_pattern<decltype(s1.conditions), decltype(s1.ids)>{
        std::move(regex), std::move(s1.conditions)};
}

auto _string_expression_compile(utf8::regex regex)
{
    return _compiled_pattern<_pattern_condition_list<>, _pattern_id_list<>>{
        std::move(regex), _pattern_condition_list<>{}};
}

template<typename Left, typename Right>
auto _string_expression_compile(_pattern_rule<Left, Right> rule)
{
    auto cp = _string_expression_compile(std::move(rule.left));
    auto s = _string_expression_compilation_state<
        _pattern_condition_list<>, typename decltype(cp)::PatternIdList>{};
    auto format = string();
    [[maybe_unused]] auto s1 = _string_replacement_compile(
        std::move(rule.right), format, std::move(s));
    //std::cout << "format = " << format._ascii_string() << std::endl;
    return _compiled_pattern_rule<decltype(cp)>{
        std::move(cp), std::move(format)};
}

inline void _string_expression_format(
    utf8::smatch match, const string& format, string& ret)
{
    auto begin = format.byte_begin();
    auto end = format.byte_end();
    while (begin != end)
    {
        if (*begin != '$')
        {
            ret.append<false>(*begin++);
        }
        else if (++begin == end)
        {
            ret.append<false>('$');
        }
        else if (*begin == '$')
        {
            ret.append<false>('$');
            ++begin;
        }
        else if (*begin == '`')
        {
            auto prefix_begin = match.prefix().first;
            auto prefix_end = match.prefix().second;
            ret.append<false>(prefix_begin.get_pointer(),
                size_t(prefix_end.byte_difference(prefix_begin)));
            ++begin;
        }
        else if (*begin == '\'')
        {
            auto suffix_begin = match.suffix().first;
            auto suffix_end = match.suffix().second;
            ret.append<false>(suffix_begin.get_pointer(),
                size_t(suffix_end.byte_difference(suffix_begin)));
            ++begin;
        }
        else if (*begin == '&')
        {
            if (match.size() > 0u)
            {
                auto match_begin = match[0].first;
                auto match_end = match[0].second;
                ret.append<false>(match_begin.get_pointer(),
                    size_t(match_end.byte_difference(match_begin)));
            }
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
            if (group_idx == 0)
            {
                ret.append<false>('$');
                ret.append<false>('0');
                if (is_two_digit)
                    ret.append<false>('0');
            }
            else if (group_idx < match.size())
            {
                auto match_begin = match[group_idx].first;
                auto match_end = match[group_idx].second;
                ret.append<false>(match_begin.get_pointer(),
                    size_t(match_end.byte_difference(match_begin)));
            }
        }
        else
        {
            ret.append<false>('$');
            ret.append<false>(*begin++);
        }
    }
    ret.place_null_character();
}

template<typename String, typename CL, typename IL>
auto _string_count_impl(String&& str, const _compiled_pattern<CL, IL>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    auto begin = str.begin();
    auto end = str.end();
    utf8::smatch match;
    size_t n = 0;
    while (std::regex_search(begin, end, match, pattern.regex))
    {
        if (pattern.test_match(match))
        {
            ++n;
            begin = match.suffix().first;
        }
        else
        {
            begin = match[0].first;
            if (begin == end || ++begin == end)
                break;
        }
    }
    return n;
}

template<typename String, typename CL, typename IL>
auto _string_match_q_impl(String&& str,
    const _compiled_pattern<CL, IL>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    auto begin = str.begin();
    auto end = str.end();
    utf8::smatch match;
    size_t n = 0;

    if (!std::regex_match(begin, end, match, pattern.regex))
        return const_false;
    return boolean(pattern.test_match(match));
}

template<typename String, typename CP>
auto _string_cases_impl(String&& str, const _compiled_pattern_rule<CP>& rule)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    auto begin = str.begin();
    auto end = str.end();
    utf8::smatch match;
    ndarray<string, 1u> ret;
    while (std::regex_search(begin, end, match, rule.pattern.regex))
    {
        if (rule.pattern.test_match(match))
        {
            auto str = string();
            _string_expression_format(match, rule.format, str);
            ret.append(std::move(str));
            begin = match.suffix().first;
        }
        else
        {
            begin = match[0].first;
            if (begin == end || ++begin == end)
                break;
        }
    }
    return ret;
}

template<typename String, typename CL, typename IL>
auto _string_cases_impl(String&& str, const _compiled_pattern<CL, IL>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    auto begin = str.begin();
    auto end = str.end();
    utf8::smatch match;
    ndarray<string, 1u> ret;
    while (std::regex_search(begin, end, match, pattern.regex))
    {
        if (pattern.test_match(match))
        {
            ret.append(string(match[0].first, match[0].second));
            begin = match.suffix().first;
        }
        else
        {
            begin = match[0].first;
            if (begin == end || ++begin == end)
                break;
        }
    }
    return ret;
}

template<typename String, typename CP>
auto _string_replace_impl(String&& str, const _compiled_pattern_rule<CP>& rule)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    auto begin = str.begin();
    auto end = str.end();
    utf8::smatch match;
    string ret;
    while (std::regex_search(begin, end, match, rule.pattern.regex))
    {
        auto prefix_begin = match.prefix().first;
        auto prefix_end = match.prefix().second;
        ret.append<false>(prefix_begin.get_pointer(),
            prefix_end.byte_difference(prefix_begin));
        if (rule.pattern.test_match(match))
        {
            _string_expression_format(match, rule.format, ret);
            begin = match.suffix().first;
        }
        else
        {
            begin = match[0].first;
            if (begin == end || ++begin == end)
                break;
            else
                ret.append<false>(match[0].first.get_pointer(),
                    match[0].first.num_bytes());
        }
    }
    ret.append<false>(begin.get_pointer(), size_t(end.byte_difference(begin)));
    ret.place_null_character();
    return ret;
}

template<typename String, typename Any>
auto _string_count_impl(String&& str, const Any& any)
{
    static_assert(always_false_v<String>, WL_ERROR_STRING_COUNT_PATTERN);
    return 0;
}

template<typename String, typename Any>
auto _string_cases_impl(String&& str, const Any& any)
{
    static_assert(always_false_v<String>, WL_ERROR_STRING_CASES_PATTERN);
    return 0;
}

template<typename String, typename Any>
auto _string_replace_impl(String&& str, const Any& any)
{
    static_assert(always_false_v<String>, WL_ERROR_STRING_REPLACE_PATTERN);
    return 0;
}

template<typename String, typename Any>
auto _string_match_q_impl(String&& str, const Any& any)
{
    static_assert(always_false_v<String>, WL_ERROR_STRING_MATCH_Q_PATTERN);
    return 0;
}

template<typename String, typename Any>
auto string_count(String&& str, Any&& any)
{
    if constexpr (!_is_compiled_pattern_v<remove_cvref_t<Any>>)
        return _string_count_impl(std::forward<decltype(str)>(str),
            _string_expression_compile(std::forward<decltype(any)>(any)));
    else
        return _string_count_impl(std::forward<decltype(str)>(str),
            std::forward<decltype(any)>(any));
}

template<typename String, typename Any>
auto string_cases(String&& str, Any&& any)
{
    if constexpr (!_is_compiled_pattern_v<remove_cvref_t<Any>>)
        return _string_cases_impl(std::forward<decltype(str)>(str),
            _string_expression_compile(std::forward<decltype(any)>(any)));
    else
        return _string_cases_impl(std::forward<decltype(str)>(str),
            std::forward<decltype(any)>(any));
}

template<typename String, typename Any>
auto string_replace(String&& str, Any&& any)
{
    if constexpr (!_is_compiled_pattern_v<remove_cvref_t<Any>>)
        return _string_replace_impl(std::forward<decltype(str)>(str),
            _string_expression_compile(std::forward<decltype(any)>(any)));
    else
        return _string_replace_impl(std::forward<decltype(str)>(str),
            std::forward<decltype(any)>(any));
}

template<typename String, typename Any>
auto string_match_q(String&& str, Any&& any)
{
    if constexpr (!_is_compiled_pattern_v<remove_cvref_t<Any>>)
        return _string_match_q_impl(std::forward<decltype(str)>(str),
            _string_expression_compile(std::forward<decltype(any)>(any)));
    else
        return _string_match_q_impl(std::forward<decltype(str)>(str),
            std::forward<decltype(any)>(any));
}

}
