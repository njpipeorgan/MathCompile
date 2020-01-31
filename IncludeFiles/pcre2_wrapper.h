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
#include <exception>
#include <iostream>
#include <memory>
#include <type_traits>

#define PCRE2_STATIC 1
#define PCRE2_CODE_UNIT_WIDTH 8

#include "pcre2.h"
#include "types.h"
#include "u8string.h"

namespace pcre2
{

using regex_t = std::shared_ptr<const pcre2_code_8>;

using match_data_dtor_t = std::integral_constant<
    decltype(&pcre2_match_data_free_8), &pcre2_match_data_free_8>;
using match_data_t = std::unique_ptr<
    pcre2_match_data_8, match_data_dtor_t>;

using match_context_dtor_t = std::integral_constant<
    decltype(&pcre2_match_context_free_8), &pcre2_match_context_free_8>;
using match_context_t = std::unique_ptr<
    pcre2_match_context_8, match_context_dtor_t>;

//struct _stack_buffer
//{
//    void* address = nullptr;
//    PCRE2_SIZE byte_size = 0u;
//};
//
//inline void* _static_malloc(PCRE2_SIZE size, void* buffer_ptr)
//{
//    const auto& buffer = *static_cast<_stack_buffer*>(buffer_ptr);
//    if (size > buffer.byte_size)
//        return nullptr;
//    else
//        return buffer.address;
//}
//
//inline void _static_free(void*, void*)
//{
//}
//
//struct match_data_t
//{
//    static constexpr size_t context_byte_size = 32u;
//    static constexpr size_t match_byte_size = 128u;
//    char context_buffer[context_byte_size / sizeof(char)];
//    char match_buffer[match_byte_size / sizeof(char)];
//
//    match_data_t()
//    {
//        auto buffer = _stack_buffer{(void*)context_buffer, context_byte_size};
//        auto context_ptr = pcre2_general_context_create_8(
//            &_static_malloc, &_static_free, (void*)(&buffer));
//        buffer = _stack_buffer{(void*)match_buffer, match_byte_size};
//        pcre2_match_data_create_8(1, (pcre2_general_context_8*)context_buffer);
//    }
//
//    ~match_data_t()
//    {
//    }
//
//    match_data_t(const match_data_t&) = delete;
//    match_data_t(match_data_t&&) = delete;
//
//    match_data_t& operator=(const match_data_t&) = delete;
//    match_data_t& operator=(match_data_t&&) = delete;
//
//};

template<typename String>
regex_t new_regex(const String& pattern)
{
    static_assert(wl::is_string_view_v<String>, "");
    int error = 0;
    PCRE2_SIZE pos = 0;
    pcre2_code_8* regex_ptr = pcre2_compile_8((PCRE2_SPTR8)pattern.c_str(),
        pattern.byte_size(), 0, &error, &pos, nullptr);
    if (!regex_ptr)
    {
        constexpr PCRE2_SIZE buffer_size = 256;
        PCRE2_UCHAR buffer[buffer_size];
        pcre2_get_error_message_8(error, buffer, buffer_size);
        throw std::logic_error((const char*)buffer);
    }
    return {regex_ptr, &pcre2_code_free_8};
}

template<typename C, typename P>
struct _regex_search_state
{
    const wl::strexp::compiled_pattern<C, P>& pattern_;
    uint32_t capture_count_ = 0;
    const PCRE2_SPTR8 text_data_;
    const size_t text_size_;
    size_t start_pos_ = 0;
    match_data_t match_{};
    PCRE2_SIZE* match_ptr_ = nullptr;

    _regex_search_state(const wl::strexp::compiled_pattern<C, P>& pattern,
        PCRE2_SPTR8 text_data, size_t text_size) :
        pattern_{pattern}, text_data_{text_data}, text_size_{text_size}
    {
        *pattern_.conditions_ptr = &pattern_.conditions;
        pcre2_pattern_info_8(pattern_.regex_ptr.get(),
            PCRE2_INFO_CAPTURECOUNT, &capture_count_);
        match_.reset(pcre2_match_data_create_8(capture_count_ + 1u, nullptr));
        match_ptr_ = pcre2_get_ovector_pointer_8(match_.get());
        match_ptr_[0] = PCRE2_SIZE(-1);
        match_ptr_[1] = PCRE2_SIZE(0);
    }

    wl::string_view prefix() const
    {
        return {text_data_ + start_pos_, text_data_ + match_ptr_[0u]};
    }

    auto capture_count() const
    {
        return size_t(capture_count_);
    }

    size_t match_begin_idx() const
    {
        return size_t(match_ptr_[0u]);
    }

    size_t match_end_idx() const
    {
        return size_t(match_ptr_[1u]);
    }

    wl::string_view match() const
    {
        return {text_data_ + match_ptr_[0u], text_data_ + match_ptr_[1u]};
    }

    wl::string_view match(size_t i) const
    {
        assert(i <= capture_count_);
        return {text_data_ + match_ptr_[2u * i],
            text_data_ + match_ptr_[2u * i + 1u]};
    }

    bool find_next(bool overlap = false, uint32_t options = 0u)
    {
        WL_THROW_IF_ABORT()
        start_pos_ = overlap ? match_ptr_[0] + PCRE2_SIZE(1) : match_ptr_[1];
        auto result = pcre2_match_8(pattern_.regex_ptr.get(),
            text_data_, text_size_, start_pos_, options, match_.get(),
            pattern_.match_context_ptr.get());
        return result >= 0;
    }
};

template<typename C, typename P, typename CharT>
auto regex_search(const wl::strexp::compiled_pattern<C, P>& pattern,
    const CharT* text_data, size_t text_size)
{
    return _regex_search_state<C, P>(pattern, (PCRE2_SPTR8)text_data,
        text_size);
}

template<typename PatternIdList>
struct callout_matches_t
{
    PCRE2_SPTR text_data;
    const PCRE2_SIZE* offset_vector;
    const size_t capture_top;

    template<int64_t I>
    wl::string_view operator[](wl::const_int<I> id) const
    {
        if constexpr (I > 0)
        { // Condition
            constexpr auto group_idx = PatternIdList::find(id);
            assert(group_idx < capture_top);
            return {text_data + offset_vector[2u * group_idx],
                text_data + offset_vector[2u * group_idx + 1u]};
        }
        else
        { // PatternTest
            return {text_data + offset_vector[2u * capture_top - 2u],
                text_data + offset_vector[2u * capture_top - 1u]};
        }
    }
};

template<typename PL, typename CL, size_t... Is>
auto callout_evaluate(size_t number, const callout_matches_t<PL>& matches,
    const CL& conditions, std::index_sequence<Is...>)
{
    assert(number < CL::size);
    return ((number != Is || conditions.template get<Is>()(matches)) && ...);
}

template<typename PatternIdList, typename ConditionList>
int callout_function(pcre2_callout_block_8* block_ptr,
    void* condition_list_ptr)
{
    WL_THROW_IF_ABORT()
    auto matches = callout_matches_t<PatternIdList>{block_ptr->subject,
        block_ptr->offset_vector, block_ptr->capture_top};
    const ConditionList& condition_list =
        **(const ConditionList**)condition_list_ptr;
    bool pass = callout_evaluate(
        block_ptr->callout_number, matches, condition_list,
        std::make_index_sequence<ConditionList::size>{});
    return pass ? 0 : 1;
}

}
