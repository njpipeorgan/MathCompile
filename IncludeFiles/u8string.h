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

namespace wl
{

namespace utf8
{

struct size_properties
{
    size_t byte_size;
    size_t string_size;
};

template<bool CheckValid = false, size_t N>
size_properties _get_sizes_impl(const char* in_str)
{
    size_t byte_size = 0;
    size_t trailing_size = 0;
    auto str_begin = reinterpret_cast<const int8_t*>(in_str);
    auto str = str_begin;

#if defined(__AVX2__) || defined(__SSE4_1__)
    using namespace wl::simd;
#  if defined(__AVX2__)
    using M = __m256i;
#  else
    using M = __m128i;
#endif
    const auto upper = set1<M>(int8_t(0b1100'0000));
    auto trailing = zero<M>();
    for (size_t i = 0;; ++i, str += 32, byte_size += 32)
    {
        auto data = loadu<M>(str);
        if constexpr (N > 0u)
        {
            if (int64_t(byte_size) >= int64_t(N) - 32)
            {
                auto tmask = movemask_epi8(simd::cmpgt_epi8(upper, data));
                auto excess_byte = N - byte_size;
                auto excess_trailing = utils::_popcnt(
                    uint64_t(tmask) << (64u - excess_byte));
                byte_size = N - 1u;
                trailing_size += hsum_epi8(trailing);
                trailing_size += excess_trailing;
                return {byte_size, byte_size - trailing_size};
            }
        }
        else
        {
            auto compare = cmpeq_epi8(data, zero<M>());
            if (!testc(zero<M>(), compare))
            {
                auto mask = movemask_epi8(compare);
                auto tmask = movemask_epi8(simd::cmpgt_epi8(upper, data));
                auto excess_byte = utils::tzcnt_u64(mask);
                auto excess_trailing = utils::_popcnt(
                    uint64_t(tmask) << (64u - excess_byte));
                byte_size += excess_byte;
                trailing_size += hsum_epi8(trailing);
                trailing_size += excess_trailing;
                return {byte_size, byte_size - trailing_size};
            }
        }
        trailing = sub_epi8(trailing, cmpgt_epi8(upper, data));
        if (i >= 100u)
        {
            trailing_size += hsum_epi8(trailing);
            trailing = zero<M>();
            i = 0u;
        }
    }
    return {0, 0};
#else
    if constexpr (N > 0u)
    {
        for (size_t i = 0; i < N - 1u; ++i)
            if (str[i] < int8_t(0b1100'0000))
                ++trailing_size;
        byte_size = N - 1u;
    }
    else
    {
        for (; *str; ++str)
            if (*str < int8_t(0b1100'0000))
                ++trailing_size;
        byte_size = size_t(str - str_begin);
    }
    return {byte_size, byte_size - trailing_size};
#endif
}

template<bool CheckValid = false, typename Str>
size_properties get_sizes(const Str& str)
{
    if constexpr (std::is_array_v<Str>)
    {
        constexpr auto N = std::extent_v<Str>;
        static_assert(N >= 1u, WL_ERROR_INTERNAL);
        return _get_sizes_impl<CheckValid, N>(static_cast<const char*>(str));
    }
    else
    {
        return _get_sizes_impl<CheckValid, 0>(static_cast<const char*>(str));
    }
}

struct iterator
{
    const uint8_t* ptr_;

    static bool _is_valid_codepoint_begin(uint8_t byte)
    {
        return (byte < 0b1000'0000u) || (byte >= 0b1100'0000u);
    }

    static bool _is_valid_codepoint_tailing(uint8_t byte)
    {
        return (byte & 0b1100'0000u) == 0b1000'0000u;
    }

    const uint8_t* pointer() const
    {
        return ptr_;
    }

    iterator& operator++()
    {
        ptr_ += num_bytes();
        return *this;
    }

    iterator& operator--()
    {
        ptr_ -= previous_num_bytes();
        return *this;
    }

    iterator& operator+=(ptrdiff_t n)
    {
        _offset(n);
        return *this;
    }

    iterator& operator-=(ptrdiff_t n)
    {
        _offset(-n);
        return *this;
    }

    bool operator==(const iterator& other) const
    {
        assert(_is_valid_codepoint_begin(*this->ptr_));
        assert(_is_valid_codepoint_begin(*other.ptr_));
        return this->ptr_ == other.ptr_;
    }

    bool operator!=(const iterator& other) const
    {
        return !(this->ptr_ == other.ptr_);
    }

    ptrdiff_t operator-(const iterator& other) const
    {
        bool this_is_behind = this->ptr_ > other.ptr_;
        const auto* begin = this_is_behind ? other.ptr_ : this->ptr_;
        const auto* end = this_is_behind ? this->ptr_ : other.ptr_;
        assert(_is_valid_codepoint_begin(*begin));
        assert(_is_valid_codepoint_begin(*end));

        ptrdiff_t n = 0;
        for (; begin != end; ++begin)
            n += ptrdiff_t(_is_valid_codepoint_begin(*begin));
        return this_is_behind ? n : -n;
    }

    uint32_t operator*() const
    {
        return codepoint();
    }

    void _offset(ptrdiff_t n)
    {
        if (n >= 0)
        {
            for (ptrdiff_t i = 0u; i < n;)
                i += ptrdiff_t(_is_valid_codepoint_begin(*++ptr_));
        }
        else
        {
            for (ptrdiff_t i = 0u; i < n;)
                i += ptrdiff_t(_is_valid_codepoint_begin(*--ptr_));
        }
    }

    size_t previous_num_bytes() const
    {
        if (ptr_[-1] < 0b1000'0000u)
        {
            return 1u;
        }
        else if (ptr_[-2] >= 0b1100'0000u)
        {
            assert(_is_valid_codepoint_tailing(ptr_[-1]));
            return 2u;
        }
        else if (ptr_[-3] >= 0b1100'0000u)
        {
            assert(_is_valid_codepoint_tailing(ptr_[-1]));
            assert(_is_valid_codepoint_tailing(ptr_[-2]));
            return 3u;
        }
        else
        {
            assert(_is_valid_codepoint_tailing(ptr_[-1]));
            assert(_is_valid_codepoint_tailing(ptr_[-2]));
            assert(_is_valid_codepoint_tailing(ptr_[-3]));
            assert((*ptr_ > 0u) && _is_valid_codepoint_begin(ptr_[-4]));
            return 4u;
        }
    }

    size_t num_bytes() const
    {
        assert((*ptr_ > 0u) && _is_valid_codepoint_begin(*ptr_));
        if (*ptr_ < 0b1000'0000u)
        {
            return 1u;
        }
        else if (*ptr_ < 0b1110'0000u)
        {
            assert(_is_valid_codepoint_tailing(ptr_[1]));
            return 2u;
        }
        else if (*ptr_ < 0b1111'0000u)
        {
            assert(_is_valid_codepoint_tailing(ptr_[1]));
            assert(_is_valid_codepoint_tailing(ptr_[2]));
            return 3u;
        }
        else
        {
            assert(_is_valid_codepoint_tailing(ptr_[1]));
            assert(_is_valid_codepoint_tailing(ptr_[2]));
            assert(_is_valid_codepoint_tailing(ptr_[3]));
            return 4u;
        }
    }

    uint32_t codepoint() const
    {
        assert((*ptr_ > 0u) && _is_valid_codepoint_begin(*ptr_));
        switch (num_bytes())
        {
        case 1:
            return uint32_t(ptr_[0]);
        case 2:
            return uint32_t(
                ((ptr_[0] & 0b0001'1111) << 6) |
                ((ptr_[1] & 0b0011'1111)));
        case 3:
            return uint32_t(
                ((ptr_[0] & 0b0000'1111) << 12) |
                ((ptr_[1] & 0b0011'1111) << 6) |
                ((ptr_[2] & 0b0011'1111)));
        case 4:
            return uint32_t(
                ((ptr_[0] & 0b0000'0111) << 18) |
                ((ptr_[1] & 0b0011'1111) << 12) |
                ((ptr_[2] & 0b0011'1111) << 6) |
                ((ptr_[2] & 0b0011'1111)));
        }
    }
};

}

union u8string
{
    static constexpr size_t small_string_byte_size = 28u; // excluding \0
    
    struct static_t
    {
        bool    is_static_ = true;
        uint8_t byte_size_;   // size excluding \0
        uint8_t string_size_; // number of codepoints
        uint8_t string_[small_string_byte_size + 1u];

        static_t(size_t byte_size, size_t string_size) :
            byte_size_{uint8_t(byte_size)}, string_size_{uint8_t(string_size)}
        {
        }
    };

    struct dynamic_t
    {
        bool     is_static_ = false;
        uint64_t byte_size_;   // size excluding \0
        uint64_t string_size_; // number of codepoints
        uint8_t* string_;
        
        dynamic_t(size_t byte_size, size_t string_size) :
            byte_size_{byte_size}, string_size_{string_size}, string_{nullptr}
        {
            allocate(byte_size_);
        }

        ~dynamic_t()
        {
            free();
        }

        dynamic_t(const dynamic_t& other)
        {
            copy_sizes(other);
            allocate(byte_size_);
            std::copy_n(other.string_, byte_size_ + 1u, string_);
        }

        dynamic_t(dynamic_t&& other)
        {
            copy_sizes(other);
            std::swap(string_, other.string_);
        }

        dynamic_t& operator=(const dynamic_t& other)
        {
            copy_sizes(other);
            free();
            allocate(byte_size_);
            std::copy_n(other.string_, byte_size_ + 1u, string_);
            return *this;
        }

        dynamic_t& operator=(dynamic_t&& other)
        {
            copy_sizes(other);
            std::swap(this->string_, other.string_);
            return *this;
        }

        void allocate(size_t byte_size)
        {
            assert(!string_);
            string_ = (uint8_t*)std::malloc(byte_size + 1u);
            if (!string_)
                throw std::bad_alloc();
        }

        void free()
        {
            assert(string_);
            std::free(string_);
        }

        void copy_sizes(const dynamic_t& other)
        {
            byte_size_   = other.byte_size_;
            string_size_ = other.string_size_;
        }
    };
    
    static_assert(sizeof(bool) == 1u, WL_ERROR_INTERNAL);
    static_assert(sizeof(static_t) == 32u, WL_ERROR_INTERNAL);
    static_assert(sizeof(dynamic_t) == 32u, WL_ERROR_INTERNAL);

    static_t  static_;
    dynamic_t dynamic_;

    u8string()
    {
        new(&static_) static_t(0u, 0u);
    }

    ~u8string()
    {
        destroy();
    }

    template<typename Any>
    explicit u8string(const Any* str)
    {
        static_assert(sizeof(Any) == 1u, WL_ERROR_INTERNAL);
        auto [byte_size, string_size] = utf8::get_sizes(str);
        if (byte_size <= small_string_byte_size)
        {
            new(&static_) static_t(byte_size, string_size);
            std::memcpy(static_byte_data(), str, byte_size + 1u);
        }
        else
        {
            new(&dynamic_) dynamic_t(byte_size, string_size);
            std::memcpy(dynamic_byte_data(), str, byte_size + 1u);
        }
    }

    u8string(const u8string& other)
    {
        copy_from(other);
    }

    u8string(u8string&& other)
    {
        swap_with(other);
    }

    u8string& operator=(const u8string& other)
    {
        destroy();
        copy_from(other);
        return *this;
    }

    u8string& operator=(u8string&& other)
    {
        swap_with(other);
        return *this;

    }

    void destroy()
    {
        if (!is_static())
            dynamic_.~dynamic_t();
    }

    void swap_with(u8string& other)
    {
        char buffer[sizeof(u8string)];
        std::memcpy(buffer, &other, sizeof(u8string));
        std::memcpy(&other, this, sizeof(u8string));
        std::memcpy(this, buffer, sizeof(u8string));
    }

    void copy_from(const u8string& other)
    {
        if (other.is_static())
        {
            new(&static_) static_t(other.static_);
        }
        else
        {
            if (other.dynamic_byte_size() <= small_string_byte_size)
            { // convert to static
                new(&static_) static_t(other.dynamic_byte_size(),
                    other.dynamic_string_size());
                std::memcpy(static_byte_data(), other.dynamic_byte_data(),
                    static_byte_size() + 1u);
            }
            else
            { // remain dynamic
                new(&dynamic_) dynamic_t(other.dynamic_byte_size(),
                    other.dynamic_string_size());
                std::memcpy(dynamic_byte_data(), other.dynamic_byte_data(),
                    dynamic_byte_size() + 1u);
            }
        }
    }

    bool is_static() const
    {
        bool value;
        std::memcpy(&value, &static_.is_static_, sizeof(bool));
        return value;
    }

    size_t static_byte_size() const { return static_.byte_size_; }
    size_t dynamic_byte_size() const { return dynamic_.byte_size_; }

    size_t byte_size() const
    {
        if (is_static())
            return static_byte_size();
        else
            return dynamic_byte_size();
    }

    size_t static_string_size() const { return static_.string_size_; }
    size_t dynamic_string_size() const { return dynamic_.string_size_; }

    size_t size() const
    {
        if (is_static())
            return static_string_size();
        else
            return dynamic_string_size();
    }

    uint8_t* static_byte_data() { return static_.string_; }
    uint8_t* dynamic_byte_data() { return dynamic_.string_; }

    uint8_t* byte_data()
    {
        if (is_static())
            return static_byte_data();
        else
            return dynamic_byte_data();
    }

    const uint8_t* static_byte_data() const { return static_.string_; }
    const uint8_t* dynamic_byte_data() const { return dynamic_.string_; }

    const uint8_t* byte_data() const
    {
        if (is_static())
            return static_byte_data();
        else
            return dynamic_byte_data();
    }

    bool ascii_only() const
    {
        return byte_size() == size();
    }

    const char* c_str() const
    {
        return reinterpret_cast<const char*>(byte_data());
    }

    uint8_t* byte_begin() { return byte_data(); }
    uint8_t* byte_end() { return byte_data() + byte_size(); }

    const uint8_t* byte_begin() const { return byte_data(); }
    const uint8_t* byte_end() const { return byte_data() + byte_size(); }

    utf8::iterator begin() const { return utf8::iterator{byte_begin()}; }
    utf8::iterator end() const { return utf8::iterator{byte_end()}; }

    std::string _ascii_string() const
    {
        std::string str(size(), ' ');
        const size_t size = this->size();
        auto begin = this->begin();
        auto end = this->end();
        for (size_t i = 0; i < size; ++i, ++begin)
        {
            const auto cp = *begin;
            str[i] = cp > 0b1000'0000u ? '?' : char(cp);
        }
        assert(begin == end);
        return str;
    }

    std::string _string_info() const
    {
        std::string info = "{ ";
        info += "size=" + std::to_string(size()) + ", ";
        info += "byte_size=" + std::to_string(byte_size()) + ", ";
        info += "is_static=" +
            std::string(is_static() ? "true" : "false") + ", ";
        info += "content=";
        info += _ascii_string();
        info += " }";
        return info;
    }
};

}
