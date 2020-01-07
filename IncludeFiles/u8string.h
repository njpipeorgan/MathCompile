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

using char_t = uint8_t;
constexpr char_t null_character = '\0';

inline bool is_ascii(char_t ch)
{
    return ch < char_t(0b1000'0000);
}

inline size_t _get_string_size_impl(const char_t* str, const size_t byte_size)
{
#if defined(__AVX2__) || defined(__SSE4_1__)
    using namespace wl::simd;
#  if defined(__AVX2__)
    using M = __m256i;
#  else
    using M = __m128i;
#endif
    const auto upper = set1<M>(int8_t(0b1100'0000));
    auto trailing = zero<M>();
    size_t trailing_size = 0u;
    size_t i_byte = 0u;
    for (size_t i = 0u; i_byte + sizeof(M) < byte_size;
        ++i, str += sizeof(M), i_byte += sizeof(M))
    {
        trailing = sub_epi8(trailing, cmpgt_epi8(upper, loadu<M>(str)));
        if (i >= 100u)
        {
            trailing_size += hsum_epi8(trailing);
            trailing = zero<M>();
            i = 0u;
        }
    }
    auto tmask = unsigned(movemask_epi8(cmpgt_epi8(upper, loadu<M>(str))));
    auto excess_trailing = utils::_popcnt(
        uint64_t(tmask) << (64u - (byte_size - i_byte)));
    trailing_size += hsum_epi8(trailing);
    trailing_size += excess_trailing;
    return byte_size - trailing_size;
#else
    size_t trailing_size = 0u;
    for (size_t i = 0; i < byte_size; ++i)
    {
        if (int8_t(str[i]) < int8_t(0b1100'0000))
            ++trailing_size;
    }
    return byte_size - trailing_size;
#endif
}

inline size_t _get_string_size_check_valid_impl(
    const char_t* in_str, const size_t ref_byte_size)
{
    size_t byte_size = 0;
    size_t trailing_size = 0;
    auto str_begin = reinterpret_cast<const int8_t*>(in_str);
    auto str = str_begin;

    for (;;)
    {
        size_t n_bytes = 0u;
        char_t byte = *str++;
        if (!byte)
        {
            if (byte_size != ref_byte_size)
                throw std::logic_error(WL_ERROR_INTERNAL);
            return byte_size - trailing_size;
        }
        if (byte_size >= ref_byte_size)
            throw std::logic_error(WL_ERROR_BAD_UTF8_NULL_TERMINATED);
        if (byte < 0b1000'0000u)
            n_bytes = 1u;
        else if ((byte & 0b1110'0000u) == 0b1100'0000u)
            n_bytes = 2u;
        else if ((byte & 0b1111'0000u) == 0b1110'0000u)
            n_bytes = 3u;
        else if ((byte & 0b1111'1000u) == 0b1111'0000u)
            n_bytes = 4u;
        else
            throw std::logic_error(WL_ERROR_BAD_UTF8_CODEPOINT);

        for (size_t i = 1u; i < n_bytes; ++i)
        {
            if ((*str++ & 0b1100'0000u) != 0b1000'0000u)
                throw std::logic_error(WL_ERROR_BAD_UTF8_CODEPOINT);
        }
        trailing_size += n_bytes - 1u;
        byte_size += n_bytes;
    }
}

template<bool CheckValid = false>
size_t get_string_size(const char_t* str, const size_t byte_size)
{
    if (CheckValid)
        return _get_string_size_check_valid_impl(str, byte_size);
    else
        return _get_string_size_impl(str, byte_size);
}

bool is_ascii_only(const char_t* str, const size_t byte_size)
{
#if defined(__AVX2__) || defined(__SSE4_1__)
    using namespace wl::simd;
#  if defined(__AVX2__)
    using M = __m256i;
#  else
    using M = __m128i;
#endif
    const auto upper = set1<M>(int8_t(0b1100'0000));
    size_t i_byte = 0u;
    for (size_t i = 0u; i_byte + sizeof(M) < byte_size;
        ++i, str += sizeof(M), i_byte += sizeof(M))
    {
        auto tmask = unsigned(movemask_epi8(loadu<M>(str)));
        if (tmask)
            return false;
    }
    auto tmask = unsigned(movemask_epi8(loadu<M>(str)));
    return !bool(uint64_t(tmask) << (64u - (byte_size - i_byte)));
#else
    size_t trailing_size = 0u;
    for (size_t i = 0; i < byte_size; ++i)
    {
        if (int8_t(str[i]) < int8_t(0b1100'0000))
            return true;
    }
    return false;
#endif
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
        default:
            return uint32_t(0);
        }
    }
};

}

union u8string
{
    static constexpr size_t small_string_byte_size = 28u; // excluding \0
    static_assert(sizeof(char) == 1u, WL_ERROR_SIZEOF_CHAR);

    enum class trilean_t : uint8_t { True, False, Unknown };
    
    struct static_t
    {
        static constexpr size_t capacity_ = small_string_byte_size;

        bool is_static_ = true;
        mutable trilean_t ascii_only_ = trilean_t::True;
        uint8_t byte_size_ = 0u;
        utf8::char_t string_[small_string_byte_size + 1u];

        static_t() = default;

        static_t(size_t byte_size) : byte_size_{uint8_t(byte_size)}
        {
            if (byte_size_ > 0u)
                ascii_only_ = trilean_t::Unknown;
            assert(byte_size_ <= capacity_);
        }

        void put_null_character()
        {
            string_[byte_size_] = utf8::null_character;
        }

        template<bool PutNull = true, bool UpdateASCII = true>
        void push(utf8::char_t ch)
        {
            assert(byte_size_ + 1u <= capacity_);
            string_[byte_size_++] = ch;
            if constexpr (UpdateASCII)
                if (!utf8::is_ascii(ch))
                    ascii_only_ = trilean_t::False;
            if constexpr (PutNull)
                put_null_character();
        }

        template<bool PutNull = true, bool UpdateASCII = true>
        void push(const utf8::char_t* ch, size_t size)
        {
            assert(byte_size_ + size <= capacity_);
            utils::restrict_copy_n(ch, size, string_ + byte_size_);
            byte_size_ += uint8_t(size);
            if constexpr (UpdateASCII)
                ascii_only_ = trilean_t::Unknown;
            if constexpr (PutNull)
                put_null_character();
        }
    };

    struct dynamic_t
    {
        bool is_static_ = false;
        mutable trilean_t ascii_only_ = trilean_t::True;
        uint64_t byte_size_ = 0u;
        uint64_t capacity_ = 0u;
        utf8::char_t* string_ = nullptr;
        
        dynamic_t(size_t byte_size) : byte_size_{byte_size}
        {
            if (byte_size_ > 0u)
                ascii_only_ = trilean_t::Unknown;
            resize_buffer(byte_size_);
        }

        dynamic_t(size_t byte_size, size_t capacity) : byte_size_{byte_size}
        {
            if (byte_size_ > 0u)
                ascii_only_ = trilean_t::Unknown;
            capacity = std::max(byte_size, capacity);
            resize_buffer(capacity);
        }

        ~dynamic_t()
        {
            free_buffer();
        }

        dynamic_t(const dynamic_t& other)
        {
            ascii_only_ = other.ascii_only_;
            byte_size_  = other.byte_size_;
            resize_buffer(byte_size_);
            std::copy_n(other.string_, byte_size_ + 1u, string_);
        }

        dynamic_t(dynamic_t&& other)
        {
            ascii_only_ = other.ascii_only_;
            byte_size_  = other.byte_size_;
            std::swap(string_, other.string_);
            std::swap(capacity_, other.capacity_);
        }

        dynamic_t& operator=(const dynamic_t& other)
        {
            free_buffer();
            ascii_only_ = other.ascii_only_;
            byte_size_  = other.byte_size_;
            resize_buffer(byte_size_);
            std::copy_n(other.string_, byte_size_ + 1u, string_);
            return *this;
        }

        dynamic_t& operator=(dynamic_t&& other)
        {
            ascii_only_ = other.ascii_only_;
            byte_size_  = other.byte_size_;
            std::swap(string_, other.string_);
            std::swap(capacity_, other.capacity_);
            return *this;
        }

        void put_null_character()
        {
            string_[byte_size_] = utf8::null_character;
        }

        template<bool PutNull = true, bool UpdateASCII = true>
        void push(utf8::char_t ch)
        {
            if (byte_size_ >= capacity_)
                grow_buffer();
            assert(byte_size_ + 1u <= capacity_);
            string_[byte_size_++] = ch;
            if constexpr (UpdateASCII)
                if (!utf8::is_ascii(ch))
                    ascii_only_ = trilean_t::False;
            if constexpr (PutNull)
                put_null_character();
        }

        template<bool PutNull = true, bool UpdateASCII = true>
        void push(const utf8::char_t* ch, size_t size)
        {
            if (byte_size_ + size > capacity_)
                grow_buffer(size);
            assert(byte_size_ + size <= capacity_);
            utils::restrict_copy_n(ch, size, string_ + byte_size_);
            byte_size_ += uint8_t(size);
            if constexpr (UpdateASCII)
                ascii_only_ = trilean_t::Unknown;
            if constexpr (PutNull)
                put_null_character();
        }

        void resize_buffer(size_t new_capacity)
        {
            if (new_capacity <= capacity_)
                return;
            auto new_buffer_size = new_capacity + 1u;
            auto storage = (uint8_t*)std::realloc(string_, new_buffer_size);
            if (!storage)
                throw std::bad_alloc();
            string_ = storage;
            capacity_ = new_capacity;
        }

        void grow_buffer(size_t extra = 0u)
        {
            auto new_buffer_size = capacity_ + 1u;
            new_buffer_size += std::max(capacity_ + 1u, extra);
            auto storage = (uint8_t*)std::realloc(string_, new_buffer_size);
            if (!storage)
                throw std::bad_alloc();
            string_ = storage;
            capacity_ = new_buffer_size - 1u;
        }

        void free_buffer()
        {
            assert(string_);
            std::free(string_);
            capacity_ = 0u;
        }
    };
    
    static_assert(sizeof(bool) == 1u, WL_ERROR_INTERNAL);
    static_assert(sizeof(static_t) == 32u, WL_ERROR_INTERNAL);
    static_assert(sizeof(dynamic_t) == 32u, WL_ERROR_INTERNAL);

    static_t  static_;
    dynamic_t dynamic_;

    u8string()
    {
        new(&static_) static_t(0u);
        static_.put_null_character();
    }

    explicit u8string(size_t byte_size)
    {
        if (byte_size <= small_string_byte_size)
            new(&static_) static_t(byte_size);
        else
            new(&dynamic_) dynamic_t(byte_size);
    }

    template<size_t N>
    explicit u8string(const char (&str)[N])
    {
        static_assert(N >= 1u, WL_ERROR_INTERNAL);
        constexpr auto byte_size = N - 1u;
#if !defined(_NDEBUG)
        // check validity
        utf8::get_string_size<true>((const utf8::char_t*)str, N - 1u);
#endif
        if (byte_size <= small_string_byte_size)
        {
            new(&static_) static_t(byte_size);
            std::memcpy(byte_data(), (const char*)str, byte_size + 1u);
        }
        else
        {
            new(&dynamic_) dynamic_t(byte_size);
            std::memcpy(byte_data(), (const char*)str, byte_size + 1u);
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

    ~u8string()
    {
        destroy();
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
            const auto byte_size = other.byte_size();
            if (byte_size <= small_string_byte_size)
            { // convert to static
                new(&static_) static_t(byte_size);
                utils::restrict_copy_n(
                    other.byte_data(), byte_size + 1u, byte_data());
            }
            else
            { // remain dynamic
                new(&dynamic_) dynamic_t(byte_size);
                utils::restrict_copy_n(
                    other.byte_data(), byte_size + 1u, byte_data());
            }
        }
    }

    void set_dynamic_capacity(size_t capacity)
    {
        if (is_static())
        {
            const static_t copy = static_;
            const size_t byte_size = copy.byte_size_;
            new(&dynamic_) dynamic_t(byte_size, capacity);
            utils::restrict_copy_n(
                copy.string_, byte_size + 1u, dynamic_.string_);
            dynamic_.ascii_only_ = copy.ascii_only_;
        }
        else
        {
            dynamic_.resize_buffer(capacity);
        }
    }

    WL_INLINE bool is_static() const
    {
        bool value;
        std::memcpy(&value, &static_.is_static_, sizeof(bool));
        return value;
    }

    WL_INLINE size_t byte_size() const
    {
        return is_static() ? static_.byte_size_ : dynamic_.byte_size_;
    }
    WL_INLINE size_t capacity() const
    {
        return is_static() ? static_.capacity_ : dynamic_.capacity_;
    }
    WL_INLINE size_t size() const
    {
        const auto str = is_static() ? static_.string_ : dynamic_.string_;
        const auto string_size = utf8::get_string_size(str, byte_size());
        set_ascii_only(string_size == byte_size());
        return string_size;
    }

    WL_INLINE uint8_t* byte_data()
    {
        return is_static() ? static_.string_ : dynamic_.string_;
    }
    WL_INLINE const uint8_t* byte_data() const
    {
        return is_static() ? static_.string_ : dynamic_.string_;
    }

    WL_INLINE const char* c_str() const
    {
        return reinterpret_cast<const char*>(byte_data());
    }

    bool ascii_only() const
    {
        auto only = is_static() ? &static_.ascii_only_ : &dynamic_.ascii_only_;
        if (*only == trilean_t::Unknown)
        {
            if (utf8::is_ascii_only(byte_data(), byte_size()))
                *only = trilean_t::True;
            else
                *only = trilean_t::False;
        }
        return *only == trilean_t::True ? true : false;
    }

    void set_ascii_only(bool ascii_only) const
    {
        auto only = is_static() ? &static_.ascii_only_ : &dynamic_.ascii_only_;
        *only = ascii_only ? trilean_t::True : trilean_t::False;
    }

    uint8_t* byte_begin() { return byte_data(); }
    uint8_t* byte_end() { return byte_data() + byte_size(); }

    const uint8_t* byte_begin() const { return byte_data(); }
    const uint8_t* byte_end() const { return byte_data() + byte_size(); }

    utf8::iterator begin() const { return utf8::iterator{byte_begin()}; }
    utf8::iterator end() const { return utf8::iterator{byte_end()}; }

    void append(const utf8::char_t* str, const size_t append_size)
    {
        const size_t new_byte_size = byte_size() + append_size;
        if (is_static())
        {
            if (new_byte_size > small_string_byte_size)
            {
                set_dynamic_capacity(new_byte_size);
                assert(!is_static());
                dynamic_.push(str, append_size);
            }
            else
            {
                static_.push(str, append_size);
            }
        }
        else
        {
            dynamic_.push(str, append_size);
        }
    }

    std::string _ascii_string() const
    {
        std::string str(size(), ' ');
        const size_t size = this->size();
        auto begin = this->begin();
        auto end = this->end();
        for (size_t i = 0; i < size; ++i, ++begin)
        {
            const auto cp = *begin;
            str[i] = cp > 128u ? '?' : char(cp);
        }
        assert(begin == end);
        return str;
    }

    std::string _string_info() const
    {
        std::string info = "{ ";
        info += "is_static=" +
            std::string(is_static() ? "true" : "false") + ", ";
        info += "ascii_only=" +
            std::string(ascii_only() ? "true" : "false") + ", ";
        info += "size=" + std::to_string(size()) + ", ";
        info += "byte_size=" + std::to_string(byte_size()) + ", ";
        info += "capacity=" + std::to_string(capacity()) + ", ";
        info += "content=";
        info += _ascii_string();
        info += " }";
        return info;
    }
};

}
