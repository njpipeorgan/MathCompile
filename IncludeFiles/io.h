﻿// Compiler from Wolfram Language to C++
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

#ifdef _WIN32
#include <wchar.h>
#else
#include <unistd.h>
#endif

#include <cstdio>
#include <locale>
#include <fstream>
#include <iostream>

#include "types.h"
#include "ndarray.h"
#include "utils.h"
#include "stringfn.h"

namespace wl
{

#if defined(WL_USE_MATHLINK)

template<typename X>
auto print(const X& x);

template<typename X>
auto echo(X&& x);

#else

template<typename X>
auto print(const X& x)
{
    std::cout << to_string(x)._ascii_string() << std::endl;
    return const_null;
}

template<typename X>
auto echo(X&& x)
{
    std::cout << to_string(x)._ascii_string() << std::endl;
    return std::forward<decltype(x)>(x);
}

#endif

template<typename Function>
auto echo_function(Function f)
{
    return [=](auto&& x)
    {
        const auto& xref = x;
        echo(f(x));
        return std::forward<decltype(x)>(x);
    };
}

#ifdef _WIN32
#  define WL_GETCWD _wgetcwd
#  define WL_PATH_CHAR_TYPE wchar_t

struct _codecvt_wchar_t:
    std::codecvt<char16_t, char, std::mbstate_t>
{
    template<typename... Args>
    _codecvt_wchar_t(Args&& ...args) :
        std::codecvt<char16_t, char, std::mbstate_t>(
            std::forward<Args>(args)...)
    {
    }
    ~_codecvt_wchar_t()
    {
    }
};

inline std::wstring _from_u8path(const char* u8path)
{
    static std::wstring_convert<_codecvt_wchar_t, char16_t> conv;
    const auto ospath = conv.from_bytes(u8path);
    return std::wstring((const wchar_t*)ospath.c_str());
}
inline wl::string _to_u8path(const WL_PATH_CHAR_TYPE* ospath)
{
    static std::wstring_convert<_codecvt_wchar_t, char16_t> conv;
    const auto u8path = conv.to_bytes((const char16_t*)ospath);
    return wl::string(u8path.c_str());
}
#else
#  define WL_GETCWD getcwd
#  define WL_PATH_CHAR_TYPE char

inline std::string _from_u8path(const char* u8path)
{
    return std::string(u8path);
}
inline wl::string _to_u8path(const WL_PATH_CHAR_TYPE* ospath)
{
    return wl::string(ospath);
}
inline std::string _working_directory()
{
    char buffer[FILENAME_MAX];
    if (getcwd(buffer, FILENAME_MAX))
        return std::string(buffer);
    else
        throw std::logic_error(WL_ERROR_GETCWD);
}
#endif

#define WL_PATH_STRING_TYPE std::basic_string<WL_PATH_CHAR_TYPE>

inline WL_PATH_STRING_TYPE _get_working_directory()
{
    size_t buffer_size = 256;
    WL_PATH_CHAR_TYPE* buffer;
    for (;;)
    {
        buffer = (WL_PATH_CHAR_TYPE*)std::malloc(
            buffer_size * sizeof(WL_PATH_CHAR_TYPE));
        if (!buffer)
            throw std::bad_alloc();

        if (WL_GETCWD(buffer, int(buffer_size)))
        {
            auto ret = WL_PATH_STRING_TYPE(buffer);
            free(buffer);
            return ret;
        }
        else
        {
            free(buffer);
            if (errno == ERANGE) // buffer is too small
                buffer_size *= 2;
            else
                throw std::logic_error(WL_ERROR_GETCWD);
        }
    }
}

inline auto directory()
{
    WL_TRY_BEGIN()
    WL_THROW_IF_ABORT()
    auto cwd = _get_working_directory();
    return _to_u8path(cwd.c_str());
    WL_TRY_END(__func__, __FILE__, __LINE__)
}


namespace stream_separator
{

enum : int { None = 0, Field, Line, EOS };

struct separator_table
{
    static constexpr size_t table_size = 128u;

    const std::array<uint8_t, table_size> table_;

    template<size_t NL, size_t NF, size_t... Is>
    constexpr separator_table(const char (&line_seps)[NL],
        const char (&field_seps)[NF], std::index_sequence<Is...>) :
        table_{(uint8_t(char(Is) == '\0' ? EOS :
            matches_any(char(Is), line_seps) ? Line :
            matches_any(char(Is), field_seps) ? Field : None))...}
    {
    }

    template<size_t N, size_t... Is>
    static constexpr bool _matches_any_impl(const char ch,
        const char (&seps)[N], std::index_sequence<Is...>)
    {
        return ((ch == seps[Is]) || ...);
    }

    template<size_t N>
    static constexpr bool matches_any(const char ch, const char (&seps)[N])
    {
        static_assert(N >= 1u, WL_ERROR_INTERNAL);
        return _matches_any_impl(ch, seps, std::make_index_sequence<N - 1u>{});
    }

    template<typename CharT>
    constexpr bool is_separator(CharT ch, const int sep_class) const
    {
        const auto uch = std::make_unsigned_t<CharT>(ch);
        return (uch < unsigned(table_size)) && table_[uch] >= sep_class;
    }
};

template<size_t NL, size_t NF>
constexpr auto make_table(const char(&line_seps)[NL],
    const char(&field_seps)[NF])
{
    return separator_table(line_seps, field_seps,
        std::make_index_sequence<separator_table::table_size>{});
}

constexpr auto default_table = make_table("\n\r", " \t");
constexpr auto tsv_table = make_table("\n\r", "\t");
constexpr auto csv_table = make_table("\n\r", ",");

}

template<stream_direction Direction>
struct file_stream
{
    using separator_table = stream_separator::separator_table;
    static constexpr auto is_input  = (Direction == stream_direction::In);
    static constexpr auto is_output = (Direction == stream_direction::Out);
    using stream_t = std::conditional_t<is_input,
        std::ifstream, std::ofstream>;
    static constexpr size_t chunk_size = 1048576u;

    const string path_;
    mutable stream_t stream_;

    template<typename String>
    file_stream(const String& path, bool binary, bool append = false) :
        path_{path}, stream_{}
    {
        int mode = 0;
        if (binary) mode |= std::ios_base::binary;
        if (append) mode |= std::ios_base::app;
        auto ospath = _from_u8path(path.c_str());
        stream_.open(ospath, mode);
        if (stream_.fail())
            throw std::logic_error(WL_ERROR_CANNOT_OPEN_FILE);
    }

    ~file_stream()
    {
        close();
    }

    file_stream(const file_stream&) = delete;
    file_stream(file_stream&&) = default;

    file_stream& operator=(const file_stream&) = delete;
    file_stream& operator=(file_stream&&) = default;

    void close()
    {
        if (!stream_.is_open())
            throw std::logic_error(WL_ERROR_STREAM_ALREADY_CLOSED);
        stream_.close();
    }

    bool binary() const
    {
        return stream_.openmode & std::ios_base::binary;
    }

    string path() const
    {
        if (stream_.is_open())
            return path_;
        else
            return string();
    }

    template<typename Pos>
    void seek(const Pos& pos)
    {
        static_assert(is_integral_v<Pos>, WL_ERROR_INTERNAL);
        static_assert(is_input, WL_ERROR_STREAM_SEEK_ON_OUTPUT);
        stream_.clear();
        if (pos >= Pos(0))
            stream_.seekg(pos, std::ios_base::beg);
        else
            stream_.seekg(int64_t(pos), std::ios_base::end);
        stream_.peek();
        if (!stream_.good())
            throw std::logic_error(WL_ERROR_STREAM_SEEK_FAILED);
    }

    auto tell() const
    {
        if constexpr (is_input)
            return stream_.tellg() - std::streampos(0);
        else
            return stream_.tellp() - std::streampos(0);
    }

    size_t remaining_size() const
    {
        if (test_eof())
            return 0u;
        else
        {
            const auto cur_pos = stream_.tellg();
            const auto end_pos = stream_.seekg(0, std::ios_base::end).tellg();
            stream_.seekg(cur_pos);
            return size_t(end_pos - cur_pos);
        }
    }

    // Get the entire file as a string
    auto read_string()
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        auto ret_size = remaining_size();
        auto ret = string(ret_size);
        auto ret_data = (char*)ret.byte_begin();
        for (;;)
        {
            WL_THROW_IF_ABORT()
            if (ret_size <= chunk_size)
            {
                stream_.read(ret_data, ret_size);
                break;
            }
            else
            {
                stream_.read(ret_data, chunk_size);
                ret_size -= chunk_size;
                ret_data += chunk_size;
            }
        }
        if (!ret.check_validity())
            throw std::logic_error(WL_ERROR_INVALID_UTF8_STRING);
        stream_.test_eof();
        return ret;
    }
    
    template<typename CharT>
    size_t get_until(CharT* str, size_t max_count,
        const separator_table& sep_table, const int sep_class, char& sep_found)
    {
        using traits = std::char_traits<char>;
        using sentry_t = typename decltype(stream_)::sentry;
        const sentry_t sentry(stream_, true);
        if (!sentry || max_count == 0u)
            return 0u;

        size_t count = 0u;
        auto* rdbuf = stream_.rdbuf();
        for (auto meta = rdbuf->sgetc();; meta = rdbuf->snextc())
        {
            const auto meta_char = traits::to_char_type(meta);
            if (traits::eq_int_type(traits::eof(), meta))
            {
                stream_.setstate(std::ios_base::eofbit);
                break;
            }
            else if (sep_table.is_separator(meta, sep_class))
            {
                sep_found = meta_char;
                rdbuf->sbumpc();
                break;
            }
            else if (count >= max_count)
            {
                return max_count + 1;
            }
            else
            {
                ++count;
                *str++ = CharT(meta_char);
            }
        }
        *str = '\0';
        return count;
    }

    // Get the next line/field as a string
    // EOF: returns empty string, sep_found = '\0'
    auto read(const separator_table& sep_table, const int sep_class,
        char& sep_found)
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        if (test_eof())
        {
            sep_found = '\0';
            return string();
        }
        auto ret = string(size_t(0)); // is_ascii = Unknown
        for (;;)
        {
            const auto ret_capacity = ret.capacity();
            const auto max_count = ret_capacity - ret.byte_size();
            const auto count = get_until(ret.byte_end(), max_count,
                sep_table, sep_class, sep_found);
            if (count > max_count)
            { // buffer is too small
                ret.uninitialized_resize(ret.byte_size() + max_count);
                ret.set_capacity(2 * ret_capacity);
            }
            else
            {
                ret.uninitialized_resize(ret.byte_size() + count);
                break;
            }
        }
        if (!ret.check_validity())
            throw std::logic_error(WL_ERROR_INVALID_UTF8_STRING);
        return ret;
    }

    // Get the next byte in the stream as a char
    // EOF: returns '\0'
    char read_byte()
    {
        using traits = std::char_traits<char>;
        auto ch = stream_.get();
        return traits::eq_int_type(traits::eof(), ch) ?
            '\0' : traits::to_char_type(ch);
    }

    // Get the next character as a string
    // EOF: returns empty string
    auto read_character()
    {
        using traits = std::char_traits<char>;
        utf8::char_t buffer[4];
        auto leading = stream_.get();
        if (traits::eq_int_type(traits::eof(), leading))
            return string();
        buffer[0] = utf8::char_t(leading);
        if (!utf8::string_iterator::_is_valid_codepoint_leading(buffer[0]))
            throw std::logic_error(WL_ERROR_INVALID_UTF8_STRING);
        const auto num_bytes =
            utf8::string_iterator::_num_bytes_by_leading(buffer[0]);
        for (size_t i = 1; i < num_bytes; ++i)
        {
            auto trailing = stream_.get();
            if (traits::eq_int_type(traits::eof(), trailing))
                throw std::logic_error(WL_ERROR_INVALID_UTF8_STRING);
            buffer[i] = utf8::char_t(trailing);
        }
        auto ret = string(&buffer[0], num_bytes);
        if (!ret.check_validity())
            throw std::logic_error(WL_ERROR_INVALID_UTF8_STRING);
        return ret;
    }

    // Read the next number from the stream
    // EOF: returns false
    template<typename Val>
    bool read_number(const separator_table& sep_table, char& sep_found,
        Val& val)
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        for (;;)
        {
            sep_found = read_byte();
            if (sep_found == '\0')
            {
                return false;
            }
            if (!sep_table.is_separator(sep_found, stream_separator::Field))
            {
                stream_.clear();
                stream_.unget();
                break;
            }
        }
        auto str = read(sep_table, stream_separator::Field, sep_found);
        assert(str.byte_size() > 0);
        if constexpr (std::is_same_v<Val, double>)
        {
            if (!_from_string_impl::string_to_double(str.c_str(), val))
                throw std::logic_error(WL_ERROR_STREAM_CANNOT_READ_NUMBER);
        }
        else if constexpr (std::is_same_v<Val, int64_t>)
        {
            if (!_from_string_impl::string_to_integer(str.c_str(), val))
                throw std::logic_error(WL_ERROR_STREAM_CANNOT_READ_NUMBER);
        }
        else
        {
            static_assert(always_false_v<Val>, WL_ERROR_INTERNAL);
        }
        return true;
    }

    template<typename Val>
    bool binary_read(Val& val)
    {
        if constexpr (is_real_v<Val> || is_complex_v<Val>)
        {
            stream_.read((char*)(&val), sizeof(Val));
            return (stream_.gcount() == sizeof(Val));
        }
        if constexpr (is_string_v<Val>)
        {
            char sep_found;
            return read(stream_separator::default_table,
                stream_separator::EOS, sep_found);
        }
        else
        {
            static_assert(always_false_v<Val>, WL_ERROR_INTERNAL);
        }
    }

    template<typename Val>
    auto binary_read(Val, int64_t count)
    {
        static_assert(is_real_v<Val> || is_complex_v<Val>, WL_ERROR_INTERNAL);
        ndarray<Val, 1u> ret;
        size_t ret_size = (count == const_int_infinity) ?
            (remaining_size() / sizeof(Val)) :
            (count > 0) ? size_t(count) : 0u;
        if (ret_size == 0u)
            return ret;
        ret.uninitialized_resize(std::array<size_t, 1u>{ret_size});
        stream_.read((char*)(ret.data()), ret_size * sizeof(Val));
        if (stream_.gcount() != ret_size * sizeof(Val))
            throw std::logic_error(WL_ERROR_STREAM_READ);
        return ret;
    }

    template<typename CharT, size_t N>
    void write(const CharT* begin, size_t count, const char (&end_chars)[N])
    {
        static_assert(is_output, WL_ERROR_INTERNAL);
        stream_.write((const char*)begin, count);
        if (N >= 2u)
            stream_.write(end_chars, N - 1u);
        if (!stream_.good())
            throw std::logic_error(WL_ERROR_STREAM_WRITE);
    }

    template<typename CharT>
    void write(const CharT* begin, size_t count)
    {
        write(begin, count, "");
    }

    template<typename Val>
    void binary_write(const Val* begin, size_t count)
    {
        static_assert(is_output, WL_ERROR_INTERNAL);
        stream_.write((const char*)begin, count * sizeof(Val));
        if (!stream_.good())
            throw std::logic_error(WL_ERROR_STREAM_WRITE);
    }

    bool eof() const
    {
        return stream_.eof();
    }

    bool fail() const
    {
        return stream_.fail();
    }

    bool test_eof() const
    {
        stream_.clear();
        stream_.peek();
        return stream_.eof();
    }
};

template<bool Binary = false, typename String>
auto open_read(const String& path)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    return input_file_stream(path, Binary);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<bool Binary = false, typename String>
auto open_write(const String& path)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    return output_file_stream(path, Binary);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<bool Binary = false, typename String>
auto open_append(const String& path)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    return output_file_stream(path, Binary, true);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Stream>
auto close(const Stream& stream)
{
    WL_TRY_BEGIN()
    static_assert(is_file_stream_v<Stream>, WL_ERROR_FILE_STREAM_ONLY);
    const auto path = stream.path();
    stream.close();
    return path;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<bool Binary = false, typename Any>
auto _as_input_file_stream(Any& any) -> decltype(auto)
{
    if constexpr (is_file_stream_v<remove_cvref_t<Any>>)
    {
        if (Binary != any.binary())
            throw std::logic_error(WL_ERROR_STREAM_BINARINESS);
        return any;
    }
    else if constexpr (is_string_view_v<remove_cvref_t<Any>>)
    {
        return open_read<Binary>(any);
    }
    else
    {
        static_assert(always_false_v<Any>, WL_ERROR_FILE_STREAM_OR_PATH_ONLY);
        return 0;
    }
}

template<bool Binary = false, typename Any>
auto _as_output_file_stream(Any& any) -> decltype(auto)
{
    if constexpr (is_file_stream_v<remove_cvref_t<Any>>)
    {
        if (Binary != any.binary())
            throw std::logic_error(WL_ERROR_STREAM_BINARINESS);
        return any;
    }
    else if constexpr (is_string_view_v<remove_cvref_t<Any>>)
    {
        return open_write<Binary>(any);
    }
    else
    {
        static_assert(always_false_v<Any>, WL_ERROR_FILE_STREAM_OR_PATH_ONLY);
        return 0;
    }
}

template<typename Stream>
auto stream_position(const Stream& stream)
{
    WL_TRY_BEGIN()
    static_assert(is_file_stream_v<Stream>, WL_ERROR_FILE_STREAM_ONLY);
    return int64_t(stream.tell());
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Stream, typename Pos>
auto set_stream_position(Stream& stream, const Pos& pos)
{
    WL_TRY_BEGIN()
    static_assert(is_file_stream_v<Stream>, WL_ERROR_FILE_STREAM_ONLY);
    static_assert(is_integral_v<Pos>, WL_ERROR_STREAM_POSITION_INTEGRAL);
    return int64_t(stream.seek(pos));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any>
auto read_string(Any& any)
{
    WL_TRY_BEGIN()
    auto& stream = _as_input_file_stream(any);
    return stream.read_string();
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename ReadType>
auto read(Any& any, ReadType)
{
    WL_TRY_BEGIN()
    using namespace stream_separator;
    auto& stream = _as_input_file_stream(any);
    char sep_found = '\0';
    if constexpr (std::is_same_v<ReadType, byte_type>)
    {
        return int64_t(stream.read_byte());
    }
    else if constexpr (std::is_same_v<ReadType, character_type>)
    {
        return stream.read_character();
    }
    else if constexpr (std::is_same_v<ReadType, word_type> ||
        std::is_same_v<ReadType, string_type>)
    {
        for (;;)
        {
            auto str = stream.read(default_table,
                std::is_same_v<ReadType, word_type> ? Field : Line, sep_found);
            if ((str.byte_size() > 0u) || (sep_found == '\0'))
                return str;
        }
    }
    else if constexpr (std::is_same_v<ReadType, real_type> ||
        std::is_same_v<ReadType, integer_type>)
    {
        using Ret = std::conditional_t<
            std::is_same_v<ReadType, real_type>, double, int64_t>;
        Ret ret;
        if (!stream.read_number(default_table, sep_found, ret))
            throw std::logic_error(WL_ERROR_STREAM_CANNOT_READ_NUMBER);
        return ret;
    }
    else
    {
        static_assert(always_false_v<ReadType>, WL_ERROR_READ_UNKNOWN_TYPE);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any>
auto read_line(Any& any)
{
    WL_TRY_BEGIN()
    return read(any, const_string);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename ReadType, typename Count>
auto read_list(Any& any, ReadType, const Count& in_count)
{
    WL_TRY_BEGIN()
    using namespace stream_separator;
    static_assert(is_integral_v<Count>, WL_ERROR_READ_LIST_COUNT_INTEGRAL);
    const auto count = int64_t(std::max(Count(0), in_count));
    auto& stream = _as_input_file_stream(any);
    char ignored = '\0';
    if constexpr (std::is_same_v<ReadType, byte_type>)
    {
        ndarray<utf8::char_t, 1u> ret;
        for (int64_t i = 0; i < count; ++i)
        {
            auto ch = utf8::char_t(stream.read_byte());
            if (ch == 0)
                break;
            ret.append(ch);
        }
        return ret;
    }
    else if constexpr (std::is_same_v<ReadType, character_type>)
    {
        ndarray<string, 1u> ret;
        for (int64_t i = 0; i < count; ++i)
        {
            auto ch = stream.read_character();
            if (ch.byte_size() == 0u)
                break;
            ret.append(std::move(ch));
        }
        return ret;
    }
    else if constexpr (std::is_same_v<ReadType, word_type> ||
        std::is_same_v<ReadType, string_type>)
    {
        ndarray<string, 1u> ret;
        char sep_found = '\0';
        for (int64_t i = 0; i < count; ++i)
        {
            auto str = stream.read(default_table,
                std::is_same_v<ReadType, word_type> ? Field : Line, sep_found);
            if (sep_found == '\0')
                break;
            ret.append(std::move(str));
        }
        return ret;
    }
    else if constexpr (std::is_same_v<ReadType, real_type> ||
        std::is_same_v<ReadType, integer_type>)
    {
        using Elem = std::conditional_t<
            std::is_same_v<ReadType, real_type>, double, int64_t>;
        ndarray<Elem, 1u> ret;
        char sep_found = '\0';
        for (int64_t i = 0; i < count; ++i)
        {
            Elem elem;
            if (!stream.read_number(default_table, Field, sep_found, elem))
            { // EOF
                break;
            }
            ret.append(elem);
        }
        return ret;
    }
    else
    {
        static_assert(always_false_v<ReadType>, WL_ERROR_READ_UNKNOWN_TYPE);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename ReadType>
auto read_list(Any& any, ReadType)
{
    return read_list(any, ReadType{}, const_int_infinity);
}

template<typename Val>
auto _write_impl(output_file_stream& stream, const Val& val)
{
    if constexpr (is_string_view_v<Val>)
        stream.write(val.byte_begin(), val.byte_size(), "\n");
    else
        write(stream, to_string(val));
    return 0;
}

template<typename Val>
auto _write_string_impl(output_file_stream& stream, const Val& val)
{
    static_assert(is_string_view_v<Val>, WL_ERROR_WRITE_STRING_ONLY);
    stream.write(val.byte_begin(), val.byte_size());
    return 0;
}

template<typename Val>
auto _write_line_impl(output_file_stream& stream, const Val& val)
{
    static_assert(is_string_view_v<Val>, WL_ERROR_WRITE_STRING_ONLY);
    stream.write(val.byte_begin(), val.byte_size(), "\n");
    return 0;
}

template<typename Any, typename... Vals>
auto write(Any& any, const Vals&... vals)
{
    WL_TRY_BEGIN()
    auto& stream = _as_output_file_stream(any);
    [[maybe_unused]] const auto& _1 = (_write_impl(stream, vals), ...);
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename... Vals>
auto write_string(Any& any, const Vals&... vals)
{
    WL_TRY_BEGIN()
    auto& stream = _as_output_file_stream(any);
    [[maybe_unused]] const auto& _1 = (_write_string_impl(stream, vals), ...);
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename... Vals>
auto write_line(Any& any, const Vals&... vals)
{
    WL_TRY_BEGIN()
    auto& stream = _as_output_file_stream(any);
    [[maybe_unused]] const auto& _1 = (_write_line_impl(stream, vals), ...);
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename ReadType>
auto binary_read(Any& any, ReadType)
{
    WL_TRY_BEGIN()
    auto& stream = _as_input_file_stream<true>(any);
    if constexpr (is_real_v<ReadType> || is_complex_v<ReadType>)
    {
        ReadType val;
        if (!stream.binary_read(val))
            throw std::logic_error(WL_ERROR_STREAM_CANNOT_READ_BINARY);
        return val;
    }
    else if constexpr (is_string_v<ReadType>)
    {
        static_assert(always_false_v<ReadType>,
            WL_ERROR_BINARY_READ_WRITE_UNKNOWN_TYPE);
    }
    else
    {
        static_assert(always_false_v<ReadType>,
            WL_ERROR_BINARY_READ_WRITE_UNKNOWN_TYPE);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename ReadType, typename Count>
auto binary_read_list(Any& any, ReadType, const Count& in_count)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<Count>, WL_ERROR_READ_LIST_COUNT_INTEGRAL);
    const auto count = int64_t(std::max(Count(0), in_count));
    auto& stream = _as_input_file_stream<true>(any);
    static_assert(is_real_v<ReadType> || is_complex_v<ReadType>,
        WL_ERROR_BINARY_READ_WRITE_UNKNOWN_TYPE);
    return stream.binary_read(ReadType{}, count);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename ReadType>
auto binary_read_list(Any& any, ReadType)
{
    WL_TRY_BEGIN()
    return binary_read_list(any, ReadType{}, const_int_infinity);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename X, typename WriteType>
void binary_write(Any& any, const X& x, WriteType)
{
    WL_TRY_BEGIN()
    auto& stream = _as_output_file_stream<true>(any);
    static_assert(is_real_v<WriteType> || is_complex_v<WriteType>,
        WL_ERROR_BINARY_READ_WRITE_UNKNOWN_TYPE);
    constexpr auto XR = array_rank_v<X>;
    if constexpr (XR ==  0u)
    {
        const auto& val = cast<WriteType>(x);
        stream.binary_write(&val, 1u);
    }
    else
    {
        const auto& val = cast<ndarray<WriteType, XR>>(x);
        stream.binary_write(val.data(), val.size());
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename X>
void binary_write(Any& any, const X& x)
{
    WL_TRY_BEGIN()
    if constexpr (array_rank_v<X> == 0u)
        binary_write(any, x, X{});
    else
        binary_write(any, x, value_type_t<X>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
