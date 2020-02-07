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

#if __has_include(<filesystem>)
#include <filesystem>
#  define STD_FILESYSTEM std::filesystem
#else
#include <experimental/filesystem>
#  define STD_FILESYSTEM std::experimental::filesystem
#endif

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

inline auto directory()
{
    WL_TRY_BEGIN()
    auto dir = STD_FILESYSTEM::current_path().u8string();
    return string(dir.c_str(), dir.size());
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<stream_direction Direction>
struct file_stream
{
    static constexpr auto is_input  = (Direction == stream_direction::In);
    static constexpr auto is_output = (Direction == stream_direction::Out);
    using stream_t = std::conditional_t<
        is_input, std::ifstream, std::ofstream>;
    static constexpr size_t chunk_size = 1048576u;

    const STD_FILESYSTEM::path path_;
    mutable stream_t stream_;

    template<typename String>
    file_stream(const String& path, bool binary, bool append = false) :
        path_{(const char*)path.byte_begin(), (const char*)path.byte_end()},
        stream_{}
    {
        int mode = 0;
        if (binary) mode |= std::ios_base::binary;
        if (append) mode |= std::ios_base::app;
        stream_.open(path_, mode);
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

    auto read_string()
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        if (eof())
            return string();
        const auto cur_pos = stream_.tellg();
        const auto end_pos = stream_.seekg(0, std::ios_base::end).tellg();
        stream_.seekg(cur_pos);
        auto ret_size = end_pos - cur_pos;
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

    template<size_t N, size_t... Is>
    static bool _match_any_delimiter_impl(char ch, const char(&delim)[N],
        std::index_sequence<Is...>)
    {
        return ((ch == delim[Is]) || ...);
    }

    template<size_t N>
    static bool match_any_delimiter(char ch, const char(&delim)[N])
    {
        if constexpr (N <= 1u)
            return false;
        else
            return _match_any_delimiter_impl(ch, delim,
                std::make_index_sequence<N - 1u>{});
    }

    template<typename CharT, size_t N>
    size_t get_until(CharT* str, size_t max_count, const char (&delim)[N])
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
            else if (match_any_delimiter(meta_char, delim))
            {
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

    template<size_t N>
    auto read_line(const char (&delim)[N])
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        if (eof())
            return string();
        auto ret = string(size_t(0));
        for (;;)
        {
            const auto ret_capacity = ret.capacity();
            const auto max_count = ret_capacity - ret.byte_size();
            const auto count = get_until(ret.byte_end(), max_count, delim);
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

    auto read_line()
    {
        return read_line("\n");
    }

    auto read_character()
    {
        stream.clear();
        auto ch = stream_.get();
        if (!stream_.good())
            throw std::logic_error(WL_ERROR_STREAM_SEEK_FAILED);
        return char(ch);
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
auto _as_input_file_stream(Any& any) -> std::conditional_t<
    is_file_stream_v<remove_cvref_t<Any>>, Any&, input_file_stream>
{
    if constexpr (is_file_stream_v<remove_cvref_t<Any>>)
        return any;
    else if constexpr (is_string_view_v<remove_cvref_t<Any>>)
        return open_read(any);
    else
        static_assert(always_false_v<Any>, WL_ERROR_FILE_STREAM_OR_PATH_ONLY);
}

template<bool Binary = false, typename Any>
auto _as_output_file_stream(Any& any) -> std::conditional_t<
    is_file_stream_v<remove_cvref_t<Any>>, Any&, output_file_stream>
{
    if constexpr (is_file_stream_v<remove_cvref_t<Any>>)
        return any;
    else if constexpr (is_string_view_v<remove_cvref_t<Any>>)
        return open_write(any);
    else
        static_assert(always_false_v<Any>, WL_ERROR_FILE_STREAM_OR_PATH_ONLY);
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
auto stream_position(Stream& stream, const Pos& pos)
{
    WL_TRY_BEGIN()
    static_assert(is_file_stream_v<Stream>, WL_ERROR_FILE_STREAM_ONLY);
    static_assert(is_integral_v<Pos>, WL_ERROR_STREAM_POSITION_INTEGRAL);
    stream.seek(pos);
    return stream_position();
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

template<typename Any>
auto read_line(Any& any)
{
    WL_TRY_BEGIN()
    auto& stream = _as_input_file_stream(any);
    return stream.read_line();
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<size_t N, typename Val>
bool _read_impl(input_file_stream& stream, const char (&delim)[N], Val& val)
{
    while (!stream.eof())
    {
        auto str = stream.read_line(delim);
        if (str.byte_size() > 0)
        {
            if constexpr (std::is_same_v<Val, double>)
                return (std::sscanf(str.c_str(), "%lf", &val) > 0);
            else if constexpr (std::is_same_v<Val, int64_t>)
                return (std::sscanf(str.c_str(), "%dll", &val) > 0);
            else
                static_assert(always_false_v<Val>, WL_ERROR_INTERNAL);
        }
    }
    return false;
}

template<typename Any, typename ReadType>
auto read(Any& any, ReadType)
{
    WL_TRY_BEGIN()
    auto& stream = _as_input_file_stream(any);
    if constexpr (std::is_same_v<ReadType, byte_type>)
    {
        return int64_t(stream.read_character());
    }
    else if constexpr (std::is_same_v<ReadType, character_type>)
    {
        char ch = stream.read_character();
        if (!utf8::is_ascii(ch))
            throw std::logic_error(WL_ERROR_INVALID_UTF8_STRING);
        return string(&ch, 1u);
    }
    else if constexpr (std::is_same_v<ReadType, word_type>)
    {
        while (!stream.eof())
        {
            auto str = stream.read_line(" \t");
            if (str.byte_size() > 0)
                return str;
        }
        return string();
    }
    else if constexpr (std::is_same_v<ReadType, string_type>)
    {
        while (!stream.eof())
        {
            auto str = stream.read_line("\n\r");
            if (str.byte_size() > 0)
                return str;
        }
        return string();
    }
    else if constexpr (std::is_same_v<ReadType, real_type>)
    {
        double ret;
        if (!_read_impl(stream, " \t,", ret))
            throw std::logic_error(WL_ERROR_STREAM_CANNOT_READ_NUMBER);
        return ret;
    }
    else if constexpr (std::is_same_v<ReadType, integer_type>)
    {
        int64_t ret;
        if (!_read_impl(stream, " \t,", ret))
            throw std::logic_error(WL_ERROR_STREAM_CANNOT_READ_NUMBER);
        return ret;
    }
    else
    {
        static_assert(always_false_v<ReadType>, WL_ERROR_UNKNOWN_READ_TYPE);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
