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

#ifdef _WIN32
#include <wchar.h>
#else
#include <unistd.h>
#endif

#include <stdio.h>
#include <locale>
#include <string>

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

struct _codecvt_wchar_t :
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

inline wl::string _to_u8path(const char* ospath)
{
    return wl::string(ospath);
}
inline wl::string _to_u8path(const wchar_t* ospath)
{
    static std::wstring_convert<_codecvt_wchar_t, char16_t> conv;
    const auto u8path = conv.to_bytes((const char16_t*)ospath);
    return wl::string(u8path.c_str());
}

#if defined(_WIN32)
#    define WL_GETCWD _wgetcwd
#    define WL_PATH_CHAR_TYPE wchar_t
#  if defined(_MSC_VER)
inline std::wstring _from_u8path(const char* u8path)
{
    static std::wstring_convert<_codecvt_wchar_t, char16_t> conv;
    const auto ospath = conv.from_bytes(u8path);
    return std::wstring((const wchar_t*)ospath.c_str());
    
}
#  else
inline std::string _from_u8path(const char* u8path)
{
    if (!utf8::is_ascii_only((const utf8::char_t*)u8path, std::strlen(u8path)))
        throw std::logic_error(WL_ERROR_NON_ASCII_PATH);
    return std::string(u8path);
}
#  endif
#else
#  define WL_GETCWD getcwd
#  define WL_PATH_CHAR_TYPE char
inline std::string _from_u8path(const char* u8path)
{
    return std::string(u8path);
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

template<stream_mode Mode, bool Binary>
struct fstream
{
    using state_t = int;
    static constexpr state_t good_state   = 0x00;
    static constexpr state_t failed_state = 0x01;
    static constexpr state_t eof_state    = 0x02;

    static constexpr auto mode = Mode;
    static constexpr auto is_binary = Binary;
    static constexpr bool is_input  = (Mode == stream_mode::Read);
    static constexpr bool is_output = !is_input;

    mutable FILE* file_;
    mutable state_t state_;
    bool is_open_;

    auto is_open() const
    {
        return is_open_;
    }

    auto _set_state(state_t state) const
    {
        state_ |= state;
    }

    auto _clear_state(state_t state) const
    {
        state_ &= ~state;
    }

    auto failed() const
    {
        return state_ & failed_state;
    }

    auto eof() const
    {
        return state_ & eof_state;
    }

    template<typename CharT>
    void open(const CharT* filename)
    {
        CharT mode_str[3];
        mode_str[0] = (mode == stream_mode::Read) ? 'r' :
            (mode == stream_mode::Write) ? 'w' : 'a';
        mode_str[1] = Binary ? 'b' : '\0';
        mode_str[2] = '\0';
        if constexpr (std::is_same_v<CharT, char>)
            file_ = fopen(filename, mode_str);
        else
            file_ = _wfopen(filename, mode_str);
        if (!file_)
            _set_state(failed_state);
        else
        {
            is_open_ = true;
            _clear_state(failed_state | eof_state);
        }
    }

    void close()
    {
        if (is_open())
            fclose(file_);
        file_ = nullptr;
        is_open_ = false;
    }

    fstream() : file_{nullptr}, state_{good_state}, is_open_{false}
    {
    }

    template<typename CharT>
    fstream(const CharT* filename) : fstream()
    {
        open(filename);
    }

    ~fstream()
    {
        close();
    }

    fstream(const fstream&) = delete;
    fstream(fstream&& other) : fstream()
    {
        *this = std::move(other);
    }

    fstream& operator=(const fstream&) = delete;
    fstream& operator=(fstream&& other)
    {
        std::swap(file_, other.file_);
        std::swap(state_, other.state_);
        std::swap(is_open_, other.is_open_);
        return *this;
    }

    bool seek(int64_t pos)
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        assert(is_open());
        assert(pos == int32_t(pos));
        if (fseek(file_, long(pos), pos >= 0 ? SEEK_SET : SEEK_END))
            _set_state(failed_state);
        else
            _clear_state(failed_state | eof_state);
        return check_eof();
    }

    bool seek(int64_t pos, int origin)
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        assert(is_open());
        assert(pos == int32_t(pos));
        if (fseek(file_, long(pos), origin))
            _set_state(failed_state);
        else
            _clear_state(failed_state | eof_state);
        return check_eof();
    }

    auto tell() const
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        assert(is_open());
        auto pos = ftell(file_);
        if (pos == -1)
            _set_state(failed_state);
        return pos;
    }

    auto get()
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        assert(is_open());
        const int ch = fgetc(file_);
        if (ch == EOF)
        {
            _set_state(eof_state);
            return 0;
        }
        else
            return ch;
    }

    template<typename CharT>
    auto unget(CharT ch)
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        _clear_state(eof_state);
        ungetc(ch, file_);
    }

    template<typename CharT>
    auto get(CharT* buffer, size_t size)
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        assert(is_open());
        const auto size_read = fread(buffer, 1u, size, file_);
        if (size_read < size)
            _set_state(feof(file_) ? eof_state : failed_state);
        return size_read;
    }

    auto peek() const
    {
        static_assert(is_input, WL_ERROR_INTERNAL);
        assert(is_open());
        const auto ch = fgetc(file_);
        if (ch == EOF)
            _set_state(eof_state);
        else
            ungetc(ch, file_);
        return ch;
    }

    template<typename CharT>
    void put(CharT ch) const
    {
        static_assert(is_output, WL_ERROR_INTERNAL);
        assert(is_open());
        if (fputc(int(ch), file_) == EOF)
            _set_state(failed_state);
    }

    template<typename CharT>
    void put(const CharT* buffer, size_t size) const
    {
        static_assert(is_output, WL_ERROR_INTERNAL);
        assert(is_open());
        if (fwrite(buffer, 1u, size, file_) < size)
            _set_state(failed_state);
    }

    auto check_eof() const
    {
        assert(is_open());
        peek();
        return eof();
    }
};

template<stream_mode Mode, bool Binary>
struct file_stream
{
    using separator_table = stream_separator::separator_table;
    using stream_t = fstream<Mode, Binary>;
    static constexpr size_t chunk_size = 5u;
    static constexpr bool is_binary = Binary;
    
    const string path_;
    stream_t stream_;

    template<typename String>
    file_stream(const String& path) :
        path_{path}, stream_{}
    {
        const auto ospath = _from_u8path(path.c_str());
        stream_.open(ospath.c_str());
        if (stream_.failed())
            throw std::logic_error(WL_ERROR_CANNOT_OPEN_FILE);
    }

    void close()
    {
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
        static_assert(stream_t::is_input, WL_ERROR_STREAM_SEEK_ON_OUTPUT);
        if (!stream_.seek(int64_t(pos)))
            throw std::logic_error(WL_ERROR_STREAM_SEEK_FAILED);
    }

    auto tell() const
    {
        return int64_t(stream_.tell());
    }

    size_t remaining_byte_size()
    {
        if (stream_.check_eof())
            return 0u;
        else
        {
            const auto cur_pos = stream_.tell();
            stream_.seek(0, SEEK_END);
            const auto end_pos = stream_.tell();
            stream_.seek(cur_pos);
            return size_t(end_pos - cur_pos);
        }
    }

    // Get the entire file as a string
    auto read_string()
    {
        auto byte_size = remaining_byte_size();
        auto ret = string(byte_size);
        auto ret_data = ret.byte_begin();
        for (;;)
        {
            WL_THROW_IF_ABORT()
            ret_data += stream_.get((char*)ret_data, chunk_size);
            if (eof())
                break;
        }
        ret.uninitialized_resize(ret_data - ret.byte_begin());
        if (!ret.check_validity())
            throw std::logic_error(WL_ERROR_INVALID_UTF8_STRING);
        stream_.check_eof();
        return ret;
    }
    
    template<typename CharT>
    size_t get_until(CharT* str, size_t max_count,
        const separator_table& sep_table, const int sep_class, char& sep_found)
    {
        if (max_count == 0u)
            return 0u;
        size_t count = 0u;
        for (auto ch = stream_.get();; ch = stream_.get())
        {
            if (stream_.eof())
            {
                break;
            }
            else if (sep_table.is_separator(ch, sep_class))
            {
                sep_found = char(ch);
                break;
            }
            else if (count >= max_count)
            {
                return max_count + 1;
            }
            else
            {
                ++count;
                *str++ = CharT(ch);
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
        if (stream_.check_eof())
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
    auto read_byte()
    {
        return char(stream_.get());
    }

    // Get the next character as a string
    // EOF: returns empty string
    auto read_character()
    {
        utf8::char_t buffer[4];
        auto leading = stream_.get();
        if (stream_.eof())
            return string();
        buffer[0] = utf8::char_t(leading);
        if (!utf8::string_iterator::_is_valid_codepoint_leading(buffer[0]))
            throw std::logic_error(WL_ERROR_INVALID_UTF8_STRING);
        const auto num_bytes =
            utf8::string_iterator::_num_bytes_by_leading(buffer[0]);
        for (size_t i = 1; i < num_bytes; ++i)
        {
            auto trailing = stream_.get();
            if (stream_.eof())
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
        for (;;)
        {
            sep_found = char(stream_.get());
            if (sep_found == '\0')
            {
                return false;
            }
            if (!sep_table.is_separator(sep_found, stream_separator::Field))
            {
                stream_.unget(sep_found);
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
        if constexpr (is_arithmetic_v<Val>)
        {
            return (stream_.get((char*)(&val), sizeof(Val)) == sizeof(Val));
        }
        else if constexpr (is_string_v<Val>)
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
        static_assert(is_arithmetic_v<Val>, WL_ERROR_INTERNAL);
        ndarray<Val, 1u> ret;
        size_t ret_size = (count == const_int_infinity) ?
            (remaining_byte_size() / sizeof(Val)) :
            (count > 0) ? size_t(count) : 0u;
        if (ret_size == 0u)
            return ret;
        ret.uninitialized_resize(std::array<size_t, 1u>{ret_size});
        auto gcount = stream_.get((char*)(ret.data()), ret_size * sizeof(Val));
        if (gcount != ret_size * sizeof(Val))
            throw std::logic_error(WL_ERROR_STREAM_READ);
        return ret;
    }

    template<typename CharT, size_t N>
    void write(const CharT* begin, size_t count, const char (&end_chars)[N])
    {
        stream_.put((const char*)begin, count);
        if (N >= 2u)
            stream_.put(end_chars, N - 1u);
        if (stream_.failed())
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
        stream_.put((const char*)begin, count * sizeof(Val));
        if (stream_.failed())
            throw std::logic_error(WL_ERROR_STREAM_WRITE);
    }

    bool eof() const
    {
        return stream_.eof();
    }

    bool failed() const
    {
        return stream_.failed();
    }
};

template<bool Binary = false, typename String>
auto open_read(const String& path)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    return file_stream<stream_mode::Read, Binary>(path);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<bool Binary = false, typename String>
auto open_write(const String& path)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    return file_stream<stream_mode::Write, Binary>(path);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<bool Binary = false, typename String>
auto open_append(const String& path)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    return file_stream<stream_mode::Append, Binary>(path);
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
        static_assert(Binary == remove_cvref_t<Any>::is_binary,
            WL_ERROR_STREAM_BINARINESS);
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
        static_assert(Binary == remove_cvref_t<Any>::is_binary,
            WL_ERROR_STREAM_BINARINESS);
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
    decltype(auto) stream = _as_input_file_stream(any);
    return stream.read_string();
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename ReadType>
auto read(Any& any, ReadType)
{
    WL_TRY_BEGIN()
    using namespace stream_separator;
    decltype(auto) stream = _as_input_file_stream(any);
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
        std::is_same_v<ReadType, string_type> ||
        std::is_same_v<ReadType, record_type>)
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
    decltype(auto) stream = _as_input_file_stream(any);
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
        std::is_same_v<ReadType, string_type> ||
        std::is_same_v<ReadType, record_type>)
    {
        ndarray<string, 1u> ret;
        char sep_found = '\0';
        for (int64_t i = 0; i < count; ++i)
        {
            auto str = stream.read(default_table,
                std::is_same_v<ReadType, word_type> ? Field : Line, sep_found);
            if (str.byte_size() > 0u)
                ret.append(std::move(str));
            if (sep_found == '\0')
                break;
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
            if (!stream.read_number(default_table, sep_found, elem))
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

template<typename Stream, typename Val>
auto _write_impl(Stream& stream, const Val& val)
{
    if constexpr (is_string_view_v<Val>)
        stream.write(val.byte_begin(), val.byte_size(), "\n");
    else
        write(stream, to_string(val));
    return 0;
}

template<typename Stream, typename Val>
auto _write_string_impl(Stream& stream, const Val& val)
{
    static_assert(is_string_view_v<Val>, WL_ERROR_WRITE_STRING_ONLY);
    stream.write(val.byte_begin(), val.byte_size());
    return 0;
}

template<typename Stream, typename Val>
auto _write_line_impl(Stream& stream, const Val& val)
{
    static_assert(is_string_view_v<Val>, WL_ERROR_WRITE_STRING_ONLY);
    stream.write(val.byte_begin(), val.byte_size(), "\n");
    return 0;
}

template<typename Any, typename... Vals>
auto write(Any& any, const Vals&... vals)
{
    WL_TRY_BEGIN()
    decltype(auto) stream = _as_output_file_stream(any);
    [[maybe_unused]] const auto& _1 = (_write_impl(stream, vals), ...);
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename... Vals>
auto write_string(Any& any, const Vals&... vals)
{
    WL_TRY_BEGIN()
    decltype(auto) stream = _as_output_file_stream(any);
    [[maybe_unused]] const auto& _1 = (_write_string_impl(stream, vals), ...);
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename... Vals>
auto write_line(Any& any, const Vals&... vals)
{
    WL_TRY_BEGIN()
    decltype(auto) stream = _as_output_file_stream(any);
    [[maybe_unused]] const auto& _1 = (_write_line_impl(stream, vals), ...);
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename ReadType>
auto binary_read(Any& any, ReadType)
{
    WL_TRY_BEGIN()
    decltype(auto) stream = _as_input_file_stream<true>(any);
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
    decltype(auto) stream = _as_input_file_stream<true>(any);
    static_assert(is_arithmetic_v<ReadType>,
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
auto binary_write(Any& any, const X& x, WriteType)
{
    WL_TRY_BEGIN()
    decltype(auto) stream = _as_output_file_stream<true>(any);
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
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Any, typename X>
auto binary_write(Any& any, const X& x)
{
    WL_TRY_BEGIN()
    if constexpr (array_rank_v<X> == 0u)
        return binary_write(any, x, X{});
    else
        return binary_write(any, x, value_type_t<X>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

enum class file_format { Table, TSV, CSV };

template<typename Stream, typename InWriteType, typename X, size_t XR>
void _export_text_impl(Stream& stream, const X& x,
    const std::array<size_t, XR>& x_dims, char line_sep, char field_sep)
{
    static_assert(XR <= 2u, WL_ERROR_INTERNAL);
    char buffer[_to_string_impl::default_buffer_size];
    using XV = std::conditional_t<XR == 0u, X, value_type_t<X>>;
    using WriteType = std::conditional_t<
        std::is_same_v<InWriteType, void_type>, XV, InWriteType>;
    static_assert(is_arithmetic_v<WriteType>, WL_ERROR_EXPORT_TYPE);
    if constexpr (XR == 0u)
    {
        auto view = _to_string_scalar_impl(cast<WriteType>(x), buffer);
        stream.write(view.byte_data(), view.byte_size());
    }
    else if constexpr (XR == 1u)
    {
        const auto row_size = x_dims[0];
        auto x_data = x.data();
        for (size_t i = 0; i < row_size; ++i, ++x_data)
        {
            if (i > 0)
                stream.write(&line_sep, 1u);
            auto str = _to_string_scalar_impl(
                cast<WriteType>(*x_data), buffer);
            stream.write(str.byte_data(), str.byte_size());
        }
    }
    else
    {
        const auto row_size = x_dims[0];
        const auto col_size = x_dims[1];
        auto x_data = x.data();
        for (size_t i = 0; i < row_size; ++i)
        {
            if (i > 0)
                stream.write(&line_sep, 1u);
            for (size_t j = 0; j < col_size; ++j, ++x_data)
            {
                if (j > 0)
                    stream.write(&field_sep, 1u);
                auto str = _to_string_scalar_impl(
                    cast<WriteType>(*x_data), buffer);
                stream.write(str.byte_data(), str.byte_size());
            }
        }
    }
}

template<typename String, typename X, typename WriteType>
auto export_text(const String& path, const X& x, file_format format, WriteType)
{
    WL_TRY_BEGIN()
    decltype(auto) stream = _as_output_file_stream(path);
    char line_sep = '\n';
    char field_sep = (format == file_format::CSV) ? ',' : '\t';
    const auto& valx = allows<view_category::Simple>(x);
    if constexpr (array_rank_v<X> == 0u)
        _export_text_impl<WriteType>(stream, valx, std::array<size_t, 0u>{},
            line_sep, field_sep);
    else if constexpr (array_rank_v<X> <= 2u)
        _export_text_impl<WriteType>(stream, valx, valx.dims(),
            line_sep, field_sep);
    else
        static_assert(always_false_v<X>, WL_ERROR_EXPORT_ARRAY_RANK);
    return stream.path();
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename String, typename ReadType, typename Padding>
auto import_text(const String& path, file_format format, ReadType,
    Padding in_padding)
{
    WL_TRY_BEGIN()
    using namespace stream_separator;
    decltype(auto) stream = _as_input_file_stream(path);
    const separator_table* sep_table_ptr = &default_table;
    switch (format)
    {
    case file_format::TSV:
        sep_table_ptr = &tsv_table; break;
    case file_format::CSV:
        sep_table_ptr = &csv_table; break;
    default:
        sep_table_ptr = &default_table;
    }

    static_assert(is_real_v<ReadType>, WL_ERROR_IMPORT_UNKNOWN_TYPE);
    constexpr auto has_padding = !std::is_same_v<Padding, void_type>;
    auto padding = ReadType{};
    if constexpr (has_padding)
    {
        static_assert(is_convertible_v<Padding, ReadType>,
            WL_ERROR_IMPORT_PADDING_TYPE);
        const ReadType padding = cast<ReadType>(in_padding);
    }

    ndarray<ReadType, 1u> list;
    ndarray<size_t, 1u> num_fields;
    num_fields.append(0u);
    size_t* num_fields_back_ptr = num_fields.end() - 1;
    for (;;)
    {
        using Val = std::conditional_t<is_float_v<ReadType>, double, int64_t>;
        Val val;
        char sep_found;
        auto has_number = stream.read_number(*sep_table_ptr, sep_found, val);
        if (!has_number)
            break;
        list.append(ReadType(val));
        ++(*num_fields_back_ptr);
        if (sep_table_ptr->is_separator(sep_found, EOS))
            break;
        else if (sep_table_ptr->is_separator(sep_found, Line))
        {
            num_fields.append(0u);
            num_fields_back_ptr = num_fields.end() - 1;
        }
    }

    size_t row_size = 0u;
    size_t col_size = 0u;
    bool is_regular = true;
    num_fields.for_each([&](const size_t& n)
        {
            if (n > 0u)
            {
                ++row_size;
                if (col_size == 0u)
                    col_size = n;
                else if (col_size > n)
                    is_regular = false;
                else if (col_size < n)
                    col_size = n;
            }
        });
    const auto ret_dims = std::array<size_t, 2u>{row_size, col_size};
    if (is_regular)
        return ndarray<ReadType, 2u>(ret_dims, std::move(list).data_vector());
    else if constexpr (!has_padding)
        throw std::logic_error(WL_ERROR_IMPORT_NO_PADDING);
    else
    {
        ndarray<ReadType, 2u> ret(ret_dims);
        auto ret_data = ret.data();
        auto list_data = list.data();
        auto num_fields_data = num_fields.data();
        for (size_t i = 0; i < row_size; ++i, ++num_fields_data)
        {
            size_t j = 0;
            for (; j < *num_fields_data; ++j, ++list_data, ++ret_data)
                *ret_data = *list_data;
            for (; j < col_size; ++j, ++ret_data)
                *ret_data = padding;
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}


template<typename String, typename ReadType>
auto import_text(const String& path, file_format format, ReadType)
{
    WL_TRY_BEGIN()
    return import_text(path, format, ReadType{}, const_null);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename String, typename X, typename WriteType>
void export_binary(const String& path, const X& x, WriteType)
{
    WL_TRY_BEGIN()
    decltype(auto) stream = _as_output_file_stream<true>(path);
    write_binary(stream, x, WriteType{});
    return stream.path();
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename String, typename X>
void export_binary(const String& path, const X& x)
{
    WL_TRY_BEGIN()
    decltype(auto) stream = _as_output_file_stream<true>(path);
    write_binary(stream, x);
    return stream.path();
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename String, typename ReadType>
auto import_binary(const String& path, ReadType)
{
    WL_TRY_BEGIN()
    return binary_read_list(path, ReadType{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
