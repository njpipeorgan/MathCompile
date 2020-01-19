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

#include <memory>
#include <type_traits>

#include "re2/re2.h"

extern "C" void* _re2_RE2_construct_impl(const char* pattern);

extern "C" void _re2_RE2_destruct_impl(void* object_ptr);

extern "C" bool _re2_RE2_match_impl(const void* regex_ptr,
    const void* piece_ptr, size_t startpos, size_t endpos, int re_anchor,
    void* submatch, int nsubmatch);

extern "C" bool _re2_RE2_ok(const void* regex_ptr);

extern "C" void _re2_RE2_error(const void* regex_ptr, char* buffer,
    const size_t buffer_size);

namespace re2
{

template<typename Regex>
bool re2_ok(const Regex& regex)
{
    return _re2_RE2_ok(static_cast<const void*>(&regex));
}

template<typename Regex>
std::string re2_error(const Regex& regex)
{
    if (re2_ok(regex))
    {
        return std::string();
    }
    else
    {
        constexpr size_t buffer_size = 256u;
        char buffer[buffer_size];
        _re2_RE2_error(static_cast<const void*>(&regex), buffer, buffer_size);
        return std::string(buffer);
    }
}

template<typename CharT,
    typename = typename std::enable_if<sizeof(CharT) == 1u>::type>
std::shared_ptr<re2::RE2> re2_new(const CharT* pattern)
{
    auto re2_deleter = [](re2::RE2* ptr)
    {
        _re2_RE2_destruct_impl(static_cast<void*>(ptr));
    };
    auto ptr = static_cast<re2::RE2*>(
        _re2_RE2_construct_impl(static_cast<const char*>(pattern)));
    if (!ptr)
        throw std::bad_alloc();
    if (!re2_ok(*ptr))
        throw std::logic_error(re2_error(*ptr));
    return std::shared_ptr<re2::RE2>(ptr, re2_deleter);
}

template<typename Regex,
    typename = std::enable_if<std::is_same<Regex, re2::RE2>::value>>
bool re2_match(
    const Regex& regex, const StringPiece& string_piece, size_t startpos,
    size_t endpos, RE2::Anchor re_anchor, StringPiece* submatch, int nsubmatch)
{
    return _re2_RE2_match_impl(static_cast<const void*>(&regex),
        static_cast<const void*>(&string_piece), startpos, endpos,
        int(re_anchor), static_cast<void*>(submatch), nsubmatch);
}

}
