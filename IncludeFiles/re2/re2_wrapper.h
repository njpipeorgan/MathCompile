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

namespace re2
{

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
    return std::shared_ptr<re2::RE2>(ptr, re2_deleter);
}

template<typename PieceT,
    typename = std::enable_if<std::is_same<PieceT, re2::StringPiece>::value>>
bool re2_match(
    const re2::RE2& regex, const PieceT& string_piece, size_t startpos,
    size_t endpos, RE2::Anchor re_anchor, PieceT* submatch, int nsubmatch)
{
    return _re2_RE2_match_impl(static_cast<const void*>(&regex),
        static_cast<const void*>(&string_piece), startpos, endpos,
        int(re_anchor), static_cast<void*>(submatch), nsubmatch);
}

}
