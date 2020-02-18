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

#include <immintrin.h>

#include "macros.h"
#include "traits.h"
#include "types.h"

namespace wl
{

namespace simd
{

#define WL_DEFINE_DEFULAT_SIMD_UNARY(name)                                  \
WL_INLINE auto name(__m256i x) { return _mm256_##name(x); }                 \
WL_INLINE auto name(__m128i x) { return _mm_##name(x); }
#define WL_DEFINE_DEFULAT_SIMD_BINARY(name)                                 \
WL_INLINE auto name(__m256i x, __m256i y) { return _mm256_##name(x, y); }   \
WL_INLINE auto name(__m128i x, __m128i y) { return _mm_##name(x, y); }

WL_DEFINE_DEFULAT_SIMD_BINARY(unpacklo_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(unpackhi_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(add_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(add_epi16)
WL_DEFINE_DEFULAT_SIMD_BINARY(sub_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(sub_epi16)
WL_DEFINE_DEFULAT_SIMD_BINARY(cmpeq_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(cmpgt_epi8)
WL_DEFINE_DEFULAT_SIMD_UNARY(movemask_epi8)

WL_INLINE auto testc(__m128i x, __m128i y) { return _mm_testc_si128(x, y); }
WL_INLINE auto testc(__m256i x, __m256i y) { return _mm256_testc_si256(x, y); }

template<typename M>
WL_INLINE auto zero()
{
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_setzero_si256();
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_setzero_si128();
    else
        static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}
template<typename M, typename T>
WL_INLINE auto set1(T x)
{
    if constexpr (std::is_same_v<M, __m256i>)
    {
        if constexpr (sizeof(T) == 1u) return _mm256_set1_epi8(x);
        else if constexpr (sizeof(T) == 2u) return _mm256_set1_epi16(x);
        else if constexpr (sizeof(T) == 4u) return _mm256_set1_epi32(x);
        else if constexpr (sizeof(T) == 8u) return _mm256_set1_epi64(x);
        else static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
    }
    else if constexpr (std::is_same_v<M, __m128i>)
    {
        if constexpr (sizeof(T) == 1u) return _mm_set1_epi8(x);
        else if constexpr (sizeof(T) == 2u) return _mm_set1_epi16(x);
        else if constexpr (sizeof(T) == 4u) return _mm_set1_epi32(x);
        else if constexpr (sizeof(T) == 8u) return _mm_set1_epi64(x);
        else static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
    }
    else static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}

template<typename M, typename Ptr>
WL_INLINE auto loadu(const Ptr* ptr)
{
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_loadu_si256((const __m256i*)ptr);
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_loadu_si128((const __m128i*)ptr);
    else
        static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}
template<typename M, typename Ptr>
WL_INLINE auto storeu(Ptr* ptr, M x)
{
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_storeu_si256((__m256i*)ptr, x);
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_storeu_si128((__m128i*)ptr, x);
    else
        static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}

template<int I, typename M>
WL_INLINE auto extract_epi32(M x)
{
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_extract_epi32(x, I);
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_extract_epi32(x, I);
    else
        static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}

template<int I, typename M>
WL_INLINE auto extract_epi64(M x)
{
#if defined(_WIN32) && !defined(_WIN64)
    // 32-bit Windows
    const uint64_t lo32 = uint32_t(extract_epi32<2 * I>(x));
    const uint64_t hi32 = uint32_t(extract_epi32<2 * I + 1>(x));
    return (hi32 << 32) | lo32;
#else
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_extract_epi64(x, I);
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_extract_epi64(x, I);
    else
        static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
#endif
}

WL_INLINE int64_t hsum_epi8(__m256i v)
{
    v = add_epi16(unpacklo_epi8(v, zero<__m256i>()),
        unpackhi_epi8(v, zero<__m256i>()));
    auto total = uint64_t(extract_epi64<0>(v) + extract_epi64<1>(v) +
        extract_epi64<2>(v) + extract_epi64<3>(v));
    total += total >> 32;
    total += total >> 16;
    return int64_t(total & 0xffffu);
}
WL_INLINE int64_t hsum_epi8(__m128i v)
{
    v = add_epi16(unpacklo_epi8(v, zero<__m128i>()),
        unpackhi_epi8(v, zero<__m128i>()));
    auto total = uint64_t(extract_epi64<0>(v) + extract_epi64<1>(v));
    total += total >> 32;
    total += total >> 16;
    return int64_t(total & 0xffffu);
}

}

}

