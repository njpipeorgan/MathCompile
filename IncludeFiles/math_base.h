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

template<size_t Bytes, typename T>
union _vec;

template<typename T>
union _vec<16u, T>
{
    static_assert(is_arithmetic_v<T>, WL_ERROR_INTERNAL);
    using base_type = value_type_t<T>;
    using storage_type =
        std::conditional_t<std::is_same_v<base_type, double>, __m128d,
        std::conditional_t<std::is_same_v<base_type, float>, __m128, __m128i>>;
    storage_type data;
    T values[16u / sizeof(T)];
};

template<size_t Bytes, typename T>
union _vec
{
    static_assert(is_arithmetic_v<T>, WL_ERROR_INTERNAL);
    using base_type = value_type_t<T>;
    using storage_type =
        std::conditional_t<std::is_same_v<base_type, double>, __m256d,
        std::conditional_t<std::is_same_v<base_type, float>, __m256, __m256i>>;
    storage_type data;
    T values[16u / sizeof(T)];
};

template<typename YT, size_t XW, typename XT>
auto _convert()
{
}

//template<typename X, typename Y>
//auto _add(const X& x, const Y& y)
//{
//    using XV = value_type_t<XT>;
//    using YV = value_type_t<YT>;
//    using C = common_type_t<XV, YV>;
//    if constexpr (is_complex_v<XT>)
//    {
//        if constexpr (is_complex_v<YT>)
//            return complex<C>(std::real(x) + std::real(y),
//                std::imag(x) + std::imag(y));
//        else
//            return complex<C>(std::real(x) + cast<C>(y), std::imag(x));
//    }
//    else
//    {
//        if constexpr (is_complex_v<YT>)
//            return complex<C>(cast<C>(x) + std::real(y), std::imag(y));
//        else
//            return cast<C>(cast<C>(x) + cast<C>(y));
//    }
//}

}

