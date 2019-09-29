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

#include <type_traits>

namespace wl
{

template<typename T, size_t R>
struct ndarray;

template<typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

template<typename T>
constexpr auto is_integral_v = std::is_integral_v<T>;

template<typename T>
constexpr auto is_float_v = std::is_floating_point_v<T>;

template<typename T>
constexpr auto is_string_v = std::is_same_v<T, std::string>;

template<typename T>
struct is_complex : std::false_type {};
template<typename T>
struct is_complex<std::complex<T>> : std::true_type {};
template<typename T>
constexpr auto is_complex_v = is_complex<T>::value;

template<typename T>
constexpr auto is_arithmetic_v = is_integral_v<T> || is_float_v<T> || is_complex_v<T>;

template<typename T>
struct is_array : std::false_type {};
template<typename T, size_t R>
struct is_array<ndarray<T, R>> : std::true_type {};
template<typename T>
constexpr auto is_array_v = is_array<T>::value;

}
