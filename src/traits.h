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

template<typename T> struct _type_to_index : std::integral_constant<int, -1> {};
template<> struct _type_to_index<double> : std::integral_constant<int, 1> {};
template<> struct _type_to_index<float> : std::integral_constant<int, 2> {};
template<> struct _type_to_index<uint64_t> : std::integral_constant<int, 3> {};
template<> struct _type_to_index<int64_t> : std::integral_constant<int, 4> {};
template<> struct _type_to_index<uint32_t> : std::integral_constant<int, 5> {};
template<> struct _type_to_index<int32_t> : std::integral_constant<int, 6> {};
template<> struct _type_to_index<uint16_t> : std::integral_constant<int, 7> {};
template<> struct _type_to_index<int16_t> : std::integral_constant<int, 8> {};
template<> struct _type_to_index<uint8_t> : std::integral_constant<int, 9> {};
template<> struct _type_to_index<int8_t> : std::integral_constant<int, 10> {};

template<int I> struct _index_to_type { using type = void; };
template<> struct _index_to_type<1> { using type = double; };
template<> struct _index_to_type<2> { using type = float; };
template<> struct _index_to_type<3> { using type = uint64_t; };
template<> struct _index_to_type<4> { using type = int64_t; };
template<> struct _index_to_type<5> { using type = uint32_t; };
template<> struct _index_to_type<6> { using type = int32_t; };
template<> struct _index_to_type<7> { using type = uint16_t; };
template<> struct _index_to_type<8> { using type = int16_t; };
template<> struct _index_to_type<9> { using type = uint8_t; };
template<> struct _index_to_type<10> { using type = int8_t; };

template<typename T, typename U>
struct common_type : _index_to_type<std::min(_type_to_index<T>::value, _type_to_index<U>::value)> {};

template<typename T, typename U>
using common_type_t = typename common_type<T, U>::type;

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


template<typename T, typename U>
struct is_convertible
{
    static constexpr bool value =
        is_complex_v<U> ||
        is_integral_v<T> ||
        (is_float_v<T> && is_float_v<U>);
};

template<typename T, typename U>
constexpr auto is_convertible_v = is_convertible<T, U>::value;


#define WL_NO_STRING(type) static_assert(!is_string_v<type>, "badargtype")
#define WL_NO_COMPLEX(type) static_assert(!is_complex_v<type>, "badargtype")
#define WL_NO_ARRAY(type) static_assert(!is_array_v<type>, "badargtype")

}
