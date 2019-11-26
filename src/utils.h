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

#include <cstring>
#include <chrono>
#include <type_traits>
#include <vector>

#include "macros.h"
#include "types.h"
#include "traits.h"

namespace wl
{

template<typename Y, typename XV, size_t XR>
auto cast(ndarray<XV, XR>&& x) -> decltype(auto)
{
    static_assert(is_convertible_v<ndarray<XV, XR>, Y>, WL_ERROR_BAD_CAST);
    using YV = value_type_t<Y>;
    if constexpr (std::is_same_v<XV, YV>)
        return std::move(x);
    else
    {
        ndarray<YV, XR> ret(x.dims());
        x.copy_to(ret.begin());
        return ret;
    }
}

template<typename Y, typename XV, size_t XR>
auto cast(const ndarray<XV, XR>& x) -> decltype(auto)
{
    static_assert(is_convertible_v<ndarray<XV, XR>, Y>, WL_ERROR_BAD_CAST);
    using YV = value_type_t<Y>;
    if constexpr (std::is_same_v<XV, YV>)
        return x;
    else
    {
        ndarray<YV, XR> ret(x.dims());
        x.copy_to(ret.begin());
        return ret;
    }
}

template<typename Y, typename X>
auto cast(const X& x)
{
    if constexpr (is_array_view_v<X>)
    {
        constexpr auto XR = array_rank_v<X>;
        using XV = value_type_t<X>;
        using YV = value_type_t<Y>;
        static_assert(is_convertible_v<ndarray<XV, XR>, Y>, WL_ERROR_BAD_CAST);
        if constexpr (std::is_same_v<XV, YV>)
            return x.to_array();
        else
        {
            ndarray<YV, XR> ret(x.dims());
            x.copy_to(ret.begin());
            return ret;
        }
    }
    else if constexpr (is_real_v<X>)
    {
        static_assert(is_convertible_v<X, Y>, WL_ERROR_BAD_CAST);
        return Y(x);
    }
    else
    {
        static_assert(std::is_same_v<Y, X>, WL_ERROR_BAD_CAST);
        return x;
    }
}

template<typename Y, typename X>
auto cast(const complex<X>& x)
{
    static_assert(is_complex_v<Y>, WL_ERROR_BAD_CAST);
    return std::complex<value_type_t<Y>>(x);
}

template<typename Y>
auto cast(const boolean& x)
{
    static_assert(is_boolean_v<Y>, WL_ERROR_BAD_CAST);
    return x;
}

template<typename Y>
auto cast(const std::string& x)
{
    static_assert(is_string_v<Y>, WL_ERROR_BAD_CAST);
    return x;
}

template<typename Y>
auto cast(std::string&& x)
{
    static_assert(is_string_v<Y>, WL_ERROR_BAD_CAST);
    return std::move(x);
}

namespace utils
{

inline auto _get_time()
{
    auto now = std::chrono::high_resolution_clock::now();
    return now.time_since_epoch().count();
}

template<size_t R1, size_t R2, size_t... Is1, size_t... Is2>
auto _dims_join_impl(
    const std::array<size_t, R1>& dims1, std::index_sequence<Is1...>,
    const std::array<size_t, R2>& dims2, std::index_sequence<Is2...>)
{
    return std::array<size_t, R1 + R2>{dims1[Is1]..., dims2[Is2]...};
}

template<size_t R1, size_t R2>
auto dims_join(const std::array<size_t, R1>& dims1,
    const std::array<size_t, R2>& dims2)
{
    return _dims_join_impl(dims1, std::make_index_sequence<R1>{},
        dims2, std::make_index_sequence<R2>{});
}

template<size_t... Is>
auto _dims_take_impl(const size_t* dims, std::index_sequence<Is...>)
{
    return std::array<size_t, sizeof...(Is)>{dims[Is]...};
}

// uses one-based indexing, inclusive on both ends
template<size_t I1, size_t I2>
auto dims_take(const size_t* dims)
{
    static_assert(1u <= I1 && 1u <= I2, WL_ERROR_INTERNAL);
    if constexpr (I1 <= I2)
        return _dims_take_impl(dims + I1 - 1,
            std::make_index_sequence<I2 - I1 + 1>{});
    else
        return std::array<size_t, 0u>{};
}

// uses one-based indexing, inclusive on both ends
template<size_t I1, size_t I2, size_t R>
auto dims_take(const std::array<size_t, R>& dims)
{
    if constexpr (I1 > I2)
        return std::array<size_t, 0u>{};
    else
    {
        static_assert(1u <= I1 && I1 <= R && 1u <= I2 && I2 <= R,
            WL_ERROR_INTERNAL);
        return dims_take<I1, I2>(dims.data());
    }
}

template<typename Dims, size_t... Is>
auto _size_of_dims_impl(const Dims* dims, std::index_sequence<Is...>)
{
    if constexpr (std::is_unsigned_v<Dims>)
        return (size_t(dims[Is]) * ...);
    else
    {
#if !defined(NDEBUG)
        if (!((dims[Is] >= 0) && ...))
            throw std::logic_error(WL_ERROR_INTERNAL);
#endif
        return (size_t(dims[Is]) * ...);
    }
}

template<size_t R, typename Dims>
auto size_of_dims(const Dims* dims)
{
    static_assert(is_integral_v<Dims>, WL_ERROR_INTERNAL);
    if constexpr (R == 0u)
        return size_t(1);
    else
        return _size_of_dims_impl(dims, std::make_index_sequence<R>{});
}

template<size_t R, typename Dims>
auto size_of_dims(const std::array<Dims, R>& dims)
{
    static_assert(is_integral_v<Dims>, WL_ERROR_INTERNAL);
    if constexpr (R == 0u)
        return size_t(1);
    else
        return _size_of_dims_impl(dims.data(), std::make_index_sequence<R>{});
}

template<size_t... Is>
bool check_dims_impl(const size_t* dims1, const size_t* dims2,
    std::index_sequence<Is...>)
{
    return ((dims1[Is] == dims2[Is]) && ...);
}

template<size_t R>
auto check_dims(const size_t* dims1, const size_t* dims2)
{
    if constexpr (R == 0u)
        return true;
    else
        return check_dims_impl(dims1, dims2, std::make_index_sequence<R>{});
}

template<size_t R>
auto check_dims(const std::array<size_t, R>& dims1,
    const std::array<size_t, R>& dims2)
{
    static_assert(R >= 1u, WL_ERROR_INTERNAL);
    return check_dims_impl(dims1.data(), dims2.data(),
        std::make_index_sequence<R>{});
}

template<size_t Level, size_t R, typename... Is>
void _linear_position_impl(const std::array<size_t, R>& dims, size_t& pos,
    const size_t& i1, const Is&... is)
{
    pos += i1;
    if constexpr (Level < R - 1)
    {
        pos *= dims[Level + 1u];
        _linear_position_impl<Level + 1u>(dims, pos, is...);
    }
}

template<size_t R, typename... Is>
size_t linear_position(const std::array<size_t, R>& dims, const Is&... is)
{
    static_assert(R == sizeof...(Is), WL_ERROR_INTERNAL);
    size_t pos = 0u;
    _linear_position_impl<0u>(dims, pos, is...);
    return pos;
}

template<typename X>
auto _lzcnt(X x)
{
    static_assert(std::is_unsigned_v<X>, WL_ERROR_INTERNAL);
#if defined(__LZCNT__)
    return _lzcnt_u64(uint64_t(x));
#elif defined(__POPCNT__)
    uint64_t y = int64_t(x);
    y |= (y >> 1);
    y |= (y >> 2);
    y |= (y >> 4);
    if constexpr (sizeof(X) >= 2) y |= (y >> 8);
    if constexpr (sizeof(X) >= 4) y |= (y >> 16);
    if constexpr (sizeof(X) >= 8) y |= (y >> 32);
    return _mm_popcnt_u64(~y);
#else
    int64_t n = 64;
    uint64_t y = x;
    if constexpr (sizeof(X) >= 8) if (y >> 32) { n -= 32; y >>= 32; }
    if constexpr (sizeof(X) >= 4) if (y >> 16) { n -= 16; y >>= 16; }
    if constexpr (sizeof(X) >= 2) if (y >> 8) { n -= 8; y >>= 8; }
    if (y >> 4) { n -= 4; y >>= 4; }
    if (y >> 2) { n -= 2; y >>= 2; }
    return n - ((y >> 1) ? int64_t(2) : int64_t(y));
#endif
}

template<typename XIter, typename YIter>
WL_INLINE void copy_n(XIter x_iter, size_t n, YIter y_iter)
{
    WL_IGNORE_DEPENDENCIES
    for (size_t i = 0u; i < n; ++i, ++x_iter, ++y_iter)
        *y_iter = *x_iter;
}

}

#if defined(WL_USE_MATHLINK)

namespace librarylink
{

template<typename String>
void send_error(const String& what) noexcept;

inline std::string get_stack_message(const std::string& err)
{
    std::string message = "error: " + err;
    return message;
}

inline std::string extract_filename(const char* file)
{
#if defined (_MSC_VER)
    const char separator = '\\';
#else
    const char separator = '/';
#endif
    const char* output = std::strrchr(file, separator);
    return output ? std::string(output + 1) : std::string(file);
}

}

#endif

#if defined(WL_USE_MATHLINK) && !defined(NDEBUG)

#  define WL_TRY_BEGIN()                                        \
    try                                                         \
    {
#  define WL_TRY_END(func, file, line)                          \
    } catch (std::logic_error& err)                             \
    {                                                           \
        throw std::logic_error(err.what() +                     \
            (std::string("\n>> from function \"") + func +      \
            "\" in \"" + librarylink::extract_filename(file) +  \
            "\" line " + std::to_string(line)));                \
    }

#else

#  define WL_TRY_BEGIN() (void)(0);
#  define WL_TRY_END(...) (void)(0);

#endif

}
