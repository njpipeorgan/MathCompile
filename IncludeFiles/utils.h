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
#include <limits>
#include <memory>
#include <type_traits>
#include <thread>
#include <vector>

#include "macros.h"
#include "types.h"
#include "traits.h"
#include "simd.h"

namespace wl
{

namespace strexp
{

template<typename Expression>
auto compile(Expression);

}

template<typename Any>
auto val(Any&& any) -> decltype(auto)
{
    if constexpr (is_array_view_v<remove_cvref_t<Any>>)
        return std::forward<decltype(any)>(any).to_array();
    else if constexpr (is_pattern_v<remove_cvref_t<Any>>)
        return strexp::compile(std::forward<decltype(any)>(any));
    else
        return std::forward<decltype(any)>(any);
}

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
    else if constexpr (is_real_v<X> || is_string_view_v<X>)
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

template<bool AllowEmpty = false, typename... Dims>
auto get_dims_array(const Dims&... dims)
{
    static_assert(all_is_integral_v<Dims...> && (sizeof...(Dims) >= 1u),
        WL_ERROR_DIMENSIONS_SPEC);
    if (!((dims >= Dims(0)) && ...))
        throw std::logic_error(WL_ERROR_NEGATIVE_DIMS);
    return std::array<size_t, sizeof...(Dims)>{size_t(dims)...};
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
size_t lzcnt_u64(X x)
{
#if defined(__LZCNT__)
    return _lzcnt_u64(uint64_t(x));
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

template<typename X>
size_t tzcnt_u64(X x)
{
#if defined(__BMI__)
    return _tzcnt_u64(uint64_t(x));
#else
    int64_t n = 64;
    uint64_t y = x;
    if constexpr (sizeof(X) >= 8) if (y << 32) { n -= 32; y <<= 32; }
    if constexpr (sizeof(X) >= 4) if (y << 16) { n -= 16; y <<= 16; }
    if constexpr (sizeof(X) >= 2) if (y << 8) { n -= 8; y <<= 8; }
    if (y << 4) { n -= 4; y <<= 4; }
    if (y << 2) { n -= 2; y <<= 2; }
    return n - ((y << 1) ? int64_t(2) : int64_t(y >> 63));
#endif
}

template<typename X>
size_t _popcnt(X x)
{
    static_assert(std::is_unsigned_v<X>, WL_ERROR_INTERNAL);
#if defined(__POPCNT__)
    return _mm_popcnt_u64(uint64_t(x));
#else
    constexpr uint64_t m1  = 0x5555555555555555u;
    constexpr uint64_t m2  = 0x3333333333333333u;
    constexpr uint64_t m4  = 0x0f0f0f0f0f0f0f0fu;
    constexpr uint64_t h01 = 0x0101010101010101u;
    x -= (x >> 1) & m1;
    x = (x & m2) + ((x >> 2) & m2);
    x = (x + (x >> 4)) & m4;
    return size_t((x * h01) >> 56);
#endif
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

extern volatile bool global_abort_in_progress;
extern volatile bool global_stop_check_abort;

template<typename AbortQ>
void start_check_abort(std::unique_ptr<std::thread>& thread, AbortQ* abort_q)
{
#if defined(WL_USE_MATHLINK) && defined(WL_CHECK_ABORT)
    global_stop_check_abort = false;
    global_abort_in_progress = false;
    if (thread)
        return;
    thread = std::make_unique<std::thread>([=]
        {
            for (;;)
            {
                global_abort_in_progress = bool(abort_q());
                std::this_thread::sleep_for(
                    std::chrono::milliseconds(WL_CHECK_ABORT_PERIOD));
                if (global_stop_check_abort || global_abort_in_progress)
                    return;
            }
        });
#endif
}

inline void stop_check_abort(std::unique_ptr<std::thread>& thread)
{
#if defined(WL_USE_MATHLINK) && defined(WL_CHECK_ABORT)
    global_stop_check_abort = true;
    global_abort_in_progress = false;
    if (thread)
        thread.release()->join();
#endif
}

}

#endif

#if defined(WL_USE_MATHLINK) && !defined(NDEBUG)
#  define WL_TRY_BEGIN()                                            \
    try                                                             \
    {
#  define WL_TRY_END(func, file, line)                              \
    } catch (std::logic_error& err)                                 \
    {                                                               \
        throw std::logic_error(err.what() +                         \
            (std::string("\n>> from function \"") + func +          \
            "\" in \"" + wl::librarylink::extract_filename(file) +  \
            "\" line " + std::to_string(line)));                    \
    }
#else
#  define WL_TRY_BEGIN() ((void)0);
#  define WL_TRY_END(...) ((void)0);
#endif


#if defined(WL_USE_MATHLINK) && (defined(WL_CHECK_ABORT) || defined(WL_CHECK_ABORT_TEST))
#  define WL_THROW_IF_ABORT()                                           \
    {                                                                   \
        if (WL_UNLIKELY(wl::librarylink::global_abort_in_progress))     \
            throw std::logic_error("AbortQ is called.");                \
    }
#  define WL_CHECK_ABORT_LOOP_BEGIN(n)                                  \
    const int64_t _loop_total_size = int64_t(n);                        \
    for (int64_t _loop_begin = 0;                                       \
        _loop_begin < _loop_total_size;                                 \
        _loop_begin += WL_CHECK_ABORT_LENGTH)                           \
    {                                                                   \
        constexpr int64_t _loop_zero = int64_t(0);                      \
        const int64_t _loop_size =                                      \
            _loop_begin + WL_CHECK_ABORT_LENGTH > _loop_total_size ?    \
            _loop_total_size - _loop_begin : WL_CHECK_ABORT_LENGTH;     \
        const int64_t _loop_end = _loop_begin + _loop_size;
#  define WL_CHECK_ABORT_LOOP_END()                                     \
        WL_THROW_IF_ABORT()                                             \
    }
#else
#  define WL_THROW_IF_ABORT() ((void)0);
#  define WL_CHECK_ABORT_LOOP_BEGIN(n)                                  \
    {                                                                   \
        constexpr int64_t _loop_zero = int64_t(0);                      \
        const int64_t _loop_size = int64_t(n);                          \
        const int64_t _loop_begin = 0;                                  \
        const int64_t _loop_end = _loop_size;
#  define WL_CHECK_ABORT_LOOP_END()                                     \
    }
#endif

namespace utils
{

template<typename XIter, typename YIter>
WL_INLINE void restrict_copy_n(XIter x_iter, const size_t n, YIter y_iter)
{
    using XV = remove_cvref_t<decltype(*x_iter)>;
    using YV = remove_cvref_t<decltype(*y_iter)>;
    constexpr bool use_memcpy =
        std::is_same_v<XV, YV> && std::is_trivially_copyable_v<XV> &&
        std::is_pointer_v<XIter> && std::is_pointer_v<YIter>;

    if constexpr (use_memcpy)
    {
        std::memcpy((void*)y_iter, (void*)x_iter, n * sizeof(XV));
        WL_THROW_IF_ABORT()
    }
    else
    {
        WL_CHECK_ABORT_LOOP_BEGIN(n)
            WL_IGNORE_DEPENDENCIES
            for (auto i = _loop_zero; i < _loop_size; ++i, ++x_iter, ++y_iter)
                *y_iter = *x_iter;
        WL_CHECK_ABORT_LOOP_END()
    }
}

template<typename XIter, typename YIter>
WL_INLINE void restrict_copy_n(XIter x_iter, const size_t n, YIter y_iter,
    no_check_abort_tag)
{
    using XV = remove_cvref_t<decltype(*x_iter)>;
    using YV = remove_cvref_t<decltype(*y_iter)>;
    constexpr bool use_memcpy =
        std::is_same_v<XV, YV> && std::is_trivially_copyable_v<XV> &&
        std::is_pointer_v<XIter> && std::is_pointer_v<YIter>;

    if constexpr (use_memcpy)
    {
        std::memcpy((void*)y_iter, (void*)x_iter, n * sizeof(XV));
    }
    else
    {
        const auto size = int64_t(n);
        for (int64_t i = 0; i < size; ++i, ++x_iter, ++y_iter)
            *y_iter = *x_iter;
    }
}

}

template<typename X>
auto pause(const X& x)
{
    constexpr uint64_t giga = 1'000'000'000;
    constexpr uint64_t max_sec = std::numeric_limits<uint64_t>::max() / giga;
    static_assert(is_real_v<X>, WL_ERROR_REAL_TYPE_ARG);
    if (x == X(0))
        return const_null;
    else if (x < X(0))
        throw std::logic_error(WL_ERROR_PAUSE_NEGATIVE);

    uint64_t nanosec = 0;
    if constexpr (is_integral_v<X>)
        nanosec = giga * std::min(uint64_t(x), max_sec);
    else
        nanosec = uint64_t(std::min(double(x), double(giga * max_sec)));

    constexpr uint64_t check_abort_nanosec =
        (WL_CHECK_ABORT_PERIOD * (giga / 1000u));
    while (nanosec > check_abort_nanosec)
    {
        WL_THROW_IF_ABORT()
        std::this_thread::sleep_for(
            std::chrono::nanoseconds(check_abort_nanosec));
        nanosec -= check_abort_nanosec;
    }
    WL_THROW_IF_ABORT()
    std::this_thread::sleep_for(std::chrono::nanoseconds(nanosec));
    return const_null;
}

}
