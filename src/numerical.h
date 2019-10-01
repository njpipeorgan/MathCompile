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

#include "types.h"
#include "traits.h"

namespace wl
{

//=============================================================================
// Scalar numerical functions
//=============================================================================

template<typename X>
auto _scalar_n(const X& x)
{
    static_assert(is_arithmetic_v<X>, "badargtype");
    if constexpr (is_integral_v<X> || is_float_v<X>)
    {
        return double(x);
    }
    else // complex
    {
        return complex<double>(x.real(), x.imag());
    }
}

template<typename X>
auto _scalar_round(const X& x)
{
    static_assert(is_arithmetic_v<X>, "badargtype");
    if constexpr (is_integral_v<X>)
    {
        return x;
    }
    else if constexpr (is_float_v<X>)
    {
        return int64_t(std::round(x));
    }
    else // complex
    {
        return complex<int64_t>(
            _scalar_round(x.real()), _scalar_round(x.imag()));
    }
}

template<typename X, typename Y>
auto _scalar_less(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return x < y;
}


//=============================================================================
// Scalar numerical functions
//=============================================================================

template<typename T>
auto n(T&& x)
{
    using X = remove_cvref_t<T>;
    if constexpr (is_arithmetic_v<X>)
    {
        return _scalar_n(x);
    }
    else if constexpr (is_array_v<X>)
    {
        if constexpr (std::is_same_v<typename X::value_type, double>)
        {
            return std::forward<decltype(x)>(x);
        }
        else
        {
            ndarray<double, X::rank> y(x.dims());
            std::transform(x.begin(), x.end(), y.begin(),
                [](auto a) { return _scalar_n(a); });
            return y;
        }
    }
    else
    {
        static_assert(always_false_v<T>, "badargtype");
    }
}

template<typename T>
auto round(T&& x)
{
    using X = remove_cvref_t<T>;
    static_assert(!is_string_v<X>, "badargtype");

    if constexpr (is_arithmetic_v<X>)
    {
        return _scalar_round(x);
    }
    else if constexpr (is_array_v<X>)
    {
        if constexpr (is_integral_v<typename X::value_type>)
        {
            return std::forward<decltype(x)>(x);
        }
        else
        {
            ndarray<double, X::rank> y(x.dims());
            std::transform(x.begin(), x.end(), y.begin(),
                [](auto a) { return _scalar_round(a); });
            return y;
        }
    }
    else
    {
        static_assert(always_false_v<T>, "badargtype");
    }
}

template<typename X, typename Y>
auto less(const X& x, const Y& y)
{
    return _scalar_less(x, y);
}

}
