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

#include <cmath>

#include "types.h"
#include "traits.h"

namespace wl
{

template<typename Re, typename Im>
auto make_complex(const Re& re, const Im& im)
{
    static_assert(is_real_v<Re> && is_real_v<Im>, "badargtype");
    using C = common_type_t<Re, Im>;
    using T = std::conditional_t<std::is_same_v<C, float>, float, double>;
    return complex<T>(T(re), T(im));
}

//=============================================================================
// Scalar numerical complex functions
//=============================================================================

template<typename X>
auto _scalar_re(const X& x)
{
    static_assert(is_arithmetic_v<X>, "badargtype");
    if constexpr (is_complex_v<X>)
    {
        return x.real();
    }
    else
    {
        return x;
    }
}

template<typename X>
auto _scalar_im(const X& x)
{
    static_assert(is_arithmetic_v<X>, "badargtype");
    if constexpr (is_complex_v<X>)
    {
        return x.imag();
    }
    else
    {
        return X(0);
    }
}

template<typename X>
auto _scalar_abs(const X& x)
{
    static_assert(is_arithmetic_v<X>, "badargtype");
    if constexpr (is_complex_v<X>)
    {
        return std::abs(x);
    }
    else if constexpr (std::is_unsigned_v<X>)
    {
        return x;
    }
    else
    {
        return x >= X(0) ? x : -x;
    }
}

template<typename X>
auto _scalar_arg(const X& x)
{
    static_assert(is_arithmetic_v<X>, "badargtype");
    if constexpr (is_complex_v<X>)
    {
        return std::arg(x);
    }
    else if constexpr (is_integral_v<X>)
    {
        if constexpr (std::is_unsigned_v<X>)
        {
            return double(0.0);
        }
        else
        {
            return x >= X(0) ? double(0) : const_pi;
        }
    }
    else // float/double
    {
        return x >= X(0) ? X(0) : X(const_pi);
    }
}

template<typename X>
auto _scalar_conjugate(const X& x)
{
    static_assert(is_arithmetic_v<X>, "badargtype");
    if constexpr (is_complex_v<X>)
    {
        return std::conj(x);
    }
    else
    {
        return x;
    }
}

//=============================================================================
// Array numerical complex functions
//=============================================================================


template<typename T>
auto re(T&& x)
{
    using X = remove_cvref_t<T>;
    if constexpr (is_arithmetic_v<X>)
    {
        return _scalar_re(x);
    }
    else if constexpr (is_array_v<X>)
    {
        using ValueType = typename X::value_type;
        if constexpr (is_real_v<ValueType>)
        {
            return std::forward<decltype(x)>(x);
        }
        else
        {
            ndarray<typename ValueType::value_type, X::rank> y(x.dims());
            std::transform(x.begin(), x.end(), y.begin(),
                [](auto a) { return _scalar_re(a); });
            return y;
        }
    }
    else
    {
        static_assert(always_false_v<T>, "badargtype");
    }
}

template<typename T>
auto im(const T& x)
{
    using X = remove_cvref_t<T>;
    if constexpr (is_arithmetic_v<X>)
    {
        return _scalar_im(x);
    }
    else if constexpr (is_array_v<X>)
    {
        using ValueType = typename X::value_type;
        if constexpr (is_real_v<ValueType>)
        {
            ndarray<ValueType, X::rank> y(x.dims(), ValueType(0));
            return y;
        }
        else
        {
            ndarray<typename ValueType::value_type, X::rank> y(x.dims());
            std::transform(x.begin(), x.end(), y.begin(),
                [](auto a) { return _scalar_im(a); });
            return y;
        }
    }
    else
    {
        static_assert(always_false_v<T>, "badargtype");
    }
}

template<typename T>
auto abs(T&& x)
{
    using X = remove_cvref_t<T>;
    if constexpr (is_arithmetic_v<X>)
    {
        return _scalar_abs(x);
    }
    else if constexpr (is_array_v<X>)
    {
        using ValueType = typename X::value_type;
        if constexpr (is_real_v<ValueType>)
        {
            if constexpr (std::is_unsigned_v<ValueType>)
            {
                return std::forward<decltype(x)>(x);
            }
            else // signed integer or float/double
            {
                ndarray<ValueType, X::rank> y(x);
                for (ValueType& val : y)
                {
                    if (val < ValueType(0))
                        val = -val;
                }
                return y;
            }
        }
        else
        {
            ndarray<typename ValueType::value_type, X::rank> y(x.dims());
            std::transform(x.begin(), x.end(), y.begin(),
                [](auto a) { return _scalar_abs(a); });
            return y;
        }
    }
    else
    {
        static_assert(always_false_v<T>, "badargtype");
    }
}

template<typename T>
auto arg(T&& x)
{
    using X = remove_cvref_t<T>;
    if constexpr (is_arithmetic_v<X>)
    {
        return _scalar_arg(x);
    }
    else if constexpr (is_array_v<X>)
    {
        using ValueType = typename X::value_type;
        using ResultType = decltype(_scalar_arg(ValueType{}));
        if constexpr (is_real_v<ValueType>)
        {
            ndarray<ResultType, X::rank> y(x.dims());
            if constexpr (!std::is_unsigned_v<ValueType>)
            {
                for (size_t i = 0; i < x.size(); ++i)
                {
                    if (x[i] < ValueType(0))
                        y[i] = ResultType(const_pi);
                }
            }
            return y;
        }
        else
        {
            ndarray<typename ValueType::value_type, X::rank> y(x.dims());
            std::transform(x.begin(), x.end(), y.begin(),
                [](auto a) { return _scalar_arg(a); });
            return y;
        }
    }
    else
    {
        static_assert(always_false_v<T>, "badargtype");
    }
}

template<typename T>
auto conjugate(T&& x)
{
    using X = remove_cvref_t<T>;
    if constexpr (is_arithmetic_v<X>)
    {
        return _scalar_conjugate(x);
    }
    else if constexpr (is_array_v<X>)
    {
        using ValueType = typename X::value_type;
        if constexpr (is_real_v<ValueType>)
        {
            return std::forward<decltype(x)>(x);
        }
        else
        {
            ndarray<ValueType, X::rank> y(x.dims());
            std::transform(x.begin(), x.end(), y.begin(),
                [](auto a) { return _scalar_conjugate(a); });
            return y;
        }
    }
    else
    {
        static_assert(always_false_v<T>, "badargtype");
    }
}

}
