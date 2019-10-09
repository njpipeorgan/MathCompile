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
#include "utils.h"

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


template<typename X>
auto re(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_re = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x.real();
        else
            return x;
    };
    return utils::listable_function(scalar_re, std::forward<decltype(x)>(x));
}

template<typename X>
auto im(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_im = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x.imag();
        else
            return XV(0);
    };
    return utils::listable_function(scalar_im, std::forward<decltype(x)>(x));
}

template<typename X>
auto arg(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_arg = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return std::arg(x);
        else if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return double(0);
            else
                return x >= X(0) ? double(0) : const_pi;
        }
        else
            return x >= X(0) ? X(0) : X(const_pi);
    };
    return utils::listable_function(scalar_arg, std::forward<decltype(x)>(x));
}

template<typename X>
auto conjugate(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_conjugate = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return std::conj(x);
        else
            return x;
    };
    return utils::listable_function(scalar_conjugate, 
        std::forward<decltype(x)>(x));
}

}
