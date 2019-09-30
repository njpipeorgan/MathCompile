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

template<typename T>
auto n(T&& x)
{
    using X = remove_cvref_t<T>;
    WL_NO_STRING(X);

    if constexpr (is_integral_v<X> || is_float_v<X>)
    {
        return double(x);
    }
    else if constexpr (is_complex_v<X>)
    {
        return std::complex<double>(x);
    }
    else if constexpr (is_array_v<X>)
    {
        if constexpr (std::is_same_v<typename X::value_type, double>)
        {
            return std::forward<decltype(x)>(x);
        }
        else
        {
            ndarray<double, X::Rank> y(x.dims());
            std::transform(begin(x), end(x), begin(y), 
                [](auto a) { return n(a); });
            return y;
        }
    }
}

template<typename T>
auto round(T&& x)
{
    using X = remove_cvref_t<T>;
    WL_NO_STRING(X);

    if constexpr (is_integral_v<X>)
    {
        return int64_t(x);
    }
    else if constexpr (is_float_v<X>)
    {
        return int64_t(std::round(x));
    }
    else if constexpr (is_complex_v<X>)
    {
        return std::complex<int64_t>(round(x.real()), round(x.imag()));
    }
    else if constexpr (is_array_v<X>)
    {
        if constexpr (std::is_same_v<typename X::value_type, int64_t>)
        {
            return std::forward<decltype(x)>(x);
        }
        else
        {
            ndarray<int64_t, X::Rank> y(x.dims());
            std::transform(begin(x), end(x), begin(y), 
                [](auto a) { return round(a); });
            return y;
        }
    }

}

#undef WL_NO_STRING
}
