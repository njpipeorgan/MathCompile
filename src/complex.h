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
#include <complex>

#include "types.h"
#include "ndarray.h"
#include "utils.h"
#include "listable.h"

namespace wl
{

template<typename Re, typename Im>
auto make_complex(const Re& re, const Im& im)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Re> && is_real_v<Im>, WL_ERROR_REAL_TYPE_ARG);
    using C = common_type_t<Re, Im>;
    using T = std::conditional_t<std::is_same_v<C, float>, float, double>;
    return complex<T>(T(re), T(im));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto re(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto scalar_re = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x.real();
        else
            return x;
    };
    return utils::listable_function(scalar_re, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto im(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto scalar_im = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x.imag();
        else
            return XV(0);
    };
    return utils::listable_function(scalar_im, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto arg(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
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
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto conjugate(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
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
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto re_im(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>,
        WL_ERROR_NUMERIC_ONLY);
    constexpr auto rank = array_rank_v<XT>;
    if constexpr (rank == 0)
    {
        ndarray<XT, 1u> ret(std::array<size_t, 1u>{2});
        ret[0] = re(x);
        ret[1] = im(x);
    }
    else
    {
        using XV = typename XT::value_type;
        auto dims = utils::dims_join(x.dims(), std::array<size_t, 1u>{2});
        ndarray<XV, rank + 1u> ret(dims);
        auto iter = ret.begin();
        x.for_each([&](const auto& a)
            { *iter++ = re(a), *iter++ = im(a); });
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto abs_arg(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>,
        WL_ERROR_NUMERIC_ONLY);
    constexpr auto rank = array_rank_v<XT>;
    if constexpr (rank == 0)
    {
        using T = decltype(arg(XT{}));
        ndarray<T, 1u> ret(std::array<size_t, 1u>{2});
        ret[0] = T(abs(x));
        ret[1] = T(arg(x));
    }
    else
    {
        using XV = typename XT::value_type;
        using T = decltype(arg(XV{}));
        auto dims = utils::dims_join(x.dims(), std::array<size_t, 1u>{2});
        ndarray<T, rank + 1u> ret(dims);
        auto iter = ret.begin();
        x.for_each([&](const auto& a)
            { *iter++ = T(abs(a)), *iter++ = T(arg(a)); });
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
