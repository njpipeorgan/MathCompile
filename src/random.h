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

#include <omp.h>

#include <iostream>
#include <random>

#include "types.h"
#include "traits.h"
#include "arithmetic.h"

namespace wl
{

extern std::default_random_engine global_random_engine;

namespace distribution
{

auto _min = [](const auto& x, const auto& y) { return x < y ? x : y; };
auto _max = [](const auto& x, const auto& y) { return x < y ? y : x; };

template<typename T>
struct uniform
{
    static_assert(is_real_v<T>, "internal");
    using value_type = T;
    static constexpr size_t rank = 0;

    using _dist_type = std::conditional_t<is_integral_v<T>,
        std::uniform_int_distribution<T>,
        std::uniform_real_distribution<T>>;
    _dist_type dist_;

    uniform(T a, T b) :
        dist_(_min(a, b), _max(a, b))
    {
    }

    size_t size() const
    {
        return 1u;
    }

    T operator()() const
    {
        return dist_(global_random_engine);
    }

    template<typename Ptr>
    void operator()(Ptr ptr) const
    {
        *ptr = dist_(global_random_engine);
    }
};

template<typename T>
struct uniform<complex<T>>
{
    static_assert(is_float_v<T>, "internal");
    using value_type = complex<T>;
    static constexpr size_t rank = 0;

    std::uniform_real_distribution<T> dist_re_;
    std::uniform_real_distribution<T> dist_im_;

    uniform(const complex<T>& a, const complex<T>& b) :
        dist_re_(_min(a.real(), b.real()), _max(a.real(), b.real())),
        dist_im_(_min(a.imag(), b.imag()), _max(a.imag(), b.imag()))
    {
    }

    size_t size() const
    {
        return 1u;
    }

    complex<T> operator()() const
    {
        return complex<T>(
            dist_re_(global_random_engine),
            dist_im_(global_random_engine));
    }

    template<typename Ptr>
    void operator()(Ptr ptr) const
    {
        ptr->real() = dist_re_(global_random_engine);
        ptr->imag() = dist_im_(global_random_engine);
    }
};

template<typename T>
struct normal
{
    static_assert(is_float_v<T>, "internal");
    using value_type = T;
    static constexpr size_t rank = 0;

    std::normal_distribution<T> dist_;

    normal(T mean, T stddev) :
        dist_(mean, stddev)
    {
    }

    size_t size() const
    {
        return 1u;
    }

    T operator()() const
    {
        return dist_(global_random_engine);
    }

    template<typename Ptr>
    void operator()(Ptr ptr) const
    {
        *ptr = dist_(global_random_engine);
    }
};

}

template<typename Dist, typename... Dims>
auto _random_variate_impl(const Dist& dist, const Dims&... dims)
{
    using T = typename Dist::value_type;
    constexpr size_t R1 = Dist::rank;
    constexpr size_t R2 = sizeof...(dims);
    constexpr size_t R = R1 + R2;
    static_assert(R1 <= 1, "internal");

    if constexpr (R1 == 0u)
    {
        if constexpr (R2 == 0u)
        {
            return dist();
        }
        else
        {
            wl::ndarray<T, R> x(std::array<int64_t, R>{int64_t(dims)...});
            for (auto& val : x)
            {
                val = dist();
            }
            return x;
        }
    }
    else if constexpr (R2 == 0u)
    {
        size_t dimN = dist.size();
        ndarray<T, 1u> x(std::array<int64_t, 1u>{dimN});
        dist(x.begin());
        return x;
    }
    else
    {
        size_t dimN = dist.size();
        ndarray<T, R> x(std::array<int64_t, R>{int64_t(dims)..., dimN});

        auto x_iter = x.begin();
        auto x_end = x.end();
        for (; x_iter += dimN; x_iter != x_end)
        {
            dist(x_iter);
        }
        return x;
    }
}

template<typename Min, typename Max, typename... Dims>
auto random_integer(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_integral_v<Min> && is_integral_v<Max>, "badargtype");
    using T = decltype(plus(min, max));

    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
}

template<typename Max, typename... Dims>
auto random_integer(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_integral_v<Max>, "badargtype");
    return random_integer(Max(0), max, varg_tag{}, dims...);
}

template<typename Min, typename Max, typename... Dims>
auto random_real(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_real_v<Min> && is_real_v<Max>, "badargtype");
    using T = decltype(plus(plus(min, max), float{}));

    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
}

template<typename Max, typename... Dims>
auto random_real(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_real_v<Max>, "badargtype");
    return random_real(Max(0), max, varg_tag{}, dims...);
}

template<typename Min, typename Max, typename... Dims>
auto random_complex(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_arithmetic_v<Min> && is_arithmetic_v<Max>, "badargtype");
    using C = decltype(plus(min, max));
    using T = std::conditional_t<std::is_same_v<C, complex<float>>, complex<float>, complex<double>>;

    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
}

template<typename Max, typename... Dims>
auto random_complex(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_arithmetic_v<Max>, "badargtype");
    return random_complex(Max{}, max, varg_tag{}, dims...);
}

}
