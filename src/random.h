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

#include <random>
#include <tuple>

#include "types.h"
#include "arithmetic.h"
#include "ndarray.h"
#include "io.h"

namespace wl
{

extern WL_RANDOM_ENGINE global_random_engine;

namespace distribution
{

constexpr auto _min = [](const auto x, const auto y)
{
    return x < y ? x : y;
};

constexpr auto _max = [](const auto x, const auto y)
{
    return x < y ? y : x;
};

template<typename T>
struct uniform
{
    static_assert(is_real_v<T>, WL_ERROR_INTERNAL);
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

    T operator()()
    {
        return dist_(global_random_engine);
    }
};

template<typename T>
struct uniform<complex<T>>
{
    static_assert(is_float_v<T>, WL_ERROR_INTERNAL);
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

    complex<T> operator()()
    {
        return complex<T>(
            dist_re_(global_random_engine),
            dist_im_(global_random_engine));
    }
};

template<typename T>
struct normal
{
    static_assert(is_float_v<T>, WL_ERROR_INTERNAL);
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

    T operator()()
    {
        return dist_(global_random_engine);
    }
};

}

template<typename Dist, typename... Dims>
auto _random_variate_impl(Dist dist, const Dims&... dims)
{
    using T = typename Dist::value_type;
    constexpr size_t R1 = Dist::rank;
    constexpr size_t R2 = sizeof...(dims);
    constexpr size_t R = R1 + R2;
    static_assert(R1 == 0u, WL_ERROR_OPERAND_RANK);

    if constexpr (R2 == 0u)
        return dist();
    else
    {
        wl::ndarray<T, R> x(utils::get_dims_array(dims...));
        x.for_each([&](auto& v) { v = dist(); });
        return x;
    }
}

template<typename Min, typename Max, typename... Dims>
auto random_integer(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(all_is_integral_v<Min, Max>, WL_ERROR_RANDOM_BOUNDS);
    using T = common_type_t<Min, Max>;
    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Max, typename... Dims>
auto random_integer(const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<Max>, WL_ERROR_RANDOM_BOUNDS);
    return random_integer(Max{}, max, varg_tag{}, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Min, typename Max, typename... Dims>
auto random_real(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Min> && is_real_v<Max>, WL_ERROR_RANDOM_BOUNDS);
    using C = common_type_t<Min, Max>;
    using T = std::conditional_t<is_integral_v<C>, double, C>;
    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Max, typename... Dims>
auto random_real(const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Max>, WL_ERROR_RANDOM_BOUNDS);
    return random_real(Max{}, max, varg_tag{}, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Min, typename Max, typename... Dims>
auto random_complex(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_arithmetic_v<Min> && is_arithmetic_v<Max>,
        WL_ERROR_RANDOM_BOUNDS);
    using C = common_type_t<value_type_t<Min>, value_type_t<Max>>;
    using T = std::conditional_t<std::is_same_v<C, float>,
        complex<float>, complex<double>>;
    auto dist = distribution::uniform<T>(cast<T>(min), cast<T>(max));
    return _random_variate_impl(dist, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Max, typename... Dims>
auto random_complex(const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_arithmetic_v<Max>, WL_ERROR_RANDOM_BOUNDS);
    return random_complex(Max{}, max, varg_tag{}, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Random, typename X, size_t OuterRank>
auto _random_choice_batch_impl(const Random& random, const X& x,
    const std::array<size_t, OuterRank>& outer_dims)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<X>;
    const auto& valx = allows<view_category::Regular>(x);
    const auto x_iter = valx.begin();

    const auto outer_size = utils::size_of_dims(outer_dims);
    if constexpr (XR == 1u)
    {
        ndarray<XV, OuterRank> ret(outer_dims);
        auto ret_iter = ret.data();
        for (size_t i = 0; i < outer_size; ++i, ++ret_iter)
            *ret_iter = *(x_iter + random());
        return ret;
    }
    else
    {
        auto item_dims = utils::dims_take<2u, XR>(x.dims());
        auto item_size = utils::size_of_dims(item_dims);
        ndarray<XV, OuterRank + XR - 1u> ret(
            utils::dims_join(outer_dims, item_dims));
        auto base_iter = ret.data();
        for (size_t i = 0u; i < outer_size; ++i, base_iter += item_size)
            utils::restrict_copy_n(
                x_iter + item_size * random(),
                item_size, base_iter);
        return ret;
    }
}

template<typename Random, typename X>
auto random_choice_single_impl(const Random& random, const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<X>;
    const auto& valx = allows<view_category::Regular>(x);
    const auto x_iter = valx.begin();

    if constexpr (XR == 1u)
    {
        return *(x_iter + random());
    }
    else
    {
        auto item_dims = utils::dims_take<2u, XR>(x.dims());
        auto item_size = utils::size_of_dims(item_dims);
        ndarray<XV, XR - 1u> ret(item_dims);
        utils::restrict_copy_n(
            x_iter + item_size * random(), item_size, ret.data());
        return ret;
    }
}

inline auto _random_choice_prepare_uniform(const size_t x_length)
{
    if (!(x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_ELEM_LENGTH);
    return [max = size_t(x_length - 1u)]
    {
        auto dist = std::uniform_int_distribution<size_t>(0u, max);
        return dist(global_random_engine);
    };
}

template<typename W>
auto _random_choice_prepare_binary(const W& w, const size_t x_length)
{
    static_assert(array_rank_v<W> == 1u && is_real_v<value_type_t<W>>,
        WL_ERROR_RANDOM_WEIGHTS_TYPE);
    if (!(x_length == w.dims()[0] && x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_WEIGHTS_LENGTH);

    const auto& valw = allows<view_category::Regular>(w);
    const auto w_size = valw.size();
    auto ret = ndarray<double, 1u>(valw.dims());
    auto w_iter = valw.begin();
    auto ret_iter = ret.data();
    double accumulate = 0.0;

    WL_IGNORE_DEPENDENCIES
    for (size_t i = 0; i < w_size; ++i, ++w_iter, ++ret_iter)
    {
        const auto weight = double(*w_iter);
        if (weight < 0.)
            throw std::logic_error(WL_ERROR_NEGATIVE_WEIGHT);
        accumulate += weight;
        *ret_iter = accumulate;
    }
    return [weights = std::move(ret), sum = accumulate]
    {
        auto w_begin = weights.begin();
        auto w_end = weights.end();
        auto dist = std::uniform_real_distribution<>(0., sum);
        return size_t(std::lower_bound(
            w_begin, w_end, dist(global_random_engine)) - w_begin);
    };
}

template<typename W>
auto _random_choice_prepare_walker74(const W& w, const size_t x_length)
{
    // Walker, A.J. (1974), Electronics Letters, 10(8), 127
    static_assert(array_rank_v<W> == 1u && is_real_v<value_type_t<W>>,
        WL_ERROR_RANDOM_WEIGHTS_TYPE);
    if (!(x_length == w.dims()[0] && x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_WEIGHTS_LENGTH);

    const size_t n = x_length;
    std::vector<double> p(n);
    w.copy_to(p.data());

    struct alias_t { double prob; uint64_t index; };
    std::vector<alias_t> alias_vec(n);
    auto small_base = new alias_t[n];
    auto large_base = new alias_t[n];
    auto* alias = alias_vec.data();
    auto* small = small_base;
    auto* large = large_base;

    auto push = [](auto*& ptr, alias_t elem) { *(ptr++) = elem; };
    auto pop = [](auto*& ptr) { return *(--ptr); };

    auto normalize = double(n) / std::accumulate(p.cbegin(), p.cend(), 0.0);
    for (uint64_t i = 0; i < n; ++i)
    {
        const double prob = p[i] * normalize;
        push(prob <= 1. ? small : large, alias_t{prob, i});
    }
    while (small > small_base && large > large_base)
    {
        auto l = pop(small);
        auto g = pop(large);
        alias[l.index].prob = l.prob;
        alias[l.index].index = g.index;
        g.prob += l.prob - 1.;
        push(g.prob <= 1. ? small : large, g);
    }
    while (large > large_base)
    {
        alias[pop(large).index].prob = 1.;
    }
    while (small > small_base)
    {
        alias[pop(small).index].prob = 1.;
    }
    delete[] small_base;
    delete[] large_base;

    return[alias_vec = std::move(alias_vec), max = double(n)]
    {
        auto dist = std::uniform_real_distribution<>(0.0, max);
        double rand = dist(global_random_engine);
        double index = std::floor(rand);
        const auto& a = alias_vec[size_t(index)];
        return (rand - index <= a.prob) ? size_t(index) : a.index;
    };
}

template<typename X>
auto random_choice(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return _random_choice_single_impl(
        _random_choice_prepare_uniform(x.dims()[0]), x);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename... Dims>
auto random_choice(const X& x, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return _random_choice_batch_impl(
        _random_choice_prepare_uniform(x.dims()[0]),
        x, utils::get_dims_array(dims...));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename W, typename X>
auto random_choice(const W& w, const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return _random_choice_single_impl(
        _random_choice_prepare_binary(w, x.dims()[0]), x);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename W, typename X, typename... Dims>
auto random_choice(const W& w, const X& x, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    const auto outer_dims = utils::get_dims_array(dims...);
    const auto outer_size = utils::size_of_dims(outer_dims);
    // automatically select between binary and walker74
    if ((outer_size >= 50u) && (outer_size * 5 >= x_length))
        return _random_choice_batch_impl(
            _random_choice_prepare_walker74(w, x_length), x, outer_dims);
    else
        return _random_choice_batch_impl(
            _random_choice_prepare_binary(w, x_length), x, outer_dims);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
