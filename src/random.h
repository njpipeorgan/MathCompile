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

#include <algorithm>
#include <random>
#include <tuple>

#include "types.h"
#include "arithmetic.h"
#include "ndarray.h"
#include "io.h"

namespace wl
{

struct random_engine
{
    using _base_engine_t = WL_BASE_RANDOM_ENGINE;
    static_assert(uint64_t(_base_engine_t::max()) -
        uint64_t(_base_engine_t::min()) == uint64_t(-1),
        "64-bit random engine is required.");
    static constexpr size_t bits_width = 64u;
    using result_type = uint64_t;

    _base_engine_t base_engine_{};
    uint64_t active_bits_{};
    uint64_t reserved_bits_{};
    size_t n_bits_active_{};

    random_engine()
    {
        update_all_bits();
    }

    static constexpr auto max()
    {
        return _base_engine_t::max();
    }

    static constexpr auto min()
    {
        return _base_engine_t::min();
    }

    WL_INLINE auto base_engine_get_bits()
    {
        return uint64_t(base_engine_());
    }

    auto get_bits(size_t n)
    {
        if (n >= 64u)
            return base_engine_get_bits();
        else if (n > 32u)
            return base_engine_get_bits() >> (64u - n);
        uint64_t ret = active_bits_ >> (bits_width - n);
        if (n <= n_bits_active_)
        {
            active_bits_ <<= n;
            n_bits_active_ -= n;
        }
        else
        {
            size_t n_bits_short = n - n_bits_active_;
            ret |= reserved_bits_ >> (bits_width - n_bits_short);
            active_bits_ = reserved_bits_ << n_bits_short;
            n_bits_active_ = (bits_width - n_bits_short);
            reserved_bits_ = base_engine_get_bits();
        }
        return ret;
    }

    void update_all_bits()
    {
        active_bits_ = base_engine_get_bits();
        reserved_bits_ = base_engine_get_bits();
        n_bits_active_ = bits_width;
    }

    template<typename Any>
    void seed(Any&& any)
    {
        base_engine_.seed(std::forward<decltype(any)>(any));
        update_all_bits();
    }

    WL_INLINE auto operator()()
    {
        return base_engine_get_bits();
    }
};

extern random_engine global_random_engine;

//extern WL_BASE_RANDOM_ENGINE global_random_engine;

namespace distribution
{

template<typename T>
void adjust_bounds(T& min, T& max)
{
    if (min > max)
        std::swap(min, max);
}

template<typename T>
void adjust_bounds(complex<T>& min, complex<T>& max)
{
    auto min_real = min.real();
    auto min_imag = min.imag();
    auto max_real = max.real();
    auto max_imag = max.imag();
    adjust_bounds(min_real, max_real);
    adjust_bounds(min_imag, max_imag);
    min = complex<T>(min_real, min_imag);
    max = complex<T>(max_real, max_imag);
}

template<typename T, bool Multiple = false>
struct uniform
{
    static_assert(is_real_v<T>, WL_ERROR_INTERNAL);
    using value_type = T;
    static constexpr size_t rank = 0;

    T min_;
    T max_;
    uint64_t range_{};
    size_t n_bits_{};

    uniform(T a, T b) : min_{a}, max_{b}
    {
        adjust_bounds(min_, max_);
        if constexpr (is_integral_v<T> && Multiple)
        {
            const auto diff = uint64_t(max_) - uint64_t(min_);
            n_bits_ = size_t(64u - utils::_lzcnt(diff));
            range_ = diff + 1u; // could be zero
        }
    }

    T generate_int()
    {
        if (range_ == 0u)
            return T(global_random_engine());
        if constexpr (Multiple)
        {
            for (;;)
            {
                uint64_t rand = global_random_engine.get_bits(n_bits_);
                if (rand <= uint64_t(range_ - 1u))
                    return T(rand + std::make_unsigned_t<T>(min_));
            }
        }
        else
        {
            for (;;)
            {
                uint64_t rand = global_random_engine();
                uint64_t mask = uint64_t(-1);
                if (rand / range_ < mask / range_ ||
                    mask % range_ == (range_ - 1u)) {
                    return T((rand % range_) + std::make_unsigned_t<T>(min_));
                }
            }
        }
    }

    void generate(T* res)
    {
        if constexpr (is_integral_v<T>)
            *res = generate_int();
        else
            *res = std::uniform_real_distribution<T>(
                min_, max_)(global_random_engine);
    }
};

template<typename T, bool Multiple>
struct uniform<complex<T>, Multiple>
{
    static_assert(is_float_v<T>, WL_ERROR_INTERNAL);
    using value_type = complex<T>;
    static constexpr size_t rank = 0;

    complex<T> min_;
    complex<T> max_;

    uniform(const complex<T>& a, const complex<T>& b) : min_{a}, max_{b}
    {
        adjust_bounds(min_, max_);
    }

    void generate(complex<T>* res)
    {
        using dist_type = std::uniform_real_distribution<T>;
        auto re_dist = dist_type(min_.real(), max_.real());
        auto im_dist = dist_type(min_.imag(), max_.imag());
        *res = complex<T>(re_dist(global_random_engine),
            im_dist(global_random_engine));
    }
};

template<typename T>
struct multi_uniform
{
    static_assert(is_arithmetic_v<T>, WL_ERROR_INTERNAL);

    using value_type = T;
    static constexpr size_t rank = 1;

    ndarray<T, 2u> bounds_;

    multi_uniform(const ndarray<T, 2u>& bounds) : bounds_(bounds)
    {
        _initialize();
    }

    multi_uniform(ndarray<T, 2u>&& bounds) : bounds_(std::move(bounds))
    {
        _initialize();
    }

    void _initialize()
    {
        if (!(bounds_.dims()[0] >= 1u && bounds_.dims()[1] == 2u))
            throw std::logic_error(WL_ERROR_INTERNAL);
        const auto length = bounds_.dims()[0];
        auto iter = bounds_.data();
        for (size_t i = 0; i < length; ++i, iter += 2)
            adjust_bounds(iter[0], iter[1]);
    }

    size_t length() const
    {
        return bounds_.dims()[0];
    }

    void _single_generate(T* res, T* bounds)
    {
        if constexpr (is_complex_v<T>)
        {
            using dist_type = std::uniform_real_distribution<T>;
            auto re_dist = dist_type(bounds[0].real(), bounds[1].real());
            auto im_dist = dist_type(bounds[0].imag(), bounds[1].imag());
            res->real() = re_dist(global_random_engine);
            res->imag() = im_dist(global_random_engine);
        }
        else
        {
            using dist_type = std::conditional_t<is_integral_v<T>,
                std::uniform_int_distribution<T>,
                std::uniform_real_distribution<T>>;
            auto dist = dist_type(bounds[0], bounds[1]);
            *res = dist(global_random_engine);
        }
    }

    void generate(T* res)
    {
        const auto length = this->length();
        auto bounds_ptr = bounds_.data();
        for (size_t i = 0; i < length; ++i, ++res, bounds_ptr += 2)
            _single_generate(res, bounds_ptr);
    }
};

template<typename T>
struct default_multi_uniform
{
    static_assert(is_float_v<T>, WL_ERROR_INTERNAL);

    using value_type = T;
    static constexpr size_t rank = 1;

    size_t length_;

    default_multi_uniform(size_t length) : length_{length}
    {
        if (!(length >= 1u))
            throw std::logic_error(WL_ERROR_INTERNAL);
    }

    size_t length() const
    {
        return length_;
    }

    void generate(T* res)
    {
        for (size_t i = 0; i < length_; ++i, ++res)
        {
            auto dist = std::uniform_real_distribution<T>();
            *res = dist(global_random_engine);
        }
    }
};

template<typename RT, template<typename> typename Dist, typename... Params>
struct scalar_distribution
{
    using value_type = RT;
    static constexpr size_t rank = 0;

    Dist<RT> dist_;

    scalar_distribution(const Params&... params) : dist_(params...)
    {
    }

    void generate(RT* res)
    {
        *res = dist_(global_random_engine);
    }
};

#define WL_DEFINE_DISTRIBUTION_TYPE(name, stdname)          \
template<typename ReturnType, typename... Params>           \
using name = scalar_distribution<                           \
    ReturnType, std::stdname##_distribution, Params...>;

WL_DEFINE_DISTRIBUTION_TYPE(log_normal, lognormal)
WL_DEFINE_DISTRIBUTION_TYPE(normal, normal)
WL_DEFINE_DISTRIBUTION_TYPE(chi_square, chi_squared)
WL_DEFINE_DISTRIBUTION_TYPE(cauchy, cauchy)
WL_DEFINE_DISTRIBUTION_TYPE(student_t_nu, student_t)
WL_DEFINE_DISTRIBUTION_TYPE(f_ratio, fisher_f)
WL_DEFINE_DISTRIBUTION_TYPE(exponential, exponential)
WL_DEFINE_DISTRIBUTION_TYPE(poisson, poisson)
WL_DEFINE_DISTRIBUTION_TYPE(gamma, gamma)
WL_DEFINE_DISTRIBUTION_TYPE(weibull, weibull)
WL_DEFINE_DISTRIBUTION_TYPE(extreme_value, extreme_value)
WL_DEFINE_DISTRIBUTION_TYPE(geometric, geometric)
WL_DEFINE_DISTRIBUTION_TYPE(binomial, binomial)
WL_DEFINE_DISTRIBUTION_TYPE(negative_binomial, negative_binomial)

template<typename RT, typename Mu, typename Sigma, typename Nu>
struct student_t
{
    using value_type = RT;
    static constexpr size_t rank = 0;

    RT mu_;
    RT sigma_;
    std::student_t_distribution<RT> dist_;

    student_t(const Mu& mu, const Sigma& sigma, const Nu& nu) :
        mu_{RT(mu)}, sigma_{RT(sigma)}, dist_(nu)
    {
    }

    void generate(RT* res)
    {
        *res = dist_(global_random_engine) * sigma_ + mu_;
    }
};

template<typename RT, typename P>
struct bernoulli
{
    using value_type = RT;
    static constexpr size_t rank = 0;

    double p_;

    bernoulli(const P& p) : p_{double(p)}
    {
    }

    void generate(RT* res)
    {
        double rand = std::uniform_real_distribution<>()(global_random_engine);
        *res = boolean(rand < double(p_));
    }
};

}

inline auto uniform_distribution()
{
    return distribution::uniform<double>(0., 1.);
}

template<typename X>
auto uniform_distribution(const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR <= 2u, WL_ERROR_UNIFORM_BOUNDS_SPEC);
    
    if constexpr (XR == 0u)
    {
        static_assert(is_integral_v<X>, WL_ERROR_UNIFORM_BOUNDS_SPEC);
        if (x <= X(0))
            throw std::logic_error(WL_ERROR_UNIFORM_BOUNDS_SPEC);
        return distribution::default_multi_uniform<double>(x);
    }
    else
    {
        using XV = value_type_t<X>;
        static_assert(is_arithmetic_v<XV>, WL_ERROR_UNIFORM_BOUNDS_SPEC);
        using RV = promote_integral_t<XV>;

        if constexpr (XR == 1u)
        {
            if (x.dims()[0] != 2u)
                throw std::logic_error(WL_ERROR_UNIFORM_BOUNDS_SPEC);
            std::array<RV, 2u> bounds;
            x.copy_to(bounds.data());
            return distribution::uniform<RV>(bounds[0], bounds[1]);
        }
        else // XR == 2u
        {
            static_assert(is_arithmetic_v<XV>, WL_ERROR_UNIFORM_BOUNDS_SPEC);
            if (!(x.dims()[0] >= 1u && x.dims()[1] == 2u))
                throw std::logic_error(WL_ERROR_UNIFORM_BOUNDS_SPEC);
            auto bounds = cast<ndarray<RV, 2u>>(x);
            return distribution::multi_uniform<RV>(bounds);
        }
    }
}

template<typename Nu>
auto chi_square_distribution(const Nu& nu)
{
    static_assert(is_real_v<Nu>, WL_ERROR_REAL_TYPE_ARG);
    if (!(nu > Nu(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::chi_square<promote_integral_t<Nu>, Nu>(nu);
}

template<typename Mean, typename Dev>
auto normal_distribution(const Mean& mean, const Dev& dev)
{
    static_assert(all_is_real_v<Mean, Dev>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Mean, Dev>>;
    if (!(dev > Dev(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::normal<P, Mean, Dev>(mean, dev);
}

inline auto normal_distribution()
{
    return normal_distribution(0., 1.);
}

template<typename Mean, typename Dev>
auto log_normal_distribution(const Mean& mean, const Dev& dev)
{
    static_assert(all_is_real_v<Mean, Dev>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Mean, Dev>>;
    if (!(dev > Dev(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::log_normal<P, Mean, Dev>(mean, dev);
}

template<typename A, typename B>
auto cauchy_distribution(const A& a, const B& b)
{
    static_assert(all_is_real_v<A, B>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<A, B>>;
    if (!(b > B(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::cauchy<P, A, B>(a, b);
}

inline auto cauchy_distribution()
{
    return cauchy_distribution(0., 1.);
}

template<typename Mean, typename Scale, typename Nu>
auto student_t_distribution(const Mean& mean, const Scale& scale, const Nu& nu)
{
    static_assert(all_is_real_v<Mean, Scale, Nu>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Mean, Scale, Nu>>;
    if (!(scale > Scale(0) && nu > Nu(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::student_t<P, Mean, Scale, Nu>(mean, scale, nu);
}

template<typename Nu>
auto student_t_distribution(const Nu& nu)
{
    static_assert(is_real_v<Nu>, WL_ERROR_REAL_TYPE_ARG);
    if (!(nu > Nu(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::student_t_nu<promote_integral_t<Nu>, Nu>(nu);
}

template<typename N, typename M>
auto f_ratio_distribution(const N& n, const M& m)
{
    static_assert(all_is_real_v<N, M>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<N, M>>;
    if (!(n > N(0) && m > M(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::f_ratio<P, N, M>(n, m);
}

template<typename Mean>
auto poisson_distribution(const Mean& mean)
{
    static_assert(is_real_v<Mean>, WL_ERROR_REAL_TYPE_ARG);
    if (!(mean > Mean(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::poisson<int64_t, Mean>(mean);
}

template<typename Lambda>
auto exponential_distribution(const Lambda& lambda)
{
    static_assert(is_real_v<Lambda>, WL_ERROR_REAL_TYPE_ARG);
    if (!(lambda > Lambda(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::exponential<
        promote_integral_t<Lambda>, Lambda>(lambda);
}

template<typename P>
auto bernoulli_distribution(const P& p)
{
    static_assert(is_real_v<P>, WL_ERROR_REAL_TYPE_ARG);
    if (!(P(0) <= p && p <= P(1)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::bernoulli<boolean, P>(p);
}

template<typename Alpha, typename Beta>
auto gamma_distribution(const Alpha& alpha, const Beta& beta)
{
    static_assert(all_is_real_v<Alpha, Beta>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Alpha, Beta>>;
    if (!(alpha > Alpha(0) && beta > Beta(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::gamma<P, Alpha, Beta>(alpha, beta);
}

template<typename Alpha, typename Beta>
auto weibull_distribution(const Alpha& alpha, const Beta& beta)
{
    static_assert(all_is_real_v<Alpha, Beta>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Alpha, Beta>>;
    if (!(alpha > Alpha(0) && beta > Beta(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::weibull<P, Alpha, Beta>(alpha, beta);
}

template<typename Alpha, typename Beta>
auto extreme_value_distribution(const Alpha& alpha, const Beta& beta)
{
    static_assert(all_is_real_v<Alpha, Beta>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Alpha, Beta>>;
    if (!(beta > Beta(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::extreme_value<P, Alpha, Beta>(alpha, beta);
}

template<typename P>
auto geometric_distribution(const P& p)
{
    static_assert(is_real_v<P>, WL_ERROR_REAL_TYPE_ARG);
    if (!(P(0) < p && p < P(1)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::geometric<int64_t, P>(p);
}

template<typename N, typename P>
auto binomial_distribution(const N& n, const P& p)
{
    static_assert(is_integral_v<N>, WL_ERROR_INTEGRAL_TYPE_ARG);
    static_assert(is_real_v<P>, WL_ERROR_REAL_TYPE_ARG);
    if (!(n >= N(0) && P(0) <= p && p <= P(1)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::binomial<N, N, P>(n, p);
}

template<typename N, typename P>
auto negative_binomial_distribution(const N& n, const P& p)
{
    static_assert(is_integral_v<N>, WL_ERROR_INTEGRAL_TYPE_ARG);
    static_assert(is_real_v<P>, WL_ERROR_REAL_TYPE_ARG);
    if (!(n > N(0) && P(0) < p && p <= P(1)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::negative_binomial<N, N, P>(n, p);
}


template<typename Dist, typename... Dims>
auto _random_variate_impl(Dist dist, const Dims&... dims)
{
    using T = typename Dist::value_type;
    constexpr size_t R1 = Dist::rank;
    constexpr size_t R2 = sizeof...(dims);
    static_assert(R1 <= 1u, WL_ERROR_INTERNAL);

    if constexpr (R2 == 0u)
    {
        if constexpr (R1 == 0u)
        {
            T ret;
            dist.generate(&ret);
            return ret;
        }
        else
        {
            ndarray<T, 1u> ret(std::array<size_t, 1u>{dist.length()});
            dist.generate(ret.data());
            return ret;
        }
    }
    else
    {
        if constexpr (R1 == 0u)
        {
            wl::ndarray<T, R2> ret(utils::get_dims_array(dims...));
            auto iter = ret.data();
            WL_CHECK_ABORT_LOOP_BEGIN(ret.size())
                for (auto i = _loop_zero; i < _loop_size; ++i, ++iter)
                    dist.generate(iter);
            WL_CHECK_ABORT_LOOP_END()
            return ret;
        }
        else
        {
            const auto length = dist.length();
            wl::ndarray<T, R2 + 1u> ret(
                utils::get_dims_array(dims..., length));
            const auto outer_size = utils::size_of_dims<R2>(ret.dims().data());
            auto iter = ret.data();
            WL_CHECK_ABORT_LOOP_BEGIN(outer_size)
                for (auto i = _loop_zero; i < _loop_size; ++i, iter += length)
                    dist.generate(iter);
            WL_CHECK_ABORT_LOOP_END()
            return ret;
        }
    }
}

template<typename Dist, typename... Dims>
auto random_variate(Dist dist, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    return _random_variate_impl(dist, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Min, typename Max, typename... Dims>
auto random_integer(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(all_is_integral_v<Min, Max>, WL_ERROR_RANDOM_BOUNDS);
    using T = common_type_t<Min, Max>;
    auto dist = distribution::uniform<T, true>(T(min), T(max));
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
    static_assert(all_is_real_v<Min, Max>, WL_ERROR_RANDOM_BOUNDS);
    using P = promote_integral_t<common_type_t<Min, Max>>;
    auto dist = distribution::uniform<P>(min, max);
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
auto _random_choice_batch_impl(Random&& random, const X& x,
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

    double normalize = 0.0;
    for (const auto& prob : p)
    {
        if (prob < 0.)
            throw std::logic_error(WL_ERROR_NEGATIVE_WEIGHT);
        normalize += prob;
    }
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
    if ((outer_size >= 50u) && (outer_size * 5u >= x_length))
        return _random_choice_batch_impl(
            _random_choice_prepare_walker74(w, x_length), x, outer_dims);
    else
        return _random_choice_batch_impl(
            _random_choice_prepare_binary(w, x_length), x, outer_dims);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

inline auto _random_sample_prepare_uniform(const size_t x_length)
{
    if (!(x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_ELEM_LENGTH);
    ndarray<size_t, 1u> idx_vec(std::array<size_t, 1u>{x_length});
    auto* const idx = idx_vec.data();
    for (size_t i = 0; i < x_length; ++i)
        idx[i] = i;

    return [idx_vec = std::move(idx_vec), remain = x_length]() mutable
    {
        if (remain == 0u)
            throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
        else if (remain == 1u)
            return idx_vec.data()[0];
        auto* const idx = idx_vec.data();
        const auto this_idx = std::uniform_int_distribution<size_t>(
            0u, remain - 1u)(global_random_engine);
        if (this_idx + 1u < remain) // did not pick the last element
            idx[this_idx] = idx[remain - 1u];
        --remain;
        return this_idx;
    };
}

inline auto _random_sample_prepare_short_uniform(const size_t x_length)
{
    if (!(x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_ELEM_LENGTH);

    return [idx_vec = ndarray<size_t, 1u>{}, x_length]() mutable
    {
        const auto n_taken = idx_vec.size();
        auto* const idx = idx_vec.data();
        if (x_length == n_taken)
            throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
        if (n_taken >= 2u)
            std::push_heap(idx, idx + n_taken, std::greater<>{});
        auto this_idx = std::uniform_int_distribution<size_t>(
            0u, x_length - n_taken - 1u)(global_random_engine);
        idx_vec.for_each([&](const auto& taken)
            {
                if (this_idx >= taken)
                    ++this_idx;
            });
        idx_vec.append(size_t(this_idx));
        return this_idx;
    };
}

template<typename W>
auto _random_sample_prepare_binary(const W& w, const size_t x_length)
{
    static_assert(array_rank_v<W> == 1u && is_real_v<value_type_t<W>>,
        WL_ERROR_RANDOM_WEIGHTS_TYPE);
    if (!(x_length == w.dims()[0] && x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_WEIGHTS_LENGTH);

    const size_t n = x_length;
    if (n <= 3u)
        throw std::logic_error(WL_ERROR_INTERNAL);
    ndarray<double, 1u> probs_vec(std::array<size_t, 1>{n});
    ndarray<uint64_t, 1u> uint_probs_vec(std::array<size_t, 1>{n});
    auto* const probs = probs_vec.data();
    auto* const uint_probs = uint_probs_vec.data();

    w.copy_to(probs);
    double sum_prob = 0.0;
    for (const auto& prob : probs_vec)
    {
        if (prob < 0.)
            throw std::logic_error(WL_ERROR_NEGATIVE_WEIGHT);
        sum_prob += prob;
    }
    constexpr double target_sum_prob = 1.0e+19; // ~2^63
    const double factor = target_sum_prob / sum_prob;
    WL_IGNORE_DEPENDENCIES
    for (size_t i = 0; i < n; ++i)
    {
        const auto prob = uint64_t(std::floor(probs[i] * factor + 0.5));
        // each element should have a probability of at least ~2^-63
        uint_probs[i] = (prob > 0u) ? prob : uint64_t(1);
    }

    const auto lzcnt = utils::_lzcnt(uint64_t(n - 1u));
    const auto max_jump = size_t(1) << (63u - lzcnt);

    // fold the ragged part
    for (size_t i = max_jump; i < n; ++i)
        uint_probs[i - max_jump] += uint_probs[i];
    // fold the regular parts
    for (auto jump = max_jump >> 1; jump > 0u; jump >>= 1)
    {
        for (size_t i = 0; i < jump; ++i)
            uint_probs[i] += uint_probs[jump + i];
    }
    
    return [uint_probs_vec = std::move(uint_probs_vec), n, max_jump]() mutable
    {
        auto* uint_probs = uint_probs_vec.data();
        if (uint_probs[0] == 0u)
            throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
        auto rand = std::uniform_int_distribution<uint64_t>(
            0u, uint_probs[0] - 1u)(global_random_engine);

        // pick the index
        auto index = uint64_t(0);
        auto weight = uint_probs[0]; // to be removed from uint_probs
        for (size_t jump = 1u; jump <= max_jump; jump <<= 1)
        {
            if (index + jump >= n)
                break;
            else
            {
                const auto proposed = uint_probs[index + jump];
                if (rand >= proposed)
                {
                    rand -= proposed;
                    weight -= proposed;
                }
                else
                {
                    index += jump;
                    weight = proposed;
                }
            }
        }
        const auto ret_index = size_t(index);

        // remove weight from the uint_probs
        uint_probs[index] -= weight;
        for (size_t jump = max_jump; true; jump >>= 1)
        {
            if (jump <= index)
            {
                index -= jump;
                uint_probs[index] -= weight;
            }
            if (index == 0u)
                break;
        }
        return ret_index;
    };
}

template<typename W>
auto _random_sample_prepare_linear(const W& w, const size_t x_length)
{
    static_assert(array_rank_v<W> == 1u && is_real_v<value_type_t<W>>,
        WL_ERROR_RANDOM_WEIGHTS_TYPE);
    if (!(x_length == w.dims()[0] && x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_WEIGHTS_LENGTH);

    const size_t n = x_length;
    if (n <= 1u)
        throw std::logic_error(WL_ERROR_INTERNAL);
    ndarray<double, 1u> pmf_vec(std::array<size_t, 1u>{n});
    ndarray<double, 1u> cmf_vec(std::array<size_t, 1u>{n});
    auto* const pmf = pmf_vec.data();
    auto* const cmf = cmf_vec.data();

    w.copy_to(pmf);
    if (std::any_of(pmf, pmf + n, [](auto p) { return p < 0.; }))
        throw std::logic_error(WL_ERROR_NEGATIVE_WEIGHT);
    std::partial_sum(pmf, pmf + n, cmf);

    return [cmf_vec = std::move(cmf_vec), n, last_index = n]() mutable
    {
        auto* const cmf = cmf_vec.data();
        if (last_index < n)
        { // if not the last element, update pmf and cmf
            const auto diff = last_index == 0u ? cmf[0u] :
                (cmf[last_index] - cmf[last_index - 1u]);
            for (auto i = last_index; i < n; ++i)
                cmf[i] -= diff;
        }
        if (cmf[n - 1u] <= 0.)
            throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_ZERO_WEIGHTS);
        const auto rand = std::uniform_real_distribution<>(
            0., cmf[n - 1u])(global_random_engine);
        size_t index = size_t(std::lower_bound(cmf, cmf + n, rand) - cmf);
        last_index = index;
        return index;
    };
}

template<typename W, typename X, typename Size>
auto random_sample(const W& w, const X& x, varg_tag, const Size& size)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    const auto outer_dims = utils::get_dims_array(size);
    const auto outer_size = utils::size_of_dims(outer_dims);
    if (size > x_length)
        throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
    // automatically select between binary and linear
    if (outer_size > 5u && x_length > 35u)
        return _random_choice_batch_impl(
            _random_sample_prepare_binary(w, x_length), x, outer_dims);
    else
        return _random_choice_batch_impl(
            _random_sample_prepare_linear(w, x_length), x, outer_dims);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename W, typename X>
auto random_sample(const W& w, const X& x, varg_tag)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    return random_sample(w, x, x_length);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Size>
auto random_sample(const X& x, const Size& size)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    const auto outer_dims = utils::get_dims_array(size);
    const auto outer_size = utils::size_of_dims(outer_dims);
    if (size > x_length)
        throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
    // automatically select between two methods
    if (x_length == 1u && size == 1u)
        return val(x);
    else if (size <= 2u || 10 * size * size <= x_length)
        return _random_choice_batch_impl(
            _random_sample_prepare_short_uniform(x_length), x, outer_dims);
    else
        return _random_choice_batch_impl(
            _random_sample_prepare_uniform(x_length), x, outer_dims);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto random_sample(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    return random_sample(x, x_length);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
