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
#include "ndarray.h"
#include "arrayview.h"
#include "numerical.h"
#include "functional.h"
#include "utils.h"

namespace wl
{

template<typename Z, typename X, typename Y>
void _dot_vv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K)
{
    auto z = Z(0);
    for (size_t k = 0u; k < K; ++k)
        z += Z(px[k]) * Z(py[k]);
    *pz += z;
}

template<typename Z, typename X, typename Y>
void _dot_mv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K)
{
    WL_THROW_IF_ABORT()
    for (size_t m = 0u; m < M; ++m)
        _dot_vv(pz + m, px + m * K, py, K);
}

template<typename Z, typename X, typename Y>
auto _dot_vm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const size_t N)
{
    WL_THROW_IF_ABORT()
    for (size_t k = 0u; k < K; k += 1)
    {
        const auto xk = Z(px[k]);
        const auto* WL_RESTRICT pyk = py + k * N;
        for (size_t n = 0u; n < N; ++n)
            pz[n] += xk * Z(pyk[n]);
    }
}

template<typename Z, typename X, typename Y>
auto _dot_mm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, const size_t N)
{
    // x: M * K, y : K * M
    for (size_t m = 0; m < M; ++m)
        _dot_vm(pz + m * N, px + m * K, py, K, N);
}

template<typename Z, typename X, typename Y>
auto _dot_mmt(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, const size_t N)
{
    // x: M * K, y : N * K
    for (size_t m = 0; m < M; ++m)
        for (size_t n = 0; n < N; ++n, ++pz, *pz = Z(0))
            _dot_vv(pz, px + m * K, py + n * K, K);
}

template<typename X, typename Y>
auto dot(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X> && is_numerical_type_v<Y>,
        WL_ERROR_NUMERIC_ONLY);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR >= 1u && YR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<X>;
    using YV = value_type_t<Y>;
    using C = common_type_t<XV, YV>;
    WL_THROW_IF_ABORT()
    const auto& valx = allows<view_category::Simple>(x);
    const auto& valy = allows<view_category::Simple>(y);
    const auto* px = valx.data();
    const auto* py = valy.data();

    const auto K = valx.dims()[XR - 1u];
    if (K != valy.dims()[0])
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);

    if constexpr (XR == 1u)
    {
        if constexpr (YR == 1u)
        {
            auto z = C(0);
            _dot_vv(&z, px, py, K);
            return z;
        }
        else
        {
            const auto ret_dims = utils::dims_take<2u, YR>(valy.dims());
            ndarray<C, YR - 1u> ret(ret_dims, C(0));
            const auto N = ret.size();
            auto* pz = ret.data();
            _dot_vm(pz, px, py, K, N);
            return ret;
        }
    }
    else
    {
        if constexpr (YR == 1u)
        {
            const auto ret_dims = utils::dims_take<1u, XR - 1u>(valx.dims());
            ndarray<C, XR - 1u> ret(ret_dims, C(0));
            const auto M = ret.size();
            auto* pz = ret.data();
            _dot_mv(pz, px, py, M, K);
            return ret;
        }
        else
        {
            const auto M_dims = utils::dims_take<1u, XR - 1u>(valx.dims());
            const auto N_dims = utils::dims_take<2u, YR>(valy.dims());
            const auto M = utils::size_of_dims(M_dims);
            const auto N = utils::size_of_dims(N_dims);
            ndarray<C, XR + YR - 2u> ret(
                utils::dims_join(M_dims, N_dims), C(0));
            auto* pz = ret.data();
            _dot_mm(pz, px, py, M, K, N);
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y, typename Z, typename... Rest>
auto dot(const X& x, const Y& y, const Z& z, const Rest&... rest)
{
    return dot(dot(x, y), z, rest...);
}

template<typename C, typename X, typename Y, typename F>
void _inner_f(C* WL_RESTRICT pc, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const ptrdiff_t dy, size_t K, F f)
{
    if (dy != 1)
    {
        WL_CHECK_ABORT_LOOP_BEGIN(K)
            for (auto k = _loop_begin; k < _loop_end; ++k)
                pc[k] = f(px[k], py[k * dy]);
        WL_CHECK_ABORT_LOOP_END()
    }
    else
    {
        WL_CHECK_ABORT_LOOP_BEGIN(K)
            for (auto k = _loop_begin; k < _loop_end; ++k)
                pc[k] = f(px[k], py[k]);
        WL_CHECK_ABORT_LOOP_END()
    }
}

template<typename C, typename X, typename Y, typename F, typename G>
auto _inner_vv(const X* WL_RESTRICT px, const Y* WL_RESTRICT py,
    const size_t K, F f, G g)
{
    ndarray<C, 1u> c(std::array<size_t, 1u>{K});
    auto pc = c.data();
    auto pack = argument_pack<C*, false>(pc, K);
    _inner_f(pc, px, py, 1, K, f);
    return g(pack);
}

template<typename C, typename Z, typename X, typename Y, typename F, typename G>
void _inner_mv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, F f, G g)
{
    ndarray<C, 1u> c(std::array<size_t, 1u>{K});
    auto pc = c.data();
    auto pack = argument_pack<C*, false>(pc, K);
    for (size_t m = 0; m < M; ++m, ++pz, px += K)
    {
        _inner_f(pc, px, py, 1, K, f);
        *pz = g(pack);
    }
}

template<typename C, typename Z, typename X, typename Y, typename F, typename G>
void _inner_vm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const size_t N, F f, G g)
{
    ndarray<C, 1u> c(std::array<size_t, 1u>{K});
    auto pc = c.data();
    auto pack = argument_pack<C*, false>(pc, K);
    for (size_t n = 0; n < N; ++n, ++pz, ++py)
    {
        _inner_f(pc, px, py, N, K, f);
        *pz = g(pack);
    }
}

template<typename C, typename Z, typename X, typename Y, typename F, typename G>
void _inner_mm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, const size_t N,
    F f, G g)
{
    ndarray<C, 1u> c(std::array<size_t, 1u>{K});
    auto pc = c.data();
    auto pack = argument_pack<C*, false>(pc, K);
    for (size_t m = 0; m < M; ++m, px += K)
    {
        for (size_t n = 0; n < N; ++n, ++pz)
        {
            _inner_f(pc, px, py + n, N, K, f);
            *pz = g(pack);
        }
    }
}

template<typename F, typename X, typename Y, typename G>
auto inner(F f, const X& x, const Y& y, G g)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR >= 1u && YR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_variadic_function_v<G>, WL_ERROR_REQUIRE_VARIADIC);
    using XV = value_type_t<X>;
    using YV = value_type_t<Y>;
    using C = remove_cvref_t<decltype(f(XV{}, YV{}))>;
    using Z = remove_cvref_t<decltype(
        g(std::declval<argument_pack<C*, false>>()))>;
    const auto& valx = allows<view_category::Simple>(x);
    const auto& valy = allows<view_category::Simple>(y);
    const auto* px = valx.data();
    const auto* py = valy.data();

    const auto K = valx.dims()[XR - 1u];
    if (K != valy.dims()[0])
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);

    if constexpr (XR == 1u)
    {
        if constexpr (YR == 1u)
        {
            return _inner_vv<C>(px, py, K, f, g);
        }
        else
        {
            const auto ret_dims = utils::dims_take<2u, YR>(valy.dims());
            ndarray<Z, YR - 1u> ret(ret_dims);
            const auto N = ret.size();
            auto* pz = ret.data();
            _inner_vm<C>(pz, px, py, K, N, f, g);
            return ret;
        }
    }
    else
    {
        if constexpr (YR == 1u)
        {
            const auto ret_dims = utils::dims_take<1u, XR - 1u>(valx.dims());
            ndarray<Z, XR - 1u> ret(ret_dims);
            const auto M = ret.size();
            auto* pz = ret.data();
            _inner_mv<C>(pz, px, py, M, K, f, g);
            return ret;
        }
        else
        {
            const auto M_dims = utils::dims_take<1u, XR - 1u>(valx.dims());
            const auto N_dims = utils::dims_take<2u, YR>(valy.dims());
            const auto M = utils::size_of_dims(M_dims);
            const auto N = utils::size_of_dims(N_dims);
            ndarray<Z, XR + YR - 2u> ret(utils::dims_join(M_dims, N_dims));
            auto* pz = ret.data();
            _inner_mm<C>(pz, px, py, M, K, N, f, g);
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<size_t Level, size_t R>
void _tr_dims(size_t& ret_stride, size_t& ret_tr_size,
    const std::array<size_t, R>& x_dims)
{
    static_assert(1 <= Level && Level <= R, WL_ERROR_INTERNAL);
    size_t stride = 0u;
    size_t inner_size = 1u;
    size_t tr_size = size_t(-1);
    for (auto i = ptrdiff_t(Level - 1u); i >= 0; --i)
    {
        const size_t dim = x_dims[i];
        stride += inner_size;
        inner_size *= dim;
        if (dim < tr_size)
            tr_size = dim;
    }
    ret_stride = stride;
    ret_tr_size = tr_size;
}

template<typename X, typename F, int64_t I>
auto tr(const X& x, F f, const_int<I>)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(0 < I && I <= int64_t(XR), WL_ERROR_BAD_LEVEL);
    static_assert(is_variadic_function_v<F>, WL_ERROR_REQUIRE_VARIADIC);
    using XV = value_type_t<X>;
    constexpr auto Level = size_t(I);

    if constexpr (Level == XR)
    {
        using RV = remove_cvref_t<decltype(
            f(std::declval<argument_pack<XV*, true>>()))>;
        if (x.size() == 0u)
        {
            return RV();
        }
        else
        {
            const auto& valx = allows<view_category::Simple>(x);
            size_t stride = 0u;
            size_t tr_size = 0u;
            _tr_dims<XR>(stride, tr_size, valx.dims());
            const auto pack = argument_pack<const XV*, true>(
                valx.data(), tr_size, stride);
            return f(pack);
        }
    }
    else
    {
        const auto& valx = allows<view_category::Array>(x);
        auto x_iter = valx.template view_begin<Level>();
        using PackType = argument_pack<decltype(x_iter), true>;
        using RT = remove_cvref_t<decltype(f(std::declval<PackType>()))>;
        size_t stride = 0u;
        size_t tr_size = 0u;
        _tr_dims<Level>(stride, tr_size, valx.dims());
        const auto pack = PackType(x_iter, tr_size, stride);
        return f(pack);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename F>
auto tr(const X& x, F f)
{
    WL_TRY_BEGIN()
    return tr(x, f, const_int<array_rank_v<X>>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto tr(const X& x)
{
    WL_TRY_BEGIN()
    return tr(x, WL_FUNCTION(plus), const_int<array_rank_v<X>>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
