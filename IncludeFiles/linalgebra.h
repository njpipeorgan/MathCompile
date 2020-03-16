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

#if defined(WL_BLAS_USE_EIGEN)
#include <Eigen/Dense>
#endif

namespace wl
{

namespace blas
{

template<typename T> constexpr T const_one [2] = {T(1), T(0)};
template<typename T> constexpr T const_zero[2] = {T(0), T(0)};

template<typename T, typename... Sizes>
void check_sizes(const Sizes&... sizes)
{
    if (((size_t(sizes) > size_t(std::numeric_limits<T>::max())) || ...))
        throw std::logic_error(WL_ERROR_BLAS_SIZE);
}

template<typename Z, typename X, typename Y>
WL_INLINE void dot_vv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K)
{
    auto z = Z(0);
    for (size_t k = 0u; k < K; ++k)
        z += Z(px[k]) * Z(py[k]);
    *pz += z;
}

template<typename Z, typename X, typename Y>
void dot_mv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K)
{
#if defined(WL_BLAS_USE_EIGEN)
    using namespace Eigen;
    using lhs = ::Eigen::internal::blas_data_mapper<
        const X, ptrdiff_t, RowMajor>;
    using rhs = ::Eigen::internal::blas_data_mapper<
        const Y, ptrdiff_t, RowMajor>;
    ::Eigen::internal::general_matrix_vector_product<
        ptrdiff_t, X, lhs, RowMajor, false, Y, rhs, false>::run(
            M, K, lhs(px, K), rhs(py, 1), pz, 1, 1);
#elif defined(WL_USE_CBLAS)
    if constexpr (is_float_v<Z> || is_complex_v<Z>)
    {
        check_sizes<int>(M, K);
        const auto iM = int(M);
        const auto iK = int(K);
        if constexpr (std::is_same_v<Z, float>)
            cblas_sgemv(CblasRowMajor, CblasNoTrans,
                iM, iK, 1, px, iK, py, 1, 0, pz, 1);
        else if constexpr (std::is_same_v<Z, double>)
            cblas_dgemv(CblasRowMajor, CblasNoTrans,
                iM, iK, 1, px, iK, py, 1, 0, pz, 1);
        else if constexpr (std::is_same_v<Z, complex<float>>)
            cblas_sgemv(CblasRowMajor, CblasNoTrans,
                iM, iK, &const_one<float>, px, iK, py, 1,
                &const_zero<float>, pz, 1);
        else if constexpr (std::is_same_v<Z, complex<double>>)
            cblas_zgemv(CblasRowMajor, CblasNoTrans,
                iM, iK, &const_one<double>, px, iK, py, 1,
                &const_zero<double>, pz, 1);
    }
    else
    {
#endif
#if !defined(WL_BLAS_USE_EIGEN)
    for (size_t m = 0u; m < M; ++m, ++pz, px += K)
        dot_vv(pz, px, py, K);
#endif
#if defined(WL_USE_CBLAS)
    }
#endif
}

template<typename Z, typename Y>
WL_INLINE auto dot_sv(Z* WL_RESTRICT pz, const Z x, const Y* WL_RESTRICT py,
    const size_t N)
{
    for (size_t n = 0u; n < N; ++n)
        pz[n] += x * py[n];
}

template<typename Z, typename X, typename Y>
auto dot_vm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const size_t N)
{
#if defined(WL_BLAS_USE_EIGEN)
    using namespace Eigen;
    using lhs = ::Eigen::internal::blas_data_mapper<
        const Y, ptrdiff_t, ColMajor>;
    using rhs = ::Eigen::internal::blas_data_mapper<
        const X, ptrdiff_t, RowMajor>;
    ::Eigen::internal::general_matrix_vector_product<
        ptrdiff_t, Y, lhs, ColMajor, false, X, rhs, false>::run(
            N, K, lhs(py, N), rhs(px, 1), pz, 1, 1);
#elif defined(WL_USE_CBLAS)
    if constexpr (is_float_v<Z> || is_complex_v<Z>)
    {
        check_sizes<int>(K, N);
        const auto iK = int(K);
        const auto iN = int(N);
        if constexpr (std::is_same_v<Z, float>)
            cblas_sgemv(CblasColMajor, CblasNoTrans,
                iN, iK, 1, py, iN, px, 1, 0, pz, 1);
        else if constexpr (std::is_same_v<Z, double>)
            cblas_dgemv(CblasColMajor, CblasNoTrans,
                iN, iK, 1, py, iN, px, 1, 0, pz, 1);
        else if constexpr (std::is_same_v<Z, complex<float>>)
            cblas_cgemv(CblasColMajor, CblasNoTrans,
                iN, iK, &const_one<float>, py, iN, px, 1,
                &const_zero<double>, pz, 1);
        else if constexpr (std::is_same_v<Z, complex<double>>)
            cblas_zgemv(CblasColMajor, CblasNoTrans,
                iN, iK, &const_one<double>, py, iN, px, 1,
                &const_zero<double>, pz, 1);
    }
    else
    {
#endif
#if !defined(WL_BLAS_USE_EIGEN)
    for (size_t k = 0u; k < K; ++k, py += N)
        dot_sv(pz, Z(px[k]), py, N);
#endif
#if defined(WL_USE_CBLAS)
    }
#endif
}

template<typename Z, typename X, typename Y>
auto dot_mm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, const size_t N)
{
#if defined(WL_BLAS_USE_EIGEN)
    using namespace Eigen;
    auto blocking = ::Eigen::internal::gemm_blocking_space<
        ColMajor, Y, X, Dynamic, Dynamic, Dynamic>(N, M, K, 1, true);
    Eigen::internal::general_matrix_matrix_product<
        ptrdiff_t, Y, ColMajor, false, X, ColMajor, false, ColMajor>::run(
            N, M, K, py, N, px, K, pz, N, 1, blocking, 0);
#elif defined(WL_USE_CBLAS)
    if constexpr (is_float_v<Z> || is_complex_v<Z>)
    {
        check_sizes<int>(M, K, N);
        const auto iM = int(M);
        const auto iK = int(K);
        const auto iN = int(N);
        if constexpr (std::is_same_v<Z, float>)
            cblas_sgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                iN, iM, iK, 1, py, iN, px, iK, 0, pz, iN);
        else if constexpr (std::is_same_v<Z, double>)
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                iN, iM, iK, 1, py, iN, px, iK, 0, pz, iN);
        else if constexpr (std::is_same_v<Z, complex<float>>)
            cblas_cgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                iN, iM, iK, &const_one<float>, py, iN, px, iK,
                &const_zero<float>, pz, iN);
        else if constexpr (std::is_same_v<Z, complex<double>>)
            cblas_zgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                iN, iM, iK, &const_one<double>, py, iN, px, iK,
                &const_zero<double>, pz, iN);
    }
    else
    {
#endif
#if !defined (WL_BLAS_USE_EIGEN)
        constexpr size_t BM = 16;
        constexpr size_t BN = 1024 / sizeof(Z);
        constexpr size_t BK = 16;
        for (size_t m1 = 0u; m1 < M; m1 += BM)
        for (size_t k1 = 0u; k1 < K; k1 += BK)
        for (size_t n1 = 0u; n1 < N; n1 += BN)
        {
            WL_THROW_IF_ABORT()
            for (size_t m = m1; m < std::min(m1 + BM, M); ++m)
            for (size_t k = k1; k < std::min(k1 + BK, K); ++k)
            {
                auto* WL_RESTRICT pz1 = pz + (m * N + n1);
                auto* WL_RESTRICT py1 = py + (k * N + n1);
                dot_sv(pz1, Z(px[m * K + k]), py1, std::min(N - n1, BN));
            }
        }
#endif
#if defined(WL_USE_CBLAS)
    }
#endif
}

}

template<typename B, typename X>
auto _dot_get_array(const X& x) -> decltype(auto)
{
    constexpr auto XR = array_rank_v<X>;
    using XV = value_type_t<X>;
#if defined(WL_USE_CBLAS)
    using C = std::conditional_t<is_complex_v<XV>,
        complex<value_type_t<B>>, value_type_t<B>>;
#else // native or Eigen
    using C = B;
#endif
    static_assert(XR >= 1u && is_arithmetic_v<B> && is_convertible_v<XV, C>,
        WL_ERROR_INTERNAL);
    if constexpr ((X::category == view_category::Array ||
        X::category == view_category::Simple) && (std::is_same_v<XV, C> ||
            is_integral_v<XV> && is_integral_v<C> && sizeof(XV) == sizeof(C)))
    {
        return x;
    }
    else
    {
        return cast<ndarray<C, XR>>(x);
    }
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
    using C = common_type_t<value_type_t<X>, value_type_t<Y>>;

    WL_THROW_IF_ABORT()
    const auto& valx = _dot_get_array<C>(x);
    const auto& valy = _dot_get_array<C>(y);
    const auto* px = valx.data();
    const auto* py = valy.data();
    using XV = remove_cvref_t<decltype(*px)>;
    using YV = remove_cvref_t<decltype(*py)>;

    const auto K = valx.dims()[XR - 1u];
    if (K != valy.dims()[0])
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);

    if constexpr (XR == 1u)
    {
        if constexpr (YR == 1u)
        {
            auto z = C(0);
            blas::dot_vv(&z, px, py, K);
            return z;
        }
        else
        {
            const auto ret_dims = utils::dims_take<2u, YR>(valy.dims());
            ndarray<C, YR - 1u> ret(ret_dims, C(0));
            const auto N = ret.size();
            auto* pz = ret.data();
            blas::dot_vm(pz, px, py, K, N);
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
            blas::dot_mv(pz, px, py, M, K);
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
            blas::dot_mm(pz, px, py, M, K, N);
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
