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
#include "listable.h"
#include "complex.h"
#include "utils.h"

#include <Eigen/Dense>

namespace wl
{

namespace blas
{

enum : int
{
    NoTrans   = 111,
    Trans     = 112,
    ConjTrans = 113
};

#if !defined(WL_USE_LAPACKE)
#  define LAPACK_ROW_MAJOR               101
#  define LAPACK_COL_MAJOR               102
#  define LAPACK_WORK_MEMORY_ERROR       -1010
#  define LAPACK_TRANSPOSE_MEMORY_ERROR  -1011
#endif

template<typename T>
constexpr T const_one[2] = {T(1), T(0)};

template<typename T, int Storage = Eigen::ColMajor>
using EigenMatrix = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Storage>;

template<typename T, typename... Sizes>
WL_INLINE void check_sizes(const Sizes&... sizes)
{
    if (((size_t(sizes) > size_t(std::numeric_limits<T>::max())) || ...))
        throw std::logic_error(WL_ERROR_BLAS_SIZE);
}

template<bool Trans, bool Conj, typename X, typename Y>
WL_INLINE void omatcopy(const X* WL_RESTRICT px, Y* WL_RESTRICT py,
    const size_t M, const size_t N)
{
    if constexpr (Trans)
    {
        for (size_t m = 0u; m < M; ++m, px += N, ++py)
            for (size_t n = 0u; n < N; ++n)
            {
                if constexpr (Conj && !is_real_v<X>)
                    py[n * M] = conjugate(px[n]);
                else
                    py[n * M] = px[n];
            }
    }
    else
    {
        const auto size = M * N;
        for (size_t i = 0; i < size; ++i)
        {
            if constexpr (Conj && !is_real_v<X>)
                py[i] = conjugate(px[i]);
            else
                py[i] = px[i];
        }
    }
}

template<bool Trans, bool Conj, typename X>
WL_INLINE void imatcopy(X* px, const size_t M, const size_t N)
{
    if constexpr (Trans)
    {
        assert(M == N);
        if constexpr (Conj && !is_real_v<X>)
        {
            for (size_t n = 0u; n < N; ++n)
            {
                for (size_t m = 0u; m < n; ++m)
                {
                    auto x1 = conjugate(px[n * M + m]);
                    auto x2 = conjugate(px[m * M + n]);
                    px[n * M + m] = x2;
                    px[m * M + n] = x1;
                }
                px[n * M + n] = conjugate(px[n * M + n]);
            }
        }
        else
        {
            for (size_t n = 1u; n < N; ++n)
                for (size_t m = 0u; m < n; ++m)
                    std::swap(px[n * M + m], px[m * M + n]);
        }
    }
    else if constexpr (Conj && !is_real_v<X>)
    {
        const auto size = M * N;
        for (size_t i = 0; i < size; ++i)
            px[i] = conjugate(px[i]);
    }
}

template<typename Z, typename X, typename Y, typename Alpha>
WL_INLINE void dot(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const Alpha alpha)
{
    WL_THROW_IF_ABORT()
    auto z = Z(0);
    for (size_t k = 0u; k < K; ++k)
        z += Z(alpha * px[k] * py[k]);
    *pz += z;
}

template<typename Z, typename X, typename Y>
WL_INLINE void dot(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K)
{
    WL_THROW_IF_ABORT()
    dot(pz, px, py, K, 1);
}

template<typename Z, typename X, typename Y, typename Alpha>
inline void gevv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t N,
    const Alpha alpha)
{
    WL_THROW_IF_ABORT()
    for (size_t m = 0u; m < M; ++m, pz += N)
    {
        const auto xm = alpha * px[m];
        for (size_t n = 0u; n < N; ++n)
            pz[n] += Z(xm * py[n]);
    }
}

template<typename Z, typename X, typename Y, typename Alpha>
WL_INLINE void gevv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t N)
{
    gevv(pz, px, py, M, N, 1);
}

template<typename Z, typename X, typename Y>
void gemv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K,
    const Z alpha = Z(1))
{
    WL_THROW_IF_ABORT()
#if defined(WL_USE_CBLAS)
    if constexpr (is_float_v<Z> || is_complex_v<Z>)
    {
        check_sizes<int>(M, K);
        const auto iM = int(M);
        const auto iK = int(K);
        if constexpr (std::is_same_v<Z, float>)
            cblas_sgemv(CblasRowMajor, CblasNoTrans,
                iM, iK, alpha, px, iK, py, 1, 1, pz, 1);
        else if constexpr (std::is_same_v<Z, double>)
            cblas_dgemv(CblasRowMajor, CblasNoTrans,
                iM, iK, alpha, px, iK, py, 1, 1, pz, 1);
        else if constexpr (std::is_same_v<Z, complex<float>>)
            cblas_sgemv(CblasRowMajor, CblasNoTrans,
                iM, iK, &alpha, px, iK, py, 1,
                &const_one<float>, pz, 1);
        else if constexpr (std::is_same_v<Z, complex<double>>)
            cblas_zgemv(CblasRowMajor, CblasNoTrans,
                iM, iK, &alpha, px, iK, py, 1,
                &const_one<double>, pz, 1);
    }
    else
    {
#endif
    using lhs = Eigen::internal::blas_data_mapper<
        const X, ptrdiff_t, Eigen::RowMajor>;
    using rhs = Eigen::internal::blas_data_mapper<
        const Y, ptrdiff_t, Eigen::RowMajor>;
    Eigen::internal::general_matrix_vector_product<
        ptrdiff_t, X, lhs, Eigen::RowMajor, false, Y, rhs, false>::run(
            M, K, lhs(px, K), rhs(py, 1), pz, 1, alpha);
#if defined(WL_USE_CBLAS)
    }
#endif
}

template<typename Z, typename X, typename Y>
auto gevm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const size_t N,
    const Z alpha = Z(1))
{
    WL_THROW_IF_ABORT()
#if defined(WL_USE_CBLAS)
    if constexpr (is_float_v<Z> || is_complex_v<Z>)
    {
        check_sizes<int>(K, N);
        const auto iK = int(K);
        const auto iN = int(N);
        if constexpr (std::is_same_v<Z, float>)
            cblas_sgemv(CblasColMajor, CblasNoTrans,
                iN, iK, alpha, py, iN, px, 1, 1, pz, 1);
        else if constexpr (std::is_same_v<Z, double>)
            cblas_dgemv(CblasColMajor, CblasNoTrans,
                iN, iK, alpha, py, iN, px, 1, 1, pz, 1);
        else if constexpr (std::is_same_v<Z, complex<float>>)
            cblas_cgemv(CblasColMajor, CblasNoTrans,
                iN, iK, &alpha, py, iN, px, 1,
                &const_one<double>, pz, 1);
        else if constexpr (std::is_same_v<Z, complex<double>>)
            cblas_zgemv(CblasColMajor, CblasNoTrans,
                iN, iK, &alpha, py, iN, px, 1,
                &const_one<double>, pz, 1);
    }
    else
    {
#endif
    using lhs = Eigen::internal::blas_data_mapper<
        const Y, ptrdiff_t, Eigen::ColMajor>;
    using rhs = Eigen::internal::blas_data_mapper<
        const X, ptrdiff_t, Eigen::RowMajor>;
    Eigen::internal::general_matrix_vector_product<
        ptrdiff_t, Y, lhs, Eigen::ColMajor, false, X, rhs, false>::run(
            N, K, lhs(py, N), rhs(px, 1), pz, 1, alpha);
#if defined(WL_USE_CBLAS)
    }
#endif
}

template<int TX = NoTrans, int TY = NoTrans,
    typename Z, typename X, typename Y>
auto gemm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, const size_t N,
    const Z alpha = Z(1), const Z beta = Z(1))
{
    WL_THROW_IF_ABORT()
    const size_t LDX = TX == NoTrans ? K : M;
    const size_t LDY = TY == NoTrans ? N : K;
#if defined(WL_USE_CBLAS)
    if constexpr (is_float_v<Z> || is_complex_v<Z>)
    {
        check_sizes<int>(M, K, N);
        const auto iM = int(M), iK = int(K), iN = int(N);
        const auto iLDX = int(LDX), iLDY = int(LDY);
        if constexpr (std::is_same_v<Z, float>)
            cblas_sgemm(CblasRowMajor,
                TX == NoTrans ? CblasNoTrans : CblasTrans,
                TY == NoTrans ? CblasNoTrans : CblasTrans,
                iM, iN, iK, alpha, px, iLDX, py, iLDY, beta, pz, iN);
        else if constexpr (std::is_same_v<Z, double>)
            cblas_dgemm(CblasRowMajor,
                TX == NoTrans ? CblasNoTrans : CblasTrans,
                TY == NoTrans ? CblasNoTrans : CblasTrans,
                iM, iN, iK, alpha, px, iLDX, py, iLDY, beta, pz, iN);
        else if constexpr (std::is_same_v<Z, complex<float>>)
            cblas_cgemm(CblasRowMajor,
                CBLAS_TRANSPOSE(TX), CBLAS_TRANSPOSE(TY),
                iM, iN, iK, &alpha, px, iLDX, py, iLDY,
                &beta, pz, iN);
        else if constexpr (std::is_same_v<Z, complex<double>>)
            cblas_zgemm(CblasRowMajor,
                CBLAS_TRANSPOSE(TX), CBLAS_TRANSPOSE(TY),
                iM, iN, iK, &alpha, px, iLDX, py, iLDY,
                &beta, pz, iN);
    }
    else
    {
#endif
    if (beta != Z(1))
    {
        for (size_t i = 0; i < K * N; ++i)
            pz[i] *= beta;
    }
    using blocking_t = Eigen::internal::gemm_blocking_space<Eigen::RowMajor,
        X, Y, Eigen::Dynamic, Eigen::Dynamic, Eigen::Dynamic>;
    auto blocking = blocking_t(M, N, K, 1, true);
    Eigen::internal::general_matrix_matrix_product<ptrdiff_t,
        X, TX == NoTrans ? Eigen::RowMajor : Eigen::ColMajor,
        is_complex_v<X> && TX == ConjTrans,
        Y, TY == NoTrans ? Eigen::RowMajor : Eigen::ColMajor,
        is_complex_v<Y> && TY == ConjTrans,
        Eigen::RowMajor>::run(
            M, N, K, px, LDX, py, LDY, pz, N, alpha, blocking, 0);
#if defined(WL_USE_CBLAS)
    }
#endif
}

template<typename Z>
auto getri(Z* pz, const size_t N)
{
    WL_THROW_IF_ABORT()
    static_assert(is_float_v<Z> || is_complex_v<Z>);
#if defined(WL_USE_LAPACKE)
    check_sizes<lapack_int>(N);
    const auto iN = lapack_int(N);
    lapack_int info = 0;
    ndarray<lapack_int, 1u> ipiv(std::array<size_t, 1u>{N});

    if constexpr (std::is_same_v<Z, float>)
        info = LAPACKE_sgetrf(LAPACK_COL_MAJOR, iN, iN, pz, iN, ipiv.data());
    else if constexpr (std::is_same_v<Z, double>)
        info = LAPACKE_dgetrf(LAPACK_COL_MAJOR, iN, iN, pz, iN, ipiv.data());
    else if constexpr (std::is_same_v<Z, complex<float>>)
        info = LAPACKE_cgetrf(LAPACK_COL_MAJOR, iN, iN,
            (lapack_complex_float*)pz, iN, ipiv.data());
    else if constexpr (std::is_same_v<Z, complex<double>>)
        info = LAPACKE_zgetrf(LAPACK_COL_MAJOR, iN, iN,
            (lapack_complex_double*)pz, iN, ipiv.data());
    if (info < 0) throw std::logic_error(WL_ERROR_INTERNAL);

    if constexpr (std::is_same_v<Z, float>)
        info = LAPACKE_sgetri(LAPACK_COL_MAJOR, iN, pz, iN, ipiv.data());
    else if constexpr (std::is_same_v<Z, double>)
        info = LAPACKE_dgetri(LAPACK_COL_MAJOR, iN, pz, iN, ipiv.data());
    else if constexpr (std::is_same_v<Z, complex<float>>)
        info = LAPACKE_cgetri(LAPACK_COL_MAJOR, iN,
            (lapack_complex_float*)pz, iN, ipiv.data());
    else if constexpr (std::is_same_v<Z, complex<double>>)
        info = LAPACKE_zgetri(LAPACK_COL_MAJOR, iN,
            (lapack_complex_double*)pz, iN, ipiv.data());
    if (info < 0) throw std::logic_error(WL_ERROR_INTERNAL);
    if (info > 0) throw std::logic_error(WL_ERROR_LAPACKE_MATRIX_SINGULAR);
#else
    Eigen::Map<EigenMatrix<Z>> mapz(pz, N, N);
    mapz = mapz.inverse();
#endif
}

template<typename Z, typename P, typename C>
auto getrf(Z* pz, P* piv, C* cond, const size_t M, const size_t N)
{
    static_assert(is_float_v<Z> || is_complex_v<Z>);
    static_assert(is_integral_v<P>);
    static_assert(std::is_same_v<value_type_t<Z>, C>);
#if defined(WL_USE_LAPACKE)
    check_sizes<lapack_int>(M, N);
    const auto K = std::min(M, N);
    const auto iM = lapack_int(M), iN = lapack_int(N);
    lapack_int info = 0;
    lapack_int* ipiv = (lapack_int*)malloc(K * sizeof(lapack_int));
    if (!ipiv) throw std::bad_alloc{};
    C norm = 0;
    if (M == N)
    {
        if constexpr (std::is_same_v<Z, float>)
            norm = LAPACKE_slange(LAPACK_ROW_MAJOR, 'I', iM, iN, pz, iN);
        else if constexpr (std::is_same_v<Z, double>)
            norm = LAPACKE_dlange(LAPACK_ROW_MAJOR, 'I', iM, iN, pz, iN);
        else if constexpr (std::is_same_v<Z, complex<float>>)
            norm = LAPACKE_clange(LAPACK_ROW_MAJOR, 'I', iM, iN,
                (lapack_complex_float*)pz, iN);
        else if constexpr (std::is_same_v<Z, complex<double>>)
            norm = LAPACKE_zlange(LAPACK_ROW_MAJOR, 'I', iM, iN,
                (lapack_complex_double*)pz, iN);
    }
    if constexpr (std::is_same_v<Z, float>)
        info = LAPACKE_sgetrf(LAPACK_ROW_MAJOR, iM, iN, pz, iN, ipiv);
    else if constexpr (std::is_same_v<Z, double>)
        info = LAPACKE_dgetrf(LAPACK_ROW_MAJOR, iM, iN, pz, iN, ipiv);
    else if constexpr (std::is_same_v<Z, complex<float>>)
        info = LAPACKE_cgetrf(LAPACK_ROW_MAJOR, iM, iN,
            (lapack_complex_float*)pz, iN, ipiv);
    else if constexpr (std::is_same_v<Z, complex<double>>)
        info = LAPACKE_zgetrf(LAPACK_ROW_MAJOR, iM, iN,
            (lapack_complex_double*)pz, iN, ipiv);
    if constexpr (sizeof(lapack_int) != sizeof(P))
    {
        for (size_t i = 0u; i < K; ++i)
            piv[i] = P(i + 1);
        for (size_t i = 0u; i < K; ++i)
            std::swap(piv[i], piv[ipiv[i] - 1]);
        free(ipiv);
    }
    if (info < 0) throw std::logic_error(WL_ERROR_INTERNAL);
    if (M == N)
    {
        if constexpr (std::is_same_v<Z, float>)
            info = LAPACKE_dgecon(LAPACK_ROW_MAJOR, 'I', iM,
                pz, iN, norm, cond);
        else if constexpr (std::is_same_v<Z, double>)
            info = LAPACKE_dgecon(LAPACK_ROW_MAJOR, 'I', iM,
                pz, iN, norm, cond);
        else if constexpr (std::is_same_v<Z, complex<float>>)
            info = LAPACKE_dgecon(LAPACK_ROW_MAJOR, 'I', iM,
                (lapack_complex_float*)pz, iN, norm, cond);
        else if constexpr (std::is_same_v<Z, complex<double>>)
            info = LAPACKE_dgecon(LAPACK_ROW_MAJOR, 'I', iM,
                (lapack_complex_double*)pz, iN, norm, cond);
        if (info < 0) throw std::logic_error(WL_ERROR_INTERNAL);
        *cond = C(1) / *cond;
    }
    else
    {
        *cond = C(0);
    }
#else
    Eigen::Map<EigenMatrix<Z, Eigen::RowMajor>> mapz(pz, M, N);
    Eigen::PartialPivLU<Eigen::Ref<decltype(mapz)>> lu(mapz);
    const auto& ipiv = lu.permutationP().indices();
    const auto K = std::min(M, N);
    for (size_t i = 0u; i < K; ++i)
        piv[ipiv[i]] = P(i + 1);
    *cond = C(1) / C(lu.rcond());
    mapz = lu.matrixLU();
#endif
}

template<typename Z>
auto potrf(Z* pz, const size_t N)
{
    WL_THROW_IF_ABORT()
    static_assert(is_float_v<Z> || is_complex_v<Z>);
#if defined(WL_USE_LAPACKE)
    check_sizes<lapack_int>(N);
    const auto iN = lapack_int(N);
    lapack_int info = 0;

    if constexpr (std::is_same_v<Z, float>)
        info = LAPACKE_spotrf(LAPACK_COL_MAJOR, 'L', iN, pz, iN);
    else if constexpr (std::is_same_v<Z, double>)
        info = LAPACKE_dpotrf(LAPACK_COL_MAJOR, 'L', iN, pz, iN);
    else if constexpr (std::is_same_v<Z, complex<float>>)
        info = LAPACKE_cpotrf(LAPACK_COL_MAJOR, 'L', iN,
            (lapack_complex_float*)pz, iN);
    else if constexpr (std::is_same_v<Z, complex<double>>)
        info = LAPACKE_zpotrf(LAPACK_COL_MAJOR, 'L', iN,
            (lapack_complex_double*)pz, iN);
    if (info < 0) throw std::logic_error(WL_ERROR_INTERNAL);
    for (size_t i = 0u; i < N; ++i, pz += N)
        for (size_t j = 0u; j < i; ++j)
            pz[j] = Z(0);
#else
    Eigen::Map<EigenMatrix<Z>> mapz(pz, N, N);
    Eigen::LLT<Eigen::Ref<decltype(mapz)>> llt(mapz);
    mapz = llt.matrixL();
#endif
}

template<typename Z, typename Q>
auto gees(Z* pz, Q* pq, const size_t N)
{
    WL_THROW_IF_ABORT()
    static_assert(is_float_v<Z> || is_complex_v<Z>);
#if defined(WL_USE_LAPACKE)
    check_sizes<lapack_int>(N);
    const auto iN = lapack_int(N);
    lapack_int info = 0;
    lapack_int sdim = 0;
    Z* pw = (Z*)malloc(N * sizeof(Z));
    if (!pw) throw std::bad_alloc{};

    if constexpr (std::is_same_v<Z, float>)
        info = LAPACKE_sgees(LAPACK_ROW_MAJOR, 'V', 'N', nullptr, iN,
            pz, iN, &sdim, pw, pw, pq, iN);
    else if constexpr (std::is_same_v<Z, double>)
        info = LAPACKE_dgees(LAPACK_ROW_MAJOR, 'V', 'N', nullptr, iN,
            pz, iN, &sdim, pw, pw, pq, iN);
    else if constexpr (std::is_same_v<Z, complex<float>>)
        info = LAPACKE_cgees(LAPACK_ROW_MAJOR, 'V', 'N', nullptr, iN,
            (lapack_complex_float*)pz, iN, &sdim,
            (lapack_complex_float*)pw, (lapack_complex_float*)pq, iN);
    else if constexpr (std::is_same_v<Z, complex<double>>)
        info = LAPACKE_zgees(LAPACK_ROW_MAJOR, 'V', 'N', nullptr, iN,
            (lapack_complex_double*)pz, iN, &sdim,
            (lapack_complex_double*)pw, (lapack_complex_double*)pq, iN);
    free(pw);
#else
    using EigenSchur = std::conditional_t<is_float_v<Z>,
        Eigen::RealSchur<EigenMatrix<Z, Eigen::RowMajor>>,
        Eigen::ComplexSchur<EigenMatrix<Z, Eigen::RowMajor>>>;
    Eigen::Map<EigenMatrix<Z, Eigen::RowMajor>> mapz(pz, N, N);
    Eigen::Map<EigenMatrix<Q, Eigen::RowMajor>> mapq(pq, N, N);
    EigenSchur schur(mapz, true);
    mapz = schur.matrixT();
    mapq = schur.matrixU();
#endif
}

template<typename Z, typename S, typename P, typename Q>
auto gesvd(Z* z, S* s, P* p, Q* q, const size_t M, const size_t N)
{
    WL_THROW_IF_ABORT()
    static_assert(is_float_v<Z> || is_complex_v<Z>);
    static_assert(std::is_same_v<value_type_t<Z>, S>);
#if defined(WL_USE_LAPACKE)
    check_sizes<lapack_int>(M, N);
    const auto iM = lapack_int(M), iN = lapack_int(N);
    lapack_int info = 0;
    S* b = (S*)malloc(std::min(M, N) * sizeof(Z));
    if (!b) throw std::bad_alloc{};

    if constexpr (std::is_same_v<Z, float>)
        info = LAPACKE_sgesvd(LAPACK_ROW_MAJOR, 'A', 'A', iM, iN,
            z, iN, s, p, iM, q, iN, b);
    else if constexpr (std::is_same_v<Z, double>)
        info = LAPACKE_dgesvd(LAPACK_ROW_MAJOR, 'A', 'A', iM, iN,
            z, iN, s, p, iM, q, iN, b);
    else if constexpr (std::is_same_v<Z, complex<float>>)
        info = LAPACKE_cgesvd(LAPACK_ROW_MAJOR, 'A', 'A', iM, iN,
            (lapack_complex_float*)z, iN, s,
            (lapack_complex_float*)p, iM, (lapack_complex_float*)q, iN, b);
    else if constexpr (std::is_same_v<Z, complex<double>>)
        info = LAPACKE_zgesvd(LAPACK_ROW_MAJOR, 'A', 'A', iM, iN,
            (lapack_complex_double*)z, iN, s,
            (lapack_complex_double*)p, iM, (lapack_complex_double*)q, iN, b);
    free(b);
#else
    Eigen::Map<EigenMatrix<Z, Eigen::RowMajor>> mapz(z, M, N);
    Eigen::Map<Eigen::Matrix<S, Eigen::Dynamic, 1>> maps(s, std::min(M, N));
    Eigen::Map<EigenMatrix<P, Eigen::RowMajor>> mapp(p, M, M);
    Eigen::Map<EigenMatrix<Q, Eigen::RowMajor>> mapq(q, N, N);
    Eigen::BDCSVD<EigenMatrix<Z, Eigen::RowMajor>> svd(mapz,
        Eigen::ComputeFullU | Eigen::ComputeFullV);
    maps = svd.singularValues();
    mapp = svd.matrixU();
    mapq = svd.matrixV();
#endif
}

template<typename A, typename B>
auto gesv(A* pa, B* pb, const size_t M, const size_t N)
{
    WL_THROW_IF_ABORT()
    static_assert(is_float_v<A> || is_complex_v<A>);
#if defined(WL_USE_LAPACKE)
    check_sizes<lapack_int>(M, N);
    const auto iM = lapack_int(M), iN = lapack_int(N);
    lapack_int info = 0;
    ndarray<lapack_int, 1u> ipiv(std::array<size_t, 1u>{M});

    if constexpr (std::is_same_v<A, float>)
        info = LAPACKE_sgesv(LAPACK_ROW_MAJOR, iM, iN, pa, iM, ipiv.data(),
            pb, iN);
    else if constexpr (std::is_same_v<A, double>)
        info = LAPACKE_dgesv(LAPACK_ROW_MAJOR, iM, iN, pa, iM, ipiv.data(),
            pb, iN);
    else if constexpr (std::is_same_v<A, complex<float>>)
        info = LAPACKE_cgesv(LAPACK_ROW_MAJOR, iM, iN,
            (lapack_complex_float*)pa, iM, ipiv.data(),
            (lapack_complex_float*)pb, iN);
    else if constexpr (std::is_same_v<A, complex<double>>)
        info = LAPACKE_zgesv(LAPACK_ROW_MAJOR, iM, iN,
            (lapack_complex_double*)pa, iM, ipiv.data(),
            (lapack_complex_double*)pb, iN);
    if (info < 0) throw std::logic_error(WL_ERROR_INTERNAL);
    if (info > 0) throw std::logic_error(WL_ERROR_LAPACKE_MATRIX_SINGULAR);
#else
    Eigen::Map<EigenMatrix<A, Eigen::RowMajor>> mapa(pa, M, M);
    Eigen::Map<EigenMatrix<B, Eigen::RowMajor>> mapb(pb, M, N);
    Eigen::PartialPivLU<Eigen::Ref<decltype(mapa)>> lu(mapa);
    mapb = lu.solve(mapb);
#endif
}

template<typename A, typename B>
auto gels(A* pa, B* pb, const size_t M, const size_t K, const size_t N)
{
    WL_THROW_IF_ABORT()
    static_assert(is_float_v<A> || is_complex_v<A>);
#if defined(WL_USE_LAPACKE)
    check_sizes<lapack_int>(M, K, N);
    const auto iM = lapack_int(M), iK = lapack_int(K), iN = lapack_int(N);
    lapack_int info = 0;

    if constexpr (std::is_same_v<A, float>)
        info = LAPACKE_sgelss(LAPACK_ROW_MAJOR, 'N', iM, iK, iN, pa, iK,
            pb, iN);
    else if constexpr (std::is_same_v<A, double>)
        info = LAPACKE_dgels(LAPACK_ROW_MAJOR, 'N', iM, iK, iN, pa, iK,
            pb, iN);
    else if constexpr (std::is_same_v<A, complex<float>>)
        info = LAPACKE_cgels(LAPACK_ROW_MAJOR, 'N', iM, iK, iN,
            (lapack_complex_float*)pa, iK, (lapack_complex_float*)pb, iN);
    else if constexpr (std::is_same_v<A, complex<double>>)
        info = LAPACKE_zgels(LAPACK_ROW_MAJOR, 'N', iM, iK, iN,
            (lapack_complex_double*)pa, iK, (lapack_complex_double*)pb, iN);
    if (info < 0) throw std::logic_error(WL_ERROR_INTERNAL);
    if (info > 0) throw std::logic_error(WL_ERROR_LAPACKE_MATRIX_SINGULAR);
#else
    Eigen::Map<EigenMatrix<A, Eigen::RowMajor>> mapa(pa, M, K);
    Eigen::Map<EigenMatrix<B, Eigen::RowMajor>> mapb(pb, M, N);
    Eigen::ColPivHouseholderQR<Eigen::Ref<decltype(mapa)>> qr(mapa);
    mapb = qr.solve(mapb);
#endif
}

template<typename B, bool Conjugate = false, typename X>
auto get_input_array(const X& x) -> decltype(auto)
{
    constexpr auto XR = array_rank_v<X>;
    using XV = value_type_t<X>;
#if defined(WL_USE_CBLAS)
    using C = std::conditional_t<is_complex_v<XV>,
        complex<value_type_t<B>>, value_type_t<B>>;
#else // Eigen
    using C = B;
#endif
    static_assert(XR >= 1u && is_arithmetic_v<B> && is_convertible_v<XV, C>,
        WL_ERROR_INTERNAL);
    constexpr bool same_type = std::is_same_v<XV, C> ||
        is_integral_v<XV> && is_integral_v<C> && sizeof(XV) == sizeof(C);
    if constexpr ((X::category == view_category::Array ||
        X::category == view_category::Simple) && same_type &&
        !(is_complex_v<C> && Conjugate))
    {
        return x;
    }
    else if constexpr (Conjugate)
    {
        return utils::listable_function(
            [](const auto& a) { return conjugate(C(a)); }, x);
    }
    else
    {
        return cast<ndarray<C, XR>>(x);
    }
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
    const auto& valx = blas::get_input_array<C>(x);
    const auto& valy = blas::get_input_array<C>(y);
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
            blas::dot(&z, px, py, K);
            return z;
        }
        else
        {
            const auto ret_dims = utils::dims_take<2u, YR>(valy.dims());
            ndarray<C, YR - 1u> ret(ret_dims, C(0));
            const auto N = ret.size();
            auto* pz = ret.data();
            blas::gevm(pz, px, py, K, N);
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
            blas::gemv(pz, px, py, M, K);
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
            blas::gemm(pz, px, py, M, K, N);
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

template<typename X>
auto inverse(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> == 2u, WL_ERROR_SQUARE_MATRIX);
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using P = promote_integral_t<value_type_t<XT>>;

    auto ret = cast<ndarray<P, 2u>>(std::forward<decltype(x)>(x));
    const auto M = ret.dims()[0];
    const auto N = ret.dims()[1];
    if (M != N)
        throw std::logic_error(WL_ERROR_SQUARE_MATRIX);
    if (N == 0)
        return ret;
    blas::getri(ret.data(), N);
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto pseudo_inverse(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> == 2u, WL_ERROR_SQUARE_MATRIX);
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using P = promote_integral_t<value_type_t<XT>>;

    auto valx = cast<ndarray<P, 2u>>(std::forward<decltype(x)>(x));
    const auto M = valx.dims()[0];
    const auto N = valx.dims()[1];
    WL_THROW_IF_ABORT()
    ndarray<P, 2u> ret(std::array<size_t, 2u>{N, M});
    if (M == 0u || N == 0u)
        return ret;
    Eigen::Map<blas::EigenMatrix<P, Eigen::RowMajor>> mapx(valx.data(), M, N);
    Eigen::Map<blas::EigenMatrix<P, Eigen::RowMajor>> mapy(ret.data(), N, M);
    Eigen::CompleteOrthogonalDecomposition<
        Eigen::Ref<decltype(mapx)>> co(mapx);
    mapy = co.pseudoInverse();
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename A, typename B>
auto linear_solve(A&& a, B&& b)
{
    WL_TRY_BEGIN()
    using AT = remove_cvref_t<A>;
    using BT = remove_cvref_t<B>;
    constexpr auto BR = array_rank_v<BT>;
    static_assert(array_rank_v<AT> == 2u, WL_ERROR_LINEAR_SOLVE_A);
    static_assert(BR == 1u || BR == 2u, WL_ERROR_LINEAR_SOLVE_B);
    static_assert(is_numerical_type_v<AT> && is_numerical_type_v<BT>,
        WL_ERROR_NUMERIC_ONLY);
    using P = promote_integral_t<
        common_type_t<value_type_t<AT>, value_type_t<BT>>>;

    auto vala = cast<ndarray<P, 2u>>(std::forward<decltype(a)>(a));
    auto valb = cast<ndarray<P, BR>>(std::forward<decltype(b)>(b));
    const auto M = vala.dims()[0];
    const auto N = BR == 1u ? size_t(1) : valb.dims()[1];
    if (M != vala.dims()[1])
        throw std::logic_error(WL_ERROR_LINEAR_SOLVE_A);
    if (M != valb.dims()[0])
        throw std::logic_error(WL_ERROR_LINEAR_SOLVE_B);
    if (M == 0u || N == 0u)
        return valb;
    blas::gesv(vala.data(), valb.data(), M, N);
    return valb;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename A, typename B>
auto least_squares(A&& a, B&& b)
{
    WL_TRY_BEGIN()
    using AT = remove_cvref_t<A>;
    using BT = remove_cvref_t<B>;
    constexpr auto BR = array_rank_v<BT>;
    static_assert(array_rank_v<AT> == 2u, WL_ERROR_LEAST_SQUARES_A);
    static_assert(BR == 1u || BR == 2u, WL_ERROR_LINEAR_SOLVE_B);
    static_assert(is_numerical_type_v<AT> && is_numerical_type_v<BT>,
        WL_ERROR_NUMERIC_ONLY);
    using P = promote_integral_t<
        common_type_t<value_type_t<AT>, value_type_t<BT>>>;

    auto vala = cast<ndarray<P, 2u>>(std::forward<decltype(a)>(a));
    auto valb = cast<ndarray<P, BR>>(std::forward<decltype(b)>(b));
    const auto M = vala.dims()[0];
    const auto K = vala.dims()[1];
    const auto N = BR == 1u ? size_t(1) : valb.dims()[1];
    if (M != valb.dims()[0])
        throw std::logic_error(WL_ERROR_LINEAR_SOLVE_B);
    if (M == 0u || K == 0u || N == 0u)
        return valb;
    if (M < K)
    {
        if constexpr (BR == 1u)
            valb.uninitialized_resize(std::array<size_t, 1u>{K});
        else
            valb.uninitialized_resize(std::array<size_t, 2u>{K, N});
    }
    blas::gels(vala.data(), valb.data(), M, K, N);
    if (M > K)
    {
        if constexpr (BR == 1u)
            valb.uninitialized_resize(std::array<size_t, 1u>{K});
        else
            valb.uninitialized_resize(std::array<size_t, 2u>{K, N});
    }
    return valb;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto lu_decomposition(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> == 2u, WL_ERROR_REQUIRE_ARRAY_RANK"2.");
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using P = promote_integral_t<value_type_t<XT>>;

    auto lu = cast<ndarray<P, 2u>>(std::forward<decltype(x)>(x));
    const auto M = lu.dims()[0];
    const auto N = lu.dims()[1];
    ndarray<int64_t, 1u> ipiv(std::array<size_t, 1u>{std::min(M, N)});
    value_type_t<P> cond{};
    if (ipiv.size() == 0)
        return std::make_tuple(lu, ipiv, cond);
    blas::getrf(lu.data(), ipiv.data(), &cond, M, N);
    return std::make_tuple(lu, ipiv, cond);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto cholesky_decomposition(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> == 2u, WL_ERROR_CHOLESKY);
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using P = promote_integral_t<value_type_t<XT>>;

    auto l = cast<ndarray<P, 2u>>(std::forward<decltype(x)>(x));
    const auto N = l.dims()[0];
    if (N != l.dims()[1])
        throw std::logic_error(WL_ERROR_CHOLESKY);
    if (N == 0u)
        return l;
    blas::potrf(l.data(), N);
    return l;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto schur_decomposition(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> == 2u, WL_ERROR_REQUIRE_SQUARE_MATRIX);
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using P = promote_integral_t<value_type_t<XT>>;

    auto t = cast<ndarray<P, 2u>>(std::forward<decltype(x)>(x));
    const auto N = t.dims()[0];
    if (N != t.dims()[1])
        throw std::logic_error(WL_ERROR_REQUIRE_SQUARE_MATRIX);
    ndarray<P, 2u> q(t.dims());
    if (N == 0u)
        return std::make_tuple(q, t);
    blas::gees(t.data(), q.data(), N);
    return std::make_tuple(q, t);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename T>
auto _svd_get_s(const ndarray<T, 1u>& s, const size_t M, const size_t N)
{
    const auto K = s.size();
    ndarray<T, 2u> ret(std::array<size_t, 2u>{M, N}, T(0));
    auto s_data = s.data();
    auto ret_data = ret.data();
    for (size_t i = 0; i < K; ++i)
        ret_data[i * N + i] = s_data[i];
    return ret;
}

template<typename X>
auto singular_value_decomposition(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> == 2u, WL_ERROR_REQUIRE_ARRAY_RANK"2.");
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using P = promote_integral_t<value_type_t<XT>>;

    auto valx = cast<ndarray<P, 2u>>(std::forward<decltype(x)>(x));
    const auto M = valx.dims()[0];
    const auto N = valx.dims()[1];
    ndarray<value_type_t<P>, 1u> s(std::array<size_t, 1u>{std::min(M, N)});
    ndarray<P, 2u> p(std::array<size_t, 2u>{M, M});
    ndarray<P, 2u> q(std::array<size_t, 2u>{N, N});

    if (M == 0u || N == 0u)
        return std::make_tuple(p, _svd_get_s(s, M, N), q);
    blas::gesvd(valx.data(), s.data(), p.data(), q.data(), M, N);
    return std::make_tuple(p, _svd_get_s(s, M, N), q);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Ret = int64_t, typename N>
auto identity_matrix(const N& n)
{
    WL_TRY_BEGIN()
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    if constexpr (is_integral_v<N>)
    {
        if (n < N(0))
            throw std::logic_error(WL_ERROR_IDENTITY_MATRIX);
        const auto size = size_t(n);
        ndarray<Ret, 2u> ret(std::array<size_t, 2u>{size, size}, Ret(0));
        auto ret_data = ret.data();
        for (size_t i = 0u; i < size; ++i)
            ret_data[i * size + i] = Ret(1);
        return ret;
    }
    else if constexpr (array_rank_v<N> == 1u && is_integral_v<value_type_t<N>>)
    {
        using NV = value_type_t<N>;
        static_assert(is_integral_v<NV>, WL_ERROR_IDENTITY_MATRIX);
        if (n.size() != 2u)
            throw std::logic_error(WL_ERROR_IDENTITY_MATRIX);
        NV sizes[2];
        n.copy_to(&sizes[0]);
        if (sizes[0] < NV(0) || sizes[1] < NV(0))
            throw std::logic_error(WL_ERROR_IDENTITY_MATRIX);
        const auto sizem = size_t(sizes[0]);
        const auto sizen = size_t(sizes[1]);
        const auto sizek = std::min(sizem, sizen);
        ndarray<Ret, 2u> ret(std::array<size_t, 2u>{sizem, sizen}, Ret(0));
        auto ret_data = ret.data();
        for (size_t i = 0u; i < sizek; ++i)
            ret_data[i * sizen + i] = Ret(1);
        return ret;
    }
    else
    {
        static_assert(always_false_v<N>, WL_ERROR_IDENTITY_MATRIX);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto _diagonal_matrix_impl(const X* x_data, const size_t x_size,
    const int64_t k, const size_t m, const size_t n)
{
    ndarray<X, 2u> ret(std::array<size_t, 2u>{m, n}, X(0));
    auto ret_data = ret.data() + (k >= 0 ? k : size_t(-k) * n);
    auto size = size_t(std::min(int64_t(x_size), std::max(std::min(
        int64_t(n) - (k >= 0 ? k : 0), int64_t(m) + (k < 0 ? k : 0)),
        int64_t(0))));
    for (size_t i = 0; i < size; ++i)
        ret_data[(n + 1) * i] = x_data[i];
    return ret;
}

template<typename X, typename K, typename N>
auto diagonal_matrix(const X& x, const K& k, const N& n)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<K>, WL_ERROR_DIAGONAL_MATRIX_K);
    static_assert(array_rank_v<X> == 1u && is_numerical_type_v<X>,
        WL_ERROR_DIAGONAL_MATRIC_DIAG);
    const auto& valx = allows<view_category::Simple>(x);
    
    if constexpr (is_integral_v<N>)
    {
        if (n < N(0))
            throw std::logic_error(WL_ERROR_DIAGONAL_MATRIX_SIZE);
        return _diagonal_matrix_impl(valx.data(), valx.size(),
            int64_t(k), size_t(n), size_t(n));
    }
    else if constexpr (array_rank_v<N> == 1u && is_integral_v<value_type_t<N>>)
    {
        using NV = value_type_t<N>;
        static_assert(is_integral_v<NV>, WL_ERROR_DIAGONAL_MATRIX_SIZE);
        if (n.size() != 2u)
            throw std::logic_error(WL_ERROR_DIAGONAL_MATRIX_SIZE);
        NV sizes[2];
        n.copy_to(&sizes[0]);
        if (sizes[0] < NV(0) || sizes[1] < NV(0))
            throw std::logic_error(WL_ERROR_DIAGONAL_MATRIX_SIZE);
        return _diagonal_matrix_impl(valx.data(), valx.size(),
            int64_t(k), size_t(sizes[0]), size_t(sizes[1]));
    }
    else
    {
        static_assert(always_false_v<N>, WL_ERROR_DIAGONAL_MATRIX_SIZE);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename K>
auto diagonal_matrix(const X& x, const K& k)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<K>, WL_ERROR_DIAGONAL_MATRIX_K);
    static_assert(array_rank_v<X> == 1u && is_numerical_type_v<X>,
        WL_ERROR_DIAGONAL_MATRIC_DIAG);
    const auto& valx = allows<view_category::Simple>(x);
    const auto x_size = valx.size();
    return _diagonal_matrix_impl(valx.data(), x_size, int64_t(k),
        x_size + size_t(abs(k)), x_size + size_t(abs(k)));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto diagonal_matrix(const X& x)
{
    WL_TRY_BEGIN()
    return diagonal_matrix(x, 0u);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename E>
auto matrix_power(X&& x, E e)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> == 2u, WL_ERROR_REQUIRE_SQUARE_MATRIX);
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    static_assert(is_integral_v<E>, WL_ERROR_MATRIX_POWER_EXPONENT);
    using P = promote_integral_t<value_type_t<XT>>;

    const auto N = x.dims()[0];
    if (N != x.dims()[1])
        throw std::logic_error(WL_ERROR_REQUIRE_SQUARE_MATRIX);
    auto x0 = cast<ndarray<P, 2u>>(std::forward<decltype(x)>(x));
    if (e == E(0))
        return identity_matrix<P>(N);
    if (e < E(0))
        x0 = inverse(std::move(x0));
    auto ue = e >= E(0) ? size_t(e) : size_t(-e);
    if (ue == 1u)
        return x0;

    auto x1 = ndarray<P, 2u>(x0.dims());
    auto x2 = ndarray<P, 2u>(x0.dims());
    auto* src = x0.data();
    auto* dst = x1.data();
    auto* ret = x2.data();

    for (; !bool(ue & size_t(1)); ue /= 2u)
    {
        blas::gemm(dst, src, src, N, N, N, P(1), P(0));
        std::swap(src, dst);
    }
    if (ue == 1)
        return src == x0.data() ? x0 : x1;
    utils::restrict_copy_n(src, N * N, ret);
    ue -= 1u;
    for (; ue > 0u; ue /= 2u)
    {
        if (bool(ue & size_t(1)))
        {
            blas::gemm(dst, src, ret, N, N, N, P(1), P(0));
            std::swap(ret, dst);
            if (ue == 1u)
                break;
        }
        blas::gemm(dst, src, src, N, N, N, P(1), P(0));
        std::swap(src, dst);
    }
    return ret == x0.data() ? x0 : ret == x1.data() ? x1 : x2;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
