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
    for (size_t m = 0u; m < M; ++m)
        _dot_vv(pz + m, px + m * K, py, K);
}

template<typename Z, typename X, typename Y>
auto _dot_vm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const size_t N)
{
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
    for (size_t m = 0; m < M; ++m)
        _dot_vm(pz + m * N, px + m * K, py, K, N);
}

template<typename X, typename Y>
auto dot(const X& x, const Y& y)
{
    static_assert(is_numerical_type_v<X> && is_numerical_type_v<Y>,
        WL_ERROR_NUMERIC_ONLY);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR >= 1u && YR >= 1u);
    using XV = value_type_t<X>;
    using YV = value_type_t<Y>;
    using C = common_type_t<XV, YV>;
    const auto& valx = allows<view_category::Simple>(x);
    const auto& valy = allows<view_category::Simple>(y);
    const auto* px = valx.data();
    const auto* py = valy.data();

    const auto K = valx.dims()[XR - 1u];
    if (K != valy.dims()[0])
        throw std::logic_error("baddims");

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
}

}
