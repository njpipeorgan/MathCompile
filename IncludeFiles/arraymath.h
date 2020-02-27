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
#include <complex>
#include <functional>
#include <type_traits>

#include "types.h"
#include "ndarray.h"
#include "arrayview.h"
#include "arithmetic.h"
#include "numerical.h"
#include "arrayfn.h"
#include "mathfunction.h"
#include "utils.h"

namespace wl
{

template<typename Array, int64_t I1, int64_t I2>
auto total(const Array& a, const_int<I1>, const_int<I2>)
{
    WL_TRY_BEGIN()
    constexpr auto rank = array_rank_v<Array>;
    static_assert(rank >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<Array>;
    constexpr int64_t L1 = I1 >= 0 ? I1 : I1 + int64_t(rank) + 1;
    constexpr int64_t L2 = I2 >= 0 ? I2 : I2 + int64_t(rank) + 1;
    static_assert(1 <= L1 && L1 <= L2 && L2 <= int64_t(rank),
        WL_ERROR_BAD_LEVEL);

    if constexpr (Array::category == view_category::General)
        return total(a.to_array(), const_int<I1>{}, const_int<I2>{});
    else
    {
        WL_THROW_IF_ABORT()
            if constexpr (L1 == 1)
            {
                if constexpr (L2 == rank)
                {
                    const auto inter_size = a.size();
                    auto a_iter = a.begin();
                    auto ret = XV{};
                    for (size_t j = 0; j < inter_size; ++j, ++a_iter)
                        ret += *a_iter;
                    return ret;
                }
                else
                {
                    auto ret_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                    ndarray<XV, rank - L2> ret(ret_dims, XV{});
                    const auto inter_size = utils::size_of_dims(
                        utils::dims_take<L1, L2>(a.dims()));
                    const auto inner_size = ret.size();
                    auto a_iter = a.begin();
                    auto ret_iter = ret.begin();
                    for (size_t j = 0; j < inter_size; ++j)
                        for (size_t k = 0; k < inner_size; ++k, ++a_iter)
                            ret_iter[k] += *a_iter;
                    return ret;
                }
            }
            else // L1 > 1
            {
                if constexpr (L2 == rank)
                {
                    auto ret_dims = utils::dims_take<1, L1 - 1>(a.dims());
                    ndarray<XV, L1 - 1> ret(ret_dims);
                    const auto outer_size = ret.size();
                    const auto inter_size = utils::size_of_dims(
                        utils::dims_take<L1, L2>(a.dims()));
                    auto a_iter = a.begin();
                    auto ret_iter = ret.begin();
                    for (size_t i = 0; i < outer_size; ++i, ++ret_iter)
                    {
                        auto sum = XV{};
                        for (size_t j = 0; j < inter_size; ++j, ++a_iter)
                            sum += *a_iter;
                        *ret_iter = sum;
                    }
                    return ret;
                }
                else
                {
                    auto outer_dims = utils::dims_take<1, L1 - 1>(a.dims());
                    auto inter_dims = utils::dims_take<L1, L2>(a.dims());
                    auto inner_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                    auto ret_dims = utils::dims_join(outer_dims, inner_dims);
                    ndarray<XV, rank - (L2 - L1 + 1)> ret(ret_dims, XV{});
                    auto a_iter = a.begin();
                    auto ret_iter = ret.begin();
                    const auto outer_size = utils::size_of_dims(outer_dims);
                    const auto inter_size = utils::size_of_dims(inter_dims);
                    const auto inner_size = utils::size_of_dims(inner_dims);
                    for (size_t i = 0; i < outer_size; ++i, ret_iter += inner_size)
                        for (size_t j = 0; j < inter_size; ++j)
                            for (size_t k = 0; k < inner_size; ++k, ++a_iter)
                                ret_iter[k] += *a_iter;
                    return ret;
                }
            }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Array, int64_t I>
auto total(const Array& a, const_int<I>)
{
    WL_TRY_BEGIN()
    return total(a, const_int<1>{}, const_int<I>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename Array>
auto total(const Array& a)
{
    WL_TRY_BEGIN()
    if constexpr (array_rank_v<Array> == 0u)
        return a;
    else
        return total(a, const_int<1>{}, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto mean(const X& x)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    using RV = promote_integral_t<value_type_t<X>>;

    if constexpr (XR == 1u)
    {
        RV total = 0;
        x.for_each([&](const auto& a) { total += RV(a); });
        return total / RV(x.size());
    }
    else
    {
        const auto& valx = allows<view_category::Simple>(x);
        ndarray<RV, XR - 1u> ret(utils::dims_take<2u, XR>(valx.dims()), 0);
        const auto row_size = valx.dims()[0];
        const auto col_size = ret.size();
        WL_THROW_IF_ABORT();
        auto x_data = valx.data();
        auto ret_data = ret.data();
        for (size_t i = 0; i < row_size; ++i, x_data += col_size)
            for (size_t j = 0; j < col_size; ++j)
                ret_data[j] += RV(x_data[j]);
        const auto row_size_inv = RV(1) / RV(row_size);
        for (size_t j = 0; j < col_size; ++j)
            ret_data[j] *= row_size_inv;
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename XV>
auto _accumulate_impl(XV* data, const size_t total_size,
    const size_t inner_size)
{
    if (total_size == 0u)
        return;
    if (inner_size == 1u)
    {
        for (size_t i = 1u; i < total_size; ++i)
            data[i] += data[i - 1];
    }
    else if (inner_size > 1u)
    {
        for (size_t i = inner_size; i < total_size; ++i)
            data[i] += data[i - inner_size];
    }
}

template<typename XV>
auto _accumulate_impl(const XV* input_data, XV* output_data,
    const size_t total_size, const size_t inner_size)
{
    WL_THROW_IF_ABORT();
    if (total_size == 0u)
        return;
    if (inner_size == 1u)
    {
        output_data[0] = input_data[0];
        for (size_t i = 1u; i < total_size; ++i)
            output_data[i] = output_data[i - 1] + input_data[i];
    }
    else if (inner_size > 1u)
    {
        for (size_t i = 0; i < inner_size; ++i)
            output_data[i] = input_data[i];
        for (size_t i = inner_size; i < total_size; ++i)
            output_data[i] = output_data[i - inner_size] + input_data[i];
    }
}

template<typename X>
auto accumulate(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    using XV = value_type_t<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    if constexpr (XT::category == view_category::Array ||
        XT::category == view_category::Simple)
    {
        const auto total_size = x.size();
        const auto inner_size = (XR == 1u) ?
            size_t(1) : utils::size_of_dims<XR - 1u>(&x.dims()[1]);
        auto input_data = x.data();
        if constexpr (is_array_v<XT> && is_movable_v<X&&>)
        {
            _accumulate_impl(input_data, total_size, inner_size);
            return std::move(x);
        }
        else
        {
            ndarray<XV, XR> ret(x.dims());
            auto output_data = ret.data();
            _accumulate_impl(input_data, output_data, total_size, inner_size);
            return ret;
        }
    }
    else
    {
        return accumulate(x.to_array());
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<size_t XR, typename Any>
auto _differences_general_get_orders(const Any& any)
{
    std::array<int64_t, XR> orders{};
    if constexpr (is_integral_v<Any>)
    {
        orders[0] = int64_t(any);
    }
    else if constexpr (array_rank_v<Any> == 1u &&
        is_integral_v<value_type_t<Any>>)
    {
        if (any.size() > XR)
            throw std::logic_error(WL_ERROR_DIFFERENCES_ORDER_COUNT);
        any.copy_to(orders.data());
    }
    else
    {
        static_assert(always_false_v<Any>, WL_ERROR_DIFFERENCES_ORDER);
    }
    for (size_t i = 0; i < XR; ++i)
    {
        if (orders[i] < 0)
            throw std::logic_error(WL_ERROR_DIFFERENCES_ORDER_NEGATIVE);
    }
    return orders;
}

template<size_t XR>
auto _differences_general_get_strides(const std::array<size_t, XR>& dims)
{
    std::array<int64_t, XR + 1u> strides{};
    strides[XR] = 1u;
    for (size_t i = XR; i > 0; --i)
        strides[i - 1u] = strides[i] * dims[i - 1u];
    return strides;
}

template<bool UseRatio, typename XV>
void _differences_general_impl(XV* data, const size_t total_size,
    const size_t mid_size, const size_t diff_size, const size_t inner_size)
{
    WL_THROW_IF_ABORT()
    const auto data_end = data + total_size;
    if (inner_size == 1u)
    {
        for (; data < data_end; data += mid_size)
            for (size_t i = 0; i < diff_size; ++i)
            {
                if constexpr (UseRatio)
                    data[i] = data[i + 1u] / data[i];
                else
                    data[i] = data[i + 1u] - data[i];
            }
    }
    else if (inner_size > 1u)
    {
        for (; data < data_end; data += mid_size)
            for (size_t i = 0; i < diff_size; ++i)
            {
                if constexpr (UseRatio)
                    data[i] = data[i + inner_size] / data[i];
                else
                    data[i] = data[i + inner_size] - data[i];
            }
    }
}

template<typename XV, size_t XR, size_t... Is>
auto _differences_general_crop(ndarray<XV, XR>&& x,
    const std::array<int64_t, XR>& orders, std::index_sequence<Is...>)
{
    auto x_dims = x.dims();
    for (size_t i = 0; i < XR; ++i)
    {
        if (x_dims[i] > size_t(orders[i]))
            x_dims[i] -= size_t(orders[i]);
        else
            x_dims[i] = 0u;
    }
    return val(part(x, make_span(size_t(1), x_dims[Is])...));
}

template<bool UseRatio, typename XV, size_t XR, typename Any>
auto _differences_general(ndarray<XV, XR>&& x, const Any& any)
{
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto total_size = x.size();
    const auto orders = _differences_general_get_orders<XR>(any);
    const auto strides = _differences_general_get_strides(x.dims());
    auto x_data = x.data();
    for (size_t level = 0u; level < XR; ++level)
    {
        if (size_t(orders[level]) >= x.dims()[level])
            continue;
        const auto mid_size = strides[level];
        const auto inner_size = strides[level + 1u];
        auto diff_size = mid_size;
        for (int64_t order = 0; order < orders[level]; ++order)
        {
            diff_size -= inner_size;
            if (diff_size == 0u)
                break;
            _differences_general_impl<UseRatio>(x_data, total_size, mid_size,
                diff_size, inner_size);
        }
    }
    return _differences_general_crop(std::move(x), orders,
        std::make_index_sequence<XR>{});
}

template<typename X, typename Any>
auto differences(X&& x, const Any& any)
{
    WL_TRY_BEGIN()
    if constexpr (is_array_v<remove_cvref_t<X>> && is_movable_v<X&&>)
    {
        return _differences_general<false>(std::move(x), any);
    }
    else
    {
        auto x_copy = x.to_array();
        return _differences_general<false>(std::move(x_copy), any);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto differences(X&& x)
{
    WL_TRY_BEGIN()
    return differences(std::forward<decltype(x)>(x), 1);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Any>
auto ratios(X&& x, const Any& any)
{
    WL_TRY_BEGIN()
    if constexpr (is_array_v<remove_cvref_t<X>> && is_movable_v<X&&> &&
        !is_integral_v<value_type_t<remove_cvref_t<X>>>)
    {
        return _differences_general<true>(std::move(x), any);
    }
    else
    { // need to copy
        using XT = remove_cvref_t<X>;
        constexpr auto XR = array_rank_v<XT>;
        using PV = wl::promote_integral_t<value_type_t<XT>>;
        auto x_copy = cast<ndarray<PV, XR>>(x);
        return _differences_general<true>(std::move(x_copy), any);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto ratios(X&& x)
{
    WL_TRY_BEGIN()
    return ratios(std::forward<decltype(x)>(x), 1);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename XV, typename UX, typename P>
auto _norm_impl_real(const XV* begin, const XV* end, const UX max, P p)
{
    const auto max_inv = UX(1) / max;
    UX total = 0.0;
    for (auto iter = begin; iter < end; ++iter)
        total += wl::power(std::abs(UX(*iter)) * max_inv, p);
    if constexpr (is_const_int_v<P>)
        return max * wl::power(total, UX(1) / UX(P::value));
    else
        return max * wl::power(total, UX(1) / UX(p));
}

template<typename XV, typename UX, typename P>
auto _norm_impl_complex(const XV* begin, const XV* end, const UX abs2_max, P p)
{
    const auto max_inv = UX(1) / abs2_max;
    UX total = 0.0;
    if constexpr (is_const_int_v<P>)
    {
        for (auto iter = begin; iter < end; ++iter)
        {
            if constexpr (!(P::value & int64_t(1)))
                total += wl::power(*iter * max_inv, UX(P::value) / UX(2));
            else
                total += wl::power(*iter * max_inv,
                    const_int<P::value / 2>{});
        }
    }
    else if (is_integral_v<P> && !(p & P(1)))
    { // p is even integer
        const auto p2 = p / 2;
        for (auto iter = begin; iter < end; ++iter)
            total += wl::power(*iter * max_inv, p2);
    }
    else
    {
        const auto p2 = UX(p) / UX(2);
        for (auto iter = begin; iter < end; ++iter)
            total += wl::power(*iter * max_inv, p2);
    }
    if constexpr (is_const_int_v<P>)
        return std::sqrt(abs2_max) * wl::power(total, UX(1) / UX(P::value));
    else
        return std::sqrt(abs2_max) * wl::power(total, UX(1) / UX(p));
}

template<typename X, typename P>
auto _norm_list(const X& x, const P& p)
{
    static_assert(array_rank_v<X> == 1u, WL_ERROR_REQUIRE_ARRAY_RANK"1.");
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    static_assert(is_const_int_v<P> || is_arithmetic_v<P>, WL_ERROR_NORM_P);
    if constexpr (is_const_int_v<P>)
        static_assert(P::value >= 1, WL_ERROR_NORM_P);
    else if (!(p >= P(1)))
        throw std::logic_error(WL_ERROR_NORM_P);
    using XV = value_type_t<X>;
    bool calculate_total = false;
    if constexpr (is_const_int_v<P>)
        calculate_total = (P::value == 1);
    else
        calculate_total = (p == 1);

    if (calculate_total)
    {
        using Ret = promote_integral_t<value_type_t<XV>>;
        Ret ret = 0;
        if constexpr (is_real_v<XV>)
            x.for_each([&](const auto& a) { ret += wl::abs(Ret(a)); });
        else
            x.for_each([&](const auto& a) { ret += wl::abs(a); });
        return ret;
    }

    const auto& valx = allows<view_category::Simple>(x);
    const auto x_data = valx.data();
    const auto x_size = valx.size();
    
    if constexpr (is_integral_v<XV>)
    {
        using UX = decltype(std::make_unsigned_t<XV>{} + unsigned{});
        UX abs_max = 0u;
        for (size_t i = 0; i < x_size; ++i)
        {
            if constexpr (std::is_unsigned_v<XV>)
            {
                if (x_data[i] > abs_max)
                    abs_max = x_data[i];
            }
            else
            {
                auto ux = UX(x_data[i]);
                if (ux >= UX(std::numeric_limits<XV>::min()))
                    ux = ~ux + UX(1);
                if (ux > abs_max)
                    abs_max = ux;
            }
        }
        if (abs_max == UX(0))
            return double(0.0);
        else
            return _norm_impl_real(x_data, x_data + x_size,
                double(abs_max), p);
    }
    else if constexpr (is_float_v<XV>)
    {
        XV abs_max = 0;
        for (size_t i = 0; i < x_size; ++i)
        {
            auto ux = std::abs(x_data[i]);
            if (ux > abs_max)
                abs_max = ux;
        }
        if (abs_max == XV(0))
            return XV(0.0);
        else
            return _norm_impl_real(x_data, x_data + x_size, abs_max, p);
    }
    else // complex
    {
        using XRV = value_type_t<XV>;
        ndarray<XRV, 1u> abs2(valx.dims());
        const auto abs2_data = abs2.data();
        XRV abs2_max = 0;
        for (size_t i = 0; i < x_size; ++i)
        {
            const auto re = x_data[i].real();
            const auto im = x_data[i].imag();
            const auto abs2 = re * re + im * im;
            abs2_data[i] = abs2;
            if (abs2 > abs2_max)
                abs2_max = abs2;
        }
        if (abs2_max == XRV(0))
            return XRV(0.0);
        else
            return _norm_impl_complex(abs2.begin(), abs2.end(), abs2_max, p);
    }
}

template<typename X, typename P>
auto norm(const X& x, const P& p)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> <= 1u, WL_ERROR_REQUIRE_ARRAY_RANK"1.");
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    static_assert(is_const_int_v<P> || is_arithmetic_v<P>, WL_ERROR_NORM_P);
    if constexpr (is_const_int_v<P>)
        static_assert(P::value >= 1, WL_ERROR_NORM_P);
    else if (!(p >= P(1)))
        throw std::logic_error(WL_ERROR_NORM_P);

    if constexpr (array_rank_v<X> == 0u)
        return wl::abs(x);
    else
        return _norm_list(x, p);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto norm(const X& x)
{
    return norm(x, const_int<2>{});
}

template<typename X, typename Function>
auto normalize(const X& x, Function f)
{
    WL_TRY_BEGIN()
    static_assert(std::is_convertible_v<Function, std::function<double(X)>>,
        WL_ERROR_NORMALIZE_NORM);
    static_assert(array_rank_v<X> <= 1u, WL_ERROR_REQUIRE_ARRAY_RANK"1.");
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);

    const auto x_norm = f(x);
    if (x_norm == 0)
        return cast<decltype(divide(x, x_norm))>(x);
    else
        return divide(x, x_norm);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto normalize(const X& x)
{
    return normalize(x,
        [](const auto& v) { return norm(v, const_int<2>{}); });
}

template<typename Iter>
auto _variance_impl(Iter data, const size_t size)
{
    WL_THROW_IF_ABORT();
    if (size < 2u)
        throw std::logic_error(WL_ERROR_VARIANCE_ELEMENTS);
    using P = promote_integral_t<remove_cvref_t<decltype(*data)>>;
    using RP = value_type_t<P>;
    P ref_val = *data;
    P total = 0;
    RP square_total = 0;
    for (size_t i = 0; i < size; ++i, ++data)
    {
        auto val = P(*data) - ref_val;
        total += val;
        square_total += _scalar_abs_square(val);
    }
    const RP total_square = _scalar_abs_square(total);
    return std::max(RP(0),
        (square_total - total_square / RP(size)) / RP(size - 1));
}

template<typename Iter, typename RP>
void _variance_impl(const Iter data, RP* ret_data, const size_t size,
    const size_t stride)
{
    WL_THROW_IF_ABORT();
    if (size < 2u)
        throw std::logic_error(WL_ERROR_VARIANCE_ELEMENTS);
    using P = promote_integral_t<remove_cvref_t<decltype(*data)>>;
    ndarray<P, 1u> totals(std::array<size_t, 1u>{stride}, P(0));
    auto totals_data = totals.data();

    Iter all_data = data;
    for (size_t i = 0; i < size; ++i)
    {
        Iter ref_data = data;
        for (size_t j = 0; j < stride; ++j, ++all_data, ++ref_data)
        {
            auto val = P(*all_data) - P(*ref_data);
            totals_data[j] += val;
            ret_data[j] += _scalar_abs_square(val);
        }
    }
    for (size_t j = 0; j < stride; ++j)
    {
        const RP total_square = _scalar_abs_square(totals_data[j]);
        ret_data[j] = std::max(RP(0),
            (ret_data[j] - total_square / RP(size)) / RP(size - 1));
    }
}

template<typename X>
auto variance(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    constexpr auto XR = array_rank_v<X>;
    using P = promote_integral_t<value_type_t<X>>;
    using RP = value_type_t<P>;

    const auto& valx = allows<view_category::Simple>(x);
    if constexpr (XR == 1u)
    {
        return _variance_impl(valx.begin(), valx.size());
    }
    else
    {
        ndarray<RP, XR - 1u> ret(utils::dims_take<2u, XR>(x.dims()), RP(0));
        _variance_impl(valx.begin(), ret.data(), valx.dims()[0], ret.size());
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto standard_deviation(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    return sqrt(variance(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename RV, typename Var, typename Avg>
auto _standardize_impl(RV* ret_data, Var* var_data, const Avg* avg_data,
    const size_t size, const size_t stride)
{
    WL_THROW_IF_ABORT();
    for (size_t j = 0; j < stride; ++j)
        var_data[j] = Var(1) / std::sqrt(var_data[j]);

    for (size_t i = 0; i < size; ++i, ret_data += stride)
    {
        for (size_t j = 0; j < stride; ++j)
            ret_data[j] = (ret_data[j] - avg_data[j]) * var_data[j];
    }
}

template<typename RV, typename Var, typename Avg>
auto _standardize_impl(RV* ret_data, Var var, const Avg avg, const size_t size)
{
    WL_THROW_IF_ABORT();
    var = Var(1) / std::sqrt(var);
    for (size_t i = 0; i < size; ++i)
        ret_data[i] = (ret_data[i] - avg) * var;
}

template<typename X>
auto standardize(const X& x)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    using RV = promote_integral_t<value_type_t<X>>;
    auto ret = cast<ndarray<RV, XR>>(x);
    auto var = variance(ret);
    auto avg = mean(ret);
    if constexpr (XR == 1u)
        _standardize_impl(ret.data(), var, avg, ret.size());
    else
        _standardize_impl(ret.data(), var.data(), avg.data(), ret.dims()[0],
            var.size());
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto mean_deviation(const X& x)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    using RV = promote_integral_t<value_type_t<X>>;
    const auto& valx = allows<view_category::Simple>(x);
    const auto avg = mean(valx);

    if constexpr (XR == 1u)
    {
        RV total = 0;
        valx.for_each([&](const auto& a) { total += std::abs(RV(a) - avg); });
        return total / RV(valx.size());
    }
    else
    {
        ndarray<RV, XR - 1u> ret(utils::dims_take<2u, XR>(valx.dims()), 0);
        const auto row_size = valx.dims()[0];
        const auto col_size = ret.size();
        WL_THROW_IF_ABORT();
        auto x_data = valx.data();
        auto ret_data = ret.data();
        auto avg_data = avg.data();
        for (size_t i = 0; i < row_size; ++i, x_data += col_size)
            for (size_t j = 0; j < col_size; ++j)
                ret_data[j] += std::abs(RV(x_data[j]) - avg_data[j]);
        const auto row_size_inv = RV(1) / RV(row_size);
        for (size_t j = 0; j < col_size; ++j)
            ret_data[j] *= row_size_inv;
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename XV>
auto _median_impl(XV* data, const size_t size)
{
    WL_THROW_IF_ABORT()
    using RV = promote_integral_t<XV>;
    if (size == 0u)
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);
    else if (size == 1u)
        return RV(data[0]);
    else if (size == 2u)
        return RV(0.5) * RV(data[0]) + RV(0.5) * RV(data[1]);
    else
    {
        const auto size2 = size >> 1;
        std::nth_element(data, data + size2, data + size);
        auto median = RV(data[size2]);
        if (!(size & size_t(1)))
        {
            auto prev = std::max_element(data, data + size2);
            median = RV(0.5) * median + RV(0.5) * RV(*prev);
        }
        return median;
    }
}

template<typename X>
auto median(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    using XV = value_type_t<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
    
    if constexpr (XR == 1u)
    {
        auto valx = std::forward<decltype(x)>(x).to_array();
        return _median_impl(valx.data(), valx.size());
    }
    else
    {
        const auto& valx = allows<view_category::Simple>(
            std::forward<decltype(x)>(x));
        using RV = promote_integral_t<XV>;
        ndarray<RV, 1u> ret(utils::dims_take<2u, XR>(valx.dims()));
        const size_t col_size = ret.size();
        const size_t row_size = valx.dims()[0];
        ndarray<XV, 1u> slice(std::array<size_t, 1u>{row_size});

        auto ret_data = ret.data();
        auto x_data = valx.data();
        auto slice_data = slice.data();
        for (size_t i = 0; i < col_size; ++i, ++x_data, ++ret_data)
        {
            for (size_t j = 0; j < row_size; ++j)
                slice_data[j] = x_data[j * col_size];
            *ret_data = _median_impl(slice_data, row_size);
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename RV>
auto _median_deviation_impl(RV* data, const size_t size)
{
    WL_THROW_IF_ABORT()
    if (size == 0u)
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);
    else if (size == 1u)
        return RV(0);
    else if (size == 2u)
        return std::abs(RV(0.5) * data[0] - RV(0.5) * data[1]);
    else
    {
        const auto size2 = size >> 1;
        std::nth_element(data, data + size2, data + size);
        auto median = data[size2];
        if (!(size & size_t(1)))
        {
            auto prev = std::max_element(data, data + size2);
            median = RV(0.5) * median + RV(0.5) * (*prev);
            std::swap(data[size2 - 1u], *prev);
        }
        for (size_t i = 0; i < size2; ++i)
            data[i] = median - data[i];
        for (size_t i = size2; i < size; ++i)
            data[i] = data[i] - median;
        return _median_impl(data, size);
    }
}

template<typename X>
auto median_deviation(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    using XV = value_type_t<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);

    using RV = promote_integral_t<XV>;
    auto valx = cast<ndarray<RV, XR>>(std::forward<decltype(x)>(x));
    if constexpr (XR == 1u)
    {
        return _median_deviation_impl(valx.data(), valx.size());
    }
    else
    {
        ndarray<RV, 1u> ret(utils::dims_take<2u, XR>(valx.dims()));
        const size_t col_size = ret.size();
        const size_t row_size = valx.dims()[0];
        ndarray<RV, 1u> slice(std::array<size_t, 1u>{row_size});

        auto ret_data = ret.data();
        auto x_data = valx.data();
        auto slice_data = slice.data();
        for (size_t i = 0; i < col_size; ++i, ++x_data, ++ret_data)
        {
            for (size_t j = 0; j < row_size; ++j)
                slice_data[j] = x_data[j * col_size];
            *ret_data = _median_deviation_impl(slice_data, row_size);
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
