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

#include <exception>
#include <type_traits>

#include "types.h"
#include "ndarray.h"
#include "arrayview.h"
#include "numerical.h"
#include "utils.h"

namespace wl
{

template<typename Dst, typename Src>
auto set(Dst&& dst, Src&& src)
{
    using DstType = remove_cvref_t<Dst>;
    using SrcType = remove_cvref_t<Src>;
    constexpr auto dst_rank = array_rank_v<DstType>;
    constexpr auto src_rank = array_rank_v<SrcType>;
    using DstValue = value_type_t<DstType>;
    using SrcValue = value_type_t<SrcType>;

    if constexpr (src_rank == 0u)
    {
        if constexpr (dst_rank == 0u)
        { // scalar -> scalar
            static_assert(is_convertible_v<SrcType, DstType>, "badargtype");
            dst = DstType(src);
            return std::forward<decltype(src)>(src);
        }
        else
        { // scalar -> ndarray / array_view
            static_assert(is_convertible_v<SrcType, DstValue>, "badargtype");
            dst.copy_from(make_scalar_view_iterator(DstValue(src)));
            return std::forward<decltype(src)>(src);
        }
    }
    else
    {
        static_assert(dst_rank == src_rank, "badrank");
        static_assert(is_convertible_v<SrcValue, DstValue>, "badargtype");
        if (!utils::check_dims<src_rank>(src.dims_ptr(), dst.dims_ptr()))
            throw std::logic_error("baddims");

        if (!has_aliasing(src, dst))
        {
            if constexpr (SrcType::category != view_category::General)
                std::forward<decltype(dst)>(dst).copy_from(src.begin());
            else if (DstType::category != view_category::General)
                src.copy_to(dst.begin());
            else // general_view -> general_view
                indirect_view_copy(dst, src);
            return std::forward<decltype(src)>(src);
        }
        else // has aliasing
        {
            indirect_view_copy(dst, src);
            return std::forward<decltype(src)>(src);
        }
    }
}

template<typename T, size_t R, size_t N, size_t I, typename Iter, typename Tuple>
void _copy_list_array_elements(
    Iter dst, const size_t* dims, size_t size, Tuple&& elems)
{
    if constexpr (I < N)
    {
        using SrcType = remove_cvref_t<decltype(std::get<I>(elems))>;
        static_assert(array_rank_v<SrcType> == R, "badrank");
        using SrcValueType = typename SrcType::value_type;
        static_assert(is_convertible_v<SrcValueType, T>, "badargtype");
        auto elem = std::get<I>(std::forward<decltype(elems)>(elems));
        if (!utils::check_dims<R>(elem.dims_ptr(), dims))
            throw std::logic_error("baddims");
        std::move(elem).copy_to(dst);
    }
    if constexpr (I < N - 1)
        _copy_list_array_elements<T, R, N, I + 1>(dst + size, dims, size,
            std::forward<decltype(elems)>(elems));
}

template<typename T, size_t N, size_t I, typename Iter, typename Tuple>
void _copy_list_scalar_elements(Iter dst, Tuple&& elems)
{
    if constexpr (I < N)
    {
        using SrcType = remove_cvref_t<decltype(std::get<I>(elems))>;
        static_assert(is_convertible_v<SrcType, T>, "badargtype");
        *dst = std::get<I>(elems);
    }
    if constexpr (I < N - 1)
        _copy_list_scalar_elements<T, N, I + 1>(dst + 1,
            std::forward<decltype(elems)>(elems));
}

template<typename First, typename... Rest>
auto list(First&& first, Rest&&... rest)
{
    using FirstType = remove_cvref_t<First>;
    constexpr auto first_rank = array_rank_v<FirstType>;
    constexpr auto dim0 = sizeof...(rest) + 1u;
    if constexpr (first_rank == 0)
    {
        ndarray<FirstType, 1u> ret(std::array<size_t, 1u>{dim0});
        _copy_list_scalar_elements<FirstType, dim0, 0u>(
            ret.begin(),
            std::make_tuple(std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...));
        return ret;
    }
    else
    {
        constexpr auto rank = first_rank + 1u;
        std::array<size_t, rank> dims;
        dims[0] = dim0;
        std::copy_n(first.dims().begin(), first_rank, dims.data() + 1);
        using FirstValueType = typename FirstType::value_type;
        ndarray<FirstValueType, rank> ret(dims);
        size_t item_size = first.size();
        _copy_list_array_elements<FirstValueType, first_rank, dim0, 0u>(
            ret.begin(), dims.data() + 1, item_size,
            std::make_tuple(std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...));
        return ret;

    }
}

template<typename T, typename... Dims>
auto constant_array(const T& val, varg_tag, const Dims&... dims)
{
    constexpr auto val_rank = array_rank_v<T>;
    constexpr auto rep_rank = sizeof...(dims);
    constexpr auto all_rank = val_rank + rep_rank;
    if constexpr (val_rank > 0)
    {
        using ValueType = typename T::value_type;
        std::array<int64_t, all_rank> all_dims{int64_t(dims)...};
        std::copy_n(val.dims().begin(), val_rank, all_dims.data() + rep_rank);
        ndarray<ValueType, all_rank> ret(all_dims);
        size_t val_size = val.size();
        if constexpr (T::category == view_category::Array ||
            T::category == view_category::Simple)
        {
            auto iter = ret.begin();
            auto end = iter + ret.size();
            for (; iter != end; iter += val_size)
                val.copy_to(iter);
        }
        else
        {
            auto buffer = val.to_array();
            auto iter = ret.begin();
            auto end = iter + ret.size();
            for (; iter != end; iter += val_size)
                buffer.copy_to(iter);
        }
        return ret;
    }
    else
    {
        ndarray<T, all_rank> ret(
            std::array<int64_t, all_rank>{int64_t(dims)...}, val);
        return ret;
    }
}

template<size_t>
using make_all_type = all_type;

template<typename Array, typename... Indexers>
auto _part_impl3(Array&& a, Indexers&&... indexers) -> decltype(auto)
{
    using ArrayType = remove_cvref_t<Array>;
    using ValueType = typename ArrayType::value_type;
    constexpr auto is_const = array_is_const_v<Array&&>;
    static_assert(sizeof...(Indexers) <= array_rank_v<ArrayType>, "badargc");
    using return_type = typename view_detail::view_type<Indexers...>::
        template return_type<ValueType, is_const, Indexers...>;
    if constexpr (is_array_view_v<return_type>)
        return return_type(a, std::forward<decltype(indexers)>(indexers)...);
    else
        return a.data()[a.linear_pos(indexers.offset()...)];
}

template<typename Array, size_t... Is, typename... Specs>
auto _part_impl2(Array&& a, std::index_sequence<Is...>,
    Specs&&... specs) -> decltype(auto)
{
    return _part_impl3(std::forward<decltype(a)>(a), 
        make_indexer(std::forward<decltype(specs)>(specs), a.dims_[Is])...);
}

template<size_t R, typename Array, size_t... Is, typename... Specs>
auto _part_impl1(Array&& a, std::index_sequence<Is...>,
    Specs&&... specs) -> decltype(auto)
{
    return _part_impl2(std::forward<decltype(a)>(a),
        std::make_index_sequence<R>{}, 
        std::forward<decltype(specs)>(specs)..., 
        make_all_type<Is>{}...);
}

template<typename Array, typename... Specs>
auto part(Array&& a, Specs&&... specs) -> decltype(auto)
{
    using ArrayType = remove_cvref_t<Array>;
    static_assert(is_value_type_v<ArrayType>, "badargtype");
    constexpr auto R = array_rank_v<ArrayType>;
    static_assert(sizeof...(Specs) <= R, "badargc");
    if constexpr (R == 0)
        return std::forward<decltype(a)>(a);
    else if constexpr (ArrayType::category != view_category::Array)
        return part(a.to_array(), std::forward<decltype(specs)>(specs)...);
    else
    {
        return _part_impl1<R>(std::forward<decltype(a)>(a),
            std::make_index_sequence<R - sizeof...(Specs)>{},
            std::forward<decltype(specs)>(specs)...);
    }
}

template<typename Begin, typename End, typename Step>
auto range(Begin begin, End end, Step step)
{
    static_assert(is_real_v<Begin> && is_real_v<End> && is_real_v<Step>,
        "badargtype");
    using T = common_type_t<Begin, Step>;
    if constexpr (is_integral_v<T>)
    {
        if (step != Step(0))
        {
            auto diff = ptrdiff_t(wl::integer_part(end - begin));
            if ((step > Step(0) && diff < 0) || (step < Step(0) && diff > 0))
                return ndarray<T, 1u>(std::array<size_t, 1>{0u});
            else
            {
                size_t length = size_t(diff / ptrdiff_t(step)) + 1u;
                ndarray<T, 1u> ret(std::array<size_t, 1>{length});
                for (size_t i = 0; i < length; ++i, begin += step)
                    ret[i] = begin;
                return ret;
            }
        }
        else // step = 0
            throw std::logic_error("badvalue");
    }
    else
    {
        if (step != Step(0))
        {
            auto diff = T(end - begin);
            if ((step > Step(0) && diff < 0) || (step < Step(0) && diff > 0))
                return ndarray<T, 1u>(std::array<size_t, 1>{0u});
            else
            {
                size_t length = wl::integer_part(diff / ptrdiff_t(step)) + 1u;
                auto remain = T(begin + (length - 1u) * step) - T(end);
                if (step * remain > T(0))
                    --length;
                ndarray<T, 1u> ret(std::array<size_t, 1>{length});
                for (size_t i = 0; i < length; ++i)
                    ret[i] = T(begin + i * step);
                return ret;
            }
        }
        else // step = 0
            throw std::logic_error("badvalue");
    }
}

template<typename Begin, typename End>
auto range(Begin begin, End end)
{
    static_assert(is_real_v<Begin> && is_real_v<End>, "badargtype");
    return range(begin, end, int8_t(1));
}

template<typename End>
auto range(End end)
{
    static_assert(is_real_v<End>, "badargtype");
    return range(End(1), end, int8_t(1));
}

template<typename Array, int64_t I1, int64_t I2>
auto total(const Array& a, const_int<I1>, const_int<I2>)
{
    constexpr auto rank = array_rank_v<Array>;
    static_assert(rank >= 1, "badargtype");
    using ValueType = typename Array::value_type;
    constexpr int64_t L1 = I1 >= 0 ? I1 : I1 + rank + 1;
    constexpr int64_t L2 = I2 >= 0 ? I2 : I2 + rank + 1;
    static_assert(1 <= L1 && L1 <= L2 && L2 <= rank, "badlevel");

    if constexpr (Array::category == view_category::General)
        return total(a.to_array(), const_int<I1>{}, const_int<I2>{});
    else
    {
        if constexpr (L1 == 1)
        {
            if constexpr (L2 == rank)
            {
                auto ret = ValueType{};
                a.for_each([&](const auto& val) { ret += val; });
                return ret;
            }
            else
            {
                auto ret_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                ndarray<ValueType, rank - L2> ret(ret_dims);
                const auto inter_size = utils::size_of_dims(
                    utils::dims_take<L1, L2>(a.dims()));
                const auto inner_size = ret.size();
                auto iter = a.begin();
                for (size_t j = 0; j < inter_size; ++j)
                    for (size_t k = 0; k < inner_size; ++k)
                        ret[k] += *iter++;
                return ret;
            }
        }
        else // L1 > 1
        {
            if constexpr (L2 == rank)
            {
                auto ret_dims = utils::dims_take<1, L1 - 1>(a.dims());
                ndarray<ValueType, L1 - 1> ret(ret_dims);
                const auto outer_size = ret.size();
                const auto inter_size = utils::size_of_dims(
                    utils::dims_take<L1, L2>(a.dims()));
                auto iter = a.begin();
                for (size_t i = 0; i < outer_size; ++i)
                    for (size_t j = 0; j < inter_size; ++j)
                        ret[i] += *iter++;
                return ret;
            }
            else
            {
                auto outer_dims = utils::dims_take<1, L1 - 1>(a.dims());
                auto inter_dims = utils::dims_take<L1, L2>(a.dims());
                auto inner_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                auto ret_dims = utils::dims_join(outer_dims, inner_dims);
                ndarray<ValueType, rank - (L2 - L1 + 1)> ret(ret_dims);
                auto iter = a.begin();
                const auto outer_size = utils::size_of_dims(outer_dims);
                const auto inter_size = utils::size_of_dims(inter_dims);
                const auto inner_size = utils::size_of_dims(inner_dims);
                for (size_t i = 0; i < outer_size; ++i)
                    for (size_t j = 0; j < inter_size; ++j)
                        for (size_t k = 0; k < inner_size; ++k)
                            ret[i * inner_size + k] += *iter++;
                return ret;
            }
        }
    }
}

template<typename Array, int64_t I>
auto total(const Array& a, const_int<I>)
{
    return total(a, const_int<1>{}, const_int<I>{});
}

template<typename Array>
auto total(const Array& a)
{
    return total(a, const_int<1>{}, const_int<1>{});
}

template<typename Array>
auto mean(const Array& a)
{
    return divide(total(a), a.template dimension<1>());
}

template<typename Array>
auto dimensions(const Array& a)
{
    constexpr auto rank = array_rank_v<Array>;
    if constexpr (rank == 0)
        return ndarray<int64_t, 1>();
    else
        return ndarray<int64_t, 1>(std::array<size_t, 1>{rank}, 
            a.dims_ptr(), a.dims_ptr() + rank);
}

}
