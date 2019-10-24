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
        if constexpr (is_array_v<DstType>)
        {
            static_assert(!is_movable_v<Dst&&>, "badargtype");
            static_assert(std::is_same_v<SrcValue, DstValue>, "badargtype");
            if (!has_aliasing(src, dst))
                dst = std::forward<decltype(src)>(src).to_array();
            else
                dst = std::forward<decltype(src)>(src);
        }
        else
        {
            static_assert(is_convertible_v<SrcValue, DstValue>, "badargtype");
            if (!utils::check_dims(src.dims(), dst.dims()))
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
}

template<typename Iter, size_t R>
void _copy_list_array_elements(Iter&, const std::array<size_t, R>&)
{
}

template<typename Iter, size_t R, typename First, typename... Rest>
void _copy_list_array_elements(Iter& ret_iter,
    const std::array<size_t, R>& dims, First&& first, Rest&&... rest)
{
    using FirstType = remove_cvref_t<First>;
    using T = value_type_t<remove_cvref_t<decltype(*ret_iter)>>;
    if constexpr (is_argument_pack_v<FirstType>)
    {
        using ItemType = value_type_t<FirstType>;
        static_assert(array_rank_v<ItemType> == R, "badrank");
        using ValueType = value_type_t<ItemType>;
        static_assert(is_convertible_v<ValueType, T>, "badargtype");
        const auto size = first.size();
        for (size_t i = 0u; i < size; ++i, ++ret_iter)
        {
            const auto& item = first.get(i);
            if (!utils::check_dims(item.dims(), dims))
                throw std::logic_error("baddims");
            item.copy_to(ret_iter.begin());
        }
    }
    else
    {
        static_assert(array_rank_v<FirstType> == R, "badrank");
        using ValueType = value_type_t<FirstType>;
        static_assert(is_convertible_v<ValueType, T>, "badargtype");
        if (!utils::check_dims(first.dims(), dims))
            throw std::logic_error("baddims");
        first.copy_to(ret_iter.begin());
        ++ret_iter;
    }
    _copy_list_array_elements(ret_iter, dims,
        std::forward<decltype(rest)>(rest)...);
}

template<typename T>
void _copy_list_scalar_elements(T*&)
{
}

template<typename T, typename First, typename... Rest>
void _copy_list_scalar_elements(T*& ret_iter, First&& first, Rest&&... rest)
{
    using FirstType = remove_cvref_t<First>;
    if constexpr (is_argument_pack_v<FirstType>)
    {
        using ItemType = value_type_t<FirstType>;
        static_assert(is_convertible_v<ItemType, T>, "badargtype");
        const auto size = first.size();
        for (size_t i = 0; i < size; ++i, ++ret_iter)
            *ret_iter = cast<T>(first.get(i));
    }
    else
    {
        static_assert(is_convertible_v<FirstType, T>, "badargtype");
        *ret_iter = cast<T>(std::forward<decltype(first)>(first));
        ++ret_iter;
    }
    _copy_list_scalar_elements(ret_iter,
        std::forward<decltype(rest)>(rest)...);
}


template<typename Any>
auto _list_length_by_args_impl(const Any& any)
{
    return size_t(1);
}

template<typename Iter, bool HasStride>
auto _list_length_by_args_impl(const argument_pack<Iter, HasStride>& args)
{
    return args.size();
}

template<typename... Elems>
auto _list_length_by_args(const Elems&... elems)
{
    return (_list_length_by_args_impl(elems) + ...);
};

template<typename First, typename... Rest>
auto list(First&& first, Rest&&... rest)
{
    using FirstType = remove_cvref_t<First>;
    if constexpr (is_argument_pack_v<FirstType>)
    {
        if (first.size() == 0u)
        {
            if constexpr (sizeof...(rest) == 0u)
            {
                using ItemType = value_type_t<FirstType>;
                constexpr auto ret_rank = array_rank_v<ItemType> +1u;
                return ndarray<value_type_t<ItemType>, ret_rank>{};
            }
            else
                return list(std::forward<decltype(rest)>(rest)...);
        }
        else
            return list(first.get(0), first.get_pack(1),
                std::forward<decltype(rest)>(rest)...);
    }
    else
    {
        constexpr auto first_rank = array_rank_v<FirstType>;
        const auto dim0 = _list_length_by_args(first, rest...);
        if constexpr (first_rank == 0)
        {
            ndarray<FirstType, 1u> ret(std::array<size_t, 1u>{dim0});
            auto ret_iter = ret.data();
            _copy_list_scalar_elements(ret_iter,
                std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...);
            return ret;
        }
        else
        {
            constexpr auto rank = first_rank + 1u;
            const auto dims = utils::dims_join(
                std::array<size_t, 1u>{dim0}, first.dims());
            using FirstValueType = value_type_t<FirstType>;
            ndarray<FirstValueType, rank> ret(dims);
            auto ret_iter = ret.template view_begin<1u>();
            _copy_list_array_elements(ret_iter, first.dims(),
                std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...);
            return ret;
        }
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
    static_assert(sizeof...(Indexers) <= array_rank_v<ArrayType>, "internal");
    using return_type = typename view_detail::view_type<Indexers...>::
        template return_type<ValueType, is_const, Indexers...>;

    if constexpr (is_array_view_v<return_type>)
    {
        auto view = return_type(a.identifier(), a.data(), a.dims(),
            std::forward<decltype(indexers)>(indexers)...);
        if constexpr (is_movable_v<Array&&>)
            return std::move(view).to_array();
        else
            return view;
    }
    else
    {
        auto& data_ref = a.data()[
            utils::linear_position(a.dims(), indexers.offset()...)];
        if constexpr (is_movable_v<Array&&>)
            return static_cast<remove_cvref_t<decltype(data_ref)>>(data_ref);
        else
            return data_ref;
    }
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
    else if constexpr (
        ArrayType::category == view_category::Array ||
        ArrayType::category == view_category::Simple)
    {
        return _part_impl1<R>(std::forward<decltype(a)>(a),
            std::make_index_sequence<R - sizeof...(Specs)>{},
            std::forward<decltype(specs)>(specs)...);
    }
    else
        return part(a.to_array(), std::forward<decltype(specs)>(specs)...);
}

template<typename Array>
auto first(Array&& a)
{
    static_assert(array_rank_v<remove_cvref_t<Array>> >= 1, "badargtype");
    return part(a, cidx(0));
}

template<typename Array>
auto last(Array&& a)
{
    static_assert(array_rank_v<remove_cvref_t<Array>> >= 1, "badargtype");
    return part(a, int64_t(-1));
}

template<typename Array>
auto most(Array&& a)
{
    using AT = remove_cvref_t<Array>;
    constexpr auto AR = array_rank_v<AT>;
    static_assert(AR >= 1, "badargtype");
    const auto size = a.dims()[0];
    if (size <= 1u)
        throw std::logic_error("baddims");
    return part(a, make_span(const_all, int64_t(-2)));
}

template<typename Array>
auto rest(Array&& a)
{
    using AT = remove_cvref_t<Array>;
    static_assert(array_rank_v<AT> >= 1, "badargtype");
    const auto size = a.dims()[0];
    if (size <= 1u)
        throw std::logic_error("baddims");
    return part(a, make_span(int64_t(2), const_all));
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
                auto ret_iter = ret.begin();
                for (size_t i = 0; i < length; ++i, begin += step, ++ret_iter)
                    *ret_iter = begin;
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
                size_t length = wl::integer_part(diff / step) + 1u;
                auto remain = T(begin + (length - 1u) * step) - T(end);
                if (step * remain > T(0))
                    --length;
                ndarray<T, 1u> ret(std::array<size_t, 1>{length});
                auto ret_iter = ret.begin();
                for (size_t i = 0; i < length; ++i, ++ret_iter)
                    *ret_iter = T(begin + i * step);
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
    using ValueType = value_type_t<Array>;
    constexpr int64_t L1 = I1 >= 0 ? I1 : I1 + int64_t(rank) + 1;
    constexpr int64_t L2 = I2 >= 0 ? I2 : I2 + int64_t(rank) + 1;
    static_assert(1 <= L1 && L1 <= L2 && L2 <= int64_t(rank), "badlevel");

    if constexpr (Array::category == view_category::General)
        return total(a.to_array(), const_int<I1>{}, const_int<I2>{});
    else
    {
        if constexpr (L1 == 1)
        {
            if constexpr (L2 == rank)
            {
                const auto inter_size = a.size();
                auto a_iter = a.begin();
                auto ret = ValueType{};
                for (size_t j = 0; j < inter_size; ++j, ++a_iter)
                    ret += *a_iter;
                return ret;
            }
            else
            {
                auto ret_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                ndarray<ValueType, rank - L2> ret(ret_dims);
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
                ndarray<ValueType, L1 - 1> ret(ret_dims);
                const auto outer_size = ret.size();
                const auto inter_size = utils::size_of_dims(
                    utils::dims_take<L1, L2>(a.dims()));
                auto a_iter = a.begin();
                auto ret_iter = ret.begin();
                for (size_t i = 0; i < outer_size; ++i, ++ret_iter)
                {
                    auto sum = ValueType{};
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
                ndarray<ValueType, rank - (L2 - L1 + 1)> ret(ret_dims);
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

template<typename Any>
auto array_depth(const Any& any)
{
    return int64_t(array_rank_v<Any>);
}

template<typename T, size_t R>
void _reverse_inplace(ndarray<T, R>& a,
    size_t outer_size, size_t inter_size, size_t inner_size)
{
    if (inner_size == 1u)
    {
        auto base = a.data();
        for (size_t i = 0u; i < outer_size; ++i, base += inter_size)
        {
            auto forward = base;
            auto reverse = base + inter_size;
            for (; forward != reverse && forward != --reverse; ++forward)
                std::iter_swap(forward, reverse);
        }
    }
    else
    {
        auto base = a.data();
        auto outer_step = inter_size * inner_size;
        for (size_t i = 0u; i < outer_size; ++i, base += outer_step)
        {
            size_t forward = 0u;
            size_t reverse = inter_size;
            for (; forward != reverse && forward != --reverse; ++forward)
                std::swap_ranges(
                    base + forward * inner_size,
                    base + forward * inner_size + inner_size,
                    base + reverse * inner_size);
        }
    }
}

template<typename X, int64_t I>
auto reverse(X&& x, const_int<I>)
{
    using XT = remove_cvref_t<X>;
    constexpr auto rank = array_rank_v<XT>;
    static_assert(rank >= 1, "badargtype");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(rank) + 1;
    static_assert(1 <= Level && Level <= int64_t(rank), "badlevel");

    size_t outer_size = utils::size_of_dims(
        utils::dims_take<1u, Level - 1u>(x.dims()));
    size_t inter_size = utils::size_of_dims(
        utils::dims_take<Level, Level>(x.dims()));
    size_t inner_size = utils::size_of_dims(
        utils::dims_take<Level + 1u, rank>(x.dims()));
    if constexpr (is_movable_v<X&&>)
    {
        _reverse_inplace(x, outer_size, inter_size, inner_size);
        return x;
    }
    else
    {
        auto x2 = x.to_array();
        _reverse_inplace(x2, outer_size, inter_size, inner_size);
        return x2;
    }
}

template<typename X>
auto reverse(X&& x)
{
    return reverse(std::forward<decltype(x)>(x), const_int<1>{});
}

template<typename X, typename Pad, typename... Dims>
auto array_reshape(X&& x, const Pad& padding, varg_tag, const Dims&... dims)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    static_assert(array_rank_v<XT> >= 1, "badargtype");
    static_assert(all_is_integral_v<Dims...>, "badargtype");
    constexpr auto rank = sizeof...(dims);
    static_assert(rank >= 1, "badrank");
    ndarray<XV, rank> ret(std::array<int64_t, rank>{int64_t(dims)...});

    const size_t x_size = x.size();
    const size_t ret_size = ret.size();
    if (x_size <= ret_size)
    {
        x.copy_to(ret.begin());
        if constexpr (!std::is_same_v<Pad, void_type>)
        {
            static_assert(is_convertible_v<Pad, XV>, "badargtype");
            std::fill(ret.begin() + x_size, ret.end(), cast<XV>(padding));
        }
        else if (ret.is_static()) // only std::array needs initialization
            std::fill(ret.begin() + x_size, ret.end(), XV{});
    }
    else
    {
        auto ret_iter = ret.begin();
        auto ret_end = ret.end();
        x.for_each([&](const auto& src)
            { *ret_iter++ = src; return (ret_iter == ret_end); });
    }
    return ret;
}

template<typename X, typename... Dims>
auto array_reshape(X&& x, varg_tag, const Dims&... dims)
{
    static_assert(array_rank_v<remove_cvref_t<X>> >= 1, "badargtype");
    return array_reshape(std::forward<decltype(x)>(x),
        void_type{}, varg_tag{}, dims...);
}

template<size_t R, size_t... Is>
struct _is_valid_transpose
{
    static constexpr auto between = ((1u <= Is && Is <= R) && ...);
    static constexpr auto mask = ((uint64_t(1) << (Is - 1u)) | ...);
    static constexpr auto value = between && ((mask & (mask + 1u)) == 0);
};

template<size_t... Is>
struct _transpose_max_level;

template<size_t I1, size_t I2, size_t... Is>
struct _transpose_max_level<I1, I2, Is...> :
    _transpose_max_level<(I1 > I2 ? I1 : I2), Is...> {};

template<size_t I>
struct _transpose_max_level<I> : std::integral_constant<size_t, I> {};

template<typename T, size_t... Is>
struct _padded_transpose_levels_impl;

template<size_t... Is, size_t... Pads>
struct _padded_transpose_levels_impl<std::index_sequence<Pads...>, Is...>
{
    static constexpr auto max_level = _transpose_max_level<Is...>::value;
    using type = std::index_sequence<Is..., (Pads + max_level + 1u)...>;
};

template<size_t R, size_t... Is>
struct _padded_transpose_levels :
    _padded_transpose_levels_impl<
    std::make_index_sequence<R - sizeof...(Is)>, Is...> {};

template<size_t L, typename T>
void _transpose_fill(const T* src, T*& dst,
    const size_t* dims, const size_t* strides)
{
    const size_t dim = *dims;
    const size_t stride = *strides;
    if constexpr (L > 1u)
    {
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0u; i < dim; ++i, src += stride)
            _transpose_fill<L - 1u>(src, dst, dims + 1, strides + 1);
    }
    else if (stride == 1u)
    {
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0u; i < dim; ++i, ++src, ++dst)
            *dst = *src;
    }
    else
    {
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0u; i < dim; ++i, src += stride, ++dst)
            *dst = *src;
    }
}

template<typename T, size_t R, typename Output, size_t... Is, size_t... Cs>
auto _transpose_impl(const ndarray<T, R>& a, Output ptr, 
    std::index_sequence<Is...>, std::index_sequence<Cs...>)
{
    constexpr auto RetRank = _transpose_max_level<size_t(Is)...>::value;
    auto a_dims = a.dims().data();
    std::array<size_t, R> strides;
    std::array<size_t, RetRank> ret_dims{};
    std::array<size_t, RetRank> ret_strides{};
    strides[R - 1u] = 1u;
    [[maybe_unused]] auto _1 = ((Cs > 0 ? (strides[R - Cs - 1u] =
        strides[R - Cs] * a_dims[R - Cs]) : size_t()), ...);
    [[maybe_unused]] auto _2 = ((ret_dims[Is - 1] == 0u ?
        (ret_dims[Is - 1] = a_dims[Cs], ret_strides[Is - 1] += strides[Cs]) :
        ret_dims[Is - 1] == a_dims[Cs] ? ret_strides[Is - 1] += strides[Cs] :
        throw std::logic_error("baddims")), ...);

    if constexpr (std::is_pointer_v<Output>)
    {
        auto dst_ptr = ptr;
        _transpose_fill<RetRank>(a.data(), dst_ptr,
            ret_dims.data(), ret_strides.data());
    }
    else
    {
        ndarray<T, RetRank> ret(ret_dims);
        auto dst_ptr = ret.data();
        _transpose_fill<RetRank>(a.data(), dst_ptr,
            ret_dims.data(), ret_strides.data());
        return ret;
    }
}

template<typename X, int64_t... Is>
auto transpose(X&& x, const_int<Is>...)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto NL = sizeof...(Is);
    static_assert(1 <= NL && NL <= XR, "badargtype");
    static_assert(_is_valid_transpose<XR, size_t(Is)...>::value, "badlevel");
    return _transpose_impl(val(x), void_type{}, 
        typename _padded_transpose_levels<XR, Is...>::type{},
        std::make_index_sequence<XR>{});
}

template<typename Level>
struct _flatten_max_level_impl;

template<int64_t... Is>
struct _flatten_max_level_impl<const_ints<Is...>> : 
    _transpose_max_level<size_t(Is)...> {};

template<typename... Levels>
struct _flatten_max_level :
    _transpose_max_level<_flatten_max_level_impl<Levels>::value...> {};

template<>
struct _flatten_max_level<> : std::integral_constant<size_t, 0> {};

template<typename... Levels>
struct _flatten_levels_join;

template<int64_t... Is, int64_t... Js, typename... Levels>
struct _flatten_levels_join<const_ints<Is...>, const_ints<Js...>, Levels...> :
    _flatten_levels_join<const_ints<Is..., Js...>, Levels...> {};

template<int64_t... Is>
struct _flatten_levels_join<const_ints<Is...>>
{
    using type = const_ints<Is...>;
};

template<int64_t... Is, typename... Levels>
void _flatten_get_dims(const size_t* input_dims, size_t* ret_dims, 
    const_ints<Is...>, Levels...)
{
    *ret_dims = (input_dims[Is - 1u] * ...);
    if constexpr (sizeof...(Levels) > 0u)
        _flatten_get_dims(input_dims, ret_dims + 1, Levels{}...);
}

template<size_t MaxLevel, typename X, size_t... Pads, typename... Levels>
auto _flatten_copy_impl(X&& x, std::index_sequence<Pads...>, Levels...)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto RetRank = sizeof...(Pads) + sizeof...(Levels);
    std::array<size_t, RetRank> ret_dims;
    _flatten_get_dims(x.dims().data(), ret_dims.data(), 
        Levels{}..., const_ints<int64_t(Pads + MaxLevel + 1u)>{}...);
    if constexpr (is_movable_v<X&&>)
        return ndarray<XV, RetRank>(ret_dims, std::move(x.data_vector()));
    else if constexpr (XT::category != view_category::General)
        return ndarray<XV, RetRank>(ret_dims, x.begin(), x.begin() + x.size());
    else
    {
        ndarray<XV, RetRank> ret(ret_dims);
        x.copy_to(ret.data());
        return ret;
    }
}

template<typename X, typename... Levels>
auto flatten_copy(X&& x, Levels...)
{
    constexpr auto XR = array_rank_v<remove_cvref_t<X>>;
    constexpr auto MaxLevel = _flatten_max_level<Levels...>::value;
    static_assert(1 <= MaxLevel && MaxLevel <= XR, "badlevel");
    return _flatten_copy_impl<MaxLevel>(std::forward<decltype(x)>(x),
        std::make_index_sequence<XR - MaxLevel>{}, Levels{}...);
}

template<typename T, int64_t... Is, size_t... Cs>
void _flatten_impl3(const size_t* dims, const T* src, T* dst, 
    const_ints<Is...>, std::index_sequence<Cs...>)
{
    constexpr auto R = sizeof...(Is);
    std::array<size_t, R> strides;
    std::array<size_t, R> ret_dims;
    std::array<size_t, R> ret_strides;
    strides[R - 1u] = 1u;
    [[maybe_unused]] auto _1 = ((Cs > 0 ? (strides[R - Cs - 1u] =
        strides[R - Cs] * dims[R - Cs]) : size_t()), ...);
    [[maybe_unused]] auto _2 = ((ret_strides[Cs] = strides[Is - 1],
        ret_dims[Cs] = dims[Is - 1]), ...);
    _transpose_fill<R>(src, dst, ret_dims.data(), ret_strides.data());
}

template<typename X, size_t... Pads, typename... Levels>
auto _flatten_impl2(X&& x, Levels...)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto RetRank = sizeof...(Levels);
    std::array<size_t, RetRank> ret_dims;
    _flatten_get_dims(x.dims().data(), ret_dims.data(), Levels{}...);
    ndarray<XV, RetRank> ret(ret_dims);
    const auto& valx = val(x);
    _flatten_impl3(valx.dims().data(), valx.data(), ret.data(), 
        typename _flatten_levels_join<Levels...>::type{}, 
        std::make_index_sequence<XR>{});
    return ret;
}

template<size_t MaxLevel, typename X, size_t... Pads, typename... Levels>
auto _flatten_impl1(X&& x, std::index_sequence<Pads...>, Levels...)
{
    return _flatten_impl2(std::forward<decltype(x)>(x),
        Levels{}..., const_ints<int64_t(Pads + MaxLevel + 1u)>{}...);
}

template<typename X, typename... Levels>
auto flatten(X&& x, Levels...)
{
    constexpr auto XR = array_rank_v<remove_cvref_t<X>>;
    constexpr auto MaxLevel = _flatten_max_level<Levels...>::value;
    static_assert(1 <= MaxLevel && MaxLevel <= XR, "badlevel");
    return _flatten_impl1<MaxLevel>(std::forward<decltype(x)>(x),
        std::make_index_sequence<XR - MaxLevel>{}, Levels{}...);
}

template<typename X>
auto flatten(X&& x)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(1 <= XR, "badlevel");
    std::array<size_t, 1u> ret_dims{utils::size_of_dims(x.dims())};
    if constexpr (is_movable_v<X&&>)
        return ndarray<XV, 1u>(ret_dims, std::move(x.data_vector()));
    else if constexpr (XT::category != view_category::General)
        return ndarray<XV, 1u>(ret_dims, x.begin(), x.begin() + x.size());
    else
    {
        ndarray<XV, 1u> ret(ret_dims);
        x.copy_to(ret.data());
        return ret;
    }
}

}
