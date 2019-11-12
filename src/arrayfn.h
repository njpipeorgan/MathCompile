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
auto set(Dst&& dst, Src&& src) -> decltype(auto)
{
    using DstType = remove_cvref_t<Dst>;
    using SrcType = remove_cvref_t<Src>;
    constexpr auto dst_rank = array_rank_v<DstType>;
    constexpr auto src_rank = array_rank_v<SrcType>;
    using DstValue = value_type_t<DstType>;
    using SrcValue = value_type_t<SrcType>;
    static_assert(std::is_lvalue_reference_v<Dst&&> ||
        is_array_view_v<DstType>, "badassign");

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
        if constexpr (is_array_v<DstType>)
        {
            if constexpr (std::is_same_v<SrcValue, DstValue>)
                dst = std::forward<decltype(src)>(src).to_array();
            else
            {
                dst.uninitialized_resize(src.dims(), src.size());
                src.copy_to(dst.data());
            }
            return dst; // returns an l-value reference
        }
        else // dst is an array view
        {
            if (!utils::check_dims(src.dims(), dst.dims()))
                throw std::logic_error("baddims");
            if (!has_aliasing(src, dst))
            {
                if constexpr (SrcType::category != view_category::General)
                    dst.copy_from(src.begin());
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
            if constexpr (sizeof...(rest) > 0u)
                return list(std::forward<decltype(rest)>(rest)...);
            else
            {
                using ItemType = value_type_t<FirstType>;
                constexpr auto rank = array_rank_v<ItemType> +1u;
                using ValueType = std::conditional_t<
                    rank == 0u, ItemType, value_type_t<ItemType>>;
                return ndarray<value_type_t<ItemType>, rank>{};
            }
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

template<typename Ret = int64_t, typename Array>
auto dimensions(const Array& a)
{
    static_assert(is_integral_v<Ret>, "badrettype");
    constexpr auto rank = array_rank_v<Array>;
    if constexpr (rank == 0u)
        return ndarray<Ret, 1u>{};
    else
        return ndarray<Ret, 1u>(std::array<size_t, 1u>{rank},
            a.dims_ptr(), a.dims_ptr() + rank);
}

template<typename Array>
auto length(const Array& a)
{
    constexpr auto rank = array_rank_v<Array>;
    if constexpr (rank == 0u)
        return int64_t(0);
    else
        return int64_t(a.dims()[0]);
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
auto transpose(const X& x, const_int<Is>...)
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

template<typename X>
auto transpose(const X& x)
{
    return transpose(x, const_int<2>{}, const_int<1>{});
}

template<typename X>
auto conjugate_transpose(const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 2u, "badrank");
    using XV = value_type_t<X>;

    if constexpr (is_real_v<XV>)
        return transpose(x, const_int<2>{}, const_int<1>{});
    else
    {
        const auto& valx = allows<view_category::Regular>(x);
        auto x_iter = valx.begin();
        if constexpr (XR == 2u)
        {
            const size_t dim0 = x.dims()[0];
            const size_t dim1 = x.dims()[1];
            ndarray<XV, 2u> ret(std::array<size_t, 2u>{dim1, dim0});
            auto ret_base = ret.data();
            for (size_t i = 0u; i < dim0; ++i, ++ret_base)
            {
                auto ret_iter = ret_base;
                WL_IGNORE_DEPENDENCIES
                for (size_t j = 0u; j < dim1; ++j, ++x_iter, ret_iter += dim0)
                    *ret_iter = std::conj(*x_iter);
            }
            return ret;
        }
        else
        {
            const size_t dim0 = x.dims()[0];
            const size_t dim1 = x.dims()[1];
            const auto dims_rest = utils::dims_take<3u, XR>(x.dims());
            const size_t chunk_size = utils::size_of_dims(dims_rest);
            ndarray<XV, XR> ret(utils::dims_join(
                std::array<size_t, 2u>{dim1, dim0}, dims_rest));
            auto ret_base = ret.data();
            for (size_t i = 0u; i < dim0; ++i, ret_base += chunk_size)
            {
                auto ret_base2 = ret_base;
                for (size_t j = 0u; j < dim1;
                    ++j, ret_base2 += dim0 * chunk_size)
                {
                    auto ret_iter = ret_base2;
                    WL_IGNORE_DEPENDENCIES
                    for (size_t k = 0u; k < chunk_size;
                        ++k, ++ret_iter, ++x_iter)
                    {
                        *ret_iter = std::conj(*x_iter);
                    }
                }
            }
            return ret;
        }
    }
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

inline int64_t _order_scalar(const boolean& x, const boolean& y)
{
    return x == y ? int64_t(0) : x ? int64_t(-1) : int64_t(1);
}

template<typename X>
int64_t _order_scalar(const complex<X>& x, const complex<X>& y)
{
    return x.real() == y.real() ? _order_scalar(x.imag(), y.imag()) :
        (x.real() < y.real() ? int64_t(1) : int64_t(-1));
}

template<typename X>
int64_t _order_scalar(const X& x, const X& y)
{
    return x == y ? int64_t(0) : x < y ? int64_t(1) : int64_t(-1);
}

template<typename X, typename Y>
int64_t _order_array(const X& x, const Y& y, dim_checked)
{
    int64_t ret = 0;
    if constexpr (X::category != view_category::General)
    {
        y.for_each([&ret](const auto& b, const auto& a)
            {
                auto res = _order_scalar(a, b);
                if (res == 0) return false;
                ret = res;
                return true;
            }, x.begin());
    }
    else
    {
        x.for_each([&ret](const auto& a, const auto& b)
            {
                auto res = _order_scalar(a, b);
                if (res == 0) return false;
                ret = res;
                return true;
            }, y.begin());
    }
    return ret;
}

template<typename X, typename Y>
int64_t _order_array(const X& x, const Y& y)
{
    if (utils::check_dims(x.dims(), y.dims()))
        return _order_array(x, y, dim_checked{});
    else
        return std::lexicographical_compare(x.dims().begin(), x.dims().end(),
            y.dims().begin(), x.dims().end()) ? int64_t(1) : int64_t(-1);
}

template<typename Ret = int64_t, typename X, typename Y>
int64_t order(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<X>;
    static_assert(XR == YR, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    if constexpr (XR == 0)
    {
        static_assert(std::is_same_v<X, Y>, "badargtype");
        return Ret(_order_scalar(x, y));
    }
    else
    {
        static_assert(std::is_same_v<value_type_t<X>, value_type_t<Y>>,
            "badargtype");
        return Ret(_order_array(x, y));
    }
}

template<typename Ret = int64_t, typename X, typename Pred>
auto ordering(const X& x, const int64_t n, Pred pred)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    static_assert(is_integral_v<Ret>, "badrettype");
    if (n == 0)
        return ndarray<Ret, 1u>{};
    const std::array<size_t, 1u> ret_dims{size_t(n > 0 ? n : -n)};

    const auto& valx = val(std::forward<decltype(x)>(x));
    const auto x_size = valx.dims()[0];
    const auto x_base = valx.template view_begin<1u>() - 1;
    std::vector<Ret> indices(x_size);
    for (size_t i = 0u; i < x_size; ++i)
        indices[i] = Ret(i + 1);
    if (n > 0)
    {
        auto order = [=](size_t a, size_t b)
        {
            auto res = pred(*(x_base + a), *(x_base + b));
            using OrderType = decltype(res);
            if constexpr (std::is_same_v<OrderType, bool> ||
                is_boolean_v<OrderType>)
                return bool(res);
            else
            {
                static_assert(std::is_signed_v<OrderType>, "badfunctype");
                return res > OrderType(0);
            }
        };
        if (size_t(n) > x_size)
            throw std::logic_error("badargv");
        else if (size_t(n) == x_size)
            std::sort(indices.begin(), indices.end(), order);
        else
        {
            std::partial_sort(indices.begin(), indices.begin() + n,
                indices.end(), order);
            indices.resize(size_t(n));
        }
    }
    else
    {
        auto order = [=](size_t a, size_t b)
        {
            auto res = pred(*(x_base + a), *(x_base + b));
            using OrderType = decltype(res);
            if constexpr (std::is_same_v<OrderType, bool> ||
                is_boolean_v<OrderType>)
                return !bool(res);
            else
            {
                static_assert(std::is_signed_v<OrderType>, "badfunctype");
                return res < OrderType(0);
            }
        };
        if (size_t(-n) > x_size)
            throw std::logic_error("badargv");
        else if (size_t(-n) == x_size)
            std::sort(indices.rbegin(), indices.rend(), order);
        else
        {
            std::partial_sort(indices.rbegin(), indices.rbegin() + (-n),
                indices.rend(), order);
            indices.erase(indices.begin(), indices.end() - (-n));
        }
    }
    return ndarray<Ret, 1u>(ret_dims, std::move(indices));
}

template<typename Ret = int64_t, typename X>
auto ordering(const X& x, const int64_t n)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    if constexpr (XR == 1u)
    {
        using XV = value_type_t<X>;
        if constexpr (is_complex_v<XV> || is_boolean_v<XV>)
        {
            return ordering<Ret>(x, n, [](const auto& a, const auto& b)
                { return _order_scalar(a, b) > 0; });
        }
        else
        {
            static_assert(is_real_v<XV>, "badargtype");
            return ordering<Ret>(x, n, std::less<>{});
        }
    }
    else
    {
        return ordering<Ret>(x, n,
            [](const auto& a, const auto& b)
            { return _order_array(a, b, dim_checked{}) > 0; });
    }
}

template<typename Ret = int64_t, typename X, typename Pred>
auto ordering(const X& x, all_type, Pred pred)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    return ordering<Ret>(x, x.dims()[0], pred);
}

template<typename Ret = int64_t, typename X>
auto ordering(const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    return ordering<Ret>(x, x.dims()[0]);
}

template<typename Ret = int64_t, typename X>
auto ordering(const X& x, all_type)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    return ordering<Ret>(x, x.dims()[0]);
}

template<typename T, typename Pred>
auto _sort_simple(ndarray<T, 1u>&& a, Pred pred)
{
    std::sort(a.begin(), a.end(), pred);
    return std::move(a);
}

template<typename T, typename Pred>
auto _sort_simple(const ndarray<T, 1u>& a, Pred pred)
{
    auto copy = a;
    std::sort(copy.begin(), copy.end(), pred);
    return copy;
}

template<typename X, typename Pred>
auto sort(X&& x, Pred pred)
{
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, "badargtype");
    if constexpr (XR == 1u)
    {
        auto&& valx = val(std::forward<decltype(x)>(x));
        using OrderType = remove_cvref_t<
            decltype(pred(*valx.begin(), *valx.begin()))>;
        if constexpr (std::is_same_v<OrderType, bool> ||
            is_boolean_v<OrderType>)
            return _sort_simple(valx, pred);
        else
        {
            static_assert(std::is_signed_v<OrderType>, "badfunctype");
            return _sort_simple(valx, [=](const auto& a, const auto& b)
                { return pred(a, b) > OrderType(0); });
        }
    }
    else
    {
        const auto& valx = val(std::forward<decltype(x)>(x));
        const auto x_size = valx.dims()[0];
        const auto order_indices = ordering(valx, x_size, pred);
        const auto order_data = order_indices.data();
        const auto x_base = valx.template view_begin<1u>() - 1;
        ndarray<value_type_t<XT>, XR> ret(valx.dims());
        auto ret_iter = ret.template view_begin<1u>();
        for (size_t i = 0u; i < x_size; ++i, ++ret_iter)
            (*(x_base + order_data[i])).copy_to(ret_iter.begin());
        return ret;
    }
}

template<typename X>
auto sort(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, "badargtype");
    if constexpr (XR == 1u)
    {
        using XV = value_type_t<XT>;
        if constexpr (is_complex_v<XV> || is_boolean_v<XV>)
        {
            return sort(std::forward<decltype(x)>(x),
                [](const auto& a, const auto& b)
                { return _order_scalar(a, b) > 0; });
        }
        else
        {
            static_assert(is_real_v<XV>, "badargtype");
            return sort(std::forward<decltype(x)>(x), std::less<>{});
        }
    }
    else
    {
        return sort(std::forward<decltype(x)>(x),
            [](const auto& a, const auto& b)
            { return _order_array(a, b, dim_checked{}) > 0; });
    }
}

template<typename X, typename Pred>
auto ordered_q(const X& x, Pred pred)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    const auto& copy = allows<view_category::Array>(x);
    const auto copy_length = copy.dims()[0];
    auto in_order = [=](const auto& a, const auto& b)
    {
        using OrderType = remove_cvref_t<decltype(pred(a, b))>;
        if constexpr (std::is_same_v<OrderType, bool> ||
            is_boolean_v<OrderType>)
            return bool(pred(a, b));
        else
            return pred(a, b) >= OrderType(0);
    };
    auto iter = copy.template view_begin<1u>();
    auto ret = true;
    for (size_t i = 1u; ret && i < copy_length; ++i, ++iter)
        ret = ret && in_order(*iter, *(iter + 1));
    return boolean(ret);
}

template<typename X>
auto ordered_q(const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    const auto& copy = allows<view_category::Array>(x);
    const auto copy_length = copy.dims()[0];
    auto in_order = [=](const auto& a, const auto& b)
    {
        constexpr auto AR = array_rank_v<remove_cvref_t<decltype(a)>>;
        if constexpr (AR == 0u)
            return _order_scalar(a, b) >= 0;
        else
            return _order_array(a, b, dim_checked{}) >= 0;
    };
    auto iter = copy.template view_begin<1u>();
    auto ret = true;
    for (size_t i = 1u; ret && i < copy_length; ++i, ++iter)
        ret = ret && in_order(*iter, *(iter + 1));
    return boolean(ret);
}

template<typename X, typename Y>
auto append(X&& x, Y&& y)
{
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto YR = array_rank_v<YT>;
    using XV = value_type_t<XT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u && is_convertible_v<YV, XV>, "badargtype");

    if constexpr (is_movable_v<X&&>)
    {
        x.append(std::forward<decltype(y)>(y));
        return std::move(x);
    }
    else if constexpr (XR == 1u)
    {
        ndarray<XV, XR> ret(std::array<size_t, 1u>{x.dims()[0] + 1u});
        const auto ret_iter = ret.data();
        x.copy_to(ret_iter);
        *(ret_iter + x.size()) = std::forward<decltype(y)>(y);
        return ret;
    }
    else
    {
        const auto x_elem_dims = utils::dims_take<2u, XR>(x.dims());
        if (!utils::check_dims(x_elem_dims, y.dims()))
            throw std::logic_error("baddims");
        ndarray<XV, XR> ret(utils::dims_join(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, x_elem_dims));
        const auto ret_iter = ret.data();
        x.copy_to(ret_iter);
        y.copy_to(ret_iter + x.size());
        return ret;
    }
}

template<typename X, typename Y>
auto prepend(X&& x, Y&& y)
{
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto YR = array_rank_v<YT>;
    using XV = value_type_t<XT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u && is_convertible_v<YV, XV>, "badargtype");
    if constexpr (XR == 1u)
    {
        ndarray<XV, XR> ret(std::array<size_t, 1u>{x.dims()[0] + 1u});
        const auto ret_iter = ret.data();
        *ret_iter = std::forward<decltype(y)>(y);
        x.copy_to(ret_iter + 1);
        return ret;
    }
    else
    {
        const auto x_elem_dims = utils::dims_take<2u, XR>(x.dims());
        if (!utils::check_dims(x_elem_dims, y.dims()))
            throw std::logic_error("baddims");
        ndarray<XV, XR> ret(utils::dims_join(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, x_elem_dims));
        const auto ret_iter = ret.data();
        y.copy_to(ret_iter);
        x.copy_to(ret_iter + y.size());
        return ret;
    }
}

template<typename XV, size_t XR, typename Y>
auto append_to(ndarray<XV, XR>& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    constexpr auto YR = array_rank_v<YT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u && is_convertible_v<YV, XV>, "badargtype");
    x.append(std::forward<decltype(y)>(y));
}

template<typename XV, size_t XR, typename Y>
auto prepend_to(ndarray<XV, XR>& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    constexpr auto YR = array_rank_v<YT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u && is_convertible_v<YV, XV>, "badargtype");

    if constexpr (XR == 1u)
    {
        const auto x_size = x.size();
        const auto new_size = x_size + 1u;
        x.uninitialized_resize(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, new_size);
        const auto x_iter = x.data();
        std::move_backward(x_iter, x_iter + x_size, x_iter + new_size);
        *x_iter = std::forward<decltype(y)>(y);
    }
    else
    {
        const auto x_elem_dims = utils::dims_take<2u, XR>(x.dims());
        if (!utils::check_dims(x_elem_dims, y.dims()))
            throw std::logic_error("baddims");
        const auto x_size = x.size();
        const auto y_size = y.size();
        const auto new_size = x_size + y_size;
        x.uninitialized_resize(utils::dims_join(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, x_elem_dims), new_size);
        const auto x_iter = x.data();
        std::move_backward(x_iter, x_iter + x_size, x_iter + new_size);
        y.copy_to(x_iter);
    }
    return x;
}

template<size_t Level, size_t Rank, typename Arg>
auto _join_dims_by_args_impl(
    const std::array<size_t, Rank>& dims, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        return arg.size() == 0u ? size_t(0) :
            arg.size() * _join_dims_by_args_impl<Level>(dims, arg.get(0));
    }
    else
    {
        static_assert(array_rank_v<Arg> == Rank, "badrank");
        if (arg.size() > 0u)
        {
            const auto leading_check = utils::check_dims<Level - 1u>(
                dims.data(), arg.dims().data());
            const auto trailing_check = utils::check_dims<Rank - Level>(
                dims.data() + Level, arg.dims().data() + Level);
            if (!leading_check || !trailing_check)
                throw std::logic_error("baddims");
            return arg.dims()[Level - 1];
        }
        else
            return size_t(0);
    }
}

template<size_t Level, size_t Rank, typename... Args>
void _join_dims_by_args(std::array<size_t, Rank>& dims, const Args&... args)
{
    dims[Level - 1] += (_join_dims_by_args_impl<Level>(dims, args) + ...);
}

template<size_t Level, size_t Rank, typename Iter, typename Arg>
void _join_copy_leveln(Iter& ret_base, size_t stride, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        if (arg.size() == 0)
            return;
        auto arg_dims = arg.get(0).dims();
        const auto leading_size = utils::size_of_dims(
            utils::dims_take<1u, Level - 1u>(arg_dims));
        const auto trailing_size = utils::size_of_dims(
            utils::dims_take<Level, Rank>(arg_dims));
        for (size_t k = 0u; k < arg.size(); ++k, ret_base += trailing_size)
        {
            const auto& arg_k = arg.get(k);
            auto arg_iter = arg_k.begin();
            auto ret_iter = ret_base;
            for (size_t i = 0u; i < leading_size; ++i, ret_iter += stride)
            {
                WL_IGNORE_DEPENDENCIES
                for (size_t j = 0u; j < trailing_size; ++j, ++arg_iter)
                    ret_iter[j] = *arg_iter;
            }
        }
    }
    else if (arg.size() > 0u)
    {
        const auto arg_dims = arg.dims();
        const auto leading_size = utils::size_of_dims(
            utils::dims_take<1u, Level - 1u>(arg_dims));
        const auto trailing_size = utils::size_of_dims(
            utils::dims_take<Level, Rank>(arg_dims));
        if constexpr (Arg::category == view_category::General)
        {
            auto ret_iter = ret_base;
            size_t j = 0u;
            arg.for_each([&](const auto& x)
                {
                    ret_iter[j] = x;
                    if ((++j) >= trailing_size)
                    {
                        j = 0u;
                        ret_iter += stride;
                    }
                });
        }
        else
        {
            auto arg_iter = arg.begin();
            auto ret_iter = ret_base;
            for (size_t i = 0u; i < leading_size; ++i, ret_iter += stride)
            {
                WL_IGNORE_DEPENDENCIES
                for (size_t j = 0u; j < trailing_size; ++j, ++arg_iter)
                    ret_iter[j] = *arg_iter;
            }
        }
        ret_base += trailing_size;
    }
}

template<typename Iter, typename Arg>
void _join_copy_level1(Iter& iter, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        for (size_t i = 0; i < arg.size(); ++i)
            _join_copy_level1(iter, arg.get(i));
    }
    else
    {
        arg.copy_to(iter);
        iter += arg.size();
    }
}

template<int64_t I, typename First, typename... Rest>
auto join(const_int<I>, First&& first, Rest&&... rest)
{
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    using FirstType = remove_cvref_t<First>;
    if constexpr (is_argument_pack_v<FirstType>)
    {
        if (first.size() == 0u)
        {
            if constexpr (sizeof...(rest) > 0u)
                return join(const_int<I>{},
                    std::forward<decltype(rest)>(rest)...);
            else
            {
                using ItemType = value_type_t<FirstType>;
                constexpr auto rank = array_rank_v<ItemType>;
                using ValueType = std::conditional_t<
                    rank == 0u, ItemType, value_type_t<ItemType>>;
                return ndarray<value_type_t<ItemType>, rank>{};
            }
        }
        else
            return join(const_int<I>{}, first.get(0), first.get_pack(1),
                std::forward<decltype(rest)>(rest)...);
    }
    else
    {
        constexpr auto rank = array_rank_v<FirstType>;
        static_assert(1u <= Level && Level <= rank, "badlevel");
        auto ret_dims = first.dims();
        _join_dims_by_args<Level>(ret_dims, rest...);
        ndarray<value_type_t<FirstType>, rank> ret(ret_dims);
        if constexpr (Level == 1u)
        {
            auto ret_iter = ret.data();
            _join_copy_level1(ret_iter, first);
            (_join_copy_level1(ret_iter, rest), ...);
        }
        else
        {
            auto ret_iter = ret.data();
            auto stride = utils::size_of_dims(
                utils::dims_take<Level, rank>(ret_dims));
            _join_copy_leveln<Level, rank>(ret_iter, stride, first);
            (_join_copy_leveln<Level, rank>(ret_iter, stride, rest), ...);
        }
        return ret;
    }
}

template<typename First, typename... Rest>
auto join(First&& first, Rest&&... rest)
{
    return join(const_int<1>{}, std::forward<decltype(first)>(first),
        std::forward<decltype(rest)>(rest)...);
}

template<typename First, typename... Rest>
auto set_union(const First& first, const Rest&... rest)
{
    constexpr auto R = array_rank_v<First>;
    static_assert(((R == array_rank_v<Rest>) && ... && (R >= 1u)), "badrank");
    using T = value_type_t<First>;
    static_assert((std::is_same_v<T, value_type_t<Rest>> && ...), "badargtype");

    auto scalar_less = [](const auto& x, const auto& y)
    {
        return _order_scalar(x, y) == int64_t(1);
    };
    auto copy = join(first, rest...);
    const auto copy_length = copy.dims()[0];
    const auto copy_data = copy.data();
    if constexpr (R == 1u)
    {
        std::sort(copy_data, copy_data + copy_length, scalar_less);
        const auto copy_end = std::unique(
            copy_data, copy_data + copy_length);
        const auto union_size = size_t(copy_end - copy_data);
        copy.uninitialized_resize(std::array<size_t, 1u>{union_size});
        return copy;
    }
    else
    {
        const auto item_dims = utils::dims_take<2, R>(copy.dims());
        const auto item_size = utils::size_of_dims(item_dims);
        auto idx = range(size_t(0), copy_length - 1u);
        auto idx_begin = idx.data();
        auto sort_pred = [=](size_t a, size_t b)
        {
            auto a_iter = copy_data + a * item_size;
            auto b_iter = copy_data + b * item_size;
            return std::lexicographical_compare(
                a_iter, a_iter + item_size,
                b_iter, b_iter + item_size,
                scalar_less);
        };
        std::sort(idx_begin, idx_begin + copy_length, sort_pred);
        auto unique_pred = [=](size_t a, size_t b)
        {
            auto a_iter = copy_data + a * item_size;
            auto b_iter = copy_data + b * item_size;
            return std::equal(a_iter, a_iter + item_size, b_iter);
        };
        const auto idx_end = std::unique(
            idx_begin, idx_begin + copy_length, unique_pred);
        const auto union_size = size_t(idx_end - idx_begin);
        ndarray<T, R> ret(utils::dims_join(
            std::array<size_t, 1u>{union_size}, item_dims));
        const auto copy_base = copy.template view_begin<1u>();
        auto ret_iter = ret.template view_begin<1u>();
        for (size_t i = 0u; i < union_size; ++i, ++ret_iter)
            (*(copy_base + idx_begin[i])).copy_to(ret_iter.begin());
        return ret;
    }
}


template<typename X>
auto _rotate_impl(const X& x, int64_t n)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badrank");
    using XV = value_type_t<X>;

    const auto item_count = x.dims()[0];
    if (item_count == 0u)
        return ndarray<XV, XR>(x.dims());
    const auto& valx = allows<view_category::Regular>(x);
    if (n >= int64_t(item_count))
        n = n % item_count;
    else if (n <= -int64_t(item_count))
        n = -((-n) % item_count);
    if (n == 0)
        return allows<view_category::Array>(valx);
    const auto item_size = utils::size_of_dims(
        utils::dims_take<2u, XR>(valx.dims()));
    auto x_iter = valx.begin();
    ndarray<XV, XR> ret(valx.dims());
    const auto ret_iter = ret.data();
    
    if (n >= 0)
    { // rotate right
        const auto size1 = size_t(n) * item_size;
        const auto size2 = ret.size() - size1;
        auto iter2 = ret_iter + size1;
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0; i < size2; ++i, ++iter2, ++x_iter)
            *iter2 = *x_iter;
        auto iter1 = ret_iter;
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0; i < size1; ++i, ++iter1, ++x_iter)
            *iter1 = *x_iter;
    }
    else
    { // rotate left
        const auto size1 = size_t(-n) * item_size;
        const auto size2 = ret.size() - size1;
        auto iter1 = ret_iter + size2;
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0; i < size1; ++i, ++iter1, ++x_iter)
            *iter1 = *x_iter;
        auto iter2 = ret_iter;
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0; i < size2; ++i, ++iter2, ++x_iter)
            *iter2 = *x_iter;
    }
    return ret;
}

template<typename X, typename N>
auto rotate_left(const X& x, const N& n)
{
    static_assert(is_integral_v<N>, "badargtype");
    return _rotate_impl(x, -int64_t(n));
}

template<typename X, typename N>
auto rotate_right(const X& x, const N& n)
{
    static_assert(is_integral_v<N>, "badargtype");
    return _rotate_impl(x, int64_t(n));
}

template<typename X>
auto rotate_left(const X& x)
{
    return _rotate_impl(x, -1);
}

template<typename X>
auto rotate_right(const X& x)
{
    return _rotate_impl(x, 1);
}

template<size_t I, size_t Level, typename Ret, typename Iter, typename IsEqual>
void _position_impl(const std::array<size_t, Level>& pos_dims,
    ndarray<Ret, 2u>& ret, ndarray<Ret, 1u>& pos_idx, Ret* const idx_base,
    Iter& x_iter, IsEqual is_equal)
{
    const auto dim = pos_dims[I];
    if constexpr (I + 1u < Level)
    {
        for (size_t i = 1u; i <= dim; ++i)
        {
            idx_base[I] = i;
            _position_impl<I + 1u, Level, Ret>(
                pos_dims, ret, pos_idx, idx_base, x_iter, is_equal);
        }
    }
    else
    {
        for (size_t i = 1u; i <= dim; ++i, ++x_iter)
        {
            if (is_equal(*x_iter))
            {
                idx_base[I] = i;
                ret.append(pos_idx, dim_checked{});
            }
        }
    }
}

template<typename Ret = int64_t, typename X, typename Y, int64_t I>
auto position(const X& x, const Y& y, const_int<I>)
{
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(1u <= Level && Level + YR == XR, "badlevel");

    const auto& valx = allows<view_category::Array>(x);
    const auto& valy = allows<view_category::Simple>(y);
    return position(valx, varg_tag{},
        [&](const auto& a) { return equal(a, valy); }, const_int<Level>{});
}

template<typename Ret = int64_t, typename X, typename Function, int64_t I>
auto position(const X& x, varg_tag, Function f, const_int<I>)
{
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, "badlevel");
    
    const auto& valx = allows<view_category::Array>(x);
    if constexpr (Level == 1u)
    {
        auto x_iter = valx.template view_begin<1u>();
        const auto size = valx.dims()[0];
        ndarray<Ret, 1u> ret;
        for (size_t i = 1u; i <= size; ++i, ++x_iter)
        {
            if (f(*x_iter))
                ret.append(Ret(i), dim_checked{});
        }
        const auto ret_size = ret.size();
        return ndarray<Ret, 2u>(std::array<size_t, 2u>{ret_size, 1u},
            std::move(ret).data_vector());
    }
    else
    {
        auto x_iter = valx.template view_begin<Level>();
        const auto pos_dims = utils::dims_take<1u, Level>(valx.dims());
        ndarray<Ret, 2u> ret(std::array<size_t, 2u>{0u, Level});
        ndarray<Ret, 1u> pos_idx(std::array<size_t, 1u>{Level});
        Ret* const idx_base = pos_idx.data();
        _position_impl<0u, Level, Ret>(
            pos_dims, ret, pos_idx, idx_base, x_iter, f);
        return ret;
    }
}

template<typename Ret = int64_t, typename X, typename Y>
auto position(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR > YR, "badrank");
    return position(x, y, const_int<XR - YR>{});
}

}
