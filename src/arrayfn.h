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

namespace wl
{

template<typename Dst, typename Src>
auto set(Dst&& dst, Src&& src)
{
    using DstType = remove_cvref_t<Dst>;
    using SrcType = remove_cvref_t<Src>;
    constexpr auto dst_rank = array_rank_v<DstType>;
    constexpr auto src_rank = array_rank_v<SrcType>;

    if constexpr (src_rank == 0u)
    {
        if constexpr (dst_rank == 0u)
        { // scalar -> scalar
            std::forward<decltype(dst)>(dst) =
                std::forward<decltype(src)>(src);
        }
        else
        { // scalar -> ndarray / array_view
            std::forward<decltype(dst)>(dst).copy_from(
                make_scalar_view_iterator(src));
        }
    }
    else
    {
        static_assert(dst_rank == src_rank, "badrank");
        if (!_check_dims<src_rank>(src.dims_ptr(), dst.dims_ptr()))
            throw std::logic_error("baddims");

        if (!has_aliasing(src, dst))
        {
            if constexpr (SrcType::category != view_category::General)
                std::forward<decltype(dst)>(dst).copy_from(src.begin());
            else if (DstType::category != view_category::General)
                src.copy_to(dst.begin());
            else // general_view -> general_view
                indirect_view_copy(std::forward<decltype(dst)>(dst), src);
        }
        else // has aliasing
        {
            indirect_view_copy(std::forward<decltype(dst)>(dst), src);
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
        if (!_check_dims<R>(elem.dims_ptr(), dims))
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
        _copy_list_array_elements<FirstValueType, first_rank, dim0, 0u>(
            ret.begin(), dims.data() + 1, first.size(), 
            std::make_tuple(std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...));
        return ret;

    }
}

template<typename T, typename... Dims>
auto constant_array(const T& val, varg_tag, const Dims&... dims)
{
    static_assert(is_numerical_type_v<T>, "badargtype");
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

template<typename T, size_t R, typename... Indexers>
auto _part_impl3(ndarray<T, R>& a, 
    const Indexers&... indexers) -> decltype(auto)
{
    static_assert(sizeof...(Indexers) == R, "badargc");
    using return_type = typename view_detail::view_type<Indexers...>::
        template return_type<T, false, Indexers...>;
    if constexpr (is_array_view_v<return_type>)
        return return_type(a, indexers...);
    else
        return a.data()[a.linear_pos(indexers.offset()...)];
}

template<typename T, size_t R, size_t... Is, typename... Specs>
auto _part_impl2(ndarray<T, R>& a, std::index_sequence<Is...>,
    const Specs&... specs) -> decltype(auto)
{
    static_assert(sizeof...(Specs) == R, "badargc");
    return _part_impl3(a, make_indexer(specs, a.dims_[Is])...);
}

template<typename T, size_t R, size_t... Is, typename... Specs>
auto _part_impl1(ndarray<T, R>& a, std::index_sequence<Is...>,
    const Specs&... specs) -> decltype(auto)
{
    return _part_impl2(a,
        std::make_index_sequence<R>{}, specs..., make_all_type<Is>{}...);
}

template<typename T, size_t R, typename... Specs>
auto part(ndarray<T, R>& a,
    const Specs&... specs) -> decltype(auto)
{
    static_assert(sizeof...(Specs) <= R, "badargc");
    return _part_impl1(a,
        std::make_index_sequence<R - sizeof...(Specs)>{}, specs...);
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
            throw std::logic_error("badargv");
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
            throw std::logic_error("badargv");
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

}
