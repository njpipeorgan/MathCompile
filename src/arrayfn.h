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

        if (!std::is_same_v<typename SrcType::value_type,
            typename DstType::value_type> || !has_aliasing(src, dst))
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

template<typename T, typename... Dims>
auto constant_array(const T& val, varg_tag, const Dims&... dims)
{
    static_assert(is_arithmetic_v<T> || is_string_v<T>, "badargtype");
    constexpr size_t R = sizeof...(dims);
    ndarray<T, R> x(std::array<int64_t, R>{int64_t(dims)...}, val);
    return x;
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

}
