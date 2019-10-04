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

#include <type_traits>

#include "types.h"
#include "ndarray.h"

namespace wl
{

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
decltype(auto) _part_impl3(ndarray<T, R>& a, const Indexers&... indexers)
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
decltype(auto) _part_impl2(ndarray<T, R>& a, std::index_sequence<Is...>,
    const Specs&... specs)
{
    static_assert(sizeof...(Specs) == R, "badargc");
    return _part_impl3(a, make_indexer(specs, a.dims_[Is])...);
}

template<typename T, size_t R, size_t... Is, typename... Specs>
decltype(auto) _part_impl1(ndarray<T, R>& a, std::index_sequence<Is...>,
    const Specs&... specs)
{
    return _part_impl2(a, 
        std::make_index_sequence<R>{}, specs..., make_all_type<Is>{}...);
}

template<typename T, size_t R, typename... Specs>
decltype(auto) part(ndarray<T, R>& a, const Specs&... specs)
{
    static_assert(sizeof...(Specs) <= R, "badargc");
    return _part_impl1(a, 
        std::make_index_sequence<R - sizeof...(Specs)>{}, specs...);
}

}
