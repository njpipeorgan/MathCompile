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
#include "traits.h"

namespace wl
{

namespace utils
{

template<size_t R1, size_t R2, size_t... Is1, size_t... Is2>
auto _dims_join_impl(
    const std::array<size_t, R1>& dims1, std::index_sequence<Is1...>, 
    const std::array<size_t, R2>& dims2, std::index_sequence<Is2...>)
{
    return std::array<size_t, R1 + R2>{dims1[Is1]..., dims2[Is2]...};
}

template<size_t R1, size_t R2>
auto dims_join(const std::array<size_t, R1>& dims1,
    const std::array<size_t, R2>& dims2)
{
    return _dims_join_impl(dims1, std::make_index_sequence<R1>{}, 
        dims2, std::make_index_sequence<R2>{});
}

template<size_t R, size_t... Is>
auto _size_of_dims_impl(const std::array<size_t, R> dims, 
    std::index_sequence<Is...>)
{
    return (dims[Is] * ...);
}

template<size_t R>
auto size_of_dims(const std::array<size_t, R> dims)
{
    return _size_of_dims_impl(dims, std::make_index_sequence<R>{});
}

template<size_t... Is>
bool check_dims_impl(const size_t* dims1, const size_t* dims2, 
    std::index_sequence<Is...>)
{
    return ((dims1[Is] == dims2[Is]) && ... );
}

template<size_t R>
auto check_dims(const size_t* dims1, const size_t* dims2)
{
    static_assert(R >= 1u, "internal");
    return check_dims_impl(dims1, dims2, std::make_index_sequence<R>{});
}

template<size_t R>
auto check_dims(const std::array<size_t, R>& dims1, 
    const std::array<size_t, R>& dims2)
{
    static_assert(R >= 1u, "internal");
    return check_dims_impl(dims1.data(), dims2.data(), 
        std::make_index_sequence<R>{});
}

}

}
