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

#include <tuple>

#include "types.h"
#include "traits.h"
#include "const.h"

namespace wl
{

struct scalar_indexer; // I

struct all_indexer;    // A

struct unit_indexer;   // U

struct step_indexer;   // S

struct complement_step_indexer;

struct list_indexer;

enum class view_category
{
    Scalar, 
    Array, 
    Simple, 
    Regular, 
    General
};

namespace view_detail
{

template<typename Tuple, typename... Indexers>
struct indexers_tuple_impl;

template<typename... InTuple, typename First, typename... Rest>
struct indexers_tuple_impl<std::tuple<InTuple...>, First, Rest...>
{
    using type = std::conditional_t<
        std::is_same_v<First, scalar_indexer>,
        typename indexers_tuple_impl<std::tuple<InTuple...>, Rest...>::type,
        typename indexers_tuple_impl<std::tuple<InTuple..., First>, Rest...>::type>;
};
template<typename... InTuple>
struct indexers_tuple_impl<std::tuple<InTuple...>>
{
    using type = std::tuple<InTuple...>;
};


template<typename... Indexers>
using indexers_tuple_t = typename indexers_tuple_impl<std::tuple<>, Indexers...>::type;

template<size_t A, size_t V, size_t S>
struct _scalar_base
{
    static constexpr auto category = view_category::Scalar;
    template<typename T, bool Const, typename...>
    using return_type = std::conditional_t<Const, T, T&>;
};

template<size_t A, size_t V, size_t S>
struct _simple_base
{
    static constexpr auto category = view_category::Simple;
    template<typename T, bool Const, typename...>
    using return_type = simple_view<T, A, V, Const>;
};

template<size_t A, size_t V, size_t S>
struct _regular_base
{
    static constexpr auto category = view_category::Regular;
    template<typename T, bool Const, typename...>
    using return_type = regular_view<T, A, V, S, Const>;
};

template<size_t A, size_t V, size_t S>
struct _general_base
{
    static constexpr auto category = view_category::General;
    template<typename T, bool Const, typename... Indexers>
    using return_type = general_view<T, A, V, S, indexers_tuple_t<Indexers...>, Const>;
};


template<size_t A, size_t V, size_t S>
struct G_I_ : _general_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, G_I_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};

template<size_t A, size_t V, size_t S>
struct I_SI_ : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_SI_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};

template<size_t A, size_t V, size_t S>
struct I_S : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_SI_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};

template<size_t A, size_t V, size_t S>
struct I_A_I_ : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_A_I_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};

template<size_t A, size_t V, size_t S>
struct I_A_ : _simple_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_A_I_<A + 1, V, S + 1>,
        std::conditional_t<std::is_same_v<Idx, all_indexer>, I_A_<A + 1, V + 1, 0>,
        G_I_<A + 1, V + 1, 0>
        >>;
};

template<size_t A, size_t V, size_t S>
struct I_UI_ : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_UI_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};

template<size_t A, size_t V, size_t S>
struct I_UA_I_ : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_UA_I_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};

template<size_t A, size_t V, size_t S>
struct I_UA_ : _simple_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_UA_I_<A + 1, V, S + 1>,
        std::conditional_t<std::is_same_v<Idx, all_indexer>, I_UA_<A + 1, V + 1, S>,
        G_I_<A + 1, V + 1, 0>
        >>;
};

template<size_t A, size_t V, size_t S>
struct I_U : _simple_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_UI_<A + 1, V, S + 1>,
        std::conditional_t<std::is_same_v<Idx, all_indexer>, I_UA_<A + 1, V + 1, 0>,
        G_I_<A + 1, V + 1, 0>
        >>;
};

template<size_t A, size_t V, size_t S> 
struct I_ : _scalar_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_<A + 1, V, 0>,
        std::conditional_t<std::is_same_v<Idx, all_indexer>, I_A_<A + 1, V + 1, 0>,
        std::conditional_t<std::is_same_v<Idx, unit_indexer>, I_U<A + 1, V + 1, 0>,
        std::conditional_t<std::is_same_v<Idx, step_indexer>, I_S<A + 1, V + 1, 0>,
        G_I_<A + 1, V + 1, 0>
        >>>>;
};



template<typename State, typename... Indexers>
struct view_derive_impl;

template<typename State, typename Idx1, typename... Idxs>
struct view_derive_impl<State, Idx1, Idxs...>
{
    using type = typename view_derive_impl<typename State::template collapse<Idx1>, Idxs...>::type;
};

template<typename State>
struct view_derive_impl<State>
{
    using type = State;
};

template<typename... Indexers>
using view_type = typename view_derive_impl<I_<0u, 0u, 0u>, Indexers...>::type;

}


}
