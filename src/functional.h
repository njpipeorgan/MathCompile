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

#include "traits.h"
#include "types.h"
#include "ndarray.h"
#include "utils.h"

namespace wl
{

template<typename T, size_t R, typename Function>
auto _select_impl(const ndarray<T, R>& a, Function f)
{
    static_assert(R >= 2u, "internal");
    auto ret_dims = a.dims();
    ret_dims[0] = 0u;
    ndarray<T, R> ret(ret_dims);
    size_t item_count = 0;
    auto view_iter = a.template view_begin<1u>();
    auto view_end = a.template view_end<1u>();
    size_t item_size = view_iter.size();
    for (; !view_iter.view_pos_equal(view_end); view_iter.step_forward())
    {
        auto out = f(view_iter);
        static_assert(is_boolean_v<decltype(out)>, "badfunctype");
        if (out)
        {
            ret.resize(++item_count, item_size);
            view_iter.copy_to(ret.begin() + (item_count - 1u) * item_size);
        }
    }
    return ret;
}

template<typename X, typename Function>
auto select(X&& x, Function f)
{
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> >= 1u, "badrank");
    if constexpr (array_rank_v<XT> == 1u)
    {
        std::vector<value_type_t<XT>> ret;
        x.for_each([&](const auto& a)
            {
                auto out = f(a);
                static_assert(is_boolean_v<decltype(out)>, "badfunctype");
                if (out) ret.push_back(a);
            });
        return ndarray<value_type_t<XT>, 1u>(
            std::array<size_t, 1u>{ret.size()}, std::move(ret));
    }
    else
        return _select_impl(std::forward<decltype(x)>(x).to_array(), f);
}

template<typename T, size_t R, typename Function>
auto _count_impl(const ndarray<T, R>& a, Function f)
{
    static_assert(R >= 2u, "internal");
    size_t item_count = 0;
    auto view_iter = a.template view_begin<1u>();
    auto view_end = a.template view_end<1u>();
    for (; !view_iter.view_pos_equal(view_end); view_iter.step_forward())
    {
        auto out = f(view_iter);
        static_assert(is_boolean_v<decltype(out)>, "badfunctype");
        if (out) ++item_count;
    }
    return item_count;
}

template<typename X, typename Function>
auto count(X&& x, Function f)
{
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> >= 1u, "badrank");
    if constexpr (array_rank_v<XT> == 1u)
    {
        size_t item_count = 0;
        x.for_each([&](const auto& a)
            {
                auto out = f(a);
                static_assert(is_boolean_v<decltype(out)>, "badfunctype");
                if (out) ++item_count;
            });
        return item_count;
    }
    else
        return _count_impl(std::forward<decltype(x)>(x).to_array(), f);
}


template<size_t Level, typename Function, typename T, size_t R>
auto _map_impl(Function f, const ndarray<T, R>& a)
{
    static_assert(1 <= Level && Level <= R, "badlevel");
    auto a_iter = a.template view_begin<Level>();
    using RT = remove_cvref_t<decltype(f(*a_iter))>;
    const auto map_dims = utils::dims_take<1, Level>(a.dims());
    if constexpr (array_rank_v<RT> == 0u)
    {
        ndarray<RT, Level> ret(map_dims);
        ret.for_each([&](auto& r) { r = f(*a_iter); ++a_iter; });
        return ret;
    }
    else
    {
        auto first_item = f(*a_iter);
        const auto item_dims = first_item.dims();
        ndarray<value_type_t<RT>, Level + array_rank_v<RT>> ret(
            utils::dims_join(map_dims, item_dims));
        auto ret_iter = ret.template view_begin<Level>();
        first_item.copy_to(ret_iter.begin());
        ++a_iter;
        ++ret_iter;
        for (size_t i = 0; i < utils::size_of_dims(map_dims);
            ++i, ++a_iter, ++ret_iter)
        {
            auto item = f(*a_iter);
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error("baddims");
            item.copy_to(ret_iter.begin());
        }
        return ret;
    }
}

template<typename Function, typename X, int64_t I>
auto map(Function f, X&& x, const_int<I>)
{
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 1, "badrank");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(1 <= Level && Level <= int64_t(R), "badlevel");
    if constexpr (R == size_t(Level))
    {
        using RT = remove_cvref_t<decltype(f(value_type_t<XT>{}))>;
        if constexpr (array_rank_v<RT> == 0u)
            return utils::listable_function(f, std::forward<decltype(x)>(x));
        else
            return _map_impl<Level>(f, x.to_array());
    }
    else
        return _map_impl<Level>(f, x.to_array());
}

template<typename Function, typename X>
auto map(Function f, X&& x)
{
    return map(f, std::forward<decltype(x)>(x), const_int<1>{});
}

template<typename Function, typename X>
auto nest(Function f, X&& x, const int64_t n)
{
    using XT0 = remove_cvref_t<X>;
    constexpr auto rank0 = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error("badargv");

    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    if constexpr (rank0 == 0)
    {
        static_assert(is_convertible_v<XT0, XT2> &&
            is_convertible_v<XT1, XT2>, "badargtype");
        if (n == 0)
            return cast<XT2>(x);
        else // n >= 1
        {
            auto ret = cast<XT2>(f(std::forward<decltype(x)>(x)));
            for (size_t i = 1u; i < size_t(n); ++i)
                ret = cast<XT2>(f(ret));
            return ret;
        }
    }
    else
    {
        constexpr auto rank1 = array_rank_v<XT1>;
        constexpr auto rank2 = array_rank_v<XT2>;
        using XV0 = value_type_t<XT0>;
        using XV1 = value_type_t<XT1>;
        using XV2 = value_type_t<XT2>;
        static_assert(rank0 == rank2 && rank1 == rank2, "badargtype");
        static_assert(is_convertible_v<XV0, XV2> &&
            is_convertible_v<XV1, XV2>, "badargtype");
        if (n == 0)
        {
            if constexpr (rank0 == rank2 && std::is_same_v<XV0, XV2>)
                return val(std::forward<decltype(x)>(x));
            else
            {
                ndarray<XV2, rank2> ret(x.dims());
                x.copy_to(ret.begin());
                return ret;
            }
        }
        else // n >= 1
        {
            auto ret = val(f(std::forward<decltype(x)>(x)));
            for (size_t i = 1u; i < size_t(n); ++i)
            {
                auto temp = val(f(std::move(ret)));
                set(ret, std::move(temp));
            }
            return ret;
        }
    }
}

template<typename Function, typename X>
auto nest_list(Function f, X&& x, const int64_t n)
{
    using XT0 = remove_cvref_t<X>;
    constexpr auto rank0 = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error("badargv");

    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    if constexpr (rank0 == 0)
    {
        static_assert(is_convertible_v<XT0, XT2> &&
            is_convertible_v<XT1, XT2>, "badargtype");
        ndarray<XT2, 1u> ret(std::array<size_t, 1u>{size_t(n) + 1u});
        ret[0] = cast<XT2>(x);
        for (size_t i = 1; i <= size_t(n); ++i)
            ret[i] = cast<XT2>(f(ret[i - 1]));
        return ret;
    }
    else
    {
        constexpr auto rank1 = array_rank_v<XT1>;
        constexpr auto rank2 = array_rank_v<XT2>;
        using XV0 = value_type_t<XT0>;
        using XV1 = value_type_t<XT1>;
        using XV2 = value_type_t<XT2>;
        static_assert(rank0 == rank2 && rank1 == rank2, "badargtype");
        static_assert(is_convertible_v<XV0, XV2> &&
            is_convertible_v<XV1, XV2>, "badargtype");

        auto item_dims = x.dims();
        auto ret_dims = utils::dims_join(
            std::array<size_t, 1u>{size_t(n) + 1u}, item_dims);
        if (n == 0)
        {
            if constexpr (std::is_same_v<XT0, XT2>)
            {
                return ndarray<XV2, rank2 + 1u>(ret_dims,
                    std::forward<decltype(x)>(x).data_vector());
            }
            else
            {
                ndarray<XV2, rank2 + 1u> ret(ret_dims);
                x.copy_to(ret.begin());
                return ret;
            }
        }
        else // n >= 1
        {
            ndarray<XV2, rank2 + 1u> ret(ret_dims);
            auto view_iter = ret.template view_begin<1u>();
            x.copy_to(view_iter.begin());
            ++view_iter;
            auto item = val(f(std::forward<decltype(x)>(x)));
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error("baddims");
            item.copy_to(view_iter.begin());
            ++view_iter;
            for (int64_t i = 1; i < n; ++i, ++view_iter)
            {
                auto temp = val(f(std::move(item)));
                set(item, std::move(temp));
                if (!utils::check_dims(item.dims(), item_dims))
                    throw std::logic_error("baddims");
                item.copy_to(view_iter.begin());
            }
            return ret;
        }
    }
}

template<bool FoldL, typename Function, typename X, typename YIter>
auto _fold_impl(Function f, X&& x, YIter y_iter, const size_t n)
{
    using XT0 = remove_cvref_t<X>;
    using XV0 = value_type_t<XT0>;
    constexpr auto rank0 = array_rank_v<XT0>;
    using XT1 = remove_cvref_t<decltype(f(std::declval<X&&>(), *y_iter))>;
    using XT2 = remove_cvref_t<decltype(f(std::declval<XT1&&>(), *y_iter))>;

    if constexpr (rank0 == 0)
    {
        static_assert(is_convertible_v<XT0, XT2> && 
            is_convertible_v<XT1, XT2>, "badargtype");
        if (n == 0)
            return cast<XT2>(std::forward<decltype(x)>(x));
        else // n >= 1
        {
            auto ret = cast<XT2>(
                f(std::forward<decltype(x)>(x), *y_iter));
            for (size_t i = 1u; i < n; ++i)
            {
                if constexpr (FoldL)
                    ++y_iter;
                else
                    --y_iter;
                ret = cast<XT2>(f(ret, *y_iter));
            }
            return ret;
        }
    }
    else
    {
        constexpr auto rank1 = array_rank_v<XT1>;
        constexpr auto rank2 = array_rank_v<XT2>;
        using XV0 = value_type_t<XT0>;
        using XV1 = value_type_t<XT1>;
        using XV2 = value_type_t<XT2>;
        static_assert(rank0 == rank2 && rank1 == rank2, "badargtype");
        static_assert(is_convertible_v<XV0, XV2> &&
            is_convertible_v<XV1, XV2>, "badargtype");

        if (n == 0)
        {
            if constexpr (rank0 == rank2 && std::is_same_v<XV0, XV2>)
                return val(std::forward<decltype(x)>(x));
            else
            {
                ndarray<XV2, rank2> ret(x.dims());
                x.copy_to(ret.begin());
                return ret;
            }
        }
        else // n >= 1
        {
            auto ret = val(f(std::forward<decltype(x)>(x), *y_iter));
            for (size_t i = 1u; i < n; ++i)
            {
                if constexpr (FoldL)
                    ++y_iter;
                else
                    --y_iter;
                auto temp = val(f(std::move(ret), *y_iter));
                set(ret, std::move(temp));
            }
            return ret;
        }
    }
}

template<typename Function, typename X, typename Y>
auto fold(Function f, X&& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<Y> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if constexpr (is_array_v<YT>)
    {
        return _fold_impl<true>(f, std::forward<decltype(x)>(x),
            y.template view_begin<1u>(), y.dims()[0]);
    }
    else
    {
        auto valy = val(std::forward<decltype(y)>(y));
        return _fold_impl<true>(f, std::forward<decltype(x)>(x),
            valy.template view_begin<1u>(), valy.dims()[0]);
    }
}

template<typename Function, typename X, typename Y>
auto foldr(Function f, X&& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<Y> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if constexpr (is_array_v<YT>)
    {
        return _fold_impl<false>(f, std::forward<decltype(x)>(x),
            --(y.template view_end<1u>()), y.dims()[0]);
    }
    else
    {
        auto valy = val(std::forward<decltype(y)>(y));
        return _fold_impl<false>(f, std::forward<decltype(x)>(x),
            --(valy.template view_end<1u>()), valy.dims()[0]);
    }
}

template<typename Function, typename Y>
auto fold(Function f, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<Y> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if (n < 1u)
        throw std::logic_error("baddims");
    if constexpr (is_array_v<YT>)
    {
        auto y_first = y.template view_begin<1u>();
        auto y_rest = y.template view_begin<1u>();
        return _fold_impl<true>(f, *y_first, ++y_rest, n - 1u);
    }
    else
    {
        auto valy = val(std::forward<decltype(y)>(y));
        auto y_first = valy.template view_begin<1u>();
        auto y_rest = valy.template view_begin<1u>();
        return _fold_impl<true>(f, *y_first, ++y_rest, n - 1u);
    }
}

template<typename Function, typename Y>
auto foldr(Function f, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<Y> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if (n < 1u)
        throw std::logic_error("baddims");
    if constexpr (is_array_v<YT>)
    {
        auto y_first = --(y.template view_end<1u>());
        auto y_rest = --(y.template view_end<1u>());
        return _fold_impl<false>(f, *y_first, --y_rest, n - 1u);
    }
    else
    {
        auto valy = val(std::forward<decltype(y)>(y));
        auto y_first = --(valy.template view_end<1u>());
        auto y_rest = --(valy.template view_end<1u>());
        return _fold_impl<false>(f, *y_first, --y_rest, n - 1u);
    }
}


}
