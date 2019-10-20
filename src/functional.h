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

template<typename ArgIter, bool HasStride>
struct argument_pack
{
    using value_type = remove_cvref_t<decltype(*std::declval<ArgIter>())>;

    const ArgIter iter_;
    const size_t size_;
    const size_t stride_;

    argument_pack(ArgIter iter, size_t size, size_t stride = 1u) :
        iter_{iter}, size_{size}, stride_{stride}
    {
    }

    size_t size() const
    {
        return this->size_;
    }

    auto get(size_t i) const
    {
        if (i >= size_)
            throw std::logic_error("badargc");
        if constexpr (HasStride)
            return *(this->iter_ + i * stride_);
        else
            return *(this->iter_ + i);
    }

    auto get_pack(size_t i) const
    {
        if (i >= size_)
            throw std::logic_error("badargc");
        if constexpr (HasStride)
            return argument_pack(iter_ + i * stride_, size_ - i);
        else
            return argument_pack(iter_ + i, size_ - i);
    }
};

template<typename Normal, typename Variadic>
struct variadic
{
    Normal nf_;
    Variadic vf_;

    variadic(Normal nf, Variadic vf) :
        nf_{std::move(nf)}, vf_{std::move(vf)}
    {
    }

    template<typename... Args>
    auto operator()(Args&&... args) const -> decltype(auto)
    {
        return nf_(std::forward<decltype(args)>(args)...);
    }

    template<typename T>
    auto operator()(const argument_pack<T>& args) const
    {
        return vf_(args);
    }

    template<typename T>
    auto operator()(argument_pack<T>&& args) const
    {
        return vf_(args);
    }
};

template<typename Function, typename X, int64_t I>
auto apply(Function f, const X& x, const_int<I>)
{
    static_assert(is_variadic_function_v<Function>, "badargtype");
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 1, "badrank");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(0 <= Level && Level < int64_t(R), "badlevel");

    const auto& valx = val(std::forward<decltype(x)>(x));
    auto x_iter = valx.template view_begin<Level + 1u>();
    const auto argc = valx.dims()[Level];
    using PackType = argument_pack<decltype(x_iter)>;
    using RT = remove_cvref_t<decltype(f(std::declval<PackType>()))>;

    const auto apply_dims = utils::dims_take<1u, Level>(valx.dims());
    if constexpr (Level == 0u)
    {
        return f(PackType(x_iter, argc));
    }
    else if constexpr (array_rank_v<RT> == 0u)
    {
        ndarray<value_type_t<RT>, Level> ret(apply_dims);
        auto ret_iter = ret.begin();
        auto ret_size = ret.size();
        for (size_t i = 0u; i < ret_size; ++i, ++ret_iter, x_iter += argc)
            *ret_iter = f(PackType(x_iter, argc));
        return ret;
    }
    else
    {
        auto first_item = f(PackType(x_iter, argc));
        const auto item_dims = first_item.dims();
        ndarray<value_type_t<RT>, Level + array_rank_v<RT>> ret(
            utils::dims_join(apply_dims, item_dims));
        auto ret_iter = ret.template view_begin<Level>();
        first_item.copy_to(ret_iter.begin());
        x_iter += argc;
        ++ret_iter;
        for (size_t i = 1; i < utils::size_of_dims(apply_dims);
            ++i, x_iter += argc, ++ret_iter)
        {
            auto item = f(PackType(x_iter, argc));
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error("baddims");
            item.copy_to(ret_iter.begin());
        }
        return ret;
    }
}

template<typename Function, typename X>
auto apply(Function f, X&& x)
{
    return apply(f, std::forward<decltype(x)>(x), const_int<0>{});
}

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
        for (size_t i = 1; i < utils::size_of_dims(map_dims);
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

template<typename Function, size_t R, typename... Iters>
auto _map_thread_impl2(Function f, const std::array<size_t, R>& map_dims, 
    Iters... iters)
{
    using RT = remove_cvref_t<decltype(f(*iters...))>;
    if constexpr (array_rank_v<RT> == 0u)
    {
        ndarray<RT, 1u> ret(map_dims);
        const auto ret_size = ret.size();
        auto ret_iter = ret.begin();
        for (size_t i = 0u; i < ret_size; ++i, ++ret_iter, (++iters, ...))
            *ret_iter = f(*iters...);
        return ret;
    }
    else
    {
        auto first_item = f(*iters...);
        const auto item_dims = first_item.dims();
        ndarray<value_type_t<RT>, R + array_rank_v<RT>> ret(
            utils::dims_join(map_dims, item_dims));
        const auto ret_size = utils::size_of_dims(map_dims);
        auto ret_iter = ret.template view_begin<R>();
        first_item.copy_to(ret_iter.begin());
        (++iters, ...);
        ++ret_iter;
        for (size_t i = 1; i < ret_size; ++i, ++ret_iter, (++iters, ...))
        {
            auto item = f(*iters...);
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error("baddims");
            item.copy_to(ret_iter.begin());
        }
        return ret;
    }
}

template<size_t I, typename Function, typename Arg1, typename... Args>
auto _map_thread_impl1(Function f, const Arg1& arg1, const Args&... args)
{
    const auto dims = utils::dims_take<1u, I>(arg1.dims());
    if (!(utils::check_dims(dims, utils::dims_take<1, I>(args.dims())) && ...))
        throw std::logic_error("baddims");
    return _map_thread_impl2(f, dims, arg1.template view_begin<size_t(I)>(), 
        args.template view_begin<size_t(I)>()...);
}

template<typename Function, int64_t I, typename... Args>
auto map_thread(Function f, const_int<I>, varg_tag, Args&&... args)
{
    static_assert(sizeof...(Args) >= 1u, "badargc");
    static_assert(1 <= I, "badlevel");
    static_assert(((array_rank_v<remove_cvref_t<Args>> >= size_t(I)) && ...),
        "badargtype");
    return _map_thread_impl1<size_t(I)>(f, 
        val(std::forward<decltype(args)>(args))...);
}

template<typename Function, typename... Args>
auto map_thread(Function f, varg_tag, Args&&... args)
{
    return map_thread(f, const_int<1>{}, varg_tag{}, 
        std::forward<decltype(args)>(args)...);
}

template<typename Function, typename X, int64_t I>
auto map_thread(Function f, X&& x, const_int<I>)
{
    static_assert(is_variadic_function_v<Function>, "badargtype");
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 2u, "badrank");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(1 <= Level && Level < int64_t(R), "badlevel");

    const auto& valx = val(std::forward<decltype(x)>(x));
    const auto map_dims = utils::dims_take<2u, Level + 1u>(valx.dims());
    const auto pack_size = valx.dims()[0];
    const auto pack_stride = utils::size_of_dims(map_dims);
    auto x_iter = valx.template view_begin<Level + 1u>();
    using IterType = remove_cvref_t<decltype(x_iter)>;
    using RT = remove_cvref_t<
        decltype(f(std::declval<argument_pack<IterType, true>>()))>;
        
    if constexpr (array_rank_v<RT> == 0u)
    {
        ndarray<RT, Level> ret(map_dims);
        auto ret_iter = ret.begin();
        const auto ret_size = ret.size();
        for (size_t i = 0; i < ret_size; ++i, ++ret_iter, ++x_iter)
            *ret_iter = f(argument_pack<IterType, true>(
                x_iter, pack_size, pack_stride));
        return ret;
    }
    else
    {
        auto first_item = f(argument_pack<IterType, true>(
            x_iter, pack_size, pack_stride));
        const auto item_dims = first_item.dims();
        ndarray<value_type_t<RT>, Level + array_rank_v<RT>> ret(
            utils::dims_join(map_dims, item_dims));
        auto ret_iter = ret.template view_begin<Level>();
        first_item.copy_to(ret_iter.begin());
        ++x_iter;
        ++ret_iter;
        for (size_t i = 1; i < pack_stride; ++i, ++x_iter, ++ret_iter)
        {
            auto item = f(argument_pack<IterType, true>(
                x_iter, pack_size, pack_stride));
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error("baddims");
            item.copy_to(ret_iter.begin());
        }
        return ret;
    }
}

template<typename Function, typename X>
auto map_thread(Function f, X&& x)
{
    return map_thread(f, std::forward<decltype(x)>(x), const_int<1>{});
}

template<typename Function, typename X>
auto nest(Function f, X&& x, const int64_t n)
{
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        "badargtype");
    constexpr auto rank = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error("badargv");

    if (n == 0)
        return cast<XT2>(std::forward<decltype(x)>(x));
    else // n >= 1
    {
        auto ret = cast<XT2>(f(std::forward<decltype(x)>(x)));
        for (size_t i = 1u; i < size_t(n); ++i)
        {
            if constexpr (rank == 0u)
                ret = cast<XT2>(f(ret));
            else
            {
                auto temp = val(f(std::move(ret)));
                set(ret, std::move(temp));
            }
        }
        return ret;
    }
}

template<typename Function, typename X>
auto nest_list(Function f, X&& x, const int64_t n)
{
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        "badargtype");
    constexpr auto rank = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error("badargv");

    if constexpr (rank == 0u)
    {
        ndarray<XT2, 1u> ret(std::array<size_t, 1u>{size_t(n) + 1u});
        auto ret_iter = ret.begin();
        *ret_iter = cast<XT2>(std::forward<decltype(x)>(x));
        for (size_t i = 1; i < size_t(n); ++i, ++ret_iter)
            *(ret_iter + 1) = cast<XT2>(f(*ret_iter));
        return ret;
    }
    else
    {
        using XV2 = value_type_t<XT2>;
        auto item_dims = x.dims();
        auto ret_dims = utils::dims_join(
            std::array<size_t, 1u>{size_t(n) + 1u}, item_dims);
        if (n == 0)
        {
            auto item = cast<XT2>(std::forward<decltype(x)>(x));
            return ndarray<XV2, rank + 1u>(
                ret_dims, std::move(item).data_vector());
        }
        else // n >= 1
        {
            ndarray<XV2, rank + 1u> ret(ret_dims);
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

template<typename Function, typename X, typename YIter, typename YInc>
auto _fold_single_impl(Function f, X&& x, YIter y_iter, YInc y_inc,
    const size_t n)
{
    auto nest_f = [&](auto&& arg)
    {
        auto item = f(arg, *y_iter);
        y_inc(y_iter);
        return item;
    };
    return nest(nest_f, std::forward<decltype(x)>(x), n);
}

template<typename Function, typename X, typename YIter, typename YInc>
auto _fold_list_impl(Function f, X&& x, YIter y_iter, YInc y_inc,
    const size_t n)
{
    auto nest_f = [&](auto&& arg)
    {
        auto item = f(arg, *y_iter);
        y_inc(y_iter);
        return item;
    };
    return nest_list(nest_f, std::forward<decltype(x)>(x), n);
}

template<bool List, bool FoldL, typename Function, typename Y>
auto _fold_impl1(Function f, Y&& y)
{
    static_assert(array_rank_v<remove_cvref_t<Y>> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if (n < 1u) throw std::logic_error("baddims");
    const auto& valy = val(std::forward<decltype(y)>(y));
    if constexpr (List)
    {
        if constexpr (FoldL)
            return _fold_list_impl(f, *valy.template view_begin<1u>(),
                valy.template view_begin<1u>(),
                [](auto& iter) { ++iter; }, n);
        else
            return _fold_list_impl(f, *(--(valy.template view_end<1u>())),
                --(valy.template view_end<1u>()),
                [](auto& iter) { --iter; }, n);
    }
    else
    {
        if constexpr (FoldL)
            return _fold_single_impl(f, *valy.template view_begin<1u>(),
                valy.template view_begin<1u>(),
                [](auto& iter) { ++iter; }, n);
        else
            return _fold_single_impl(f, *(--(valy.template view_end<1u>())),
                --(valy.template view_end<1u>()),
                [](auto& iter) { --iter; }, n);
    }
}

template<bool List, bool FoldL, typename Function, typename X, typename Y>
auto _fold_impl1(Function f, X&& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<YT> >= 1u, "badargtype");
    const auto& valy = val(std::forward<decltype(y)>(y));
    const size_t n = valy.dims()[0];
    if constexpr (List)
    {
        if constexpr (FoldL)
            return _fold_list_impl(f, std::forward<decltype(x)>(x),
                valy.template view_begin<1u>(),
                [](auto& iter) { ++iter; }, n);
        else
            return _fold_list_impl(f, std::forward<decltype(x)>(x),
                --(valy.template view_end<1u>()),
                [](auto& iter) { --iter; }, n);
    }
    else
    {
        if constexpr (FoldL)
            return _fold_single_impl(f, std::forward<decltype(x)>(x),
                valy.template view_begin<1u>(),
                [](auto& iter) { ++iter; }, n);
        else
            return _fold_single_impl(f, std::forward<decltype(x)>(x),
                --(valy.template view_end<1u>()),
                [](auto& iter) { --iter; }, n);
    }
}

template<typename Function, typename... Any>
auto fold(Function f, Any&&... any)
{
    return _fold_impl1<false, true>(f, std::forward<decltype(any)>(any)...);
}

template<typename Function, typename... Any>
auto foldr(Function f, Any&&... any)
{
    return _fold_impl1<false, false>(f, std::forward<decltype(any)>(any)...);
}

template<typename Function, typename... Any>
auto fold_list(Function f, Any&&... any)
{
    return _fold_impl1<true, true>(f, std::forward<decltype(any)>(any)...);
}

template<typename Function, typename... Any>
auto foldr_list(Function f, Any&&... any)
{
    return _fold_impl1<true, false>(f, std::forward<decltype(any)>(any)...);
}

}
