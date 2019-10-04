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
#include <optional>
#include <tuple>

#include "types.h"
#include "traits.h"
#include "const.h"
#include "view_derive.h"

namespace wl
{

template<bool UnitStep>
struct indexer_iter;

template<>
struct indexer_iter<false>
{
    size_t index_;
    ptrdiff_t step_;

    indexer_iter(size_t index, ptrdiff_t step) :
        index_{index}, step_{step}
    {
    }

    auto& operator++()
    {
        index_ += step_;
        return *this;
    }

    bool operator==(const indexer_iter& other) const
    {
        return this->index_ == other.index_;
    }
};

template<>
struct indexer_iter<true>
{
    size_t index_;

    explicit indexer_iter(size_t index) :
        index_{index}
    {
    }

    auto& operator++()
    {
        ++index_;
        return *this;
    }

    bool operator==(const indexer_iter& other) const
    {
        return this->index_ == other.index_;
    }
};

template<typename IndexType>
size_t convert_index(const IndexType& idx, const size_t& dim)
{
    static_assert(is_integral_v<IndexType>, "badidxtype");
    if constexpr (std::is_unsigned_v<IndexType>)
    {
        if (1u <= idx && idx <= dim)
            return size_t(idx - 1u);
        else
            throw std::logic_error("badindex");
    }
    else
    {
        ptrdiff_t pos_idx = idx >= 0 ?
            idx : idx + ptrdiff_t(dim) + 1;
        if (1 <= pos_idx && pos_idx <= ptrdiff_t(dim))
            return size_t(pos_idx - 1u);
        else
            throw std::logic_error("badindex");
    }
    return 0u;
};

struct scalar_indexer
{
    size_t index_;

    scalar_indexer() = default;

    template<typename IndexType>
    scalar_indexer(IndexType index, size_t dim) :
        index_{convert_index(index, dim)}
    {
    }

    size_t offset() const
    {
        return this->index_;
    }

    size_t size() = delete;

    ptrdiff_t stride() = delete;
};

struct all_indexer
{
    size_t size_;

    all_indexer() = default;

    explicit all_indexer(size_t dim) :
        size_{dim}
    {
    }

    size_t offset() const
    {
        return 0u;
    }

    size_t size() const
    {
        return this->size_;
    }

    ptrdiff_t stride() const
    {
        return 1;
    }
};

struct unit_indexer
{
    size_t begin_;
    size_t size_;

    unit_indexer() = default;

    unit_indexer(size_t begin, size_t size) :
        begin_{begin}, size_{size}
    {
    }

    size_t offset() const
    {
        return this->begin_;
    }

    size_t size() const
    {
        return this->size_;
    }

    ptrdiff_t stride() const
    {
        return 1;
    }

    auto begin() const
    {
        return indexer_iter<true>(begin_);
    }

    auto end() const
    {
        return indexer_iter<true>(begin_ + size_);
    }
};

struct step_indexer
{
    size_t begin_;
    size_t size_;
    ptrdiff_t step_;

    step_indexer() = default;

    step_indexer(size_t begin, size_t size, ptrdiff_t step) :
        begin_{begin}, size_{size}, step_{step}
    {
    }

    size_t offset() const
    {
        return this->begin_;
    }

    size_t size() const
    {
        return this->size_;
    }

    ptrdiff_t stride() const
    {
        return step_;
    }

    auto begin() const
    {
        return indexer_iter<false>(begin_, step_);
    }

    auto end() const
    {
        return indexer_iter<false>(begin_ + step_ * size_, step_);
    }
};

struct list_indexer
{
    std::vector<size_t> indices_;

    list_indexer() = default;

    template<typename Iter>
    list_indexer(Iter idx_begin, Iter idx_end, size_t level_dim)
    {
        using IndexType = remove_cvref_t<decltype(*idx_begin)>;
        indices_.resize(idx_end - idx_begin);
        std::transform(idx_begin, idx_end, indices_.begin(),
            [&](const auto& idx) { return convert_index(idx, level_dim); });
    }

    size_t offset() const
    {
        return 0u;
    }

    ptrdiff_t stride() = delete;

    size_t size() const
    {
        return indices_.size();
    }

    const auto& indices() const
    {
        return this->indices_;
    }
};

template<typename Begin, typename End, typename Step>
struct span
{
    static constexpr auto default_begin = std::is_same_v<Begin, all_type>;
    static constexpr auto default_end = std::is_same_v<End, all_type>;
    static constexpr auto default_step = std::is_same_v<Step, all_type>;

    static_assert(default_begin || is_integral_v<Begin>, "badargtype");
    static_assert(default_end || is_integral_v<End>, "badargtype");
    static_assert(default_step || is_integral_v<Step>, "badargtype");

    Begin begin_;
    End end_;
    Step step_;

    span(Begin begin, End end, Step step) :
        begin_{begin}, end_{end}, step_{step}
    {
    }

    auto to_indexer(size_t dim) const
    {
        if constexpr (default_begin && default_end && default_step)
            return all_indexer(dim);
        else
        {
            // convert WL-style index to C-style index
            ptrdiff_t begin = 1;
            ptrdiff_t end = ptrdiff_t(dim);
            ptrdiff_t step = 1;
            if constexpr (!default_begin)
            {
                if constexpr (std::is_unsigned_v<Begin>)
                    begin = ptrdiff_t(this->begin_);
                else if (this->begin_ >= 0)
                    begin = ptrdiff_t(this->begin_);
                else // this->begin_ < 0
                    begin = ptrdiff_t(this->begin_) + ptrdiff_t(dim + 1u);
                //check out-of-bound
                if (begin < 1 || begin > ptrdiff_t(dim))
                    throw std::logic_error("badindex");
            }
            if constexpr (!default_end)
            {
                if constexpr (std::is_unsigned_v<End>)
                    end = ptrdiff_t(this->end_);
                else if (this->end_ >= 0)
                    end = ptrdiff_t(this->end_);
                else // this->end_ < 0
                    end = ptrdiff_t(this->end_) + ptrdiff_t(dim + 1u);
                //check out-of-bound
                if (end < 1 || end > ptrdiff_t(dim))
                    throw std::logic_error("badindex");
            }
            if constexpr (!default_step)
            {
                step = step_;
                if (step == 0)
                    throw std::logic_error("badstep");
            }

            if constexpr (default_step)
            {
                if constexpr (default_begin || default_end)
                {
                    return unit_indexer(
                        size_t(begin - 1),
                        size_t(end - begin + 1));
                }
                else
                {
                    return unit_indexer(
                        size_t(begin - 1),
                        end >= begin ? size_t(end - begin + 1) : 0u);
                }
            }
            else if (step > 0)
            {
                return step_indexer(
                    size_t(begin - 1),
                    end >= begin ? size_t((end - begin) / step) + 1u : 0u,
                    step);
            }
            else // step < 0
            {
                return step_indexer(
                    size_t(begin),
                    end <= begin ? size_t((begin - end) / -step) + 1u : 0u,
                    step);
            }
        }
    }
};

template<typename Begin, typename End, typename Step>
auto make_span(const Begin& begin, const End& end, const Step& step)
{
    return span<Begin, End, Step>(begin, end, step);
}

template<typename Begin, typename End>
auto make_span(const Begin& begin, const End& end)
{
    return make_span(begin, end, const_all);
}

template<typename Begin, typename End, typename Step>
auto make_indexer(const span<Begin, End, Step>& s, size_t dim)
{
    return s.to_indexer(dim);
}

template<typename IndexType, size_t Rank>
auto make_indexer(const ndarray<IndexType, Rank>& list, size_t dim)
{
    static_assert(Rank == 1u, "badargtype");
    static_assert(is_integral_v<IndexType>, "badidxtype");
    return list_indexer(list.begin(), list.end(), dim);
}

template<typename IndexType>
auto make_indexer(const IndexType& index, size_t dim)
{
    if constexpr (std::is_same_v<IndexType, all_type>)
        return all_indexer(dim);
    else
    {
        static_assert(is_integral_v<IndexType>, "badidxtype");
        return scalar_indexer(index, dim);
    }
}

template<size_t Rank>
std::optional<size_t> _size_if_equal_dim(const size_t* dim1, const size_t* dim2)
{
    static_assert(Rank >= 1u, "internal");
    if constexpr (Rank == 1u)
    {
        if (*dim1 == *dim2)
            return *dim1;
        else
            return {};
    }
    else
    {
        auto size = _size_if_equal_dim<Rank - 1>(dim1 + 1, dim2 + 1);
        if (size && *dim1 == *dim2)
            return size * (*dim1);
        else
            return {};
    }
}

template<typename T, size_t ArrayRank, size_t ViewRank, bool Const>
struct simple_view
{
    static constexpr auto is_const = Const;
    static constexpr auto array_rank = ArrayRank;
    static constexpr auto view_rank = ViewRank;
    static constexpr auto rank = ViewRank;
    static constexpr auto category = view_category::Simple;

    using value_type = T;
    using array_ref_type = std::conditional_t<Const,
        const ndarray<T, ArrayRank>&, ndarray<T, ArrayRank>&>;
    using pointer_type = std::conditional_t<Const, const T*, T*>;
    using _dims_t = std::array<size_t, ViewRank>;

    pointer_type const identifier_;
    pointer_type data_;
    _dims_t dims_;

    template<typename... Specs>
    simple_view(array_ref_type base, const Specs&... specs) :
        identifier_{base.data()}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        size_t offset = 0u;
        this->_initialize<0u>(offset, base.dims_ptr(), specs...);
        this->data_ = base.data() + offset;
    }

    template<size_t Level, typename Spec1, typename... Specs>
    void _initialize(size_t& offset, const size_t* dims,
        const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level > 0)
            offset *= dims[Level];
        offset += spec1.offset();

        if constexpr (Level < ArrayRank - ViewRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->_initialize<Level + 1u>(offset, dims, specs...);
        }
        else
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>, "internal");
            constexpr size_t ViewLevel = Level - (ArrayRank - ViewRank);
            this->dims_[ViewLevel] = spec1.size();
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(offset, dims, specs...);
        }
    }

    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }

    auto begin() const
    {
        return this->data_;
    }

    template<typename Function>
    void for_each(Function f, size_t size) const
    {
        std::for_each_n(this->begin(), size, f);
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter, size_t size) const
    {
        this->for_each(
            [iter](const auto& val) mutable { *(iter++) = val; }, size);
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter, size_t size) const
    {
        this->for_each(
            [iter](auto& val) mutable { val = *(iter++); }, size);
    }

    auto to_array() const
    {
        return ndarray<T, ViewRank>(this->dims_, this->begin());
    }
};

template<typename T, bool Const>
struct regular_view_iterator
{
    using _my_type = regular_view_iterator;
    using value_type = T;
    using pointer_type = std::conditional_t<Const, const T*, T*>;

    pointer_type pointer_;
    size_t stride_;

    regular_view_iterator(pointer_type pointer, size_t stride) : 
        pointer_{pointer}, stride_{stride}
    {
    }

    auto& operator*()
    {
        return *this->pointer_;
    }

    auto operator*() const
    {
        return *this->pointer_;
    }

    auto operator++()
    {
        this->pointer_ += this->stride_;
        return *this;
    }

    auto operator==(const _my_type& other)
    {
        return this->pointer_ == other.pointer_;
    }

    auto operator+(ptrdiff_t diff) const
    {
        return _my_type(pointer_ + diff * stride_, stride_);
    }
};

template<typename T, size_t ArrayRank, size_t ViewRank, size_t StrideRank, bool Const>
struct regular_view
{
    static constexpr auto is_const = Const;
    static constexpr auto array_rank = ArrayRank;
    static constexpr auto view_rank = ViewRank;
    static constexpr auto stride_rank = StrideRank;
    static constexpr auto rank = ViewRank;
    static constexpr auto category = view_category::Regular;

    using value_type = T;
    using array_ref_type = std::conditional_t<Const,
        const ndarray<T, ArrayRank>&, ndarray<T, ArrayRank>&>;
    using pointer_type = std::conditional_t<Const, const T*, T*>;
    using _dims_t = std::array<size_t, ViewRank>;

    pointer_type const identifier_;
    pointer_type data_;
    ptrdiff_t stride_;
    _dims_t dims_;

    template<typename... Specs>
    regular_view(array_ref_type base, const Specs&... specs) : 
        identifier_{base.data()}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        this->stride_ = 1;
        size_t offset = 0u;
        this->_initialize<0u>(offset, base.dims_ptr(), specs...);
        this->data_ = base.data() + offset;
    }

    template<size_t Level, typename Spec1, typename... Specs>
    auto _initialize(size_t& offset, const size_t* dims,
        const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level > 0)
            offset *= dims[Level];
        offset += spec1.offset();

        if constexpr (Level < ArrayRank - ViewRank - StrideRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->_initialize<Level + 1>(offset, dims, specs...);
        }
        else if constexpr (Level < ArrayRank - StrideRank)
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>, "internal");
            constexpr size_t ViewLevel = Level - 
                (ArrayRank - ViewRank - StrideRank);
            this->dims_[ViewLevel] = spec1.size();
            if constexpr (std::is_same_v<Spec1, step_indexer>)
                this->stride_ *= spec1.stride();
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(offset, dims, specs...);
        }
        else
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->stride_ *= dims[Level];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(offset, dims, specs...);
        }
    }

    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }

    auto begin() const
    {
        return regular_view_iterator<T, Const>(this->data_, this->stride_);
    }

    template<typename Function>
    void for_each(Function f, size_t size) const
    {
        std::for_each_n(this->begin(), size, f);
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter, size_t size) const
    {
        this->for_each(
            [iter](const auto& val) mutable { *(iter++) = val; }, size);
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter, size_t size) const
    {
        this->for_each(
            [iter](auto& val) mutable { val = *(iter++); }, size);
    }

    auto to_array() const
    {
        return ndarray<T, ViewRank>(this->dims_, this->begin());
    }
};

template<typename T, size_t ArrayRank, size_t ViewRank, size_t StrideRank, typename IndexersTuple, bool Const>
struct general_view
{
    static_assert(ViewRank == std::tuple_size_v<IndexersTuple>, "internal");

    static constexpr auto is_const = Const;
    static constexpr auto array_rank = ArrayRank;
    static constexpr auto view_rank = ViewRank;
    static constexpr auto stride_rank = StrideRank;
    static constexpr auto rank = ViewRank;
    static constexpr auto category = view_category::General;

    using value_type = T;
    using array_ref_type = std::conditional_t<Const,
        const ndarray<T, ArrayRank>&, ndarray<T, ArrayRank>&>;
    using pointer_type = std::conditional_t<Const, const T*, T*>;
    using _dims_t = std::array<size_t, ViewRank>;
    using _strides_t = std::array<ptrdiff_t, ViewRank>;
    using _indexers_tuple = IndexersTuple;

    static constexpr auto _has_last_stride = (StrideRank != 0) || 
        std::is_same_v<
        std::tuple_element_t<ViewRank - 1u, IndexersTuple>, 
        step_indexer>;

    pointer_type const identifer_;
    pointer_type data_;
    _dims_t dims_;
    _strides_t strides_;
    _indexers_tuple indexers_;

    template<typename... Specs>
    general_view(array_ref_type base, const Specs&... specs) :
        identifier_{base.data()}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        size_t offset = 0u;
        this->_initialize<0u, 0u>(offset, base.dims_ptr(), specs...);
        this->data_ = base.data() + offset;
    }

    template<size_t Level, size_t ViewLevel, typename Spec1, typename... Specs>
    auto _initialize(size_t& offset, const size_t* dims,
        const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level > 0)
            offset *= dims[Level];
        offset += spec1.offset();

        if constexpr (std::is_same_v<Spec1, scalar_indexer>)
        {
            if constexpr (ViewLevel > 0u)
                this->strides_[ViewLevel - 1] *= dims[Level];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u, ViewLevel>(offset, dims, specs...);
        }
        else
        {
            if constexpr (ViewLevel > 0u)
                this->strides_[ViewLevel - 1] *= dims[Level];
            if constexpr (std::is_same_v<Spec1, step_indexer>)
                this->strides_[ViewLevel] = spec1.stride();
            else
                this->strides_[ViewLevel] = 1;
            this->dims_[ViewLevel] = spec1.size();
            std::get<ViewLevel>(this->indexers_) = spec1;
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u, ViewLevel + 1u>(
                    offset, dims, specs...);
        }
    }

    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }

    auto begin() const = delete;
    
    template<size_t ViewLevel, typename Function>
    void _for_each_impl(size_t index, Function& f) const
    {
        const auto& indexer = std::get<ViewLevel>(this->indexers_);
        using Indexer = remove_cvref_t<decltype(indexer)>;

        if constexpr (ViewLevel > 0u)
            index *= this->strides_[ViewLevel - 1];
        if constexpr (ViewLevel < ViewRank - 1u)
        {
            if constexpr (std::is_same_v<Indexer, list_indexer>)
                for (const auto& i : indexer.indices())
                    _copy_from_impl<ViewLevel + 1u>(index + i, iter);
            else
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                    _copy_from_impl<ViewLevel + 1u>(index + i, iter);
        }
        else if constexpr (_has_last_stride)
        {
            const auto last_stride = this->strides_[ViewLevel];
            if constexpr (std::is_same_v<Indexer, list_indexer>)
                for (const auto& i : indexer.indices())
                    f(this->data_[(index + i) * last_stride]);
            else
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                    f(this->data_[(index + i) * last_stride]);
        }
        else
        {
            if constexpr (std::is_same_v<Indexer, list_indexer>)
                for (const auto& i : indexer.indices())
                    f(this->data_[index + i]);
            else
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                    f(this->data_[index + i]);
        }
    }

    template<typename Function>
    void for_each(Function f, size_t = 0u) const
    {
        this->_for_each_impl(0u, f);
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter, size_t = 0u) const
    {
        this->for_each(
            [iter](const auto& val) mutable { *(iter++) = val; });
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter, size_t = 0u) const
    {
        this->for_each(
            [iter](auto& val) mutable { val = *(iter++); });
    }

    auto to_array() const
    {
        auto ret = ndarray<T, ViewRank>(this->dims_);
        this->copy_to(ret.begin());
        return ret;
    }
};

template<typename T, size_t ArrayRank, size_t ViewRank, bool Const>
auto view_guard(const simple_view<T, ArrayRank, ViewRank, Const>& view)
{
    return view.to_array();
}

template<typename Any>
auto view_guard(Any&& any)
{
    if constexpr (is_array_view_v<remove_cvref_t<Any>>)
        return std::forward<decltype(any)>(any).to_array();
    else
        return std::forward<decltype(any)>(any);
}

template<typename T>
struct scalar_view_iterator
{
    using _my_type = scalar_view_iterator;
    using value_type = T;

    T val_;

    scalar_view_iterator(const T& val) : val_{val}
    {
    }

    auto operator++() const
    {
        return *this;
    }
    
    auto operator==(const _my_type&) = delete;

    const T& operator*() const
    {
        return val_;
    }
};

template<typename ViewDst, typename ViewSrc>
auto _view_view_assign(const ViewDst& dst, const ViewSrc& src)
{
    static_assert(!ViewDst::is_const, "modifyconst");
    static_assert(is_array_view_v<ViewDst> && is_array_view_v<ViewSrc>,
        "internal");
    static_assert(ViewDst::rank == ViewSrc::rank, "badrank");

    auto size = _size_if_equal_dim<ViewDst::rank>(
        dst.dims_ptr(), src.dims_ptr());
    if (!size.has_value())
        throw std::logic_error("baddims");
    if constexpr (ViewSrc::category != view_category::General)
        dst.copy_from(src.begin(), size.value());
    else if constexpr (ViewDst::category != view_category::General)
        src.copy_to(dst.begin(), size.value());
    else // both are general view
    {
        std::vector<typename ViewDst::value_type> buffer(size.value());
        src.copy_to(buffer.begin(), size.value());
        dst.copy_from(buffer.begin(), size.value());
    }
    return src;
}

template<typename View, typename T, size_t R>
auto view_assign(const View& dst, const ndarray<T, R>& src)
{
    static_assert(!View::is_const, "modifyconst");
    static_assert(View::rank == R, "badrank");
    auto size = _size_if_equal_dim<R>(dst.dims_ptr(), src.dims_ptr());
    if (!size.has_value())
        throw std::logic_error("baddims");
    dst.copy_from(src.begin(), size.value());
    return src;
}

template<typename Dst, typename Src>
auto view_assign(Dst&& dst, const Src& src)
{
    if constexpr (is_array_view_v<remove_cvref_t<Dst>>)
    {
        static_assert(!remove_cvref_t<Dst>::is_const, "modifyconst");
        if constexpr (is_array_view_v<Src>)
            return _view_view_assign(dst, src);
        else
        {
            auto size = _size_if_equal_dim<Dst::rank>(
                dst.dims_ptr(), dst.dims_ptr());
            auto iter = scalar_view_iterator(src);
            dst.copy_from(iter, size.value());
            return src;
        }
    }
    else
    {
        std::forward<decltype(dst)>(dst) = src;
    }
}


}
