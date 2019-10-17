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
#include <exception>
#include <iterator>
#include <optional>
#include <tuple>
#include <vector>

#include "types.h"
#include "traits.h"
#include "const.h"
#include "view_derive.h"
#include "utils.h"

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

struct cidx
{
    const size_t value_;

    constexpr explicit cidx(int64_t value) : value_{size_t(value)}
    {
    }

    constexpr size_t value() const
    {
        return value_;
    }
};

template<typename IndexType>
size_t convert_index(const IndexType& idx, const size_t& dim)
{
    if constexpr (std::is_same_v<IndexType, cidx>)
    {
        if (idx.value() < dim)
            return idx.value();
        else
            throw std::logic_error("badindex");
    }
    else if constexpr (std::is_unsigned_v<IndexType>)
    {
        if (1u <= idx && idx <= dim)
            return size_t(idx - 1u);
        else
            throw std::logic_error("badindex");
    }
    else
    {
        static_assert(is_integral_v<IndexType>, "internal");
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

    template<typename IndexType, typename Dim>
    scalar_indexer(IndexType index, const Dim& dim) :
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
                    throw std::logic_error("badvalue");
                else if (step < 0)
                {
                    if constexpr (default_begin)
                        begin = ptrdiff_t(dim);
                    if constexpr (default_end)
                        end = 1;
                }
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
                    size_t(begin - 1),
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
    else if constexpr (std::is_same_v<IndexType, cidx>)
        return scalar_indexer(index, dim);
    else
    {
        static_assert(is_integral_v<IndexType>, "badidxtype");
        return scalar_indexer(index, dim);
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
    using _base_dims_t = std::array<size_t, ArrayRank>;
    using _dims_t = std::array<size_t, ViewRank>;

    const void* const identifier_;
    pointer_type data_;
    _dims_t dims_;
    size_t size_;

    simple_view(const void* base_id, pointer_type view_data, const size_t* dims) :
        identifier_{base_id}, data_{view_data}
    {
        std::copy_n(dims, ViewRank, this->dims_.begin());
        this->size_ = utils::size_of_dims(this->dims_);
    }

    template<typename... Specs>
    simple_view(const T* base_id, pointer_type base_data, 
        const _base_dims_t& base_dims, const Specs&... specs) :
        identifier_{base_id}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        this->_initialize<0u>(specs...);
        this->data_ = base_data + 
            utils::linear_position(base_dims, specs.offset()...);
    }

    template<size_t Level, typename Spec1, typename... Specs>
    void _initialize(const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level < ArrayRank - ViewRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->_initialize<Level + 1u>(specs...);
        }
        else
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>, "internal");
            constexpr size_t ViewLevel = Level - (ArrayRank - ViewRank);
            this->dims_[ViewLevel] = spec1.size();
            this->size_ *= this->dims_[ViewLevel];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(specs...);
        }
    }

    auto dims() const
    {
        return this->dims_;
    }

    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }

    auto identifier() const
    {
        return this->identifier_;
    }

    auto begin() const
    {
        return this->data_;
    }

    auto data() const
    {
        return this->data_;
    }

    auto size() const
    {
        return this->size_;
    }

    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        if constexpr (std::is_same_v<bool, decltype(f(*data_, *iters...))>)
        {
            bool continue_flag = true;
            for (size_t i = 0u; i < this->size_ && continue_flag; ++i)
                continue_flag = !f(this->data_[i], iters[i]...);
        }
        else
        {
            for (size_t i = 0u; i < this->size_; ++i)
                f(this->data_[i], iters[i]...);
        }
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        for (size_t i = 0u; i < this->size_; ++i)
            iter[i] = this->data_[i];
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter) const
    {
        for (size_t i = 0u; i < this->size_; ++i)
            this->data_[i] = iter[i];
    }

    auto to_array() const
    {
        return ndarray<T, ViewRank>(this->dims_,
            this->begin(), this->begin() + this->size());
    }

    void apply_pointer_offset(ptrdiff_t diff)
    {
        this->data_ += diff;
    }

    simple_view& operator++()
    {
        this->apply_pointer_offset(ptrdiff_t(this->size_));
        return *this;
    }

    simple_view& operator--()
    {
        this->apply_pointer_offset(-ptrdiff_t(this->size_));
        return *this;
    }

    simple_view operator+(ptrdiff_t diff) const
    {
        auto copy = *this;
        copy.apply_pointer_offset(ptrdiff_t(this->size_) * diff);
        return copy;
    }

    simple_view operator-(ptrdiff_t diff) const
    {
        auto copy = *this;
        copy.apply_pointer_offset(ptrdiff_t(this->size_) * (-diff));
        return copy;
    }

    const simple_view& operator*() const
    {
        return *this;
    }

    bool operator==(const simple_view& other) const
    {
        return this->data_ == other.data_;
    }

    bool operator!=(const simple_view& other) const
    {
        return !(*this == other);
    }
};

template<typename T, bool Const>
struct regular_view_iterator
{
    using _my_type = regular_view_iterator;
    using iterator_category = std::forward_iterator_tag;
    using value_type = T;
    using difference_type = ptrdiff_t;
    using pointer = std::conditional_t<Const, const T*, T*>;
    using reference = std::conditional_t<Const, const T&, T&>;

    pointer pointer_;
    ptrdiff_t stride_;

    regular_view_iterator(pointer pointer, ptrdiff_t stride) :
        pointer_{pointer}, stride_{stride}
    {
    }

    reference operator*() const
    {
        return *this->pointer_;
    }

    auto operator++()
    {
        this->pointer_ += this->stride_;
        return *this;
    }

    auto operator++(int)
    {
        auto old_pointer = this->pointer_;
        this->pointer_ += this->stride_;
        return _my_type(old_pointer, this->stride_);
    }

    auto operator==(const _my_type& other) const
    {
        return this->pointer_ == other.pointer_;
    }

    auto operator!=(const _my_type& other) const
    {
        return !(*this == other);
    }

    auto operator+(ptrdiff_t diff) const
    {
        return _my_type(pointer_ + diff * stride_, stride_);
    }

    auto& operator[](ptrdiff_t diff) const
    {
        return pointer_ + diff * stride_;
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
    using _base_dims_t = std::array<size_t, ArrayRank>;
    using _dims_t = std::array<size_t, ViewRank>;

    const void* const identifier_;
    pointer_type data_;
    ptrdiff_t stride_;
    _dims_t dims_;
    size_t size_;

    template<typename... Specs>
    regular_view(const void* base_id, pointer_type base_data,
        const _base_dims_t& base_dims, const Specs&... specs) :
        identifier_{base_id}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        this->stride_ = 1;
        this->_initialize<0u>(base_dims, specs...);
        this->data_ = base_data +
            utils::linear_position(base_dims, specs.offset()...);
    }

    template<size_t Level, typename Spec1, typename... Specs>
    auto _initialize(const _base_dims_t& dims,
        const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level < ArrayRank - ViewRank - StrideRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->_initialize<Level + 1>(dims, specs...);
        }
        else if constexpr (Level < ArrayRank - StrideRank)
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>, "internal");
            constexpr size_t ViewLevel = Level -
                (ArrayRank - ViewRank - StrideRank);
            this->dims_[ViewLevel] = spec1.size();
            this->size_ *= this->dims_[ViewLevel];
            if constexpr (std::is_same_v<Spec1, step_indexer>)
                this->stride_ *= spec1.stride();
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(dims, specs...);
        }
        else
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->stride_ *= dims[Level];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(dims, specs...);
        }
    }

    auto dims() const
    {
        return this->dims_;
    }

    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }

    auto identifier() const
    {
        return this->identifier_;
    }

    auto begin() const
    {
        return regular_view_iterator<T, Const>(this->data_, this->stride_);
    }

    auto data() const
    {
        return this->data_;
    }

    auto size() const
    {
        return this->size_;
    }

    auto stride() const
    {
        return this->stride_;
    }

    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        if constexpr (std::is_same_v<bool, decltype(f(*data_, *iters...))>)
        {
            bool continue_flag = true;
            for (size_t i = 0u; i < this->size_ && continue_flag; ++i)
                continue_flag = !f(this->data_[i * stride_], iters[i]...);
        }
        else
        {
            for (size_t i = 0u; i < this->size_; ++i)
                f(this->data_[i * stride_], iters[i]...);
        }
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        for (size_t i = 0u; i < this->size_; ++i)
            iter[i] = this->data_[i * stride_];
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter) const
    {
        for (size_t i = 0u; i < this->size_; ++i)
            this->data_[i * stride_] = iter[i];
    }

    auto to_array() const
    {
        return ndarray<T, ViewRank>(this->dims_,
            this->begin(), this->begin() + this->size());
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
    using _base_dims_t = std::array<size_t, ArrayRank>;
    using _dims_t = std::array<size_t, ViewRank>;
    using _strides_t = std::array<ptrdiff_t, ViewRank>;
    using _indexers_tuple = IndexersTuple;

    static constexpr auto _has_last_stride = (StrideRank != 0) ||
        std::is_same_v<
        std::tuple_element_t<ViewRank - 1u, IndexersTuple>,
        step_indexer>;

    const void* const identifier_;
    pointer_type data_;
    _strides_t strides_;
    _indexers_tuple indexers_;
    _dims_t dims_;
    size_t size_;

    template<typename... Specs>
    general_view(const void* base_id, pointer_type base_data,
        const _base_dims_t& base_dims, Specs&&... specs) :
        identifier_{base_id}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        this->_initialize<0u, 0u>(
            base_dims, std::forward<decltype(specs)>(specs)...);
        this->data_ = base_data +
            utils::linear_position(base_dims, specs.offset()...);
    }

    template<size_t Level, size_t ViewLevel, typename Spec1, typename... Specs>
    auto _initialize(const _base_dims_t& dims,
        Spec1&& spec1, Specs&&... specs)
    {
        if constexpr (std::is_same_v<remove_cvref_t<Spec1>, scalar_indexer>)
        {
            if constexpr (ViewLevel > 0u)
                this->strides_[ViewLevel - 1] *= dims[Level];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u, ViewLevel>(
                    dims, std::forward<decltype(specs)>(specs)...);
        }
        else
        {
            if constexpr (ViewLevel > 0u)
                this->strides_[ViewLevel - 1] *= dims[Level];
            if constexpr (std::is_same_v<remove_cvref_t<Spec1>, step_indexer>)
                this->strides_[ViewLevel] = spec1.stride();
            else
                this->strides_[ViewLevel] = 1;
            this->dims_[ViewLevel] = spec1.size();
            this->size_ *= this->dims_[ViewLevel];
            std::get<ViewLevel>(this->indexers_) = 
                std::forward<decltype(spec1)>(spec1);
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u, ViewLevel + 1u>(
                    dims, std::forward<decltype(specs)>(specs)...);
        }
    }

    auto dims() const
    {
        return this->dims_;
    }

    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }

    auto identifier() const
    {
        return this->identifier_;
    }

    auto begin() const = delete;

    auto data() const = delete;

    auto size() const
    {
        return this->size_;
    }

    template<size_t ViewLevel, typename Function, typename... Iters>
    void _for_each_impl(size_t index, bool& break_flag, 
        Function& f, Iters&... iters) const
    {
        constexpr bool check_break =
            std::is_same_v<bool, decltype(f(*data_, *iters...))>;
        const auto& indexer = std::get<ViewLevel>(this->indexers_);
        using Indexer = remove_cvref_t<decltype(indexer)>;

        if constexpr (ViewLevel > 0u)
            index *= this->strides_[ViewLevel - 1];
        if constexpr (ViewLevel < ViewRank - 1u)
        {
            if constexpr (std::is_same_v<Indexer, list_indexer>)
                for (const auto& i : indexer.indices())
                {
                    _for_each_impl<ViewLevel + 1u>(
                        index + i, break_flag, f, iters...);
                    if (check_break && break_flag)
                        break;
                }
            else
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                {
                    _for_each_impl<ViewLevel + 1u>(
                        index + i, break_flag, f, iters...);
                    if (check_break && break_flag)
                        break;
                }
        }
        else
        {
            const auto last_stride = _has_last_stride ? 
                this->strides_[ViewLevel] : ptrdiff_t(1);
            if constexpr (std::is_same_v<Indexer, list_indexer>)
                for (const auto& i : indexer.indices())
                {
                    if constexpr (check_break)
                    {
                        if (f(this->data_[(index + i) * last_stride], 
                            (*iters++)...))
                            break;
                    }
                    else
                        f(this->data_[(index + i) * last_stride], 
                        (*iters++)...);
                }
            else
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                {
                    if constexpr (check_break)
                    {
                        if (f(this->data_[(index + i) * last_stride], 
                            (*iters++)...))
                            break;
                    }
                    else
                        f(this->data_[(index + i) * last_stride], 
                        (*iters++)...);
                }
        }
    }

    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        bool break_flag = false;
        this->_for_each_impl<0u>(0u, break_flag, f, iters...);
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        this->for_each([](const auto& src, auto& dst) { dst = src; }, iter);
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter) const
    {
        this->for_each([](auto& dst, const auto& src) { dst = src; }, iter);
    }

    auto to_array() const
    {
        auto ret = ndarray<T, ViewRank>(this->dims_);
        this->copy_to(ret.begin());
        return ret;
    }
};

template<typename Any>
auto val(Any&& any)
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

    constexpr scalar_view_iterator(const T& val) : val_{val}
    {
    }

    constexpr auto operator++() const
    {
        return *this;
    }

    auto operator==(const _my_type&) = delete;

    constexpr const T& operator*() const
    {
        return val_;
    }
};

template<typename T>
constexpr auto make_scalar_view_iterator(const T& val)
{
    return scalar_view_iterator<T>(val);
}

template<typename View>
auto _data_coverage(const View& view)
{
    if constexpr (View::category == view_category::Simple)
        return std::make_pair(view.data(), view.data() + view.size() - 1u);
    else
    {
        static_assert(View::category == view_category::Regular, "internal");
        return std::make_pair(view.data(),
            view.data() + (view.size() - 1u) * view.stride() +
            (view.stride() > 0 ? 1 : -1));
    }
}

template<typename Dst, typename Src>
auto has_aliasing(const Dst& dst, const Src& src)
{
    if constexpr (std::is_same_v<typename Dst::value_type,
        typename Src::value_type>)
    {
        if (dst.identifier() != src.identifier())
            return false;
        if constexpr (
            Dst::category == view_category::Array   ||
            Dst::category == view_category::General ||
            Src::category == view_category::Array   ||
            Src::category == view_category::General)
        {
            return true;
        }
        else
        {
            auto[src_min, src_max] = _data_coverage(src);
            auto[dst_min, dst_max] = _data_coverage(dst);
            return (src_max >= dst_min) && (src_min <= dst_max);
        }
    }
    else
        return false;
}

template<typename Dst, typename Src>
auto indirect_view_copy(Dst&& dst, const Src& src)
{
    using SrcValueType = typename Src::value_type;
    using DstValueType = typename remove_cvref_t<Dst>::value_type;
    using T = std::conditional_t<sizeof(SrcValueType) < sizeof(DstValueType),
        SrcValueType, DstValueType>;

    std::vector<T> buffer(src.size());
    src.copy_to(buffer.begin());
    dst.copy_from(buffer.begin());
}

}
