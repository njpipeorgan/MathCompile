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

#include <cassert>

#include <algorithm>
#include <array>
#include <complex>
#include <initializer_list>
#include <numeric>
#include <variant>
#include <vector>

#include "types.h"
#include "arrayview.h"
#include "utils.h"

namespace wl
{

#ifndef WL_SMALL_ARRAY_SIZE
#define WL_SMALL_ARRAY_SIZE 256
#endif

template<typename T, size_t N>
struct _small_vector
{
    using static_t = std::array<T, N>;
    using dynamic_t = std::vector<T>;
    union data_t
    {
        static_t static_;
        dynamic_t dynamic_;
        data_t() {}
        ~data_t() {}
        data_t(const data_t&) = delete;
        data_t(data_t&&) = delete;
        data_t& operator=(const data_t&) = delete;
        data_t& operator=(data_t&&) = delete;
    };

    bool is_static_ = true;
    size_t size_ = 0u;
    data_t data_;

    _small_vector()
    {
    }

    ~_small_vector()
    {
        this->destroy();
    }

    explicit _small_vector(size_t size) : size_{size}
    {
        bool is_static = (size_ <= N);
        if (is_static)
            std::uninitialized_default_construct_n(static_begin(), size);
        else
            new(&data_.dynamic_) dynamic_t(size);
        this->is_static_ = is_static;
    }

    _small_vector(size_t size, const T& val) : size_{size}
    {
        bool is_static = (size_ <= N);
        if (is_static)
            std::uninitialized_fill_n(static_begin(), size, val);
        else
            new(&data_.dynamic_) dynamic_t(size, val);
        this->is_static_ = is_static;
    }

    template<typename FwdIter>
    _small_vector(FwdIter begin, FwdIter end) : size_{size_t(end - begin)}
    {
        bool is_static = (size_ <= N);
        if (is_static)
            std::uninitialized_copy_n(begin, size_, static_begin());
        else
            new(&data_.dynamic_) dynamic_t(begin, end);
        this->is_static_ = is_static;
    }

    explicit _small_vector(const dynamic_t& other) : size_{other.size()}
    {
        bool is_static = (size_ <= N);
        if (is_static)
            std::uninitialized_copy_n(other.begin(), size_, static_begin());
        else
            new(&data_.dynamic_) dynamic_t(other);
        this->is_static_ = is_static;
    }

    explicit _small_vector(dynamic_t&& other) : size_{other.size()}
    {
        new(&data_.dynamic_) dynamic_t(std::move(other));
        this->is_static_ = false;
    }

    _small_vector(const _small_vector& other) : size_{other.size_}
    {
        if (other.size_ <= N)
        {
            std::uninitialized_copy_n(other.begin(), size_, static_begin());
            this->is_static_ = true;
        }
        else
        {
            new(&data_.dynamic_) dynamic_t(other.data_.dynamic_);
            this->is_static_ = false;
        }
    }

    _small_vector(_small_vector&& other) : size_{other.size_}
    {
        if (other.is_static_)
            std::uninitialized_move_n(
                other.static_begin(), size_, static_begin());
        else
            new(&data_.dynamic_) dynamic_t(std::move(other.data_.dynamic_));
        this->is_static_ = other.is_static_;
    }

    _small_vector& operator=(const _small_vector& other)
    {
        this->destroy();
        if (other.size_ <= N)
        {
            std::uninitialized_copy_n(other.begin(), size_, static_begin());
            this->is_static_ = true;
        }
        else
        {
            new(&data_.dynamic_) dynamic_t(other.data_.dynamic_);
            this->is_static_ = false;
        }
        this->size_ = other.size_;
        return *this;
    }

    _small_vector& operator=(_small_vector&& other)
    {
        this->destroy();
        if (other.is_static_)
            std::uninitialized_move_n(
                other.static_begin(), other.size_, static_begin());
        else
            new(&data_.dynamic_) dynamic_t(std::move(other.data_.dynamic_));
        this->is_static_ = other.is_static_;
        this->size_ = other.size_;
        return *this;
    }

    WL_INLINE void destroy_static()
    {
        std::destroy(static_begin(), static_end());
    }
    WL_INLINE void destroy_dynamic()
    {
        data_.dynamic_.~dynamic_t();
    }

    WL_INLINE void destroy()
    {
        if (is_static_)
        {
            destroy_static();
        }
        else
        {
            destroy_dynamic();
        }
    }

    size_t size() const
    {
        return size_;
    }

    bool is_static() const
    {
        return is_static_;
    }

    const T* data() const
    {
        return is_static_ ? static_begin() : dynamic_begin();
    }
    T* data()
    {
        return is_static_ ? static_begin() : dynamic_begin();
    }

    T* static_begin() { return data_.static_.data(); }
    T* static_end() { return static_begin() + size_; }
    const T* static_begin() const { return data_.static_.data(); }
    const T* static_end() const { return static_begin() + size_; }

    T* dynamic_begin() { return data_.dynamic_.data(); }
    T* dynamic_end() { return dynamic_begin() + size_; }
    const T* dynamic_begin() const { return data_.dynamic_.data(); }
    const T* dynamic_end() const { return dynamic_begin() + size_; }

    T* begin() { return data(); }
    T* end() { return data() + size_; }

    const T* begin() const { return data(); }
    const T* end() const { return data() + size_; }

    void resize(size_t new_size)
    {
        const auto prev_size = size_;
        if (!is_static_)
        {
            data_.dynamic_.resize(new_size);
            if (new_size < prev_size)
                data_.dynamic_.shrink_to_fit();
        }
        else if (new_size > N)
        {
            dynamic_t new_data(new_size);
            std::move(static_begin(), static_end(), new_data.data());
            std::destroy(static_begin(), static_end());
            new(&data_.dynamic_) dynamic_t(std::move(new_data));
            is_static_ = false;
        }
        else
        {
            const auto diff_size = ptrdiff_t(new_size - prev_size);
            if (diff_size > 0)
                std::uninitialized_default_construct_n(
                    static_begin() + prev_size, size_t(diff_size));
            else if (diff_size < 0)
                std::destroy_n(static_begin() + new_size, size_t(-diff_size));
        }
        size_ = new_size;
    }

    const void* identifier() const
    {
        return reinterpret_cast<const void*>(this);
    }
};

template<typename T, size_t R>
struct ndarray
{
    static_assert(1u <= R, WL_ERROR_ZERO_RANK);
    static_assert(R <= MaximumArrayRank, WL_ERROR_LARGE_RANK);
    static_assert(std::is_same_v<T, remove_cvref_t<T>>, WL_ERROR_INTERNAL);
    static_assert(is_arithmetic_v<T> || is_boolean_v<T> || is_string_v<T>,
        WL_ERROR_ARRAY_VALUE_TYPE);

    using value_type = T;
    static constexpr auto rank = R;
    using _dims_t = std::array<size_t, R>;
    static constexpr auto category = view_category::Array;

    static constexpr auto small_size = size_t(WL_SMALL_ARRAY_SIZE) / sizeof(T);
    using _data_t = _small_vector<T, small_size>;

    _dims_t dims_;
    _data_t data_;

    ndarray() : dims_{0u}, data_{}
    {
    }

    template<typename DimsT>
    ndarray(const std::array<DimsT, R>& dims, const T& val) :
        data_(utils::size_of_dims(dims), val)
    {
        std::copy(dims.begin(), dims.end(), this->dims_.data());
    }

    template<typename DimsT>
    ndarray(const std::array<DimsT, R>& dims) :
        data_(utils::size_of_dims(dims))
    {
        std::copy(dims.begin(), dims.end(), this->dims_.data());
    }

    template<typename FwdIter>
    ndarray(std::array<size_t, R> dims, FwdIter begin, FwdIter end) :
        dims_{dims}, data_(begin, end)
    {
    }

    ndarray(std::array<size_t, R> dims, std::initializer_list<T> data) :
        dims_{dims}, data_(data.begin(), data.end())
    {
    }

    ndarray(std::array<size_t, R> dims, _data_t&& movable) :
        dims_{dims}, data_(std::move(movable))
    {
    }

    ndarray(std::array<size_t, R> dims, std::vector<T>&& movable) :
        dims_{dims}, data_(std::move(movable))
    {
    }

    size_t size() const
    {
        return data_.size();
    }

    template<size_t Level>
    size_t partial_size() const
    {
        static_assert(Level <= R, WL_ERROR_INTERNAL);
        return utils::size_of_dims<R - Level>(this->dims_.data());
    }

    // uses one-based indexing
    template<size_t Level>
    size_t dimension() const
    {
        static_assert(1 <= Level && Level <= R, WL_ERROR_INTERNAL);
        return this->dims_[Level - 1];
    }

    const auto& dims() const
    {
        return this->dims_;
    }

    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }

    const T* data() const
    {
        return this->data_.data();
    }

    T* data()
    {
        return this->data_.data();
    }

    auto is_static() const
    {
        return this->data_.is_static();
    }

    auto data_vector() const & -> const auto&
    {
        return this->data_;
    }

    auto data_vector() & -> auto&
    {
        return this->data_;
    }

    auto data_vector() && -> auto&&
    {
        return std::move(this->data_);
    }

    const void* identifier() const
    {
        return this->data_.identifier();
    }

    auto begin() { return this->data_.begin(); }
    auto end() { return this->data_.end(); }

    auto begin() const { return this->data_.begin(); }
    auto end() const { return this->data_.end(); }

    template<size_t Level>
    auto view_begin()
    {
        static_assert(Level <= R, WL_ERROR_INTERNAL);
        if constexpr (Level == R)
            return this->begin();
        else
            return simple_view<T, R, R - Level, false>(
                this->identifier(), this->data(), this->dims_ptr() + Level);
    }

    template<size_t Level>
    auto view_end()
    {
        if constexpr (Level == R)
            return this->end();
        else
        {
            auto iter = this->template view_begin<Level>();
            iter.apply_pointer_offset(this->size());
            return iter;
        }
    }

    template<size_t Level>
    auto view_begin() const
    {
        static_assert(Level <= R, WL_ERROR_INTERNAL);
        if constexpr (Level == R)
            return this->begin();
        else
            return simple_view<T, R, R - Level, true>(
                this->identifier(), this->data(), this->dims_ptr() + Level);
    }

    template<size_t Level>
    auto view_end() const
    {
        if constexpr (Level == R)
            return this->end();
        else
        {
            auto iter = this->template view_begin<Level>();
            iter.apply_pointer_offset(this->size());
            return iter;
        }
    }

    void uninitialized_resize(const _dims_t& new_dims, size_t new_size)
    {
        assert(utils::size_of_dims(new_dims) == new_size);
        this->dims_ = new_dims;
        this->data_.resize(new_size);
    }

    void uninitialized_resize(const _dims_t& new_dims)
    {
        this->dims_ = new_dims;
        this->data_.resize(utils::size_of_dims(this->dims_));
    }

    template<typename X>
    void _append_scalar(X&& x)
    {
        size_t prev_size = this->size();
        this->data_.resize(prev_size + 1u);
        *(this->data_.data() + prev_size) = std::forward<decltype(x)>(x);
        ++this->dims_[0];
    }

    template<typename X>
    void _append_array(X&& x)
    {
        if (this->size() == 0u)
        {
            this->dims_ = utils::dims_join(
                std::array<size_t, 1u>{1u}, x.dims());
            this->data_ = cast<ndarray<T, R - 1u>>(
                std::forward<decltype(x)>(x)).data_vector();
        }
        else
        {
            size_t prev_size = this->size();
            this->data_.resize(prev_size + x.size());
            auto dst_ptr = this->data_.data() + prev_size;
            x.copy_to(dst_ptr);
            ++this->dims_[0];
        }
    }

    template<typename X>
    void append(X&& x, dim_checked)
    {
        if constexpr (R > 1u)
            _append_array(std::forward<decltype(x)>(x));
        else
            _append_scalar(std::forward<decltype(x)>(x));
    }

    template<typename X>
    void append(X&& x)
    {
        static_assert(array_rank_v<remove_cvref_t<X>> + 1u == R,
            WL_ERROR_APPEND_RANK);
        if constexpr (R > 1u)
        {
            if (this->size() > 0u && !utils::check_dims<R - 1u>(
                this->dims_ptr() + 1, x.dims().data()))
                throw std::logic_error(WL_ERROR_INTERNAL);
        }
        this->append(std::forward<decltype(x)>(x), dim_checked{});
    }

    template<typename... Is>
    size_t linear_position(const Is&... is) const
    {
        return utils::linear_position(this->dims_, is...);
    }

    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters)
    {
        auto ptr = this->data();
        using RV = remove_cvref_t<decltype(f(*ptr, *iters...))>;
        WL_CHECK_ABORT_LOOP_BEGIN(this->size())
            WL_IGNORE_DEPENDENCIES
            for (auto i = _loop_zero; i < _loop_size;
                ++i, ++ptr, (++iters, ...))
            {
                if constexpr (!std::is_same_v<bool, RV>)
                    f(*ptr, *iters...);
                else if (f(*ptr, *iters...))
                    break;
            }
        WL_CHECK_ABORT_LOOP_END()
    }

    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        auto ptr = this->data();
        using RV = remove_cvref_t<decltype(f(*ptr, *iters...))>;
        WL_CHECK_ABORT_LOOP_BEGIN(this->size())
            WL_IGNORE_DEPENDENCIES
            for (auto i = _loop_zero; i < _loop_size;
                ++i, ++ptr, (++iters, ...))
            {
                if constexpr (!std::is_same_v<bool, RV>)
                    f(*ptr, *iters...);
                else if (f(*ptr, *iters...))
                    break;
            }
        WL_CHECK_ABORT_LOOP_END()
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        utils::restrict_copy_n(this->data(), this->size(), iter);
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter) &
    {
        utils::restrict_copy_n(iter, this->size(), this->data());
    }

    template<typename FwdIter>
    void copy_from(FwdIter) && = delete;

    auto to_array() const & -> decltype(auto)
    {
        return *this;
    }

    auto to_array() && -> decltype(auto)
    {
        return std::move(*this);
    }
};

}
