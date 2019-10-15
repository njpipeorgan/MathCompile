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
#include <numeric>
#include <vector>

#include "types.h"
#include "arrayview.h"
#include "utils.h"

namespace wl
{

template<typename T, size_t R>
struct ndarray
{
    static_assert(1u <= R && R <= 1024u, "badrank");
    static_assert(std::is_same_v<T, remove_cvref_t<T>>, "internal");
    static_assert(is_arithmetic_v<T> || is_boolean_v<T> || is_string_v<T>, 
        "badargtype");

    using value_type = T;
    static constexpr auto rank = R;
    using _dims_t = std::array<size_t, R>;
    static constexpr auto category = view_category::Array;

    _dims_t dims_;
    std::vector<T> data_;

    ndarray() : dims_{{0u}}, data_{}
    {
    }

    template<typename DimsT>
    ndarray(std::array<DimsT, rank> dims, const T& val = T{}) :
        dims_{}, data_(_input_dims_size(dims), val)
    {
        std::copy(dims.begin(), dims.end(), dims_.begin());
    }

    template<typename DimsT>
    ndarray(ndarray<DimsT, 1> dims, const T& val = T{}) :
        dims_{}, data_(_input_dims_size(dims), val)
    {
        std::copy(dims.begin(), dims.end(), dims_.begin());
    }

    template<typename FwdIter>
    ndarray(std::array<size_t, rank> dims, FwdIter begin, FwdIter end) :
        dims_{dims}, data_(begin, end)
    {
    }

    ndarray(std::array<size_t, rank> dims, std::vector<T>&& movable) :
        dims_{dims}, data_(std::move(movable))
    {
    }

    template<typename Dims>
    static size_t _input_dims_size(const Dims& dims)
    {
        static_assert(is_integral_v<typename Dims::value_type>, "badargtype");
        if (dims.size() != rank)
            throw std::logic_error("baddims");
        size_t size = 1u;
        for (const auto& d : dims)
        {
            if constexpr (std::is_signed_v<typename Dims::value_type>)
                if (d < 0)
                    throw std::logic_error("baddims");
            size *= d;
        }
        return size;
    }

    size_t size() const
    {
        return data_.size();
    }

    template<size_t Level>
    size_t partial_size() const
    {
        static_assert(Level <= R, "internal");
        if constexpr (Level == R)
            return 1u;
        else
            return this->dims_[Level] * this->partial_size<Level + 1u>();
    }

    // uses one-based indexing
    template<size_t Level>
    size_t dimension() const
    {
        static_assert(1 <= Level && Level <= R, "internal");
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

    auto data_vector() const & -> const std::vector<T>&
    {
        return this->data_;
    }

    auto data_vector() & -> std::vector<T>&
    {
        return this->data_;
    }

    auto data_vector() && -> std::vector<T>&&
    {
        return std::move(this->data_);
    }

    auto identifier() const
    {
        return this->data();
    }

    T& operator[](size_t i)
    {
        return data_[i];
    }

    const T& operator[](size_t i) const
    {
        return data_[i];
    }

    auto begin() { return data_.begin(); }
    auto end() { return data_.end(); }

    auto begin() const { return data_.begin(); }
    auto end() const { return data_.end(); }

    template<size_t Level>
    auto view_begin()
    {
        static_assert(Level <= R, "internal");
        if constexpr (Level == R)
            return this->begin();
        else
            return simple_view<T, R, R - Level, false>(
                this->identifier(), this->data(), this->dims_.data() + Level);
    }

    template<size_t Level>
    auto view_end()
    {
        if constexpr (Level == R)
            return this->end();
        else
        {
            auto iter = this->view_begin();
            iter.apply_pointer_offset(this->size());
            return iter;
        }
    }

    template<size_t Level>
    auto view_begin() const
    {
        static_assert(Level <= R, "internal");
        if constexpr (Level == R)
            return this->begin();
        else
            return simple_view<T, R, R - Level, true>(
                this->identifier(), this->data(), this->dims_.data() + Level);
    }

    template<size_t Level>
    auto view_end() const
    {
        if constexpr (Level == R)
            return this->end();
        else
        {
            auto iter = this->view_begin();
            iter.apply_pointer_offset(this->size());
            return iter;
        }
    }

    void resize(size_t new_dim0, ptrdiff_t size_diff)
    {
        assert(size_diff ==
            ptrdiff_t((new_dim0 - dims_[0]) * partial_size<1u>()));
        this->data_.resize(data_.size() + size_diff);
        this->dims_[0] = new_dim0;
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
#pragma omp simd
        for (size_t i = 0u; i < this->size(); ++i)
        {
            if constexpr (std::is_same_v<bool, decltype(f(*ptr, *iters...))>)
            {
                if (f(*ptr++, (*iters++)...))
                    break;
            }
            else
                f(*ptr++, (*iters++)...);
        }
    }

    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        auto ptr = this->data();
#pragma omp simd
        for (size_t i = 0u; i < this->size(); ++i)
        {
            if constexpr (std::is_same_v<bool, decltype(f(*ptr, *iters...))>)
            {
                if (f(*ptr++, (*iters++)...))
                    break;
            }
            else
                f(*ptr++, (*iters++)...);
        }
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        auto ptr = this->data();
#pragma omp simd
        for (size_t i = 0u; i < this->size(); ++i)
            *iter++ = *ptr++;
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter) &
    {
        auto ptr = this->data();
#pragma omp simd
        for (size_t i = 0u; i < this->size(); ++i)
            *ptr++ = *iter++;
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
