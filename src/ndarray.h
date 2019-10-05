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
#include <array>
#include <complex>
#include <numeric>
#include <vector>

#include "types.h"
#include "arrayview.h"

namespace wl
{

template<typename T, size_t R>
struct ndarray
{
    static_assert(R > 0, "");

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

    template<typename Dims>
    static size_t _input_dims_size(const Dims& dims)
    {
        static_assert(is_integral_v<typename Dims::value_type>, "badargtype");
        if (dims.size() != rank)
        {
            throw std::logic_error("baddims");
        }
        size_t size = 1u;
        for (const auto& d : dims)
        {
            if constexpr (std::is_signed_v<typename Dims::value_type>)
            {
                if (d < 0)
                {
                    throw std::logic_error("baddims");
                }
            }
            size *= d;
        }
        return size;
    }

    size_t size() const
    {
        return data_.size();
    }

    template<size_t Level>
    size_t dimension() const
    {
        return this->dims_[Level];
    }

    auto dims() const
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

    auto cbegin() const { return data_.cbegin(); }
    auto cend() const { return data_.cend(); }

    template<size_t Level, typename... Is>
    void linear_pos_impl(size_t& pos, 
        const size_t& i1, const Is&... is) const
    {
        pos += i1;
        if constexpr (Level < R - 1)
            pos *= this->dims_[Level + 1u];
        if constexpr (Level < R - 1)
            linear_pos_impl<Level + 1u>(pos, is...);
    }

    template<typename... Is>
    size_t linear_pos(const Is&... is) const
    {
        size_t pos = 0u;
        linear_pos_impl<0u>(pos, is...);
        return pos;
    }
    
    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters)
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i)
        {
            f(*ptr++, (*iters++)...);
        }
    }

    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i)
        {
            f(*ptr++, (*iters++)...);
        }
    }

    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i)
            *iter++ = *ptr++;
    }

    template<typename FwdIter>
    void copy_from(FwdIter iter) &
    {
        auto ptr = this->data();
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
