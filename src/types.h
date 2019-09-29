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

#include <cstddef>
#include <cstdint>

#include <algorithm>
#include <array>
#include <complex>
#include <numeric>
#include <vector>

#include "traits.h"

namespace wl
{

struct void_type
{
};

template<typename T, size_t R>
struct ndarray
{
    static_assert(R > 0, "");

    using value_type = T;
    using _dims_t = std::array<size_t, R>;
    static constexpr auto Rank = R;

    _dims_t dims_;
    std::vector<T> data_;

    ndarray() :
        dims_{{0}}, data_{}
    {
    }

    ndarray(_dims_t dims) :
        dims_{dims}, data_{}
    {
        data_.resize(this->size());
    }

    size_t size() const
    {
        size_t s = 1;
        for (size_t d : dims_)
            s *= d;
        return s;
    }

    const _dims_t& dims() const
    {
        return this->dims_;
    }

    T& operator[](size_t i)
    {
        return data_[i];
    }

    T operator[](size_t i) const
    {
        return data_[i];
    }

    auto begin() { return data_.begin(); }
    auto end() { return data_.end(); }
    auto begin() const { return data_.begin(); }
    auto end() const { return data_.end(); }
    auto cbegin() const { return data_.cbegin(); }
    auto cend() const { return data_.cend(); }

    template<typename Integral>
    auto part(Integral i) const
    {
        static_assert(is_integral_v<Integral>, "");
        std::array<size_t, R - 1> new_dims;
        std::copy(begin(dims_) + 1, end(dims_), begin(new_dims));
        ndarray<T, R - 1> new_array(new_dims);
        std::copy_n(begin(*this), new_array.size(), begin(new_array));
        return new_array;
    }

};

template<typename T, size_t R>
auto begin(const ndarray<T, R>& x)
{
    return x.begin();
}

template<typename T, size_t R>
auto end(const ndarray<T, R>& x)
{
    return x.end();
}

template<typename T, size_t R>
auto begin(ndarray<T, R>& x)
{
    return x.begin();
}

template<typename T, size_t R>
auto end(ndarray<T, R>& x)
{
    return x.end();
}

template<typename T, size_t R>
auto cbegin(const ndarray<T, R>& x)
{
    return x.cbegin();
}

template<typename T, size_t R>
auto cend(const ndarray<T, R>& x)
{
    return x.cend();
}

}
