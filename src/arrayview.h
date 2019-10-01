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

#include "types.h"

namespace wl
{

template<bool UnitStep>
struct index_span_iter
{
    size_t index_;
    ptrdiff_t step_;

    index_span_iter(size_t index, ptrdiff_t step) :
        index_{index}, step_{step}
    {
    }

    auto& operator++()
    {
        index_ += step_;
        return *this;
    }

    bool operator==(const index_span_iter& other) const
    {
        return this->index_ == other.index_;
    }
};

template<>
struct index_span_iter<true>
{
    size_t index_;

    index_span_iter(size_t index) : 
        index_{index}
    {
    }

    auto& operator++()
    {
        ++index_;
        return *this;
    }

    bool operator==(const index_span_iter& other) const
    {
        return this->index_ == other.index_;
    }
};

template<bool UnitStep>
struct index_span;

template<>
struct index_span<false>
{
    size_t begin_;
    ptrdiff_t step_;
    size_t size_;

    index_span(size_t begin, ptrdiff_t step, size_t size) : 
        begin_{begin}, step_{step}, size_{size}
    {
    }

    auto begin() const
    {
        return index_span_iter<false>(begin_, step_);
    }

    auto end() const
    {
        return index_span_iter<false>(begin_ + step_ * size_, step_);
    }
};

template<>
struct index_span<true>
{
    size_t begin_;
    size_t size_;

    index_span(size_t begin, size_t size) :
        begin_{begin}, size_{size}
    {
    }

    auto begin() const
    {
        return index_span_iter<true>(begin_);
    }

    auto end() const
    {
        return index_span_iter<true>(begin_ + size_);
    }

};

}
