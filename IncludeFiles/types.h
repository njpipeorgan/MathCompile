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

#include <string>
#include <tuple>

#include "macros.h"
#include "traits.h"

namespace wl
{

struct void_type
{
};

struct all_type
{
};

struct none_type
{
};

struct any_type
{
};

struct string_type
{
};

struct integer_type
{
};

struct real_type
{
};

struct complex_type
{
    template<typename... Args>
    auto operator()(Args&&... args) const;
};

struct list_type
{
    template<typename... Args>
    auto operator()(Args&&... args) const;
};

struct varg_tag
{
};

template<int64_t>
struct level_tag
{
};

struct loop_break
{
};

struct dim_checked
{
};

struct complement_span_tag
{
};

struct no_check_abort_tag
{
};

struct boolean
{
    bool val_ = false;

    constexpr explicit boolean(bool val) : val_{val}
    {
    }

    constexpr boolean() = default;

    constexpr boolean operator&&(boolean other) const
    {
        return boolean(this->val_ && other.val_);
    }
    constexpr boolean operator||(boolean other) const
    {
        return boolean(this->val_ || other.val_);
    }
    constexpr boolean operator^ (boolean other) const
    {
        return boolean(this->val_ ^ other.val_);
    }
    constexpr boolean operator!() const
    {
        return boolean(!this->val_);
    }
    constexpr operator bool() const
    {
        return this->val_;
    }
};

template<int64_t I>
struct const_int
{
    static constexpr auto value = I;
};

template<int64_t... Is>
struct const_ints
{
    template<size_t N>
    static constexpr auto value = 
        std::tuple_element_t<N, 
        std::tuple<std::integral_constant<int64_t, Is>...>>::value;
};

}
