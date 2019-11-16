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

#include <cmath>

#include "types.h"
#include "traits.h"
#include "ndarray.h"
#include "utils.h"
#include "const.h"

namespace wl
{

template<typename X>
auto even_q(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
            return boolean((x & XV(1)) == XV(0));
        else if constexpr (is_float_v<XV>)
            return boolean(std::fmod(x, XV(2)) == XV(0));
        else
            return const_false;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X>
auto odd_q(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
            return boolean((x & XV(1)) == XV(1));
        else if constexpr (is_float_v<XV>)
            return boolean(std::fmod(x, XV(2)) == std::copysign(XV(1), x));
        else
            return const_false;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X, typename Y>
auto divisible(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, WL_ERROR_REAL_TYPE_ARG);
        if constexpr (is_integral_v<XV> && is_integral_v<YV>)
        {
            using C = std::conditional_t<
                std::is_signed_v<XV> || std::is_signed_v<YV>,
                make_signed_t<common_type_t<XV, YV>>,
                common_type_t<XV, YV>>;
            return boolean(y != YV(0) && C(x) % C(y) == C(0));
        }
        else
        {
            using C = common_type_t<XV, YV>;
            return boolean(std::fmod(C(x), C(y)) == C(0));
        }
    };
    return utils::listable_function(pure,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}

inline uint64_t _fibonacci_impl(uint64_t n, uint64_t* prev)
{
    constexpr size_t data_size = 94;
    static const std::array<uint64_t, data_size> data ={
        0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,
        10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,
        1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,
        63245986,102334155,165580141,267914296,433494437,701408733,1134903170,
        1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,
        32951280099,53316291173,86267571272,139583862445,225851433717,
        365435296162,591286729879,956722026041,1548008755920,2504730781961,
        4052739537881,6557470319842,10610209857723,17167680177565,
        27777890035288,44945570212853,72723460248141,117669030460994,
        190392490709135,308061521170129,498454011879264,806515533049393,
        1304969544928657,2111485077978050,3416454622906707,5527939700884757,
        8944394323791464,14472334024676221,23416728348467685,37889062373143906,
        61305790721611591,99194853094755497,160500643816367088,
        259695496911122585,420196140727489673,679891637638612258,
        1100087778366101931,1779979416004714189,2880067194370816120,
        4660046610375530309,7540113804746346429,12200160415121876738u};

    if (n < data_size)
    {
        *prev = n == 0u ? uint64_t(1) : data[n - 1];
        return data[n];
    }
    auto lzcnt = utils::_lzcnt(n);
    uint64_t leading = n >> (58 - lzcnt);
    uint64_t mask = uint64_t(1) << (57 - lzcnt);
    uint64_t a = data[leading - 1];
    uint64_t b = data[leading];
    uint64_t c = 0u;
    for (; mask; mask >>= 1)
    {
        c = a * a + b * b;
        b *= (2 * a + b);
        a = c;
        if (n & mask)
        {
            c = b;
            b += a;
            a = c;
        }
    }
    *prev = a;
    return b;
}

inline uint64_t _fibonacci(uint64_t n)
{
    uint64_t n1;
    return _fibonacci_impl(n, &n1);
}

inline uint64_t _lucas_l(uint64_t n)
{
    uint64_t n1;
    uint64_t n2 = _fibonacci_impl(n - 1u, &n1);
    return n2 * 3u + n1;
}

template<typename X>
auto fibonacci(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_fibonacci(x));
            else if (x >= XV(0))
                return XV(_fibonacci(x));
            else
            {
                auto val = XV(_fibonacci(-x));
                return (x & XV(1)) ? val : -val;
            }
        }
        else
        {
            XV phi_x = std::pow(XV(1.6180339887498948482), x);
            XV cos_pi_x = std::cos(XV(const_pi) * x);
            return XV(0.44721359549995793928) * (phi_x - cos_pi_x / phi_x);
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

template<typename X>
auto lucas_l(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_lucas_l(x));
            else if (x >= XV(0))
                return XV(_lucas_l(x));
            else
            {
                auto val = XV(_lucas_l(-x));
                return (x & XV(1)) ? -val : val;
            }
        }
        else
        {
            XV phi_x = std::pow(XV(1.6180339887498948482), x);
            XV cos_pi_x = std::cos(XV(const_pi) * x);
            return phi_x + cos_pi_x / phi_x;
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}

}
