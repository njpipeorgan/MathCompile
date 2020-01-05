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
#include "ndarray.h"
#include "utils.h"
#include "listable.h"
#include "const.h"

namespace wl
{

template<typename X>
auto even_q(X&& x)
{
    WL_TRY_BEGIN()
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
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto odd_q(X&& x)
{
    WL_TRY_BEGIN()
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
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X, typename Y>
auto divisible(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
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
    WL_TRY_END(__func__, __FILE__, __LINE__)
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
    WL_TRY_BEGIN()
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
            else // x < 0
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
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto lucas_l(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if (x == XV(0))
                return XV(2);
            else if constexpr (std::is_unsigned_v<XV>)
                return XV(_lucas_l(x));
            else if (x > XV(0))
                return XV(_lucas_l(x));
            else // x < 0
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
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

inline uint64_t _factorial(uint64_t n)
{
    constexpr size_t data_size = 66;
    static const std::array<uint64_t, data_size> data ={
        1,1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,
        6227020800,87178291200,1307674368000,20922789888000,355687428096000,
        6402373705728000,121645100408832000,2432902008176640000u,
        14197454024290336768u,17196083355034583040u,8128291617894825984u,
        10611558092380307456u,7034535277573963776u,16877220553537093632u,
        12963097176472289280u,12478583540742619136u,11390785281054474240u,
        9682165104862298112u,4999213071378415616u,12400865694432886784u,
        3400198294675128320u,4926277576697053184u,6399018521010896896u,
        9003737871877668864u,1096907932701818880u,4789013295250014208u,
        2304077777655037952u,18376134811363311616u,15551764317513711616u,
        7538058755741581312u,10541877243825618944u,2673996885588443136u,
        9649395409222631424u,1150331055211806720u,17172071447535812608u,
        12602690238498734080u,8789267254022766592u,15188249005818642432u,
        18284192274659147776u,9994050523088551936u,13175843659825807360u,
        10519282829630636032u,6711489344688881664u,6908521828386340864u,
        6404118670120845312u,2504001392817995776u,162129586585337856u,
        9727775195120271360u,3098476543630901248u,7638104968020361216u,
        1585267068834414592u,9223372036854775808u,9223372036854775808u};

    if (n < data_size)
        return data[n];
    else
        return uint64_t(0);
}

inline uint64_t _factorial2(uint64_t n)
{
    constexpr size_t data_size = 66;
    static const std::array<uint64_t, data_size> data ={
        1,1,2,3,8,15,48,105,384,945,3840,10395,46080,135135,645120,2027025,
        10321920,34459425,185794560,654729075,3715891200,13749310575,
        81749606400,316234143225,1961990553600,7905853580625,51011754393600,
        213458046676875,1428329123020800,6190283353629375,42849873690624000,
        191898783962510625,1371195958099968000,6332659870762850625u,
        9727174427979808768u,282166592185152483u,18136886080501186560u,
        10440163910850641871u,6672140331791679488u,1338022901564897417u,
        8631196239733456896u,17965450816741690865u,12022104668323708928u,
        16197878097801090939u,12463771342375747584u,9481495526376579231u,
        1484415464288288768u,2908431970669985073u,15911710064709206016u,
        13385958046862407265u,2375508065949581312u,154329662729360723u,
        12845954987120918528u,8179472124656118319u,11152038577276190720u,
        7149109087057268761u,15771605895051476992u,1670848340654183825u,
        10862682301217636352u,6346331730049087595u,6124895493223874560u,
        18191354058803310975u,10808639105689190400u,2357173134616391233u,
        9223372036854775808u,5642301160389017217u};

    if (n < data_size)
        return data[n];
    else if (n % 2u == 0u)
        return uint64_t(0);
    else
    {
        uint64_t res = 1u;
        for (;;)
        {
            res *= n;
            n -= 2u;
            if (n < data_size)
                return uint64_t(res * data[n]);
        }
    }
}

template<typename X>
auto factorial(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_factorial(x));
            else if (x >= XV(0))
                return XV(_factorial(x));
            else // x < 0
                throw std::logic_error(WL_ERROR_FACTORIAL_NEGATIVE);
        }
        else
        {
            return XV(wl::gamma(XV(1) + x));
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto factorial2(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_factorial2(x));
            else if (x >= XV(0))
                return XV(_factorial2(x));
            else // x < 0
            {
                if (x == XV(-1))
                    return XV(1);
                else if (x == XV(-3))
                    return XV(-1);
                else
                    throw std::logic_error(WL_ERROR_FACTORIAL2_DOMAIN);
            }
        }
        else
        {
            // x!!:=exp((log(2)/2)*x+(log(pi/2)/4)*(cos(pi*x)-1))*gamma(1+x/2)
            const auto log_2_2 = XV(0.34657359027997265471);
            const auto log_pi_2_4 = XV(0.11289567632236371618);
            XV t1 = log_2_2 * x;
            XV t2 = log_pi_2_4 * (std::cos(XV(const_pi) * x) - XV(1));
            XV t3 = wl::gamma(XV(1) + XV(0.5) * x);
            return XV(std::exp(t1 + t2) * t3);
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}
