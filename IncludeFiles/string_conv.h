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
#include <cstdlib>
#include <cstring>

#include "macros.h"
#include "utils.h"
#include "u8string.h"

namespace wl
{

namespace _to_string_impl
{

struct float_pair
{
    static constexpr int      mantissa_size = 64;
    static constexpr int      fraction_size = 52;
    static constexpr int      exponent_bias = 0x3ff + fraction_size;
    static constexpr int      min_exponent  = -exponent_bias;
    static constexpr uint64_t exponent_mask = 0x7ff0'0000'0000'0000u;
    static constexpr uint64_t fraction_mask = 0x000f'ffff'ffff'ffffu;
    static constexpr uint64_t hidden_bit    = 0x0010'0000'0000'0000u;

    uint64_t mantissa = 0;
    int      exponent = 0;

    constexpr float_pair() = default;

    constexpr float_pair(uint64_t mantissa, int exponent) :
        mantissa{mantissa}, exponent{exponent}
    {
    }

    float_pair(double d)
    {
        uint64_t ud;
        std::memcpy(&ud, &d, sizeof(double));
        auto biased_e = int(exponent_bits(ud));
        auto fraction = fraction_bits(ud);
        if (biased_e > 0)
        {
            mantissa = fraction + hidden_bit;
            exponent = biased_e - exponent_bias;
        }
        else
        {
            mantissa = fraction;
            exponent = min_exponent + 1;
        }
    }

    float_pair operator-(const float_pair& other) const
    {
        assert(this->exponent == other.exponent);
        assert(this->mantissa >= other.mantissa);
        return {this->mantissa - other.mantissa, this->exponent};
    }

    float_pair operator*(const float_pair& other) const
    {
#if defined (__BMI2__)
        uint64_t hi, lo;
        lo = _mulx_u64(mantissa, other.mantissa, (unsigned long long*)(&hi));
        return {hi + (lo >> 63u), exponent + other.exponent + 64};
#else
        static constexpr uint64_t mask_32 = 0xffff'ffffu;
        uint64_t ll = (mantissa & mask_32) * (other.mantissa & mask_32);
        uint64_t hl = (mantissa >> 32u)    * (other.mantissa & mask_32);
        uint64_t lh = (mantissa & mask_32) * (other.mantissa >> 32u);
        uint64_t hh = (mantissa >> 32u)    * (other.mantissa >> 32u);
        uint64_t carry = ((ll >> 32u) + (hl & mask_32) + (lh & mask_32) +
            (uint64_t(1) << 31u)) >> 32u;
        return {hh + (hl >> 32u) + (lh >> 32u) + (carry >> 32u),
            exponent + other.exponent + 64};
#endif
    }

    float_pair& normalize(ptrdiff_t offset = 0)
    {
        size_t leading_zero = (mantissa & (hidden_bit << offset)) ?
            (mantissa_size - fraction_size - 1 - offset) :
            wl::utils::lzcnt_u64(mantissa);
        mantissa <<= leading_zero;
        exponent -= int(leading_zero);
        return *this;
    }

    void normalize_boundaries(float_pair& minus, float_pair& plus) const
    {
        plus = float_pair((mantissa << 1u) + 1u, exponent - 1);
        plus.normalize(1);
        if (mantissa == hidden_bit)
            minus = float_pair((mantissa << 2u) - 1u, exponent - 2);
        else
            minus = float_pair((mantissa << 1u) - 1u, exponent - 1);
        minus.mantissa <<= minus.exponent - plus.exponent;
        minus.exponent = plus.exponent;
    }

    static uint64_t exponent_bits(uint64_t ud)
    {
        return (ud & exponent_mask) >> fraction_size;
    }
    static uint64_t fraction_bits(uint64_t ud)
    {
        return ud & fraction_mask;
    }
};

inline float_pair cached_power(int exponent_2, int& exponent_10)
{
    // 10^-348, 10^-340, ..., 10^340
    // Table[{"0x"<>IntegerString[Round[10^p*2^#],16]<>"u",#}&@NestWhile[
    //   #-1&,Round[68-N@Log2[10^p]],10^p*2^#>=2^64&],{p,-348,340,8}]
    static constexpr float_pair cached_powers[] =
    {
        {0xfa8fd5a0081c0288u,-1220},{0xbaaee17fa23ebf76u,-1193},
        {0x8b16fb203055ac76u,-1166},{0xcf42894a5dce35eau,-1140},
        {0x9a6bb0aa55653b2du,-1113},{0xe61acf033d1a45dfu,-1087},
        {0xab70fe17c79ac6cau,-1060},{0xff77b1fcbebcdc4fu,-1034},
        {0xbe5691ef416bd60cu,-1007},{0x8dd01fad907ffc3cu,-980},
        {0xd3515c2831559a83u,-954},{0x9d71ac8fada6c9b5u,-927},
        {0xea9c227723ee8bcbu,-901},{0xaecc49914078536du,-874},
        {0x823c12795db6ce57u,-847},{0xc21094364dfb5637u,-821},
        {0x9096ea6f3848984fu,-794},{0xd77485cb25823ac7u,-768},
        {0xa086cfcd97bf97f4u,-741},{0xef340a98172aace5u,-715},
        {0xb23867fb2a35b28eu,-688},{0x84c8d4dfd2c63f3bu,-661},
        {0xc5dd44271ad3cdbau,-635},{0x936b9fcebb25c996u,-608},
        {0xdbac6c247d62a584u,-582},{0xa3ab66580d5fdaf6u,-555},
        {0xf3e2f893dec3f126u,-529},{0xb5b5ada8aaff80b8u,-502},
        {0x87625f056c7c4a8bu,-475},{0xc9bcff6034c13053u,-449},
        {0x964e858c91ba2655u,-422},{0xdff9772470297ebdu,-396},
        {0xa6dfbd9fb8e5b88fu,-369},{0xf8a95fcf88747d94u,-343},
        {0xb94470938fa89bcfu,-316},{0x8a08f0f8bf0f156bu,-289},
        {0xcdb02555653131b6u,-263},{0x993fe2c6d07b7facu,-236},
        {0xe45c10c42a2b3b06u,-210},{0xaa242499697392d3u,-183},
        {0xfd87b5f28300ca0eu,-157},{0xbce5086492111aebu,-130},
        {0x8cbccc096f5088ccu,-103},{0xd1b71758e219652cu,-77},
        {0x9c40000000000000u,-50},{0xe8d4a51000000000u,-24},
        {0xad78ebc5ac620000u,3},{0x813f3978f8940984u,30},
        {0xc097ce7bc90715b3u,56},{0x8f7e32ce7bea5c70u,83},
        {0xd5d238a4abe98068u,109},{0x9f4f2726179a2245u,136},
        {0xed63a231d4c4fb27u,162},{0xb0de65388cc8ada8u,189},
        {0x83c7088e1aab65dbu,216},{0xc45d1df942711d9au,242},
        {0x924d692ca61be758u,269},{0xda01ee641a708deau,295},
        {0xa26da3999aef774au,322},{0xf209787bb47d6b85u,348},
        {0xb454e4a179dd1877u,375},{0x865b86925b9bc5c2u,402},
        {0xc83553c5c8965d3du,428},{0x952ab45cfa97a0b3u,455},
        {0xde469fbd99a05fe3u,481},{0xa59bc234db398c25u,508},
        {0xf6c69a72a3989f5cu,534},{0xb7dcbf5354e9beceu,561},
        {0x88fcf317f22241e2u,588},{0xcc20ce9bd35c78a5u,614},
        {0x98165af37b2153dfu,641},{0xe2a0b5dc971f303au,667},
        {0xa8d9d1535ce3b396u,694},{0xfb9b7cd9a4a7443cu,720},
        {0xbb764c4ca7a44410u,747},{0x8bab8eefb6409c1au,774},
        {0xd01fef10a657842cu,800},{0x9b10a4e5e9913129u,827},
        {0xe7109bfba19c0c9du,853},{0xac2820d9623bf429u,880},
        {0x80444b5e7aa7cf85u,907},{0xbf21e44003acdd2du,933},
        {0x8e679c2f5e44ff8fu,960},{0xd433179d9c8cb841u,986},
        {0x9e19db92b4e31ba9u,1013},{0xeb96bf6ebadf77d9u,1039},
        {0xaf87023b9bf0ee6bu,1066}
    };
    static_assert(sizeof(cached_powers) / sizeof(cached_powers[0]) == 87u);
    constexpr double log10_2 = 0.301029995663981195;

    const auto k = int(std::ceil(log10_2 * (-61 - exponent_2))) + 347;
    const auto index = k / 8 + 1;
    exponent_10 = -(-348 + 8 * index);
    assert(index < sizeof(cached_powers) / sizeof(cached_powers[0]));
    return cached_powers[index];
}

inline void grisu_round(char* buffer, int length, uint64_t delta,
    uint64_t rest, uint64_t ten_kappa, uint64_t wp_w)
{
    while ((rest < wp_w) && (delta - rest >= ten_kappa) &&
        ((rest + ten_kappa < wp_w) || (wp_w - rest > rest + ten_kappa - wp_w)))
    {
        buffer[length - 1]--;
        rest += ten_kappa;
    }
}

inline int count_decimal_digits(uint32_t n) {
    if (n < 10)
        return 1;
    else if (n < 100)
        return 2;
    else if (n < 1000)
        return 3;
    else if (n < 10000)
        return 4;
    else if (n < 100000)
        return 5;
    else if (n < 1000000)
        return 6;
    else if (n < 10000000)
        return 7;
    else if (n < 100000000)
        return 8;
    else if (n < 1000000000)
        return 9;
    else
        return 10;
}

inline void generate_digits(const float_pair& val, const float_pair& upper,
    uint64_t mantissa_window, char* buffer, int& length, int& exponent_10)
{
    static constexpr uint64_t power_10[] =
    {
        1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000,
        1000000000, 10000000000, 100000000000, 1000000000000, 10000000000000,
        100000000000000
    };

    const auto upper_exponent = upper.exponent;
    const auto mantissa_mask = (uint64_t(1) << -upper_exponent) - 1u;
    const auto upper_window = upper - val;
    auto integer_part = upper.mantissa >> -upper_exponent;
    auto fraction_part = upper.mantissa & mantissa_mask;
    auto exponent_10_diff = count_decimal_digits(uint32_t(integer_part));

    while (exponent_10_diff > 0)
    {
        uint64_t d = 0;
        switch (exponent_10_diff)
        {
#define WL_GENERATE_DIGITS_INTEGER_PART_CASE(i)     \
        case i:                                     \
            d = integer_part / power_10[i - 1];     \
            integer_part %= power_10[i - 1];        \
            break;
            WL_GENERATE_DIGITS_INTEGER_PART_CASE(10)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(9)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(8)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(7)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(6)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(5)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(4)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(3)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(2)
                WL_GENERATE_DIGITS_INTEGER_PART_CASE(1)
#undef WL_GENERATE_DIGITS_INTEGER_PART_CASE
        default: d = 0;
        }
        if (d || length)
            buffer[length++] = char('0' + int(d));
        --exponent_10_diff;
        uint64_t new_mantissa = (integer_part << -upper_exponent) + fraction_part;
        if (new_mantissa <= mantissa_window)
        {
            exponent_10 += exponent_10_diff;
            grisu_round(buffer, length, mantissa_window, new_mantissa,
                power_10[exponent_10_diff] << -upper_exponent,
                upper_window.mantissa);
            return;
        }
    }
    for (;;)
    {
        fraction_part *= 10;
        mantissa_window *= 10;
        auto d = char(fraction_part >> -upper_exponent);
        if (d || length)
            buffer[length++] = '0' + d;
        fraction_part &= mantissa_mask;
        --exponent_10_diff;
        if (fraction_part < mantissa_window)
        {
            exponent_10 += exponent_10_diff;
            grisu_round(buffer, length, mantissa_window, fraction_part,
                (uint64_t(1) << -upper_exponent),
                upper_window.mantissa * power_10[-exponent_10_diff]);
            return;
        }
    }
}

inline void grisu2(double value, char* buffer, int& length, int& exponent_10)
{
    float_pair v(value);
    float_pair w_m, w_p;
    v.normalize_boundaries(w_m, w_p);

    const auto c_mk = cached_power(w_p.exponent, exponent_10);
    v.normalize();
    const auto W = v * c_mk;
    auto Wp = w_p * c_mk;
    auto Wm = w_m * c_mk;
    ++Wm.mantissa;
    --Wp.mantissa;
    generate_digits(W, Wp, Wp.mantissa - Wm.mantissa, buffer, length,
        exponent_10);
}

inline char* write_exponent(int exponent_10, char* buffer)
{
    if (exponent_10 < 0)
    {
        *buffer++ = '-';
        exponent_10 = -exponent_10;
    }
    if (exponent_10 >= 100)
    {
        buffer[2] = char('0' + (exponent_10 % 10));
        exponent_10 /= 10;
        buffer[1] = char('0' + (exponent_10 % 10));
        buffer[0] = char('0' + (exponent_10 / 10));
        buffer += 3;
    }
    else if (exponent_10 >= 10)
    {
        *buffer++ = char('0' + (exponent_10 / 10));
        *buffer++ = char('0' + (exponent_10 % 10));
    }
    else
    {
        *buffer++ = char('0' + exponent_10);
    }
    *buffer = '\0';
    return buffer;
}

inline char* format(char* buffer0, int digits_length, int exponent_10)
{
    char* buffer = buffer0;
    const auto scientific_e = digits_length + exponent_10 - 1;
    const auto integral_length = std::max(0, scientific_e) + 1;
    const auto fractional_length = std::max(0, -exponent_10);
    const auto plain_length = integral_length + fractional_length + 1;
    const auto scientific_length = digits_length + 2 +
        (scientific_e >= 0 ? count_decimal_digits(uint32_t(scientific_e)) :
            count_decimal_digits(uint32_t(-scientific_e)) + 1);

    if (plain_length <= scientific_length)
    {
        if (exponent_10 >= 0)
        {
            std::memset(buffer + digits_length, '0', exponent_10);
            buffer += digits_length + exponent_10;
            *buffer++ = '.';
        }
        else if (scientific_e < 0)
        {
            std::memmove(&buffer[1 - scientific_e], &buffer[0], digits_length);
            buffer[0] = '0';
            buffer[1] = '.';
            std::memset(&buffer[2], '0', -scientific_e - 1);
            buffer += 1 - scientific_e + digits_length;
        }
        else
        {
            std::memmove(&buffer[scientific_e + 2], &buffer[scientific_e + 1],
                digits_length - (scientific_e + 1));
            buffer[scientific_e + 1] = '.';
            buffer += digits_length + 1;
        }
        *buffer = '\0';
        assert(buffer0 + plain_length == buffer);
        return buffer;
    }
    else
    {
        std::memmove(&buffer[2], &buffer[1], size_t(digits_length) - 1);
        buffer[1] = '.';
        buffer[digits_length + 1] = 'e';
        buffer = write_exponent(scientific_e, &buffer[digits_length + 2]);
        assert(buffer0 + scientific_length == buffer);
        return buffer;
    }
}

constexpr size_t integer_buffer_size = 21u;
constexpr size_t double_buffer_size = 25u;
constexpr size_t complex_buffer_size = 2u * double_buffer_size + 2u;
constexpr size_t default_buffer_size = 64u;

template<typename X>
auto integer_to_string(char* const first, char* const last, const X& x)
{
    assert(first + integer_buffer_size <= last);
    using Unsigned = std::make_unsigned_t<decltype(x + 0)>;

    char* ptr = last;
    if (x == X(0))
    {
        *--ptr = '0';
    }
    else
    {
        bool is_negative = false;
        Unsigned ux = 0;
        if constexpr (std::is_unsigned_v<X>)
            ux = Unsigned(x);
        else
        {
            is_negative = x < X(0);
            ux = is_negative ? Unsigned(-x) : Unsigned(x);
        }
        do
        {
            *--ptr = char(Unsigned('0') + (ux % 10u));
            ux /= 10u;
        } while (ux != 0u);
        if (!std::is_unsigned_v<X> && is_negative)
            *--ptr = '-';
    }
    return string_view(ptr, last);
}

template<bool AlwaysPrintSign = false>
auto double_to_string(char* const first, char* const last, double x)
{
    assert(first + double_buffer_size <= last);
#ifdef WL_DOUBLE_TO_STRING_SHORT
        int nchar = snprintf(first, last - first,
            AlwaysPrintSign ? "%+g" : "%g", x);
    return string_view(first, first + nchar);
#endif
    char* ptr = first;
    int fpclass = std::fpclassify(x);
    int signbit = std::signbit(x);
    switch (fpclass)
    {
    case FP_ZERO:
        if (signbit)
            *ptr++ = '-';
        else if constexpr (AlwaysPrintSign)
            *ptr++ = '+';
        *ptr++ = '0';
        *ptr++ = '.';
        *ptr = '\0';
        break;
    case FP_INFINITE:
        if (signbit)
            *ptr++ = '-';
        else if constexpr (AlwaysPrintSign)
            *ptr++ = '+';
        *ptr++ = 'i';
        *ptr++ = 'n';
        *ptr++ = 'f';
        *ptr = '\0';
        break;
    case FP_NAN:
        *ptr++ = 'n';
        *ptr++ = 'a';
        *ptr++ = 'n';
        *ptr = '\0';
        break;
    default:
        if (signbit)
        {
            *ptr++ = '-';
            x = -x;
        }
        else if constexpr (AlwaysPrintSign)
            *ptr++ = '+';
        int length = 0;
        int exponent_10 = 0;
        grisu2(x, ptr, length, exponent_10);
        ptr = format(ptr, length, exponent_10);
    }
    return string_view(first, ptr);
}

template<typename T>
auto complex_to_string(char* const first, char* const last, complex<T> x)
{
    assert(first + complex_buffer_size <= last);
    auto ptr = first;
    ptr = (char*)double_to_string<false>(
        ptr, last, double(x.real())).byte_end();
    ptr = (char*)double_to_string<true>(
        ptr, last, double(x.imag())).byte_end();
    *ptr++ = '*';
    *ptr++ = 'I';
    *ptr = '\0';
    return string_view(first, ptr);
}

}

namespace _from_string_impl
{

inline bool string_to_double(const char* str, double& val)
{
    char* end = nullptr;
    val = std::strtod(str, &end);
    return (end > str) && (*end == '\0');
}

inline bool string_to_integer(const char* str, int64_t& val)
{
    char* end = nullptr;
    val = std::strtoll(str, &end, 10);
    return (end > str) && (*end == '\0');
}

}

}
