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

#include <random>
#include <string>

#include "math_compile.h"
#include "WolframLibrary.h"
#include "WolframNumericArrayLibrary.h"

namespace wl
{

namespace librarylink
{

template<typename T>
inline auto get(MArgument arg)
{
    if constexpr (is_boolean_v<T>)
    {
        return boolean(bool(MArgument_getBoolean(arg)));
    }
    else if constexpr (is_integral_v<T>)
    {
        return T(MArgument_getInteger(arg));
    }
    else if constexpr (is_float_v<T>)
    {
        return T(MArgument_getReal(arg));
    }
    else if constexpr (is_complex_v<T>)
    {
        const mcomplex& val = MArgument_getComplex(arg);
        return T(value_type_t<T>(val.ri[0]), value_type_t<T>(val.ri[1]));
    }
    else
    {
        return 0;
    }
}

template<typename T>
inline void set(MArgument& res, const T& val)
{
    if constexpr (is_boolean_v<T>)
    {
        MArgument_setBoolean(res, mbool(val));
    }
    else if constexpr (is_integral_v<T>)
    {
        MArgument_setInteger(res, mint(val));
    }
    else if constexpr (is_float_v<T>)
    {
        MArgument_setReal(res, mreal(val));
    }
    else if constexpr (is_complex_v<T>)
    {
        mcomplex wval;
        wval.ri[0] = double(std::real(val));
        wval.ri[1] = double(std::imag(val));
        MArgument_setComplex(res, wval);
    }
}

}

}
