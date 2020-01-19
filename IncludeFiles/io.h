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

#include <iostream>

#include "types.h"
#include "ndarray.h"
#include "utils.h"

namespace wl
{

namespace io
{

#if defined(WL_USE_MATHLINK)

template<typename X>
auto print(const X& x);

template<typename X>
auto echo(X&& x);

#else

template<typename X>
auto print(const X& x)
{
    std::cout << x << std::endl;
    return const_null;
}

template<typename X>
auto echo(X&& x)
{
    std::cout << x << std::endl;
    return std::forward<decltype(x)>(x);
}

#endif

template<typename Function>
auto echo_function(Function f)
{
    return [=](auto&& x)
    {
        const auto& xref = x;
        echo(f(x));
        return std::forward<decltype(x)>(x);
    };
}

}

}
