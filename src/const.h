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

constexpr auto const_null        = void_type{};
constexpr auto const_all         = all_type{};
constexpr auto const_pi          = double(3.1415926535897932385e+0);
constexpr auto const_e           = double(2.7182818284590452354e+0);
constexpr auto const_degree      = double(1.7453292519943295769e-2);
constexpr auto const_euler_gamma = double(5.7721566490153286061e-1);
constexpr auto const_i           = complex<double>(0.f, 1.f);
constexpr auto const_true        = boolean(true);
constexpr auto const_false       = boolean(false);

}
