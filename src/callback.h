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

#if defined(WL_USE_MATHLINK)

#include <string>
#include <sstream>

#include "mathlink.h"
#include "WolframLibrary.h"

#include "const.h"

namespace wl
{

namespace librarylink
{

extern WolframLibraryData lib_data;

}

namespace mathlink
{

struct link_t
{
    MLINK link_;

    link_t()
    {
        link_ = librarylink::lib_data->getMathLink(librarylink::lib_data);
    }

    ~link_t()
    {
        int pkt = MLNextPacket(link_);
        if (pkt == RETURNPKT)
            MLNewPacket(link_);
    }

    [[noreturn]] void failed()
    {
        throw std::logic_error("callback failed");
    }

    template<size_t Level, typename T, size_t R>
    void _put_array_impl(const std::array<size_t, R>& dims, const T*& ptr)
    {
        if constexpr (Level + 1u == R)
        {
            this->put("List", dims[Level]);
            for (size_t i = 0; i < dims[Level]; ++i, ++ptr)
                this->put(*ptr);
        }
        else
        {
            this->put("List", dims[Level]);
            for (size_t i = 0; i < dims[Level]; ++i)
                this->_put_array_impl<Level + 1u>(dims, ptr);
        }
    }

    template<typename T, size_t R>
    link_t& put_array(const std::array<size_t, R>& dims, const T* ptr)
    {
        _put_array_impl<0u>(dims, ptr);
        return *this;
    }

    link_t& put(const char* name, size_t argc)
    {
        if (!MLPutFunction(link_, name, int(argc)))
            this->failed();
        return *this;
    }

    link_t& put(const char* name)
    {
        if (!MLPutSymbol(link_, name))
            this->failed();
        return *this;
    }

    template<typename T>
    link_t& put(const T& val)
    {
        int noerror = 0;
        if constexpr (std::is_same_v<T, void_type>)
            noerror = MLPutSymbol(link_, "Null");
        else if constexpr (is_boolean_v<T>)
            noerror = MLPutSymbol(link_, val ? "True" : "False");
        else if constexpr (is_integral_v<T>)
            noerror = MLPutInteger64(link_, mlint64(val));
        else if constexpr (is_float_v<T>)
            noerror = MLPutReal64(link_, double(val));
        else if constexpr (is_complex_v<T>)
            (*this).put("Complex", 2).put(val.real()).put(val.imag());
        else if constexpr (is_string_v<T>)
            noerror = MLPutString(link_, val.c_str());
        else
            static_assert(always_false_v<T>, "badargtype");
        if (!noerror)
            this->failed();
        return *this;
    }

    void eof()
    {
        if (!librarylink::lib_data->processMathLink(link_))
            this->failed();
    }
};

}

}

#endif
