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

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <random>
#include <string>
#include <sstream>
#include <type_traits>

#include "math_compile.h"

#include "mathlink.h"
#include "WolframLibrary.h"
#include "WolframLibrary.h"
#include "WolframNumericArrayLibrary.h"

namespace wl
{

namespace librarylink
{

extern WolframLibraryData lib_data;

struct mathlink_t
{
    MLINK link_;

    mathlink_t()
    {
        link_ = librarylink::lib_data->getMathLink(librarylink::lib_data);
    }

    ~mathlink_t()
    {
        int pkt = MLNextPacket(link_);
        if (pkt == RETURNPKT)
            MLNewPacket(link_);
    }

    [[noreturn]] void failed()
    {
        throw std::logic_error(WL_ERROR_CALLBACK);
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
    mathlink_t& put_array(const std::array<size_t, R>& dims, const T* ptr)
    {
        _put_array_impl<0u>(dims, ptr);
        return *this;
    }

    mathlink_t& put(const char* name, size_t argc)
    {
        if (!MLPutFunction(link_, name, int(argc)))
            this->failed();
        return *this;
    }

    mathlink_t& put(const char* name)
    {
        if (!MLPutSymbol(link_, name))
            this->failed();
        return *this;
    }

    template<typename T>
    mathlink_t& put(const T& val)
    {
        int noerror = 0;
        if constexpr (std::is_same_v<T, void_type>)
            noerror = MLPutSymbol(link_, "Null");
        else if constexpr (std::is_same_v<T, all_type>)
            noerror = MLPutSymbol(link_, "All");
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
            static_assert(always_false_v<T>, WL_ERROR_INTERNAL);
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

template<typename ReturnType>
mint get_return_type_id()
{
    enum : mint
    {
        MathLink = 1, Null, Bool,
        I8, U8, I16, U16, I32, U32, I64, U64,
        R32, R64, C32, C64
    };
    using V = std::conditional_t<wl::is_array_v<ReturnType>,
        wl::value_type_t<ReturnType>, ReturnType>;
    mint rank = mint(wl::array_rank_v<ReturnType>);
    mint type =
        std::is_same_v<V, void_type>       ? Null :
        std::is_same_v<V, boolean>         ? Bool :
        std::is_same_v<V, int8_t>          ? I8   :
        std::is_same_v<V, uint8_t>         ? U8   :
        std::is_same_v<V, int16_t>         ? I16  :
        std::is_same_v<V, uint16_t>        ? U16  :
        std::is_same_v<V, int32_t>         ? I32  :
        std::is_same_v<V, uint32_t>        ? U32  :
        std::is_same_v<V, int64_t>         ? I64  :
        std::is_same_v<V, uint64_t>        ? U64  :
        std::is_same_v<V, float>           ? R32  :
        std::is_same_v<V, double>          ? R64  :
        std::is_same_v<V, complex<float>>  ? C32  :
        std::is_same_v<V, complex<double>> ? C64  :
        MathLink;
    constexpr mint max_type_count = 256;
    return rank * max_type_count + type;
}

template<typename T>
numericarray_data_t get_numeric_array_type()
{
    if constexpr (std::is_same_v<T, int8_t>)
        return MNumericArray_Type_Bit8;
    else if constexpr (std::is_same_v<T, uint8_t>)
        return MNumericArray_Type_UBit8;
    else if constexpr (std::is_same_v<T, int16_t>)
        return MNumericArray_Type_Bit16;
    else if constexpr (std::is_same_v<T, uint16_t>)
        return MNumericArray_Type_UBit16;
    else if constexpr (std::is_same_v<T, int32_t>)
        return MNumericArray_Type_Bit32;
    else if constexpr (std::is_same_v<T, uint32_t>)
        return MNumericArray_Type_UBit32;
    else if constexpr (std::is_same_v<T, int64_t>)
        return MNumericArray_Type_Bit64;
    else if constexpr (std::is_same_v<T, uint64_t>)
        return MNumericArray_Type_UBit64;
    else if constexpr (std::is_same_v<T, boolean>)
        return MNumericArray_Type_Bit8;
    else if constexpr (std::is_same_v<T, float>)
        return MNumericArray_Type_Real32;
    else if constexpr (std::is_same_v<T, double>)
        return MNumericArray_Type_Real64;
    else if constexpr (std::is_same_v<T, complex<float>>)
        return MNumericArray_Type_Complex_Real32;
    else if constexpr (std::is_same_v<T, complex<double>>)
        return MNumericArray_Type_Complex_Real64;
    else
        static_assert(always_false_v<T>, WL_ERROR_INTERNAL);
}

template<typename T, size_t R>
auto get_array(MArgument arg)
{
    const numericarray_data_t type = get_numeric_array_type<T>();
    bool pass_by_tensor = false;
    switch (type)
    {
    case MNumericArray_Type_Bit64:
    case MNumericArray_Type_Real64:
    case MNumericArray_Type_Complex_Real64:
        pass_by_tensor = true;
        break;
    case MNumericArray_Type_Undef:
        throw LIBRARY_TYPE_ERROR;
    }

    if (pass_by_tensor)
    {
        auto tensor = MArgument_getMTensor(arg);
        auto input_type = lib_data->MTensor_getType(tensor);
        auto input_rank = size_t(lib_data->MTensor_getRank(tensor));
        const mint* input_dims = lib_data->MTensor_getDimensions(tensor);

        if (input_rank != R) throw LIBRARY_RANK_ERROR;
        std::array<size_t, R> dims;
        std::copy_n(input_dims, R, dims.data());
        const size_t size = utils::size_of_dims(dims);

        if (type == MNumericArray_Type_Bit64)
        {
            if (input_type != MType_Integer) throw LIBRARY_TYPE_ERROR;
            const auto* ptr = lib_data->MTensor_getIntegerData(tensor);
            ndarray<T, R> ret(dims);
            std::memcpy(ret.data(), ptr, ret.size() * sizeof(T));
            return ret;
        }
        else if (type == MNumericArray_Type_Real64)
        {
            if (input_type != MType_Real) throw LIBRARY_TYPE_ERROR;
            const auto* ptr = lib_data->MTensor_getRealData(tensor);
            ndarray<T, R> ret(dims);
            std::memcpy(ret.data(), ptr, ret.size() * sizeof(T));
            return ret;
        }
        else // type == MNumericArray_Type_Complex_Real64
        {
            if (input_type != MType_Complex) throw LIBRARY_TYPE_ERROR;
            const auto* ptr = lib_data->MTensor_getComplexData(tensor);
            ndarray<T, R> ret(dims);
            std::memcpy(ret.data(), ptr, ret.size() * sizeof(T));
            return ret;
        }
    }
    else // pass by numeric array
    {
        auto na_lib_data = lib_data->numericarrayLibraryFunctions;

        auto narray = MArgument_getMNumericArray(arg);
        auto input_type = na_lib_data->MNumericArray_getType(narray);
        auto input_rank = size_t(na_lib_data->MNumericArray_getRank(narray));
        const mint* input_dims =
            na_lib_data->MNumericArray_getDimensions(narray);

        if (input_type != type) throw LIBRARY_TYPE_ERROR;
        if (input_rank != R) throw LIBRARY_RANK_ERROR;
        std::array<size_t, R> dims;
        std::copy_n(input_dims, R, dims.data());
        const size_t size = utils::size_of_dims(dims);

        const auto* ptr = na_lib_data->MNumericArray_getData(narray);
        ndarray<T, R> ret(dims);
        std::memcpy(ret.data(), ptr, ret.size() * sizeof(T));
        return ret;
    }
}

template<typename T>
auto get(MArgument arg)
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
    else if constexpr (is_array_v<T>)
    {
        return get_array<value_type_t<T>, array_rank_v<T>>(arg);
    }
    else
    {
        static_assert(always_false_v<T>, WL_ERROR_INTERNAL);
        return 0;
    }
}

template<typename T, size_t R>
void set_array(MArgument& res, const ndarray<T, R>& val)
{
    const numericarray_data_t type = get_numeric_array_type<T>();
    bool pass_by_tensor = false;
    switch (type)
    {
    case MNumericArray_Type_Bit64:
    case MNumericArray_Type_Real64:
    case MNumericArray_Type_Complex_Real64:
        pass_by_tensor = true;
        break;
    case MNumericArray_Type_Undef:
        throw LIBRARY_TYPE_ERROR;
    }

    std::array<mint, R> output_dims;
    std::copy_n(val.dims().data(), R, output_dims.data());

    if (pass_by_tensor)
    {
        MTensor tensor;
        if (type == MNumericArray_Type_Bit64)
        {
            auto error = lib_data->MTensor_new(
                MType_Integer, R, output_dims.data(), &tensor);
            if (error) throw error;
            auto* ptr = lib_data->MTensor_getIntegerData(tensor);
            std::memcpy(ptr, val.data(), val.size() * sizeof(T));
        }
        else if (type == MNumericArray_Type_Real64)
        {
            auto error = lib_data->MTensor_new(
                MType_Real, R, output_dims.data(), &tensor);
            if (error) throw error;
            auto* ptr = lib_data->MTensor_getRealData(tensor);
            std::memcpy(ptr, val.data(), val.size() * sizeof(T));
        }
        else // type == MNumericArray_Type_Complex_Real64
        {
            auto error = lib_data->MTensor_new(
                MType_Complex, R, output_dims.data(), &tensor);
            if (error) throw error;
            auto* ptr = lib_data->MTensor_getComplexData(tensor);
            std::memcpy(ptr, val.data(), val.size() * sizeof(T));
        }
        MArgument_setMTensor(res, tensor);
    }
    else
    {
        auto na_lib_data = lib_data->numericarrayLibraryFunctions;
        MNumericArray narray;
        auto error = na_lib_data->MNumericArray_new(
            type, R, output_dims.data(), &narray);
        if (error) throw error;
        auto* ptr = na_lib_data->MNumericArray_getData(narray);
        std::memcpy(ptr, val.data(), val.size() * sizeof(T));
        MArgument_setMTensor(res, narray);
    }
}

template<typename Any>
void set_expr(const Any& any)
{
    mathlink_t link;
    link.put("EvaluatePacket", 1).
        put("CompoundExpression", 2).
        put("Set", 2).
        put("MathCompile`Private`linkreturn");
    if constexpr (is_array_v<Any>)
        link.put_array(any.dims(), any.data());
    else
        link.put(any);
    link.put("Null").
        eof();
}

template<typename T>
void set(MArgument& res, const T& val)
{
    if constexpr (std::is_same_v<T, void_type>)
    {
    }
    else if constexpr (is_boolean_v<T>)
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
        mcomplex dest;
        dest.ri[0] = mreal(std::real(val));
        dest.ri[1] = mreal(std::imag(val));
        MArgument_setComplex(res, dest);
    }
    else if constexpr (is_array_v<T>)
    {
        if constexpr (is_string_v<value_type_t<T>>)
            set_expr(val);
        else
            set_array(res, val);
    }
    else if constexpr (is_string_v<T>)
    {
        set_expr(val);
    }
    else
    {
        static_assert(always_false_v<T>, WL_ERROR_INTERNAL);
    }
}

template<typename String>
void send_error(const String& what) noexcept
{
    try
    {
        mathlink_t link;
        link.put("EvaluatePacket", 1).
            put("Message", 2).
            put("MessageName", 2).
            put("runtime").put(std::string("error")).
            put(get_stack_message(what)).
            eof();
    }
    catch (...)
    {
    }
}

}

namespace io
{

template<typename X>
auto print(const X& x)
{
    WL_TRY_BEGIN()
    librarylink::mathlink_t link;
    link.put("EvaluatePacket", 1).
        put("Print", 1);
    if constexpr (is_array_v<>)
        link.put_array(x.dims(), x.data());
    else
        link.put(x);
    link.eof();
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

template<typename X>
auto echo(X&& x)
{
    WL_TRY_BEGIN()
    librarylink::mathlink_t link;
    link.put("EvaluatePacket", 1).
        put("CompoundExpression", 2).
        put("Echo", 1);
    if constexpr (is_array_v<X>)
        link.put_array(x.dims(), x.data());
    else
        link.put(x);
    link.put("Null").
        eof();
    return std::forward<decltype(x)>(x);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}

}

}

#endif // defined (WL_USE_MATHLINK)
