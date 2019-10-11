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
#include <random>
#include <string>
#include <type_traits>

#include "math_compile.h"
#include "WolframLibrary.h"
#include "WolframNumericArrayLibrary.h"

namespace wl
{

namespace librarylink
{

extern WolframLibraryData lib_data;

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
        return MNumericArray_Type_Undef;
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
            T* ptr = reinterpret_cast<T*>(
                lib_data->MTensor_getIntegerData(tensor));
            return ndarray<T, R>(dims, ptr, ptr + size);
        }
        else if (type == MNumericArray_Type_Real64)
        {
            if (input_type != MType_Real) throw LIBRARY_TYPE_ERROR;
            T* ptr = reinterpret_cast<T*>(
                lib_data->MTensor_getRealData(tensor));
            return ndarray<T, R>(dims, ptr, ptr + size);
        }
        else // type == MNumericArray_Type_Complex_Real64
        {
            if (input_type != MType_Complex) throw LIBRARY_TYPE_ERROR;
            T* ptr = reinterpret_cast<T*>(
                lib_data->MTensor_getComplexData(tensor));
            return ndarray<T, R>(dims, ptr, ptr + size);
        }
    }
    else // pass by numeric array
    {
        auto na_lib_data = lib_data->numericarrayLibraryFunctions;

        auto narray = MArgument_getMNumericArray(arg);
        auto input_type = na_lib_data->MNumericArray_getType(narray);
        auto input_rank = size_t(na_lib_data->MNumericArray_getRank(narray));
        const mint* input_dims = na_lib_data->MNumericArray_getDimensions(narray);

        if (input_type != type) throw LIBRARY_TYPE_ERROR;
        if (input_rank != R) throw LIBRARY_RANK_ERROR;
        std::array<size_t, R> dims;
        std::copy_n(input_dims, R, dims.data());
        const size_t size = utils::size_of_dims(dims);

        T* ptr = reinterpret_cast<T*>(
            na_lib_data->MNumericArray_getData(narray));
        return ndarray<T, R>(dims, ptr, ptr + size);
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
        static_assert(always_false_v<T>, "internal");
        return 0;
    }
}

template<typename T, size_t R>
int set_array(MArgument& res, const wl::ndarray<T, R>& val)
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
            int error = lib_data->MTensor_new(
                MType_Integer, R, output_dims.data(), &tensor);
            if (error) throw error;
            int64_t* ptr = lib_data->MTensor_getIntegerData(tensor);
            std::copy_n(val.data(), val.size(), ptr);
        }
        else if (type == MNumericArray_Type_Real64)
        {
            int error = lib_data->MTensor_new(
                MType_Real, R, output_dims.data(), &tensor);
            if (error) throw error;
            double* ptr = lib_data->MTensor_getRealData(tensor);
            std::copy_n(val.data(), val.size(), ptr);
        }
        else // type == MNumericArray_Type_Complex_Real64
        {
            int error = lib_data->MTensor_new(
                MType_Complex, R, output_dims.data(), &tensor);
            if (error) throw error;
            complex<double>* ptr = reinterpret_cast<complex<double>*>(
                lib_data->MTensor_getRealData(tensor));
            std::copy_n(val.data(), val.size(), ptr);
        }
        MArgument_setMTensor(res, tensor);
    }
    else
    {
        auto na_lib_data = lib_data->numericarrayLibraryFunctions;
        MNumericArray narray;
        int error = na_lib_data->MNumericArray_new(
            type, R, output_dims.data(), &narray);
        if (error) throw error;

        T* ptr = reinterpret_cast<T*>(
            na_lib_data->MNumericArray_getData(narray));
        std::copy_n(val.data(), val.size(), ptr);
        MArgument_setMTensor(res, narray);
    }
}

template<typename T>
void set(MArgument& res, const T& val)
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
    else if constexpr (is_array_v<T>)
    {
        set_array(res, val);
    }
    else
    {
        static_assert(always_false_v<T>, "internal");
    }
}

}

}
