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
#include "WolframCompileLibrary.h"
#include "WolframNumericArrayLibrary.h"
#include "WolframRTL.h"

namespace wl
{

namespace librarylink
{

extern WolframLibraryData lib_data;
extern WolframCompileLibrary_Functions lib_functions;
extern std::unique_ptr<void*[]> kernel_fptrs;

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
        int noerror = 1;
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
            noerror = MLPutUTF8String(link_,
            (const unsigned char*)val.c_str(), int(val.byte_size()));
        else if constexpr (std::is_same_v<T, std::string>)
            noerror = MLPutUTF8String(link_,
            (const unsigned char*)val.c_str(), int(val.size()));
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
#define WL_CATEGORIZE_INTEGRAL_V(byte, sign) \
        std::is_integral_v<V> && sizeof(V) == byte && std::is_##sign##_v<V>
    mint type =
        std::is_same_v<V, void_type>          ? Null :
        std::is_same_v<V, boolean>            ? Bool :
        WL_CATEGORIZE_INTEGRAL_V(1, signed)   ? I8   :
        WL_CATEGORIZE_INTEGRAL_V(1, unsigned) ? U8   :
        WL_CATEGORIZE_INTEGRAL_V(2, signed)   ? I16  :
        WL_CATEGORIZE_INTEGRAL_V(2, unsigned) ? U16  :
        WL_CATEGORIZE_INTEGRAL_V(4, signed)   ? I32  :
        WL_CATEGORIZE_INTEGRAL_V(4, unsigned) ? U32  :
        WL_CATEGORIZE_INTEGRAL_V(8, signed)   ? I64  :
        WL_CATEGORIZE_INTEGRAL_V(8, unsigned) ? U64  :
        std::is_same_v<V, float>              ? R32  :
        std::is_same_v<V, double>             ? R64  :
        std::is_same_v<V, complex<float>>     ? C32  :
        std::is_same_v<V, complex<double>>    ? C64  :
        MathLink;
#undef WL_CATEGORIZE_INTEGRAL_V
    constexpr mint max_type_count = 256;
    return rank * max_type_count + type;
}

template<typename T>
constexpr numericarray_data_t get_numeric_array_type()
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

template<typename T, size_t R, bool GetArrayView = false>
auto mtensor_to_ndarray(const MTensor& tensor,
    const int mtensor_value_type)
{
    auto input_type = lib_data->MTensor_getType(tensor);
    auto input_rank = size_t(lib_data->MTensor_getRank(tensor));
    const mint* input_dims = lib_data->MTensor_getDimensions(tensor);

    if (input_rank != R)
        throw int(LIBRARY_RANK_ERROR);
    if (input_type != mtensor_value_type)
        throw int(LIBRARY_TYPE_ERROR);

    std::array<size_t, R> dims;
    std::copy_n(input_dims, R, dims.data());
    const size_t size = utils::size_of_dims(dims);
    if constexpr (GetArrayView)
    {
        if constexpr (is_integral_v<T> && sizeof(T) == sizeof(mint))
        {
            if (mtensor_value_type != MType_Integer)
                throw std::logic_error("type error");
        }
        else if constexpr (is_float_v<T> && sizeof(T) == sizeof(mreal))
        {
            if (mtensor_value_type != MType_Real)
                throw std::logic_error("type error");
        }
        else if constexpr (is_complex_v<T> && sizeof(T) == sizeof(mcomplex))
        {
            if (mtensor_value_type != MType_Complex)
                throw std::logic_error("type error");
        }
        else
        {
            static_assert(always_false_v<T>, WL_ERROR_INTERNAL);
        }
        const void* input_ptr;
        if (mtensor_value_type == MType_Integer)
            input_ptr = (const void*)lib_data->MTensor_getIntegerData(tensor);
        else if (mtensor_value_type == MType_Real)
            input_ptr = (const void*)lib_data->MTensor_getRealData(tensor);
        else
            input_ptr = (const void*)lib_data->MTensor_getComplexData(tensor);
        return simple_view<T, R, R, true>(
            (const void*)input_dims, (const T*)input_ptr, dims.data());
    }
    else
    {
        ndarray<T, R> ret(dims);
        if (ret.size() == 0u)
            return ret;
        if (mtensor_value_type == MType_Integer)
        {
            const auto* input_ptr = lib_data->MTensor_getIntegerData(tensor);
            WL_THROW_ERROR_IF_NULLPTR(input_ptr, LIBRARY_TYPE_ERROR);
            if constexpr (is_integral_v<T>)
                utils::restrict_copy_n(input_ptr, size, ret.data());
        }
        else if (mtensor_value_type == MType_Real)
        {
            const auto* input_ptr = lib_data->MTensor_getRealData(tensor);
            WL_THROW_ERROR_IF_NULLPTR(input_ptr, LIBRARY_TYPE_ERROR);
            if constexpr (is_float_v<T>)
                utils::restrict_copy_n(input_ptr, size, ret.data());
        }
        else if (mtensor_value_type == MType_Complex)
        {
            const auto* input_ptr = (const complex<mreal>*)
                lib_data->MTensor_getComplexData(tensor);
            WL_THROW_ERROR_IF_NULLPTR(input_ptr, LIBRARY_TYPE_ERROR);
            if constexpr (is_complex_v<T>)
                utils::restrict_copy_n(input_ptr, size, ret.data());
        }
        else
        {
            throw LIBRARY_TYPE_ERROR;
        }
        return ret;
    }
}

template<typename X>
void ndarray_to_mtensor(const X& val, MTensor& tensor,
    const int mtensor_value_type)
{
    using T = value_type_t<X>;
    constexpr auto R = array_rank_v<X>;
    std::array<mint, R> output_dims;
    std::copy_n(val.dims().data(), R, output_dims.data());

    bool type_is_matched = false;
    if constexpr (is_integral_v<T>)
        type_is_matched = (mtensor_value_type == MType_Integer);
    else if constexpr (is_float_v<T>)
        type_is_matched = (mtensor_value_type == MType_Real);
    else if constexpr (is_complex_v<T>)
        type_is_matched = (mtensor_value_type == MType_Complex);
    if (!type_is_matched)
        throw LIBRARY_TYPE_ERROR;

    auto error = lib_data->MTensor_new(
        mtensor_value_type, R, output_dims.data(), &tensor);
    if (error)
        throw error;
    if (val.size() == 0u)
        return;
    if (mtensor_value_type == MType_Integer)
    {
        if constexpr (is_integral_v<T>)
        {
            auto* output_ptr = (mint*)lib_data->MTensor_getIntegerData(tensor);
            WL_THROW_ERROR_IF_NULLPTR(output_ptr, LIBRARY_TYPE_ERROR);
            val.copy_to(output_ptr);
        }
    }
    else if (mtensor_value_type == MType_Real)
    {
        if constexpr (is_float_v<T>)
        {
            auto* output_ptr = (mreal*)lib_data->MTensor_getRealData(tensor);
            WL_THROW_ERROR_IF_NULLPTR(output_ptr, LIBRARY_TYPE_ERROR);
            val.copy_to(output_ptr);
        }
    }
    else
    {
        if constexpr (is_complex_v<T>)
        {
            auto* output_ptr =
                (complex<mreal>*)lib_data->MTensor_getComplexData(tensor);
            WL_THROW_ERROR_IF_NULLPTR(output_ptr, LIBRARY_TYPE_ERROR);
            val.copy_to(output_ptr);
        }
    }
}

template<typename T, size_t R>
auto get_array(MArgument arg)
{
    constexpr numericarray_data_t type = get_numeric_array_type<T>();
    int mtensor_value_type = 0;
    switch (type)
    {
    case MNumericArray_Type_Bit64:
        mtensor_value_type = MType_Integer;
        break;
    case MNumericArray_Type_Real64:
        mtensor_value_type = MType_Real;
        break;
    case MNumericArray_Type_Complex_Real64:
        mtensor_value_type = MType_Complex;
        break;
    case MNumericArray_Type_Undef:
        throw int(LIBRARY_TYPE_ERROR);
    }

    if constexpr (type == MNumericArray_Type_Bit64 ||
        type == MNumericArray_Type_Real64 ||
        type == MNumericArray_Type_Complex_Real64)
    {
        auto tensor = MArgument_getMTensor(arg);
        return mtensor_to_ndarray<T, R, true>(tensor, mtensor_value_type);
    }
    else // pass by numeric array
    {
        auto na_lib_data = lib_data->numericarrayLibraryFunctions;

        auto narray = MArgument_getMNumericArray(arg);
        auto input_type = na_lib_data->MNumericArray_getType(narray);
        auto input_rank = size_t(na_lib_data->MNumericArray_getRank(narray));
        const mint* input_dims =
            na_lib_data->MNumericArray_getDimensions(narray);

        if (input_type != type) throw int(LIBRARY_TYPE_ERROR);
        if (input_rank != R) throw int(LIBRARY_RANK_ERROR);
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
    else if constexpr (is_string_v<T>)
    {
        char* str = MArgument_getUTF8String(arg);
        bool ascii_only = true;
        size_t byte_size = wl::utf8::_get_byte_size(
            (const wl::utf8::char_t*)str, ascii_only);
        auto ret = wl::string((const wl::utf8::char_t*)str,
            byte_size, ascii_only);
        lib_data->UTF8String_disown(str);
        return ret;
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
    int mtensor_value_type = 0;
    switch (type)
    {
    case MNumericArray_Type_Bit64:
        mtensor_value_type = MType_Integer;
        break;
    case MNumericArray_Type_Real64:
        mtensor_value_type = MType_Real;
        break;
    case MNumericArray_Type_Complex_Real64:
        mtensor_value_type = MType_Complex;
        break;
    case MNumericArray_Type_Undef:
        throw int(LIBRARY_TYPE_ERROR);
    }

    if (mtensor_value_type != 0)
    {
        MTensor tensor;
        ndarray_to_mtensor(val, tensor, mtensor_value_type);
        MArgument_setMTensor(res, tensor);
    }
    else
    {
        std::array<mint, R> output_dims;
        std::copy_n(val.dims().data(), R, output_dims.data());
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
            put("MCRuntime").put(std::string("error")).
            put(get_stack_message(what)).
            eof();
    }
    catch (...)
    {
    }
}

template<typename Ret>
struct kernel_function
{
    void* fptr_ = nullptr;

    kernel_function(size_t i)
    {
        fptr_ = kernel_fptrs.get()[i];
        if (!fptr_)
            throw std::logic_error("failed to get kernel function ptr.");
    }

    template<typename X>
    int get_mtype() const
    {
        if constexpr (array_rank_v<X> == 0u)
        {
            if constexpr (is_integral_v<X>)
                return MType_Integer;
            else if constexpr (is_float_v<X>)
                return MType_Real;
            else if constexpr (is_complex_v<X>)
                return MType_Complex;
            else
                return 0;
        }
        else
        {
            return get_mtype<value_type_t<X>>();
        }
    }

    template<typename X>
    void _prepare_args_impl(int& type, void* storage, const X& x) const
    {
        static_assert(is_numerical_type_v<X>,
            WL_ERROR_KERNEL_FUNCTION_ARG_TYPE);
        constexpr auto XR = array_rank_v<X>;
        if constexpr (XR == 0u)
        {
            type = get_mtype<X>();
            switch (type)
            {
            case MType_Integer:
                if constexpr (is_integral_v<X>)
                    new((mint*)storage) mint(cast<mint>(x));
                break;
            case MType_Real:
                if constexpr (is_float_v<X>)
                    new((mreal*)storage) mreal(cast<mreal>(x));
                break;
            case MType_Complex:
                if constexpr (is_complex_v<X>)
                    new((complex<mreal>*)storage) complex<mreal>(
                        cast<complex<mreal>>(x));
                break;
            default:
                throw LIBRARY_TYPE_ERROR;
            }
        }
        else
        {
            type = 0; // means MTensor
            using XV = value_type_t<X>;
            new((MTensor*)storage) MTensor(nullptr);
            ndarray_to_mtensor(x, *(MTensor*)storage, get_mtype<XV>());
        }
    }

    template<size_t NArgs, typename Storage, typename ArgsTuple, size_t... Is>
    void prepare_args(std::array<int, NArgs>& arg_types,
        std::array<Storage, NArgs>& arg_storages,
        const ArgsTuple& args, std::index_sequence<Is...>) const
    {
        [[maybe_unused]] const auto& _1 = (
            _prepare_args_impl(arg_types[Is], (void*)(&arg_storages[Is]),
                std::get<Is>(args)),
            ..., 0);
    }

    template<typename X>
    void _cleanup_args_impl(const X& x, void* storage) const
    {
        if constexpr (array_rank_v<X> >= 1u)
        {
            MTensor& mtensor = *(MTensor*)storage;
            lib_data->MTensor_free(mtensor);
        }
    }

    template<size_t NArgs, typename Storage, typename ArgsTuple, size_t... Is>
    void cleanup_args(std::array<Storage, NArgs>& arg_storages,
        const ArgsTuple& args, std::index_sequence<Is...>) const
    {
        [[maybe_unused]] const auto& _1 = (
            _cleanup_args_impl(std::get<Is>(args), (void*)(&arg_storages[Is])),
            ..., 0);
    }

    template<typename Storage>
    void prepare_ret(Storage& ret_storage, void*& ret_ptr,
        MTensorInitializationData& init_data) const
    {
        static_assert(is_numerical_type_v<Ret>,
            WL_ERROR_KERNEL_FUNCTION_ARG_TYPE);
        constexpr auto RR = array_rank_v<Ret>;
        if constexpr (RR == 0u)
        {
            ret_ptr = (void*)(&ret_storage);
        }
        else
        {
            std::array<mint, RR> dims{3};
            int err = lib_data->MTensor_new(get_mtype<Ret>(), RR, dims.data(), (MTensor*)(&ret_storage));
            ret_ptr = (void*)(&ret_storage);
        }
    }

    template<typename Storage>
    auto retrieve_ret(int err, Storage& storage, void*& ret_ptr,
        MTensorInitializationData& init_data) const
    {
        constexpr auto RR = array_rank_v<Ret>;
        if constexpr (RR == 0u)
        {
            if constexpr (is_integral_v<Ret>)
                return cast<Ret>(*(mint*)(&storage));
            else if constexpr (is_float_v<Ret>)
                return cast<Ret>(*(mreal*)(&storage));
            else if constexpr (is_complex_v<Ret>)
                return cast<Ret>(*(complex<mreal>*)(&storage));
            else
                static_assert(always_false_v<Storage>, WL_ERROR_INTERNAL);
        }
        else
        {
            auto ret = ndarray<value_type_t<Ret>, RR>{};
            if (!err)
            {
                const MTensor& mtensor = *(MTensor*)(&storage);
                const mint* dims = lib_data->MTensor_getDimensions(mtensor);
                ret = mtensor_to_ndarray<value_type_t<Ret>, RR>(
                    mtensor, get_mtype<Ret>());
            }
            lib_data->MTensor_free(*(MTensor*)(&storage));
            return ret;
        }
    }

    template<typename... Args>
    auto operator()(const Args&... args) const
    {
        //int (*evaluateFunctionExpression)(zstruct st_WolframLibraryData*, void *, mint, mint, mint, int *, void **, int, mint, void *);
        constexpr size_t NArgs = sizeof...(Args);
        using storage_t = char[sizeof(complex<double>)];
        std::array<int, NArgs> arg_types;
        std::array<storage_t, NArgs> arg_storages;
        std::array<void*, NArgs> arg_ptrs;
        storage_t ret_storage;
        void* ret_ptr = nullptr;
        MTensorInitializationData mtensor_init_data = nullptr;

        prepare_args(arg_types, arg_storages, std::tie(args...),
            std::make_index_sequence<NArgs>{});
        prepare_ret(ret_storage, ret_ptr, mtensor_init_data);
        // prepare the addresses of the arguments
        for (size_t i = 0; i < NArgs; ++i)
            arg_ptrs[i] = (void*)(&arg_storages[i]);
        
        int err = lib_functions->evaluateFunctionExpression(
            lib_data, fptr_, 0, 0,
            NArgs, arg_types.data(), arg_ptrs.data(),
            get_mtype<Ret>(), array_rank_v<Ret>, ret_ptr);
        
        cleanup_args(arg_storages, std::tie(args...),
            std::make_index_sequence<NArgs>{});
        auto ret = retrieve_ret(err, ret_storage, ret_ptr, mtensor_init_data);
        if (err)
            throw std::logic_error(WL_ERROR_KERNEL_FUNCTION_CALL);
        return ret;
    }
};

template<typename Ret>
auto extern_function(int64_t i, Ret)
{
    return kernel_function<Ret>(size_t(i));
}

}

template<typename X>
auto print(const X& x)
{
    WL_TRY_BEGIN()
    WL_THROW_IF_ABORT()
    librarylink::mathlink_t link;
    link.put("EvaluatePacket", 1).
        put("Print", 1);
    if constexpr (is_array_v<X>)
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
    WL_THROW_IF_ABORT()
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

#endif // defined (WL_USE_MATHLINK)
