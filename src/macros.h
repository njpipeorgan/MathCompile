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

#include <immintrin.h>

#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
#  if _MSC_VER < 1920
#    pragma message (": error cxx::compilerver")
#  endif
#  define WL_INLINE __forceinline
#  define WL_IGNORE_DEPENDENCIES __pragma(loop(ivdep))
#  define WL_RESTRICT __restrict
#  ifdef __AVX2__
#    define __AVX__ 1
#    define __BMI__ 1
#    define __BMI2__ 1
#  endif
#  ifdef __AVX__
#    define __POPCNT__ 1
#    define __LZCNT__ 1
#  endif
#  define _wl_popcnt64 __popcnt64
#  define NOMINMAX // disable min, max macros
#  define WL_FUNCSIG __FUNCSIG__
#elif defined(__INTEL_COMPILER)
#  if __INTEL_COMPILER < 1900
#    pragma message ("error: cxx::compilerver")
#  endif
#  define WL_INLINE __forceinline
#  define WL_IGNORE_DEPENDENCIES __pragma(ivdep)
#  define WL_RESTRICT __restrict
#  define WL_FUNCSIG __PRETTY_FUNCTION__
#  pragma warning (disable:1011)
#elif defined(__clang__)
#  if __clang_major__ < 5
#    pragma message ("error: cxx::compilerver")
#  endif
#  define WL_INLINE __attribute__((always_inline))
#  define WL_IGNORE_DEPENDENCIES _Pragma("ivdep")
#  define WL_RESTRICT __restrict__
#  define WL_FUNCSIG __PRETTY_FUNCTION__
#elif defined(__GNUC__)
#  if __GNUC__ < 7
#    pragma message ("error: cxx::compilerver")
#  endif
#  define WL_INLINE __attribute__((always_inline))
#  define WL_IGNORE_DEPENDENCIES _Pragma("ivdep")
#  define WL_RESTRICT __restrict__
#  define WL_FUNCSIG __PRETTY_FUNCTION__
#endif

// no random device for MinGW
#if defined(__MINGW64__)
#  define WL_NO_RANDOM_DEVICE 1
#endif

namespace wl
{

#define WL_FUNCTION(fn) wl::variadic( \
    [](auto&&... args) { return fn(std::forward<decltype(args)>(args)...); }, \
    [](auto&& arg) { return fn(std::forward<decltype(arg)>(arg)); })

#define WL_PASS(var) std::forward<decltype(var)>(var)

#define WL_RANDOM_ENGINE std::mt19937_64


// static assertion error messages

#define WL_ERROR_INTERNAL \
"An internal error is encountered."

#define WL_ERROR_MUTABLE_TYPE \
"The type of the result cannot be converted to that of the target."

#define WL_ERROR_MUTABLE_RANK \
"The array rank of the result is different from that of the target."

#define WL_ERROR_ASSIGN_TYPE \
"The type of the source cannot be converted to that of the target."

#define WL_ERROR_ASSIGN_RANK \
"The array rank of the source is different from that of the target."

#define WL_ERROR_LIST_ELEM_TYPE \
"The elements of the list do not have a consistent type."

#define WL_ERROR_LIST_ELEM_RANK \
"The elements of the list do not have a consistent array rank."

#define WL_ERROR_MODIFY_TARGET \
"The target is not a variable, so it cannot be modified."

#define WL_ERROR_OPERAND_RANK \
"The operands should be scalars or arrays of the same rank."

#define WL_ERROR_OPERAND_TYPE \
"The operands should contain values of the same type."

#define WL_ERROR_FUNCTION_TYPE \
"The argument is not a function gives an appropriate type."

#define WL_ERROR_PRED_TYPE \
"The function is not called with a valid predicate."

#define WL_ERROR_ORDER_PRED_TYPE \
"The function is not called with a valid ordering function."

#define WL_ERROR_NUMERIC_ONLY \
"The function can only take numerical values as its argument(s)."

#define WL_ERROR_PART_DEPTH \
"The number of part specifications is larger than the array rank."

#define WL_ERROR_REQUIRE_ARRAY \
"The function requires its argument to be an array."

#define WL_ERROR_REQUIRE_ARRAY_RANK \
"The function requires its argument to be an array with rank "

#define WL_ERROR_REQUIRE_VARIADIC \
"The function requires the argument to be a variadic function."

#define WL_ERROR_BAD_LEVEL \
"The function is not called with appropriate level specifications."

#define WL_ERROR_LARGE_RANK \
"The rank of the array produced is too large; the maximum rank is 16."

#define WL_ERROR_ZERO_RANK \
"The rank of the array produced is 0; the minimum rank is 1."

#define WL_ERROR_LARGE_ARGC \
"The number of arguments is too large; the maximum count is 16."

#define WL_ERROR_ARRAY_VALUE_TYPE \
"Only numerical values and strings can be stored in an array."

#define WL_ERROR_APPEND_RANK \
"The argument to be appended or prepended does not have an appropriate rank."

#define WL_ERROR_JOIN_RANK \
"The arrays to be joined should have the same rank."

#define WL_ERROR_JOIN_VALUE_TYPE \
"The values to be concatenated should have consistent types."

#define WL_ERROR_ITERATOR_TYPE \
"The bounds of an iterator can only be integers and real numbers."

#define WL_ERROR_BAD_RETURN \
"The specified return type of the values is invalid."

#define WL_ERROR_DIMENSIONS_SPEC \
"The specified dimensions should all be integers and non-empty."

#define WL_ERROR_ARRAY_RESHAPE_PAD_TYPE \
"The type of padding value is not consistent with the type of array."

#define WL_ERROR_POSITION_RANK \
"The pattern should have a smaller rank than the array."

#define WL_ERROR_RANDOM_BOUNDS \
"The bounds of the random numbers do not have appropriate types."

#define WL_ERROR_BRANCH_RETURN \
"All branches should have the same return type."

#define WL_ERROR_SUM_ELEMENT \
"The elements of accumulation should be of numerical types."

#define WL_ERROR_MAP_THREAD_LEVEL \
"The level specified should be less than the rank of all arrays."

#define WL_ERROR_NEST_TYPE \
"The type should be consistent when the function is applied repeatedly."

#define WL_ERROR_COUNTING_ARG \
"The argument indicating a count should be an integer."

#define WL_ERROR_INDEXER_LIST \
"The specification should be a list of integers."

#define WL_ERROR_INDEXER_SCALAR \
"The specification should be All or an integer."

#define WL_ERROR_REAL_TYPE_ARG \
"The arguments should be integers or real numbers."

#define WL_ERROR_INTEGRAL_TYPE_ARG \
"The arguments should be integers."

#define WL_ERROR_BOOLEAN_ARG \
"The arguments should be logical values."

#define WL_ERROR_BAD_COMPARE \
"The values cannot be compared with each other."

#define WL_ERROR_BAD_CAST \
"The requested type conversion is not valid."

#define WL_ERROR_LOOP_TEST \
"The test statement should evaluates to a logical value."

#define WL_ERROR_INSERT_RANK \
"The element to be inserted should have a rank less than the array by one."

#define WL_ERROR_INSERT_POS_FORM \
"The position should be an integer or a rank-2 array of integers."

#define WL_ERROR_DELETE_CASES_LEVEL \
"DeleteCases can only operate on the first level."

#define WL_ERROR_RANDOM_WEIGHTS_TYPE \
"The weights should form a list of integers or real numbers."

#define WL_ERROR_UNIFORM_BOUNDS_SPEC \
"The arguments should be an integer, a pair of number, or a list of pairs."

#define WL_ERROR_DIST_PARAMETER_COUNT \
"The distribution is not specified with a correct number of parameters."

// runtime error messages

#define WL_ERROR_CALLBACK \
"Callback failed."

#define WL_ERROR_NEGATIVE_DIMS \
"The dimension specifications should be non-negative integers."

#define WL_ERROR_LIST_ELEM_DIMS \
"All elements of a list should have the same dimensions."

#define WL_ERROR_ARITHMETIC_DIMS \
"Both arguments of the arithmetic operation should have the same dimensions."

#define WL_ERROR_OPERAND_DIMS \
"All of the arguments of the function should have the same dimensions."

#define WL_ERROR_REQUIRE_NON_EMPTY \
"The argument should be a non-empty list."

#define WL_ERROR_ITERATOR_ZERO_STEP \
"The size of the step should not be zero."

#define WL_ERROR_OUT_OF_RANGE \
"The index is out of range."

#define WL_ERROR_SPAN_OUT_OF_RANGE \
"Some of the indices are out of range."

#define WL_ERROR_INTEGER_DIGITS_NEGATIVE \
"The argument should be non-negative."

#define WL_ERROR_CLIP_LIMIT_SIZE \
"The upper and lower limits should be specified as a list of two elements."

#define WL_ERROR_SUM_ZERO_SIZE \
"The sum of zero elements is undefined for non-arithmetic scalars."

#define WL_ERROR_PRODUCT_ZERO_SIZE \
"The product of zero elements is undefined for non-arithmetic scalars."

#define WL_ERROR_TRANSPOSE_COLLAPSE \
"Collapsing dimensions should have equal lengths."

#define WL_ERROR_ORDERING_OUT_OF_RANGE \
"The requested number of indices is larger than the size of the list."

#define WL_ERROR_INSERT_ELEM_DIMS \
"The element to be inserted should have the same dimensions as the others."

#define WL_ERROR_ARGPACK_OUT_OF_RANGE \
"The slot index is larger than the number of arguments."

#define WL_ERROR_EMPTY_PACK \
"The function cannot be called with zero arguments."

#define WL_ERROR_ITERATION_NEGATIVE \
"The number of iterations should be non-negative integer."

#define WL_ERROR_INSERT_POS_DIMS \
"The each position specification should be a list of one integer."

#define WL_ERROR_RANDOM_WEIGHTS_LENGTH \
"The number of weights should match that of elements and more than zero."

#define WL_ERROR_RANDOM_ELEM_LENGTH \
"The number of elements should be more than zero."

#define WL_ERROR_NEGATIVE_WEIGHT \
"The weights should be non-negative."

#define WL_ERROR_DIST_PARAMETER_DOMAIN \
"The distribution is not specified with appropriate parameter values."

#define WL_ERROR_RANDOM_SAMPLE_NO_ELEM \
"There are not enough elements in the list to sample from."

#define WL_ERROR_RANDOM_SAMPLE_ZERO_WEIGHTS \
"The total weights of the elements are effectively zero."

}
