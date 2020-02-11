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
#    define __SSE4_2__ 1
#    define __SSE4_1__ 1
#    define __SSE3__ 1
#    define __SSE2__ 1
#    define __SSE__ 1
#  endif
#  define _wl_popcnt64 __popcnt64
#  define NOMINMAX // disable min, max macros
#  define WL_FUNCSIG __FUNCSIG__
#  define WL_LIKELY(x) x
#  define WL_UNLIKELY(x) x
#elif defined(__INTEL_COMPILER)
#  if __INTEL_COMPILER < 1900
#    pragma message ("error: cxx::compilerver")
#  endif
#  define WL_INLINE __forceinline
#  define WL_IGNORE_DEPENDENCIES __pragma(ivdep)
#  define WL_RESTRICT __restrict
#  define WL_FUNCSIG __PRETTY_FUNCTION__
#  define WL_LIKELY(x) __builtin_expect(!!(x), 1)
#  define WL_UNLIKELY(x) __builtin_expect(!!(x), 0)
#  pragma warning (disable:1011)
#  pragma warning (disable:650)
#elif defined(__clang__)
#  if __clang_major__ < 5
#    pragma message ("error: cxx::compilerver")
#  endif
#  define WL_INLINE __attribute__((always_inline)) inline
#  define WL_IGNORE_DEPENDENCIES _Pragma("ivdep")
#  define WL_RESTRICT __restrict__
#  define WL_FUNCSIG __PRETTY_FUNCTION__
#  define WL_LIKELY(x) __builtin_expect(!!(x), 1)
#  define WL_UNLIKELY(x) __builtin_expect(!!(x), 0)
#  if defined(__APPLE__) // Apple Clang
#    define WL_DISABLE_SPECIAL_FUNCTIONS
#  endif
#elif defined(__GNUC__)
#  if __GNUC__ < 7
#    pragma message ("error: cxx::compilerver")
#  endif
#  define WL_INLINE __attribute__((always_inline)) inline
#  define WL_IGNORE_DEPENDENCIES _Pragma("ivdep")
#  define WL_RESTRICT __restrict__
#  define WL_FUNCSIG __PRETTY_FUNCTION__
#  define WL_LIKELY(x) __builtin_expect(!!(x), 1)
#  define WL_UNLIKELY(x) __builtin_expect(!!(x), 0)
#  pragma GCC diagnostic ignored "-Wignored-attributes"
#  pragma GCC diagnostic ignored "-Wpsabi"
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

#define WL_BASE_RANDOM_ENGINE std::mt19937_64

#define WL_CHECK_ABORT_PERIOD 100 // milliseconds
#define WL_CHECK_ABORT_LENGTH 1024


// static assertion error messages

#define WL_ERROR_INTERNAL \
"An internal error is encountered."

#define WL_ERROR_SIZEOF_CHAR \
"The size of char should be 1 byte."

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

#define WL_ERROR_STRING_ONLY \
"The function can only take strings as its arguments."

#define WL_ERROR_STRING_TYPE_ONLY \
"The function can only take a string or a list of strings."

#define WL_ERROR_FILE_STREAM_ONLY \
"The function can only take a file stream."

#define WL_ERROR_FILE_STREAM_OR_PATH_ONLY \
"The function can only take a file stream or the path to a file."

#define WL_ERROR_PART_DEPTH \
"The number of part specifications is larger than the array rank."

#define WL_ERROR_PART_SPEC_INTEGRAL \
"Part specifications should have integral types."

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

#define WL_ERROR_TAKE_SPEC_TYPE \
"The specification should be All, an integer, or a list of integers."

#define WL_ERROR_PARTITION_LEVEL \
"The level of partition should be between one and the rank of the array."

#define WL_ERROR_PARTITION_SPEC \
"The lengths and offsets of partition should be integers or lists of integers."

#define WL_ERROR_TO_MANY_REGEX_GROUPS \
"The number of groups needed by the string expression exceeds the limit(99)."

#define WL_ERROR_REPEATED_SPEC \
"The number of repetitions should be an integer or a pair of integers."

#define WL_ERROR_STRING_FUNCTION_STRING \
"The first argument to the function should be a string or a list of strings."

#define WL_ERROR_NOT_A_PATTERN \
"The expression does not evaluate to a valid string pattern."

#define WL_ERROR_NOT_A_REPLACEMENT \
"The expression does not evaluate to a valid string pattern replacement."

#define WL_ERROR_STRING_EXCEPT \
"Except only takes a set of characters as its argument in string expressions."

#define WL_ERROR_CHARACTER_RANGE \
"The arguments should be non-negative integers or single character strings."

#define WL_ERROR_FROM_CHARACTER_CODE_ARG \
"The argument should be an integer or a list of integers."

#define WL_ERROR_STRING_RIFFLE_STRINGS \
"The function only takes an array of strings as its first argument."

#define WL_ERROR_STRING_RIFFLE_DELIMITERS \
"The number of delimiters must match the rank of the string array."

#define WL_ERROR_STRING_RIFFLE_DELIMITER_TYPES \
"A delimiter should be a string or a list of three strings."

#define WL_ERROR_TO_STRING_UNKNOWN \
"The value cannot be converted to a string."

#define WL_ERROR_STREAM_SEEK_ON_OUTPUT \
"The stream position can only be changed for input streams."

#define WL_ERROR_STREAM_POSITION_INTEGRAL \
"The stream position should be an integer."

#define WL_ERROR_READ_UNKNOWN_TYPE \
"Read type should be one of Byte, Character, String, Word, Real, or Integer."

#define WL_ERROR_WRITE_STRING_ONLY \
"The function can only write strings."

#define WL_ERROR_READ_LIST_COUNT_INTEGRAL \
"The number of elements to read should be an integer."

#define WL_ERROR_BINARY_READ_WRITE_UNKNOWN_TYPE \
"This type is not supported for binary read/write."

#define WL_ERROR_IMPORT_UNKNOWN_TYPE \
"This type is not supported for import."

#define WL_ERROR_IMPORT_PADDING_TYPE \
"The padding value cannot be converted into import data type."

#define WL_ERROR_EXPORT_ARRAY_RANK \
"The array for export should have a rank not greater than two."

#define WL_ERROR_EXPORT_TYPE \
"Only arithmetic types are supported for export."

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

#define WL_ERROR_TAKE_SPEC_LIST_LENGTH \
"The list as a specification should have between one and three integers."

#define WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH \
"Only lists with 1 or 2 integers are supported as StringTake specifications."

#define WL_ERROR_STRING_DROP_SPEC_LIST_LENGTH \
"Only lists with 1 or 2 integers are supported as StringDrop specifications."

#define WL_ERROR_PAUSE_NEGATIVE \
"The duration should be non-negative."

#define WL_ERROR_PARTITION_SPEC_LENGTH \
"The lengths and offsets of partition should match the level specification."

#define WL_ERROR_PARTITION_NEGATIVE_SPEC \
"The lengths and offsets of partition should be positive."

#define WL_ERROR_PARTITION_DEFAULT_LEVEL \
"Lengths and offsets should match array rank when the level is not specified."

#define WL_ERROR_NEST_WHILE_OFFSET \
"There are not sufficient results from NestWhile for the specified offset."

#define WL_ERROR_NEST_WHILE_LIST_OFFSET \
"There are not sufficient results from NestWhileList for the specified offset."

#define WL_ERROR_FACTORIAL_NEGATIVE \
"Factorial of a negative integer equals infinity."

#define WL_ERROR_FACTORIAL2_DOMAIN \
"Factorial2 of the argument cannot be represented by an integer."

#define WL_ERROR_BAD_UTF8_CODEPOINT \
"The string contains an invalid UTF-8 codepoint."

#define WL_ERROR_BAD_UTF8_NULL_TERMINATED \
"The string is not terminated by a null character."

#define WL_ERROR_REGEX \
"Regex is not valid: "

#define WL_ERROR_BAD_PATTERN \
"The expression does not specify a valid pattern or a pattern rule."

#define WL_ERROR_REPEATED_INVALID_SPEC \
"The specification of repetition should be non-negative and in order."

#define WL_ERROR_INVALID_CODEPOINT \
"A valid codepoint should be an non-negative integer less than 1114112."

#define WL_ERROR_CANNOT_OPEN_FILE \
"An error has occurred while openning the file."

#define WL_ERROR_STREAM_SEEK_FAILED \
"The requested stream position is outside the range of the file."

#define WL_ERROR_STREAM_ALREADY_CLOSED \
"The stream cannot be closed because it does not associate to any resource."

#define WL_ERROR_INVALID_UTF8_STRING \
"The content is not a valid UTF-8 string."

#define WL_ERROR_STREAM_CANNOT_READ_NUMBER \
"Cannot read the number from the stream."

#define WL_ERROR_STREAM_CANNOT_READ_BINARY \
"Cannot read the binary data from the stream."

#define WL_ERROR_GETCWD \
"An error has occurred while getting the current working directory."

#define WL_ERROR_STREAM_READ \
"An error has occurred while reading from the stream."

#define WL_ERROR_STREAM_WRITE \
"An error has occurred while writing to the stream."

#define WL_ERROR_STREAM_BINARINESS \
"The read/write operation does not match the binariness of the stream."

#define WL_ERROR_IMPORT_NO_PADDING \
"A padding element is required while reading an irregular table."

#define WL_ERROR_NON_ASCII_PATH \
"MinGW compiler does not support non-ascii characters in filenames."

}
