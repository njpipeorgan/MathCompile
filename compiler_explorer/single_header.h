#include <immintrin.h>
#include <cstddef>
#include <cstdint>
#include <string>
#include <tuple>
#include <algorithm>
#include <complex>
#include <type_traits>
#include <cassert>
#include <array>
#include <initializer_list>
#include <numeric>
#include <variant>
#include <vector>
#include <exception>
#include <iterator>
#include <optional>
#include <limits>
#include <cstring>
#include <chrono>
#include <memory>
#include <thread>
#include <cmath>
#include <random>
#include <filesystem>
#include <iostream>
#include <limits.h>
#include <stdlib.h>
#include <inttypes.h>
#include <functional>
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
}
namespace wl
{
template<typename T, size_t R>
struct ndarray;
template<typename T, size_t ArrayRank, size_t ViewRank, bool Const>
struct simple_view;
template<typename T, size_t ArrayRank, size_t ViewRank, size_t StrideRank, bool Const>
struct regular_view;
template<typename T, size_t ArrayRank, size_t ViewRank, size_t StrideRank, typename IndexersTuple, bool Const>
struct general_view;
template<typename T>
using complex = std::complex<T>;
union u8string;
using string = u8string;
struct u8string_view;
using string_view = u8string_view;
struct void_type;
struct all_type;
struct list_type;
struct boolean;
template<typename F>
struct function;
template<typename ArgIter, bool HasStride = false>
struct argument_pack;
template<typename Normal, typename Variadic>
struct variadic;
template<typename...>
constexpr auto always_false_v = false;
template<typename...>
struct undefined_type;
template<size_t...>
struct undefined_integral;
template<typename T, size_t...>
struct identity_type
{
    using type = T;
};
template<int64_t Id, typename Pattern>
struct _named_pattern;
template<int64_t Id>
struct _named_replacement;
template<typename... Head>
struct _pattern_blank;
template<typename... Head>
struct _pattern_blank_sequence;
template<typename... Head>
struct _pattern_blank_null_sequence;
template<typename... Patterns>
struct _pattern_alternatives;
template<typename Pattern>
struct _pattern_repeated;
template<typename Pattern>
struct _pattern_except;
template<typename Pattern>
struct _pattern_longest;
template<typename Pattern>
struct _pattern_shortest;
template<typename Left, typename Right>
struct _pattern_rule;
template<typename... Patterns>
struct _string_expression;
template<typename Pattern, typename Condition>
struct _condition;
template<int64_t... Ids>
struct _pattern_id_list;
namespace strexp
{
template<typename ConditionList, typename PatternIdList_>
struct compiled_pattern;
template<typename CompiledPattern, typename CompiledReplacement>
struct compiled_pattern_rule;
}
template<typename T>
constexpr auto is_integral_v = std::is_integral_v<T>;
template<typename T>
constexpr auto is_float_v = std::is_floating_point_v<T>;
template<typename T>
constexpr auto is_string_v = std::is_same_v<T, string>;
template<typename T>
constexpr auto is_string_view_v =
    std::is_same_v<T, string> || std::is_same_v<T, string_view>;
template<typename T>
constexpr auto is_boolean_v = std::is_same_v<T, boolean>;
template<typename T>
struct is_complex : std::false_type {};
template<typename T>
struct is_complex<complex<T>> : std::true_type {};
template<typename T>
constexpr auto is_complex_v = is_complex<T>::value;
template<typename T>
constexpr auto is_real_v = is_integral_v<T> || is_float_v<T>;
template<typename T>
constexpr auto is_arithmetic_v = is_real_v<T> || is_complex_v<T>;
template<typename T>
struct is_pattern : std::false_type {};
template<int64_t Id, typename Pattern>
struct is_pattern<_named_pattern<Id, Pattern>> : std::true_type {};
template<int64_t Id>
struct is_pattern<_named_replacement<Id>> : std::true_type {};
template<typename... Head>
struct is_pattern<_pattern_blank<Head...>> : std::true_type {};
template<typename... Head>
struct is_pattern<_pattern_blank_sequence<Head...>> : std::true_type {};
template<typename... Head>
struct is_pattern<_pattern_blank_null_sequence<Head...>> : std::true_type {};
template<typename... Patterns>
struct is_pattern<_pattern_alternatives<Patterns...>> : std::true_type {};
template<typename Pattern>
struct is_pattern<_pattern_repeated<Pattern>> : std::true_type {};
template<typename Pattern>
struct is_pattern<_pattern_except<Pattern>> : std::true_type {};
template<typename Pattern>
struct is_pattern<_pattern_longest<Pattern>> : std::true_type {};
template<typename Pattern>
struct is_pattern<_pattern_shortest<Pattern>> : std::true_type {};
template<typename Left, typename Right>
struct is_pattern<_pattern_rule<Left, Right>> : std::true_type {};
template<typename Pattern, typename Condition>
struct is_pattern<_condition<Pattern, Condition>> : std::true_type {};
#define WL_DEFINE_STRING_PATTERN_CONSTANT(pattern_type)         \
struct pattern_type {};                                         \
template<> struct is_pattern<pattern_type> : std::true_type {};
WL_DEFINE_STRING_PATTERN_CONSTANT(_whitespace_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_number_string_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_word_character_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_digit_character_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_hexadecimal_character_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_letter_character_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_whitespace_character_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_punctuation_character_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_word_boundary_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_start_of_line_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_end_of_line_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_start_of_string_type)
WL_DEFINE_STRING_PATTERN_CONSTANT(_end_of_string_type)
template<typename T>
constexpr auto is_pattern_v = is_pattern<T>::value;
template<typename T>
struct promote_integral
{
    using type = std::conditional_t<is_integral_v<T>, double, T>;
};
template<typename T>
using promote_integral_t = typename promote_integral<T>::type;
template<typename T, typename U>
struct _common_type_impl
{
    using type = std::conditional_t<is_float_v<T>, T, U>;
};
template<> struct _common_type_impl<double, double> { using type = double; };
template<> struct _common_type_impl<float, double> { using type = double; };
template<> struct _common_type_impl<double, float> { using type = double; };
template<> struct _common_type_impl<float, float> { using type = float; };
template<typename... Ts>
struct common_type;
template<typename T, typename U>
struct common_type<T, U>
{
    using type = std::conditional_t<
        is_float_v<T> || is_float_v<U>,
        typename _common_type_impl<T, U>::type,
        std::conditional_t<(sizeof(T) > sizeof(U)), T,
        std::conditional_t<(sizeof(U) > sizeof(T)), U,
        std::conditional_t<std::is_unsigned_v<T>, T, U
        >>>>;
};
template<typename T, typename U>
struct common_type<T, complex<U>> : common_type<complex<U>, T> {};
template<typename T, typename U>
struct common_type<complex<T>, U>
{
    using type = std::conditional_t<is_integral_v<U>,
        complex<T>, complex<typename common_type<T, U>::type>>;
};
template<typename T, typename U>
struct common_type<complex<T>, complex<U>>
{
    using type = complex<typename common_type<T, U>::type>;
};
template<typename T1, typename T2, typename T3, typename... Ts>
struct common_type<T1, T2, T3, Ts...>
{
    using type = typename common_type<
        typename common_type<T1, T2>::type, T3, Ts...>::type;
};
template<typename... Ts>
using common_type_t = typename common_type<Ts...>::type;
template<typename T> struct make_signed { using type = T; };
template<> struct make_signed<uint8_t> { using type = int8_t; };
template<> struct make_signed<uint16_t> { using type = int16_t; };
template<> struct make_signed<uint32_t> { using type = int32_t; };
template<> struct make_signed<uint64_t> { using type = int64_t; };
template<typename T>
using make_signed_t = typename make_signed<T>::type;
template<typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;
template<typename T>
struct is_array : std::false_type {};
template<typename T, size_t R>
struct is_array<ndarray<T, R>> : std::true_type {};
template<typename T>
constexpr auto is_array_v = is_array<T>::value;
template<typename T>
struct is_array_view : std::false_type {};
template<typename T, size_t A, size_t V, bool C>
struct is_array_view<simple_view<T, A, V, C>> : std::true_type {};
template<typename T, size_t A, size_t V, size_t S, bool C>
struct is_array_view<regular_view<T, A, V, S, C>> : std::true_type {};
template<typename T, size_t A, size_t V, size_t S, typename IT, bool C>
struct is_array_view<general_view<T, A, V, S, IT, C>> : std::true_type {};
template<typename T>
constexpr auto is_array_view_v = is_array_view<T>::value;
template<typename T, typename = void>
struct value_type { using type = T; };
template<typename T>
struct value_type<T, std::void_t<typename T::value_type>>
{
    using type = typename T::value_type;
};
template<typename T>
using value_type_t = typename value_type<T>::type;
template<typename T>
struct array_rank : std::integral_constant<size_t, 0u> {};
template<typename T, size_t R>
struct array_rank<ndarray<T, R>> :
    std::integral_constant<size_t, R> {};
template<typename T, size_t A, size_t V, bool C>
struct array_rank<simple_view<T, A, V, C>> :
    std::integral_constant<size_t, V> {};
template<typename T, size_t A, size_t V, size_t S, bool C>
struct array_rank<regular_view<T, A, V, S, C>> :
    std::integral_constant<size_t, V> {};
template<typename T, size_t A, size_t V, size_t S, typename IT, bool C>
struct array_rank<general_view<T, A, V, S, IT, C>> :
    std::integral_constant<size_t, V> {};
template<typename T>
constexpr auto array_rank_v = array_rank<T>::value;
template<typename T>
constexpr auto is_numerical_type_v = (array_rank_v<T> == 0u ?
    is_arithmetic_v<T> : is_arithmetic_v<value_type_t<T>>);
template<typename T>
constexpr auto is_boolean_type_v = is_boolean_v<value_type_t<T>>;
template<typename T>
constexpr auto is_string_type_v =
    is_string_v<value_type_t<T>> || is_string_view_v<T>;
template<typename T>
constexpr auto is_value_type_v = is_arithmetic_v<T> || is_array_v<T> ||
    is_array_view_v<T> || is_boolean_v<T> || is_string_view_v<T> ||
    std::is_same_v<T, void_type> || std::is_same_v<T, all_type>;
template<typename T>
struct is_movable : std::false_type {};
template<typename T, size_t R>
struct is_movable<ndarray<T, R>&&> : std::true_type {};
template<typename T>
constexpr auto is_movable_v = is_movable<T>::value;
template<typename T>
constexpr auto array_is_const_v = std::is_const_v<
    std::remove_pointer_t<decltype(std::declval<T>().data())>>;
template<typename T, typename U>
struct _is_convertible_impl
{
    static constexpr bool value =
        (is_arithmetic_v<T> && is_complex_v<U>) ||
        (is_integral_v<T> && is_arithmetic_v<U>) ||
        (is_float_v<T> && is_float_v<U>) ||
        (is_string_view_v<T> && is_string_v<U>);
};
template<typename T, typename U>
struct is_convertible
{
    static constexpr bool value =
        _is_convertible_impl<T, U>::value ||
        (is_array_v<T> && is_array_v<U> &&
            array_rank_v<T> == array_rank_v<U> &&
            _is_convertible_impl<value_type_t<T>, value_type_t<U>>::value);
};
template<typename T, typename Ret, typename... Args>
struct is_convertible<T, function<Ret(Args...)>> : std::true_type {};
template<typename T>
struct is_convertible<T, T> : std::true_type {};
template<typename T, typename U>
constexpr auto is_convertible_v = is_convertible<T, U>::value;
template<typename... Ts>
struct all_is_integral;
template<>
struct all_is_integral<> : std::true_type {};
template<typename T1, typename... Ts>
struct all_is_integral<T1, Ts...> :
    std::bool_constant<is_integral_v<T1> && all_is_integral<Ts...>::value> {};
template<typename... Ts>
constexpr auto all_is_integral_v = all_is_integral<Ts...>::value;
template<typename... Ts>
struct all_is_real;
template<>
struct all_is_real<> : std::true_type {};
template<typename T1, typename... Ts>
struct all_is_real<T1, Ts...> :
    std::bool_constant<is_real_v<T1> && all_is_real<Ts...>::value> {};
template<typename... Ts>
constexpr auto all_is_real_v = all_is_real<Ts...>::value;
template<typename F>
struct is_variadic_function : std::false_type {};
template<typename Normal, typename Variadic>
struct is_variadic_function<variadic<Normal, Variadic>> : std::true_type {};
template<>
struct is_variadic_function<list_type> : std::true_type {};
template<typename Fn>
constexpr auto is_variadic_function_v = is_variadic_function<Fn>::value;
template<typename T>
struct is_argument_pack : std::false_type {};
template<typename Iter, bool HasStride>
struct is_argument_pack<argument_pack<Iter, HasStride>> : std::true_type {};
template<typename Iter>
constexpr auto is_argument_pack_v = is_argument_pack<Iter>::value;
}
namespace wl
{
struct void_type
{
};
struct all_type
{
};
struct none_type
{
};
struct any_type
{
};
struct string_type
{
};
struct integer_type
{
};
struct real_type
{
};
struct complex_type
{
    template<typename... Args>
    auto operator()(Args&&... args) const;
};
struct list_type
{
    template<typename... Args>
    auto operator()(Args&&... args) const;
};
struct varg_tag
{
};
template<int64_t>
struct level_tag
{
};
struct loop_break
{
};
struct dim_checked
{
};
struct complement_span_tag
{
};
struct no_check_abort_tag
{
};
struct boolean
{
    bool val_ = false;
    constexpr explicit boolean(bool val) : val_{val}
    {
    }
    constexpr boolean() = default;
    constexpr boolean operator&&(boolean other) const
    {
        return boolean(this->val_ && other.val_);
    }
    constexpr boolean operator||(boolean other) const
    {
        return boolean(this->val_ || other.val_);
    }
    constexpr boolean operator^ (boolean other) const
    {
        return boolean(this->val_ ^ other.val_);
    }
    constexpr boolean operator!() const
    {
        return boolean(!this->val_);
    }
    constexpr operator bool() const
    {
        return this->val_;
    }
};
template<int64_t I>
struct const_int
{
    static constexpr auto value = I;
};
template<int64_t... Is>
struct const_ints
{
    template<size_t N>
    static constexpr auto value = 
        std::tuple_element_t<N, 
        std::tuple<std::integral_constant<int64_t, Is>...>>::value;
};
}
namespace wl
{
constexpr auto const_null    = void_type{};
constexpr auto const_all     = all_type{};
constexpr auto const_none    = none_type{};
constexpr auto const_real    = real_type{};
constexpr auto const_integer = integer_type{};
constexpr auto const_complex = complex_type{};
constexpr auto const_string  = string_type{};
constexpr auto const_list    = list_type{};
constexpr auto const_i       = complex<double>(0.f, 1.f);
constexpr auto const_true    = boolean(true);
constexpr auto const_false   = boolean(false);
constexpr auto const_whitespace            = _whitespace_type{};
constexpr auto const_number_string         = _number_string_type{};
constexpr auto const_word_character        = _word_character_type{};
constexpr auto const_digit_character       = _digit_character_type{};
constexpr auto const_hexadecimal_character = _hexadecimal_character_type{};
constexpr auto const_letter_character      = _letter_character_type{};
constexpr auto const_whitespace_character  = _whitespace_character_type{};
constexpr auto const_punctuation_character = _punctuation_character_type{};
constexpr auto const_word_boundary         = _word_boundary_type{};
constexpr auto const_start_of_line         = _start_of_line_type{};
constexpr auto const_end_of_line           = _end_of_line_type{};
constexpr auto const_start_of_string       = _start_of_string_type{};
constexpr auto const_end_of_string         = _end_of_string_type{};
constexpr auto const_pi           = double(3.1415926535897932385e+0);
constexpr auto const_e            = double(2.7182818284590452354e+0);
constexpr auto const_degree       = double(1.7453292519943295769e-2);
constexpr auto const_golden_ratio = double(1.6180339887498948482e+0);
constexpr auto const_golden_angle = double(2.3999632297286533222e+0);
constexpr auto const_euler_gamma  = double(5.7721566490153286061e-1);
constexpr auto const_catalan      = double(9.1596559417721901505e-1);
constexpr auto const_glaisher     = double(1.2824271291006226369e+0);
constexpr auto const_khinchin     = double(2.6854520010653064453e+0);
constexpr auto MaximumArrayRank = 16;
constexpr auto MaximumArgCount  = 16;
constexpr auto const_int_infinity  = std::numeric_limits<int64_t>::max();
constexpr auto const_real_infinity = std::numeric_limits<double>::max();
}
namespace wl
{
struct scalar_indexer; // I
struct all_indexer;    // A
struct unit_indexer;   // U
struct step_indexer;   // S
struct complement_step_indexer;
struct list_indexer;
enum class view_category
{
    Scalar, 
    Array, 
    Simple, 
    Regular, 
    General
};
namespace view_detail
{
template<typename Tuple, typename... Indexers>
struct indexers_tuple_impl;
template<typename... InTuple, typename First, typename... Rest>
struct indexers_tuple_impl<std::tuple<InTuple...>, First, Rest...>
{
    using type = std::conditional_t<
        std::is_same_v<First, scalar_indexer>,
        typename indexers_tuple_impl<std::tuple<InTuple...>, Rest...>::type,
        typename indexers_tuple_impl<std::tuple<InTuple..., First>, Rest...>::type>;
};
template<typename... InTuple>
struct indexers_tuple_impl<std::tuple<InTuple...>>
{
    using type = std::tuple<InTuple...>;
};
template<typename... Indexers>
using indexers_tuple_t = typename indexers_tuple_impl<std::tuple<>, Indexers...>::type;
template<size_t A, size_t V, size_t S>
struct _scalar_base
{
    static constexpr auto category = view_category::Scalar;
    template<typename T, bool Const, typename...>
    using return_type = std::conditional_t<Const, T, T&>;
};
template<size_t A, size_t V, size_t S>
struct _simple_base
{
    static constexpr auto category = view_category::Simple;
    template<typename T, bool Const, typename...>
    using return_type = simple_view<T, A, V, Const>;
};
template<size_t A, size_t V, size_t S>
struct _regular_base
{
    static constexpr auto category = view_category::Regular;
    template<typename T, bool Const, typename...>
    using return_type = regular_view<T, A, V, S, Const>;
};
template<size_t A, size_t V, size_t S>
struct _general_base
{
    static constexpr auto category = view_category::General;
    template<typename T, bool Const, typename... Indexers>
    using return_type = general_view<T, A, V, S, indexers_tuple_t<Indexers...>, Const>;
};
template<size_t A, size_t V, size_t S>
struct G_I_ : _general_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, G_I_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};
template<size_t A, size_t V, size_t S>
struct I_SI_ : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_SI_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};
template<size_t A, size_t V, size_t S>
struct I_S : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_SI_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};
template<size_t A, size_t V, size_t S>
struct I_A_I_ : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_A_I_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};
template<size_t A, size_t V, size_t S>
struct I_A_ : _simple_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_A_I_<A + 1, V, S + 1>,
        std::conditional_t<std::is_same_v<Idx, all_indexer>, I_A_<A + 1, V + 1, 0>,
        G_I_<A + 1, V + 1, 0>
        >>;
};
template<size_t A, size_t V, size_t S>
struct I_UI_ : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_UI_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};
template<size_t A, size_t V, size_t S>
struct I_UA_I_ : _regular_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_UA_I_<A + 1, V, S + 1>,
        G_I_<A + 1, V + 1, 0>
        >;
};
template<size_t A, size_t V, size_t S>
struct I_UA_ : _simple_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_UA_I_<A + 1, V, S + 1>,
        std::conditional_t<std::is_same_v<Idx, all_indexer>, I_UA_<A + 1, V + 1, S>,
        G_I_<A + 1, V + 1, 0>
        >>;
};
template<size_t A, size_t V, size_t S>
struct I_U : _simple_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_UI_<A + 1, V, S + 1>,
        std::conditional_t<std::is_same_v<Idx, all_indexer>, I_UA_<A + 1, V + 1, 0>,
        G_I_<A + 1, V + 1, 0>
        >>;
};
template<size_t A, size_t V, size_t S> 
struct I_ : _scalar_base<A, V, S>
{
    template<typename Idx>
    using collapse =
        std::conditional_t<std::is_same_v<Idx, scalar_indexer>, I_<A + 1, V, 0>,
        std::conditional_t<std::is_same_v<Idx, all_indexer>, I_A_<A + 1, V + 1, 0>,
        std::conditional_t<std::is_same_v<Idx, unit_indexer>, I_U<A + 1, V + 1, 0>,
        std::conditional_t<std::is_same_v<Idx, step_indexer>, I_S<A + 1, V + 1, 0>,
        G_I_<A + 1, V + 1, 0>
        >>>>;
};
template<typename State, typename... Indexers>
struct view_derive_impl;
template<typename State, typename Idx1, typename... Idxs>
struct view_derive_impl<State, Idx1, Idxs...>
{
    using type = typename view_derive_impl<typename State::template collapse<Idx1>, Idxs...>::type;
};
template<typename State>
struct view_derive_impl<State>
{
    using type = State;
};
template<typename... Indexers>
using view_type = typename view_derive_impl<I_<0u, 0u, 0u>, Indexers...>::type;
}
}
namespace wl
{
namespace simd
{
#define WL_DEFINE_DEFULAT_SIMD_UNARY(name)                                  \
WL_INLINE auto name(__m256i x) { return _mm256_##name(x); }                 \
WL_INLINE auto name(__m128i x) { return _mm_##name(x); }
#define WL_DEFINE_DEFULAT_SIMD_BINARY(name)                                 \
WL_INLINE auto name(__m256i x, __m256i y) { return _mm256_##name(x, y); }   \
WL_INLINE auto name(__m128i x, __m128i y) { return _mm_##name(x, y); }
WL_DEFINE_DEFULAT_SIMD_BINARY(unpacklo_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(unpackhi_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(add_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(add_epi16)
WL_DEFINE_DEFULAT_SIMD_BINARY(sub_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(sub_epi16)
WL_DEFINE_DEFULAT_SIMD_BINARY(cmpeq_epi8)
WL_DEFINE_DEFULAT_SIMD_BINARY(cmpgt_epi8)
WL_DEFINE_DEFULAT_SIMD_UNARY(movemask_epi8)
WL_INLINE auto testc(__m128i x, __m128i y) { return _mm_testc_si128(x, y); }
WL_INLINE auto testc(__m256i x, __m256i y) { return _mm256_testc_si256(x, y); }
template<typename M>
WL_INLINE auto zero()
{
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_setzero_si256();
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_setzero_si128();
    else
        static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}
template<typename M, typename T>
WL_INLINE auto set1(T x)
{
    if constexpr (std::is_same_v<M, __m256i>)
    {
        if constexpr (sizeof(T) == 1u) return _mm256_set1_epi8(x);
        else if constexpr (sizeof(T) == 2u) return _mm256_set1_epi16(x);
        else if constexpr (sizeof(T) == 4u) return _mm256_set1_epi32(x);
        else if constexpr (sizeof(T) == 8u) return _mm256_set1_epi64(x);
        else static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
    }
    else if constexpr (std::is_same_v<M, __m128i>)
    {
        if constexpr (sizeof(T) == 1u) return _mm_set1_epi8(x);
        else if constexpr (sizeof(T) == 2u) return _mm_set1_epi16(x);
        else if constexpr (sizeof(T) == 4u) return _mm_set1_epi32(x);
        else if constexpr (sizeof(T) == 8u) return _mm_set1_epi64(x);
        else static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
    }
    else static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}
template<typename M, typename Ptr>
WL_INLINE auto loadu(const Ptr* ptr)
{
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_loadu_si256((const __m256i*)ptr);
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_loadu_si128((const __m128i*)ptr);
    else
        static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}
template<typename M, typename Ptr>
WL_INLINE auto storeu(Ptr* ptr, M x)
{
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_storeu_si256((__m256i*)ptr, x);
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_storeu_si128((__m128i*)ptr, x);
    else
        static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}
template<int I, typename M>
WL_INLINE auto extract_epi64(M x)
{
    if constexpr (std::is_same_v<M, __m256i>)
        return _mm256_extract_epi64(x, I);
    else if constexpr (std::is_same_v<M, __m128i>)
        return _mm_extract_epi64(x, I);
    else static_assert(always_false_v<M>, WL_ERROR_INTERNAL);
}
WL_INLINE int64_t hsum_epi8(__m256i v)
{
    v = add_epi16(unpacklo_epi8(v, zero<__m256i>()),
        unpackhi_epi8(v, zero<__m256i>()));
    auto total = uint64_t(extract_epi64<0>(v) + extract_epi64<1>(v) +
        extract_epi64<2>(v) + extract_epi64<3>(v));
    total += total >> 32;
    total += total >> 16;
    return int64_t(total & 0xffffu);
}
WL_INLINE int64_t hsum_epi8(__m128i v)
{
    v = add_epi16(unpacklo_epi8(v, zero<__m128i>()),
        unpackhi_epi8(v, zero<__m128i>()));
    auto total = uint64_t(extract_epi64<0>(v) + extract_epi64<1>(v));
    total += total >> 32;
    total += total >> 16;
    return int64_t(total & 0xffffu);
}
}
}
namespace wl
{
namespace strexp
{
template<typename Expression>
auto compile(Expression);
}
template<typename Any>
auto val(Any&& any) -> decltype(auto)
{
    if constexpr (is_array_view_v<remove_cvref_t<Any>>)
        return std::forward<decltype(any)>(any).to_array();
    else if constexpr (is_pattern_v<remove_cvref_t<Any>>)
        return strexp::compile(std::forward<decltype(any)>(any));
    else
        return std::forward<decltype(any)>(any);
}
template<typename Y, typename XV, size_t XR>
auto cast(ndarray<XV, XR>&& x) -> decltype(auto)
{
    static_assert(is_convertible_v<ndarray<XV, XR>, Y>, WL_ERROR_BAD_CAST);
    using YV = value_type_t<Y>;
    if constexpr (std::is_same_v<XV, YV>)
        return std::move(x);
    else
    {
        ndarray<YV, XR> ret(x.dims());
        x.copy_to(ret.begin());
        return ret;
    }
}
template<typename Y, typename XV, size_t XR>
auto cast(const ndarray<XV, XR>& x) -> decltype(auto)
{
    static_assert(is_convertible_v<ndarray<XV, XR>, Y>, WL_ERROR_BAD_CAST);
    using YV = value_type_t<Y>;
    if constexpr (std::is_same_v<XV, YV>)
        return x;
    else
    {
        ndarray<YV, XR> ret(x.dims());
        x.copy_to(ret.begin());
        return ret;
    }
}
template<typename Y, typename X>
auto cast(const X& x)
{
    if constexpr (is_array_view_v<X>)
    {
        constexpr auto XR = array_rank_v<X>;
        using XV = value_type_t<X>;
        using YV = value_type_t<Y>;
        static_assert(is_convertible_v<ndarray<XV, XR>, Y>, WL_ERROR_BAD_CAST);
        if constexpr (std::is_same_v<XV, YV>)
            return x.to_array();
        else
        {
            ndarray<YV, XR> ret(x.dims());
            x.copy_to(ret.begin());
            return ret;
        }
    }
    else if constexpr (is_real_v<X> || is_string_view_v<X>)
    {
        static_assert(is_convertible_v<X, Y>, WL_ERROR_BAD_CAST);
        return Y(x);
    }
    else
    {
        static_assert(std::is_same_v<Y, X>, WL_ERROR_BAD_CAST);
        return x;
    }
}
template<typename Y, typename X>
auto cast(const complex<X>& x)
{
    static_assert(is_complex_v<Y>, WL_ERROR_BAD_CAST);
    return std::complex<value_type_t<Y>>(x);
}
template<typename Y>
auto cast(const boolean& x)
{
    static_assert(is_boolean_v<Y>, WL_ERROR_BAD_CAST);
    return x;
}
template<typename Y>
auto cast(const std::string& x)
{
    static_assert(is_string_v<Y>, WL_ERROR_BAD_CAST);
    return x;
}
template<typename Y>
auto cast(std::string&& x)
{
    static_assert(is_string_v<Y>, WL_ERROR_BAD_CAST);
    return std::move(x);
}
template<typename X>
auto normal(X&& x)
{
    using XT = remove_cvref_t<X>;
    if constexpr (array_rank_v<X> == 0u)
    {
        return std::forward<decltype(x)>(x);
    }
    else
    {
        using XV = value_type_t<XT>;
        constexpr auto XR = array_rank_v<XT>;
        using RV = std::conditional_t<is_integral_v<XV>, int64_t,
            std::conditional_t<is_real_v<XV>, double,
            std::conditional_t<is_complex_v<XV>, complex<double>, XV>>>;
        return cast<ndarray<RV, XR>>(std::forward<decltype(x)>(x));
    }
}
namespace utils
{
inline auto _get_time()
{
    auto now = std::chrono::high_resolution_clock::now();
    return now.time_since_epoch().count();
}
template<bool AllowEmpty = false, typename... Dims>
auto get_dims_array(const Dims&... dims)
{
    static_assert(all_is_integral_v<Dims...> && (sizeof...(Dims) >= 1u),
        WL_ERROR_DIMENSIONS_SPEC);
    if (!((dims >= Dims(0)) && ...))
        throw std::logic_error(WL_ERROR_NEGATIVE_DIMS);
    return std::array<size_t, sizeof...(Dims)>{size_t(dims)...};
}
template<size_t R1, size_t R2, size_t... Is1, size_t... Is2>
auto _dims_join_impl(
    const std::array<size_t, R1>& dims1, std::index_sequence<Is1...>,
    const std::array<size_t, R2>& dims2, std::index_sequence<Is2...>)
{
    return std::array<size_t, R1 + R2>{dims1[Is1]..., dims2[Is2]...};
}
template<size_t R1, size_t R2>
auto dims_join(const std::array<size_t, R1>& dims1,
    const std::array<size_t, R2>& dims2)
{
    return _dims_join_impl(dims1, std::make_index_sequence<R1>{},
        dims2, std::make_index_sequence<R2>{});
}
template<size_t... Is>
auto _dims_take_impl(const size_t* dims, std::index_sequence<Is...>)
{
    return std::array<size_t, sizeof...(Is)>{dims[Is]...};
}
template<size_t I1, size_t I2>
auto dims_take(const size_t* dims)
{
    static_assert(1u <= I1 && 1u <= I2, WL_ERROR_INTERNAL);
    if constexpr (I1 <= I2)
        return _dims_take_impl(dims + I1 - 1,
            std::make_index_sequence<I2 - I1 + 1>{});
    else
        return std::array<size_t, 0u>{};
}
template<size_t I1, size_t I2, size_t R>
auto dims_take(const std::array<size_t, R>& dims)
{
    if constexpr (I1 > I2)
        return std::array<size_t, 0u>{};
    else
    {
        static_assert(1u <= I1 && I1 <= R && 1u <= I2 && I2 <= R,
            WL_ERROR_INTERNAL);
        return dims_take<I1, I2>(dims.data());
    }
}
template<typename Dims, size_t... Is>
auto _size_of_dims_impl(const Dims* dims, std::index_sequence<Is...>)
{
    if constexpr (std::is_unsigned_v<Dims>)
        return (size_t(dims[Is]) * ...);
    else
    {
#if !defined(NDEBUG)
        if (!((dims[Is] >= 0) && ...))
            throw std::logic_error(WL_ERROR_INTERNAL);
#endif
        return (size_t(dims[Is]) * ...);
    }
}
template<size_t R, typename Dims>
auto size_of_dims(const Dims* dims)
{
    static_assert(is_integral_v<Dims>, WL_ERROR_INTERNAL);
    if constexpr (R == 0u)
        return size_t(1);
    else
        return _size_of_dims_impl(dims, std::make_index_sequence<R>{});
}
template<size_t R, typename Dims>
auto size_of_dims(const std::array<Dims, R>& dims)
{
    static_assert(is_integral_v<Dims>, WL_ERROR_INTERNAL);
    if constexpr (R == 0u)
        return size_t(1);
    else
        return _size_of_dims_impl(dims.data(), std::make_index_sequence<R>{});
}
template<size_t... Is>
bool check_dims_impl(const size_t* dims1, const size_t* dims2,
    std::index_sequence<Is...>)
{
    return ((dims1[Is] == dims2[Is]) && ...);
}
template<size_t R>
auto check_dims(const size_t* dims1, const size_t* dims2)
{
    if constexpr (R == 0u)
        return true;
    else
        return check_dims_impl(dims1, dims2, std::make_index_sequence<R>{});
}
template<size_t R>
auto check_dims(const std::array<size_t, R>& dims1,
    const std::array<size_t, R>& dims2)
{
    static_assert(R >= 1u, WL_ERROR_INTERNAL);
    return check_dims_impl(dims1.data(), dims2.data(),
        std::make_index_sequence<R>{});
}
template<size_t Level, size_t R, typename... Is>
void _linear_position_impl(const std::array<size_t, R>& dims, size_t& pos,
    const size_t& i1, const Is&... is)
{
    pos += i1;
    if constexpr (Level < R - 1)
    {
        pos *= dims[Level + 1u];
        _linear_position_impl<Level + 1u>(dims, pos, is...);
    }
}
template<size_t R, typename... Is>
size_t linear_position(const std::array<size_t, R>& dims, const Is&... is)
{
    static_assert(R == sizeof...(Is), WL_ERROR_INTERNAL);
    size_t pos = 0u;
    _linear_position_impl<0u>(dims, pos, is...);
    return pos;
}
template<typename X>
size_t lzcnt_u64(X x)
{
#if defined(__LZCNT__)
    return _lzcnt_u64(uint64_t(x));
#else
    int64_t n = 64;
    uint64_t y = x;
    if constexpr (sizeof(X) >= 8) if (y >> 32) { n -= 32; y >>= 32; }
    if constexpr (sizeof(X) >= 4) if (y >> 16) { n -= 16; y >>= 16; }
    if constexpr (sizeof(X) >= 2) if (y >> 8) { n -= 8; y >>= 8; }
    if (y >> 4) { n -= 4; y >>= 4; }
    if (y >> 2) { n -= 2; y >>= 2; }
    return n - ((y >> 1) ? int64_t(2) : int64_t(y));
#endif
}
template<typename X>
size_t tzcnt_u64(X x)
{
#if defined(__BMI__)
    return _tzcnt_u64(uint64_t(x));
#else
    int64_t n = 64;
    uint64_t y = x;
    if constexpr (sizeof(X) >= 8) if (y << 32) { n -= 32; y <<= 32; }
    if constexpr (sizeof(X) >= 4) if (y << 16) { n -= 16; y <<= 16; }
    if constexpr (sizeof(X) >= 2) if (y << 8) { n -= 8; y <<= 8; }
    if (y << 4) { n -= 4; y <<= 4; }
    if (y << 2) { n -= 2; y <<= 2; }
    return n - ((y << 1) ? int64_t(2) : int64_t(y >> 63));
#endif
}
template<typename X>
size_t _popcnt(X x)
{
    static_assert(std::is_unsigned_v<X>, WL_ERROR_INTERNAL);
#if defined(__POPCNT__)
    return _mm_popcnt_u64(uint64_t(x));
#else
    constexpr uint64_t m1  = 0x5555555555555555u;
    constexpr uint64_t m2  = 0x3333333333333333u;
    constexpr uint64_t m4  = 0x0f0f0f0f0f0f0f0fu;
    constexpr uint64_t h01 = 0x0101010101010101u;
    x -= (x >> 1) & m1;
    x = (x & m2) + ((x >> 2) & m2);
    x = (x + (x >> 4)) & m4;
    return size_t((x * h01) >> 56);
#endif
}
}
#if defined(WL_USE_MATHLINK)
namespace librarylink
{
template<typename String>
void send_error(const String& what) noexcept;
inline std::string get_stack_message(const std::string& err)
{
    std::string message = "error: " + err;
    return message;
}
inline std::string extract_filename(const char* file)
{
#if defined (_MSC_VER)
    const char separator = '\\';
#else
    const char separator = '/';
#endif
    const char* output = std::strrchr(file, separator);
    return output ? std::string(output + 1) : std::string(file);
}
extern volatile bool global_abort_in_progress;
extern volatile bool global_stop_check_abort;
template<typename AbortQ>
void start_check_abort(std::unique_ptr<std::thread>& thread, AbortQ* abort_q)
{
#if defined(WL_USE_MATHLINK) && defined(WL_CHECK_ABORT)
    global_stop_check_abort = false;
    global_abort_in_progress = false;
    if (thread)
        return;
    thread = std::make_unique<std::thread>([=]
        {
            for (;;)
            {
                global_abort_in_progress = bool(abort_q());
                std::this_thread::sleep_for(
                    std::chrono::milliseconds(WL_CHECK_ABORT_PERIOD));
                if (global_stop_check_abort || global_abort_in_progress)
                    return;
            }
        });
#endif
}
inline void stop_check_abort(std::unique_ptr<std::thread>& thread)
{
#if defined(WL_USE_MATHLINK) && defined(WL_CHECK_ABORT)
    global_stop_check_abort = true;
    global_abort_in_progress = false;
    if (thread)
        thread.release()->join();
#endif
}
}
#endif
#if defined(WL_USE_MATHLINK) && !defined(NDEBUG)
#  define WL_TRY_BEGIN()                                            \
    try                                                             \
    {
#  define WL_TRY_END(func, file, line)                              \
    } catch (std::logic_error& err)                                 \
    {                                                               \
        throw std::logic_error(err.what() +                         \
            (std::string("\n>> from function \"") + func +          \
            "\" in \"" + wl::librarylink::extract_filename(file) +  \
            "\" line " + std::to_string(line)));                    \
    }
#else
#  define WL_TRY_BEGIN() ((void)0);
#  define WL_TRY_END(...) ((void)0);
#endif
#if defined(WL_USE_MATHLINK) && (defined(WL_CHECK_ABORT) || defined(WL_CHECK_ABORT_TEST))
#  define WL_THROW_IF_ABORT()                                           \
    {                                                                   \
        if (WL_UNLIKELY(wl::librarylink::global_abort_in_progress))     \
            throw std::logic_error("AbortQ is called.");                \
    }
#  define WL_CHECK_ABORT_LOOP_BEGIN(n)                                  \
    const int64_t _loop_total_size = int64_t(n);                        \
    for (int64_t _loop_begin = 0;                                       \
        _loop_begin < _loop_total_size;                                 \
        _loop_begin += WL_CHECK_ABORT_LENGTH)                           \
    {                                                                   \
        constexpr int64_t _loop_zero = int64_t(0);                      \
        const int64_t _loop_size =                                      \
            _loop_begin + WL_CHECK_ABORT_LENGTH > _loop_total_size ?    \
            _loop_total_size - _loop_begin : WL_CHECK_ABORT_LENGTH;     \
        const int64_t _loop_end = _loop_begin + _loop_size;
#  define WL_CHECK_ABORT_LOOP_END()                                     \
        WL_THROW_IF_ABORT()                                             \
    }
#else
#  define WL_THROW_IF_ABORT() ((void)0);
#  define WL_CHECK_ABORT_LOOP_BEGIN(n)                                  \
    {                                                                   \
        constexpr int64_t _loop_zero = int64_t(0);                      \
        const int64_t _loop_size = int64_t(n);                          \
        const int64_t _loop_begin = 0;                                  \
        const int64_t _loop_end = _loop_size;
#  define WL_CHECK_ABORT_LOOP_END()                                     \
    }
#endif
namespace utils
{
template<typename XIter, typename YIter>
WL_INLINE void restrict_copy_n(XIter x_iter, const size_t n, YIter y_iter)
{
    using XV = remove_cvref_t<decltype(*x_iter)>;
    using YV = remove_cvref_t<decltype(*y_iter)>;
    constexpr bool use_memcpy =
        std::is_same_v<XV, YV> && std::is_trivially_copyable_v<XV> &&
        std::is_pointer_v<XIter> && std::is_pointer_v<YIter>;
    if constexpr (use_memcpy)
    {
        std::memcpy((void*)y_iter, (void*)x_iter, n * sizeof(XV));
        WL_THROW_IF_ABORT()
    }
    else
    {
        WL_CHECK_ABORT_LOOP_BEGIN(n)
            WL_IGNORE_DEPENDENCIES
            for (auto i = _loop_zero; i < _loop_size; ++i, ++x_iter, ++y_iter)
                *y_iter = *x_iter;
        WL_CHECK_ABORT_LOOP_END()
    }
}
template<typename XIter, typename YIter>
WL_INLINE void restrict_copy_n(XIter x_iter, const size_t n, YIter y_iter,
    no_check_abort_tag)
{
    using XV = remove_cvref_t<decltype(*x_iter)>;
    using YV = remove_cvref_t<decltype(*y_iter)>;
    constexpr bool use_memcpy =
        std::is_same_v<XV, YV> && std::is_trivially_copyable_v<XV> &&
        std::is_pointer_v<XIter> && std::is_pointer_v<YIter>;
    if constexpr (use_memcpy)
    {
        std::memcpy((void*)y_iter, (void*)x_iter, n * sizeof(XV));
    }
    else
    {
        const auto size = int64_t(n);
        for (int64_t i = 0; i < size; ++i, ++x_iter, ++y_iter)
            *y_iter = *x_iter;
    }
}
}
template<typename X>
auto pause(const X& x)
{
    constexpr uint64_t giga = 1'000'000'000;
    constexpr uint64_t max_sec = std::numeric_limits<uint64_t>::max() / giga;
    static_assert(is_real_v<X>, WL_ERROR_REAL_TYPE_ARG);
    if (x == X(0))
        return const_null;
    else if (x < X(0))
        throw std::logic_error(WL_ERROR_PAUSE_NEGATIVE);
    uint64_t nanosec = 0;
    if constexpr (is_integral_v<X>)
        nanosec = giga * std::min(uint64_t(x), max_sec);
    else
        nanosec = uint64_t(std::min(double(x), double(giga * max_sec)));
    constexpr uint64_t check_abort_nanosec =
        (WL_CHECK_ABORT_PERIOD * (giga / 1000u));
    while (nanosec > check_abort_nanosec)
    {
        WL_THROW_IF_ABORT()
        std::this_thread::sleep_for(
            std::chrono::nanoseconds(check_abort_nanosec));
        nanosec -= check_abort_nanosec;
    }
    WL_THROW_IF_ABORT()
    std::this_thread::sleep_for(std::chrono::nanoseconds(nanosec));
    return const_null;
}
}
namespace wl
{
template<bool UnitStep>
struct indexer_iter;
template<>
struct indexer_iter<false>
{
    size_t index_;
    ptrdiff_t step_;
    indexer_iter(size_t index, ptrdiff_t step) :
        index_{index}, step_{step}
    {
    }
    auto& operator++()
    {
        index_ += step_;
        return *this;
    }
    bool operator==(const indexer_iter& other) const
    {
        return this->index_ == other.index_;
    }
};
template<>
struct indexer_iter<true>
{
    size_t index_;
    explicit indexer_iter(size_t index) :
        index_{index}
    {
    }
    auto& operator++()
    {
        ++index_;
        return *this;
    }
    bool operator==(const indexer_iter& other) const
    {
        return this->index_ == other.index_;
    }
};
struct cidx
{
    const size_t value_;
    constexpr explicit cidx(int64_t value) : value_{size_t(value)}
    {
    }
    constexpr size_t value() const
    {
        return value_;
    }
};
template<typename IndexType>
size_t convert_index(const IndexType& idx, const size_t& dim)
{
    WL_TRY_BEGIN()
    if constexpr (std::is_same_v<IndexType, cidx>)
    {
        if (idx.value() < dim)
            return idx.value();
        else
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
    }
    else if constexpr (std::is_unsigned_v<IndexType>)
    {
        if (1u <= idx && idx <= dim)
            return size_t(idx - 1u);
        else
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
    }
    else
    {
        static_assert(is_integral_v<IndexType>, WL_ERROR_PART_SPEC_INTEGRAL);
        ptrdiff_t pos_idx = idx >= 0 ?
            idx : idx + ptrdiff_t(dim) + 1;
        if (1 <= pos_idx && pos_idx <= ptrdiff_t(dim))
            return size_t(pos_idx - 1u);
        else
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
    }
    return 0u;
    WL_TRY_END(__func__, __FILE__, __LINE__)
};
struct scalar_indexer
{
    size_t index_;
    scalar_indexer() = default;
    template<typename IndexType, typename Dim>
    scalar_indexer(IndexType index, const Dim& dim) :
        index_{convert_index(index, dim)}
    {
    }
    size_t offset() const
    {
        return this->index_;
    }
    size_t size() = delete;
    ptrdiff_t stride() = delete;
};
struct all_indexer
{
    size_t size_;
    all_indexer() = default;
    explicit all_indexer(size_t dim) :
        size_{dim}
    {
    }
    size_t offset() const
    {
        return 0u;
    }
    size_t size() const
    {
        return this->size_;
    }
    ptrdiff_t stride() const
    {
        return 1;
    }
};
struct unit_indexer
{
    size_t begin_;
    size_t size_;
    unit_indexer() = default;
    unit_indexer(size_t begin, size_t size) :
        begin_{begin}, size_{size}
    {
    }
    size_t offset() const
    {
        return this->begin_;
    }
    size_t size() const
    {
        return this->size_;
    }
    ptrdiff_t stride() const
    {
        return 1;
    }
    auto begin() const
    {
        return indexer_iter<true>(begin_);
    }
    auto end() const
    {
        return indexer_iter<true>(begin_ + size_);
    }
};
struct step_indexer
{
    size_t begin_;
    size_t size_;
    ptrdiff_t step_;
    step_indexer() = default;
    step_indexer(size_t begin, size_t size, ptrdiff_t step) :
        begin_{begin}, size_{size}, step_{step}
    {
    }
    size_t offset() const
    {
        return this->begin_;
    }
    size_t size() const
    {
        return this->size_;
    }
    ptrdiff_t stride() const
    {
        return step_;
    }
    auto begin() const
    {
        return indexer_iter<false>(begin_, step_);
    }
    auto end() const
    {
        return indexer_iter<false>(begin_ + step_ * size_, step_);
    }
};
struct complement_step_indexer_iterator
{
    size_t next_;
    size_t step_;
    size_t remain_;
    size_t current_;
    complement_step_indexer_iterator(size_t begin, size_t size, size_t step) :
        next_{begin}, remain_{size}, step_{step}, current_{0}
    {
        seek_if_invalid();
    }
    auto operator*() const
    {
        return current_;
    }
    void seek_if_invalid()
    {
        if (current_ == next_)
        {
            if (step_ == 1u)
            {
                current_ += remain_;
                remain_ = 0u;
            }
            else
            {
                ++current_;
                --remain_;
                if (remain_ > 0u)
                    next_ += step_;
            }
        }
    }
    auto& operator++()
    {
        ++current_;
        seek_if_invalid();
        return *this;
    }    
};
struct complement_step_indexer
{
    size_t begin_;
    size_t size_;
    size_t step_;
    size_t dim_;
    complement_step_indexer() = default;
    complement_step_indexer(
        size_t begin, size_t size, size_t step, size_t dim) :
        begin_{begin}, size_{size}, step_{step}, dim_{dim}
    {
    }
    size_t offset() const
    {
        return 0u;
    }
    ptrdiff_t stride() = delete;
    size_t size() const
    {
        return dim_ - size_;
    }
    auto iterator() const
    {
        return complement_step_indexer_iterator(begin_, size_, step_);
    }
};
struct list_indexer
{
    std::vector<size_t> indices_;
    list_indexer() = default;
    template<typename Iter>
    list_indexer(Iter idx_begin, Iter idx_end, size_t level_dim)
    {
        indices_.resize(idx_end - idx_begin);
        std::transform(idx_begin, idx_end, indices_.begin(),
            [&](const auto& idx) { return convert_index(idx, level_dim); });
    }
    size_t offset() const
    {
        return 0u;
    }
    ptrdiff_t stride() = delete;
    size_t size() const
    {
        return indices_.size();
    }
    const auto& indices() const
    {
        return this->indices_;
    }
};
template<typename Begin, typename End, typename Step>
struct span
{
    static constexpr auto default_begin = std::is_same_v<Begin, all_type>;
    static constexpr auto default_end = std::is_same_v<End, all_type>;
    static constexpr auto default_step = std::is_same_v<Step, all_type>;
    static_assert((default_begin || is_integral_v<Begin>) &&
        (default_end || is_integral_v<End>) &&
        (default_step || is_integral_v<Step>), WL_ERROR_INTEGRAL_TYPE_ARG);
    Begin begin_;
    End end_;
    Step step_;
    span(Begin begin, End end, Step step) :
        begin_{begin}, end_{end}, step_{step}
    {
    }
    auto to_indexer(size_t dim) const
    {
        if constexpr (default_begin && default_end && default_step)
            return all_indexer(dim);
        else
        {
            // convert WL-style index to C-style index
            ptrdiff_t begin = 1;
            ptrdiff_t end = ptrdiff_t(dim);
            ptrdiff_t step = 1;
            if constexpr (!default_begin)
            {
                if constexpr (std::is_unsigned_v<Begin>)
                    begin = ptrdiff_t(this->begin_);
                else if (this->begin_ >= 0)
                    begin = ptrdiff_t(this->begin_);
                else // this->begin_ < 0
                    begin = ptrdiff_t(this->begin_) + ptrdiff_t(dim + 1u);
                //check out-of-range
                if (begin < 1 || begin > ptrdiff_t(dim))
                    throw std::logic_error(WL_ERROR_SPAN_OUT_OF_RANGE);
            }
            if constexpr (!default_end)
            {
                if constexpr (std::is_unsigned_v<End>)
                    end = ptrdiff_t(this->end_);
                else if (this->end_ >= 0)
                    end = ptrdiff_t(this->end_);
                else // this->end_ < 0
                    end = ptrdiff_t(this->end_) + ptrdiff_t(dim + 1u);
                //check out-of-range
                if (end < 1 || end > ptrdiff_t(dim))
                    throw std::logic_error(WL_ERROR_SPAN_OUT_OF_RANGE);
            }
            if constexpr (!default_step)
            {
                step = step_;
                if (step == 0)
                    throw std::logic_error(WL_ERROR_ITERATOR_ZERO_STEP);
                else if (step < 0)
                {
                    if constexpr (default_begin)
                        begin = ptrdiff_t(dim);
                    if constexpr (default_end)
                        end = 1;
                }
            }
            if constexpr (default_step)
            {
                if constexpr (default_begin || default_end)
                {
                    return unit_indexer(
                        size_t(begin - 1),
                        size_t(end - begin + 1));
                }
                else
                {
                    return unit_indexer(
                        size_t(begin - 1),
                        end >= begin ? size_t(end - begin + 1) : 0u);
                }
            }
            else if (step > 0)
            {
                return step_indexer(
                    size_t(begin - 1),
                    end >= begin ? size_t((end - begin) / step) + 1u : 0u,
                    step);
            }
            else // step < 0
            {
                return step_indexer(
                    size_t(begin - 1),
                    end <= begin ? size_t((begin - end) / -step) + 1u : 0u,
                    step);
            }
        }
    }
};
template<typename Begin, typename End, typename Step>
struct complement_span
{
    using normal_span = span<Begin, End, Step>;
    normal_span normal_span_;
    complement_span(Begin begin, End end, Step step) :
        normal_span_(begin, end, step)
    {
    }
    auto to_indexer(size_t dim) const
    {
        const auto normal_indexer = normal_span_.to_indexer(dim);
        const auto begin = normal_indexer.offset();
        const auto size = normal_indexer.size();
        const auto step = normal_indexer.stride();
        if (step > 0)
            return complement_step_indexer(begin, size, size_t(step), dim);
        else
            return complement_step_indexer(
                begin + step * (size - 1u), size, size_t(-step), dim);
    }
};
template<typename Begin, typename End, typename Step>
auto make_span(const Begin& begin, const End& end, const Step& step)
{
    return span<Begin, End, Step>(begin, end, step);
}
template<typename Begin, typename End>
auto make_span(const Begin& begin, const End& end)
{
    return make_span(begin, end, const_all);
}
template<typename Begin, typename End, typename Step>
auto make_span(complement_span_tag,
    const Begin& begin, const End& end, const Step& step)
{
    return complement_span<Begin, End, Step>(begin, end, step);
}
template<typename Begin, typename End>
auto make_span(complement_span_tag, const Begin& begin, const End& end)
{
    return make_span(complement_span_tag{}, begin, end, const_all);
}
template<typename Begin, typename End, typename Step>
auto make_indexer(const span<Begin, End, Step>& s, size_t dim)
{
    WL_TRY_BEGIN()
    return s.to_indexer(dim);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Begin, typename End, typename Step>
auto make_indexer(const complement_span<Begin, End, Step>& s, size_t dim)
{
    WL_TRY_BEGIN()
    return s.to_indexer(dim);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename IndexType, size_t Rank>
auto make_indexer(const ndarray<IndexType, Rank>& list, size_t dim)
{
    WL_TRY_BEGIN()
    static_assert(Rank == 1u && is_integral_v<IndexType>,
        WL_ERROR_INDEXER_LIST);
    return list_indexer(list.begin(), list.end(), dim);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename IndexType>
auto make_indexer(const IndexType& index, size_t dim)
{
    WL_TRY_BEGIN()
    if constexpr (std::is_same_v<IndexType, all_type>)
        return all_indexer(dim);
    else if constexpr (std::is_same_v<IndexType, cidx>)
        return scalar_indexer(index, dim);
    else
    {
        static_assert(is_integral_v<IndexType>, WL_ERROR_INDEXER_SCALAR);
        return scalar_indexer(index, dim);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename T, size_t ArrayRank, size_t ViewRank, bool Const>
struct simple_view
{
    static constexpr auto is_const = Const;
    static constexpr auto array_rank = ArrayRank;
    static constexpr auto view_rank = ViewRank;
    static constexpr auto rank = ViewRank;
    static constexpr auto category = view_category::Simple;
    using value_type = T;
    using array_ref_type = std::conditional_t<Const,
        const ndarray<T, ArrayRank>&, ndarray<T, ArrayRank>&>;
    using pointer_type = std::conditional_t<Const, const T*, T*>;
    using _base_dims_t = std::array<size_t, ArrayRank>;
    using _dims_t = std::array<size_t, ViewRank>;
    const void* const identifier_;
    pointer_type data_;
    _dims_t dims_;
    size_t size_;
    simple_view(const void* base_id, pointer_type view_data, const size_t* dims) :
        identifier_{base_id}, data_{view_data}
    {
        std::copy_n(dims, ViewRank, this->dims_.begin());
        this->size_ = utils::size_of_dims(this->dims_);
    }
    template<typename... Specs>
    simple_view(const void* base_id, pointer_type base_data, 
        const _base_dims_t& base_dims, const Specs&... specs) :
        identifier_{base_id}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank, WL_ERROR_INTERNAL);
        this->_initialize<0u>(specs...);
        this->data_ = base_data + 
            utils::linear_position(base_dims, specs.offset()...);
    }
    template<size_t Level, typename Spec1, typename... Specs>
    void _initialize(const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level < ArrayRank - ViewRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>,
                WL_ERROR_INTERNAL);
            this->_initialize<Level + 1u>(specs...);
        }
        else
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>,
                WL_ERROR_INTERNAL);
            constexpr size_t ViewLevel = Level - (ArrayRank - ViewRank);
            this->dims_[ViewLevel] = spec1.size();
            this->size_ *= this->dims_[ViewLevel];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(specs...);
        }
    }
    auto dims() const
    {
        return this->dims_;
    }
    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }
    auto identifier() const
    {
        return this->identifier_;
    }
    auto begin() const
    {
        return this->data_;
    }
    auto data() const
    {
        return this->data_;
    }
    auto size() const
    {
        return this->size_;
    }
    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        if constexpr (std::is_same_v<bool, decltype(f(*data_, *iters...))>)
        {
            bool continue_flag = true;
            for (size_t i = 0u; i < this->size_ && continue_flag; ++i)
                continue_flag = !f(this->data_[i], iters[i]...);
        }
        else
        {
            for (size_t i = 0u; i < this->size_; ++i)
                f(this->data_[i], iters[i]...);
        }
    }
    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        for (size_t i = 0u; i < this->size_; ++i)
            iter[i] = this->data_[i];
    }
    template<typename FwdIter>
    void copy_from(FwdIter iter) const
    {
        for (size_t i = 0u; i < this->size_; ++i)
            this->data_[i] = iter[i];
    }
    auto to_array() const
    {
        return ndarray<T, ViewRank>(this->dims_,
            this->begin(), this->begin() + this->size());
    }
    void apply_pointer_offset(ptrdiff_t diff)
    {
        this->data_ += diff;
    }
    simple_view& operator++()
    {
        this->apply_pointer_offset(ptrdiff_t(this->size_));
        return *this;
    }
    simple_view& operator--()
    {
        this->apply_pointer_offset(-ptrdiff_t(this->size_));
        return *this;
    }
    simple_view& operator+=(ptrdiff_t diff)
    {
        this->apply_pointer_offset(ptrdiff_t(this->size_) * diff);
        return *this;
    }
    simple_view& operator-=(ptrdiff_t diff)
    {
        return *this += (-diff);
    }
    simple_view operator+(ptrdiff_t diff) const
    {
        auto copy = *this;
        copy += diff;
        return copy;
    }
    simple_view operator-(ptrdiff_t diff) const
    {
        auto copy = *this;
        copy -= diff;
        return copy;
    }
    const simple_view& operator*() const
    {
        return *this;
    }
    bool operator==(const simple_view& other) const
    {
        return this->data_ == other.data_;
    }
    bool operator!=(const simple_view& other) const
    {
        return !(*this == other);
    }
};
template<typename T, bool Const>
struct regular_view_iterator
{
    using _my_type = regular_view_iterator;
    using iterator_category = std::forward_iterator_tag;
    using value_type = T;
    using difference_type = ptrdiff_t;
    using pointer = std::conditional_t<Const, const T*, T*>;
    using reference = std::conditional_t<Const, const T&, T&>;
    pointer pointer_;
    ptrdiff_t stride_;
    regular_view_iterator(pointer pointer, ptrdiff_t stride) :
        pointer_{pointer}, stride_{stride}
    {
    }
    reference operator*() const
    {
        return *this->pointer_;
    }
    auto operator++()
    {
        this->pointer_ += this->stride_;
        return *this;
    }
    auto operator++(int)
    {
        auto old_pointer = this->pointer_;
        this->pointer_ += this->stride_;
        return _my_type(old_pointer, this->stride_);
    }
    auto operator==(const _my_type& other) const
    {
        return this->pointer_ == other.pointer_;
    }
    auto operator!=(const _my_type& other) const
    {
        return !(*this == other);
    }
    auto operator+(ptrdiff_t diff) const
    {
        return _my_type(pointer_ + diff * stride_, stride_);
    }
    auto& operator[](ptrdiff_t diff) const
    {
        return pointer_ + diff * stride_;
    }
    auto operator-(const _my_type& other) const
    {
        return ptrdiff_t(this->pointer_ - other.pointer_) / this->stride_;
    }
};
template<typename T, size_t ArrayRank, size_t ViewRank, size_t StrideRank, bool Const>
struct regular_view
{
    static constexpr auto is_const = Const;
    static constexpr auto array_rank = ArrayRank;
    static constexpr auto view_rank = ViewRank;
    static constexpr auto stride_rank = StrideRank;
    static constexpr auto rank = ViewRank;
    static constexpr auto category = view_category::Regular;
    using value_type = T;
    using array_ref_type = std::conditional_t<Const,
        const ndarray<T, ArrayRank>&, ndarray<T, ArrayRank>&>;
    using pointer_type = std::conditional_t<Const, const T*, T*>;
    using _base_dims_t = std::array<size_t, ArrayRank>;
    using _dims_t = std::array<size_t, ViewRank>;
    const void* const identifier_;
    pointer_type data_;
    ptrdiff_t stride_;
    _dims_t dims_;
    size_t size_;
    template<typename... Specs>
    regular_view(const void* base_id, pointer_type base_data,
        const _base_dims_t& base_dims, const Specs&... specs) :
        identifier_{base_id}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank, WL_ERROR_INTERNAL);
        this->stride_ = 1;
        this->_initialize<0u>(base_dims, specs...);
        this->data_ = base_data +
            utils::linear_position(base_dims, specs.offset()...);
    }
    template<size_t Level, typename Spec1, typename... Specs>
    auto _initialize(const _base_dims_t& dims,
        const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level < ArrayRank - ViewRank - StrideRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>,
                WL_ERROR_INTERNAL);
            this->_initialize<Level + 1>(dims, specs...);
        }
        else if constexpr (Level < ArrayRank - StrideRank)
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>,
                WL_ERROR_INTERNAL);
            constexpr size_t ViewLevel = Level -
                (ArrayRank - ViewRank - StrideRank);
            this->dims_[ViewLevel] = spec1.size();
            this->size_ *= this->dims_[ViewLevel];
            if constexpr (std::is_same_v<Spec1, step_indexer>)
                this->stride_ *= spec1.stride();
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(dims, specs...);
        }
        else
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>,
                WL_ERROR_INTERNAL);
            this->stride_ *= dims[Level];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(dims, specs...);
        }
    }
    auto dims() const
    {
        return this->dims_;
    }
    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }
    auto identifier() const
    {
        return this->identifier_;
    }
    auto begin() const
    {
        return regular_view_iterator<T, Const>(this->data_, this->stride_);
    }
    auto data() const
    {
        return this->data_;
    }
    auto size() const
    {
        return this->size_;
    }
    auto stride() const
    {
        return this->stride_;
    }
    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        if constexpr (std::is_same_v<bool, decltype(f(*data_, *iters...))>)
        {
            bool continue_flag = true;
            for (size_t i = 0u; i < this->size_ && continue_flag; ++i)
                continue_flag = !f(this->data_[i * stride_], iters[i]...);
        }
        else
        {
            for (size_t i = 0u; i < this->size_; ++i)
                f(this->data_[i * stride_], iters[i]...);
        }
    }
    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        for (size_t i = 0u; i < this->size_; ++i)
            iter[i] = this->data_[i * stride_];
    }
    template<typename FwdIter>
    void copy_from(FwdIter iter) const
    {
        for (size_t i = 0u; i < this->size_; ++i)
            this->data_[i * stride_] = iter[i];
    }
    auto to_array() const
    {
        return ndarray<T, ViewRank>(this->dims_,
            this->begin(), this->begin() + this->size());
    }
};
template<typename T, size_t ArrayRank, size_t ViewRank, size_t StrideRank, typename IndexersTuple, bool Const>
struct general_view
{
    static_assert(ViewRank == std::tuple_size_v<IndexersTuple>,
        WL_ERROR_INTERNAL);
    static constexpr auto is_const = Const;
    static constexpr auto array_rank = ArrayRank;
    static constexpr auto view_rank = ViewRank;
    static constexpr auto stride_rank = StrideRank;
    static constexpr auto rank = ViewRank;
    static constexpr auto category = view_category::General;
    using value_type = T;
    using array_ref_type = std::conditional_t<Const,
        const ndarray<T, ArrayRank>&, ndarray<T, ArrayRank>&>;
    using pointer_type = std::conditional_t<Const, const T*, T*>;
    using _base_dims_t = std::array<size_t, ArrayRank>;
    using _dims_t = std::array<size_t, ViewRank>;
    using _strides_t = std::array<ptrdiff_t, ViewRank>;
    using _indexers_tuple = IndexersTuple;
    static constexpr auto _has_last_stride = (StrideRank != 0);
    const void* const identifier_;
    pointer_type data_;
    _strides_t strides_;
    _indexers_tuple indexers_;
    _dims_t dims_;
    size_t size_;
    template<typename... Specs>
    general_view(const void* base_id, pointer_type base_data,
        const _base_dims_t& base_dims, Specs&&... specs) :
        identifier_{base_id}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank,
            WL_ERROR_INTERNAL);
        this->_initialize<0u, 0u>(
            base_dims, std::forward<decltype(specs)>(specs)...);
        this->data_ = base_data +
            utils::linear_position(base_dims, specs.offset()...);
    }
    template<size_t Level, size_t ViewLevel, typename Spec1, typename... Specs>
    auto _initialize(const _base_dims_t& dims,
        Spec1&& spec1, Specs&&... specs)
    {
        if constexpr (std::is_same_v<remove_cvref_t<Spec1>, scalar_indexer>)
        {
            if constexpr (ViewLevel > 0u)
                this->strides_[ViewLevel - 1] *= dims[Level];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u, ViewLevel>(
                    dims, std::forward<decltype(specs)>(specs)...);
        }
        else
        {
            if constexpr (ViewLevel > 0u)
                this->strides_[ViewLevel - 1] *= dims[Level];
            this->strides_[ViewLevel] = 1;
            this->dims_[ViewLevel] = spec1.size();
            this->size_ *= this->dims_[ViewLevel];
            std::get<ViewLevel>(this->indexers_) = 
                std::forward<decltype(spec1)>(spec1);
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u, ViewLevel + 1u>(
                    dims, std::forward<decltype(specs)>(specs)...);
        }
    }
    auto dims() const
    {
        return this->dims_;
    }
    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }
    auto identifier() const
    {
        return this->identifier_;
    }
    auto begin() const = delete;
    auto data() const = delete;
    auto size() const
    {
        return this->size_;
    }
    template<size_t ViewLevel, typename Function, typename... Iters>
    void _for_each_impl(size_t index, bool& break_flag, 
        Function& f, Iters&... iters) const
    {
        constexpr bool check_break =
            std::is_same_v<bool, decltype(f(*data_, *iters...))>;
        const auto& indexer = std::get<ViewLevel>(this->indexers_);
        using Indexer = remove_cvref_t<decltype(indexer)>;
        if constexpr (ViewLevel > 0u)
            index *= this->strides_[ViewLevel - 1];
        if constexpr (ViewLevel < ViewRank - 1u)
        {
            if constexpr (std::is_same_v<Indexer, list_indexer>)
            {
                for (const auto& i : indexer.indices())
                {
                    _for_each_impl<ViewLevel + 1u>(
                        index + i, break_flag, f, iters...);
                    if (check_break && break_flag)
                        break;
                }
            }
            else if constexpr (
                std::is_same_v<Indexer, complement_step_indexer>)
            {
                const auto dim_size = indexer.size();
                auto index_iter = indexer.iterator();
                for (size_t i = 0; i < dim_size; ++i, ++index_iter)
                {
                    _for_each_impl<ViewLevel + 1u>(
                        index + (*index_iter), break_flag, f, iters...);
                    if (check_break && break_flag)
                        break;
                }
            }
            else if constexpr (std::is_same_v<Indexer, step_indexer>)
            {
                const auto this_stride = indexer.stride();
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                {
                    _for_each_impl<ViewLevel + 1u>(
                        index + i * this_stride, break_flag, f, iters...);
                    if (check_break && break_flag)
                        break;
                }
            }
            else
            {
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                {
                    _for_each_impl<ViewLevel + 1u>(
                        index + i, break_flag, f, iters...);
                    if (check_break && break_flag)
                        break;
                }
            }
        }
        else
        {
            const auto last_stride = _has_last_stride ? 
                this->strides_[ViewLevel] : ptrdiff_t(1);
            if constexpr (std::is_same_v<Indexer, list_indexer>)
            {
                for (const auto& i : indexer.indices())
                {
                    if constexpr (check_break)
                    {
                        if (f(this->data_[(index + i) * last_stride],
                            (*iters++)...))
                            break;
                    }
                    else
                        f(this->data_[(index + i) * last_stride],
                        (*iters++)...);
                }
            }
            else if constexpr (
                std::is_same_v<Indexer, complement_step_indexer>)
            {
                const auto dim_size = indexer.size();
                auto index_iter = indexer.iterator();
                for (size_t i = 0; i < dim_size; ++i, ++index_iter)
                {
                    const auto this_index = *index_iter;
                    if constexpr (check_break)
                    {
                        if (f(this->data_[(index + this_index) * last_stride],
                            (*iters++)...))
                            break;
                    }
                    else
                        f(this->data_[(index + this_index) * last_stride],
                        (*iters++)...);
                }
            }
            else if constexpr (std::is_same_v<Indexer, step_indexer>)
            {
                const auto this_stride = indexer.stride();
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                {
                    const auto this_i = i * this_stride;
                    if constexpr (check_break)
                    {
                        if (f(this->data_[(index + this_i) * last_stride],
                            (*iters++)...))
                            break;
                    }
                    else
                        f(this->data_[(index + this_i) * last_stride],
                        (*iters++)...);
                }
            }
            else
            {
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                {
                    if constexpr (check_break)
                    {
                        if (f(this->data_[(index + i) * last_stride],
                            (*iters++)...))
                            break;
                    }
                    else
                        f(this->data_[(index + i) * last_stride],
                        (*iters++)...);
                }
            }
        }
    }
    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        bool break_flag = false;
        this->_for_each_impl<0u>(0u, break_flag, f, iters...);
    }
    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        this->for_each([](const auto& src, auto& dst) { dst = src; }, iter);
    }
    template<typename FwdIter>
    void copy_from(FwdIter iter) const
    {
        this->for_each([](auto& dst, const auto& src) { dst = src; }, iter);
    }
    auto to_array() const
    {
        auto ret = ndarray<T, ViewRank>(this->dims_);
        this->copy_to(ret.begin());
        return ret;
    }
};
template<view_category Category, typename Any>
auto allows(Any&& any) -> decltype(auto)
{
    using AnyType = remove_cvref_t<Any>;
    if constexpr (array_rank_v<AnyType> == 0)
        return std::forward<decltype(any)>(any);
    else
    {
        constexpr auto array_filter =
            Category == view_category::Array && is_array_view_v<AnyType>;
        constexpr auto simple_filter =
            Category == view_category::Simple &&
            (AnyType::category == view_category::General ||
                AnyType::category == view_category::Regular);
        constexpr auto regular_filter =
            Category == view_category::Regular &&
            AnyType::category == view_category::General;
        if constexpr (array_filter || simple_filter || regular_filter)
            return std::forward<decltype(any)>(any).to_array();
        else
            return std::forward<decltype(any)>(any);
    }
}
template<typename T>
struct scalar_view_iterator
{
    using _my_type = scalar_view_iterator;
    using value_type = T;
    T val_;
    constexpr scalar_view_iterator(const T& val) : val_{val}
    {
    }
    constexpr auto operator++() const
    {
        return *this;
    }
    constexpr auto operator++(int) const
    {
        return *this;
    }
    auto operator==(const _my_type&) = delete;
    constexpr const T& operator[](ptrdiff_t) const
    {
        return val_;
    }
    constexpr const T& operator*() const
    {
        return val_;
    }
};
template<typename T>
constexpr auto make_scalar_view_iterator(const T& val)
{
    return scalar_view_iterator<T>(val);
}
template<typename View>
auto _data_coverage(const View& view)
{
    if constexpr (View::category == view_category::Simple)
        return std::make_pair(view.data(), view.data() + view.size() - 1u);
    else
    {
        static_assert(View::category == view_category::Regular,
            WL_ERROR_INTERNAL);
        return std::make_pair(view.data(),
            view.data() + (view.size() - 1u) * view.stride() +
            (view.stride() > 0 ? 1 : -1));
    }
}
template<typename Dst, typename Src>
auto has_aliasing(const Dst& dst, const Src& src)
{
    if constexpr (std::is_same_v<value_type_t<Dst>, value_type_t<Src>>)
    {
        if (dst.identifier() != src.identifier())
            return false;
        if constexpr (
            Dst::category == view_category::Array   ||
            Dst::category == view_category::General ||
            Src::category == view_category::Array   ||
            Src::category == view_category::General)
        {
            return true;
        }
        else
        {
            auto[src_min, src_max] = _data_coverage(src);
            auto[dst_min, dst_max] = _data_coverage(dst);
            return (src_max >= dst_min) && (src_min <= dst_max);
        }
    }
    else
        return false;
}
template<typename Dst, typename Src>
auto indirect_view_copy(Dst&& dst, const Src& src)
{
    using SrcValueType = typename Src::value_type;
    using DstValueType = typename remove_cvref_t<Dst>::value_type;
    using T = std::conditional_t<sizeof(SrcValueType) < sizeof(DstValueType),
        SrcValueType, DstValueType>;
    std::vector<T> buffer(src.size());
    src.copy_to(buffer.begin());
    dst.copy_from(buffer.begin());
}
}
namespace wl
{
#ifndef WL_SMALL_ARRAY_SIZE
#define WL_SMALL_ARRAY_SIZE 256
#endif
template<typename T, size_t N>
struct _small_vector
{
    using static_t = std::array<T, N>;
    using dynamic_t = std::vector<T>;
    union data_t
    {
        static_t static_;
        dynamic_t dynamic_;
        data_t() {}
        ~data_t() {}
        data_t(const data_t&) = delete;
        data_t(data_t&&) = delete;
        data_t& operator=(const data_t&) = delete;
        data_t& operator=(data_t&&) = delete;
    };
    bool is_static_ = true;
    size_t size_ = 0u;
    data_t data_;
    _small_vector()
    {
    }
    ~_small_vector()
    {
        this->destroy();
    }
    explicit _small_vector(size_t size) : size_{size}
    {
        bool is_static = (size_ <= N);
        if (is_static)
        {
            std::uninitialized_default_construct_n(static_begin(), size);
        }
        else
        {
            WL_THROW_IF_ABORT()
            new(&data_.dynamic_) dynamic_t(size);
        }
        this->is_static_ = is_static;
    }
    _small_vector(size_t size, const T& val) : size_{size}
    {
        bool is_static = (size_ <= N);
        if (is_static)
        {
            std::uninitialized_fill_n(static_begin(), size, val);
        }
        else
        {
            WL_THROW_IF_ABORT()
            new(&data_.dynamic_) dynamic_t(size, val);
        }
        this->is_static_ = is_static;
    }
    template<typename FwdIter>
    _small_vector(FwdIter begin, FwdIter end) : size_{size_t(end - begin)}
    {
        bool is_static = (size_ <= N);
        if (is_static)
        {
            std::uninitialized_copy_n(begin, size_, static_begin());
        }
        else
        {
            WL_THROW_IF_ABORT()
            new(&data_.dynamic_) dynamic_t(begin, end);
        }
        this->is_static_ = is_static;
    }
    explicit _small_vector(const dynamic_t& other) : size_{other.size()}
    {
        bool is_static = (size_ <= N);
        if (is_static)
        {
            std::uninitialized_copy_n(other.begin(), size_, static_begin());
        }
        else
        {
            WL_THROW_IF_ABORT()
            new(&data_.dynamic_) dynamic_t(other);
        }
        this->is_static_ = is_static;
    }
    explicit _small_vector(dynamic_t&& other) : size_{other.size()}
    {
        WL_THROW_IF_ABORT()
        new(&data_.dynamic_) dynamic_t(std::move(other));
        this->is_static_ = false;
    }
    _small_vector(const _small_vector& other) : size_{other.size_}
    {
        if (other.size_ <= N)
        {
            std::uninitialized_copy_n(other.begin(), size_, static_begin());
            this->is_static_ = true;
        }
        else
        {
            WL_THROW_IF_ABORT()
            new(&data_.dynamic_) dynamic_t(other.data_.dynamic_);
            this->is_static_ = false;
        }
    }
    _small_vector(_small_vector&& other) : size_{other.size_}
    {
        if (other.is_static_)
        {
            std::uninitialized_move_n(
                other.static_begin(), size_, static_begin());
        }
        else
        {
            WL_THROW_IF_ABORT()
            new(&data_.dynamic_) dynamic_t(std::move(other.data_.dynamic_));
        }
        this->is_static_ = other.is_static_;
    }
    _small_vector& operator=(const _small_vector& other)
    {
        this->destroy();
        if (other.size_ <= N)
        {
            std::uninitialized_copy_n(other.begin(), size_, static_begin());
            this->is_static_ = true;
        }
        else
        {
            WL_THROW_IF_ABORT()
            new(&data_.dynamic_) dynamic_t(other.data_.dynamic_);
            this->is_static_ = false;
        }
        this->size_ = other.size_;
        return *this;
    }
    _small_vector& operator=(_small_vector&& other)
    {
        this->destroy();
        if (other.is_static_)
        {
            std::uninitialized_move_n(
                other.static_begin(), other.size_, static_begin());
        }
        else
        {
            WL_THROW_IF_ABORT()
            new(&data_.dynamic_) dynamic_t(std::move(other.data_.dynamic_));
        }
        this->is_static_ = other.is_static_;
        this->size_ = other.size_;
        return *this;
    }
    WL_INLINE void destroy_static()
    {
        std::destroy(static_begin(), static_end());
    }
    WL_INLINE void destroy_dynamic()
    {
        WL_THROW_IF_ABORT()
        data_.dynamic_.~dynamic_t();
    }
    WL_INLINE void destroy()
    {
        if (is_static_)
        {
            destroy_static();
        }
        else
        {
            destroy_dynamic();
        }
    }
    size_t size() const
    {
        return size_;
    }
    bool is_static() const
    {
        return is_static_;
    }
    const T* data() const
    {
        return is_static_ ? static_begin() : dynamic_begin();
    }
    T* data()
    {
        return is_static_ ? static_begin() : dynamic_begin();
    }
    T* static_begin() { return data_.static_.data(); }
    T* static_end() { return static_begin() + size_; }
    const T* static_begin() const { return data_.static_.data(); }
    const T* static_end() const { return static_begin() + size_; }
    T* dynamic_begin() { return data_.dynamic_.data(); }
    T* dynamic_end() { return dynamic_begin() + size_; }
    const T* dynamic_begin() const { return data_.dynamic_.data(); }
    const T* dynamic_end() const { return dynamic_begin() + size_; }
    T* begin() { return data(); }
    T* end() { return data() + size_; }
    const T* begin() const { return data(); }
    const T* end() const { return data() + size_; }
    void resize(size_t new_size)
    {
        const auto prev_size = size_;
        if (!is_static_)
        {
            WL_THROW_IF_ABORT()
            data_.dynamic_.resize(new_size);
            if (new_size < prev_size)
                data_.dynamic_.shrink_to_fit();
        }
        else if (new_size > N)
        {
            WL_THROW_IF_ABORT()
            dynamic_t new_data(new_size);
            std::move(static_begin(), static_end(), new_data.data());
            std::destroy(static_begin(), static_end());
            new(&data_.dynamic_) dynamic_t(std::move(new_data));
            is_static_ = false;
        }
        else
        {
            const auto diff_size = ptrdiff_t(new_size - prev_size);
            if (diff_size > 0)
                std::uninitialized_default_construct_n(
                    static_begin() + prev_size, size_t(diff_size));
            else if (diff_size < 0)
                std::destroy_n(static_begin() + new_size, size_t(-diff_size));
        }
        size_ = new_size;
    }
    const void* identifier() const
    {
        return reinterpret_cast<const void*>(this);
    }
};
template<typename T, size_t R>
struct ndarray
{
    static_assert(1u <= R, WL_ERROR_ZERO_RANK);
    static_assert(R <= MaximumArrayRank, WL_ERROR_LARGE_RANK);
    static_assert(std::is_same_v<T, remove_cvref_t<T>>, WL_ERROR_INTERNAL);
    static_assert(is_arithmetic_v<T> || is_boolean_v<T> || is_string_v<T>,
        WL_ERROR_ARRAY_VALUE_TYPE);
    using value_type = T;
    static constexpr auto rank = R;
    using _dims_t = std::array<size_t, R>;
    static constexpr auto category = view_category::Array;
    static constexpr auto small_size = size_t(WL_SMALL_ARRAY_SIZE) / sizeof(T);
    using _data_t = _small_vector<T, small_size>;
    _dims_t dims_;
    _data_t data_;
    ndarray() : dims_{0u}, data_{}
    {
    }
    template<typename DimsT>
    ndarray(const std::array<DimsT, R>& dims, const T& val) :
        data_(utils::size_of_dims(dims), val)
    {
        std::copy(dims.begin(), dims.end(), this->dims_.data());
    }
    template<typename DimsT>
    ndarray(const std::array<DimsT, R>& dims) :
        data_(utils::size_of_dims(dims))
    {
        std::copy(dims.begin(), dims.end(), this->dims_.data());
    }
    template<typename FwdIter>
    ndarray(std::array<size_t, R> dims, FwdIter begin, FwdIter end) :
        dims_{dims}, data_(begin, end)
    {
    }
    template<typename U>
    ndarray(std::array<size_t, R> dims, std::initializer_list<U> data) :
        dims_{dims}, data_(data.begin(), data.end())
    {
    }
    ndarray(std::array<size_t, R> dims, _data_t&& movable) :
        dims_{dims}, data_(std::move(movable))
    {
    }
    ndarray(std::array<size_t, R> dims, std::vector<T>&& movable) :
        dims_{dims}, data_(std::move(movable))
    {
    }
    size_t size() const
    {
        return data_.size();
    }
    template<size_t Level>
    size_t partial_size() const
    {
        static_assert(Level <= R, WL_ERROR_INTERNAL);
        return utils::size_of_dims<R - Level>(this->dims_.data());
    }
    // uses one-based indexing
    template<size_t Level>
    size_t dimension() const
    {
        static_assert(1 <= Level && Level <= R, WL_ERROR_INTERNAL);
        return this->dims_[Level - 1];
    }
    const auto& dims() const
    {
        return this->dims_;
    }
    const size_t* dims_ptr() const
    {
        return this->dims_.data();
    }
    const T* data() const
    {
        return this->data_.data();
    }
    T* data()
    {
        return this->data_.data();
    }
    auto is_static() const
    {
        return this->data_.is_static();
    }
    auto data_vector() const & -> const auto&
    {
        return this->data_;
    }
    auto data_vector() & -> auto&
    {
        return this->data_;
    }
    auto data_vector() && -> auto&&
    {
        return std::move(this->data_);
    }
    const void* identifier() const
    {
        return this->data_.identifier();
    }
    auto begin() { return this->data_.begin(); }
    auto end() { return this->data_.end(); }
    auto begin() const { return this->data_.begin(); }
    auto end() const { return this->data_.end(); }
    template<size_t Level>
    auto view_begin()
    {
        static_assert(Level <= R, WL_ERROR_INTERNAL);
        if constexpr (Level == R)
            return this->begin();
        else
            return simple_view<T, R, R - Level, false>(
                this->identifier(), this->data(), this->dims_ptr() + Level);
    }
    template<size_t Level>
    auto view_end()
    {
        if constexpr (Level == R)
            return this->end();
        else
        {
            auto iter = this->template view_begin<Level>();
            iter.apply_pointer_offset(this->size());
            return iter;
        }
    }
    template<size_t Level>
    auto view_begin() const
    {
        static_assert(Level <= R, WL_ERROR_INTERNAL);
        if constexpr (Level == R)
            return this->begin();
        else
            return simple_view<T, R, R - Level, true>(
                this->identifier(), this->data(), this->dims_ptr() + Level);
    }
    template<size_t Level>
    auto view_end() const
    {
        if constexpr (Level == R)
            return this->end();
        else
        {
            auto iter = this->template view_begin<Level>();
            iter.apply_pointer_offset(this->size());
            return iter;
        }
    }
    void uninitialized_resize(const _dims_t& new_dims, size_t new_size)
    {
        assert(utils::size_of_dims(new_dims) == new_size);
        this->dims_ = new_dims;
        this->data_.resize(new_size);
    }
    void uninitialized_resize(const _dims_t& new_dims)
    {
        this->dims_ = new_dims;
        this->data_.resize(utils::size_of_dims(this->dims_));
    }
    template<typename X>
    void _append_scalar(X&& x)
    {
        size_t prev_size = this->size();
        this->data_.resize(prev_size + 1u);
        *(this->data_.data() + prev_size) = std::forward<decltype(x)>(x);
        ++this->dims_[0];
    }
    template<typename X>
    void _append_array(X&& x)
    {
        if (this->size() == 0u)
        {
            using XV = value_type_t<remove_cvref_t<X>>;
            this->dims_ = utils::dims_join(
                std::array<size_t, 1u>{1u}, x.dims());
            if constexpr (is_movable_v<X&&> && std::is_same_v<XV, T>)
                this->data_ = std::move(std::move(x).data_vector());
            else
            {
                this->data_.resize(x.size());
                x.copy_to(this->data_.data());
            }
        }
        else
        {
            size_t prev_size = this->size();
            this->data_.resize(prev_size + x.size());
            auto dst_ptr = this->data_.data() + prev_size;
            x.copy_to(dst_ptr);
            ++this->dims_[0];
        }
    }
    template<typename X>
    void append(X&& x, dim_checked)
    {
        if constexpr (R > 1u)
            _append_array(std::forward<decltype(x)>(x));
        else
            _append_scalar(std::forward<decltype(x)>(x));
    }
    template<typename X>
    void append(X&& x)
    {
        static_assert(array_rank_v<remove_cvref_t<X>> + 1u == R,
            WL_ERROR_APPEND_RANK);
        if constexpr (R > 1u)
        {
            if (this->size() > 0u && !utils::check_dims<R - 1u>(
                this->dims_ptr() + 1, x.dims().data()))
                throw std::logic_error(WL_ERROR_INTERNAL);
        }
        this->append(std::forward<decltype(x)>(x), dim_checked{});
    }
    template<typename... Is>
    size_t linear_position(const Is&... is) const
    {
        return utils::linear_position(this->dims_, is...);
    }
    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters)
    {
        auto ptr = this->data();
        const auto size = this->size();
        if constexpr (std::is_same_v<bool, decltype(f(*ptr, *iters...))>)
        {
            bool continue_flag = true;
            for (size_t i = 0u; i < size && continue_flag; ++i)
                continue_flag = !f(ptr[i], iters[i]...);
        }
        else
        {
            for (size_t i = 0u; i < size; ++i)
                f(ptr[i], iters[i]...);
        }
    }
    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        auto ptr = this->data();
        const auto size = this->size();
        if constexpr (std::is_same_v<bool, decltype(f(*ptr, *iters...))>)
        {
            bool continue_flag = true;
            for (size_t i = 0u; i < size && continue_flag; ++i)
                continue_flag = !f(ptr[i], iters[i]...);
        }
        else
        {
            for (size_t i = 0u; i < size; ++i)
                f(ptr[i], iters[i]...);
        }
    }
    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        auto ptr = this->data();
        const auto size = this->size();
        for (size_t i = 0u; i < size; ++i)
            iter[i] = ptr[i];
    }
    template<typename FwdIter>
    void copy_from(FwdIter iter) &
    {
        auto ptr = this->data();
        const auto size = this->size();
        for (size_t i = 0u; i < size; ++i)
            ptr[i] = iter[i];
    }
    template<typename FwdIter>
    void copy_from(FwdIter) && = delete;
    auto to_array() const & -> decltype(auto)
    {
        return *this;
    }
    auto to_array() && -> decltype(auto)
    {
        return std::move(*this);
    }
};
}
namespace wl
{
struct _returns_function_tag {};
struct _returns_value_tag {};
template<typename... Ts>
struct _branch_type_check_impl;
template<typename T1, typename... Ts>
struct _branch_type_check_impl<T1, Ts...>
{
    static constexpr auto value = is_value_type_v<T1> ?
        std::conjunction_v<std::is_same<T1, Ts>...> :
        std::conjunction_v<std::bool_constant<!is_value_type_v<Ts>>...>;
};
template<typename... Ts>
struct _branch_type_check : _branch_type_check_impl<remove_cvref_t<Ts>...> {};
template<typename T1, typename...>
struct _branch_returns_value :
    std::bool_constant<is_value_type_v<remove_cvref_t<T1>>> {};
template<typename T, typename...>
struct _get_first_type
{
    using type = T;
};
template<typename... Ts>
using _get_first_type_t = typename _get_first_type<Ts...>::type;
template<typename A, typename B>
WL_INLINE auto branch_if(const boolean cond, A&& a, B&& b)
{
    WL_TRY_BEGIN()
    using AT = decltype(val(std::declval<A&&>()()));
    using BT = decltype(val(std::declval<B&&>()()));
    static_assert(_branch_type_check<AT, BT>::value, WL_ERROR_BRANCH_RETURN);
    if constexpr (_branch_returns_value<AT>::value)
    {
        return cond ? val(std::forward<decltype(a)>(a)()) :
            val(std::forward<decltype(b)>(b)());
    }
    else
    {
        return
            [cond,
            a = std::forward<decltype(a)>(a)(),
            b = std::forward<decltype(b)>(b)()
            ](auto&&... args)
        {
            auto ra = [&, a] {
                return a(std::forward<decltype(args)>(args)...); };
            auto rb = [&, b] {
                return b(std::forward<decltype(args)>(args)...); };
            return branch_if(cond, std::move(ra), std::move(rb));
        };
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename... Conds>
auto _which_conditions(Conds&&... conds)
{
    static_assert(std::conjunction_v<std::is_same<
        remove_cvref_t<decltype(conds())>, boolean>...>,
        WL_ERROR_BRANCH_RETURN);
    size_t n = 0u;
    [[maybe_unused]] auto _1 = ((conds() ? true : (++n, false)) || ...);
    return n;
}
template<typename Ret, size_t... Is, typename... Cases>
auto _which_impl(const size_t n, _returns_value_tag,
    std::index_sequence<Is...>, Cases&&... cases)
{
    if constexpr (std::is_same_v<Ret, void_type>)
    {
        [[maybe_unused]] auto _1 = ((n == Is ?
            (std::forward<decltype(cases)>(cases)(), true) : false
            ) || ...);
        return const_null;
    }
    else
    {
        Ret ret;
        if (n >= sizeof...(Cases))
            throw std::logic_error(WL_ERROR_INTERNAL);
        [[maybe_unused]] auto _1 = ((n == Is ?
            (ret = val(std::forward<decltype(cases)>(cases)()), true) : false
            ) || ...);
        return ret;
    }
}
template<size_t... Is, typename... Cases>
auto _which_impl(const size_t n, _returns_function_tag,
    std::index_sequence<Is...>, Cases&&... cases)
{
    return
        [n,
        cases = std::make_tuple(std::forward<decltype(cases)>(cases)()...)
        ](auto&&... args)
    {
        return _which_impl(n, std::get<Is>(cases)...);
    };
}
template<typename... Cases>
auto which(const size_t n, Cases&&... cases)
{
    WL_TRY_BEGIN()
    static_assert(_branch_type_check<
        decltype(val(std::declval<Cases&&>()()))...>::value,
        WL_ERROR_BRANCH_RETURN);
    using FirstType =
        decltype(val(std::declval<_get_first_type_t<Cases&&...>>()()));
    if constexpr (_branch_returns_value<FirstType>::value)
    {
        return _which_impl<remove_cvref_t<FirstType>>(
            n, _returns_value_tag{},
            std::make_index_sequence<sizeof...(Cases)>{},
            std::forward<decltype(cases)>(cases)...);
    }
    else
    {
        return _which_impl(
            n, _returns_function_tag{},
            std::make_index_sequence<sizeof...(Cases)>{},
            std::forward<decltype(cases)>(cases)...);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Test, typename Incr, typename Body>
auto loop_for(Test test, Incr incr, Body body)
{
    WL_TRY_BEGIN()
    static_assert(is_boolean_v<remove_cvref_t<decltype(test())>>,
        WL_ERROR_LOOP_TEST);
    try
    {
        for (;;)
        {
            for (int64_t i = 0; i < WL_CHECK_ABORT_LENGTH; ++i)
            {
                if (!test())
                    return const_null;
                body();
                incr();
            }
            WL_THROW_IF_ABORT()
        }
    }
    catch (const loop_break&)
    {
    }
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Test, typename Body>
auto loop_while(Test test, Body body)
{
    WL_TRY_BEGIN()
    static_assert(is_boolean_v<remove_cvref_t<decltype(test())>>,
        WL_ERROR_LOOP_TEST);
    try
    {
        for (;;)
        {
            for (int64_t i = 0; i < WL_CHECK_ABORT_LENGTH; ++i)
            {
                if (!test())
                    return const_null;
                body();
            }
            WL_THROW_IF_ABORT()
        }
    }
    catch (const loop_break&)
    {
    }
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
namespace utils
{
template<typename Fn, typename X>
auto listable_function(Fn fn, X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto x_rank = array_rank_v<XT>;
    if constexpr (x_rank == 0)
    {
        return fn(x);
    }
    else
    {
        using XV = typename XT::value_type;
        using RV = decltype(fn(XV{}));
        if constexpr (is_movable_v<X&&> && std::is_same_v<RV, XV>)
        {
            x.for_each([=](auto& a) { a = fn(a); });
            return std::move(x);
        }
        else
        {
            ndarray<RV, x_rank> ret(x.dims());
            x.for_each([=](const auto& a, auto& r) { r = fn(a); },
                ret.begin());
            return ret;
        }
    }
}
template<typename Fn, typename X, typename Y>
auto listable_function(Fn fn, X&& x, Y&& y)
{
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto x_rank = array_rank_v<XT>;
    constexpr auto y_rank = array_rank_v<YT>;
    if constexpr (x_rank == 0 && y_rank == 0)
    {
        return fn(x, y);
    }
    else if constexpr (x_rank == 0 && y_rank >= 1)
    {
        using YV = typename YT::value_type;
        using RV = decltype(fn(x, YV{}));
        if constexpr (is_movable_v<Y&&> && std::is_same_v<RV, YV>)
        {
            y.for_each([=](auto& b) { b = fn(x, b); });
            return std::move(y);
        }
        else
        {
            ndarray<RV, y_rank> ret(y.dims());
            y.for_each([=](const auto& b, auto& r) { r = fn(x, b); },
                ret.begin());
            return ret;
        }
    }
    else if constexpr (x_rank >= 1 && y_rank == 0)
    {
        using XV = typename XT::value_type;
        using RV = decltype(fn(XV{}, y));
        if constexpr (is_movable_v<X&&> && std::is_same_v<RV, XV>)
        {
            x.for_each([=](auto& a) { a = fn(a, y); });
            return std::move(x);
        }
        else
        {
            ndarray<RV, x_rank> ret(x.dims());
            x.for_each([=](const auto& a, auto& r) { r = fn(a, y); },
                ret.begin());
            return ret;
        }
    }
    else
    {
        static_assert(x_rank == y_rank, WL_ERROR_OPERAND_RANK);
        if (!utils::check_dims(x.dims(), y.dims()))
            throw std::logic_error(WL_ERROR_OPERAND_DIMS);
        using XV = typename XT::value_type;
        using YV = typename YT::value_type;
        using RV = decltype(fn(XV{}, YV{}));
        if constexpr (is_movable_v<X&&> && std::is_same_v<XV, RV>)
        {
            y.for_each([=](const auto& b, auto& a) { a = fn(a, b); },
                x.begin());
            return std::move(x);
        }
        else if constexpr (XT::category == view_category::General)
        {
            if constexpr (is_movable_v<Y&&> && std::is_same_v<YV, RV>)
            {
                x.for_each([=](const auto& a, auto& b) { b = fn(a, b); },
                    y.begin());
                return std::move(y);
            }
            else if constexpr (YT::category == view_category::General)
            {
                if constexpr (std::is_same_v<XV, RV>)
                    return _listable_numerical_function(fn,
                        x.to_array(), std::forward<decltype(y)>(y));
                else
                    return _listable_numerical_function(fn,
                        std::forward<decltype(x)>(x), y.to_array());
            }
            else
            {
                ndarray<RV, x_rank> ret(x.dims());
                x.for_each([=](const auto& a, const auto& b, auto& r)
                    { r = fn(a, b); },
                    y.begin(), ret.begin());
                return ret;
            }
        }
        else
        {
            if constexpr (is_movable_v<Y&&>&& std::is_same_v<YV, RV>)
            {
                x.for_each([=](const auto& a, auto& b) { b = fn(a, b); },
                    y.begin());
                return std::move(y);
            }
            else
            {
                ndarray<RV, x_rank> ret(x.dims());
                y.for_each([=](const auto& b, const auto& a, auto& r)
                    { r = fn(a, b); },
                    x.begin(), ret.begin());
                return ret;
            }
        }
    }
}
#define WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_VARIADIC(name)          \
template<typename Iter, bool HasStride>                             \
auto _variadic_##name(const argument_pack<Iter, HasStride>& args)   \
{                                                                   \
    auto ret = val(args.get(0));                                    \
    WL_CHECK_ABORT_LOOP_BEGIN(args.size() - 1u)                     \
        for (auto i = _loop_begin; i < _loop_end; ++i)              \
            ret = name(std::move(ret),                              \
                args.get(i + 1, dim_checked{}));                    \
    WL_CHECK_ABORT_LOOP_END()                                       \
    return ret;                                                     \
}
#define WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(name)        \
if constexpr (is_argument_pack_v<remove_cvref_t<Y>>)                \
{                                                                   \
    if (y.size() == 0u)                                             \
        return std::forward<decltype(x)>(x);                        \
    else                                                            \
        return name(std::forward<decltype(x)>(x),                   \
            _variadic_##name(y));                                   \
}                                                                   \
else if constexpr (is_argument_pack_v<remove_cvref_t<X>>)           \
{                                                                   \
    if (x.size() == 0u)                                             \
        return std::forward<decltype(y)>(y);                        \
    else                                                            \
        return name(_variadic_##name(x),                            \
            std::forward<decltype(y)>(y));                          \
}                                                                   \
else
#define WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(name, expr)     \
constexpr auto name()                                               \
{                                                                   \
    return expr;                                                    \
}
#define WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(name)             \
template<typename X>                                                \
auto name(X&& x)                                                    \
{                                                                   \
    if constexpr (is_argument_pack_v<remove_cvref_t<X>>)            \
    {                                                               \
        using ArgType = remove_cvref_t<decltype(x.get(0))>;         \
        if constexpr (is_convertible_v<                             \
            remove_cvref_t<decltype(name())>,                       \
            remove_cvref_t<decltype(_variadic_##name(x))>>)         \
        {                                                           \
            if (x.size() == 0u)                                     \
                return cast<remove_cvref_t<                         \
                    decltype(_variadic_##name(x))>>(name());        \
            else                                                    \
                return _variadic_##name(x);                         \
        }                                                           \
        else                                                        \
        {                                                           \
            if (x.size() == 0u)                                     \
                throw std::logic_error(WL_ERROR_EMPTY_PACK);        \
            else                                                    \
                return _variadic_##name(x);                         \
        }                                                           \
    }                                                               \
    else                                                            \
        return std::forward<decltype(x)>(x);                        \
}
#define WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(name)              \
template<typename X1, typename X2, typename X3, typename... Xs>     \
auto name(X1&& x1, X2&& x2, X3&& x3, Xs&&... xs)                    \
{                                                                   \
    return name(name(std::forward<decltype(x1)>(x1),                \
        std::forward<decltype(x2)>(x2)),                            \
        std::forward<decltype(x3)>(x3),                             \
        std::forward<decltype(xs)>(xs)...);                         \
}
}
}
namespace wl
{
template<typename X>
auto n(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    if constexpr (!is_integral_v<XV>)
        return std::forward<decltype(x)>(x);
    else
        return utils::listable_function(
            [](const auto& a) { return double(a); },
            std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
#define WL_DEFINE_ROUNDING_FUNCTION(name, stdname)                      \
template<typename X>                                                    \
auto name(X&& x)                                                        \
{                                                                       \
    WL_TRY_BEGIN()                                                      \
    using XT = remove_cvref_t<X>;                                       \
    constexpr auto XR = array_rank_v<XT>;                               \
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);      \
    using XV = std::conditional_t<(XR == 0u), XT, value_type_t<XT>>;    \
    if constexpr (is_integral_v<XV>)                                    \
        return std::forward<decltype(x)>(x);                            \
    else                                                                \
    {                                                                   \
        auto pure = [](const auto& x)                                   \
        {                                                               \
            if constexpr (is_float_v<XV>)                               \
                return int64_t(std::stdname(x));                        \
            else                                                        \
                return complex<value_type_t<XV>>(                       \
                    std::stdname(std::real(x)),                         \
                    std::stdname(std::imag(x)));                        \
        };                                                              \
        return utils::listable_function(pure,                           \
            std::forward<decltype(x)>(x));                              \
    }                                                                   \
    WL_TRY_END(__func__, __FILE__, __LINE__)                            \
}
WL_DEFINE_ROUNDING_FUNCTION(round, round)
WL_DEFINE_ROUNDING_FUNCTION(ceiling, ceil)
WL_DEFINE_ROUNDING_FUNCTION(floor, floor)
WL_DEFINE_ROUNDING_FUNCTION(integer_part, trunc)
template<typename X>
auto fractional_part(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    auto pure = [](const auto& x)
    {
        if constexpr (is_integral_v<XV>)
            return double(0);
        else if constexpr (is_float_v<XV>)
            return x - std::trunc(x);
        else
            return XV(std::real(x) - std::trunc(std::real(x)),
                std::imag(x) - std::trunc(std::imag(x)));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto abs(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    if constexpr (std::is_unsigned_v<XV>)
        return std::forward<decltype(x)>(x);
    else
        return utils::listable_function([](auto x) { return std::abs(x); },
            std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto ramp(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    if constexpr (std::is_unsigned_v<XV>)
        return std::forward<decltype(x)>(x);
    else
    {
        auto pure = [](const auto& x)
        {
            return x >= XV(0) ? x : XV(0);
        };
        return utils::listable_function(pure, std::forward<decltype(x)>(x));
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
boolean greater(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, WL_ERROR_BAD_COMPARE);
    return boolean(x > y);
}
template<typename X, typename Y>
boolean less(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, WL_ERROR_BAD_COMPARE);
    return boolean(x < y);
}
template<typename X, typename Y>
boolean greater_equal(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, WL_ERROR_BAD_COMPARE);
    return boolean(x >= y);
}
template<typename X, typename Y>
boolean less_equal(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, WL_ERROR_BAD_COMPARE);
    return boolean(x <= y);
}
template<bool DimChecked, typename X, typename Y>
boolean _equal_impl(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR == YR, WL_ERROR_OPERAND_RANK);
    if constexpr (XR == 0u)
    {
        static_assert((is_arithmetic_v<X> && is_arithmetic_v<X>) ||
            (is_string_view_v<X> && is_string_view_v<Y>) ||
            (is_value_type_v<X> && std::is_same_v<X, Y>),
            WL_ERROR_BAD_COMPARE);
        if constexpr (is_complex_v<X>)
            return boolean(x == cast<X>(y));
        else if constexpr (is_complex_v<Y>)
            return boolean(cast<Y>(x) == y);
        else
            return boolean(x == y);
    }
    else
    {
        if constexpr (!DimChecked)
            if (!utils::check_dims(x.dims(), y.dims()))
                return const_false;
        if constexpr (X::category != view_category::General)
        {
            auto equal_flag = true;
            y.for_each([&](const auto& a, const auto& b)
                {
                    equal_flag = _equal_impl<true>(a, b);
                    return !equal_flag;
                }, x.begin());
            return boolean(equal_flag);
        }
        else if constexpr (Y::category != view_category::General)
        {
            return _equal_impl<true>(y, x);
        }
        else
        {
            if constexpr (sizeof(value_type_t<X>) < sizeof(value_type_t<Y>))
                return _equal_impl<true>(x.to_array(), y);
            else
                return _equal_impl<true>(x, y.to_array());
        }
    }
}
template<typename X, typename Y>
boolean equal(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    return _equal_impl<false>(x, y);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
boolean equal(const X& x, const Y& y, dim_checked)
{
    WL_TRY_BEGIN()
    return _equal_impl<true>(x, y);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
boolean unequal(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    return !equal(x, y);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
boolean unequal(const X& x, const Y& y, dim_checked)
{
    WL_TRY_BEGIN()
    return !equal(x, y, dim_checked{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y, typename... DimChecked>
boolean same_q(const X& x, const Y& y, DimChecked...)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    constexpr auto same_type = (XR == 0u) ? std::is_same_v<X, Y> :
        std::is_same_v<value_type_t<X>, value_type_t<Y>>;
    if constexpr (XR == YR && same_type)
        return equal(x, y, DimChecked{}...);
    else
        return const_false;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y, typename... DimChecked>
boolean unsame_q(const X& x, const Y& y, DimChecked...)
{
    WL_TRY_BEGIN()
    return !same_q(x, y, DimChecked{}...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto mod(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, WL_ERROR_REAL_TYPE_ARG);
        if constexpr (is_integral_v<XV> && is_integral_v<YV>)
        {
            using C = std::conditional_t<
                std::is_signed_v<XV> || std::is_signed_v<YV>,
                make_signed_t<common_type_t<XV, YV>>,
                common_type_t<XV, YV>>;
            if (y == 0)
                return C(0);
            else
            {
                auto xi = C(x);
                auto yi = C(y);
                auto rem = xi % yi;
                if constexpr (std::is_unsigned_v<C>)
                    return rem;
                else if (rem == C(0))
                    return rem;
                else
                    return rem + (yi & ((xi ^ yi) >> (8u * sizeof(C) - 1u)));
            }
        }
        else
        {
            using C = common_type_t<XV, YV>;
            auto cx = C(x);
            auto cy = C(y);
            return cx - std::floor(cx / cy) * cy;
        }
    };
    return utils::listable_function(pure,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto quotient(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, WL_ERROR_REAL_TYPE_ARG);
        if constexpr (is_integral_v<XV> && is_integral_v<YV>)
        {
            using C = std::conditional_t<
                std::is_signed_v<XV> || std::is_signed_v<YV>,
                make_signed_t<common_type_t<XV, YV>>,
                common_type_t<XV, YV>>;
            if (y == 0)
                return C(0);
            else
            {
                auto xi = C(x);
                auto yi = C(y);
                auto rem = xi % yi;
                auto quot = xi / yi;
                if constexpr (std::is_unsigned_v<C>)
                    return quot;
                else if (rem == C(0))
                    return quot;
                else
                    return quot + ((xi ^ yi) >> (8u * sizeof(C) - 1u));
            }
        }
        else
        {
            using C = common_type_t<XV, YV>;
            auto cx = C(x);
            auto cy = C(y);
            return std::floor(cx / cy);
        }
    };
    return utils::listable_function(pure,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto sign(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x / std::abs(x);
        else if (x == XV(0))
            return Ret(0);
        else if (x > XV(0))
            return Ret(1);
        else
            return Ret(-1);
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X, typename Y, typename N>
auto integer_digits(const X& x, const Y& y, const N& n)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<X> && is_integral_v<Y>,
        WL_ERROR_INTEGRAL_TYPE_ARG);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    constexpr auto fixed_length = !std::is_same_v<N, void_type>;
    if (y < Y(0)) throw std::logic_error(WL_ERROR_INTEGER_DIGITS_NEGATIVE);
    auto ux = (x >= X(0)) ? uint64_t(x) : uint64_t(-x);
    auto uy = uint64_t(y);
    ndarray<Ret, 1u> ret;
    if constexpr (fixed_length)
    {
        static_assert(is_integral_v<N>, WL_ERROR_COUNTING_ARG);
        if (n < N(0)) throw std::logic_error(WL_ERROR_INTEGER_DIGITS_NEGATIVE);
        ret.uninitialized_resize(std::array<size_t, 1u>{size_t(n)}, size_t(n));
        auto ret_begin = ret.data();
        auto ret_end = ret_begin + size_t(n);
        auto ret_iter = ret_begin;
        for (;;)
        {
            uint64_t rem = ux % uy;
            ux = ux / uy;
            *ret_iter = Ret(rem);
            ++ret_iter;
            if (ux == 0u)
            {
                for (; ret_iter < ret_end; ++ret_iter)
                    *ret_iter = Ret(0);
                break;
            }
        }
        std::reverse(ret_begin, ret_end);
    }
    else if (ux == 0u)
        ret.append(Ret(0));
    else
    {
        for (;;)
        {
            uint64_t rem = ux % uy;
            ux = ux / uy;
            ret.append(Ret(rem));
            if (ux == 0u)
                break;
        }
        std::reverse(ret.begin(), ret.end());
    }
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X, typename Y>
auto integer_digits(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    return integer_digits<Ret>(x, y, const_null);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto integer_digits(const X& x)
{
    WL_TRY_BEGIN()
    return integer_digits<Ret>(x, uint64_t(10), const_null);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto positive(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        return boolean(x > XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto negative(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        return boolean(x < XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto non_positive(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        return boolean(x <= XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto non_negative(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        return boolean(x >= XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename L>
auto clip(X&& x, const L& limit)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<L> == 1u, WL_ERROR_REQUIRE_ARRAY_RANK"one.");
    if (limit.size() != 2u) throw std::logic_error(WL_ERROR_CLIP_LIMIT_SIZE);
    std::array<value_type_t<L>, 2u> limit_pair;
    limit.copy_to(limit_pair.data());
    return clip(std::forward<decltype(x)>(x), varg_tag{},
        limit_pair[0], limit_pair[1]);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename L, typename VL>
auto clip(X&& x, const L& limit, const VL& vlimit)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<L> == 1u, WL_ERROR_REQUIRE_ARRAY_RANK"one.");
    static_assert(array_rank_v<VL> == 1u, WL_ERROR_REQUIRE_ARRAY_RANK"one.");
    if (limit.size() != 2u) throw std::logic_error(WL_ERROR_CLIP_LIMIT_SIZE);
    if (vlimit.size() != 2u) throw std::logic_error(WL_ERROR_CLIP_LIMIT_SIZE);
    std::array<value_type_t<L>, 2u> limit_pair;
    std::array<value_type_t<VL>, 2u> vlimit_pair;
    limit.copy_to(limit_pair.data());
    vlimit.copy_to(vlimit_pair.data());
    return clip(std::forward<decltype(x)>(x), varg_tag{},
        limit_pair[0], limit_pair[1], vlimit_pair[0], vlimit_pair[1]);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto clip(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        if (x < XV(0))
            return XV(0);
        else if (x > XV(1))
            return XV(1);
        else
            return x;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Min, typename Max>
auto clip(X&& x, varg_tag, Min min, Max max)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    static_assert(is_real_v<Min> && is_real_v<Max>, WL_ERROR_REAL_TYPE_ARG);
    using C = common_type_t<value_type<XT>, Min, Max>;
    const auto cmin = cast<C>(min);
    const auto cmax = cast<C>(max);
    auto pure = [=](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        auto cx = cast<C>(x);
        if (cx < cmin)
            return cmin;
        else if (cx > cmax)
            return cmax;
        else
            return cx;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Min, typename Max, typename VMin, typename VMax>
auto clip(X&& x, varg_tag, Min min, Max max, VMin vmin, VMax vmax)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    static_assert(is_real_v<Min> && is_real_v<Max>, WL_ERROR_REAL_TYPE_ARG);
    static_assert(is_real_v<VMin> && is_real_v<VMax>, WL_ERROR_REAL_TYPE_ARG);
    using C = common_type_t<value_type_t<XT>, Min, Max, VMin, VMax>;
    const auto cmin = cast<C>(min);
    const auto cmax = cast<C>(max);
    const auto cvmin = cast<C>(vmin);
    const auto cvmax = cast<C>(vmax);
    auto pure = [=](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        auto cx = cast<C>(x);
        if (cx < cmin)
            return cvmin;
        else if (cx > cmax)
            return cvmax;
        else
            return cx;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto unitize(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        return Ret(int8_t(x != XV(0)));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto unit_step(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        if (x >= XV(0))
            return Ret(1);
        else
            return Ret(0);
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
constexpr auto min()
{
    return const_real_infinity;
}
template<typename X>
auto min(const X& x)
{
    WL_TRY_BEGIN()
    if constexpr (is_argument_pack_v<X>)
    {
        using XV = value_type_t<value_type_t<X>>;
        const auto pack_size = x.size();
        auto ret = std::numeric_limits<XV>::max();
        WL_CHECK_ABORT_LOOP_BEGIN(x.size())
            for (auto i = _loop_begin; i < _loop_end; ++i)
                ret = std::min(ret, min(x.get(i, dim_checked{})));
        WL_CHECK_ABORT_LOOP_END()
        return ret;
    }
    else if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_real_v<X>, WL_ERROR_REAL_TYPE_ARG);
        return x;
    }
    else
    {
        using XV = value_type_t<X>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        auto ret = std::numeric_limits<XV>::max();
        x.for_each([&](const auto& a) { ret = std::min(ret, a); });
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X1, typename X2, typename... Xs>
auto min(const X1& x1, const X2& x2, const Xs&... xs)
{
    WL_TRY_BEGIN()
    using MinType1 = decltype(min(x1));
    using MinType2 = decltype(min(x2));
    using LimitType = common_type_t<MinType1, MinType2, uint64_t>;
    constexpr auto signed1 =
        std::is_signed_v<MinType1> && std::is_unsigned_v<MinType2>;
    constexpr auto signed2 =
        std::is_signed_v<MinType2> && std::is_unsigned_v<MinType1>;
    constexpr auto high1 = LimitType(std::numeric_limits<MinType1>::max());
    constexpr auto high2 = LimitType(std::numeric_limits<MinType2>::max());
    using RT =
        std::conditional_t<signed1, MinType1,
        std::conditional_t<signed2, MinType2,
        std::conditional_t<(high1 > high2), MinType1, MinType2>>>;
    if constexpr (signed1)
    {
        auto min1 = min(x1);
        if (min1 <= MinType1(0))
            return min(RT(min1), xs...);
        else
        {
            auto min2 = min(x2);
            if (LimitType(min2) > LimitType(high1))
                return min(RT(min1), xs...);
            else
                return min((RT(min1) < RT(min2)) ? RT(min1) : RT(min2), xs...);
        }
    }
    else if constexpr (signed2)
        return min(x2, x1, xs...);
    else
    {
        auto min1 = min(x1);
        auto min2 = min(x2);
        return min((RT(min1) < RT(min2)) ? RT(min1) : RT(min2), xs...);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
constexpr auto max()
{
    return -const_real_infinity;
}
template<typename X>
auto max(const X& x)
{
    WL_TRY_BEGIN()
    if constexpr (is_argument_pack_v<X>)
    {
        using XV = value_type_t<value_type_t<X>>;
        auto ret = std::numeric_limits<XV>::min();
        WL_CHECK_ABORT_LOOP_BEGIN(x.size())
            for (auto i = _loop_begin; i < _loop_end; ++i)
                ret = std::max(ret, max(x.get(i, dim_checked{})));
        WL_CHECK_ABORT_LOOP_END()
        return ret;
    }
    else if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_real_v<X>, WL_ERROR_REAL_TYPE_ARG);
        return x;
    }
    else
    {
        using XV = value_type_t<X>;
        static_assert(is_real_v<XV>, WL_ERROR_REAL_TYPE_ARG);
        auto ret = std::numeric_limits<XV>::min();
        x.for_each([&](const auto& a) { ret = std::max(ret, a); });
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X1, typename X2, typename... Xs>
auto max(const X1& x1, const X2& x2, const Xs&... xs)
{
    WL_TRY_BEGIN()
    using MaxType1 = decltype(max(x1));
    using MaxType2 = decltype(max(x2));
    using LimitType = common_type_t<MaxType1, MaxType2, uint64_t>;
    constexpr auto high1 = LimitType(std::numeric_limits<MaxType1>::max());
    constexpr auto high2 = LimitType(std::numeric_limits<MaxType2>::max());
    using RT = std::conditional_t<(high1 > high2), MaxType1, MaxType2>;
    if constexpr (high1 < high2)
    {
        auto max2 = max(x2);
        if (LimitType(max2) > LimitType(high1))
            return max(RT(max2), xs...);
        else
        {
            auto max1 = max(x1);
            if (std::is_signed_v<MaxType1> && max1 <= MaxType1(0))
                return max(RT(max2), xs...);
            else
                return max((RT(max1) > RT(max2)) ? RT(max1) : RT(max2), xs...);
        }
    }
    else if constexpr (high1 > high2)
        return max(x2, x1, xs...);
    else
    {
        auto max1 = max(x1);
        auto max2 = max(x2);
        return max((RT(max1) > RT(max2)) ? RT(max1) : RT(max2), xs...);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto chop(X&& x, const Y& y)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Y>, WL_ERROR_REAL_TYPE_ARG);
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    constexpr auto XR = array_rank_v<XT>;
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    if constexpr (is_integral_v<XV>)
        return std::forward<decltype(x)>(x);
    else
    {
        auto pure = [lim = cast<value_type_t<XV>>(y)](const auto& x)
        {
            if constexpr (is_real_v<XV>)
                return std::abs(x) < lim ? XV(0) : x;
            else
            {
                using T = value_type_t<XV>;
                T re = std::real(x);
                T im = std::imag(x);
                re = std::abs(re) < lim ? T(0) : re;
                im = std::abs(im) < lim ? T(0) : im;
                return complex<T>(re, im);
            }
        };
        return utils::listable_function(pure, std::forward<decltype(x)>(x));
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
template<typename T, bool HasVariable>
struct step_iterator
{
    static constexpr bool has_variable = HasVariable;
    using value_type = T;
    T begin_;
    T step_;
    size_t length_;
    step_iterator(T begin, T step, size_t length) :
        begin_{begin}, step_{step}, length_{length}
    {
    }
    size_t length() const
    {
        return length_;
    }
    auto operator[](size_t i) const
    {
        return T(begin_ + step_ * i);
    }
};
template<typename Array>
struct list_iterator
{
    static constexpr bool has_variable = true;
    using Iter = remove_cvref_t<decltype(
        std::declval<Array>().template view_begin<1u>())>;
    using value_type = decltype(*std::declval<Iter>());
    Array values_;
    Iter iter_;
    list_iterator(Array&& values) :
        values_{std::move(values)}, iter_{values_.template view_begin<1u>()}
    {
    }
    list_iterator(const Array& values) :
        values_{values}, iter_{values_.template view_begin<1u>()}
    {
    }
    size_t length() const
    {
        return values_.dims()[0];
    }
    auto operator[](size_t i) const
    {
        return *(iter_ + i);
    }
};
template<bool HasVariable, typename Begin, typename End, typename Step>
auto make_step_iterator(Begin begin, End end, Step step)
{
    static_assert(is_real_v<Begin> && is_real_v<End> && is_real_v<Step>,
        WL_ERROR_ITERATOR_TYPE);
    using T = common_type_t<Begin, Step>;
    if (step == Step(0))
        throw std::logic_error(WL_ERROR_ITERATOR_ZERO_STEP);
    if constexpr (is_integral_v<T>)
    {
        auto diff = ptrdiff_t(wl::integer_part(end - begin));
        if ((step > Step(0) && diff < 0) || (step < Step(0) && diff > 0))
            return step_iterator<T, HasVariable>(begin, step, 0u);
        else
            return step_iterator<T, HasVariable>(begin, step,
                size_t(diff / ptrdiff_t(step)) + 1u);
    }
    else
    {
        auto diff = T(end - begin);
        if ((step > Step(0) && diff < 0) || (step < Step(0) && diff > 0))
            return step_iterator<T, HasVariable>(begin, step, 0u);
        else
        {
            size_t length = wl::integer_part(diff / step) + 1u;
            auto remain = T(begin + (length - 1u) * step) - T(end);
            if (step * remain > T(0))
                --length;
            return step_iterator<T, HasVariable>(begin, step, length);
        }
    }
}
template<typename Begin, typename End, typename Step>
auto var_iterator(Begin begin, End end, Step step)
{
    WL_TRY_BEGIN()
    return make_step_iterator<true>(begin, end, step);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Begin, typename End>
auto var_iterator(Begin begin, End end)
{
    WL_TRY_BEGIN()
    return make_step_iterator<true>(begin, end, int8_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Begin, typename End>
auto var_iterator(End end)
{
    WL_TRY_BEGIN()
    return make_step_iterator<true>(int8_t(1), end, int8_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Any>
auto var_iterator(Any&& any)
{
    WL_TRY_BEGIN()
    using Type = remove_cvref_t<Any>;
    if constexpr (array_rank_v<Type> == 0u)
    {
        if constexpr (is_integral_v<Type>)
            return make_step_iterator<true>(Type(1), any, Type(1));
        else
            return make_step_iterator<true>(int64_t(1), any, int64_t(1));
    }
    else
    {
        if constexpr (is_movable_v<Any&&>)
            return list_iterator<Type>(std::move(any));
        else
        {
            auto copy = allows<view_category::Array>(
                std::forward<decltype(any)>(any));
            return list_iterator<decltype(copy)>(std::move(copy));
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Any>
auto iterator(Any&& any)
{
    WL_TRY_BEGIN()
    return make_step_iterator<false>(int8_t(1), any, int8_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
constexpr auto _scalar_plus = [](const auto& x, const auto& y)
{
    using XT = remove_cvref_t<decltype(x)>;
    using YT = remove_cvref_t<decltype(y)>;
    using XV = value_type_t<XT>;
    using YV = value_type_t<YT>;
    using C = common_type_t<XV, YV>;
    if constexpr (is_complex_v<XT>)
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(std::real(x) + std::real(y),
                std::imag(x) + std::imag(y));
        else
            return complex<C>(std::real(x) + cast<C>(y), std::imag(x));
    }
    else
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(cast<C>(x) + std::real(y), std::imag(y));
        else
            return cast<C>(cast<C>(x) + cast<C>(y));
    }
};
constexpr auto _scalar_subtract = [](const auto& x, const auto& y)
{
    using XT = remove_cvref_t<decltype(x)>;
    using YT = remove_cvref_t<decltype(y)>;
    using XV = value_type_t<XT>;
    using YV = value_type_t<YT>;
    using C = common_type_t<XV, YV>;
    if constexpr (is_complex_v<XT>)
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(std::real(x) - std::real(y),
                std::imag(x) - std::imag(y));
        else
            return complex<C>(std::real(x) - cast<C>(y), std::imag(x));
    }
    else
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(cast<C>(x) - std::real(y), -std::imag(y));
        else
            return cast<C>(cast<C>(x) - cast<C>(y));
    }
};
constexpr auto _scalar_times = [](const auto& x, const auto& y)
{
    using XT = remove_cvref_t<decltype(x)>;
    using YT = remove_cvref_t<decltype(y)>;
    using XV = value_type_t<XT>;
    using YV = value_type_t<YT>;
    using C = common_type_t<XV, YV>;
    if constexpr (is_complex_v<XT>)
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(x) * complex<C>(y);
        else
            return complex<C>(std::real(x) * cast<C>(y),
                std::imag(x) * cast<C>(y));
    }
    else
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(cast<C>(x) * std::real(y),
                cast<C>(x) * std::imag(y));
        else
            return cast<C>(cast<C>(x) * cast<C>(y));
    }
};
constexpr auto _scalar_divide = [](const auto& x, const auto& y)
{
    using XT = remove_cvref_t<decltype(x)>;
    using YT = remove_cvref_t<decltype(y)>;
    using XV = value_type_t<XT>;
    using YV = value_type_t<YT>;
    using C = common_type_t<XV, YV>;
    if constexpr (is_complex_v<XT>)
    {
        if constexpr (is_complex_v<YT>)
            return complex<C>(x) / complex<C>(y);
        else
            return complex<C>(x) / cast<C>(y);
    }
    else
    {
        if constexpr (is_complex_v<YT>)
            return cast<C>(x) / complex<C>(y);
        else if constexpr (is_integral_v<XT>&& is_integral_v<YT>)
            return double(x) / double(y);
        else
            return cast<C>(x) / cast<C>(y);
    }
};
#define WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(name, func)                 \
template<typename X, typename Y>                                        \
auto _scalar_##name(X& x, const Y& y)                                   \
{                                                                       \
    static_assert(is_convertible_v<common_type_t<X, Y>, X>,             \
        WL_ERROR_MUTABLE_TYPE);                                         \
    x = X(_scalar_##func(x, y));                                        \
    return x;                                                           \
}
WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(add_to, plus)
WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(subtract_from, subtract)
WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(times_by, times)
WL_DEFINE_MUTABLE_SCALAR_OPERATIONS(divide_by, divide)
#define WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(name)                         \
template<typename X, typename Y>                                            \
auto name(X&& x, Y&& y) -> decltype(auto)                                   \
{                                                                           \
    WL_TRY_BEGIN()                                                          \
    using XType = remove_cvref_t<X>;                                        \
    using YType = remove_cvref_t<Y>;                                        \
    constexpr auto x_rank = array_rank_v<XType>;                            \
    constexpr auto y_rank = array_rank_v<YType>;                            \
    static_assert(std::is_lvalue_reference_v<X&&> ||                        \
        is_array_view_v<XType>, WL_ERROR_MODIFY_TARGET);                    \
    if constexpr (x_rank > 0)                                               \
    {                                                                       \
        if constexpr (y_rank == 0)                                          \
        {                                                                   \
            x.for_each([y](auto& dst) { _scalar_##name(dst, y); });         \
        }                                                                   \
        else                                                                \
        {                                                                   \
            static_assert(x_rank == y_rank, WL_ERROR_OPERAND_RANK);         \
            if (!utils::check_dims<x_rank>(x.dims_ptr(), y.dims_ptr()))     \
                throw std::logic_error(WL_ERROR_ARITHMETIC_DIMS);           \
            if (has_aliasing(x, y))                                         \
            {                                                               \
                std::vector<value_type_t<YType>> buffer(y.size());          \
                y.copy_to(buffer.begin());                                  \
                x.for_each([](auto& dst, const auto& src)                   \
                    { _scalar_##name(dst, src); }, buffer.begin());         \
            }                                                               \
            else if constexpr (YType::category != view_category::General)   \
                x.for_each([](auto& dst, const auto& src)                   \
                    { _scalar_##name(dst, src); }, y.begin());              \
            else if constexpr (XType::category != view_category::General)   \
                y.for_each([](const auto& src, auto& dst)                   \
                    { _scalar_##name(dst, src); }, x.begin());              \
            else /* general_view ?= general_view */                         \
            {                                                               \
                std::vector<value_type_t<YType>> buffer(y.size());          \
                y.copy_to(buffer.begin());                                  \
                x.for_each([](auto& dst, const auto& src)                   \
                    { _scalar_##name(dst, src); }, buffer.begin());         \
            }                                                               \
        }                                                                   \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        static_assert(y_rank == 0, WL_ERROR_MUTABLE_RANK);                  \
        _scalar_##name(x, y);                                               \
    }                                                                       \
    return std::forward<decltype(x)>(x);                                    \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(add_to)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(subtract_from)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(times_by)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(divide_by)
template<typename X>
auto pre_increment(X&& x) -> decltype(auto)
{
    WL_TRY_BEGIN()
    return add_to(std::forward<decltype(x)>(x), int64_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto pre_decrement(X&& x) -> decltype(auto)
{
    WL_TRY_BEGIN()
    return subtract_from(std::forward<decltype(x)>(x), int64_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto increment(X&& x)
{
    WL_TRY_BEGIN()
    auto valx = val(x);
    pre_increment(std::forward<decltype(x)>(x));
    return valx;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto decrement(X&& x)
{
    WL_TRY_BEGIN()
    auto valx = val(x);
    pre_decrement(std::forward<decltype(x)>(x));
    return valx;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto minus(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    return utils::listable_function([](const auto& x) { return -x; },
        std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Iter, bool HasStride>
auto _variadic_plus(const argument_pack<Iter, HasStride>& args)
{
    auto ret = val(args.get(0));
    WL_CHECK_ABORT_LOOP_BEGIN(args.size() - 1u)
        for (auto i = _loop_begin; i < _loop_end; ++i)
            add_to(ret, args.get(i + 1, dim_checked{}));
    WL_CHECK_ABORT_LOOP_END()
    return ret;
}
template<typename X, typename Y>
auto plus(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(plus)
    {
        static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
            is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
        return utils::listable_function(_scalar_plus,
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(plus, int64_t(0))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(plus)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(plus)
template<typename X, typename Y>
auto subtract(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    return utils::listable_function(_scalar_subtract,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Iter, bool HasStride>
auto _variadic_times(const argument_pack<Iter, HasStride>& args)
{
    auto ret = val(args.get(0));
    WL_CHECK_ABORT_LOOP_BEGIN(args.size() - 1u)
        for (auto i = _loop_begin; i < _loop_end; ++i)
            times_by(ret, args.get(i + 1, dim_checked{}));
    WL_CHECK_ABORT_LOOP_END()
    return ret;
}
template<typename X, typename Y>
auto times(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(times)
    {
        static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
            is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
        return utils::listable_function(_scalar_times,
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(times, int64_t(1))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(times)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(times)
template<typename X, typename Y>
auto divide(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    return utils::listable_function(_scalar_divide,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
template<typename Fn, typename First, typename... Rest>
auto _iterator_apply_first(Fn fn, const First& first, const Rest&... rest)
{
    if constexpr (sizeof...(Rest) == 0u)
    {
        if constexpr (First::has_variable)
            return val(fn(first[0]));
        else
            return val(fn());
    }
    else
    {
        if constexpr (First::has_variable)
        {
            const auto& arg1 = first[0];
            return _iterator_apply_first(
                [&](const auto&... args) { return fn(arg1, args...); },
                rest...);
        }
        else
            return _iterator_apply_first(fn, rest...);
    }
}
template<typename Skip, typename Fn, typename First, typename... Rest>
auto _clause_impl(Skip& skip_flag,
    Fn fn, const First& first, const Rest&... rest)
{
    if constexpr (sizeof...(Rest) == 0u)
    {
        WL_CHECK_ABORT_LOOP_BEGIN(first.length())
            for (auto i = _loop_begin; i < _loop_end; ++i)
            {
                if constexpr (std::is_same_v<Skip, bool>)
                    if (skip_flag)
                    {
                        skip_flag = false;
                        continue;
                    }
                if constexpr (First::has_variable)
                    fn(first[i]);
                else
                    fn();
            }
        WL_CHECK_ABORT_LOOP_END();
    }
    else
    {
        for (size_t i = 0; i < first.length(); ++i)
        {
            if constexpr (First::has_variable)
            {
                const auto& arg1 = first[i];
                _clause_impl(skip_flag,
                    [&](const auto&... args) { return fn(arg1, args...); },
                    rest...);
            }
            else
                _clause_impl(skip_flag, fn, rest...);
        }
    }
}
template<typename Fn, typename... Iters>
auto clause_do(Fn fn, const Iters&... iters)
{
    WL_TRY_BEGIN()
    static_assert(sizeof...(Iters) >= 1u, WL_ERROR_INTERNAL);
    wl::void_type skip_flag;    // skip flag is not used
    try
    {
        _clause_impl(skip_flag, fn, iters...);
    }
    catch (const loop_break&)
    {
    }
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Fn, typename... Iters>
auto clause_table(Fn fn, const Iters&... iters)
{
    WL_TRY_BEGIN()
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, WL_ERROR_INTERNAL);
    using InnerType = remove_cvref_t<
        decltype(_iterator_apply_first(fn, iters...))>;
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);
    if (outer_size == 0u)
    {
        if constexpr (array_rank_v<InnerType> == 0u)
            return ndarray<InnerType, outer_rank>{};
        else
        {
            return ndarray<value_type_t<InnerType>,
                outer_rank + array_rank_v<InnerType>>{};
        }
    }
    else
    {
        if constexpr (array_rank_v<InnerType> == 0u)
        {
            ndarray<InnerType, outer_rank> ret(outer_dims);
            auto ret_iter = ret.begin();
            wl::void_type skip_flag;    // skip flag is not used
            _clause_impl(skip_flag,
                [&](const auto&... args) { *ret_iter++ = fn(args...); },
                iters...);
            return ret;
        }
        else
        {
            using ValueType = typename InnerType::value_type;
            constexpr auto inner_rank = array_rank_v<InnerType>;
            auto first_item = _iterator_apply_first(fn, iters...);
            auto inner_dims = first_item.dims();
            auto all_dims = utils::dims_join(outer_dims, inner_dims);
            ndarray<ValueType, outer_rank + inner_rank> ret(all_dims);
            auto ret_iter = ret.template view_begin<outer_rank>();
            first_item.copy_to(ret_iter.begin());
            ++ret_iter;
            bool skip_flag = true;      // skip flag is used
            _clause_impl(skip_flag,
                [&](const auto&... args)
                {
                    auto item = fn(args...);
                    if (!utils::check_dims(ret_iter.dims(), item.dims()))
                        throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
                    item.copy_to(ret_iter.begin());
                    ++ret_iter;
                },
                iters...);
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Fn, typename... Iters>
auto clause_sum(Fn fn, const Iters&... iters)
{
    WL_TRY_BEGIN()
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, WL_ERROR_INTERNAL);
    using InnerType = remove_cvref_t<
        decltype(_iterator_apply_first(fn, iters...))>;
    static_assert(is_numerical_type_v<InnerType>, WL_ERROR_SUM_ELEMENT);
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);
    if (outer_size == 0u)
    {
        if constexpr (is_arithmetic_v<InnerType>)
            return InnerType{};
        else
            throw std::logic_error(WL_ERROR_SUM_ZERO_SIZE);
    }
    else
    {
        auto ret = _iterator_apply_first(fn, iters...);
        bool skip_flag = true;      // skip flag is not used
        if constexpr (array_rank_v<InnerType> >= 1u)
        {
            _clause_impl(skip_flag,
                [&](const auto&... args)
                {
                    add_to(ret, fn(args...));
                },
                iters...);
        }
        else
        {
            _clause_impl(skip_flag,
                [&](const auto&... args) { add_to(ret, fn(args...)); },
                iters...);
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Fn, typename... Iters>
auto clause_product(Fn fn, const Iters&... iters)
{
    WL_TRY_BEGIN()
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, WL_ERROR_INTERNAL);
    using InnerType = remove_cvref_t<
        decltype(_iterator_apply_first(fn, iters...))>;
    static_assert(is_numerical_type_v<InnerType>, WL_ERROR_SUM_ELEMENT);
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);
    if (outer_size == 0u)
    {
        if constexpr (is_arithmetic_v<InnerType>)
            return InnerType(int8_t(1));
        else
            throw std::logic_error(WL_ERROR_PRODUCT_ZERO_SIZE);
    }
    else
    {
        auto ret = _iterator_apply_first(fn, iters...);
        bool skip_flag = true;      // skip flag is not used
        _clause_impl(skip_flag,
            [&](const auto&... args)
            {
                times_by(ret, fn(args...));
            },
            iters...);
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
template<typename Re, typename Im>
auto make_complex(const Re& re, const Im& im)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Re> && is_real_v<Im>, WL_ERROR_REAL_TYPE_ARG);
    using C = common_type_t<Re, Im>;
    using T = std::conditional_t<std::is_same_v<C, float>, float, double>;
    return complex<T>(T(re), T(im));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename... Args>
inline auto complex_type::operator()(Args&&... args) const
{
    return make_complex(std::forward<decltype(args)>(args)...);
}
template<typename X>
auto re(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto scalar_re = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x.real();
        else
            return x;
    };
    return utils::listable_function(scalar_re, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto im(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto scalar_im = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x.imag();
        else
            return XV(0);
    };
    return utils::listable_function(scalar_im, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto arg(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto scalar_arg = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
        {
            return std::arg(x);
        }
        else if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return double(0);
            else
                return x >= XV(0) ? double(0) : const_pi;
        }
        else
            return x >= XV(0) ? XV(0) : XV(const_pi);
    };
    return utils::listable_function(scalar_arg, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto conjugate(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto scalar_conjugate = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return std::conj(x);
        else
            return x;
    };
    return utils::listable_function(scalar_conjugate,
        std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto re_im(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, WL_ERROR_NUMERIC_ONLY);
    constexpr auto rank = array_rank_v<XT>;
    if constexpr (rank == 0)
    {
        using XV = value_type_t<XT>;
        ndarray<XV, 1u> ret(std::array<size_t, 1u>{2});
        auto iter = ret.data();
        iter[0] = re(x);
        iter[1] = im(x);
        return ret;
    }
    else
    {
        using XV = value_type_t<value_type_t<XT>>;
        auto dims = utils::dims_join(x.dims(), std::array<size_t, 1u>{2});
        ndarray<XV, rank + 1u> ret(dims);
        auto iter = ret.data();
        x.for_each([&](const auto& a)
            { *iter++ = re(a), *iter++ = im(a); });
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto abs_arg(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>,
        WL_ERROR_NUMERIC_ONLY);
    constexpr auto rank = array_rank_v<XT>;
    if constexpr (rank == 0)
    {
        using XV = value_type_t<XT>;
        ndarray<XV, 1u> ret(std::array<size_t, 1u>{2});
        auto iter = ret.data();
        iter[0] = XV(abs(x));
        iter[1] = XV(arg(x));
        return ret;
    }
    else
    {
        using XV = promote_integral_t<value_type_t<value_type_t<XT>>>;
        auto dims = utils::dims_join(x.dims(), std::array<size_t, 1u>{2});
        ndarray<XV, rank + 1u> ret(dims);
        auto iter = ret.begin();
        x.for_each([&](const auto& a)
            { *iter++ = XV(abs(a)), *iter++ = XV(arg(a)); });
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
#define WL_DEFINE_UNARY_MATH_FUNCTION(name, expr)                           \
template<typename X>                                                        \
WL_INLINE auto name(X&& x)                                                  \
{                                                                           \
    WL_TRY_BEGIN()                                                          \
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,                   \
        WL_ERROR_NUMERIC_ONLY);                                             \
    return utils::listable_function([](auto x) {                            \
            using PX = promote_integral_t<decltype(x)>;                     \
            return expr;                                                    \
        }, std::forward<decltype(x)>(x));                                   \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}
#define WL_DEFINE_BINARY_MATH_FUNCTION(name, expr)                          \
template<typename X, typename Y>                                            \
WL_INLINE auto name(X&& x, Y&& y)                                           \
{                                                                           \
    WL_TRY_BEGIN()                                                          \
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&                 \
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);     \
    return utils::listable_function([](auto x, auto y)                      \
        {                                                                   \
            using PX = promote_integral_t<decltype(x)>;                     \
            using PY = promote_integral_t<decltype(y)>;                     \
            using PC = promote_integral_t<                                  \
                common_type_t<decltype(x), decltype(y)>>;                   \
            return expr;                                                    \
        }, std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));     \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}
template<typename X>
WL_INLINE auto _scalar_square(const X x)
{
    return X(x * x);
}
template<typename X, typename Y>
WL_INLINE auto _scalar_power(const X& x, const Y& y)
{
    if constexpr (is_integral_v<Y>)
    {
        auto tmp = x;
        auto count = y < 0 ? uint64_t(0) - uint64_t(y) :  uint64_t(y);
        auto ret = X(1);
        for (;; tmp *= tmp)
        {
            if ((count & uint64_t(1)) != 0u)
                ret *= tmp;
            if ((count >>= 1) == 0u)
                return y < 0 ? X(1) / ret : ret;
        }
    }
    else
    {
        return std::pow(x, y);
    }
}
template<typename X, int64_t y>
WL_INLINE auto _scalar_power(const X& x, const_int<y>)
{
    if constexpr (y < 0)
        return X(1) / _scalar_power(x, const_int<-y>{});
    else if constexpr (y == 0)
        return X(1);
    else if constexpr (y == 1)
        return x;
    else if constexpr (y == 2)
        return X(x * x);
    else if (y < 16)
    {
        X ret = _scalar_power(x, const_int<(y / 2)>{});
        ret *= ret;
        if constexpr ((y & int64_t(1)) != 0)
            ret *= x;
        return ret;
    }
    else
        return _scalar_power(x, y);
}
template<typename X, int64_t I>
WL_INLINE auto power(X&& x, const_int<I>)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    return utils::listable_function([](auto x)
        {
            using PC = promote_integral_t<common_type_t<decltype(x), int64_t>>;
            return _scalar_power(PC(x), const_int<I>{});
        }, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
WL_INLINE X _scalar_gamma(const X& x)
{
    static_assert(is_float_v<X>, WL_ERROR_INTERNAL);
    return X(std::tgamma(x));
}
template<typename X>
complex<X> _scalar_gamma(const complex<X>& x)
{
    constexpr size_t data_size = 9;
    static const std::array<double, data_size> data ={
        0.99999999999980993, 676.5203681218851, -1259.1392167224028,
        771.32342877765313, -176.61502916214059, 12.507343278686905,
        -0.13857109526572012, 9.9843695780195716e-6,
        1.5056327351493116e-7};
    if (std::real(x) < X(0.5))
    {
        return X(const_pi) / (
            std::sin(X(const_pi) * x) * _scalar_gamma(X(1) - x));
    }
    else
    {
        complex<X> z = x - X(1);
        complex<X> y = data[0];
        for (size_t i = 1; i < data_size; ++i)
            y += data[i] / (z + X(i));
        complex<X> t = z + X(7.5);
        return std::sqrt(X(2 * const_pi)) *
            std::pow(t, z + X(0.5)) * std::exp(-t) * y;
    }
}
WL_DEFINE_UNARY_MATH_FUNCTION(log, std::log(x))
WL_DEFINE_BINARY_MATH_FUNCTION(log, std::log(PC(y)) / std::log(PC(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(log2, PX(1.4426950408889634074) * std::log(x))
WL_DEFINE_UNARY_MATH_FUNCTION(log10, PX(0.43429448190325182765) * std::log(x))
WL_DEFINE_UNARY_MATH_FUNCTION(exp, std::exp(x))
WL_DEFINE_UNARY_MATH_FUNCTION(sqrt, std::sqrt(x))
WL_DEFINE_BINARY_MATH_FUNCTION(power, _scalar_power(PC(x), y))
WL_DEFINE_UNARY_MATH_FUNCTION(sin, std::sin(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cos, std::cos(x))
WL_DEFINE_UNARY_MATH_FUNCTION(tan, std::tan(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cot, PX(1) / std::tan(x))
WL_DEFINE_UNARY_MATH_FUNCTION(sec, PX(1) / std::cos(x))
WL_DEFINE_UNARY_MATH_FUNCTION(csc, PX(1) / std::sin(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arcsin, std::asin(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccos, std::acos(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arctan, std::atan(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccot, std::atan(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(arcsec, std::acos(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccsc, std::asin(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(sinh, std::sinh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(cosh, std::cosh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(tanh, std::tanh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(coth, PX(1) / std::tanh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(sech, PX(1) / std::cosh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(csch, PX(1) / std::sinh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arcsinh, std::asinh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccosh, std::acosh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arctanh, std::atanh(x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccoth, std::atanh(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(arcsech, std::acosh(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(arccsch, std::asinh(PX(1) / x))
WL_DEFINE_UNARY_MATH_FUNCTION(sinc, (x == PX(0) ? PX(1) : std::sin(x) / PX(x)))
WL_DEFINE_BINARY_MATH_FUNCTION(arctan, std::atan2(PC(x), PC(y)))
WL_DEFINE_UNARY_MATH_FUNCTION(haversine,
    _scalar_square(std::sin(PX(0.5) * x)))
WL_DEFINE_UNARY_MATH_FUNCTION(inverse_haversine,
    PX(2) * std::asin(std::sqrt(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(gudermannian,
    PX(2) * std::atan(std::exp(x)) - PX(1.5707963267948966192))
WL_DEFINE_UNARY_MATH_FUNCTION(inverse_gudermannian,
    std::log(std::tan(PX(0.5) * x + PX(0.78539816339744830962))))
WL_DEFINE_UNARY_MATH_FUNCTION(logistic_sigmoid,
    PX(1) / (PX(1) + std::exp(-PX(x))))
WL_DEFINE_UNARY_MATH_FUNCTION(gamma, _scalar_gamma(x))
WL_DEFINE_UNARY_MATH_FUNCTION(log_gamma, std::lgamma(x))
WL_DEFINE_UNARY_MATH_FUNCTION(erf, std::erf(x))
WL_DEFINE_UNARY_MATH_FUNCTION(erfc, std::erfc(x))
#if !defined(WL_DISABLE_SPECIAL_FUNCTIONS)
WL_DEFINE_BINARY_MATH_FUNCTION(beta, std::beta(x, y))
WL_DEFINE_UNARY_MATH_FUNCTION(zeta, std::riemann_zeta(x))
#endif
}
namespace wl
{
template<typename X>
auto even_q(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
            return boolean((x & XV(1)) == XV(0));
        else if constexpr (is_float_v<XV>)
            return boolean(std::fmod(x, XV(2)) == XV(0));
        else
            return const_false;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto odd_q(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
            return boolean((x & XV(1)) == XV(1));
        else if constexpr (is_float_v<XV>)
            return boolean(std::fmod(x, XV(2)) == std::copysign(XV(1), x));
        else
            return const_false;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto divisible(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>> &&
        is_numerical_type_v<remove_cvref_t<Y>>, WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, WL_ERROR_REAL_TYPE_ARG);
        if constexpr (is_integral_v<XV> && is_integral_v<YV>)
        {
            using C = std::conditional_t<
                std::is_signed_v<XV> || std::is_signed_v<YV>,
                make_signed_t<common_type_t<XV, YV>>,
                common_type_t<XV, YV>>;
            return boolean(y != YV(0) && C(x) % C(y) == C(0));
        }
        else
        {
            using C = common_type_t<XV, YV>;
            return boolean(std::fmod(C(x), C(y)) == C(0));
        }
    };
    return utils::listable_function(pure,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
inline uint64_t _fibonacci_impl(uint64_t n, uint64_t* prev)
{
    constexpr size_t data_size = 94;
    static const std::array<uint64_t, data_size> data ={
        0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,
        10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,
        1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,
        63245986,102334155,165580141,267914296,433494437,701408733,1134903170,
        1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,
        32951280099,53316291173,86267571272,139583862445,225851433717,
        365435296162,591286729879,956722026041,1548008755920,2504730781961,
        4052739537881,6557470319842,10610209857723,17167680177565,
        27777890035288,44945570212853,72723460248141,117669030460994,
        190392490709135,308061521170129,498454011879264,806515533049393,
        1304969544928657,2111485077978050,3416454622906707,5527939700884757,
        8944394323791464,14472334024676221,23416728348467685,37889062373143906,
        61305790721611591,99194853094755497,160500643816367088,
        259695496911122585,420196140727489673,679891637638612258,
        1100087778366101931,1779979416004714189,2880067194370816120,
        4660046610375530309,7540113804746346429,12200160415121876738u};
    if (n < data_size)
    {
        *prev = n == 0u ? uint64_t(1) : data[n - 1];
        return data[n];
    }
    auto lzcnt = utils::lzcnt_u64(n);
    uint64_t leading = n >> (58 - lzcnt);
    uint64_t mask = uint64_t(1) << (57 - lzcnt);
    uint64_t a = data[leading - 1];
    uint64_t b = data[leading];
    uint64_t c = 0u;
    for (; mask; mask >>= 1)
    {
        c = a * a + b * b;
        b *= (2 * a + b);
        a = c;
        if (n & mask)
        {
            c = b;
            b += a;
            a = c;
        }
    }
    *prev = a;
    return b;
}
inline uint64_t _fibonacci(uint64_t n)
{
    uint64_t n1;
    return _fibonacci_impl(n, &n1);
}
inline uint64_t _lucas_l(uint64_t n)
{
    uint64_t n1;
    uint64_t n2 = _fibonacci_impl(n - 1u, &n1);
    return n2 * 3u + n1;
}
template<typename X>
auto fibonacci(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_fibonacci(x));
            else if (x >= XV(0))
                return XV(_fibonacci(x));
            else // x < 0
            {
                auto val = XV(_fibonacci(-x));
                return (x & XV(1)) ? val : -val;
            }
        }
        else
        {
            XV phi_x = std::pow(XV(1.6180339887498948482), x);
            XV cos_pi_x = std::cos(XV(const_pi) * x);
            return XV(0.44721359549995793928) * (phi_x - cos_pi_x / phi_x);
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto lucas_l(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if (x == XV(0))
                return XV(2);
            else if constexpr (std::is_unsigned_v<XV>)
                return XV(_lucas_l(x));
            else if (x > XV(0))
                return XV(_lucas_l(x));
            else // x < 0
            {
                auto val = XV(_lucas_l(-x));
                return (x & XV(1)) ? -val : val;
            }
        }
        else
        {
            XV phi_x = std::pow(XV(1.6180339887498948482), x);
            XV cos_pi_x = std::cos(XV(const_pi) * x);
            return phi_x + cos_pi_x / phi_x;
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
inline uint64_t _factorial(uint64_t n)
{
    constexpr size_t data_size = 66;
    static const std::array<uint64_t, data_size> data ={
        1,1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,
        6227020800,87178291200,1307674368000,20922789888000,355687428096000,
        6402373705728000,121645100408832000,2432902008176640000u,
        14197454024290336768u,17196083355034583040u,8128291617894825984u,
        10611558092380307456u,7034535277573963776u,16877220553537093632u,
        12963097176472289280u,12478583540742619136u,11390785281054474240u,
        9682165104862298112u,4999213071378415616u,12400865694432886784u,
        3400198294675128320u,4926277576697053184u,6399018521010896896u,
        9003737871877668864u,1096907932701818880u,4789013295250014208u,
        2304077777655037952u,18376134811363311616u,15551764317513711616u,
        7538058755741581312u,10541877243825618944u,2673996885588443136u,
        9649395409222631424u,1150331055211806720u,17172071447535812608u,
        12602690238498734080u,8789267254022766592u,15188249005818642432u,
        18284192274659147776u,9994050523088551936u,13175843659825807360u,
        10519282829630636032u,6711489344688881664u,6908521828386340864u,
        6404118670120845312u,2504001392817995776u,162129586585337856u,
        9727775195120271360u,3098476543630901248u,7638104968020361216u,
        1585267068834414592u,9223372036854775808u,9223372036854775808u};
    if (n < data_size)
        return data[n];
    else
        return uint64_t(0);
}
inline uint64_t _factorial2(uint64_t n)
{
    constexpr size_t data_size = 66;
    static const std::array<uint64_t, data_size> data ={
        1,1,2,3,8,15,48,105,384,945,3840,10395,46080,135135,645120,2027025,
        10321920,34459425,185794560,654729075,3715891200,13749310575,
        81749606400,316234143225,1961990553600,7905853580625,51011754393600,
        213458046676875,1428329123020800,6190283353629375,42849873690624000,
        191898783962510625,1371195958099968000,6332659870762850625u,
        9727174427979808768u,282166592185152483u,18136886080501186560u,
        10440163910850641871u,6672140331791679488u,1338022901564897417u,
        8631196239733456896u,17965450816741690865u,12022104668323708928u,
        16197878097801090939u,12463771342375747584u,9481495526376579231u,
        1484415464288288768u,2908431970669985073u,15911710064709206016u,
        13385958046862407265u,2375508065949581312u,154329662729360723u,
        12845954987120918528u,8179472124656118319u,11152038577276190720u,
        7149109087057268761u,15771605895051476992u,1670848340654183825u,
        10862682301217636352u,6346331730049087595u,6124895493223874560u,
        18191354058803310975u,10808639105689190400u,2357173134616391233u,
        9223372036854775808u,5642301160389017217u};
    if (n < data_size)
        return data[n];
    else if (n % 2u == 0u)
        return uint64_t(0);
    else
    {
        uint64_t res = 1u;
        for (;;)
        {
            res *= n;
            n -= 2u;
            if (n < data_size)
                return uint64_t(res * data[n]);
        }
    }
}
template<typename X>
auto factorial(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_factorial(x));
            else if (x >= XV(0))
                return XV(_factorial(x));
            else // x < 0
                throw std::logic_error(WL_ERROR_FACTORIAL_NEGATIVE);
        }
        else
        {
            return XV(wl::gamma(XV(1) + x));
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto factorial2(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<remove_cvref_t<X>>,
        WL_ERROR_NUMERIC_ONLY);
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_factorial2(x));
            else if (x >= XV(0))
                return XV(_factorial2(x));
            else // x < 0
            {
                if (x == XV(-1))
                    return XV(1);
                else if (x == XV(-3))
                    return XV(-1);
                else
                    throw std::logic_error(WL_ERROR_FACTORIAL2_DOMAIN);
            }
        }
        else
        {
            // x!!:=exp((log(2)/2)*x+(log(pi/2)/4)*(cos(pi*x)-1))*gamma(1+x/2)
            const auto log_2_2 = XV(0.34657359027997265471);
            const auto log_pi_2_4 = XV(0.11289567632236371618);
            XV t1 = log_2_2 * x;
            XV t2 = log_pi_2_4 * (std::cos(XV(const_pi) * x) - XV(1));
            XV t3 = wl::gamma(XV(1) + XV(0.5) * x);
            return XV(std::exp(t1 + t2) * t3);
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
constexpr auto bool_not(boolean x)
{
    return !x;
}
constexpr auto implies(boolean x, boolean y)
{
    return !x || y;
}
template<typename X, typename Y>
auto bool_and(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bool_and)
    {
        static_assert(is_boolean_v<remove_cvref_t<X>> &&
            is_boolean_v<remove_cvref_t<Y>>, WL_ERROR_BOOLEAN_ARG);
        return x && y;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Iter, bool HasStride>
auto _variadic_bool_and(const argument_pack<Iter, HasStride>& args)
{
    boolean ret = val(args.get(0));
    WL_CHECK_ABORT_LOOP_BEGIN(args.size() - 1u)
        for (auto i = _loop_begin; i < _loop_end && ret; ++i)
            ret = bool_and(ret, args.get(i + 1, dim_checked{}));
    WL_CHECK_ABORT_LOOP_END()
    return ret;
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bool_and, const_true)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bool_and)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bool_and)
template<typename X, typename Y>
auto bool_or(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bool_or)
    {
        static_assert(is_boolean_v<remove_cvref_t<X>> &&
            is_boolean_v<remove_cvref_t<Y>>, WL_ERROR_BOOLEAN_ARG);
        return x || y;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Iter, bool HasStride>
auto _variadic_bool_or(const argument_pack<Iter, HasStride>& args)
{
    boolean ret = val(args.get(0));
    WL_CHECK_ABORT_LOOP_BEGIN(args.size() - 1u)
        for (auto i = _loop_begin; i < _loop_end && !ret; ++i)
            ret = bool_or(ret, args.get(i + 1, dim_checked{}));
    WL_CHECK_ABORT_LOOP_END()
    return ret;
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bool_or, const_false)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bool_or)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bool_or)
template<typename X, typename Y>
auto bool_xor(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bool_xor)
    {
        static_assert(is_boolean_v<remove_cvref_t<X>> &&
            is_boolean_v<remove_cvref_t<Y>>, WL_ERROR_BOOLEAN_ARG);
        return x ^ y;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_VARIADIC(bool_xor)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bool_xor, const_false)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bool_xor)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bool_xor)
template<typename... Args>
auto bool_nand(Args&&... args)
{
    WL_TRY_BEGIN()
    return !bool_and(std::forward<decltype(args)>(args)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename... Args>
auto bool_nor(Args&&... args)
{
    WL_TRY_BEGIN()
    return !bool_or(std::forward<decltype(args)>(args)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename... Args>
auto bool_xnor(Args&&... args)
{
    WL_TRY_BEGIN()
    return !bool_xor(std::forward<decltype(args)>(args)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto boole(X&& x)
{
    WL_TRY_BEGIN()
    static_assert(is_boolean_type_v<remove_cvref_t<X>>, WL_ERROR_BOOLEAN_ARG);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    auto pure = [](boolean x) { return Ret(x); };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto bit_not(X&& x)
{
    WL_TRY_BEGIN()
    auto pure = [](auto x)
    {
        using XV = decltype(x);
        static_assert(is_integral_v<XV>, WL_ERROR_INTEGRAL_TYPE_ARG);
        return XV(~x);
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto bit_length(X&& x)
{
    WL_TRY_BEGIN()
    auto pure = [](auto x)
    {
        using XV = decltype(x);
        static_assert(is_integral_v<XV>, WL_ERROR_INTEGRAL_TYPE_ARG);
        if constexpr (std::is_unsigned_v<XV>)
            return Ret(64) - Ret(utils::lzcnt_u64(x));
        else
            return Ret(64) - Ret(utils::lzcnt_u64(
                std::make_unsigned_t<XV>(x >= XV(0) ? x : ~x)));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
#define WL_DEFINE_BITWISE_OPERATION(name, expr)                             \
template<typename X, typename Y>                                            \
auto name(X&& x, Y&& y)                                                     \
{                                                                           \
    WL_TRY_BEGIN()                                                          \
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bit_and)                 \
    {                                                                       \
        auto pure = [](const auto& x, const auto& y)                        \
        {                                                                   \
            using XV = remove_cvref_t<decltype(x)>;                         \
            using YV = remove_cvref_t<decltype(y)>;                         \
            static_assert(is_integral_v<XV> && is_integral_v<YV>,           \
                WL_ERROR_INTEGRAL_TYPE_ARG);                                \
            using C = common_type_t<XV, YV>;                                \
            return expr;                                                    \
        };                                                                  \
        return utils::listable_function(pure,                               \
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));    \
    }                                                                       \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}                                                                           \
template<typename X, typename Y>                                            \
void _##name##_assignment(X& x, const Y& y)                                 \
{                                                                           \
    x = X(name(x, y));                                                      \
}                                                                           \
template<typename XV, typename YV, size_t R>                                \
void _##name##_assignment(ndarray<XV, R>& x, const ndarray<YV, R>& y)       \
{                                                                           \
    assert(x.dims() == y.dims());                                           \
    x.for_each(                                                             \
        [](auto& a, const auto& b) { _##name##_assignment(a, b); },         \
        y.data());                                                          \
}                                                                           \
template<typename Iter, bool HasStride>                                     \
auto _variadic_##name(const argument_pack<Iter, HasStride>& args)           \
{                                                                           \
    auto ret = val(args.get(0));                                            \
    WL_CHECK_ABORT_LOOP_BEGIN(args.size() - 1u)                             \
        for (auto i = _loop_begin; i < _loop_end; ++i)                      \
            _##name##_assignment(ret, args.get(i + 1, dim_checked{}));      \
    WL_CHECK_ABORT_LOOP_END()                                               \
    return ret;                                                             \
}
WL_DEFINE_BITWISE_OPERATION(bit_and, C(C(x) & C(y)))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bit_and, int64_t(-1))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bit_and)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bit_and)
WL_DEFINE_BITWISE_OPERATION(bit_or, C(C(x) | C(y)))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bit_or, int64_t(0))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bit_or)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bit_or)
WL_DEFINE_BITWISE_OPERATION(bit_xor, C(C(x) ^ C(y)))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bit_xor, int64_t(0))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bit_xor)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bit_xor)
template<bool ShiftLeft, typename X, typename Y>
auto _bit_shift_impl(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    auto pure = [](auto x, auto y)
    {
        using XV = decltype(x);
        using YV = decltype(y);
        static_assert(is_integral_v<XV> && is_integral_v<YV>,
            WL_ERROR_INTEGRAL_TYPE_ARG);
        if constexpr (ShiftLeft)
        {
            return (std::is_unsigned_v<YV> || y >= YV(0)) ? XV(x << y) :
                (std::is_unsigned_v<XV> || x >= XV(0)) ? XV(x >> -y) :
                XV(-(-x >> -y));
        }
        else
        {
            return (std::is_signed_v<YV> && y < YV(0)) ? XV(x << -y) :
                (std::is_unsigned_v<XV> || x >= XV(0)) ? XV(x >> y) :
                XV(-(-x >> y));
        }
    };
    return utils::listable_function(pure,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto bit_shift_left(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    return _bit_shift_impl<true>(
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto bit_shift_right(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    return _bit_shift_impl<false>(
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
namespace utf8
{
using char_t   = uint8_t;
using char21_t = uint32_t;
constexpr char_t null_character = '\0';
constexpr char21_t max_ascii_code_point = 0x7fu;
constexpr char21_t max_code_point = 0x0010'ffffu;
inline constexpr char_t operator""_c(const char ch)
{
    return char_t(ch);
}
template<typename Char>
constexpr bool is_ascii(Char ch)
{
    static_assert(std::is_unsigned_v<Char>, WL_ERROR_INTERNAL);
    return ch <= Char(max_ascii_code_point);
}
template<typename Char>
std::pair<size_t, std::array<char_t, 4>> from_code_point(Char ch)
{
    if (ch < Char(0x80u))
        return {1u, {char_t(ch), 0, 0, 0}};
    else if (ch < 0x0800u)
        return {2u, {char_t((ch >> 6) | 0xc0u),
            char_t((ch & 0x3fu) | 0x80u), 0, 0}};
    else if (ch < Char(0x0001'0000u))
        return {3u, {char_t((ch >> 12) | 0xe0u),
            char_t(((ch >> 6) & 0x3fu) | 0x80u),
            char_t((ch & 0x3fu) | 0x80u), 0}};
    else if (ch < Char(0x0011'0000u))
        return {4u, {char_t((ch >> 18) | 0xf0u),
            char_t(((ch >> 12) & 0x3fu) | 0x80u),
            char_t(((ch >> 6) & 0x3fu) | 0x80u),
            char_t((ch & 0x3fu) | 0x80u)}};
    else
        throw std::logic_error(WL_ERROR_INVALID_CODEPOINT);
}
inline size_t _get_byte_size(const char_t* str, bool& ret_ascii_only)
{
    WL_THROW_IF_ABORT()
#if defined(__AVX2__) || defined(__SSE4_1__)
    using namespace wl::simd;
#  if defined(__AVX2__)
    using M = __m256i;
#  else
    using M = __m128i;
#  endif
    const auto upper = set1<M>(int8_t(0b1100'0000));
    size_t i_byte = 0u;
    bool ascii_only = true;
    int zmask = 0;
    M data;
    for (; true; str += sizeof(M), i_byte += sizeof(M))
    {
        data = loadu<M>(str);
        zmask = movemask_epi8(cmpeq_epi8(data, zero<M>()));
        if (zmask)
            break;
        if (ascii_only && movemask_epi8(data))
            ascii_only = false;
    }
    auto excess_byte = utils::tzcnt_u64(uint64_t(unsigned(zmask)));
    if (ascii_only && excess_byte > 0)
    {
        auto nmask = uint64_t(unsigned(movemask_epi8(data)));
        ascii_only = !bool(nmask << (64u - excess_byte));
    }
    ret_ascii_only = ascii_only;
    return i_byte + excess_byte;
#else
    auto str0 = str;
    bool ascii_only = true;
    for (; ascii_only; ++str)
    {
        auto byte = *str;
        if (!byte)
        {
            ret_ascii_only = true;
            return size_t(str - str0);
        }
        else if (uint8_t(byte) >= 0b1000'0000u)
        {
            ret_ascii_only = false;
            break;
        }
    }
    while (*++str)
    {
    }
    return size_t(str - str0);
#endif
}
inline size_t _get_string_size_impl(const char_t* str, const size_t byte_size)
{
    WL_THROW_IF_ABORT()
#if defined(__AVX2__) || defined(__SSE4_1__)
    using namespace wl::simd;
#  if defined(__AVX2__)
    using M = __m256i;
#  else
    using M = __m128i;
#  endif
    const auto upper = set1<M>(int8_t(0b1100'0000));
    auto trailing = zero<M>();
    size_t trailing_size = 0u;
    size_t i_byte = 0u;
    for (size_t i = 0u; i_byte + sizeof(M) < byte_size;
        ++i, str += sizeof(M), i_byte += sizeof(M))
    {
        trailing = sub_epi8(trailing, cmpgt_epi8(upper, loadu<M>(str)));
        if (i >= 100u)
        {
            trailing_size += hsum_epi8(trailing);
            trailing = zero<M>();
            i = 0u;
        }
    }
    auto tmask = unsigned(movemask_epi8(cmpgt_epi8(upper, loadu<M>(str))));
    auto excess_trailing = utils::_popcnt(
        uint64_t(tmask) << (64u - (byte_size - i_byte)));
    trailing_size += hsum_epi8(trailing);
    trailing_size += excess_trailing;
    return byte_size - trailing_size;
#else
    size_t trailing_size = 0u;
    for (size_t i = 0; i < byte_size; ++i)
    {
        if (int8_t(str[i]) < int8_t(0b1100'0000))
            ++trailing_size;
    }
    return byte_size - trailing_size;
#endif
}
inline size_t _get_string_size_check_valid_impl(
    const char_t* in_str, const size_t ref_byte_size)
{
    WL_THROW_IF_ABORT()
    size_t byte_size = 0;
    size_t trailing_size = 0;
    auto str_begin = reinterpret_cast<const int8_t*>(in_str);
    auto str = str_begin;
    for (;;)
    {
        size_t n_bytes = 0u;
        char_t byte = *str++;
        if (!byte)
        {
            if (byte_size != ref_byte_size)
                throw std::logic_error(WL_ERROR_INTERNAL);
            return byte_size - trailing_size;
        }
        if (byte_size >= ref_byte_size)
            throw std::logic_error(WL_ERROR_BAD_UTF8_NULL_TERMINATED);
        if (byte < 0b1000'0000u)
            n_bytes = 1u;
        else if ((byte & 0b1110'0000u) == 0b1100'0000u)
            n_bytes = 2u;
        else if ((byte & 0b1111'0000u) == 0b1110'0000u)
            n_bytes = 3u;
        else if ((byte & 0b1111'1000u) == 0b1111'0000u)
            n_bytes = 4u;
        else
            throw std::logic_error(WL_ERROR_BAD_UTF8_CODEPOINT);
        for (size_t i = 1u; i < n_bytes; ++i)
        {
            if ((*str++ & 0b1100'0000u) != 0b1000'0000u)
                throw std::logic_error(WL_ERROR_BAD_UTF8_CODEPOINT);
        }
        trailing_size += n_bytes - 1u;
        byte_size += n_bytes;
    }
}
template<bool CheckValid = false>
size_t get_string_size(const char_t* str, const size_t byte_size)
{
    if constexpr (CheckValid)
        return _get_string_size_check_valid_impl(str, byte_size);
    else
        return _get_string_size_impl(str, byte_size);
}
bool is_ascii_only(const char_t* str, const size_t byte_size)
{
    WL_THROW_IF_ABORT()
#if defined(__AVX2__) || defined(__SSE4_1__)
    using namespace wl::simd;
#  if defined(__AVX2__)
    using M = __m256i;
#  else
    using M = __m128i;
#endif
    const auto upper = set1<M>(int8_t(0b1100'0000));
    size_t i_byte = 0u;
    for (size_t i = 0u; i_byte + sizeof(M) < byte_size;
        ++i, str += sizeof(M), i_byte += sizeof(M))
    {
        auto tmask = unsigned(movemask_epi8(loadu<M>(str)));
        if (tmask)
            return false;
    }
    auto tmask = unsigned(movemask_epi8(loadu<M>(str)));
    return !bool(uint64_t(tmask) << (64u - (byte_size - i_byte)));
#else
    size_t trailing_size = 0u;
    for (size_t i = 0; i < byte_size; ++i)
    {
        if (int8_t(str[i]) < int8_t(0b1100'0000))
            return true;
    }
    return false;
#endif
}
struct string_iterator
{
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = char21_t;
    using difference_type = ptrdiff_t;
    using pointer = void;
    using reference = char21_t;
    const char_t* ptr_;
    string_iterator() : ptr_{nullptr}
    {
    }
    string_iterator(const char_t* ptr) : ptr_{ptr}
    {
    }
    WL_INLINE static bool _is_valid_codepoint_leading(uint8_t byte)
    {
        return (byte < 0b1000'0000u) || (byte >= 0b1100'0000u);
    }
    WL_INLINE static bool _is_valid_codepoint_tailing(uint8_t byte)
    {
        return (byte & 0b1100'0000u) == 0b1000'0000u;
    }
    const char_t* get_pointer() const
    {
        return ptr_;
    }
    operator const char*() const
    {
        return (const char*)ptr_;
    }
    string_iterator& operator=(const utf8::char_t* other)
    {
        ptr_ = other;
        return *this;
    }
    string_iterator& operator=(const char* other)
    {
        *this = (const utf8::char_t*)other;
        return *this;
    }
    string_iterator& operator++()
    {
        ptr_ += num_bytes();
        return *this;
    }
    string_iterator& operator--()
    {
        ptr_ -= previous_num_bytes();
        return *this;
    }
    string_iterator operator++(int)
    {
        const auto copy = *this;
        ++(*this);
        return copy;
    }
    string_iterator operator--(int)
    {
        const auto copy = *this;
        --(*this);
        return copy;
    }
    string_iterator& operator+=(ptrdiff_t n)
    {
        apply_offset(n);
        return *this;
    }
    string_iterator& operator-=(ptrdiff_t n)
    {
        apply_offset(-n);
        return *this;
    }
    bool operator==(const string_iterator& other) const
    {
        return this->ptr_ == other.ptr_;
    }
    bool operator!=(const string_iterator& other) const
    {
        return this->ptr_ != other.ptr_;
    }
    bool operator<(const string_iterator& other) const
    {
        return this->ptr_ < other.ptr_;
    }
    bool operator>(const string_iterator& other) const
    {
        return this->ptr_ > other.ptr_;
    }
    bool operator<=(const string_iterator& other) const
    {
        return this->ptr_ <= other.ptr_;
    }
    bool operator>=(const string_iterator& other) const
    {
        return this->ptr_ >= other.ptr_;
    }
    ptrdiff_t operator-(const string_iterator& other) const
    {
        bool this_is_behind = this->ptr_ > other.ptr_;
        const auto* begin = this_is_behind ? other.ptr_ : this->ptr_;
        const auto* end = this_is_behind ? this->ptr_ : other.ptr_;
        assert(_is_valid_codepoint_leading(*begin));
        assert(_is_valid_codepoint_leading(*end));
        ptrdiff_t n = 0;
        for (; begin != end; ++begin)
            n += ptrdiff_t(_is_valid_codepoint_leading(*begin));
        return this_is_behind ? n : -n;
    }
    char21_t operator*() const
    {
        assert((*ptr_ > 0u) && _is_valid_codepoint_leading(*ptr_));
        switch (num_bytes())
        {
        case 1:
            return char21_t(ptr_[0]);
        case 2:
            return char21_t(
                ((ptr_[0] & 0b0001'1111) << 6) |
                ((ptr_[1] & 0b0011'1111)));
        case 3:
            return char21_t(
                ((ptr_[0] & 0b0000'1111) << 12) |
                ((ptr_[1] & 0b0011'1111) << 6) |
                ((ptr_[2] & 0b0011'1111)));
        case 4:
            return char21_t(
                ((ptr_[0] & 0b0000'0111) << 18) |
                ((ptr_[1] & 0b0011'1111) << 12) |
                ((ptr_[2] & 0b0011'1111) << 6) |
                ((ptr_[3] & 0b0011'1111)));
        default:
            return char21_t(0);
        }
    }
    ptrdiff_t byte_difference(const string_iterator& other) const
    {
        return ptrdiff_t(this->ptr_ - other.ptr_);
    }
    void apply_pointer_offset(ptrdiff_t n)
    {
        ptr_ += n;
    }
    void apply_offset(ptrdiff_t n)
    {
        if (n >= 0)
        {
            for (ptrdiff_t i = 0u; i < n;)
                i += ptrdiff_t(_is_valid_codepoint_leading(*++ptr_));
        }
        else
        {
            for (ptrdiff_t i = 0u; i < n;)
                i += ptrdiff_t(_is_valid_codepoint_leading(*--ptr_));
        }
    }
    void apply_offset(ptrdiff_t n, const string_iterator& end)
    {
        if (n >= 0)
        {
            for (ptrdiff_t i = 0u; (i < n) && (ptr_ <= end.ptr_);)
                i += ptrdiff_t(_is_valid_codepoint_leading(*++ptr_));
        }
        else
        {
            for (ptrdiff_t i = 0u; (i < -n) && (ptr_ >= end.ptr_);)
                i += ptrdiff_t(_is_valid_codepoint_leading(*--ptr_));
        }
    }
    size_t previous_num_bytes() const
    {
        if (ptr_[-1] < 0b1000'0000u)
        {
            return 1u;
        }
        else if (ptr_[-2] >= 0b1100'0000u)
        {
            assert(_is_valid_codepoint_tailing(ptr_[-1]));
            return 2u;
        }
        else if (ptr_[-3] >= 0b1100'0000u)
        {
            assert(_is_valid_codepoint_tailing(ptr_[-1]));
            assert(_is_valid_codepoint_tailing(ptr_[-2]));
            return 3u;
        }
        else
        {
            assert(_is_valid_codepoint_tailing(ptr_[-1]));
            assert(_is_valid_codepoint_tailing(ptr_[-2]));
            assert(_is_valid_codepoint_tailing(ptr_[-3]));
            assert((*ptr_ > 0u) && _is_valid_codepoint_leading(ptr_[-4]));
            return 4u;
        }
    }
    size_t num_bytes() const
    {
        assert((*ptr_ > 0u) && _is_valid_codepoint_leading(*ptr_));
        if (*ptr_ < 0b1000'0000u)
        {
            return 1u;
        }
        else if (*ptr_ < 0b1110'0000u)
        {
            assert(_is_valid_codepoint_tailing(ptr_[1]));
            return 2u;
        }
        else if (*ptr_ < 0b1111'0000u)
        {
            assert(_is_valid_codepoint_tailing(ptr_[1]));
            assert(_is_valid_codepoint_tailing(ptr_[2]));
            return 3u;
        }
        else
        {
            assert(_is_valid_codepoint_tailing(ptr_[1]));
            assert(_is_valid_codepoint_tailing(ptr_[2]));
            assert(_is_valid_codepoint_tailing(ptr_[3]));
            return 4u;
        }
    }
};
}
enum class trilean_t : uint8_t
{
    True,
    False,
    Unknown
};
struct u8string_view
{
    using iterator = utf8::string_iterator;
    static constexpr ptrdiff_t string_size_unknown = -1;
    iterator begin_{};
    iterator end_{};
    mutable trilean_t ascii_only_ = trilean_t::Unknown;
    mutable ptrdiff_t string_size_ = string_size_unknown;
    u8string_view() = default;
    u8string_view(iterator begin, iterator end) :
        begin_{begin}, end_{end}
    {
        assert(begin <= end);
    }
    u8string_view(iterator begin, iterator end, bool ascii_only) :
        u8string_view{begin, end}
    {
        ascii_only_ = ascii_only ? trilean_t::True : trilean_t::False;
        if (ascii_only)
            string_size_ = byte_size();
    }
    u8string_view(iterator begin, iterator end, size_t string_size) :
        u8string_view{begin, end}
    {
        string_size_ = string_size;
        ascii_only_ = (size() == byte_size()) ?
            trilean_t::True : trilean_t::False;
    }
    template<typename CharT>
    u8string_view(const CharT* begin, const CharT* end) :
        begin_{(const utf8::char_t*)begin}, end_{(const utf8::char_t*)end}
    {
        static_assert(sizeof(CharT) == 1u, WL_ERROR_INTERNAL);
    }
    const utf8::char_t* byte_data() const
    {
        return begin_.get_pointer();
    }
    size_t byte_size() const
    {
        return size_t(end_.byte_difference(begin_));
    }
    const utf8::char_t* byte_begin() const
    {
        return byte_data();
    }
    const utf8::char_t* byte_end() const
    {
        return byte_data() + byte_size();
    }
    size_t size() const
    {
        if (string_size_ == string_size_unknown)
            string_size_ = utf8::get_string_size(byte_data(), byte_size());
        ascii_only_ = (string_size_ == byte_size()) ?
            trilean_t::True : trilean_t::False;
        return string_size_;
    }
    const char* c_str() const
    {
        return reinterpret_cast<const char*>(byte_data());
    }
    iterator begin() const
    {
        return begin_;
    }
    iterator end() const
    {
        return end_;
    }
    WL_INLINE trilean_t* _ascii_only_ptr() const
    {
        return &ascii_only_;
    }
    bool ascii_only() const
    {
        auto only = _ascii_only_ptr();
        if (*only == trilean_t::Unknown)
        {
            if (utf8::is_ascii_only(byte_data(), byte_size()))
                *only = trilean_t::True;
            else
                *only = trilean_t::False;
        }
        return *only == trilean_t::True ? true : false;
    }
};
union u8string
{
    static constexpr size_t small_string_byte_size = 28u; // excluding \0
    static_assert(sizeof(char) == 1u, WL_ERROR_SIZEOF_CHAR);
    using iterator = utf8::string_iterator;
    
    struct static_t
    {
        static constexpr size_t capacity_ = small_string_byte_size;
        bool is_static_ = true;
        mutable trilean_t ascii_only_ = trilean_t::Unknown;
        uint8_t byte_size_ = 0u;
        utf8::char_t string_[small_string_byte_size + 1u];
        static_t() = default;
        static_t(size_t byte_size, trilean_t ascii_only) :
            byte_size_{uint8_t(byte_size)}, ascii_only_{ascii_only}
        {
            assert(byte_size_ <= capacity_);
        }
        void place_null_character()
        {
            string_[byte_size_] = utf8::null_character;
        }
        template<bool PlaceNull = true, bool UpdateASCII = true>
        void push(utf8::char_t ch)
        {
            assert(byte_size_ + 1u <= size_t(capacity_));
            string_[byte_size_++] = ch;
            if constexpr (UpdateASCII)
                if (!utf8::is_ascii(ch))
                    ascii_only_ = trilean_t::False;
            if constexpr (PlaceNull)
                place_null_character();
        }
        template<bool PlaceNull = true>
        void push(const utf8::char_t* ch, size_t size)
        {
            assert(byte_size_ + size <= size_t(capacity_));
            utils::restrict_copy_n(ch, size, string_ + byte_size_);
            byte_size_ += uint8_t(size);
            ascii_only_ = trilean_t::Unknown;
            if constexpr (PlaceNull)
                place_null_character();
        }
    };
    struct dynamic_t
    {
        bool is_static_ = false;
        mutable trilean_t ascii_only_ = trilean_t::Unknown;
        uint64_t byte_size_ = 0u;
        uint64_t capacity_ = 0u;
        utf8::char_t* string_ = nullptr;
        dynamic_t(size_t byte_size, trilean_t ascii_only) :
            byte_size_{byte_size}, ascii_only_{ascii_only}
        {
            resize_buffer(byte_size_);
        }
        dynamic_t(size_t byte_size, size_t capacity, trilean_t ascii_only) :
            byte_size_{byte_size}, ascii_only_{ascii_only}
        {
            capacity = std::max(byte_size, capacity);
            resize_buffer(capacity);
        }
        ~dynamic_t()
        {
            free_buffer();
        }
        dynamic_t(const dynamic_t& other)
        {
            ascii_only_ = other.ascii_only_;
            byte_size_  = other.byte_size_;
            resize_buffer(byte_size_);
            std::copy_n(other.string_, byte_size_ + 1u, string_);
        }
        dynamic_t(dynamic_t&& other)
        {
            ascii_only_ = other.ascii_only_;
            byte_size_  = other.byte_size_;
            std::swap(string_, other.string_);
            std::swap(capacity_, other.capacity_);
        }
        dynamic_t& operator=(const dynamic_t& other)
        {
            free_buffer();
            ascii_only_ = other.ascii_only_;
            byte_size_  = other.byte_size_;
            resize_buffer(byte_size_);
            std::copy_n(other.string_, byte_size_ + 1u, string_);
            return *this;
        }
        dynamic_t& operator=(dynamic_t&& other)
        {
            ascii_only_ = other.ascii_only_;
            byte_size_  = other.byte_size_;
            std::swap(string_, other.string_);
            std::swap(capacity_, other.capacity_);
            return *this;
        }
        void place_null_character()
        {
            string_[byte_size_] = utf8::null_character;
        }
        template<bool PlaceNull = true, bool UpdateASCII = true>
        void push(utf8::char_t ch)
        {
            if (byte_size_ >= capacity_)
                grow_buffer();
            assert(byte_size_ + 1u <= capacity_);
            string_[byte_size_++] = ch;
            if constexpr (UpdateASCII)
                if (!utf8::is_ascii(ch))
                    ascii_only_ = trilean_t::False;
            if constexpr (PlaceNull)
                place_null_character();
        }
        template<bool PlaceNull = true>
        void push(const utf8::char_t* ch, size_t size)
        {
            if (byte_size_ + size > capacity_)
                grow_buffer(size);
            assert(byte_size_ + size <= capacity_);
            utils::restrict_copy_n(ch, size, string_ + byte_size_);
            byte_size_ += uint8_t(size);
            ascii_only_ = trilean_t::Unknown;
            if constexpr (PlaceNull)
                place_null_character();
        }
        void resize_buffer(size_t new_capacity)
        {
            if (new_capacity <= capacity_)
                return;
            auto new_buffer_size = new_capacity + 1u;
            auto storage = (uint8_t*)std::realloc(string_, new_buffer_size);
            if (!storage)
                throw std::bad_alloc();
            string_ = storage;
            capacity_ = new_capacity;
        }
        void grow_buffer(size_t extra = 0u)
        {
            auto new_buffer_size = capacity_ + 1u;
            new_buffer_size += std::max(size_t(capacity_ + 1u), extra);
            auto storage = (uint8_t*)std::realloc(string_, new_buffer_size);
            if (!storage)
                throw std::bad_alloc();
            string_ = storage;
            capacity_ = new_buffer_size - 1u;
        }
        void free_buffer()
        {
            assert(string_);
            std::free(string_);
            string_ = nullptr;
            capacity_ = 0u;
        }
    };
    
    static_assert(sizeof(bool) == 1u, WL_ERROR_INTERNAL);
    static_assert(sizeof(static_t) == 32u, WL_ERROR_INTERNAL);
    static_assert(sizeof(dynamic_t) == 32u, WL_ERROR_INTERNAL);
    static_t  static_;
    dynamic_t dynamic_;
    u8string()
    {
        new(&static_) static_t(0u, trilean_t::True);
        static_.place_null_character();
        assert(check_validity());
    }
    explicit u8string(size_t byte_size)
    {
        if (byte_size <= small_string_byte_size)
        {
            new(&static_) static_t(byte_size, trilean_t::Unknown);
            static_.place_null_character();
        }
        else
        {
            new(&dynamic_) dynamic_t(byte_size, trilean_t::Unknown);
            dynamic_.place_null_character();
        }
    }
    explicit u8string(size_t byte_size, bool ascii_only) : u8string(byte_size)
    {
        set_ascii_only(ascii_only);
    }
    u8string(const iterator& begin, const iterator& end) :
        u8string(begin.get_pointer(), end.byte_difference(begin))
    {
    }
    u8string(const iterator& begin, const iterator& end, bool ascii_only) :
        u8string(begin.get_pointer(), end.byte_difference(begin), ascii_only)
    {
    }
    template<typename CharT>
    u8string(const CharT* str, const size_t byte_size)
    {
        assert(ptrdiff_t(byte_size) >= 0);
        if (byte_size <= small_string_byte_size)
        {
            new(&static_) static_t(byte_size, trilean_t::Unknown);
            if (byte_size > 0)
                utils::restrict_copy_n(str, byte_size, static_.string_);
            static_.place_null_character();
        }
        else
        {
            new(&dynamic_) dynamic_t(byte_size, trilean_t::Unknown);
            utils::restrict_copy_n(str, byte_size, dynamic_.string_);
            dynamic_.place_null_character();
        }
        assert(check_validity());
    }
    template<typename CharT>
    u8string(const CharT* str, const size_t byte_size,
        bool ascii_only) : u8string(str, byte_size)
    {
        set_ascii_only(ascii_only);
        assert(check_validity());
    }
    template<typename CharT>
    explicit u8string(const CharT* str) : u8string(str, std::strlen(str))
    {
        assert(check_validity());
    }
    template<size_t N>
    explicit u8string(const char(&str)[N]) :
        u8string(&str[0], N - 1u)
    {
        static_assert(N >= 1u, WL_ERROR_INTERNAL);
        assert(check_validity());
    }
    template<size_t N>
    explicit u8string(const char(&str)[N], bool ascii_only) : u8string(str)
    {
        set_ascii_only(ascii_only);
        assert(check_validity());
    }
    explicit u8string(const u8string_view& other) :
        u8string(other.byte_begin(), other.byte_size())
    {
    }
    u8string(const u8string& other) : u8string()
    {
        copy_from(other);
        assert(check_validity());
    }
    u8string(u8string&& other) : u8string()
    {
        swap_with(other);
        assert(check_validity());
    }
    ~u8string()
    {
        destroy();
    }
    u8string& operator=(const u8string& other)
    {
        destroy();
        copy_from(other);
        return *this;
    }
    u8string& operator=(u8string&& other)
    {
        swap_with(other);
        return *this;
    }
    void destroy()
    {
        if (!is_static())
            dynamic_.~dynamic_t();
    }
    void swap_with(u8string& other)
    {
        char buffer[sizeof(u8string)];
        std::memcpy(buffer, &other, sizeof(u8string));
        std::memcpy(&other, this, sizeof(u8string));
        std::memcpy(this, buffer, sizeof(u8string));
    }
    void copy_from(const u8string& other)
    {
        if (other.is_static())
        {
            new(&static_) static_t(other.static_);
        }
        else
        {
            const auto byte_size = other.byte_size();
            if (byte_size <= small_string_byte_size)
            { // convert to static
                new(&static_) static_t(byte_size, other.static_.ascii_only_);
                utils::restrict_copy_n(
                    other.byte_data(), byte_size + 1u, byte_data());
            }
            else
            { // remain dynamic
                new(&dynamic_) dynamic_t(byte_size, other.static_.ascii_only_);
                utils::restrict_copy_n(
                    other.byte_data(), byte_size + 1u, byte_data());
            }
        }
    }
    void set_dynamic_capacity(size_t capacity)
    {
        if (is_static())
        {
            const auto copy = static_;
            const auto byte_size = copy.byte_size_;
            const auto ascii_only = copy.ascii_only_;
            new(&dynamic_) dynamic_t(byte_size, capacity, ascii_only);
            utils::restrict_copy_n(
                copy.string_, size_t(byte_size) + 1u, dynamic_.string_);
            dynamic_.ascii_only_ = copy.ascii_only_;
        }
        else
        {
            dynamic_.resize_buffer(capacity);
        }
    }
    WL_INLINE bool is_static() const
    {
        bool value;
        std::memcpy(&value, &static_.is_static_, sizeof(bool));
        return value;
    }
    WL_INLINE size_t byte_size() const
    {
        return is_static() ? static_.byte_size_ : dynamic_.byte_size_;
    }
    WL_INLINE size_t capacity() const
    {
        return is_static() ? static_.capacity_ : dynamic_.capacity_;
    }
    WL_INLINE size_t size() const
    {
        size_t string_size = 0;
        if (is_static())
        {
            if (static_.ascii_only_ == trilean_t::True)
                return static_.byte_size_;
            else
                string_size = utf8::get_string_size(
                    static_.string_, static_.byte_size_);
        }
        else
        {
            if (dynamic_.ascii_only_ == trilean_t::True)
                return dynamic_.byte_size_;
            else
                string_size = utf8::get_string_size(
                    dynamic_.string_, dynamic_.byte_size_);
        }
        set_ascii_only(string_size == byte_size());
        return string_size;
    }
    WL_INLINE utf8::char_t* byte_data()
    {
        return is_static() ? static_.string_ : dynamic_.string_;
    }
    WL_INLINE const utf8::char_t* byte_data() const
    {
        return is_static() ? static_.string_ : dynamic_.string_;
    }
    WL_INLINE const char* c_str() const
    {
        return reinterpret_cast<const char*>(byte_data());
    }
    WL_INLINE trilean_t* _ascii_only_ptr() const
    {
        return is_static() ? &static_.ascii_only_ : &dynamic_.ascii_only_;
    }
    bool ascii_only() const
    {
        auto only = _ascii_only_ptr();
        if (*only == trilean_t::Unknown)
        {
            if (utf8::is_ascii_only(byte_data(), byte_size()))
                *only = trilean_t::True;
            else
                *only = trilean_t::False;
        }
        return *only == trilean_t::True ? true : false;
    }
    void set_ascii_only(bool ascii_only) const
    {
        *_ascii_only_ptr() = ascii_only ? trilean_t::True : trilean_t::False;
    }
    utf8::char_t* byte_begin()
    {
        return byte_data();
    }
    utf8::char_t* byte_end()
    {
        return byte_data() + byte_size();
    }
    const utf8::char_t* byte_begin() const
    {
        return byte_data();
    }
    const utf8::char_t* byte_end() const
    {
        return byte_data() + byte_size();
    }
    iterator begin() const
    {
        return iterator{byte_begin()};
    }
    iterator end() const
    {
        return iterator{byte_end()};
    }
    operator u8string_view() const
    {
        return u8string_view{begin(), end()};
    }
    void place_null_character()
    {
        if (is_static())
            static_.place_null_character();
        else
            dynamic_.place_null_character();
    }
    template<bool PlaceNull = true>
    void append(const utf8::char_t* str, const size_t append_size)
    {
        const size_t new_byte_size = byte_size() + append_size;
        if (is_static())
        {
            if (new_byte_size > small_string_byte_size)
            {
                set_dynamic_capacity(new_byte_size);
                assert(!is_static());
                dynamic_.push<PlaceNull>(str, append_size);
            }
            else
            {
                static_.push<PlaceNull>(str, append_size);
            }
        }
        else
        {
            dynamic_.push<PlaceNull>(str, append_size);
        }
    }
    template<bool PlaceNull = true, bool UpdateASCII = true>
    void append(const utf8::char_t ch)
    {
        const size_t new_byte_size = byte_size() + 1u;
        if (is_static())
        {
            if (new_byte_size > small_string_byte_size)
            {
                set_dynamic_capacity(new_byte_size);
                assert(!is_static());
                dynamic_.push<PlaceNull, UpdateASCII>(ch);
            }
            else
            {
                static_.push<PlaceNull, UpdateASCII>(ch);
            }
        }
        else
        {
            dynamic_.push<PlaceNull, UpdateASCII>(ch);
        }
    }
    template<bool PlaceNull = true>
    u8string& join(const u8string& other)
    {
        append<PlaceNull>(other.byte_data(), other.byte_size());
        return *this;
    }
    template<bool PlaceNull = true, size_t N>
    u8string& join(const char(&str)[N])
    {
        static_assert(N >= 1u, WL_ERROR_INTERNAL);
        append<PlaceNull>((const utf8::char_t*)str, N - 1u);
        return *this;
    }
    template<bool PlaceNull = true>
    u8string& join(const u8string_view& other)
    {
        append<PlaceNull>(other.byte_data(), other.byte_size());
        return *this;
    }
    bool check_validity() const
    {
        if (capacity() < byte_size())
        {
            return false;
        }
        try
        {
            utf8::get_string_size<true>(byte_data(), byte_size());
            return true;
        }
        catch (std::logic_error&)
        {
            return false;
        }
    }
    std::string _ascii_string() const
    {
        std::string str(size(), ' ');
        const size_t size = this->size();
        auto begin = this->begin();
        auto end = this->end();
        for (size_t i = 0; i < size; ++i, ++begin)
        {
            const auto cp = *begin;
            str[i] = cp > 128u ? '?' : char(cp);
        }
        assert(begin == end);
        return str;
    }
    std::string _string_info() const
    {
        std::string info = "{ ";
        info += "is_static=" +
            std::string(is_static() ? "true" : "false") + ", ";
        info += "ascii_only=" +
            std::string(ascii_only() ? "true" : "false") + ", ";
        info += "size=" + std::to_string(size()) + ", ";
        info += "byte_size=" + std::to_string(byte_size()) + ", ";
        info += "capacity=" + std::to_string(capacity()) + ", ";
        info += "content=";
        info += _ascii_string();
        info += " }";
        return info;
    }
};
}
namespace wl
{
template<int64_t Id, typename Pattern>
struct _named_pattern
{
    Pattern pattern;
};
template<int64_t Id>
struct _named_replacement
{
};
template<typename... Head>
struct _pattern_blank
{
    static_assert(sizeof...(Head) <= 1u, WL_ERROR_INTERNAL);
};
template<typename... Head>
struct _pattern_blank_sequence
{
    static_assert(sizeof...(Head) <= 1u, WL_ERROR_INTERNAL);
};
template<typename... Head>
struct _pattern_blank_null_sequence
{
    static_assert(sizeof...(Head) <= 1u, WL_ERROR_INTERNAL);
};
template<typename... Patterns>
struct _pattern_alternatives
{
    static constexpr size_t size = sizeof...(Patterns);
    std::tuple<Patterns...> patterns;
    template<size_t I>
    auto get() const & -> const auto&
    {
        return std::get<I>(patterns);
    }
    template<size_t I>
    auto get() &&
    {
        return std::get<I>(patterns);
    }
};
template<typename Pattern>
struct _pattern_repeated
{
    Pattern pattern;
    size_t min;
    size_t max;
    bool max_is_infinity() const
    {
        return max == size_t(const_int_infinity);
    }
};
template<typename Pattern>
struct _pattern_except
{
    Pattern pattern;
};
template<typename Pattern>
struct _pattern_longest
{
    Pattern pattern;
};
template<typename Pattern>
struct _pattern_shortest
{
    Pattern pattern;
};
template<typename Left, typename Right>
struct _pattern_rule
{
    Left left;
    Right right;
};
template<typename... Patterns>
struct _string_expression
{
    std::tuple<Patterns...> patterns;
    template<size_t I>
    auto get() const &
    {
        return std::get<I>(patterns);
    }
    template<size_t I>
    auto get() &&
    {
        return std::get<I>(std::move(patterns));
    }
};
template<typename Pattern, typename Condition>
struct _condition
{
    Pattern pattern;
    Condition condition;
};
template<int64_t Id, typename Pattern>
auto pattern(const_int<Id>, Pattern&& pattern)
{
    return _named_pattern<Id, remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}
template<typename Left, typename Right>
auto rule(Left&& left, Right&& right)
{
    return _pattern_rule<remove_cvref_t<Left>, remove_cvref_t<Right>>{
        std::forward<decltype(left)>(left),
        std::forward<decltype(right)>(right)};
}
template<typename Left, typename Right>
auto rule_delayed(Left&& left, Right&& right)
{
    return rule(std::forward<decltype(left)>(left),
            std::forward<decltype(right)>(right));
}
template<typename... Head>
auto blank(const Head&...)
{
    return _pattern_blank<Head...>{};
}
template<typename... Head>
auto blank_sequence(const Head&...)
{
    return _pattern_blank_sequence<Head...>{};
}
template<typename... Head>
auto blank_null_sequence(const Head&...)
{
    return _pattern_blank_null_sequence<Head...>{};
}
template<typename... Patterns>
auto alternatives(Patterns&&... patterns)
{
    return _pattern_alternatives<remove_cvref_t<Patterns>...>{
        std::make_tuple(std::forward<decltype(patterns)>(patterns)...)};
}
template<typename Pattern, typename Spec>
auto _repeated_impl(Pattern&& pattern, const Spec min, const Spec max)
{
    if (max < min || min < Spec(0))
        throw std::logic_error(WL_ERROR_REPEATED_INVALID_SPEC);
    return _pattern_repeated<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern), size_t(min), size_t(max)};
}
template<typename Pattern, typename Spec>
auto repeated(Pattern&& pattern, const Spec& spec)
{
    if constexpr (array_rank_v<Spec> == 0u)
    {
        static_assert(is_integral_v<Spec>, WL_ERROR_REPEATED_SPEC);
        return _repeated_impl(std::forward<decltype(pattern)>(pattern),
            Spec(1), spec);
    }
    else
    {
        using XV = value_type_t<Spec>;
        static_assert(array_rank_v<Spec> == 1u && is_integral_v<XV>,
            WL_ERROR_REPEATED_SPEC);
        if (spec.size() != 2u)
            throw std::logic_error(WL_ERROR_REPEATED_SPEC);
        std::array<XV, 2u> valspec;
        spec.copy_to(valspec.data());
        return _repeated_impl(std::forward<decltype(pattern)>(pattern),
            valspec[0], valspec[1]);
    }
}
template<typename Pattern>
auto repeated(Pattern&& pattern)
{
    return _repeated_impl(std::forward<decltype(pattern)>(pattern),
        size_t(1), size_t(const_int_infinity));
}
template<typename Pattern, typename Spec>
auto repeated_null(Pattern&& pattern, const Spec& spec)
{
    if constexpr (array_rank_v<Spec> == 0u)
    {
        static_assert(is_integral_v<Spec>, WL_ERROR_REPEATED_SPEC);
        return _repeated_impl(std::forward<decltype(pattern)>(pattern),
            Spec(0), spec);
    }
    else
    {
        return repeated(std::forward<decltype(pattern)>(pattern), spec);
    }
}
template<typename Pattern>
auto repeated_null(Pattern&& pattern)
{
    return _repeated_impl(std::forward<decltype(pattern)>(pattern),
        size_t(0), size_t(const_int_infinity));
}
template<typename Pattern>
auto except(Pattern&& pattern)
{
    return _pattern_except<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}
template<typename Pattern>
auto longest(Pattern&& pattern)
{
    return _pattern_longest<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}
template<typename Pattern>
auto shortest(Pattern&& pattern)
{
    return _pattern_shortest<remove_cvref_t<Pattern>>{
        std::forward<decltype(pattern)>(pattern)};
}
template<typename Pattern, typename Condition>
auto condition(Pattern&& pattern, Condition&& condition)
{
    return _condition<remove_cvref_t<Pattern>, remove_cvref_t<Condition>>{
        std::forward<decltype(pattern)>(pattern),
        std::forward<decltype(condition)>(condition)};
}
template<int64_t Id>
auto _replacement(const_int<Id>)
{
    return _named_replacement<Id>{};
}
template<typename... Patterns>
auto string_expression(Patterns&&... patterns)
{
    return _string_expression<remove_cvref_t<Patterns>...>{
        std::make_tuple(std::forward<decltype(patterns)>(patterns)...)};
}
}
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
        lo = _mulx_u64(mantissa, other.mantissa, &hi);
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
    assert(!isnan(x));
    assert(!isinf(x));
    char* ptr = first;
    if (x == 0.0)
    {
        if constexpr (AlwaysPrintSign)
            *ptr++ = '+';
        *ptr++ = '0';
        *ptr++ = '.';
        *ptr = '\0';
        return string_view(first, ptr);
    }
    else
    {
        if (x < 0)
        {
            *ptr++ = '-';
            x = -x;
        }
        else if constexpr (AlwaysPrintSign)
        {
            *ptr++ = '+';
        }
        int length = 0;
        int exponent_10 = 0;
        grisu2(x, ptr, length, exponent_10);
        ptr = format(ptr, length, exponent_10);
        return string_view(first, ptr);
    }
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
}
#define PCRE2_STATIC 1
#define PCRE2_CODE_UNIT_WIDTH 8
/*************************************************
*       Perl-Compatible Regular Expressions      *
*************************************************/
/* This is the public header file for the PCRE library, second API, to be
#included by applications that call PCRE2 functions.
           Copyright (c) 2016-2019 University of Cambridge
-----------------------------------------------------------------------------
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the University of Cambridge nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-----------------------------------------------------------------------------
*/
#ifndef PCRE2_H_IDEMPOTENT_GUARD
#define PCRE2_H_IDEMPOTENT_GUARD
/* The current PCRE version information. */
#define PCRE2_MAJOR           10
#define PCRE2_MINOR           34
#define PCRE2_PRERELEASE      
#define PCRE2_DATE            2019-11-21
/* When an application links to a PCRE DLL in Windows, the symbols that are
imported have to be identified as such. When building PCRE2, the appropriate
export setting is defined in pcre2_internal.h, which includes this file. So we
don't change existing definitions of PCRE2_EXP_DECL. */
#if defined(_WIN32) && !defined(PCRE2_STATIC)
#  ifndef PCRE2_EXP_DECL
#    define PCRE2_EXP_DECL  extern __declspec(dllimport)
#  endif
#endif
/* By default, we use the standard "extern" declarations. */
#ifndef PCRE2_EXP_DECL
#  ifdef __cplusplus
#    define PCRE2_EXP_DECL  extern "C"
#  else
#    define PCRE2_EXP_DECL  extern
#  endif
#endif
/* When compiling with the MSVC compiler, it is sometimes necessary to include
a "calling convention" before exported function names. (This is secondhand
information; I know nothing about MSVC myself). For example, something like
  void __cdecl function(....)
might be needed. In order so make this easy, all the exported functions have
PCRE2_CALL_CONVENTION just before their names. It is rarely needed; if not
set, we ensure here that it has no effect. */
#ifndef PCRE2_CALL_CONVENTION
#define PCRE2_CALL_CONVENTION
#endif
/* Have to include limits.h, stdlib.h, and inttypes.h to ensure that size_t and
uint8_t, UCHAR_MAX, etc are defined. Some systems that do have inttypes.h do
not have stdint.h, which is why we use inttypes.h, which according to the C
standard is a superset of stdint.h. If none of these headers are available,
the relevant values must be provided by some other means. */
/* Allow for C++ users compiling this directly. */
#ifdef __cplusplus
extern "C" {
#endif
/* The following option bits can be passed to pcre2_compile(), pcre2_match(),
or pcre2_dfa_match(). PCRE2_NO_UTF_CHECK affects only the function to which it
is passed. Put these bits at the most significant end of the options word so
others can be added next to them */
#define PCRE2_ANCHORED            0x80000000u
#define PCRE2_NO_UTF_CHECK        0x40000000u
#define PCRE2_ENDANCHORED         0x20000000u
/* The following option bits can be passed only to pcre2_compile(). However,
they may affect compilation, JIT compilation, and/or interpretive execution.
The following tags indicate which:
C   alters what is compiled by pcre2_compile()
J   alters what is compiled by pcre2_jit_compile()
M   is inspected during pcre2_match() execution
D   is inspected during pcre2_dfa_match() execution
*/
#define PCRE2_ALLOW_EMPTY_CLASS   0x00000001u  /* C       */
#define PCRE2_ALT_BSUX            0x00000002u  /* C       */
#define PCRE2_AUTO_CALLOUT        0x00000004u  /* C       */
#define PCRE2_CASELESS            0x00000008u  /* C       */
#define PCRE2_DOLLAR_ENDONLY      0x00000010u  /*   J M D */
#define PCRE2_DOTALL              0x00000020u  /* C       */
#define PCRE2_DUPNAMES            0x00000040u  /* C       */
#define PCRE2_EXTENDED            0x00000080u  /* C       */
#define PCRE2_FIRSTLINE           0x00000100u  /*   J M D */
#define PCRE2_MATCH_UNSET_BACKREF 0x00000200u  /* C J M   */
#define PCRE2_MULTILINE           0x00000400u  /* C       */
#define PCRE2_NEVER_UCP           0x00000800u  /* C       */
#define PCRE2_NEVER_UTF           0x00001000u  /* C       */
#define PCRE2_NO_AUTO_CAPTURE     0x00002000u  /* C       */
#define PCRE2_NO_AUTO_POSSESS     0x00004000u  /* C       */
#define PCRE2_NO_DOTSTAR_ANCHOR   0x00008000u  /* C       */
#define PCRE2_NO_START_OPTIMIZE   0x00010000u  /*   J M D */
#define PCRE2_UCP                 0x00020000u  /* C J M D */
#define PCRE2_UNGREEDY            0x00040000u  /* C       */
#define PCRE2_UTF                 0x00080000u  /* C J M D */
#define PCRE2_NEVER_BACKSLASH_C   0x00100000u  /* C       */
#define PCRE2_ALT_CIRCUMFLEX      0x00200000u  /*   J M D */
#define PCRE2_ALT_VERBNAMES       0x00400000u  /* C       */
#define PCRE2_USE_OFFSET_LIMIT    0x00800000u  /*   J M D */
#define PCRE2_EXTENDED_MORE       0x01000000u  /* C       */
#define PCRE2_LITERAL             0x02000000u  /* C       */
#define PCRE2_MATCH_INVALID_UTF   0x04000000u  /*   J M D */
/* An additional compile options word is available in the compile context. */
#define PCRE2_EXTRA_ALLOW_SURROGATE_ESCAPES  0x00000001u  /* C */
#define PCRE2_EXTRA_BAD_ESCAPE_IS_LITERAL    0x00000002u  /* C */
#define PCRE2_EXTRA_MATCH_WORD               0x00000004u  /* C */
#define PCRE2_EXTRA_MATCH_LINE               0x00000008u  /* C */
#define PCRE2_EXTRA_ESCAPED_CR_IS_LF         0x00000010u  /* C */
#define PCRE2_EXTRA_ALT_BSUX                 0x00000020u  /* C */
/* These are for pcre2_jit_compile(). */
#define PCRE2_JIT_COMPLETE        0x00000001u  /* For full matching */
#define PCRE2_JIT_PARTIAL_SOFT    0x00000002u
#define PCRE2_JIT_PARTIAL_HARD    0x00000004u
#define PCRE2_JIT_INVALID_UTF     0x00000100u
/* These are for pcre2_match(), pcre2_dfa_match(), pcre2_jit_match(), and
pcre2_substitute(). Some are allowed only for one of the functions, and in
these cases it is noted below. Note that PCRE2_ANCHORED, PCRE2_ENDANCHORED and
PCRE2_NO_UTF_CHECK can also be passed to these functions (though
pcre2_jit_match() ignores the latter since it bypasses all sanity checks). */
#define PCRE2_NOTBOL                      0x00000001u
#define PCRE2_NOTEOL                      0x00000002u
#define PCRE2_NOTEMPTY                    0x00000004u  /* ) These two must be kept */
#define PCRE2_NOTEMPTY_ATSTART            0x00000008u  /* ) adjacent to each other. */
#define PCRE2_PARTIAL_SOFT                0x00000010u
#define PCRE2_PARTIAL_HARD                0x00000020u
#define PCRE2_DFA_RESTART                 0x00000040u  /* pcre2_dfa_match() only */
#define PCRE2_DFA_SHORTEST                0x00000080u  /* pcre2_dfa_match() only */
#define PCRE2_SUBSTITUTE_GLOBAL           0x00000100u  /* pcre2_substitute() only */
#define PCRE2_SUBSTITUTE_EXTENDED         0x00000200u  /* pcre2_substitute() only */
#define PCRE2_SUBSTITUTE_UNSET_EMPTY      0x00000400u  /* pcre2_substitute() only */
#define PCRE2_SUBSTITUTE_UNKNOWN_UNSET    0x00000800u  /* pcre2_substitute() only */
#define PCRE2_SUBSTITUTE_OVERFLOW_LENGTH  0x00001000u  /* pcre2_substitute() only */
#define PCRE2_NO_JIT                      0x00002000u  /* Not for pcre2_dfa_match() */
#define PCRE2_COPY_MATCHED_SUBJECT        0x00004000u
/* Options for pcre2_pattern_convert(). */
#define PCRE2_CONVERT_UTF                    0x00000001u
#define PCRE2_CONVERT_NO_UTF_CHECK           0x00000002u
#define PCRE2_CONVERT_POSIX_BASIC            0x00000004u
#define PCRE2_CONVERT_POSIX_EXTENDED         0x00000008u
#define PCRE2_CONVERT_GLOB                   0x00000010u
#define PCRE2_CONVERT_GLOB_NO_WILD_SEPARATOR 0x00000030u
#define PCRE2_CONVERT_GLOB_NO_STARSTAR       0x00000050u
/* Newline and \R settings, for use in compile contexts. The newline values
must be kept in step with values set in config.h and both sets must all be
greater than zero. */
#define PCRE2_NEWLINE_CR          1
#define PCRE2_NEWLINE_LF          2
#define PCRE2_NEWLINE_CRLF        3
#define PCRE2_NEWLINE_ANY         4
#define PCRE2_NEWLINE_ANYCRLF     5
#define PCRE2_NEWLINE_NUL         6
#define PCRE2_BSR_UNICODE         1
#define PCRE2_BSR_ANYCRLF         2
/* Error codes for pcre2_compile(). Some of these are also used by
pcre2_pattern_convert(). */
#define PCRE2_ERROR_END_BACKSLASH                  101
#define PCRE2_ERROR_END_BACKSLASH_C                102
#define PCRE2_ERROR_UNKNOWN_ESCAPE                 103
#define PCRE2_ERROR_QUANTIFIER_OUT_OF_ORDER        104
#define PCRE2_ERROR_QUANTIFIER_TOO_BIG             105
#define PCRE2_ERROR_MISSING_SQUARE_BRACKET         106
#define PCRE2_ERROR_ESCAPE_INVALID_IN_CLASS        107
#define PCRE2_ERROR_CLASS_RANGE_ORDER              108
#define PCRE2_ERROR_QUANTIFIER_INVALID             109
#define PCRE2_ERROR_INTERNAL_UNEXPECTED_REPEAT     110
#define PCRE2_ERROR_INVALID_AFTER_PARENS_QUERY     111
#define PCRE2_ERROR_POSIX_CLASS_NOT_IN_CLASS       112
#define PCRE2_ERROR_POSIX_NO_SUPPORT_COLLATING     113
#define PCRE2_ERROR_MISSING_CLOSING_PARENTHESIS    114
#define PCRE2_ERROR_BAD_SUBPATTERN_REFERENCE       115
#define PCRE2_ERROR_NULL_PATTERN                   116
#define PCRE2_ERROR_BAD_OPTIONS                    117
#define PCRE2_ERROR_MISSING_COMMENT_CLOSING        118
#define PCRE2_ERROR_PARENTHESES_NEST_TOO_DEEP      119
#define PCRE2_ERROR_PATTERN_TOO_LARGE              120
#define PCRE2_ERROR_HEAP_FAILED                    121
#define PCRE2_ERROR_UNMATCHED_CLOSING_PARENTHESIS  122
#define PCRE2_ERROR_INTERNAL_CODE_OVERFLOW         123
#define PCRE2_ERROR_MISSING_CONDITION_CLOSING      124
#define PCRE2_ERROR_LOOKBEHIND_NOT_FIXED_LENGTH    125
#define PCRE2_ERROR_ZERO_RELATIVE_REFERENCE        126
#define PCRE2_ERROR_TOO_MANY_CONDITION_BRANCHES    127
#define PCRE2_ERROR_CONDITION_ASSERTION_EXPECTED   128
#define PCRE2_ERROR_BAD_RELATIVE_REFERENCE         129
#define PCRE2_ERROR_UNKNOWN_POSIX_CLASS            130
#define PCRE2_ERROR_INTERNAL_STUDY_ERROR           131
#define PCRE2_ERROR_UNICODE_NOT_SUPPORTED          132
#define PCRE2_ERROR_PARENTHESES_STACK_CHECK        133
#define PCRE2_ERROR_CODE_POINT_TOO_BIG             134
#define PCRE2_ERROR_LOOKBEHIND_TOO_COMPLICATED     135
#define PCRE2_ERROR_LOOKBEHIND_INVALID_BACKSLASH_C 136
#define PCRE2_ERROR_UNSUPPORTED_ESCAPE_SEQUENCE    137
#define PCRE2_ERROR_CALLOUT_NUMBER_TOO_BIG         138
#define PCRE2_ERROR_MISSING_CALLOUT_CLOSING        139
#define PCRE2_ERROR_ESCAPE_INVALID_IN_VERB         140
#define PCRE2_ERROR_UNRECOGNIZED_AFTER_QUERY_P     141
#define PCRE2_ERROR_MISSING_NAME_TERMINATOR        142
#define PCRE2_ERROR_DUPLICATE_SUBPATTERN_NAME      143
#define PCRE2_ERROR_INVALID_SUBPATTERN_NAME        144
#define PCRE2_ERROR_UNICODE_PROPERTIES_UNAVAILABLE 145
#define PCRE2_ERROR_MALFORMED_UNICODE_PROPERTY     146
#define PCRE2_ERROR_UNKNOWN_UNICODE_PROPERTY       147
#define PCRE2_ERROR_SUBPATTERN_NAME_TOO_LONG       148
#define PCRE2_ERROR_TOO_MANY_NAMED_SUBPATTERNS     149
#define PCRE2_ERROR_CLASS_INVALID_RANGE            150
#define PCRE2_ERROR_OCTAL_BYTE_TOO_BIG             151
#define PCRE2_ERROR_INTERNAL_OVERRAN_WORKSPACE     152
#define PCRE2_ERROR_INTERNAL_MISSING_SUBPATTERN    153
#define PCRE2_ERROR_DEFINE_TOO_MANY_BRANCHES       154
#define PCRE2_ERROR_BACKSLASH_O_MISSING_BRACE      155
#define PCRE2_ERROR_INTERNAL_UNKNOWN_NEWLINE       156
#define PCRE2_ERROR_BACKSLASH_G_SYNTAX             157
#define PCRE2_ERROR_PARENS_QUERY_R_MISSING_CLOSING 158
/* Error 159 is obsolete and should now never occur */
#define PCRE2_ERROR_VERB_ARGUMENT_NOT_ALLOWED      159
#define PCRE2_ERROR_VERB_UNKNOWN                   160
#define PCRE2_ERROR_SUBPATTERN_NUMBER_TOO_BIG      161
#define PCRE2_ERROR_SUBPATTERN_NAME_EXPECTED       162
#define PCRE2_ERROR_INTERNAL_PARSED_OVERFLOW       163
#define PCRE2_ERROR_INVALID_OCTAL                  164
#define PCRE2_ERROR_SUBPATTERN_NAMES_MISMATCH      165
#define PCRE2_ERROR_MARK_MISSING_ARGUMENT          166
#define PCRE2_ERROR_INVALID_HEXADECIMAL            167
#define PCRE2_ERROR_BACKSLASH_C_SYNTAX             168
#define PCRE2_ERROR_BACKSLASH_K_SYNTAX             169
#define PCRE2_ERROR_INTERNAL_BAD_CODE_LOOKBEHINDS  170
#define PCRE2_ERROR_BACKSLASH_N_IN_CLASS           171
#define PCRE2_ERROR_CALLOUT_STRING_TOO_LONG        172
#define PCRE2_ERROR_UNICODE_DISALLOWED_CODE_POINT  173
#define PCRE2_ERROR_UTF_IS_DISABLED                174
#define PCRE2_ERROR_UCP_IS_DISABLED                175
#define PCRE2_ERROR_VERB_NAME_TOO_LONG             176
#define PCRE2_ERROR_BACKSLASH_U_CODE_POINT_TOO_BIG 177
#define PCRE2_ERROR_MISSING_OCTAL_OR_HEX_DIGITS    178
#define PCRE2_ERROR_VERSION_CONDITION_SYNTAX       179
#define PCRE2_ERROR_INTERNAL_BAD_CODE_AUTO_POSSESS 180
#define PCRE2_ERROR_CALLOUT_NO_STRING_DELIMITER    181
#define PCRE2_ERROR_CALLOUT_BAD_STRING_DELIMITER   182
#define PCRE2_ERROR_BACKSLASH_C_CALLER_DISABLED    183
#define PCRE2_ERROR_QUERY_BARJX_NEST_TOO_DEEP      184
#define PCRE2_ERROR_BACKSLASH_C_LIBRARY_DISABLED   185
#define PCRE2_ERROR_PATTERN_TOO_COMPLICATED        186
#define PCRE2_ERROR_LOOKBEHIND_TOO_LONG            187
#define PCRE2_ERROR_PATTERN_STRING_TOO_LONG        188
#define PCRE2_ERROR_INTERNAL_BAD_CODE              189
#define PCRE2_ERROR_INTERNAL_BAD_CODE_IN_SKIP      190
#define PCRE2_ERROR_NO_SURROGATES_IN_UTF16         191
#define PCRE2_ERROR_BAD_LITERAL_OPTIONS            192
#define PCRE2_ERROR_SUPPORTED_ONLY_IN_UNICODE      193
#define PCRE2_ERROR_INVALID_HYPHEN_IN_OPTIONS      194
#define PCRE2_ERROR_ALPHA_ASSERTION_UNKNOWN        195
#define PCRE2_ERROR_SCRIPT_RUN_NOT_AVAILABLE       196
#define PCRE2_ERROR_TOO_MANY_CAPTURES              197
#define PCRE2_ERROR_CONDITION_ATOMIC_ASSERTION_EXPECTED  198
/* "Expected" matching error codes: no match and partial match. */
#define PCRE2_ERROR_NOMATCH          (-1)
#define PCRE2_ERROR_PARTIAL          (-2)
/* Error codes for UTF-8 validity checks */
#define PCRE2_ERROR_UTF8_ERR1        (-3)
#define PCRE2_ERROR_UTF8_ERR2        (-4)
#define PCRE2_ERROR_UTF8_ERR3        (-5)
#define PCRE2_ERROR_UTF8_ERR4        (-6)
#define PCRE2_ERROR_UTF8_ERR5        (-7)
#define PCRE2_ERROR_UTF8_ERR6        (-8)
#define PCRE2_ERROR_UTF8_ERR7        (-9)
#define PCRE2_ERROR_UTF8_ERR8       (-10)
#define PCRE2_ERROR_UTF8_ERR9       (-11)
#define PCRE2_ERROR_UTF8_ERR10      (-12)
#define PCRE2_ERROR_UTF8_ERR11      (-13)
#define PCRE2_ERROR_UTF8_ERR12      (-14)
#define PCRE2_ERROR_UTF8_ERR13      (-15)
#define PCRE2_ERROR_UTF8_ERR14      (-16)
#define PCRE2_ERROR_UTF8_ERR15      (-17)
#define PCRE2_ERROR_UTF8_ERR16      (-18)
#define PCRE2_ERROR_UTF8_ERR17      (-19)
#define PCRE2_ERROR_UTF8_ERR18      (-20)
#define PCRE2_ERROR_UTF8_ERR19      (-21)
#define PCRE2_ERROR_UTF8_ERR20      (-22)
#define PCRE2_ERROR_UTF8_ERR21      (-23)
/* Error codes for UTF-16 validity checks */
#define PCRE2_ERROR_UTF16_ERR1      (-24)
#define PCRE2_ERROR_UTF16_ERR2      (-25)
#define PCRE2_ERROR_UTF16_ERR3      (-26)
/* Error codes for UTF-32 validity checks */
#define PCRE2_ERROR_UTF32_ERR1      (-27)
#define PCRE2_ERROR_UTF32_ERR2      (-28)
/* Miscellaneous error codes for pcre2[_dfa]_match(), substring extraction
functions, context functions, and serializing functions. They are in numerical
order. Originally they were in alphabetical order too, but now that PCRE2 is
released, the numbers must not be changed. */
#define PCRE2_ERROR_BADDATA           (-29)
#define PCRE2_ERROR_MIXEDTABLES       (-30)  /* Name was changed */
#define PCRE2_ERROR_BADMAGIC          (-31)
#define PCRE2_ERROR_BADMODE           (-32)
#define PCRE2_ERROR_BADOFFSET         (-33)
#define PCRE2_ERROR_BADOPTION         (-34)
#define PCRE2_ERROR_BADREPLACEMENT    (-35)
#define PCRE2_ERROR_BADUTFOFFSET      (-36)
#define PCRE2_ERROR_CALLOUT           (-37)  /* Never used by PCRE2 itself */
#define PCRE2_ERROR_DFA_BADRESTART    (-38)
#define PCRE2_ERROR_DFA_RECURSE       (-39)
#define PCRE2_ERROR_DFA_UCOND         (-40)
#define PCRE2_ERROR_DFA_UFUNC         (-41)
#define PCRE2_ERROR_DFA_UITEM         (-42)
#define PCRE2_ERROR_DFA_WSSIZE        (-43)
#define PCRE2_ERROR_INTERNAL          (-44)
#define PCRE2_ERROR_JIT_BADOPTION     (-45)
#define PCRE2_ERROR_JIT_STACKLIMIT    (-46)
#define PCRE2_ERROR_MATCHLIMIT        (-47)
#define PCRE2_ERROR_NOMEMORY          (-48)
#define PCRE2_ERROR_NOSUBSTRING       (-49)
#define PCRE2_ERROR_NOUNIQUESUBSTRING (-50)
#define PCRE2_ERROR_NULL              (-51)
#define PCRE2_ERROR_RECURSELOOP       (-52)
#define PCRE2_ERROR_DEPTHLIMIT        (-53)
#define PCRE2_ERROR_RECURSIONLIMIT    (-53)  /* Obsolete synonym */
#define PCRE2_ERROR_UNAVAILABLE       (-54)
#define PCRE2_ERROR_UNSET             (-55)
#define PCRE2_ERROR_BADOFFSETLIMIT    (-56)
#define PCRE2_ERROR_BADREPESCAPE      (-57)
#define PCRE2_ERROR_REPMISSINGBRACE   (-58)
#define PCRE2_ERROR_BADSUBSTITUTION   (-59)
#define PCRE2_ERROR_BADSUBSPATTERN    (-60)
#define PCRE2_ERROR_TOOMANYREPLACE    (-61)
#define PCRE2_ERROR_BADSERIALIZEDDATA (-62)
#define PCRE2_ERROR_HEAPLIMIT         (-63)
#define PCRE2_ERROR_CONVERT_SYNTAX    (-64)
#define PCRE2_ERROR_INTERNAL_DUPMATCH (-65)
#define PCRE2_ERROR_DFA_UINVALID_UTF  (-66)
/* Request types for pcre2_pattern_info() */
#define PCRE2_INFO_ALLOPTIONS            0
#define PCRE2_INFO_ARGOPTIONS            1
#define PCRE2_INFO_BACKREFMAX            2
#define PCRE2_INFO_BSR                   3
#define PCRE2_INFO_CAPTURECOUNT          4
#define PCRE2_INFO_FIRSTCODEUNIT         5
#define PCRE2_INFO_FIRSTCODETYPE         6
#define PCRE2_INFO_FIRSTBITMAP           7
#define PCRE2_INFO_HASCRORLF             8
#define PCRE2_INFO_JCHANGED              9
#define PCRE2_INFO_JITSIZE              10
#define PCRE2_INFO_LASTCODEUNIT         11
#define PCRE2_INFO_LASTCODETYPE         12
#define PCRE2_INFO_MATCHEMPTY           13
#define PCRE2_INFO_MATCHLIMIT           14
#define PCRE2_INFO_MAXLOOKBEHIND        15
#define PCRE2_INFO_MINLENGTH            16
#define PCRE2_INFO_NAMECOUNT            17
#define PCRE2_INFO_NAMEENTRYSIZE        18
#define PCRE2_INFO_NAMETABLE            19
#define PCRE2_INFO_NEWLINE              20
#define PCRE2_INFO_DEPTHLIMIT           21
#define PCRE2_INFO_RECURSIONLIMIT       21  /* Obsolete synonym */
#define PCRE2_INFO_SIZE                 22
#define PCRE2_INFO_HASBACKSLASHC        23
#define PCRE2_INFO_FRAMESIZE            24
#define PCRE2_INFO_HEAPLIMIT            25
#define PCRE2_INFO_EXTRAOPTIONS         26
/* Request types for pcre2_config(). */
#define PCRE2_CONFIG_BSR                     0
#define PCRE2_CONFIG_JIT                     1
#define PCRE2_CONFIG_JITTARGET               2
#define PCRE2_CONFIG_LINKSIZE                3
#define PCRE2_CONFIG_MATCHLIMIT              4
#define PCRE2_CONFIG_NEWLINE                 5
#define PCRE2_CONFIG_PARENSLIMIT             6
#define PCRE2_CONFIG_DEPTHLIMIT              7
#define PCRE2_CONFIG_RECURSIONLIMIT          7  /* Obsolete synonym */
#define PCRE2_CONFIG_STACKRECURSE            8  /* Obsolete */
#define PCRE2_CONFIG_UNICODE                 9
#define PCRE2_CONFIG_UNICODE_VERSION        10
#define PCRE2_CONFIG_VERSION                11
#define PCRE2_CONFIG_HEAPLIMIT              12
#define PCRE2_CONFIG_NEVER_BACKSLASH_C      13
#define PCRE2_CONFIG_COMPILED_WIDTHS        14
/* Types for code units in patterns and subject strings. */
typedef uint8_t  PCRE2_UCHAR8;
typedef uint16_t PCRE2_UCHAR16;
typedef uint32_t PCRE2_UCHAR32;
typedef const PCRE2_UCHAR8  *PCRE2_SPTR8;
typedef const PCRE2_UCHAR16 *PCRE2_SPTR16;
typedef const PCRE2_UCHAR32 *PCRE2_SPTR32;
/* The PCRE2_SIZE type is used for all string lengths and offsets in PCRE2,
including pattern offsets for errors and subject offsets after a match. We
define special values to indicate zero-terminated strings and unset offsets in
the offset vector (ovector). */
#define PCRE2_SIZE            size_t
#define PCRE2_SIZE_MAX        SIZE_MAX
#define PCRE2_ZERO_TERMINATED (~(PCRE2_SIZE)0)
#define PCRE2_UNSET           (~(PCRE2_SIZE)0)
/* Generic types for opaque structures and JIT callback functions. These
declarations are defined in a macro that is expanded for each width later. */
#define PCRE2_TYPES_LIST \
struct pcre2_real_general_context; \
typedef struct pcre2_real_general_context pcre2_general_context; \
\
struct pcre2_real_compile_context; \
typedef struct pcre2_real_compile_context pcre2_compile_context; \
\
struct pcre2_real_match_context; \
typedef struct pcre2_real_match_context pcre2_match_context; \
\
struct pcre2_real_convert_context; \
typedef struct pcre2_real_convert_context pcre2_convert_context; \
\
struct pcre2_real_code; \
typedef struct pcre2_real_code pcre2_code; \
\
struct pcre2_real_match_data; \
typedef struct pcre2_real_match_data pcre2_match_data; \
\
struct pcre2_real_jit_stack; \
typedef struct pcre2_real_jit_stack pcre2_jit_stack; \
\
typedef pcre2_jit_stack *(*pcre2_jit_callback)(void *);
/* The structures for passing out data via callout functions. We use structures
so that new fields can be added on the end in future versions, without changing
the API of the function, thereby allowing old clients to work without
modification. Define the generic versions in a macro; the width-specific
versions are generated from this macro below. */
/* Flags for the callout_flags field. These are cleared after a callout. */
#define PCRE2_CALLOUT_STARTMATCH    0x00000001u  /* Set for each bumpalong */
#define PCRE2_CALLOUT_BACKTRACK     0x00000002u  /* Set after a backtrack */
#define PCRE2_STRUCTURE_LIST \
typedef struct pcre2_callout_block { \
  uint32_t      version;           /* Identifies version of block */ \
  /* ------------------------ Version 0 ------------------------------- */ \
  uint32_t      callout_number;    /* Number compiled into pattern */ \
  uint32_t      capture_top;       /* Max current capture */ \
  uint32_t      capture_last;      /* Most recently closed capture */ \
  PCRE2_SIZE   *offset_vector;     /* The offset vector */ \
  PCRE2_SPTR    mark;              /* Pointer to current mark or NULL */ \
  PCRE2_SPTR    subject;           /* The subject being matched */ \
  PCRE2_SIZE    subject_length;    /* The length of the subject */ \
  PCRE2_SIZE    start_match;       /* Offset to start of this match attempt */ \
  PCRE2_SIZE    current_position;  /* Where we currently are in the subject */ \
  PCRE2_SIZE    pattern_position;  /* Offset to next item in the pattern */ \
  PCRE2_SIZE    next_item_length;  /* Length of next item in the pattern */ \
  /* ------------------- Added for Version 1 -------------------------- */ \
  PCRE2_SIZE    callout_string_offset; /* Offset to string within pattern */ \
  PCRE2_SIZE    callout_string_length; /* Length of string compiled into pattern */ \
  PCRE2_SPTR    callout_string;    /* String compiled into pattern */ \
  /* ------------------- Added for Version 2 -------------------------- */ \
  uint32_t      callout_flags;     /* See above for list */ \
  /* ------------------------------------------------------------------ */ \
} pcre2_callout_block; \
\
typedef struct pcre2_callout_enumerate_block { \
  uint32_t      version;           /* Identifies version of block */ \
  /* ------------------------ Version 0 ------------------------------- */ \
  PCRE2_SIZE    pattern_position;  /* Offset to next item in the pattern */ \
  PCRE2_SIZE    next_item_length;  /* Length of next item in the pattern */ \
  uint32_t      callout_number;    /* Number compiled into pattern */ \
  PCRE2_SIZE    callout_string_offset; /* Offset to string within pattern */ \
  PCRE2_SIZE    callout_string_length; /* Length of string compiled into pattern */ \
  PCRE2_SPTR    callout_string;    /* String compiled into pattern */ \
  /* ------------------------------------------------------------------ */ \
} pcre2_callout_enumerate_block; \
\
typedef struct pcre2_substitute_callout_block { \
  uint32_t      version;           /* Identifies version of block */ \
  /* ------------------------ Version 0 ------------------------------- */ \
  PCRE2_SPTR    input;             /* Pointer to input subject string */ \
  PCRE2_SPTR    output;            /* Pointer to output buffer */ \
  PCRE2_SIZE    output_offsets[2]; /* Changed portion of the output */ \
  PCRE2_SIZE   *ovector;           /* Pointer to current ovector */ \
  uint32_t      oveccount;         /* Count of pairs set in ovector */ \
  uint32_t      subscount;         /* Substitution number */ \
  /* ------------------------------------------------------------------ */ \
} pcre2_substitute_callout_block;
/* List the generic forms of all other functions in macros, which will be
expanded for each width below. Start with functions that give general
information. */
#define PCRE2_GENERAL_INFO_FUNCTIONS \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION pcre2_config(uint32_t, void *);
/* Functions for manipulating contexts. */
#define PCRE2_GENERAL_CONTEXT_FUNCTIONS \
PCRE2_EXP_DECL pcre2_general_context PCRE2_CALL_CONVENTION \
  *pcre2_general_context_copy(pcre2_general_context *); \
PCRE2_EXP_DECL pcre2_general_context PCRE2_CALL_CONVENTION \
  *pcre2_general_context_create(void *(*)(PCRE2_SIZE, void *), \
    void (*)(void *, void *), void *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_general_context_free(pcre2_general_context *);
#define PCRE2_COMPILE_CONTEXT_FUNCTIONS \
PCRE2_EXP_DECL pcre2_compile_context PCRE2_CALL_CONVENTION \
  *pcre2_compile_context_copy(pcre2_compile_context *); \
PCRE2_EXP_DECL pcre2_compile_context PCRE2_CALL_CONVENTION \
  *pcre2_compile_context_create(pcre2_general_context *);\
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_compile_context_free(pcre2_compile_context *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_bsr(pcre2_compile_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_character_tables(pcre2_compile_context *, const uint8_t *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_compile_extra_options(pcre2_compile_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_max_pattern_length(pcre2_compile_context *, PCRE2_SIZE); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_newline(pcre2_compile_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_parens_nest_limit(pcre2_compile_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_compile_recursion_guard(pcre2_compile_context *, \
    int (*)(uint32_t, void *), void *);
#define PCRE2_MATCH_CONTEXT_FUNCTIONS \
PCRE2_EXP_DECL pcre2_match_context PCRE2_CALL_CONVENTION \
  *pcre2_match_context_copy(pcre2_match_context *); \
PCRE2_EXP_DECL pcre2_match_context PCRE2_CALL_CONVENTION \
  *pcre2_match_context_create(pcre2_general_context *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_match_context_free(pcre2_match_context *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_callout(pcre2_match_context *, \
    int (*)(pcre2_callout_block *, void *), void *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_substitute_callout(pcre2_match_context *, \
    int (*)(pcre2_substitute_callout_block *, void *), void *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_depth_limit(pcre2_match_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_heap_limit(pcre2_match_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_match_limit(pcre2_match_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_offset_limit(pcre2_match_context *, PCRE2_SIZE); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_recursion_limit(pcre2_match_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_recursion_memory_management(pcre2_match_context *, \
    void *(*)(PCRE2_SIZE, void *), void (*)(void *, void *), void *);
#define PCRE2_CONVERT_CONTEXT_FUNCTIONS \
PCRE2_EXP_DECL pcre2_convert_context PCRE2_CALL_CONVENTION \
  *pcre2_convert_context_copy(pcre2_convert_context *); \
PCRE2_EXP_DECL pcre2_convert_context PCRE2_CALL_CONVENTION \
  *pcre2_convert_context_create(pcre2_general_context *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_convert_context_free(pcre2_convert_context *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_glob_escape(pcre2_convert_context *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_set_glob_separator(pcre2_convert_context *, uint32_t);
/* Functions concerned with compiling a pattern to PCRE internal code. */
#define PCRE2_COMPILE_FUNCTIONS \
PCRE2_EXP_DECL pcre2_code PCRE2_CALL_CONVENTION \
  *pcre2_compile(PCRE2_SPTR, PCRE2_SIZE, uint32_t, int *, PCRE2_SIZE *, \
    pcre2_compile_context *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_code_free(pcre2_code *); \
PCRE2_EXP_DECL pcre2_code PCRE2_CALL_CONVENTION \
  *pcre2_code_copy(const pcre2_code *); \
PCRE2_EXP_DECL pcre2_code PCRE2_CALL_CONVENTION \
  *pcre2_code_copy_with_tables(const pcre2_code *);
/* Functions that give information about a compiled pattern. */
#define PCRE2_PATTERN_INFO_FUNCTIONS \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_pattern_info(const pcre2_code *, uint32_t, void *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_callout_enumerate(const pcre2_code *, \
    int (*)(pcre2_callout_enumerate_block *, void *), void *);
/* Functions for running a match and inspecting the result. */
#define PCRE2_MATCH_FUNCTIONS \
PCRE2_EXP_DECL pcre2_match_data PCRE2_CALL_CONVENTION \
  *pcre2_match_data_create(uint32_t, pcre2_general_context *); \
PCRE2_EXP_DECL pcre2_match_data PCRE2_CALL_CONVENTION \
  *pcre2_match_data_create_from_pattern(const pcre2_code *, \
    pcre2_general_context *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_dfa_match(const pcre2_code *, PCRE2_SPTR, PCRE2_SIZE, PCRE2_SIZE, \
    uint32_t, pcre2_match_data *, pcre2_match_context *, int *, PCRE2_SIZE); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_match(const pcre2_code *, PCRE2_SPTR, PCRE2_SIZE, PCRE2_SIZE, \
    uint32_t, pcre2_match_data *, pcre2_match_context *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_match_data_free(pcre2_match_data *); \
PCRE2_EXP_DECL PCRE2_SPTR PCRE2_CALL_CONVENTION \
  pcre2_get_mark(pcre2_match_data *); \
PCRE2_EXP_DECL PCRE2_SIZE PCRE2_CALL_CONVENTION \
  pcre2_get_match_data_size(pcre2_match_data *); \
PCRE2_EXP_DECL uint32_t PCRE2_CALL_CONVENTION \
  pcre2_get_ovector_count(pcre2_match_data *); \
PCRE2_EXP_DECL PCRE2_SIZE PCRE2_CALL_CONVENTION \
  *pcre2_get_ovector_pointer(pcre2_match_data *); \
PCRE2_EXP_DECL PCRE2_SIZE PCRE2_CALL_CONVENTION \
  pcre2_get_startchar(pcre2_match_data *);
/* Convenience functions for handling matched substrings. */
#define PCRE2_SUBSTRING_FUNCTIONS \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_copy_byname(pcre2_match_data *, PCRE2_SPTR, PCRE2_UCHAR *, \
    PCRE2_SIZE *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_copy_bynumber(pcre2_match_data *, uint32_t, PCRE2_UCHAR *, \
    PCRE2_SIZE *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_substring_free(PCRE2_UCHAR *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_get_byname(pcre2_match_data *, PCRE2_SPTR, PCRE2_UCHAR **, \
    PCRE2_SIZE *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_get_bynumber(pcre2_match_data *, uint32_t, PCRE2_UCHAR **, \
    PCRE2_SIZE *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_length_byname(pcre2_match_data *, PCRE2_SPTR, PCRE2_SIZE *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_length_bynumber(pcre2_match_data *, uint32_t, PCRE2_SIZE *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_nametable_scan(const pcre2_code *, PCRE2_SPTR, PCRE2_SPTR *, \
    PCRE2_SPTR *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_number_from_name(const pcre2_code *, PCRE2_SPTR); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_substring_list_free(PCRE2_SPTR *); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substring_list_get(pcre2_match_data *, PCRE2_UCHAR ***, PCRE2_SIZE **);
/* Functions for serializing / deserializing compiled patterns. */
#define PCRE2_SERIALIZE_FUNCTIONS \
PCRE2_EXP_DECL int32_t PCRE2_CALL_CONVENTION \
  pcre2_serialize_encode(const pcre2_code **, int32_t, uint8_t **, \
    PCRE2_SIZE *, pcre2_general_context *); \
PCRE2_EXP_DECL int32_t PCRE2_CALL_CONVENTION \
  pcre2_serialize_decode(pcre2_code **, int32_t, const uint8_t *, \
    pcre2_general_context *); \
PCRE2_EXP_DECL int32_t PCRE2_CALL_CONVENTION \
  pcre2_serialize_get_number_of_codes(const uint8_t *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_serialize_free(uint8_t *);
/* Convenience function for match + substitute. */
#define PCRE2_SUBSTITUTE_FUNCTION \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_substitute(const pcre2_code *, PCRE2_SPTR, PCRE2_SIZE, PCRE2_SIZE, \
    uint32_t, pcre2_match_data *, pcre2_match_context *, PCRE2_SPTR, \
    PCRE2_SIZE, PCRE2_UCHAR *, PCRE2_SIZE *);
/* Functions for converting pattern source strings. */
#define PCRE2_CONVERT_FUNCTIONS \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_pattern_convert(PCRE2_SPTR, PCRE2_SIZE, uint32_t, PCRE2_UCHAR **, \
    PCRE2_SIZE *, pcre2_convert_context *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_converted_pattern_free(PCRE2_UCHAR *);
/* Functions for JIT processing */
#define PCRE2_JIT_FUNCTIONS \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_jit_compile(pcre2_code *, uint32_t); \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_jit_match(const pcre2_code *, PCRE2_SPTR, PCRE2_SIZE, PCRE2_SIZE, \
    uint32_t, pcre2_match_data *, pcre2_match_context *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_jit_free_unused_memory(pcre2_general_context *); \
PCRE2_EXP_DECL pcre2_jit_stack PCRE2_CALL_CONVENTION \
  *pcre2_jit_stack_create(PCRE2_SIZE, PCRE2_SIZE, pcre2_general_context *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_jit_stack_assign(pcre2_match_context *, pcre2_jit_callback, void *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_jit_stack_free(pcre2_jit_stack *);
/* Other miscellaneous functions. */
#define PCRE2_OTHER_FUNCTIONS \
PCRE2_EXP_DECL int PCRE2_CALL_CONVENTION \
  pcre2_get_error_message(int, PCRE2_UCHAR *, PCRE2_SIZE); \
PCRE2_EXP_DECL const uint8_t PCRE2_CALL_CONVENTION \
  *pcre2_maketables(pcre2_general_context *); \
PCRE2_EXP_DECL void PCRE2_CALL_CONVENTION \
  pcre2_maketables_free(pcre2_general_context *, const uint8_t *);
/* Define macros that generate width-specific names from generic versions. The
three-level macro scheme is necessary to get the macros expanded when we want
them to be. First we get the width from PCRE2_LOCAL_WIDTH, which is used for
generating three versions of everything below. After that, PCRE2_SUFFIX will be
re-defined to use PCRE2_CODE_UNIT_WIDTH, for use when macros such as
pcre2_compile are called by application code. */
#define PCRE2_JOIN(a,b) a ## b
#define PCRE2_GLUE(a,b) PCRE2_JOIN(a,b)
#define PCRE2_SUFFIX(a) PCRE2_GLUE(a,PCRE2_LOCAL_WIDTH)
/* Data types */
#define PCRE2_UCHAR                 PCRE2_SUFFIX(PCRE2_UCHAR)
#define PCRE2_SPTR                  PCRE2_SUFFIX(PCRE2_SPTR)
#define pcre2_code                  PCRE2_SUFFIX(pcre2_code_)
#define pcre2_jit_callback          PCRE2_SUFFIX(pcre2_jit_callback_)
#define pcre2_jit_stack             PCRE2_SUFFIX(pcre2_jit_stack_)
#define pcre2_real_code             PCRE2_SUFFIX(pcre2_real_code_)
#define pcre2_real_general_context  PCRE2_SUFFIX(pcre2_real_general_context_)
#define pcre2_real_compile_context  PCRE2_SUFFIX(pcre2_real_compile_context_)
#define pcre2_real_convert_context  PCRE2_SUFFIX(pcre2_real_convert_context_)
#define pcre2_real_match_context    PCRE2_SUFFIX(pcre2_real_match_context_)
#define pcre2_real_jit_stack        PCRE2_SUFFIX(pcre2_real_jit_stack_)
#define pcre2_real_match_data       PCRE2_SUFFIX(pcre2_real_match_data_)
/* Data blocks */
#define pcre2_callout_block            PCRE2_SUFFIX(pcre2_callout_block_)
#define pcre2_callout_enumerate_block  PCRE2_SUFFIX(pcre2_callout_enumerate_block_)
#define pcre2_substitute_callout_block PCRE2_SUFFIX(pcre2_substitute_callout_block_)
#define pcre2_general_context          PCRE2_SUFFIX(pcre2_general_context_)
#define pcre2_compile_context          PCRE2_SUFFIX(pcre2_compile_context_)
#define pcre2_convert_context          PCRE2_SUFFIX(pcre2_convert_context_)
#define pcre2_match_context            PCRE2_SUFFIX(pcre2_match_context_)
#define pcre2_match_data               PCRE2_SUFFIX(pcre2_match_data_)
/* Functions: the complete list in alphabetical order */
#define pcre2_callout_enumerate               PCRE2_SUFFIX(pcre2_callout_enumerate_)
#define pcre2_code_copy                       PCRE2_SUFFIX(pcre2_code_copy_)
#define pcre2_code_copy_with_tables           PCRE2_SUFFIX(pcre2_code_copy_with_tables_)
#define pcre2_code_free                       PCRE2_SUFFIX(pcre2_code_free_)
#define pcre2_compile                         PCRE2_SUFFIX(pcre2_compile_)
#define pcre2_compile_context_copy            PCRE2_SUFFIX(pcre2_compile_context_copy_)
#define pcre2_compile_context_create          PCRE2_SUFFIX(pcre2_compile_context_create_)
#define pcre2_compile_context_free            PCRE2_SUFFIX(pcre2_compile_context_free_)
#define pcre2_config                          PCRE2_SUFFIX(pcre2_config_)
#define pcre2_convert_context_copy            PCRE2_SUFFIX(pcre2_convert_context_copy_)
#define pcre2_convert_context_create          PCRE2_SUFFIX(pcre2_convert_context_create_)
#define pcre2_convert_context_free            PCRE2_SUFFIX(pcre2_convert_context_free_)
#define pcre2_converted_pattern_free          PCRE2_SUFFIX(pcre2_converted_pattern_free_)
#define pcre2_dfa_match                       PCRE2_SUFFIX(pcre2_dfa_match_)
#define pcre2_general_context_copy            PCRE2_SUFFIX(pcre2_general_context_copy_)
#define pcre2_general_context_create          PCRE2_SUFFIX(pcre2_general_context_create_)
#define pcre2_general_context_free            PCRE2_SUFFIX(pcre2_general_context_free_)
#define pcre2_get_error_message               PCRE2_SUFFIX(pcre2_get_error_message_)
#define pcre2_get_mark                        PCRE2_SUFFIX(pcre2_get_mark_)
#define pcre2_get_match_data_size             PCRE2_SUFFIX(pcre2_get_match_data_size_)
#define pcre2_get_ovector_pointer             PCRE2_SUFFIX(pcre2_get_ovector_pointer_)
#define pcre2_get_ovector_count               PCRE2_SUFFIX(pcre2_get_ovector_count_)
#define pcre2_get_startchar                   PCRE2_SUFFIX(pcre2_get_startchar_)
#define pcre2_jit_compile                     PCRE2_SUFFIX(pcre2_jit_compile_)
#define pcre2_jit_match                       PCRE2_SUFFIX(pcre2_jit_match_)
#define pcre2_jit_free_unused_memory          PCRE2_SUFFIX(pcre2_jit_free_unused_memory_)
#define pcre2_jit_stack_assign                PCRE2_SUFFIX(pcre2_jit_stack_assign_)
#define pcre2_jit_stack_create                PCRE2_SUFFIX(pcre2_jit_stack_create_)
#define pcre2_jit_stack_free                  PCRE2_SUFFIX(pcre2_jit_stack_free_)
#define pcre2_maketables                      PCRE2_SUFFIX(pcre2_maketables_)
#define pcre2_maketables_free                 PCRE2_SUFFIX(pcre2_maketables_free_)
#define pcre2_match                           PCRE2_SUFFIX(pcre2_match_)
#define pcre2_match_context_copy              PCRE2_SUFFIX(pcre2_match_context_copy_)
#define pcre2_match_context_create            PCRE2_SUFFIX(pcre2_match_context_create_)
#define pcre2_match_context_free              PCRE2_SUFFIX(pcre2_match_context_free_)
#define pcre2_match_data_create               PCRE2_SUFFIX(pcre2_match_data_create_)
#define pcre2_match_data_create_from_pattern  PCRE2_SUFFIX(pcre2_match_data_create_from_pattern_)
#define pcre2_match_data_free                 PCRE2_SUFFIX(pcre2_match_data_free_)
#define pcre2_pattern_convert                 PCRE2_SUFFIX(pcre2_pattern_convert_)
#define pcre2_pattern_info                    PCRE2_SUFFIX(pcre2_pattern_info_)
#define pcre2_serialize_decode                PCRE2_SUFFIX(pcre2_serialize_decode_)
#define pcre2_serialize_encode                PCRE2_SUFFIX(pcre2_serialize_encode_)
#define pcre2_serialize_free                  PCRE2_SUFFIX(pcre2_serialize_free_)
#define pcre2_serialize_get_number_of_codes   PCRE2_SUFFIX(pcre2_serialize_get_number_of_codes_)
#define pcre2_set_bsr                         PCRE2_SUFFIX(pcre2_set_bsr_)
#define pcre2_set_callout                     PCRE2_SUFFIX(pcre2_set_callout_)
#define pcre2_set_character_tables            PCRE2_SUFFIX(pcre2_set_character_tables_)
#define pcre2_set_compile_extra_options       PCRE2_SUFFIX(pcre2_set_compile_extra_options_)
#define pcre2_set_compile_recursion_guard     PCRE2_SUFFIX(pcre2_set_compile_recursion_guard_)
#define pcre2_set_depth_limit                 PCRE2_SUFFIX(pcre2_set_depth_limit_)
#define pcre2_set_glob_escape                 PCRE2_SUFFIX(pcre2_set_glob_escape_)
#define pcre2_set_glob_separator              PCRE2_SUFFIX(pcre2_set_glob_separator_)
#define pcre2_set_heap_limit                  PCRE2_SUFFIX(pcre2_set_heap_limit_)
#define pcre2_set_match_limit                 PCRE2_SUFFIX(pcre2_set_match_limit_)
#define pcre2_set_max_pattern_length          PCRE2_SUFFIX(pcre2_set_max_pattern_length_)
#define pcre2_set_newline                     PCRE2_SUFFIX(pcre2_set_newline_)
#define pcre2_set_parens_nest_limit           PCRE2_SUFFIX(pcre2_set_parens_nest_limit_)
#define pcre2_set_offset_limit                PCRE2_SUFFIX(pcre2_set_offset_limit_)
#define pcre2_set_substitute_callout          PCRE2_SUFFIX(pcre2_set_substitute_callout_)
#define pcre2_substitute                      PCRE2_SUFFIX(pcre2_substitute_)
#define pcre2_substring_copy_byname           PCRE2_SUFFIX(pcre2_substring_copy_byname_)
#define pcre2_substring_copy_bynumber         PCRE2_SUFFIX(pcre2_substring_copy_bynumber_)
#define pcre2_substring_free                  PCRE2_SUFFIX(pcre2_substring_free_)
#define pcre2_substring_get_byname            PCRE2_SUFFIX(pcre2_substring_get_byname_)
#define pcre2_substring_get_bynumber          PCRE2_SUFFIX(pcre2_substring_get_bynumber_)
#define pcre2_substring_length_byname         PCRE2_SUFFIX(pcre2_substring_length_byname_)
#define pcre2_substring_length_bynumber       PCRE2_SUFFIX(pcre2_substring_length_bynumber_)
#define pcre2_substring_list_get              PCRE2_SUFFIX(pcre2_substring_list_get_)
#define pcre2_substring_list_free             PCRE2_SUFFIX(pcre2_substring_list_free_)
#define pcre2_substring_nametable_scan        PCRE2_SUFFIX(pcre2_substring_nametable_scan_)
#define pcre2_substring_number_from_name      PCRE2_SUFFIX(pcre2_substring_number_from_name_)
/* Keep this old function name for backwards compatibility */
#define pcre2_set_recursion_limit PCRE2_SUFFIX(pcre2_set_recursion_limit_)
/* Keep this obsolete function for backwards compatibility: it is now a noop. */
#define pcre2_set_recursion_memory_management PCRE2_SUFFIX(pcre2_set_recursion_memory_management_)
/* Now generate all three sets of width-specific structures and function
prototypes. */
#define PCRE2_TYPES_STRUCTURES_AND_FUNCTIONS \
PCRE2_TYPES_LIST \
PCRE2_STRUCTURE_LIST \
PCRE2_GENERAL_INFO_FUNCTIONS \
PCRE2_GENERAL_CONTEXT_FUNCTIONS \
PCRE2_COMPILE_CONTEXT_FUNCTIONS \
PCRE2_CONVERT_CONTEXT_FUNCTIONS \
PCRE2_CONVERT_FUNCTIONS \
PCRE2_MATCH_CONTEXT_FUNCTIONS \
PCRE2_COMPILE_FUNCTIONS \
PCRE2_PATTERN_INFO_FUNCTIONS \
PCRE2_MATCH_FUNCTIONS \
PCRE2_SUBSTRING_FUNCTIONS \
PCRE2_SERIALIZE_FUNCTIONS \
PCRE2_SUBSTITUTE_FUNCTION \
PCRE2_JIT_FUNCTIONS \
PCRE2_OTHER_FUNCTIONS
#define PCRE2_LOCAL_WIDTH 8
PCRE2_TYPES_STRUCTURES_AND_FUNCTIONS
#undef PCRE2_LOCAL_WIDTH
#define PCRE2_LOCAL_WIDTH 16
PCRE2_TYPES_STRUCTURES_AND_FUNCTIONS
#undef PCRE2_LOCAL_WIDTH
#define PCRE2_LOCAL_WIDTH 32
PCRE2_TYPES_STRUCTURES_AND_FUNCTIONS
#undef PCRE2_LOCAL_WIDTH
/* Undefine the list macros; they are no longer needed. */
#undef PCRE2_TYPES_LIST
#undef PCRE2_STRUCTURE_LIST
#undef PCRE2_GENERAL_INFO_FUNCTIONS
#undef PCRE2_GENERAL_CONTEXT_FUNCTIONS
#undef PCRE2_COMPILE_CONTEXT_FUNCTIONS
#undef PCRE2_CONVERT_CONTEXT_FUNCTIONS
#undef PCRE2_MATCH_CONTEXT_FUNCTIONS
#undef PCRE2_COMPILE_FUNCTIONS
#undef PCRE2_PATTERN_INFO_FUNCTIONS
#undef PCRE2_MATCH_FUNCTIONS
#undef PCRE2_SUBSTRING_FUNCTIONS
#undef PCRE2_SERIALIZE_FUNCTIONS
#undef PCRE2_SUBSTITUTE_FUNCTION
#undef PCRE2_JIT_FUNCTIONS
#undef PCRE2_OTHER_FUNCTIONS
#undef PCRE2_TYPES_STRUCTURES_AND_FUNCTIONS
/* PCRE2_CODE_UNIT_WIDTH must be defined. If it is 8, 16, or 32, redefine
PCRE2_SUFFIX to use it. If it is 0, undefine the other macros and make
PCRE2_SUFFIX a no-op. Otherwise, generate an error. */
#undef PCRE2_SUFFIX
#ifndef PCRE2_CODE_UNIT_WIDTH
#error PCRE2_CODE_UNIT_WIDTH must be defined before including pcre2.h.
#error Use 8, 16, or 32; or 0 for a multi-width application.
#else  /* PCRE2_CODE_UNIT_WIDTH is defined */
#if PCRE2_CODE_UNIT_WIDTH == 8 || \
    PCRE2_CODE_UNIT_WIDTH == 16 || \
    PCRE2_CODE_UNIT_WIDTH == 32
#define PCRE2_SUFFIX(a) PCRE2_GLUE(a, PCRE2_CODE_UNIT_WIDTH)
#elif PCRE2_CODE_UNIT_WIDTH == 0
#undef PCRE2_JOIN
#undef PCRE2_GLUE
#define PCRE2_SUFFIX(a) a
#else
#error PCRE2_CODE_UNIT_WIDTH must be 0, 8, 16, or 32.
#endif
#endif  /* PCRE2_CODE_UNIT_WIDTH is defined */
#ifdef __cplusplus
}  /* extern "C" */
#endif
#endif  /* PCRE2_H_IDEMPOTENT_GUARD */
/* End of pcre2.h */
namespace pcre2
{
using regex_t = std::shared_ptr<const pcre2_code_8>;
using match_data_dtor_t = std::integral_constant<
    decltype(&pcre2_match_data_free_8), &pcre2_match_data_free_8>;
using match_data_t = std::unique_ptr<
    pcre2_match_data_8, match_data_dtor_t>;
using match_context_dtor_t = std::integral_constant<
    decltype(&pcre2_match_context_free_8), &pcre2_match_context_free_8>;
using match_context_t = std::unique_ptr<
    pcre2_match_context_8, match_context_dtor_t>;
template<typename String>
regex_t new_regex(const String& pattern)
{
    static_assert(wl::is_string_view_v<String>, "");
    int error = 0;
    PCRE2_SIZE pos = 0;
    pcre2_code_8* regex_ptr = pcre2_compile_8((PCRE2_SPTR8)pattern.c_str(),
        pattern.byte_size(), 0, &error, &pos, nullptr);
    if (!regex_ptr)
    {
        constexpr PCRE2_SIZE buffer_size = 256;
        PCRE2_UCHAR buffer[buffer_size];
        pcre2_get_error_message_8(error, buffer, buffer_size);
        throw std::logic_error((const char*)buffer);
    }
    return {regex_ptr, &pcre2_code_free_8};
}
template<typename C, typename P>
struct _regex_search_state
{
    const wl::strexp::compiled_pattern<C, P>& pattern_;
    uint32_t capture_count_ = 0;
    const PCRE2_SPTR8 text_data_;
    const size_t text_size_;
    size_t start_pos_ = 0;
    match_data_t match_{};
    PCRE2_SIZE* match_ptr_ = nullptr;
    _regex_search_state(const wl::strexp::compiled_pattern<C, P>& pattern,
        PCRE2_SPTR8 text_data, size_t text_size) :
        pattern_{pattern}, text_data_{text_data}, text_size_{text_size}
    {
        *pattern_.conditions_ptr = &pattern_.conditions;
        pcre2_pattern_info_8(pattern_.regex_ptr.get(),
            PCRE2_INFO_CAPTURECOUNT, &capture_count_);
        match_.reset(pcre2_match_data_create_8(capture_count_ + 1u, nullptr));
        match_ptr_ = pcre2_get_ovector_pointer_8(match_.get());
        match_ptr_[0] = PCRE2_SIZE(-1);
        match_ptr_[1] = PCRE2_SIZE(0);
    }
    wl::string_view prefix() const
    {
        return {text_data_ + start_pos_, text_data_ + match_ptr_[0u]};
    }
    auto capture_count() const
    {
        return size_t(capture_count_);
    }
    size_t match_begin_idx() const
    {
        return size_t(match_ptr_[0u]);
    }
    size_t match_end_idx() const
    {
        return size_t(match_ptr_[1u]);
    }
    wl::string_view match() const
    {
        return {text_data_ + match_ptr_[0u], text_data_ + match_ptr_[1u]};
    }
    wl::string_view match(size_t i) const
    {
        assert(i <= capture_count_);
        return {text_data_ + match_ptr_[2u * i],
            text_data_ + match_ptr_[2u * i + 1u]};
    }
    bool find_next(bool overlap = false, uint32_t options = 0u)
    {
        WL_THROW_IF_ABORT()
        start_pos_ = overlap ? match_ptr_[0] + PCRE2_SIZE(1) : match_ptr_[1];
        auto result = pcre2_match_8(pattern_.regex_ptr.get(),
            text_data_, text_size_, start_pos_, options, match_.get(),
            pattern_.match_context_ptr.get());
        return result >= 0;
    }
};
template<typename C, typename P, typename CharT>
auto regex_search(const wl::strexp::compiled_pattern<C, P>& pattern,
    const CharT* text_data, size_t text_size)
{
    return _regex_search_state<C, P>(pattern, (PCRE2_SPTR8)text_data,
        text_size);
}
template<typename PatternIdList>
struct callout_matches_t
{
    PCRE2_SPTR text_data;
    const PCRE2_SIZE* offset_vector;
    const size_t capture_top;
    template<int64_t I>
    wl::string_view operator[](wl::const_int<I> id) const
    {
        if constexpr (I > 0)
        { // Condition
            constexpr auto group_idx = PatternIdList::find(id);
            assert(group_idx < capture_top);
            return {text_data + offset_vector[2u * group_idx],
                text_data + offset_vector[2u * group_idx + 1u]};
        }
        else
        { // PatternTest
            return {text_data + offset_vector[2u * capture_top - 2u],
                text_data + offset_vector[2u * capture_top - 1u]};
        }
    }
};
template<typename PL, typename CL, size_t... Is>
auto callout_evaluate(size_t number, const callout_matches_t<PL>& matches,
    const CL& conditions, std::index_sequence<Is...>)
{
    assert(number < CL::size);
    return ((number != Is || conditions.template get<Is>()(matches)) && ...);
}
template<typename PatternIdList, typename ConditionList>
int callout_function(pcre2_callout_block_8* block_ptr,
    void* condition_list_ptr)
{
    WL_THROW_IF_ABORT()
    auto matches = callout_matches_t<PatternIdList>{block_ptr->subject,
        block_ptr->offset_vector, block_ptr->capture_top};
    const ConditionList& condition_list =
        **(const ConditionList**)condition_list_ptr;
    bool pass = callout_evaluate(
        block_ptr->callout_number, matches, condition_list,
        std::make_index_sequence<ConditionList::size>{});
    return pass ? 0 : 1;
}
}
namespace wl
{
template<typename X>
auto string_length(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(is_string_type_v<X>, WL_ERROR_STRING_TYPE_ONLY);
    auto pure = [](const auto& str)
    {
        return int64_t(str.size());
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Any>
auto _integer_to_string(const Any& any, const size_t min_length = 0)
{
    if constexpr (array_rank_v<Any> == 0u)
    {
        if constexpr (is_integral_v<Any>)
        {
            if (any == Any(0))
                return string("0");
            constexpr size_t buffer_size = 24;
            utf8::char_t buffer[buffer_size];
            auto ptr = buffer + buffer_size;
            size_t length = 0;
            auto value = any;
            while (value || length < min_length)
            {
                auto rem = value % Any(10);
                value = value / Any(10);
                if constexpr (std::is_unsigned_v<Any>)
                    *--ptr = utf8::char_t('0' + rem);
                else
                    *--ptr = utf8::char_t('0' + (rem < 0 ? -rem : rem));
                ++length;
            }
            if (any < Any(0))
                *--ptr = '-';
            return string(ptr, buffer_size - size_t(ptr - buffer));
        }
        else
        {
            assert(false);
            return string();
        }
    }
    else
    {
        assert(false);
        return string();
    }
}
template<typename Arg>
auto _string_join_attributes_by_args_impl(
    size_t& byte_size, bool& ascii_only, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        for (size_t i = 0; i < arg.size(); ++i)
        {
            byte_size += arg.get(i).byte_size();
            if (ascii_only)
                ascii_only = arg.get(i).ascii_only();
        }
        return byte_size;
    }
    else if constexpr (array_rank_v<Arg> >= 1u)
    {
        static_assert(is_string_v<value_type_t<Arg>>, WL_ERROR_STRING_ONLY);
        arg.for_each([&](const auto& x)
            {
                byte_size += x.byte_size();
                if (ascii_only)
                    ascii_only = x.ascii_only();
            });
        return byte_size;
    }
    else
    {
        static_assert(is_string_view_v<Arg>, WL_ERROR_STRING_ONLY);
        byte_size += arg.byte_size();
        if (ascii_only)
            ascii_only = arg.ascii_only();
    }
}
template<typename... Args>
auto _string_join_attributes_by_args(const Args&... args)
{
    size_t total_size = 0u;
    bool ascii_only = true;
    [[maybe_unused]] const auto& _1 = (
        _string_join_attributes_by_args_impl(
            total_size, ascii_only, args),
        ..., 0);
    return std::pair(total_size, ascii_only);
}
template<typename Char, typename Arg>
void _string_join_copy_by_args_impl(Char*& str, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        for (size_t i = 0; i < arg.size(); ++i)
            _string_join_copy_by_args_impl(str, arg.get(i));
    }
    else if constexpr (array_rank_v<Arg> >= 1u)
    {
        arg.for_each([&](const auto& x)
            { _string_join_copy_by_args_impl(str, x); });
    }
    else
    {
        const size_t byte_size = arg.byte_size();
        utils::restrict_copy_n(arg.byte_data(), byte_size, str);
        str += byte_size;
    }
}
template<typename Char, typename... Args>
auto _string_join_copy_by_args(Char*& str, const Args&... args)
{
    [[maybe_unused]] const auto& _1 = (
        _string_join_copy_by_args_impl(str, args), ..., 0);
}
template<typename... Args>
auto string_join(const Args&... args)
{
    WL_TRY_BEGIN()
    auto [total_size, ascii_only] = _string_join_attributes_by_args(args...);
    auto ret = string(total_size, ascii_only);
    auto ret_data = ret.byte_data();
    WL_THROW_IF_ABORT()
    _string_join_copy_by_args(ret_data, args...);
    assert(ret.check_validity());
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Spec>
auto _string_take_impl_ascii_only()
{
    return string();
}
template<typename Iter, typename Offset>
Iter _string_take_find_offset(const Iter& begin, const Iter& end,
    const Offset offset, const bool adjust_end)
{
    if (std::is_unsigned_v<Offset> || offset > 0)
    {
        auto ret = begin;
        ret.apply_offset(offset - ptrdiff_t(!adjust_end), end);
        if (ret.get_pointer() > end.get_pointer())
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        return ret;
    }
    else if (offset < 0)
    {
        auto ret = end;
        ret.apply_offset(offset + ptrdiff_t(adjust_end), begin);
        if (ret.get_pointer() < begin.get_pointer())
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        return ret;
    }
    else
    {
        throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        return begin;
    }
}
template<typename Iter>
auto _string_take_impl_unicode_1arg(const Iter& begin, const Iter& end,
    ptrdiff_t offset)
{
    if (offset > 0)
    {
        const auto mid = _string_take_find_offset(begin, end, offset, true);
        return string_view(begin, mid, size_t(offset));
    }
    else
    {
        const auto mid = _string_take_find_offset(begin, end, offset, false);
        return string_view(mid, end, size_t(-offset));
    }
}
template<typename Iter>
auto _string_take_impl_unicode_2args(const Iter& begin, const Iter& end,
    ptrdiff_t offset, ptrdiff_t string_size)
{
    if (string_size <= 0)
        return string_view(begin, begin, size_t(0));
    const auto mid1 = _string_take_find_offset(
        begin, end, offset, false);
    const auto mid2 = _string_take_find_offset(
        mid1, end, size_t(string_size), true);
    const auto byte_size = size_t(mid2.byte_difference(mid1));
    return string_view(mid1, mid2, size_t(string_size));
}
template<typename String, typename Spec>
auto _string_take_impl_unicode(const String& str, const Spec& spec,
    ptrdiff_t total_size = -1)
{
    const auto begin = str.begin();
    const auto end = str.end();
    if constexpr (is_integral_v<Spec>)
    {
        if (spec == Spec(0))
            return string_view(begin, begin, size_t(0));
        else
            return _string_take_impl_unicode_1arg(
                begin, end, ptrdiff_t(spec));
    }
    else if constexpr (array_rank_v<Spec> == 1u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto spec_size = spec.size();
        std::array<ptrdiff_t, 2u> offsets{};
        spec.copy_to(offsets.data());
        if (spec_size == 1u)
        {
            if (offsets[0] == 0)
                throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
            return _string_take_impl_unicode_2args(
                begin, end, offsets[0], 1u);
        }
        else if (spec_size == 2u)
        {
            if (offsets[0] == 0 || offsets[1] == 0)
                throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
            if (offsets[0] * offsets[1] < 0)
            {
                if (total_size < 0)
                    total_size = str.size();
                if (offsets[0] < 0)
                    offsets[0] += ptrdiff_t(total_size + 1u);
                else
                    offsets[1] += ptrdiff_t(total_size + 1u);
            }
            return _string_take_impl_unicode_2args(
                begin, end, offsets[0], offsets[1] - offsets[0] + 1u);
        }
        else
        {
            throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
        }
    }
    else if constexpr (array_rank_v<Spec> == 2u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto& valspec = allows<view_category::Simple>(spec);
        const auto ret_size = valspec.dims()[0];
        const auto spec_size = valspec.dims()[1];
        const auto spec_data = valspec.data();
        ndarray<string, 1u> ret(std::array<size_t, 1u>{ret_size});
        auto ret_data = ret.data();
        if (spec_size == 1u)
        {
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(_string_take_impl_unicode(str,
                    wl::list(spec_data[i])));
        }
        else if (spec_size == 2u)
        {
            if constexpr (std::is_signed_v<value_type_t<Spec>>)
            {
                if ((total_size < 0) && std::any_of(
                    spec_data, spec_data + 2u * ret_size,
                    [](auto i) { return i < 0; }))
                {
                    total_size = str.size();
                }
            }
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(_string_take_impl_unicode(str,
                    wl::list(spec_data[2u * i], spec_data[2u * i + 1u]),
                    total_size));
        }
        else
        {
            throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
        }
        return ret;
    }
    else
    {
        static_assert(always_false_v<Spec>, WL_ERROR_TAKE_SPEC_TYPE);
    }
}
template<typename String, typename Spec>
auto _string_take_impl_ascii(const String& str, const Spec& spec)
{
    const auto begin = str.byte_begin();
    const auto end = str.byte_end();
    const auto total_size = str.byte_size();
    if constexpr (is_integral_v<Spec>)
    {
        if (spec == Spec(0))
            return string_view(begin, begin, size_t(0));
        else if (size_t(wl::abs(spec)) > total_size)
            throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
        else if (spec > Spec(0))
            return string_view(begin, begin + ptrdiff_t(spec), size_t(spec));
        else
            return string_view(end + ptrdiff_t(spec), end,
                size_t(-ptrdiff_t(spec)));
    }
    else if constexpr (array_rank_v<Spec> == 1u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto spec_size = spec.size();
        std::array<ptrdiff_t, 2u> offsets{};
        spec.copy_to(offsets.data());
        if (spec_size == 1u)
        {
            if (offsets[0] == 0 || size_t(wl::abs(offsets[0])) > total_size)
                throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
            auto substr_begin = (offsets[0] > 0) ?
                (begin + offsets[0] - 1) : (end + offsets[0]);
            return string_view(substr_begin, substr_begin + 1, size_t(1));
        }
        else if (spec_size == 2u)
        {
            if (offsets[0] == 0 || offsets[1] == 0)
                throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
            if (offsets[0] < 0)
                offsets[0] += ptrdiff_t(total_size + 1u);
            if (offsets[1] < 0)
                offsets[1] += ptrdiff_t(total_size + 1u);
            if (offsets[0] > offsets[1])
                return string_view(begin, begin, size_t(0));
            if (size_t(offsets[0]) > total_size ||
                size_t(offsets[1]) > total_size)
                throw std::logic_error(WL_ERROR_OUT_OF_RANGE);
            return string_view(begin + offsets[0] - 1,
                begin + offsets[1], size_t(offsets[1] - offsets[0] + 1));
        }
        else
        {
            throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
        }
    }
    else if constexpr (array_rank_v<Spec> == 2u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto& valspec = allows<view_category::Simple>(spec);
        const auto ret_size = valspec.dims()[0];
        const auto spec_size = valspec.dims()[1];
        const auto spec_data = valspec.data();
        ndarray<string, 1u> ret(std::array<size_t, 1u>{ret_size});
        auto ret_data = ret.data();
        if (spec_size == 1u)
        {
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(_string_take_impl_ascii(str,
                    wl::list(spec_data[i])));
        }
        else if (spec_size == 2u)
        {
            for (size_t i = 0; i < ret_size; ++i)
                ret_data[i] = string(_string_take_impl_ascii(str,
                    wl::list(spec_data[2u * i], spec_data[2u * i + 1u])));
        }
        else
        {
            throw std::logic_error(WL_ERROR_STRING_TAKE_SPEC_LIST_LENGTH);
        }
        return ret;
    }
    else
    {
        static_assert(always_false_v<Spec>, WL_ERROR_TAKE_SPEC_TYPE);
    }
}
template<typename String, typename Spec>
auto string_take(String&& str, const Spec& spec)
{
    WL_TRY_BEGIN()
    using StringT = remove_cvref_t<String>;
    static_assert(is_string_view_v<StringT>, WL_ERROR_STRING_ONLY);
    WL_THROW_IF_ABORT()
    if constexpr (array_rank_v<Spec> <= 1u)
    {
        auto view = u8string_view{};
        if (str.ascii_only())
            view = _string_take_impl_ascii(str, spec);
        else
            view = _string_take_impl_unicode(str, spec);
        if constexpr (is_string_v<StringT> && std::is_rvalue_reference_v<String&&>)
        { // must return a string
            return string(view);
        }
        else
        { // can return a string view
            return view;
        }
    }
    else
    {
        if (str.ascii_only())
            return _string_take_impl_ascii(str, spec);
        else
            return _string_take_impl_unicode(str, spec);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<int64_t Id, int64_t... List>
struct _pattern_id_list_find_impl;
template<int64_t Id, int64_t First, int64_t... Rest>
struct _pattern_id_list_find_impl<Id, First, Rest...>
{
    static constexpr auto next =
        _pattern_id_list_find_impl<Id, Rest...>::value;
    static constexpr auto value =
        (next == -1) ? int64_t(-1) : next + 1;
};
template<int64_t Id, int64_t... Rest>
struct _pattern_id_list_find_impl<Id, Id, Rest...>
{
    static constexpr auto value = int64_t(1);
};
template<int64_t Id>
struct _pattern_id_list_find_impl<Id>
{
    static constexpr auto value = int64_t(-1);
};
template<int64_t Start, int64_t... List>
struct _pattern_id_list_max;
template<int64_t Start, int64_t First, int64_t... Rest>
struct _pattern_id_list_max<Start, First, Rest...> :
    _pattern_id_list_max<std::max(Start, First), Rest...> {};
template<int64_t Final>
struct _pattern_id_list_max<Final>
{
    static constexpr auto value = Final;
};
template<int64_t... Ids>
struct _pattern_id_list
{
    static constexpr auto size = sizeof...(Ids);
    template<int64_t Id>
    static constexpr auto find(const_int<Id>)
    {
        return _pattern_id_list_find_impl<Id, Ids...>::value;
    }
    template<int64_t Id>
    static constexpr auto append(const_int<Id>)
    {
        return _pattern_id_list<Ids..., Id>{};
    }
};
template<typename... Conditions>
struct _pattern_condition_list
{
    static constexpr auto size = sizeof...(Conditions);
    std::tuple<Conditions...> conditions_;
    template<typename Condition, size_t... Is>
    auto append_impl(Condition&& condition, std::index_sequence<Is...>) &&
    {
        return _pattern_condition_list<Conditions..., Condition>{
            std::make_tuple(std::get<Is>(std::move(conditions_))...,
                std::move(condition))};
    }
    
    template<typename Condition>
    auto append(Condition condition) &&
    {
        return std::move(*this).append_impl(std::move(condition),
            std::make_index_sequence<size>{});
    }
    template<size_t I>
    auto get() const -> const auto&
    {
        return std::get<I>(conditions_);
    }
};
namespace strexp
{
template<typename ConditionList, typename PatternIdList_>
struct compilation_state
{
    using PatternIdList = PatternIdList_;
    ConditionList conditions;
    template<int64_t Id>
    auto append_id(const_int<Id>) &&
    {
        using NewList = decltype(PatternIdList::append(const_int<Id>{}));
        return compilation_state<ConditionList, NewList>{std::move(conditions)};
    }
    template<typename Condition>
    auto append_condition(Condition condition) &&
    {
        auto new_conditions =
            std::move(conditions).append(std::move(condition));
        return compilation_state<decltype(new_conditions), PatternIdList>{
            std::move(new_conditions)};
    }
};
template<typename ConditionList, typename PatternIdList_>
struct compiled_pattern
{
    pcre2::regex_t regex_ptr;
    pcre2::match_context_t match_context_ptr;
    const ConditionList conditions;
    std::unique_ptr<const ConditionList*> conditions_ptr;
    const string regex_text;
    using PatternIdList = PatternIdList_;
    compiled_pattern(pcre2::regex_t in_regex_ptr,
        ConditionList&& in_conditions, string&& in_regex_text) :
        regex_ptr{in_regex_ptr},
        match_context_ptr{pcre2_match_context_create_8(nullptr)},
        conditions{std::move(in_conditions)},
        conditions_ptr{new (const ConditionList*){nullptr}},
        regex_text{std::move(in_regex_text)}
    {
        set_callout();
    }
    compiled_pattern(pcre2::regex_t in_regex_ptr) :
        regex_ptr{in_regex_ptr},
        match_context_ptr{pcre2_match_context_create_8(nullptr)},
        conditions{},
        conditions_ptr{new (const ConditionList*){nullptr}},
        regex_text{""}
    {
        set_callout();
    }
    void set_callout()
    {
        pcre2_set_callout_8(match_context_ptr.get(),
            &pcre2::callout_function<PatternIdList, ConditionList>,
            conditions_ptr.get());
    }
};
template<typename FormatIdList_>
struct compiled_format
{
    using FormatIdList = FormatIdList_;
    const string text;
};
template<typename Pattern, typename Format>
struct compiled_rule
{
    const Pattern pattern;
    const Format format;
};
template<typename T>
struct _is_compiled_pattern : std::false_type {};
template<typename C, typename P>
struct _is_compiled_pattern<compiled_pattern<C, P>> : std::true_type {};
template<typename P, typename R>
struct _is_compiled_pattern<compiled_rule<P, R>> : std::true_type {};
template<typename T>
constexpr auto _is_compiled_pattern_v = _is_compiled_pattern<T>::value;
enum tag_t
{
    WithinGroup = 0,
    Alternation,
    Concatenation,
    MatchShortest,
    Repetition,
    Grouping,
    CharacterSet,
    Exception
};
template<int64_t Id, typename Pattern, typename State>
auto compile(_named_pattern<Id, Pattern> p, string& str, State s, tag_t tag)
{
    constexpr auto group_idx = State::PatternIdList::find(const_int<Id>{});
    if constexpr (group_idx < 0 || Id < 0)
    {
        // group does not exist; capture a new group
        // negative id means that the group is used for PatternTest
        str.join("(");
        auto s1 = std::move(s).append_id(const_int<Id>{});
        auto s2 = compile(std::move(p.pattern), str, std::move(s1),
            tag == MatchShortest ? MatchShortest : WithinGroup);
        str.join(")");
        return std::move(s2);
    }
    else
    { // group already exists; refer to that group
        str.join("\\g{").join(_integer_to_string(group_idx)).join("}");
        return std::move(s);
    }
}
template<typename... Head, typename State>
auto compile(_pattern_blank<Head...>, string& str, State s, tag_t)
{
    str.join(".");
    return std::move(s);
}
template<typename... Head, typename State>
auto compile(_pattern_blank_sequence<Head...>, string& str, State s, tag_t tag)
{
    if (tag == MatchShortest)
        str.join(".+?");
    else if (tag < Repetition)
        str.join(".+");
    else
        str.join("(?:.+)");
    return std::move(s);
}
template<typename... Head, typename State>
auto compile(_pattern_blank_null_sequence<Head...>, string& str, State s,
    tag_t tag)
{
    if (tag == MatchShortest)
        str.join(".*?");
    else if (tag < Repetition)
        str.join(".*");
    else
        str.join("(?:.*)");
    return std::move(s);
}
template<size_t I = 0u, typename... Patterns, typename State>
auto compile_alternatives(_pattern_alternatives<Patterns...> p, string& str,
    State s, tag_t tag)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto&& pattern = std::move(p).template get<I>();
        auto s1 = compile(std::move(pattern), str, std::move(s), tag);
        if ((I + 1u < sizeof...(Patterns)) && tag == Alternation)
            str.join("|");
        auto s2 = compile_alternatives<I + 1u>(std::move(p), str,
            std::move(s1), tag);
        return std::move(s2);
    }
    else
    {
        return std::move(s);
    }
}
template<typename String>
auto is_single_character(const String& s)
{
    return (s.byte_size() == 1u) || ((s.byte_size() <= 4u) && (s.size() == 1u));
}
template<size_t I = 0u, typename... Patterns>
auto compile_use_bracket_impl(
    const _pattern_alternatives<Patterns...>& patterns)
{
    if constexpr (I < sizeof...(Patterns))
    {
        const auto& p = patterns.template get<I>();
        using PT = remove_cvref_t<decltype(p)>;
        bool pass = false;
        if constexpr (is_string_view_v<remove_cvref_t<decltype(p)>>)
            pass = is_single_character(p);
        else
            pass = std::is_same_v<PT, _whitespace_character_type> ||
                std::is_same_v<PT, _word_character_type> ||
                std::is_same_v<PT, _letter_character_type> ||
                std::is_same_v<PT, _digit_character_type> ||
                std::is_same_v<PT, _hexadecimal_character_type> ||
                std::is_same_v<PT, _punctuation_character_type>;
        return pass && compile_use_bracket_impl<I + 1u>(patterns);
    }
    else
    {
        return true;
    }
}
template<typename... Patterns>
auto compile_use_bracket(const _pattern_alternatives<Patterns...>& p)
{
    return compile_use_bracket_impl<0u>(p);
}
template<typename Any>
auto compile_use_bracket(const Any& any)
{
    if constexpr (array_rank_v<Any> == 1u && is_string_type_v<Any>)
        return bool(all_true(any, [](const string& s)
            { return boolean(is_single_character(s)); }));
    else
        return false;
}
template<typename... Patterns, typename State>
auto compile(_pattern_alternatives<Patterns...> p, string& str, State s,
    tag_t tag)
{
    auto use_bracket = compile_use_bracket(p);
    if (tag == Exception && !use_bracket)
        throw std::logic_error(WL_ERROR_STRING_EXCEPT);
    if (tag == Exception)
        str.join("[^");
    else if (use_bracket)
        str.join("[");
    else if (tag >= Alternation)
        str.join("(?:");
    auto s1 = compile_alternatives(std::move(p), str, std::move(s),
        use_bracket ? CharacterSet : Alternation);
    if (use_bracket)
        str.join("]");
    else if (tag >= Alternation)
        str.join(")");
    return std::move(s1);
}
template<typename Pattern, typename State>
auto compile(_pattern_repeated<Pattern> p, string& str, State s, tag_t tag)
{
    if (p.min == 0u)
        str.join("(?:");
    auto s1 = compile(p.pattern, str, std::move(s),
        p.min == 0u ? WithinGroup : Concatenation);
    if (p.min == 0u)
    {
        str.join(")?");
        if (tag == MatchShortest)
            str.join("?");
    }
    auto s2 = compile(std::move(p).pattern, str, std::move(s1), Repetition);
    if (p.max_is_infinity())
        str.join("*");
    else
    {
        if (p.min >= 2u)
            str.join("{").join(_integer_to_string(p.min - 1u)).join(",");
        else
            str.join("{0,");
        str.join(_integer_to_string(p.max - 1u)).join("}");
    }
    if (tag == MatchShortest)
        str.join("?");
    return std::move(s2);
}
template<typename Pattern, typename State>
auto compile(_pattern_longest<Pattern> p, string& str, State s, tag_t tag)
{
    return compile(std::move(p.pattern), str, std::move(s), tag);
}
template<typename Pattern, typename State>
auto compile(_pattern_shortest<Pattern> p, string& str, State s, tag_t tag)
{
    return compile(std::move(p.pattern), str, std::move(s), MatchShortest);
}
template<typename Pattern, typename Condition, typename State>
auto compile(_condition<Pattern, Condition> p, string& str, State s, tag_t tag)
{
    auto s1 = std::move(s).append_condition(std::move(p.condition));
    if (tag > Concatenation)
        str.join("(?:");
    auto s2 = compile(std::move(p.pattern), str, std::move(s1), Concatenation);
    str.join("(?C");
    str.join(_integer_to_string(decltype(s.conditions)::size));
    str.join(")");
    if (tag > Concatenation)
        str.join(")");
    return std::move(s2);
}
template<size_t I, typename... Patterns, typename State>
auto compile_impl(_string_expression<Patterns...>&& p, string& str, State s)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto s2 = compile(std::move(p).template get<I>(), str, std::move(s),
            Concatenation);
        return compile_impl<I + 1u>(std::move(p), str, std::move(s2));
    }
    else
    {
        return std::move(s);
    }
}
template<typename... Patterns, typename State>
auto compile(_string_expression<Patterns...> p, string& str, State s,
    tag_t tag)
{
    if (tag > Concatenation)
        str.join("(?:");
    auto s1 = compile_impl<0u>(std::move(p), str, std::move(s));
    if (tag > Concatenation)
        str.join(")");
    return std::move(s1);
}
inline void compile_impl(const utf8::char_t* begin, const utf8::char_t* end,
    string& str, tag_t tag)
{
    static const bool esc_table[256] ={
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,1,0,0,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    if (begin == end)
    {
        if (tag > WithinGroup)
            str.join("(?:)");
    }
    else
    {
        if (tag > Concatenation && tag != CharacterSet)
            str.join("(?:");
        for (; begin < end; ++begin)
        {
            if (esc_table[*begin])
                str.append<false, false>('\\');
            str.append<false, false>(*begin);
        }
        str.place_null_character();
        if (tag > Concatenation && tag != CharacterSet)
            str.join(")");
    }
}
template<typename State>
auto compile(const ndarray<string, 1u>& patterns, string& str, State s,
    tag_t tag)
{
    auto use_bracket = compile_use_bracket(patterns);
    auto begin = patterns.begin();
    auto end = patterns.end();
    if (use_bracket)
        str.append('[');
    else if (tag >= Alternation)
        str.join("(?:");
    for (; begin != end; ++begin)
    {
        compile_impl(begin->byte_begin(), begin->byte_end(), str, Alternation);
        if (!use_bracket)
            str.append('|');
    }
    if (use_bracket)
        str.append(']');
    else if (tag >= Alternation)
        str.join(")");
    return std::move(s);
}
template<typename Any, typename State>
auto compile(_pattern_except<Any> p, string& str, State s, tag_t)
{
    if constexpr (is_string_view_v<Any>)
    {
        if (!is_single_character(p.pattern))
            throw std::logic_error(WL_ERROR_STRING_EXCEPT);
        str.join("[^");
        str.join(p.pattern);
        str.join("]");
    }
    else if constexpr (array_rank_v<Any> == 1u && is_string_type_v<Any>)
    {
        if (!compile_use_bracket(p.pattern))
            throw std::logic_error(WL_ERROR_STRING_EXCEPT);
        auto begin = p.pattern.begin();
        auto end = p.pattern.end();
        str.join("[^");
        for (; begin != end; ++begin)
            str.join(*begin);
        str.join("]");
    }
    else if constexpr (std::is_same_v<Any, _word_character_type>)
        str.join("[^[:alnum:]]");
    else if constexpr (std::is_same_v<Any, _letter_character_type>)
        str.join("[^[:alpha:]]");
    else if constexpr (std::is_same_v<Any, _digit_character_type>)
        str.join("\\D");
    else if constexpr (std::is_same_v<Any, _whitespace_character_type>)
        str.join("\\S");
    else if constexpr (std::is_same_v<Any, _word_boundary_type>)
        str.join("\\B");
    else if constexpr (std::is_same_v<Any, _hexadecimal_character_type>)
        str.join("[^[:xdigit:]]");
    else if constexpr (std::is_same_v<Any, _punctuation_character_type>)
        str.join("[^[:punct:]]");
    else if constexpr (std::is_same_v<Any, _start_of_line_type>)
        str.join("(?!^)");
    else if constexpr (std::is_same_v<Any, _end_of_line_type>)
        str.join("(?!$)");
    else if constexpr (std::is_same_v<Any, _start_of_string_type>)
        str.join("(?!\\A)");
    else if constexpr (std::is_same_v<Any, _end_of_string_type>)
        str.join("(?!\\z)");
    else
        static_assert(always_false_v<Any>, WL_ERROR_STRING_EXCEPT);
    return std::move(s);
}
template<typename... Patterns, typename State>
auto compile(_pattern_except<_pattern_alternatives<Patterns...>> p,
    string& str, State s, tag_t)
{
    if (!compile_use_bracket(p.pattern))
        throw std::logic_error(WL_ERROR_STRING_EXCEPT);
    str.join("[^");
    auto s2 = compile_alternatives(std::move(p.pattern), str, std::move(s),
        CharacterSet);
    str.join("]");
    return std::move(s2);
}
template<typename Any, typename State>
auto compile(Any&& any, string& str, State s, tag_t tag)
{
    using AT = remove_cvref_t<Any>;
    if constexpr (is_string_view_v<AT>)
    {
        compile_impl(any.byte_begin(), any.byte_end(), str, tag);
        return std::move(s);
    }
    else if constexpr (array_rank_v<AT> == 1u)
    {
        static_assert(is_string_v<value_type_t<AT>>, WL_ERROR_NOT_A_PATTERN);
        const auto& valany = allows<view_category::Array>(
            std::forward<decltype(any)>(any));
        return compile(valany, str, std::move(s), tag);
    }
    else
    {
        static_assert(is_pattern_v<AT>, WL_ERROR_NOT_A_PATTERN);
        if constexpr (std::is_same_v<AT, _whitespace_type>)
        {
            if (tag > Concatenation)
                str.join("(?:\\s+)");
            else
                str.join("\\s+");
        }
        else if constexpr (std::is_same_v<AT, _number_string_type>)
            str.join("(?:(?:\\+|-)?(?:\\d+(?:\\.\\d*)?|\\.\\d+))");
        else if constexpr (std::is_same_v<AT, _word_character_type>)
        {
            if (tag == CharacterSet)
                str.join("[:alnum:]");
            else
                str.join("[[:alnum:]]");
        }
        else if constexpr (std::is_same_v<AT, _letter_character_type>)
        {
            if (tag == CharacterSet)
                str.join("[:alpha:]");
            else
                str.join("[[:alpha:]]");
        }
        else if constexpr (std::is_same_v<AT, _digit_character_type>)
            str.join("\\d");
        else if constexpr (std::is_same_v<AT, _whitespace_character_type>)
            str.join("\\s");
        else if constexpr (std::is_same_v<AT, _hexadecimal_character_type>)
        {
            if (tag == CharacterSet)
                str.join("[:xdigit:]");
            else
                str.join("[[:xdigit:]]");
        }
        else if constexpr (std::is_same_v<AT, _punctuation_character_type>)
        {
            if (tag == CharacterSet)
                str.join("[:punct:]");
            else
                str.join("[[:punct:]]");
        }
        else if constexpr (std::is_same_v<AT, _word_boundary_type>)
            str.join("\\b");
        else if constexpr (std::is_same_v<AT, _start_of_line_type>)
            str.join("^");
        else if constexpr (std::is_same_v<AT, _end_of_line_type>)
            str.join("$");
        else if constexpr (std::is_same_v<AT, _start_of_string_type>)
            str.join("\\A");
        else if constexpr (std::is_same_v<AT, _end_of_string_type>)
            str.join("\\z");
        else
            static_assert(always_false_v<Any>, WL_ERROR_INTERNAL);
        return std::move(s);
    }
}
template<size_t I, typename... Patterns, typename State, typename FormatIdList>
auto format_compile_impl(_string_expression<Patterns...>&& p,
    string& str, const State& s, FormatIdList f)
{
    if constexpr (I < sizeof...(Patterns))
    {
        auto f1 = format_compile(std::move(p).template get<I>(), str, s,
            std::move(f));
        return format_compile_impl<I + 1u>(std::move(p), str, s,
            std::move(f1));
    }
    else
    {
        return std::move(f);
    }
}
template<typename... Patterns, typename State, typename FormatIdList>
auto format_compile(_string_expression<Patterns...>&& p, string& str,
    const State& s, FormatIdList f)
{
    return format_compile_impl<0u>(std::move(p), str, s, std::move(f));
}
template<int64_t Id, typename State, typename FormatIdList>
auto format_compile(_named_replacement<Id>, string& str, const State&,
    FormatIdList)
{
    constexpr auto group_idx = State::PatternIdList::find(const_int<Id>{});
    static_assert(0u < group_idx && group_idx <= 99u, WL_ERROR_INTERNAL);
    str.append('$');
    str.join(_integer_to_string(group_idx, 2u));
    return FormatIdList::append(const_int<group_idx>{});
}
inline void format_compile_impl(const utf8::char_t* begin,
    const utf8::char_t* end, string& str)
{
    WL_THROW_IF_ABORT()
    for (; begin < end; ++begin)
    {
        if (*begin == utf8::char_t('$'))
            str.append('$');
        str.append(*begin);
    }
    str.place_null_character();
}
template<typename State, typename FormatIdList>
auto format_compile(const string& s, string& str, const State&,
    FormatIdList)
{
    format_compile_impl(s.byte_begin(), s.byte_end(), str);
    return FormatIdList{};
}
template<typename State, typename FormatIdList>
auto format_compile(const string_view& s, string& str, const State&,
    FormatIdList)
{
    format_compile_impl(s.byte_begin(), s.byte_end(), str);
    return FormatIdList{};
}
template<typename Any, typename State, typename FormatIdList>
auto _string_replacement_compile(
    Any&& any, string& str, const State&, FormatIdList f)
{
    using AT = remove_cvref_t<Any>;
    if constexpr (is_string_view_v<AT>)
    {
        format_compile_impl(any.byte_begin(), any.byte_end(), str);
        return std::move(f);
    }
    else
    {
        static_assert(always_false_v<Any>, WL_ERROR_NOT_A_REPLACEMENT);
        return std::move(f);
    }
}
template<typename Expression>
auto compile(Expression e)
{
    auto str = string("(?ms)");
    using InitialState = compilation_state<
        _pattern_condition_list<>, _pattern_id_list<>>;
    auto s1 = compile(std::move(e), str, InitialState{}, WithinGroup);
    using CP = compiled_pattern<
        decltype(s1.conditions), typename decltype(s1)::PatternIdList>;
    try
    {
        WL_THROW_IF_ABORT()
        return CP{pcre2::new_regex(str), std::move(s1.conditions),
            std::move(str)};
    }
    catch (const std::logic_error& regex_error)
    {
        throw std::logic_error(std::string(WL_ERROR_INTERNAL) +
            WL_ERROR_REGEX + regex_error.what());
    }
}
template<typename C, typename P>
auto compile(compiled_pattern<C, P>&& pattern)
{
    return std::move(pattern);
}
template<typename C, typename P>
const auto& compile(const compiled_pattern<C, P>& pattern)
{
    return pattern;
}
template<typename P, typename R>
auto compile(compiled_rule<P, R>&& rule)
{
    return std::move(rule);
}
template<typename P, typename R>
const auto& compile(const compiled_rule<P, R>& rule)
{
    return rule;
}
template<typename Left, typename Right>
auto compile(_pattern_rule<Left, Right> rule)
{
    auto cp = compile(std::move(rule.left));
    auto s = compilation_state<
        _pattern_condition_list<>, typename decltype(cp)::PatternIdList>{};
    auto format = string();
    auto f = format_compile(std::move(rule.right), format, s,
        _pattern_id_list<>{});
    auto replacement = compiled_format<decltype(f)>{format};
    return compiled_rule<decltype(cp), decltype(replacement)>{
        std::move(cp), std::move(replacement)};
}
template<typename State>
inline void format_text(const State& state, const string& format, string& ret)
{
    auto begin = format.byte_begin();
    auto end = format.byte_end();
    while (begin != end)
    {
        if (*begin != '$')
        {
            ret.template append<false>(*begin++);
        }
        else if (++begin == end)
        {
            ret.template append<false>('$');
        }
        else if (*begin == '$')
        {
            ret.template append<false>('$');
            ++begin;
        }
        else if (utf8::char_t('0') <= *begin && *begin <= utf8::char_t('9'))
        {
            auto group_idx = size_t(*begin++ - utf8::char_t('0'));
            const bool is_two_digit = begin != end &&
                (utf8::char_t('0') <= *begin && *begin <= utf8::char_t('9'));
            if (is_two_digit)
                group_idx = group_idx * 10u +
                size_t(*begin++ - utf8::char_t('0'));
            if (group_idx <= state.capture_count())
            {
                const auto& view = state.match(group_idx);
                ret.template append<false>(view.byte_data(), view.byte_size());
            }
        }
        else
        {
            ret.template append<false>('$');
            ret.template append<false>(*begin++);
        }
    }
    ret.place_null_character();
}
}
template<typename String>
auto regular_expression(const String& str)
{
    WL_TRY_BEGIN()
    using CP = strexp::compiled_pattern<
        _pattern_condition_list<>, _pattern_id_list<>>;
    try
    {
        WL_THROW_IF_ABORT()
        return CP{pcre2::new_regex(str)};
    }
    catch (const std::logic_error& regex_error)
    {
        throw std::logic_error(
            std::string(WL_ERROR_REGEX) + regex_error.what());
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename CharT>
boolean _string_equal(const CharT* a_begin, const CharT* a_end,
    const CharT* b_begin, const CharT* b_end)
{
    if ((a_end - a_begin) != (b_end - b_begin))
        return const_false;
    for (; a_begin < a_end; ++a_begin, ++b_begin)
    {
        if (*a_begin != *b_begin)
            return const_false;
    }
    return const_true;
}
inline boolean operator==(const string& a, const string& b)
{
    return _string_equal(a.byte_begin(), a.byte_end(),
        b.byte_begin(), b.byte_end());
}
inline boolean operator==(const string& a, const string_view& b)
{
    return _string_equal(a.byte_begin(), a.byte_end(),
        b.byte_begin(), b.byte_end());
}
inline boolean operator==(const string_view& a, const string& b)
{
    return _string_equal(a.byte_begin(), a.byte_end(),
        b.byte_begin(), b.byte_end());
}
inline boolean operator==(const string_view& a, const string_view& b)
{
    return _string_equal(a.byte_begin(), a.byte_end(),
        b.byte_begin(), b.byte_end());
}
template<typename C, typename P>
auto _pattern_convert_impl(const strexp::compiled_pattern<C, P>& pattern)
{
    return pattern.regex_text;
}
template<typename P, typename R>
auto _pattern_convert_impl(const strexp::compiled_rule<P, R>& rule)
{
    auto ret = _pattern_convert_impl(rule.pattern);
    ret.join(" -> ");
    ret.join(rule.format.text);
    return ret;
}
template<typename Any>
auto _pattern_convert(Any&& any)
{
    WL_TRY_BEGIN()
    return _pattern_convert_impl(strexp::compile(
        std::forward<decltype(any)>(any)));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
#define WL_DEFINE_STRING_PRED_FUNCTION_IMPL(name, anchors)                  \
template<bool Overlaps = false, typename String, typename C, typename P>    \
auto _string_##name##_q_impl(String&& str,                                  \
    const strexp::compiled_pattern<C, P>& pattern)                          \
{                                                                           \
    static_assert(is_string_view_v<remove_cvref_t<String>>,                 \
        WL_ERROR_STRING_FUNCTION_STRING);                                   \
    auto search = pcre2::regex_search(pattern, str.c_str(),                 \
        str.byte_size());                                                   \
    return boolean(search.find_next(false, anchors));                       \
}
WL_DEFINE_STRING_PRED_FUNCTION_IMPL(match, PCRE2_ANCHORED | PCRE2_ENDANCHORED)
WL_DEFINE_STRING_PRED_FUNCTION_IMPL(starts, PCRE2_ANCHORED)
WL_DEFINE_STRING_PRED_FUNCTION_IMPL(ends, PCRE2_ENDANCHORED)
WL_DEFINE_STRING_PRED_FUNCTION_IMPL(contains, 0)
template<bool Overlaps = false, typename String, typename C, typename P>
auto _string_free_q_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    return !_string_contains_q_impl(std::forward<decltype(str)>(str), pattern);
}
template<bool Overlaps, typename String, typename C, typename P>
auto _string_count_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    int64_t num_matches = 0;
    auto search = pcre2::regex_search(pattern, str.c_str(), str.byte_size());
    while (search.find_next(Overlaps))
    {
        ++num_matches;
    }
    return num_matches;
}
template<bool Overlaps, typename String, typename P, typename R>
auto _string_cases_impl(String&& str,
    const strexp::compiled_rule<P, R>& rule)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<string, 1u> ret;
    auto search = pcre2::regex_search(
        rule.pattern, str.c_str(), str.byte_size());
    while (search.find_next(Overlaps))
    {
        auto str = string();
        strexp::format_text(search, rule.format.text, str);
        ret.append(std::move(str));
    }
    return ret;
}
template<bool Overlaps, typename String, typename C, typename P>
auto _string_cases_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<string, 1u> ret;
    auto search = pcre2::regex_search(pattern, str.c_str(), str.byte_size());
    while (search.find_next(Overlaps))
    {
        ret.append(string(search.match()));
    }
    return ret;
}
template<bool Overlaps = false, typename String, typename P, typename R>
auto _string_replace_impl(String&& str,
    const strexp::compiled_rule<P, R>& rule)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    string ret;
    auto search = pcre2::regex_search(
        rule.pattern, str.c_str(), str.byte_size());
    while (search.find_next())
    {
        ret.join<false>(search.prefix());
        strexp::format_text(search, rule.format.text, ret);
    }
    ret.join<false>(
        string_view{search.prefix().byte_begin(), str.byte_end()});
    ret.place_null_character();
    return ret;
}
template<bool Overlaps = false, typename String, typename C, typename P>
auto _string_split_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<string, 1u> ret;
    auto search = pcre2::regex_search(pattern, str.c_str(), str.byte_size());
    while (search.find_next())
    {
        if (ret.size() > 0u || search.prefix().byte_size() > 0u)
            ret.append(string(search.prefix()));
    }
    const auto& prefix = string_view{
        search.prefix().byte_begin(), str.byte_end()};
    if (prefix.byte_size() > 0u)
        ret.append(string(prefix));
    return ret;
}
template<bool Overlaps = false, typename String, typename P, typename R>
auto _string_split_impl(String&& str,
    const strexp::compiled_rule<P, R>& rule)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<string, 1u> ret;
    auto search = pcre2::regex_search(
        rule.pattern, str.c_str(), str.byte_size());
    while (search.find_next())
    {
        if (ret.size() > 0u || search.prefix().byte_size() > 0u)
            ret.append(string(search.prefix()));
        auto replaced = string();
        strexp::format_text(search, rule.format.text, replaced);
        ret.append(std::move(replaced));
    }
    const auto& prefix = string_view{
        search.prefix().byte_begin(), str.byte_end()};
    if (prefix.byte_size() > 0u)
        ret.append(string(prefix));
    return ret;
}
template<bool Overlaps, typename String, typename C, typename P>
auto _string_position_impl(String&& str,
    const strexp::compiled_pattern<C, P>& pattern)
{
    static_assert(is_string_view_v<remove_cvref_t<String>>,
        WL_ERROR_STRING_FUNCTION_STRING);
    ndarray<int64_t, 1u> ret;
    size_t ret_size = 0u;
    auto search = pcre2::regex_search(pattern, str.c_str(), str.byte_size());
    while (search.find_next(Overlaps))
    {
        ret.uninitialized_resize(std::array<size_t, 1u>{ret_size + 2u});
        auto* insert_ptr = ret.data() + ret_size;
        insert_ptr[0] = int64_t(search.match_begin_idx() + 1u);
        insert_ptr[1] = int64_t(search.match_end_idx());
        ret_size += 2u;
    }
    return ndarray<int64_t, 2u>(std::array<size_t, 2u>{ret_size / 2u, 2u},
        std::move(ret).data_vector());
}
#define WL_DEFINE_STRING_FUNCTION(name, ret_type, takes_list, can_overlap,  \
    overlap_default)                                                        \
template<bool Overlaps = overlap_default, typename String, typename Any>    \
auto name(String&& str, Any&& any)                                          \
{                                                                           \
    WL_TRY_BEGIN()                                                          \
    static_assert(can_overlap || !Overlaps, WL_ERROR_INTERNAL);             \
    using ST = remove_cvref_t<String>;                                      \
    const auto& pattern = strexp::compile(std::forward<decltype(any)>(any));\
    if constexpr (is_string_view_v<ST>)                                     \
    {                                                                       \
        return _##name##_impl<Overlaps>(str, pattern);                      \
    }                                                                       \
    else if constexpr (takes_list)                                          \
    {                                                                       \
        static_assert(is_string_type_v<ST> && array_rank_v<ST> == 1u,       \
            WL_ERROR_STRING_TYPE_ONLY);                                     \
        if constexpr (is_movable_v<String> &&                               \
            std::is_same_v<ret_type, string>)                               \
        {                                                                   \
            str.for_each([&](string& s)                                     \
                { s =  _##name##_impl<Overlaps>(s, pattern); });            \
            return std::move(str);                                          \
        }                                                                   \
        else                                                                \
        {                                                                   \
            ndarray<ret_type, 1u> ret(std::array<size_t, 1u>{str.size()});  \
            str.for_each([&](const string& s, ret_type& c)                  \
                { c = _##name##_impl<Overlaps>(s, pattern); }, ret.data()); \
            return ret;                                                     \
        }                                                                   \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        static_assert(always_false_v<String>, WL_ERROR_STRING_ONLY);        \
    }                                                                       \
    WL_TRY_END(__func__, __FILE__, __LINE__)                                \
}
WL_DEFINE_STRING_FUNCTION(string_count, int64_t, true, true, false)
WL_DEFINE_STRING_FUNCTION(string_match_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_contains_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_free_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_starts_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_ends_q, boolean, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_replace, string, true, false, false)
WL_DEFINE_STRING_FUNCTION(string_cases, int, false, true, false)
WL_DEFINE_STRING_FUNCTION(string_split, int, false, false, false)
WL_DEFINE_STRING_FUNCTION(string_position, int, false, true, true)
template<typename String>
auto string_split(String&& str)
{
    WL_TRY_BEGIN()
    static auto whitespace = strexp::compile(const_whitespace);
    return string_split(std::forward<decltype(str)>(str), whitespace);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
#define WL_DEFINE_CHAR_TYPE_FUNCTION(name, crit)                    \
template<typename String>                                           \
auto name(const String& str)                                        \
{                                                                   \
    using namespace utf8;                                           \
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);  \
    auto begin = str.byte_begin();                                  \
    auto end = str.byte_end();                                      \
    for (; begin != end; ++begin)                                   \
    {                                                               \
        const auto ch = *begin;                                     \
        if (!(crit))                                                \
            return const_false;                                     \
    }                                                               \
    return const_true;                                              \
}
WL_DEFINE_CHAR_TYPE_FUNCTION(digit_q, '0'_c <= ch && ch <= '9'_c)
WL_DEFINE_CHAR_TYPE_FUNCTION(letter_q,
    ('a'_c <= ch && ch <= 'z'_c) || ('A'_c <= ch && ch <= 'Z'_c))
WL_DEFINE_CHAR_TYPE_FUNCTION(lower_case_q, 'a'_c <= ch && ch <= 'z'_c)
WL_DEFINE_CHAR_TYPE_FUNCTION(upper_case_q, 'A'_c <= ch && ch <= 'Z'_c)
WL_DEFINE_CHAR_TYPE_FUNCTION(printable_ascii_q, ' '_c <= ch && ch <= '~'_c)
template<typename String>
auto characters(const String& s)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    const auto size = s.size();
    ndarray<string, 1u> ret(std::array<size_t, 1u>{size});
    auto ret_data = ret.data();
    if (size == s.byte_size())
    { // ascii only
        auto s_begin = s.byte_data();
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i,
                ++ret_data, ++s_begin)
            {
                *ret_data = string(s_begin, 1u, true);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    else
    { // not ascii only
        auto s_begin = s.begin();
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i, ++ret_data)
            {
                size_t num_bytes = s_begin.num_bytes();
                *ret_data = string(s_begin.get_pointer(), num_bytes,
                    num_bytes == 1u);
                s_begin.apply_pointer_offset(num_bytes);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto _character_range_impl(const X& x)
{
    static_assert(is_string_view_v<X> || is_integral_v<X>,
        WL_ERROR_CHARACTER_RANGE);
    if constexpr (is_string_view_v<X>)
    {
        if (!strexp::is_single_character(x))
            throw std::logic_error(WL_ERROR_CHARACTER_RANGE);
        return *x.begin();
    }
    else
    {
        if (uint64_t(x) > uint64_t(utf8::max_code_point))
            throw std::logic_error(WL_ERROR_INVALID_CODEPOINT);
        return utf8::char21_t(x);
    }
}
template<typename X, typename Y>
auto character_range(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    utf8::char21_t first_point = _character_range_impl(x);
    utf8::char21_t last_point = _character_range_impl(y);
    if (first_point > last_point)
        return ndarray<string, 1u>{};
    else
    {
        const auto size = size_t(last_point - first_point + 1u);
        ndarray<string, 1u> ret(std::array<size_t, 1u>{size});
        auto ret_data = ret.data();
        if (utf8::is_ascii(last_point))
        {
            WL_THROW_IF_ABORT()
            for (; first_point <= last_point; ++first_point, ++ret_data)
            {
                const auto ch = utf8::char_t(first_point);
                *ret_data = string(&ch, 1u, true);
            }
        }
        else
        {
            WL_THROW_IF_ABORT()
            for (; first_point <= utf8::max_ascii_code_point;
                ++first_point, ++ret_data)
            {
                const auto ch = utf8::char_t(first_point);
                *ret_data = string(&ch, 1u, true);
            }
            WL_CHECK_ABORT_LOOP_BEGIN(last_point - first_point + 1u)
                for (auto i = _loop_begin; i < _loop_end; ++i, ++ret_data)
                {
                    auto [num_bytes, units] = 
                        utf8::from_code_point(first_point + i);
                    *ret_data = string(units.data(), num_bytes, false);
                }
            WL_CHECK_ABORT_LOOP_END()
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename String>
auto to_character_code(const String& s)
{
    WL_TRY_BEGIN()
    static_assert(is_string_view_v<String>, WL_ERROR_STRING_ONLY);
    static_assert(is_integral_v<Ret> &&
        (std::numeric_limits<Ret>::max() >= 255), WL_ERROR_BAD_RETURN);
    const auto size = s.size();
    ndarray<Ret, 1u> ret(std::array<size_t, 1u>{size});
    auto ret_data = ret.data();
    if (size == s.byte_size())
    { // ascii only
        auto s_begin = s.byte_data();
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i,
                ++ret_data, ++s_begin)
            {
                *ret_data = int64_t(*s_begin);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    else
    { // not ascii only
        auto s_begin = s.begin();
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i,
                ++ret_data, ++s_begin)
            {
                *ret_data = int64_t(*s_begin);
            }
        WL_CHECK_ABORT_LOOP_END()
    }
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto from_character_code(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> <= 1u, WL_ERROR_FROM_CHARACTER_CODE_ARG);
    if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_integral_v<X>, WL_ERROR_FROM_CHARACTER_CODE_ARG);
        const auto [num_bytes, units] =
            utf8::from_code_point(utf8::char21_t(x));
        return string(units.data(), num_bytes, num_bytes == 1u);
    }
    else
    {
        static_assert(is_integral_v<value_type_t<X>>,
            WL_ERROR_FROM_CHARACTER_CODE_ARG);
        const auto& valx = allows<view_category::Simple>(x);
        const auto size = valx.size();
        auto x_begin = valx.begin();
        bool ascii_only = true;
        auto ret = string();
        if (size > string::small_string_byte_size)
            ret.set_dynamic_capacity(size);
        WL_CHECK_ABORT_LOOP_BEGIN(size)
            for (auto i = _loop_begin; i < _loop_end; ++i, ++x_begin)
            {
                auto [num_bytes, units] = utf8::from_code_point(*x_begin);
                if (ascii_only)
                    ascii_only = (num_bytes == 1u);
                ret.template append<false>(units.data(), num_bytes);
            }
        WL_CHECK_ABORT_LOOP_END()
        ret.place_null_character();
        ret.set_ascii_only(ascii_only);
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
int64_t _order_string(const X& x, const Y& y)
{
    static_assert(is_string_view_v<X> && is_string_view_v<Y>,
        WL_ERROR_OPERAND_TYPE);
    if (x.ascii_only() && y.ascii_only())
    {
        auto x_begin = x.byte_begin();
        auto x_end = x.byte_end();
        auto y_begin = y.byte_begin();
        auto y_end = y.byte_end();
        for (; true; ++x_begin, ++y_begin)
        {
            if (x_begin == x_end)
                return (y_begin == y_end) ? 0 : 1;
            else if (y_begin == y_end)
                return -1;
            else if (*x_begin < *y_begin)
                return 1;
            else if (*x_begin > *y_begin)
                return -1;
        }
    }
    else
    {
        auto x_begin = x.begin();
        auto x_end = x.end();
        auto y_begin = y.begin();
        auto y_end = y.end();
        for (; true; ++x_begin, ++y_begin)
        {
            if (x_begin == x_end)
                return (y_begin == y_end) ? 0 : 1;
            else if (y_begin == y_end)
                return -1;
            else if (*x_begin < *y_begin)
                return 1;
            else if (*x_begin > *y_begin)
                return -1;
        }
    }
}
int64_t _order_scalar(const string& x, const string& y)
{
    return _order_string(x, y);
}
int64_t _order_scalar(const string& x, const string_view& y)
{
    return _order_string(x, y);
}
int64_t _order_scalar(const string_view& x, const string& y)
{
    return _order_string(x, y);
}
int64_t _order_scalar(const string_view& x, const string_view& y)
{
    return _order_string(x, y);
}
struct _string_riffle_delimiter
{
    std::array<string, 3u> dels_{};
    bool triple_ = false;
    _string_riffle_delimiter() = default;
    _string_riffle_delimiter(const string& str) :
        dels_{string(), str, string()}, triple_{false}
    {
    }
    _string_riffle_delimiter(const string_view& str) :
        dels_{string(), string(str), string()}, triple_{false}
    {
    }
    template<typename X>
    _string_riffle_delimiter(const X& x) : triple_{true}
    {
        static_assert(array_rank_v<X> == 1u && is_string_v<value_type_t<X>>,
            WL_ERROR_STRING_RIFFLE_DELIMITER_TYPES);
        if (x.size() != 3u)
            throw std::logic_error(WL_ERROR_STRING_RIFFLE_DELIMITER_TYPES);
        x.copy_to(dels_.data());
    }
    bool triple() const
    {
        return triple_;
    }
    const string& left() const
    {
        return dels_[0];
    }
    const string& separator() const
    {
        return dels_[1];
    }
    const string& right() const
    {
        return dels_[2];
    }
    size_t extra_byte_size(size_t num_string) const
    {
        size_t size = triple_ ? left().byte_size() + right().byte_size() : 0u;
        return (num_string < 2u) ? size :
            size + (num_string - 1u) * separator().byte_size();
    }
};
template<typename X, size_t XR, size_t ND>
auto _string_riffle_get_byte_size(const X& x,
    const std::array<size_t, XR>& x_dims,
    const std::array<_string_riffle_delimiter, ND>& del_array)
{
    size_t string_size = 0u;
    x.for_each([&](const string& s) { string_size += s.byte_size(); });
    size_t delimiter_size = 0u;
    for (auto i = int64_t(XR) - 1; i >= 0; --i)
    {
        delimiter_size *= x_dims[i];
        if constexpr (ND == 0u)
            delimiter_size += (x_dims[i] > 2u) ? x_dims[i] - 1u : 0u;
        else
            delimiter_size += del_array[i].extra_byte_size(x_dims[i]);
    }
    return string_size + delimiter_size;
}
constexpr char newline_delimiter[] = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
static_assert(MaximumArrayRank == 16u, WL_ERROR_INTERNAL);
template<size_t I, size_t XR, size_t ND>
void _string_riffle_impl(
    const string*& x_ptr, const std::array<size_t, XR>& x_dims,
    const std::array<_string_riffle_delimiter, ND>& del_array, string& ret)
{
    if constexpr (ND != 0u)
        if (del_array[I].triple())
            ret.template join<false>(del_array[I].left());
    if constexpr (I + 1u == XR)
    {
        auto size = x_dims[XR - 1u];
        if (size > 0u)
        {
            ret.join<false>(*x_ptr++);
            WL_CHECK_ABORT_LOOP_BEGIN(size - 1u)
                for (auto i = _loop_begin; i < _loop_end; ++i, ++x_ptr)
                {
                    if constexpr (ND != 0u)
                        ret.template join<false>(
                            del_array[XR - 1u].separator());
                    else
                        ret.template join<false>(" ");
                    ret.join<false>(*x_ptr);
                }
            WL_CHECK_ABORT_LOOP_END()
        }
    }
    else
    {
        const auto size = x_dims[I];
        if (size > 0u)
        {
            _string_riffle_impl<I + 1u>(x_ptr, x_dims, del_array, ret);
            for (size_t i = 1u; i < size; ++i)
            {
                if constexpr (ND != 0u)
                    ret.template join<false>(del_array[I].separator());
                else
                    ret.template append<false>(
                    (const utf8::char_t*)newline_delimiter, XR - I - 1u);
                _string_riffle_impl<I + 1u>(x_ptr, x_dims, del_array, ret);
            }
        }
    }
    if constexpr (ND != 0u)
        if (del_array[I].triple())
            ret.template join<false>(del_array[I].right());
    if (I == 0u)
        ret.place_null_character();
}
template<typename X, typename... Dels>
auto string_riffle(const X& x, const Dels&... dels)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto num_dels = sizeof...(Dels);
    static_assert(num_dels == 0u || num_dels == XR,
        WL_ERROR_STRING_RIFFLE_DELIMITERS);
    static_assert(XR >= 1u && is_string_v<value_type_t<X>>,
        WL_ERROR_STRING_RIFFLE_STRINGS);
    const auto& valx = allows<view_category::Simple>(x);
    const auto x_dims = valx.dims();
    auto x_data = valx.data();
    std::array<_string_riffle_delimiter, num_dels> del_array{dels...};
    const auto ret_byte_size = _string_riffle_get_byte_size(
        valx, x_dims, del_array);
    auto ret = string();
    if (ret_byte_size > string::small_string_byte_size)
        ret.set_dynamic_capacity(ret_byte_size);
    _string_riffle_impl<0u>(x_data, x_dims, del_array, ret);
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, size_t N>
auto _to_string_scalar_impl(const X& x, char (&buffer)[N])
{
    WL_THROW_IF_ABORT()
    static_assert(array_rank_v<X> == 0u, WL_ERROR_INTERNAL);
    if constexpr (is_integral_v<X>)
    {
        static_assert(N >= _to_string_impl::integer_buffer_size,
            WL_ERROR_INTERNAL);
        return _to_string_impl::integer_to_string(buffer, buffer + N, x);
    }
    else if constexpr (is_real_v<X>)
    {
        static_assert(N >= _to_string_impl::double_buffer_size,
            WL_ERROR_INTERNAL);
        return _to_string_impl::double_to_string(buffer, buffer + N, x);
    }
    else if constexpr (is_complex_v<X>)
    {
        static_assert(N >= _to_string_impl::complex_buffer_size,
            WL_ERROR_INTERNAL);
        return _to_string_impl::complex_to_string(buffer, buffer + N, x);
    }
    else if constexpr (is_string_view_v<X>)
    {
        return string_view(x.byte_begin(), x.byte_end());
    }
    else
    {
        static_assert(always_false_v<X>, WL_ERROR_TO_STRING_UNKNOWN);
    }
}
template<size_t I, typename XV, size_t XR, size_t N>
auto _to_string_array_impl(const XV*& x_ptr,
    const std::array<size_t, XR>& x_dims, string& ret, char(&buffer)[N])
{
    ret.template join<false>("{");
    if constexpr (I + 1u == XR)
    {
        auto size = x_dims[XR - 1u];
        if (size > 0u)
        {
            ret.join<false>(_to_string_scalar_impl(*x_ptr++, buffer));
            for (size_t i = 1u; i < size; ++i, ++x_ptr)
            {
                ret.template join<false>(", ");
                ret.join<false>(_to_string_scalar_impl(*x_ptr, buffer));
            }
        }
    }
    else
    {
        auto size = x_dims[I];
        _to_string_array_impl<I + 1u>(x_ptr, x_dims, ret, buffer);
        for (size_t i = 1u; i < size; ++i)
        {
            ret.template join<false>(", ");
            _to_string_array_impl<I + 1u>(x_ptr, x_dims, ret, buffer);
        }
    }
    ret.template join<false>("}");
    if (I == 0u)
        ret.place_null_character();
}
template<typename X>
auto to_string(const X& x)
{
    WL_TRY_BEGIN()
    char buffer[64];
    if constexpr (array_rank_v<X> == 0u)
    {
        return string(_to_string_scalar_impl(x, buffer));
    }
    else
    {
        auto ret = string();
        const auto& valx = allows<view_category::Simple>(x);
        const auto x_dims = valx.dims();
        auto x_data = valx.data();
        _to_string_array_impl<0u>(x_data, x_dims, ret, buffer);
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
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
    std::cout << to_string(x)._ascii_string() << std::endl;
    return const_null;
}
template<typename X>
auto echo(X&& x)
{
    std::cout << to_string(x)._ascii_string() << std::endl;
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
inline auto directory()
{
    WL_TRY_BEGIN()
    auto dir = std::filesystem::current_path().u8string();
    return string(dir.c_str(), dir.size());
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
struct random_engine
{
    using _base_engine_t = WL_BASE_RANDOM_ENGINE;
    static_assert(uint64_t(_base_engine_t::max()) -
        uint64_t(_base_engine_t::min()) == uint64_t(-1),
        "64-bit random engine is required.");
    static constexpr size_t bits_width = 64u;
    using result_type = uint64_t;
    _base_engine_t base_engine_{};
    uint64_t active_bits_{};
    uint64_t reserved_bits_{};
    size_t n_bits_active_{};
    random_engine()
    {
        update_all_bits();
    }
    static constexpr auto max()
    {
        return _base_engine_t::max();
    }
    static constexpr auto min()
    {
        return _base_engine_t::min();
    }
    WL_INLINE auto base_engine_get_bits()
    {
        return uint64_t(base_engine_());
    }
    auto get_bits(size_t n)
    {
        if (n >= 64u)
            return base_engine_get_bits();
        else if (n > 32u)
            return base_engine_get_bits() >> (64u - n);
        uint64_t ret = active_bits_ >> (bits_width - n);
        if (n <= n_bits_active_)
        {
            active_bits_ <<= n;
            n_bits_active_ -= n;
        }
        else
        {
            size_t n_bits_short = n - n_bits_active_;
            ret |= reserved_bits_ >> (bits_width - n_bits_short);
            active_bits_ = reserved_bits_ << n_bits_short;
            n_bits_active_ = (bits_width - n_bits_short);
            reserved_bits_ = base_engine_get_bits();
        }
        return ret;
    }
    void update_all_bits()
    {
        active_bits_ = base_engine_get_bits();
        reserved_bits_ = base_engine_get_bits();
        n_bits_active_ = bits_width;
    }
    template<typename Any>
    void seed(Any&& any)
    {
        base_engine_.seed(std::forward<decltype(any)>(any));
        update_all_bits();
    }
    WL_INLINE auto operator()()
    {
        return base_engine_get_bits();
    }
};
extern random_engine global_random_engine;
namespace distribution
{
template<typename T>
void adjust_bounds(T& min, T& max)
{
    if (min > max)
        std::swap(min, max);
}
template<typename T>
void adjust_bounds(complex<T>& min, complex<T>& max)
{
    auto min_real = min.real();
    auto min_imag = min.imag();
    auto max_real = max.real();
    auto max_imag = max.imag();
    adjust_bounds(min_real, max_real);
    adjust_bounds(min_imag, max_imag);
    min = complex<T>(min_real, min_imag);
    max = complex<T>(max_real, max_imag);
}
template<typename T, bool Multiple = false>
struct uniform
{
    static_assert(is_real_v<T>, WL_ERROR_INTERNAL);
    using value_type = T;
    static constexpr size_t rank = 0;
    T min_;
    T max_;
    uint64_t range_{};
    size_t n_bits_{};
    uniform(T a, T b) : min_{a}, max_{b}
    {
        adjust_bounds(min_, max_);
        if constexpr (is_integral_v<T> && Multiple)
        {
            const auto diff = uint64_t(max_) - uint64_t(min_);
            n_bits_ = size_t(64u - utils::lzcnt_u64(diff));
            range_ = diff + 1u; // could be zero
        }
    }
    T generate_int()
    {
        if (range_ == 0u)
            return T(global_random_engine());
        if constexpr (Multiple)
        {
            for (;;)
            {
                uint64_t rand = global_random_engine.get_bits(n_bits_);
                if (rand <= uint64_t(range_ - 1u))
                    return T(rand + std::make_unsigned_t<T>(min_));
            }
        }
        else
        {
            for (;;)
            {
                uint64_t rand = global_random_engine();
                uint64_t mask = uint64_t(-1);
                if (rand / range_ < mask / range_ ||
                    mask % range_ == (range_ - 1u)) {
                    return T((rand % range_) + std::make_unsigned_t<T>(min_));
                }
            }
        }
    }
    void generate(T* res)
    {
        if constexpr (is_integral_v<T>)
            *res = generate_int();
        else
            *res = std::uniform_real_distribution<T>(
                min_, max_)(global_random_engine);
    }
};
template<typename T, bool Multiple>
struct uniform<complex<T>, Multiple>
{
    static_assert(is_float_v<T>, WL_ERROR_INTERNAL);
    using value_type = complex<T>;
    static constexpr size_t rank = 0;
    complex<T> min_;
    complex<T> max_;
    uniform(const complex<T>& a, const complex<T>& b) : min_{a}, max_{b}
    {
        adjust_bounds(min_, max_);
    }
    void generate(complex<T>* res)
    {
        using dist_type = std::uniform_real_distribution<T>;
        auto re_dist = dist_type(min_.real(), max_.real());
        auto im_dist = dist_type(min_.imag(), max_.imag());
        *res = complex<T>(re_dist(global_random_engine),
            im_dist(global_random_engine));
    }
};
template<typename T>
struct multi_uniform
{
    static_assert(is_arithmetic_v<T>, WL_ERROR_INTERNAL);
    using value_type = T;
    static constexpr size_t rank = 1;
    ndarray<T, 2u> bounds_;
    multi_uniform(const ndarray<T, 2u>& bounds) : bounds_(bounds)
    {
        _initialize();
    }
    multi_uniform(ndarray<T, 2u>&& bounds) : bounds_(std::move(bounds))
    {
        _initialize();
    }
    void _initialize()
    {
        if (!(bounds_.dims()[0] >= 1u && bounds_.dims()[1] == 2u))
            throw std::logic_error(WL_ERROR_INTERNAL);
        const auto length = bounds_.dims()[0];
        auto iter = bounds_.data();
        for (size_t i = 0; i < length; ++i, iter += 2)
            adjust_bounds(iter[0], iter[1]);
    }
    size_t length() const
    {
        return bounds_.dims()[0];
    }
    void _single_generate(T* res, T* bounds)
    {
        if constexpr (is_complex_v<T>)
        {
            using dist_type = std::uniform_real_distribution<T>;
            auto re_dist = dist_type(bounds[0].real(), bounds[1].real());
            auto im_dist = dist_type(bounds[0].imag(), bounds[1].imag());
            res->real() = re_dist(global_random_engine);
            res->imag() = im_dist(global_random_engine);
        }
        else
        {
            using dist_type = std::conditional_t<is_integral_v<T>,
                std::uniform_int_distribution<T>,
                std::uniform_real_distribution<T>>;
            auto dist = dist_type(bounds[0], bounds[1]);
            *res = dist(global_random_engine);
        }
    }
    void generate(T* res)
    {
        const auto length = this->length();
        auto bounds_ptr = bounds_.data();
        for (size_t i = 0; i < length; ++i, ++res, bounds_ptr += 2)
            _single_generate(res, bounds_ptr);
    }
};
template<typename T>
struct default_multi_uniform
{
    static_assert(is_float_v<T>, WL_ERROR_INTERNAL);
    using value_type = T;
    static constexpr size_t rank = 1;
    size_t length_;
    default_multi_uniform(size_t length) : length_{length}
    {
        if (!(length >= 1u))
            throw std::logic_error(WL_ERROR_INTERNAL);
    }
    size_t length() const
    {
        return length_;
    }
    void generate(T* res)
    {
        for (size_t i = 0; i < length_; ++i, ++res)
        {
            auto dist = std::uniform_real_distribution<T>();
            *res = dist(global_random_engine);
        }
    }
};
template<typename RT, template<typename> typename Dist, typename... Params>
struct scalar_distribution
{
    using value_type = RT;
    static constexpr size_t rank = 0;
    Dist<RT> dist_;
    scalar_distribution(const Params&... params) : dist_(params...)
    {
    }
    void generate(RT* res)
    {
        *res = dist_(global_random_engine);
    }
};
#define WL_DEFINE_DISTRIBUTION_TYPE(name, stdname)          \
template<typename ReturnType, typename... Params>           \
using name = scalar_distribution<                           \
    ReturnType, std::stdname##_distribution, Params...>;
WL_DEFINE_DISTRIBUTION_TYPE(log_normal, lognormal)
WL_DEFINE_DISTRIBUTION_TYPE(normal, normal)
WL_DEFINE_DISTRIBUTION_TYPE(chi_square, chi_squared)
WL_DEFINE_DISTRIBUTION_TYPE(cauchy, cauchy)
WL_DEFINE_DISTRIBUTION_TYPE(student_t_nu, student_t)
WL_DEFINE_DISTRIBUTION_TYPE(f_ratio, fisher_f)
WL_DEFINE_DISTRIBUTION_TYPE(exponential, exponential)
WL_DEFINE_DISTRIBUTION_TYPE(poisson, poisson)
WL_DEFINE_DISTRIBUTION_TYPE(gamma, gamma)
WL_DEFINE_DISTRIBUTION_TYPE(weibull, weibull)
WL_DEFINE_DISTRIBUTION_TYPE(extreme_value, extreme_value)
WL_DEFINE_DISTRIBUTION_TYPE(geometric, geometric)
WL_DEFINE_DISTRIBUTION_TYPE(binomial, binomial)
WL_DEFINE_DISTRIBUTION_TYPE(negative_binomial, negative_binomial)
template<typename RT, typename Mu, typename Sigma, typename Nu>
struct student_t
{
    using value_type = RT;
    static constexpr size_t rank = 0;
    RT mu_;
    RT sigma_;
    std::student_t_distribution<RT> dist_;
    student_t(const Mu& mu, const Sigma& sigma, const Nu& nu) :
        mu_{RT(mu)}, sigma_{RT(sigma)}, dist_(nu)
    {
    }
    void generate(RT* res)
    {
        *res = dist_(global_random_engine) * sigma_ + mu_;
    }
};
template<typename RT, typename P>
struct bernoulli
{
    using value_type = RT;
    static constexpr size_t rank = 0;
    double p_;
    bernoulli(const P& p) : p_{double(p)}
    {
    }
    void generate(RT* res)
    {
        double rand = std::uniform_real_distribution<>()(global_random_engine);
        *res = RT(rand < double(p_));
    }
};
}
inline auto uniform_distribution()
{
    return distribution::uniform<double>(0., 1.);
}
template<typename X>
auto uniform_distribution(const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR <= 2u, WL_ERROR_UNIFORM_BOUNDS_SPEC);
    
    if constexpr (XR == 0u)
    {
        static_assert(is_integral_v<X>, WL_ERROR_UNIFORM_BOUNDS_SPEC);
        if (x <= X(0))
            throw std::logic_error(WL_ERROR_UNIFORM_BOUNDS_SPEC);
        return distribution::default_multi_uniform<double>(x);
    }
    else
    {
        using XV = value_type_t<X>;
        static_assert(is_arithmetic_v<XV>, WL_ERROR_UNIFORM_BOUNDS_SPEC);
        using RV = promote_integral_t<XV>;
        if constexpr (XR == 1u)
        {
            if (x.dims()[0] != 2u)
                throw std::logic_error(WL_ERROR_UNIFORM_BOUNDS_SPEC);
            std::array<RV, 2u> bounds;
            x.copy_to(bounds.data());
            return distribution::uniform<RV>(bounds[0], bounds[1]);
        }
        else // XR == 2u
        {
            static_assert(is_arithmetic_v<XV>, WL_ERROR_UNIFORM_BOUNDS_SPEC);
            if (!(x.dims()[0] >= 1u && x.dims()[1] == 2u))
                throw std::logic_error(WL_ERROR_UNIFORM_BOUNDS_SPEC);
            auto bounds = cast<ndarray<RV, 2u>>(x);
            return distribution::multi_uniform<RV>(bounds);
        }
    }
}
template<typename Nu>
auto chi_square_distribution(const Nu& nu)
{
    static_assert(is_real_v<Nu>, WL_ERROR_REAL_TYPE_ARG);
    if (!(nu > Nu(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::chi_square<promote_integral_t<Nu>, Nu>(nu);
}
template<typename Mean, typename Dev>
auto normal_distribution(const Mean& mean, const Dev& dev)
{
    static_assert(all_is_real_v<Mean, Dev>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Mean, Dev>>;
    if (!(dev > Dev(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::normal<P, Mean, Dev>(mean, dev);
}
inline auto normal_distribution()
{
    return normal_distribution(0., 1.);
}
template<typename Mean, typename Dev>
auto log_normal_distribution(const Mean& mean, const Dev& dev)
{
    static_assert(all_is_real_v<Mean, Dev>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Mean, Dev>>;
    if (!(dev > Dev(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::log_normal<P, Mean, Dev>(mean, dev);
}
template<typename A, typename B>
auto cauchy_distribution(const A& a, const B& b)
{
    static_assert(all_is_real_v<A, B>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<A, B>>;
    if (!(b > B(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::cauchy<P, A, B>(a, b);
}
inline auto cauchy_distribution()
{
    return cauchy_distribution(0., 1.);
}
template<typename Mean, typename Scale, typename Nu>
auto student_t_distribution(const Mean& mean, const Scale& scale, const Nu& nu)
{
    static_assert(all_is_real_v<Mean, Scale, Nu>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Mean, Scale, Nu>>;
    if (!(scale > Scale(0) && nu > Nu(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::student_t<P, Mean, Scale, Nu>(mean, scale, nu);
}
template<typename Nu>
auto student_t_distribution(const Nu& nu)
{
    static_assert(is_real_v<Nu>, WL_ERROR_REAL_TYPE_ARG);
    if (!(nu > Nu(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::student_t_nu<promote_integral_t<Nu>, Nu>(nu);
}
template<typename N, typename M>
auto f_ratio_distribution(const N& n, const M& m)
{
    static_assert(all_is_real_v<N, M>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<N, M>>;
    if (!(n > N(0) && m > M(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::f_ratio<P, N, M>(n, m);
}
template<typename Mean>
auto poisson_distribution(const Mean& mean)
{
    static_assert(is_real_v<Mean>, WL_ERROR_REAL_TYPE_ARG);
    if (!(mean > Mean(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::poisson<int64_t, Mean>(mean);
}
template<typename Lambda>
auto exponential_distribution(const Lambda& lambda)
{
    static_assert(is_real_v<Lambda>, WL_ERROR_REAL_TYPE_ARG);
    if (!(lambda > Lambda(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::exponential<
        promote_integral_t<Lambda>, Lambda>(lambda);
}
template<typename Ret = int64_t, typename P>
auto bernoulli_distribution(const P& p)
{
    static_assert(is_real_v<P>, WL_ERROR_REAL_TYPE_ARG);
    static_assert(is_arithmetic_v<Ret>, WL_ERROR_BAD_RETURN);
    if (!(P(0) <= p && p <= P(1)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::bernoulli<Ret, P>(p);
}
template<typename Alpha, typename Beta>
auto gamma_distribution(const Alpha& alpha, const Beta& beta)
{
    static_assert(all_is_real_v<Alpha, Beta>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Alpha, Beta>>;
    if (!(alpha > Alpha(0) && beta > Beta(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::gamma<P, Alpha, Beta>(alpha, beta);
}
template<typename Alpha, typename Beta>
auto weibull_distribution(const Alpha& alpha, const Beta& beta)
{
    static_assert(all_is_real_v<Alpha, Beta>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Alpha, Beta>>;
    if (!(alpha > Alpha(0) && beta > Beta(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::weibull<P, Alpha, Beta>(alpha, beta);
}
template<typename Alpha, typename Beta>
auto extreme_value_distribution(const Alpha& alpha, const Beta& beta)
{
    static_assert(all_is_real_v<Alpha, Beta>, WL_ERROR_REAL_TYPE_ARG);
    using P = promote_integral_t<common_type_t<Alpha, Beta>>;
    if (!(beta > Beta(0)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::extreme_value<P, Alpha, Beta>(alpha, beta);
}
inline auto extreme_value_distribution()
{
    return extreme_value_distribution(0., 1.);
}
template<typename P>
auto geometric_distribution(const P& p)
{
    static_assert(is_real_v<P>, WL_ERROR_REAL_TYPE_ARG);
    if (!(P(0) < p && p < P(1)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::geometric<int64_t, P>(p);
}
template<typename N, typename P>
auto binomial_distribution(const N& n, const P& p)
{
    static_assert(is_integral_v<N>, WL_ERROR_INTEGRAL_TYPE_ARG);
    static_assert(is_real_v<P>, WL_ERROR_REAL_TYPE_ARG);
    if (!(n >= N(0) && P(0) <= p && p <= P(1)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::binomial<N, N, P>(n, p);
}
template<typename N, typename P>
auto negative_binomial_distribution(const N& n, const P& p)
{
    static_assert(is_integral_v<N>, WL_ERROR_INTEGRAL_TYPE_ARG);
    static_assert(is_real_v<P>, WL_ERROR_REAL_TYPE_ARG);
    if (!(n > N(0) && P(0) < p && p <= P(1)))
        throw std::logic_error(WL_ERROR_DIST_PARAMETER_DOMAIN);
    return distribution::negative_binomial<N, N, P>(n, p);
}
template<typename Dist, typename... Dims>
auto _random_variate_impl(Dist dist, const Dims&... dims)
{
    using T = typename Dist::value_type;
    constexpr size_t R1 = Dist::rank;
    constexpr size_t R2 = sizeof...(dims);
    static_assert(R1 <= 1u, WL_ERROR_INTERNAL);
    if constexpr (R2 == 0u)
    {
        if constexpr (R1 == 0u)
        {
            T ret;
            dist.generate(&ret);
            return ret;
        }
        else
        {
            ndarray<T, 1u> ret(std::array<size_t, 1u>{dist.length()});
            dist.generate(ret.data());
            return ret;
        }
    }
    else
    {
        if constexpr (R1 == 0u)
        {
            wl::ndarray<T, R2> ret(utils::get_dims_array(dims...));
            auto iter = ret.data();
            WL_CHECK_ABORT_LOOP_BEGIN(ret.size())
                for (auto i = _loop_zero; i < _loop_size; ++i, ++iter)
                    dist.generate(iter);
            WL_CHECK_ABORT_LOOP_END()
            return ret;
        }
        else
        {
            const auto length = dist.length();
            wl::ndarray<T, R2 + 1u> ret(
                utils::get_dims_array(dims..., length));
            const auto outer_size = utils::size_of_dims<R2>(ret.dims().data());
            auto iter = ret.data();
            WL_CHECK_ABORT_LOOP_BEGIN(outer_size)
                for (auto i = _loop_zero; i < _loop_size; ++i, iter += length)
                    dist.generate(iter);
            WL_CHECK_ABORT_LOOP_END()
            return ret;
        }
    }
}
template<typename Dist, typename... Dims>
auto random_variate(Dist dist, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    return _random_variate_impl(dist, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Min, typename Max, typename... Dims>
auto random_integer(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(all_is_integral_v<Min, Max>, WL_ERROR_RANDOM_BOUNDS);
    using T = common_type_t<Min, Max>;
    auto dist = distribution::uniform<T, true>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Max, typename... Dims>
auto random_integer(const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<Max>, WL_ERROR_RANDOM_BOUNDS);
    return random_integer(Max{}, max, varg_tag{}, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Min, typename Max, typename... Dims>
auto random_real(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(all_is_real_v<Min, Max>, WL_ERROR_RANDOM_BOUNDS);
    using P = promote_integral_t<common_type_t<Min, Max>>;
    auto dist = distribution::uniform<P>(min, max);
    return _random_variate_impl(dist, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Max, typename... Dims>
auto random_real(const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Max>, WL_ERROR_RANDOM_BOUNDS);
    return random_real(Max{}, max, varg_tag{}, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Min, typename Max, typename... Dims>
auto random_complex(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_arithmetic_v<Min> && is_arithmetic_v<Max>,
        WL_ERROR_RANDOM_BOUNDS);
    using C = common_type_t<value_type_t<Min>, value_type_t<Max>>;
    using T = std::conditional_t<std::is_same_v<C, float>,
        complex<float>, complex<double>>;
    auto dist = distribution::uniform<T>(cast<T>(min), cast<T>(max));
    return _random_variate_impl(dist, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Max, typename... Dims>
auto random_complex(const Max& max, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(is_arithmetic_v<Max>, WL_ERROR_RANDOM_BOUNDS);
    return random_complex(Max{}, max, varg_tag{}, dims...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Random, typename X, size_t OuterRank>
auto _random_choice_batch_impl(Random&& random, const X& x,
    const std::array<size_t, OuterRank>& outer_dims)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<X>;
    const auto& valx = allows<view_category::Regular>(x);
    const auto x_iter = valx.begin();
    const auto outer_size = utils::size_of_dims(outer_dims);
    if constexpr (XR == 1u)
    {
        ndarray<XV, OuterRank> ret(outer_dims);
        auto ret_iter = ret.data();
        for (size_t i = 0; i < outer_size; ++i, ++ret_iter)
            *ret_iter = *(x_iter + random());
        return ret;
    }
    else
    {
        auto item_dims = utils::dims_take<2u, XR>(x.dims());
        auto item_size = utils::size_of_dims(item_dims);
        ndarray<XV, OuterRank + XR - 1u> ret(
            utils::dims_join(outer_dims, item_dims));
        auto base_iter = ret.data();
        for (size_t i = 0u; i < outer_size; ++i, base_iter += item_size)
            utils::restrict_copy_n(
                x_iter + item_size * random(),
                item_size, base_iter);
        return ret;
    }
}
template<typename Random, typename X>
auto _random_choice_single_impl(const Random& random, const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<X>;
    const auto& valx = allows<view_category::Regular>(x);
    const auto x_iter = valx.begin();
    if constexpr (XR == 1u)
    {
        return *(x_iter + random());
    }
    else
    {
        auto item_dims = utils::dims_take<2u, XR>(x.dims());
        auto item_size = utils::size_of_dims(item_dims);
        ndarray<XV, XR - 1u> ret(item_dims);
        utils::restrict_copy_n(
            x_iter + item_size * random(), item_size, ret.data());
        return ret;
    }
}
inline auto _random_choice_prepare_uniform(const size_t x_length)
{
    if (!(x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_ELEM_LENGTH);
    return [max = size_t(x_length - 1u)]
    {
        auto dist = std::uniform_int_distribution<size_t>(0u, max);
        return dist(global_random_engine);
    };
}
template<typename W>
auto _random_choice_prepare_binary(const W& w, const size_t x_length)
{
    static_assert(array_rank_v<W> == 1u && is_real_v<value_type_t<W>>,
        WL_ERROR_RANDOM_WEIGHTS_TYPE);
    if (!(x_length == w.dims()[0] && x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_WEIGHTS_LENGTH);
    const auto& valw = allows<view_category::Regular>(w);
    const auto w_size = valw.size();
    auto ret = ndarray<double, 1u>(valw.dims());
    auto w_iter = valw.begin();
    auto ret_iter = ret.data();
    double accumulate = 0.0;
    WL_IGNORE_DEPENDENCIES
    for (size_t i = 0; i < w_size; ++i, ++w_iter, ++ret_iter)
    {
        const auto weight = double(*w_iter);
        if (weight < 0.)
            throw std::logic_error(WL_ERROR_NEGATIVE_WEIGHT);
        accumulate += weight;
        *ret_iter = accumulate;
    }
    return [weights = std::move(ret), sum = accumulate]
    {
        auto w_begin = weights.begin();
        auto w_end = weights.end();
        auto dist = std::uniform_real_distribution<>(0., sum);
        return size_t(std::lower_bound(
            w_begin, w_end, dist(global_random_engine)) - w_begin);
    };
}
template<typename W>
auto _random_choice_prepare_walker74(const W& w, const size_t x_length)
{
    // Walker, A.J. (1974), Electronics Letters, 10(8), 127
    static_assert(array_rank_v<W> == 1u && is_real_v<value_type_t<W>>,
        WL_ERROR_RANDOM_WEIGHTS_TYPE);
    if (!(x_length == w.dims()[0] && x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_WEIGHTS_LENGTH);
    const size_t n = x_length;
    std::vector<double> p(n);
    w.copy_to(p.data());
    struct alias_t { double prob; uint64_t index; };
    std::vector<alias_t> alias_vec(n);
    auto small_base = new alias_t[n];
    auto large_base = new alias_t[n];
    auto* alias = alias_vec.data();
    auto* small = small_base;
    auto* large = large_base;
    auto push = [](auto*& ptr, alias_t elem) { *(ptr++) = elem; };
    auto pop = [](auto*& ptr) { return *(--ptr); };
    double normalize = 0.0;
    for (const auto& prob : p)
    {
        if (prob < 0.)
            throw std::logic_error(WL_ERROR_NEGATIVE_WEIGHT);
        normalize += prob;
    }
    const double factor = double(x_length) / normalize;
    for (uint64_t i = 0; i < n; ++i)
    {
        const double prob = p[i] * factor;
        push(prob <= 1. ? small : large, alias_t{prob, i});
    }
    while (small > small_base && large > large_base)
    {
        auto l = pop(small);
        auto g = pop(large);
        alias[l.index].prob = l.prob;
        alias[l.index].index = g.index;
        g.prob += l.prob - 1.;
        push(g.prob <= 1. ? small : large, g);
    }
    while (large > large_base)
    {
        alias[pop(large).index].prob = 1.;
    }
    while (small > small_base)
    {
        alias[pop(small).index].prob = 1.;
    }
    delete[] small_base;
    delete[] large_base;
    return[alias_vec = std::move(alias_vec), max = double(n)]
    {
        auto dist = std::uniform_real_distribution<>(0.0, max);
        double rand = dist(global_random_engine);
        double index = std::floor(rand);
        const auto& a = alias_vec[size_t(index)];
        return (rand - index <= a.prob) ? size_t(index) : a.index;
    };
}
template<typename X>
auto random_choice(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return _random_choice_single_impl(
        _random_choice_prepare_uniform(x.dims()[0]), x);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename... Dims>
auto random_choice(const X& x, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return _random_choice_batch_impl(
        _random_choice_prepare_uniform(x.dims()[0]),
        x, utils::get_dims_array(dims...));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename W, typename X>
auto random_choice(const W& w, const X& x, varg_tag)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return _random_choice_single_impl(
        _random_choice_prepare_binary(w, x.dims()[0]), x);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename W, typename X, typename... Dims>
auto random_choice(const W& w, const X& x, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    const auto outer_dims = utils::get_dims_array(dims...);
    const auto outer_size = utils::size_of_dims(outer_dims);
    // automatically select between binary and walker74
    if ((outer_size >= 50u) && (outer_size * 5u >= x_length))
        return _random_choice_batch_impl(
            _random_choice_prepare_walker74(w, x_length), x, outer_dims);
    else
        return _random_choice_batch_impl(
            _random_choice_prepare_binary(w, x_length), x, outer_dims);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
inline auto _random_sample_prepare_uniform(const size_t x_length)
{
    if (!(x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_ELEM_LENGTH);
    ndarray<size_t, 1u> idx_vec(std::array<size_t, 1u>{x_length});
    auto* const idx = idx_vec.data();
    for (size_t i = 0; i < x_length; ++i)
        idx[i] = i;
    return [idx_vec = std::move(idx_vec), remain = x_length]() mutable
    {
        if (remain == 0u)
            throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
        else if (remain == 1u)
            return idx_vec.data()[0];
        auto* const idx = idx_vec.data();
        const auto this_idx = std::uniform_int_distribution<size_t>(
            0u, remain - 1u)(global_random_engine);
        const auto picked_idx = idx[this_idx];
        if (this_idx + 1u < remain) // did not pick the last element
            idx[this_idx] = idx[remain - 1u];
        --remain;
        return picked_idx;
    };
}
inline auto _random_sample_prepare_short_uniform(const size_t x_length)
{
    if (!(x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_ELEM_LENGTH);
    return [idx_vec = ndarray<size_t, 1u>{}, x_length]() mutable
    {
        const auto n_taken = idx_vec.size();
        auto* const idx_begin = idx_vec.data();
        auto* const idx_end = idx_begin + n_taken;
        if (x_length == n_taken)
            throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
        if (n_taken >= 2u)
        { // move the last element to its place
            auto* ins = std::lower_bound(
                idx_begin, idx_end - 1, idx_end[-1]);
            std::rotate(ins, idx_end - 1, idx_end);
        }
        auto this_idx = std::uniform_int_distribution<size_t>(
            0u, x_length - n_taken - 1u)(global_random_engine);
        idx_vec.for_each([&](const auto& taken)
            {
                if (this_idx >= taken)
                    ++this_idx;
            });
        idx_vec.append(size_t(this_idx));
        return this_idx;
    };
}
template<typename W>
auto _random_sample_prepare_binary(const W& w, const size_t x_length)
{
    static_assert(array_rank_v<W> == 1u && is_real_v<value_type_t<W>>,
        WL_ERROR_RANDOM_WEIGHTS_TYPE);
    if (!(x_length == w.dims()[0] && x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_WEIGHTS_LENGTH);
    const size_t n = x_length;
    if (n <= 3u)
        throw std::logic_error(WL_ERROR_INTERNAL);
    ndarray<double, 1u> probs_vec(std::array<size_t, 1>{n});
    ndarray<uint64_t, 1u> uint_probs_vec(std::array<size_t, 1>{n});
    auto* const probs = probs_vec.data();
    auto* const uint_probs = uint_probs_vec.data();
    w.copy_to(probs);
    double sum_prob = 0.0;
    for (const auto& prob : probs_vec)
    {
        if (prob < 0.)
            throw std::logic_error(WL_ERROR_NEGATIVE_WEIGHT);
        sum_prob += prob;
    }
    constexpr double target_sum_prob = 1.0e+19; // ~2^63
    const double factor = target_sum_prob / sum_prob;
    WL_IGNORE_DEPENDENCIES
    for (size_t i = 0; i < n; ++i)
    {
        const auto prob = uint64_t(std::floor(probs[i] * factor + 0.5));
        // each element should have a probability of at least ~2^-63
        uint_probs[i] = (prob > 0u) ? prob : uint64_t(1);
    }
    const auto lzcnt = utils::lzcnt_u64(uint64_t(n - 1u));
    const auto max_jump = size_t(1) << (63u - lzcnt);
    // fold the ragged part
    for (size_t i = max_jump; i < n; ++i)
        uint_probs[i - max_jump] += uint_probs[i];
    // fold the regular parts
    for (auto jump = max_jump >> 1; jump > 0u; jump >>= 1)
    {
        for (size_t i = 0; i < jump; ++i)
            uint_probs[i] += uint_probs[jump + i];
    }
    
    return [uint_probs_vec = std::move(uint_probs_vec), n, max_jump]() mutable
    {
        auto* uint_probs = uint_probs_vec.data();
        if (uint_probs[0] == 0u)
            throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
        auto rand = std::uniform_int_distribution<uint64_t>(
            0u, uint_probs[0] - 1u)(global_random_engine);
        // pick the index
        auto index = uint64_t(0);
        auto weight = uint_probs[0]; // to be removed from uint_probs
        for (size_t jump = 1u; jump <= max_jump; jump <<= 1)
        {
            if (index + jump >= n)
                break;
            else
            {
                const auto proposed = uint_probs[index + jump];
                if (rand >= proposed)
                {
                    rand -= proposed;
                    weight -= proposed;
                }
                else
                {
                    index += jump;
                    weight = proposed;
                }
            }
        }
        const auto ret_index = size_t(index);
        // remove weight from the uint_probs
        uint_probs[index] -= weight;
        for (size_t jump = max_jump; true; jump >>= 1)
        {
            if (jump <= index)
            {
                index -= jump;
                uint_probs[index] -= weight;
            }
            if (index == 0u)
                break;
        }
        return ret_index;
    };
}
template<typename W>
auto _random_sample_prepare_linear(const W& w, const size_t x_length)
{
    static_assert(array_rank_v<W> == 1u && is_real_v<value_type_t<W>>,
        WL_ERROR_RANDOM_WEIGHTS_TYPE);
    if (!(x_length == w.dims()[0] && x_length >= 1u))
        throw std::logic_error(WL_ERROR_RANDOM_WEIGHTS_LENGTH);
    const size_t n = x_length;
    if (n <= 1u)
        throw std::logic_error(WL_ERROR_INTERNAL);
    ndarray<double, 1u> pmf_vec(std::array<size_t, 1u>{n});
    ndarray<double, 1u> cmf_vec(std::array<size_t, 1u>{n});
    auto* const pmf = pmf_vec.data();
    auto* const cmf = cmf_vec.data();
    w.copy_to(pmf);
    if (std::any_of(pmf, pmf + n, [](auto p) { return p < 0.; }))
        throw std::logic_error(WL_ERROR_NEGATIVE_WEIGHT);
    std::partial_sum(pmf, pmf + n, cmf);
    return [cmf_vec = std::move(cmf_vec), n, last_index = n]() mutable
    {
        auto* const cmf = cmf_vec.data();
        if (last_index < n)
        { // if not the last element, update pmf and cmf
            const auto diff = last_index == 0u ? cmf[0u] :
                (cmf[last_index] - cmf[last_index - 1u]);
            for (auto i = last_index; i < n; ++i)
                cmf[i] -= diff;
        }
        if (cmf[n - 1u] <= 0.)
            throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_ZERO_WEIGHTS);
        const auto rand = std::uniform_real_distribution<>(
            0., cmf[n - 1u])(global_random_engine);
        size_t index = size_t(std::lower_bound(cmf, cmf + n, rand) - cmf);
        last_index = index;
        return index;
    };
}
template<typename W, typename X, typename Size>
auto random_sample(const W& w, const X& x, varg_tag, const Size& size)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    const auto outer_dims = utils::get_dims_array(size);
    const auto outer_size = utils::size_of_dims(outer_dims);
    if (size > x_length)
        throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
    // automatically select between binary and linear
    if (outer_size > 5u && x_length > 35u)
        return _random_choice_batch_impl(
            _random_sample_prepare_binary(w, x_length), x, outer_dims);
    else
        return _random_choice_batch_impl(
            _random_sample_prepare_linear(w, x_length), x, outer_dims);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename W, typename X>
auto random_sample(const W& w, const X& x, varg_tag)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    return random_sample(w, x, x_length);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Size>
auto random_sample(const X& x, const Size& size)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    const auto outer_dims = utils::get_dims_array(size);
    const auto outer_size = utils::size_of_dims(outer_dims);
    if (size > x_length)
        throw std::logic_error(WL_ERROR_RANDOM_SAMPLE_NO_ELEM);
    // automatically select between two methods
    if (x_length == 1u && size == 1u)
        return val(x);
    else if (size <= 2u || 10 * size * size <= x_length)
        return _random_choice_batch_impl(
            _random_sample_prepare_short_uniform(x_length), x, outer_dims);
    else
        return _random_choice_batch_impl(
            _random_sample_prepare_uniform(x_length), x, outer_dims);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto random_sample(const X& x)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto x_length = x.dims()[0];
    return random_sample(x, x_length);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
template<typename Dst, typename Src>
auto set(Dst&& dst, Src&& src) -> decltype(auto)
{
    WL_TRY_BEGIN()
    using DstType = remove_cvref_t<Dst>;
    using SrcType = remove_cvref_t<Src>;
    constexpr auto dst_rank = array_rank_v<DstType>;
    constexpr auto src_rank = array_rank_v<SrcType>;
    using DstValue = value_type_t<DstType>;
    using SrcValue = value_type_t<SrcType>;
    static_assert(std::is_lvalue_reference_v<Dst&&> ||
        is_array_view_v<DstType>, WL_ERROR_MODIFY_TARGET);
    if constexpr (src_rank == 0u)
    {
        if constexpr (dst_rank == 0u)
        { // scalar -> scalar
            static_assert(is_convertible_v<SrcType, DstType>,
                WL_ERROR_ASSIGN_TYPE);
            dst = DstType(src);
            return std::forward<decltype(src)>(src);
        }
        else
        { // scalar -> ndarray / array_view
            static_assert(is_convertible_v<SrcType, DstValue>,
                WL_ERROR_ASSIGN_TYPE);
            dst.copy_from(make_scalar_view_iterator(DstValue(src)));
            return std::forward<decltype(src)>(src);
        }
    }
    else
    {
        static_assert(dst_rank == src_rank, WL_ERROR_ASSIGN_RANK);
        static_assert(is_convertible_v<SrcValue, DstValue>,
            WL_ERROR_ASSIGN_TYPE);
        if constexpr (is_array_v<DstType>)
        {
            if constexpr (std::is_same_v<SrcValue, DstValue>)
                dst = std::forward<decltype(src)>(src).to_array();
            else
            {
                dst.uninitialized_resize(src.dims(), src.size());
                src.copy_to(dst.data());
            }
            return dst; // returns an l-value reference
        }
        else // dst is an array view
        {
            if (!utils::check_dims(src.dims(), dst.dims()))
                throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
            if (!has_aliasing(src, dst))
            {
                if constexpr (SrcType::category != view_category::General)
                    dst.copy_from(src.begin());
                else if constexpr (DstType::category != view_category::General)
                    src.copy_to(dst.begin());
                else // general_view -> general_view
                    indirect_view_copy(dst, src);
                return std::forward<decltype(src)>(src);
            }
            else // has aliasing
            {
                indirect_view_copy(dst, src);
                return std::forward<decltype(src)>(src);
            }
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Iter, size_t R>
void _copy_list_array_elements(Iter&, const std::array<size_t, R>&)
{
}
template<typename Iter, size_t R, typename First, typename... Rest>
void _copy_list_array_elements(Iter& ret_iter,
    const std::array<size_t, R>& dims, First&& first, Rest&&... rest)
{
    using FirstType = remove_cvref_t<First>;
    using T = value_type_t<remove_cvref_t<decltype(*ret_iter)>>;
    if constexpr (is_argument_pack_v<FirstType>)
    {
        using ItemType = value_type_t<FirstType>;
        static_assert(array_rank_v<ItemType> == R, WL_ERROR_LIST_ELEM_RANK);
        using ValueType = value_type_t<ItemType>;
        static_assert(is_convertible_v<ValueType, T>, WL_ERROR_LIST_ELEM_TYPE);
        const auto size = first.size();
        for (size_t i = 0u; i < size; ++i, ++ret_iter)
        {
            const auto& item = first.get(i);
            if (!utils::check_dims(item.dims(), dims))
                throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
            item.copy_to(ret_iter.begin());
        }
    }
    else
    {
        static_assert(array_rank_v<FirstType> == R, WL_ERROR_LIST_ELEM_RANK);
        using ValueType = value_type_t<FirstType>;
        static_assert(is_convertible_v<ValueType, T>, WL_ERROR_LIST_ELEM_TYPE);
        if (!utils::check_dims(first.dims(), dims))
            throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
        first.copy_to(ret_iter.begin());
        ++ret_iter;
    }
    _copy_list_array_elements(ret_iter, dims,
        std::forward<decltype(rest)>(rest)...);
}
template<typename T>
void _copy_list_scalar_elements(T*&)
{
}
template<typename T, typename First, typename... Rest>
void _copy_list_scalar_elements(T*& ret_iter, First&& first, Rest&&... rest)
{
    using FirstType = remove_cvref_t<First>;
    if constexpr (is_argument_pack_v<FirstType>)
    {
        using ItemType = value_type_t<FirstType>;
        static_assert(is_convertible_v<ItemType, T>, WL_ERROR_LIST_ELEM_TYPE);
        WL_CHECK_ABORT_LOOP_BEGIN(first.size())
            for (auto i = _loop_begin; i < _loop_end; ++i, ++ret_iter)
                *ret_iter = cast<T>(first.get(i, dim_checked{}));
        WL_CHECK_ABORT_LOOP_END()
    }
    else
    {
        static_assert(is_convertible_v<FirstType, T>, WL_ERROR_LIST_ELEM_TYPE);
        *ret_iter = cast<T>(std::forward<decltype(first)>(first));
        ++ret_iter;
    }
    _copy_list_scalar_elements(ret_iter,
        std::forward<decltype(rest)>(rest)...);
}
template<typename Any>
auto _list_length_by_args_impl(const Any& any)
{
    return size_t(1);
}
template<typename Iter, bool HasStride>
auto _list_length_by_args_impl(const argument_pack<Iter, HasStride>& args)
{
    return args.size();
}
template<typename... Elems>
auto _list_length_by_args(const Elems&... elems)
{
    return (_list_length_by_args_impl(elems) + ...);
};
template<typename First, typename... Rest>
auto list(First&& first, Rest&&... rest)
{
    WL_TRY_BEGIN()
    using FirstType = remove_cvref_t<First>;
    if constexpr (is_argument_pack_v<FirstType>)
    {
        if (first.size() == 0u)
        {
            if constexpr (sizeof...(rest) > 0u)
                return list(std::forward<decltype(rest)>(rest)...);
            else
            {
                using ItemType = value_type_t<FirstType>;
                constexpr auto rank = array_rank_v<ItemType> + 1u;
                using ValueType = std::conditional_t<rank == 0u,
                    std::conditional_t<std::is_same_v<ItemType, string_view>,
                        string, ItemType>,
                    value_type_t<ItemType>>;
                return ndarray<ValueType, rank>{};
            }
        }
        else
            return list(first.get(0), first.get_pack(1),
                std::forward<decltype(rest)>(rest)...);
    }
    else
    {
        constexpr auto first_rank = array_rank_v<FirstType>;
        const auto dim0 = _list_length_by_args(first, rest...);
        if constexpr (first_rank == 0)
        {
            using ItemType = std::conditional_t<
                std::is_same_v<FirstType, string_view>, string, FirstType>;
            ndarray<ItemType, 1u> ret(std::array<size_t, 1u>{dim0});
            auto ret_iter = ret.data();
            _copy_list_scalar_elements(ret_iter,
                std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...);
            return ret;
        }
        else
        {
            constexpr auto rank = first_rank + 1u;
            const auto dims = utils::dims_join(
                std::array<size_t, 1u>{dim0}, first.dims());
            using FirstValueType = value_type_t<FirstType>;
            using ItemType = std::conditional_t<
                std::is_same_v<FirstValueType, string_view>,
                string, FirstValueType>;
            ndarray<ItemType, rank> ret(dims);
            auto ret_iter = ret.template view_begin<1u>();
            _copy_list_array_elements(ret_iter, first.dims(),
                std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...);
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename... Args>
inline auto list_type::operator()(Args&&... args) const
{
    return list(std::forward<decltype(args)>(args)...);
}
template<typename T, typename... Dims>
auto constant_array(const T& val, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    constexpr auto val_rank = array_rank_v<T>;
    constexpr auto rep_rank = sizeof...(dims);
    constexpr auto all_rank = val_rank + rep_rank;
    if constexpr (val_rank > 0)
    {
        using ValueType = value_type_t<T>;
        const auto ret_dims = utils::dims_join(
            utils::get_dims_array(dims...), val.dims());
        ndarray<ValueType, all_rank> ret(ret_dims);
        const auto& valx = allows<view_category::Simple>(val);
        const size_t valx_size = valx.size();
        const auto* valx_iter = valx.data();
        auto* ret_iter = ret.data();
        auto* ret_end = ret_iter + ret.size();
        for (; ret_iter != ret_end; ret_iter += valx_size)
            valx.copy_to(ret_iter);
        return ret;
    }
    else
    {
        ndarray<T, all_rank> ret(
            std::array<int64_t, all_rank>{int64_t(dims)...}, val);
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<size_t>
using make_all_type = all_type;
template<typename Array, typename... Indexers>
auto _part_impl3(Array&& a, Indexers&&... indexers) -> decltype(auto)
{
    using ArrayType = remove_cvref_t<Array>;
    using ValueType = typename ArrayType::value_type;
    constexpr auto is_const = array_is_const_v<Array&&>;
    static_assert(sizeof...(Indexers) <= array_rank_v<ArrayType>,
        WL_ERROR_INTERNAL);
    using return_type = typename view_detail::view_type<Indexers...>::
        template return_type<ValueType, is_const, Indexers...>;
    if constexpr (is_array_view_v<return_type>)
    {
        auto view = return_type(a.identifier(), a.data(), a.dims(),
            std::forward<decltype(indexers)>(indexers)...);
        if constexpr (is_movable_v<Array&&>)
            return std::move(view).to_array();
        else
            return view;
    }
    else
    {
        auto& data_ref = a.data()[
            utils::linear_position(a.dims(), indexers.offset()...)];
        if constexpr (is_movable_v<Array&&>)
            return static_cast<remove_cvref_t<decltype(data_ref)>>(data_ref);
        else
            return data_ref;
    }
}
template<typename Array, size_t... Is, typename... Specs>
auto _part_impl2(Array&& a, std::index_sequence<Is...>,
    Specs&&... specs) -> decltype(auto)
{
    return _part_impl3(std::forward<decltype(a)>(a),
        make_indexer(std::forward<decltype(specs)>(specs), a.dims_[Is])...);
}
template<size_t R, typename Array, size_t... Is, typename... Specs>
auto _part_impl1(Array&& a, std::index_sequence<Is...>,
    Specs&&... specs) -> decltype(auto)
{
    return _part_impl2(std::forward<decltype(a)>(a),
        std::make_index_sequence<R>{},
        std::forward<decltype(specs)>(specs)...,
        make_all_type<Is>{}...);
}
template<typename Array, typename... Specs>
auto part(Array&& a, Specs&&... specs) -> decltype(auto)
{
    WL_TRY_BEGIN()
    using ArrayType = remove_cvref_t<Array>;
    constexpr auto R = array_rank_v<ArrayType>;
    static_assert(sizeof...(Specs) <= R, WL_ERROR_PART_DEPTH);
    if constexpr (R == 0u)
        return std::forward<decltype(a)>(a);
    else if constexpr (
        ArrayType::category == view_category::Array ||
        ArrayType::category == view_category::Simple)
    {
        return _part_impl1<R>(std::forward<decltype(a)>(a),
            std::make_index_sequence<R - sizeof...(Specs)>{},
            std::forward<decltype(specs)>(specs)...);
    }
    else
        return part(a.to_array(), std::forward<decltype(specs)>(specs)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Array>
auto first(Array&& a)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<remove_cvref_t<Array>> >= 1u,
        WL_ERROR_REQUIRE_ARRAY);
    return part(a, cidx(0));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Array>
auto last(Array&& a)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<remove_cvref_t<Array>> >= 1u,
        WL_ERROR_REQUIRE_ARRAY);
    return part(a, int64_t(-1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto most(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<XT>;
    const auto length = x.dims()[0];
    if (length == 0u)
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);
    else if (length == 1u)
        return ndarray<XV, XR>{};
    else
        return val(part(x, make_span(const_all, int64_t(-2))));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto rest(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<XT>;
    const auto length = x.dims()[0];
    if (length == 0u)
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);
    else if (length == 1u)
        return ndarray<XV, XR>{};
    else
        return val(part(x, make_span(int64_t(2), const_all)));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Begin, typename End, typename Step>
auto range(Begin begin, End end, Step step)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Begin> && is_real_v<End> && is_real_v<Step>,
        WL_ERROR_ITERATOR_TYPE);
    using T = common_type_t<Begin, Step>;
    if constexpr (is_integral_v<T>)
    {
        if (step != Step(0))
        {
            auto diff = ptrdiff_t(wl::integer_part(end - begin));
            if ((step > Step(0) && diff < 0) || (step < Step(0) && diff > 0))
                return ndarray<T, 1u>(std::array<size_t, 1>{0u});
            else
            {
                size_t length = size_t(diff / ptrdiff_t(step)) + 1u;
                ndarray<T, 1u> ret(std::array<size_t, 1>{length});
                auto* ret_iter = ret.data();
                WL_CHECK_ABORT_LOOP_BEGIN(length)
                    for (auto i = _loop_zero; i < _loop_size;
                        ++i, begin += step, ++ret_iter)
                    {
                        *ret_iter = begin;
                    }
                WL_CHECK_ABORT_LOOP_END()
                return ret;
            }
        }
        else // step = 0
            throw std::logic_error(WL_ERROR_ITERATOR_ZERO_STEP);
    }
    else
    {
        if (step != Step(0))
        {
            auto diff = T(end - begin);
            if ((step > Step(0) && diff < 0) || (step < Step(0) && diff > 0))
                return ndarray<T, 1u>(std::array<size_t, 1>{0u});
            else
            {
                size_t length = wl::integer_part(diff / step) + 1u;
                auto remain = T(begin + (length - 1u) * step) - T(end);
                if (step * remain > T(0))
                    --length;
                ndarray<T, 1u> ret(std::array<size_t, 1>{length});
                auto* ret_iter = ret.data();
                WL_CHECK_ABORT_LOOP_BEGIN(length)
                    for (auto i = _loop_begin; i < _loop_end; ++i, ++ret_iter)
                        *ret_iter = T(begin + i * step);
                WL_CHECK_ABORT_LOOP_END()
                return ret;
            }
        }
        else // step = 0
            throw std::logic_error(WL_ERROR_ITERATOR_ZERO_STEP);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Begin, typename End>
auto range(Begin begin, End end)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<Begin> && is_real_v<End>, WL_ERROR_ITERATOR_TYPE);
    return range(begin, end, int8_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename End>
auto range(End end)
{
    WL_TRY_BEGIN()
    static_assert(is_real_v<End>, WL_ERROR_ITERATOR_TYPE);
    return range(End(1), end, int8_t(1));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Array, int64_t I1, int64_t I2>
auto total(const Array& a, const_int<I1>, const_int<I2>)
{
    WL_TRY_BEGIN()
    constexpr auto rank = array_rank_v<Array>;
    static_assert(rank >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<Array>;
    constexpr int64_t L1 = I1 >= 0 ? I1 : I1 + int64_t(rank) + 1;
    constexpr int64_t L2 = I2 >= 0 ? I2 : I2 + int64_t(rank) + 1;
    static_assert(1 <= L1 && L1 <= L2 && L2 <= int64_t(rank),
        WL_ERROR_BAD_LEVEL);
    if constexpr (Array::category == view_category::General)
        return total(a.to_array(), const_int<I1>{}, const_int<I2>{});
    else
    {
        WL_THROW_IF_ABORT()
        if constexpr (L1 == 1)
        {
            if constexpr (L2 == rank)
            {
                const auto inter_size = a.size();
                auto a_iter = a.begin();
                auto ret = XV{};
                for (size_t j = 0; j < inter_size; ++j, ++a_iter)
                    ret += *a_iter;
                return ret;
            }
            else
            {
                auto ret_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                ndarray<XV, rank - L2> ret(ret_dims, XV{});
                const auto inter_size = utils::size_of_dims(
                    utils::dims_take<L1, L2>(a.dims()));
                const auto inner_size = ret.size();
                auto a_iter = a.begin();
                auto ret_iter = ret.begin();
                for (size_t j = 0; j < inter_size; ++j)
                    for (size_t k = 0; k < inner_size; ++k, ++a_iter)
                        ret_iter[k] += *a_iter;
                return ret;
            }
        }
        else // L1 > 1
        {
            if constexpr (L2 == rank)
            {
                auto ret_dims = utils::dims_take<1, L1 - 1>(a.dims());
                ndarray<XV, L1 - 1> ret(ret_dims);
                const auto outer_size = ret.size();
                const auto inter_size = utils::size_of_dims(
                    utils::dims_take<L1, L2>(a.dims()));
                auto a_iter = a.begin();
                auto ret_iter = ret.begin();
                for (size_t i = 0; i < outer_size; ++i, ++ret_iter)
                {
                    auto sum = XV{};
                    for (size_t j = 0; j < inter_size; ++j, ++a_iter)
                        sum += *a_iter;
                    *ret_iter = sum;
                }
                return ret;
            }
            else
            {
                auto outer_dims = utils::dims_take<1, L1 - 1>(a.dims());
                auto inter_dims = utils::dims_take<L1, L2>(a.dims());
                auto inner_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                auto ret_dims = utils::dims_join(outer_dims, inner_dims);
                ndarray<XV, rank - (L2 - L1 + 1)> ret(ret_dims, XV{});
                auto a_iter = a.begin();
                auto ret_iter = ret.begin();
                const auto outer_size = utils::size_of_dims(outer_dims);
                const auto inter_size = utils::size_of_dims(inter_dims);
                const auto inner_size = utils::size_of_dims(inner_dims);
                for (size_t i = 0; i < outer_size; ++i, ret_iter += inner_size)
                    for (size_t j = 0; j < inter_size; ++j)
                        for (size_t k = 0; k < inner_size; ++k, ++a_iter)
                            ret_iter[k] += *a_iter;
                return ret;
            }
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Array, int64_t I>
auto total(const Array& a, const_int<I>)
{
    WL_TRY_BEGIN()
    return total(a, const_int<1>{}, const_int<I>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Array>
auto total(const Array& a)
{
    WL_TRY_BEGIN()
    if constexpr (array_rank_v<Array> == 0u)
        return a;
    else
    return total(a, const_int<1>{}, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Array>
auto mean(const Array& a)
{
    WL_TRY_BEGIN()
    return divide(total(a), a.template dimension<1>());
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename Array>
auto dimensions(const Array& a)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<Ret>, WL_ERROR_BAD_RETURN);
    constexpr auto rank = array_rank_v<Array>;
    if constexpr (rank == 0u)
        return ndarray<Ret, 1u>{};
    else
        return ndarray<Ret, 1u>(std::array<size_t, 1u>{rank},
            a.dims_ptr(), a.dims_ptr() + rank);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Array>
auto length(const Array& a)
{
    constexpr auto rank = array_rank_v<Array>;
    if constexpr (rank == 0u)
        return int64_t(0);
    else
        return int64_t(a.dims()[0]);
}
template<typename Any>
auto array_depth(const Any& any)
{
    return int64_t(array_rank_v<Any>);
}
template<typename T, size_t R>
void _reverse_inplace(ndarray<T, R>& a,
    size_t outer_size, size_t inter_size, size_t inner_size)
{
    WL_THROW_IF_ABORT()
    if (inner_size == 1u)
    {
        auto base = a.data();
        for (size_t i = 0u; i < outer_size; ++i, base += inter_size)
        {
            auto forward = base;
            auto reverse = base + inter_size;
            for (; forward != reverse && forward != --reverse; ++forward)
                std::iter_swap(forward, reverse);
        }
    }
    else
    {
        auto base = a.data();
        auto outer_step = inter_size * inner_size;
        for (size_t i = 0u; i < outer_size; ++i, base += outer_step)
        {
            size_t forward = 0u;
            size_t reverse = inter_size;
            for (; forward != reverse && forward != --reverse; ++forward)
                std::swap_ranges(
                    base + forward * inner_size,
                    base + forward * inner_size + inner_size,
                    base + reverse * inner_size);
        }
    }
}
template<typename X, int64_t I>
auto reverse(X&& x, const_int<I>)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto rank = array_rank_v<XT>;
    static_assert(rank >= 1, WL_ERROR_REQUIRE_ARRAY);
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(rank) + 1;
    static_assert(1 <= Level && Level <= int64_t(rank), WL_ERROR_BAD_LEVEL);
    size_t outer_size = utils::size_of_dims(
        utils::dims_take<1u, Level - 1u>(x.dims()));
    size_t inter_size = utils::size_of_dims(
        utils::dims_take<Level, Level>(x.dims()));
    size_t inner_size = utils::size_of_dims(
        utils::dims_take<Level + 1u, rank>(x.dims()));
    if constexpr (is_movable_v<X&&>)
    {
        _reverse_inplace(x, outer_size, inter_size, inner_size);
        return x;
    }
    else
    {
        auto x2 = x.to_array();
        _reverse_inplace(x2, outer_size, inter_size, inner_size);
        return x2;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto reverse(X&& x)
{
    WL_TRY_BEGIN()
    return reverse(std::forward<decltype(x)>(x), const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Pad, typename... Dims>
auto array_reshape(X&& x, const Pad& padding, varg_tag, const Dims&... dims)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    static_assert(array_rank_v<XT> >= 1, WL_ERROR_REQUIRE_ARRAY);
    constexpr auto rank = sizeof...(dims);
    ndarray<XV, rank> ret(utils::get_dims_array(dims...));
    WL_THROW_IF_ABORT()
    const size_t x_size = x.size();
    const size_t ret_size = ret.size();
    if (x_size <= ret_size)
    {
        x.copy_to(ret.begin());
        if constexpr (!std::is_same_v<Pad, void_type>)
        {
            static_assert(is_convertible_v<Pad, XV>,
                WL_ERROR_ARRAY_RESHAPE_PAD_TYPE);
            std::fill(ret.begin() + x_size, ret.end(), cast<XV>(padding));
        }
        else if (ret.is_static()) // only std::array needs initialization
            std::fill(ret.begin() + x_size, ret.end(), XV{});
    }
    else
    {
        auto ret_iter = ret.begin();
        auto ret_end = ret.end();
        x.for_each([&](const auto& src)
            { *ret_iter++ = src; return (ret_iter == ret_end); });
    }
    return ret;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename... Dims>
auto array_reshape(X&& x, varg_tag, const Dims&... dims)
{
    static_assert(array_rank_v<remove_cvref_t<X>> >= 1,
        WL_ERROR_REQUIRE_ARRAY);
    return array_reshape(std::forward<decltype(x)>(x),
        void_type{}, varg_tag{}, dims...);
}
template<size_t R, size_t... Is>
struct _is_valid_transpose
{
    static constexpr auto between = ((1u <= Is && Is <= R) && ...);
    static constexpr auto mask = ((uint64_t(1) << (Is - 1u)) | ...);
    static constexpr auto value = between && ((mask & (mask + 1u)) == 0);
};
template<size_t... Is>
struct _transpose_max_level;
template<size_t I1, size_t I2, size_t... Is>
struct _transpose_max_level<I1, I2, Is...> :
    _transpose_max_level<(I1 > I2 ? I1 : I2), Is...> {};
template<size_t I>
struct _transpose_max_level<I> : std::integral_constant<size_t, I> {};
template<typename T, size_t... Is>
struct _padded_transpose_levels_impl;
template<size_t... Is, size_t... Pads>
struct _padded_transpose_levels_impl<std::index_sequence<Pads...>, Is...>
{
    static constexpr auto max_level = _transpose_max_level<Is...>::value;
    using type = std::index_sequence<Is..., (Pads + max_level + 1u)...>;
};
template<size_t R, size_t... Is>
struct _padded_transpose_levels :
    _padded_transpose_levels_impl<
    std::make_index_sequence<R - sizeof...(Is)>, Is...> {};
template<size_t L, typename T>
void _transpose_fill(const T* src, T*& dst,
    const size_t* dims, const size_t* strides)
{
    const size_t dim = *dims;
    const size_t stride = *strides;
    if constexpr (L > 1u)
    {
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0u; i < dim; ++i, src += stride)
            _transpose_fill<L - 1u>(src, dst, dims + 1, strides + 1);
    }
    else if (stride == 1u)
    {
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0u; i < dim; ++i, ++src, ++dst)
            *dst = *src;
    }
    else
    {
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0u; i < dim; ++i, src += stride, ++dst)
            *dst = *src;
    }
}
template<typename T, size_t R, typename Output, size_t... Is, size_t... Cs>
auto _transpose_impl(const ndarray<T, R>& a, Output ptr,
    std::index_sequence<Is...>, std::index_sequence<Cs...>)
{
    constexpr auto RetRank = _transpose_max_level<size_t(Is)...>::value;
    auto a_dims = a.dims().data();
    std::array<size_t, R> strides;
    std::array<size_t, RetRank> ret_dims{};
    std::array<size_t, RetRank> ret_strides{};
    strides[R - 1u] = 1u;
    [[maybe_unused]] auto _1 = ((Cs > 0 ? (strides[R - Cs - 1u] =
        strides[R - Cs] * a_dims[R - Cs]) : size_t()), ...);
    [[maybe_unused]] auto _2 = ((ret_dims[Is - 1] == 0u ?
        (ret_dims[Is - 1] = a_dims[Cs], ret_strides[Is - 1] += strides[Cs]) :
        ret_dims[Is - 1] == a_dims[Cs] ? ret_strides[Is - 1] += strides[Cs] :
        throw std::logic_error(WL_ERROR_TRANSPOSE_COLLAPSE)), ...);
    WL_THROW_IF_ABORT()
    if constexpr (std::is_pointer_v<Output>)
    {
        auto dst_ptr = ptr;
        _transpose_fill<RetRank>(a.data(), dst_ptr,
            ret_dims.data(), ret_strides.data());
    }
    else
    {
        ndarray<T, RetRank> ret(ret_dims);
        auto dst_ptr = ret.data();
        _transpose_fill<RetRank>(a.data(), dst_ptr,
            ret_dims.data(), ret_strides.data());
        return ret;
    }
}
template<typename X, int64_t... Is>
auto transpose(const X& x, const_int<Is>...)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto NL = sizeof...(Is);
    static_assert(1 <= NL && NL <= XR, WL_ERROR_BAD_LEVEL);
    static_assert(_is_valid_transpose<XR, size_t(Is)...>::value,
        WL_ERROR_BAD_LEVEL);
    return _transpose_impl(val(x), void_type{},
        typename _padded_transpose_levels<XR, Is...>::type{},
        std::make_index_sequence<XR>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto transpose(const X& x)
{
    WL_TRY_BEGIN()
    return transpose(x, const_int<2>{}, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto conjugate_transpose(const X& x)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 2u, WL_ERROR_REQUIRE_ARRAY_RANK"two or higher.");
    using XV = value_type_t<X>;
    if constexpr (is_real_v<XV>)
        return transpose(x, const_int<2>{}, const_int<1>{});
    else
    {
        WL_THROW_IF_ABORT()
        const auto& valx = allows<view_category::Regular>(x);
        auto x_iter = valx.begin();
        if constexpr (XR == 2u)
        {
            const size_t dim0 = x.dims()[0];
            const size_t dim1 = x.dims()[1];
            ndarray<XV, 2u> ret(std::array<size_t, 2u>{dim1, dim0});
            auto ret_base = ret.data();
            for (size_t i = 0u; i < dim0; ++i, ++ret_base)
            {
                auto ret_iter = ret_base;
                WL_IGNORE_DEPENDENCIES
                for (size_t j = 0u; j < dim1; ++j, ++x_iter, ret_iter += dim0)
                    *ret_iter = std::conj(*x_iter);
            }
            return ret;
        }
        else
        {
            const size_t dim0 = x.dims()[0];
            const size_t dim1 = x.dims()[1];
            const auto dims_rest = utils::dims_take<3u, XR>(x.dims());
            const size_t chunk_size = utils::size_of_dims(dims_rest);
            ndarray<XV, XR> ret(utils::dims_join(
                std::array<size_t, 2u>{dim1, dim0}, dims_rest));
            auto ret_base = ret.data();
            for (size_t i = 0u; i < dim0; ++i, ret_base += chunk_size)
            {
                auto ret_base2 = ret_base;
                for (size_t j = 0u; j < dim1;
                    ++j, ret_base2 += dim0 * chunk_size)
                {
                    auto ret_iter = ret_base2;
                    WL_IGNORE_DEPENDENCIES
                    for (size_t k = 0u; k < chunk_size;
                        ++k, ++ret_iter, ++x_iter)
                    {
                        *ret_iter = std::conj(*x_iter);
                    }
                }
            }
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Level>
struct _flatten_max_level_impl;
template<int64_t... Is>
struct _flatten_max_level_impl<const_ints<Is...>> :
    _transpose_max_level<size_t(Is)...> {};
template<typename... Levels>
struct _flatten_max_level :
    _transpose_max_level<_flatten_max_level_impl<Levels>::value...> {};
template<>
struct _flatten_max_level<> : std::integral_constant<size_t, 0> {};
template<typename... Levels>
struct _flatten_levels_join;
template<int64_t... Is, int64_t... Js, typename... Levels>
struct _flatten_levels_join<const_ints<Is...>, const_ints<Js...>, Levels...> :
    _flatten_levels_join<const_ints<Is..., Js...>, Levels...> {};
template<int64_t... Is>
struct _flatten_levels_join<const_ints<Is...>>
{
    using type = const_ints<Is...>;
};
template<int64_t... Is, typename... Levels>
void _flatten_get_dims(const size_t* input_dims, size_t* ret_dims,
    const_ints<Is...>, Levels...)
{
    *ret_dims = (input_dims[Is - 1u] * ...);
    if constexpr (sizeof...(Levels) > 0u)
        _flatten_get_dims(input_dims, ret_dims + 1, Levels{}...);
}
template<size_t MaxLevel, typename X, size_t... Pads, typename... Levels>
auto _flatten_copy_impl(X&& x, std::index_sequence<Pads...>, Levels...)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto RetRank = sizeof...(Pads) + sizeof...(Levels);
    std::array<size_t, RetRank> ret_dims;
    _flatten_get_dims(x.dims().data(), ret_dims.data(),
        Levels{}..., const_ints<int64_t(Pads + MaxLevel + 1u)>{}...);
    if constexpr (is_movable_v<X&&>)
        return ndarray<XV, RetRank>(ret_dims, std::move(x.data_vector()));
    else if constexpr (XT::category != view_category::General)
        return ndarray<XV, RetRank>(ret_dims, x.begin(), x.begin() + x.size());
    else
    {
        ndarray<XV, RetRank> ret(ret_dims);
        x.copy_to(ret.data());
        return ret;
    }
}
template<typename X, typename... Levels>
auto flatten_copy(X&& x, Levels...)
{
    constexpr auto XR = array_rank_v<remove_cvref_t<X>>;
    constexpr auto MaxLevel = _flatten_max_level<Levels...>::value;
    static_assert(1 <= MaxLevel && MaxLevel <= XR, WL_ERROR_BAD_LEVEL);
    return _flatten_copy_impl<MaxLevel>(std::forward<decltype(x)>(x),
        std::make_index_sequence<XR - MaxLevel>{}, Levels{}...);
}
template<typename T, int64_t... Is, size_t... Cs>
void _flatten_impl3(const size_t* dims, const T* src, T* dst,
    const_ints<Is...>, std::index_sequence<Cs...>)
{
    constexpr auto R = sizeof...(Is);
    std::array<size_t, R> strides;
    std::array<size_t, R> ret_dims;
    std::array<size_t, R> ret_strides;
    strides[R - 1u] = 1u;
    [[maybe_unused]] auto _1 = ((Cs > 0 ? (strides[R - Cs - 1u] =
        strides[R - Cs] * dims[R - Cs]) : size_t()), ...);
    [[maybe_unused]] auto _2 = ((ret_strides[Cs] = strides[Is - 1],
        ret_dims[Cs] = dims[Is - 1]), ...);
    _transpose_fill<R>(src, dst, ret_dims.data(), ret_strides.data());
}
template<typename X, size_t... Pads, typename... Levels>
auto _flatten_impl2(X&& x, Levels...)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto RetRank = sizeof...(Levels);
    std::array<size_t, RetRank> ret_dims;
    _flatten_get_dims(x.dims().data(), ret_dims.data(), Levels{}...);
    ndarray<XV, RetRank> ret(ret_dims);
    const auto& valx = val(x);
    _flatten_impl3(valx.dims().data(), valx.data(), ret.data(),
        typename _flatten_levels_join<Levels...>::type{},
        std::make_index_sequence<XR>{});
    return ret;
}
template<size_t MaxLevel, typename X, size_t... Pads, typename... Levels>
auto _flatten_impl1(X&& x, std::index_sequence<Pads...>, Levels...)
{
    return _flatten_impl2(std::forward<decltype(x)>(x),
        Levels{}..., const_ints<int64_t(Pads + MaxLevel + 1u)>{}...);
}
template<typename X, typename... Levels>
auto flatten(X&& x, Levels...)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<remove_cvref_t<X>>;
    constexpr auto MaxLevel = _flatten_max_level<Levels...>::value;
    static_assert(1 <= MaxLevel && MaxLevel <= XR, WL_ERROR_BAD_LEVEL);
    return _flatten_impl1<MaxLevel>(std::forward<decltype(x)>(x),
        std::make_index_sequence<XR - MaxLevel>{}, Levels{}...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto flatten(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(1 <= XR, WL_ERROR_BAD_LEVEL);
    std::array<size_t, 1u> ret_dims{utils::size_of_dims(x.dims())};
    if constexpr (is_movable_v<X&&>)
        return ndarray<XV, 1u>(ret_dims, std::move(x.data_vector()));
    else if constexpr (XT::category != view_category::General)
        return ndarray<XV, 1u>(ret_dims, x.begin(), x.begin() + x.size());
    else
    {
        ndarray<XV, 1u> ret(ret_dims);
        x.copy_to(ret.data());
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
WL_INLINE int64_t _order_scalar(const X& x, const X& y)
{
    return x == y ? int64_t(0) : x < y ? int64_t(1) : int64_t(-1);
}
WL_INLINE int64_t _order_scalar(const boolean& x, const boolean& y)
{
    return x == y ? int64_t(0) : x ? int64_t(-1) : int64_t(1);
}
int64_t _order_scalar(const string&, const string&);
int64_t _order_scalar(const string&, const string_view&);
int64_t _order_scalar(const string_view&, const string&);
int64_t _order_scalar(const string_view&, const string_view&);
template<typename X>
WL_INLINE int64_t _order_scalar(const complex<X>& x, const complex<X>& y)
{
    const auto x_real = x.real();
    const auto y_real = y.real();
    if (x_real == y_real)
    {
        const auto x_imag = x.imag();
        const auto y_imag = y.imag();
        const auto abs_x_imag = wl::abs(x_imag);
        const auto abs_y_imag = wl::abs(y_imag);
        return abs_x_imag == abs_y_imag ? _order_scalar(x_imag, y_imag) :
            abs_x_imag < abs_y_imag ? int64_t(1) : int64_t(-1);
    }
    else
    {
        return x_real < y_real ? int64_t(1) : int64_t(-1);
    }
}
template<typename X, typename Y>
int64_t _order_array(const X& x, const Y& y, dim_checked)
{
    int64_t ret = 0;
    if constexpr (X::category != view_category::General)
    {
        y.for_each([&ret](const auto& b, const auto& a)
            {
                auto res = _order_scalar(a, b);
                if (res == 0) return false;
                ret = res;
                return true;
            }, x.begin());
    }
    else
    {
        x.for_each([&ret](const auto& a, const auto& b)
            {
                auto res = _order_scalar(a, b);
                if (res == 0) return false;
                ret = res;
                return true;
            }, y.begin());
    }
    return ret;
}
template<typename X, typename Y>
int64_t _order_array(const X& x, const Y& y)
{
    if (utils::check_dims(x.dims(), y.dims()))
        return _order_array(x, y, dim_checked{});
    else
        return std::lexicographical_compare(x.dims().begin(), x.dims().end(),
            y.dims().begin(), x.dims().end()) ? int64_t(1) : int64_t(-1);
}
template<typename Ret = int64_t, typename X, typename Y>
int64_t order(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<X>;
    static_assert(XR == YR, WL_ERROR_OPERAND_RANK);
    static_assert(is_integral_v<Ret>, WL_ERROR_BAD_RETURN);
    if constexpr (is_array_view_v<X>)
    {
        static_assert(is_array_view_v<Y>, WL_ERROR_OPERAND_TYPE);
        return Ret(_order_array(x, y));
    }
    else if constexpr (XR == 0)
    {
        static_assert(std::is_same_v<X, Y>, WL_ERROR_OPERAND_TYPE);
        return Ret(_order_scalar(x, y));
    }
    else
    {
        static_assert(std::is_same_v<value_type_t<X>, value_type_t<Y>>,
            WL_ERROR_OPERAND_TYPE);
        return Ret(_order_array(x, y));
    }
}
template<typename Ret = int64_t, typename X, typename Pred>
auto ordering(const X& x, const int64_t n, Pred pred)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_integral_v<Ret>, WL_ERROR_BAD_RETURN);
    if (n == 0)
        return ndarray<Ret, 1u>{};
    const std::array<size_t, 1u> ret_dims{size_t(n > 0 ? n : -n)};
    const auto& valx = val(std::forward<decltype(x)>(x));
    const auto x_size = valx.dims()[0];
    const auto x_base = valx.template view_begin<1u>() - 1;
    std::vector<Ret> indices(x_size);
    for (size_t i = 0u; i < x_size; ++i)
        indices[i] = Ret(i + 1);
    if (n > 0)
    {
        auto order = [=](size_t a, size_t b)
        {
            WL_THROW_IF_ABORT()
            auto res = pred(*(x_base + a), *(x_base + b));
            using OrderType = decltype(res);
            if constexpr (std::is_same_v<OrderType, bool> ||
                is_boolean_v<OrderType>)
                return bool(res);
            else
            {
                static_assert(std::is_signed_v<OrderType>,
                    WL_ERROR_ORDER_PRED_TYPE);
                return res > OrderType(0);
            }
        };
        if (size_t(n) > x_size)
            throw std::logic_error(WL_ERROR_ORDERING_OUT_OF_RANGE);
        else if (size_t(n) == x_size)
            std::sort(indices.begin(), indices.end(), order);
        else
        {
            std::partial_sort(indices.begin(), indices.begin() + n,
                indices.end(), order);
            indices.resize(size_t(n));
        }
    }
    else
    {
        auto order = [=](size_t a, size_t b)
        {
            WL_THROW_IF_ABORT()
            auto res = pred(*(x_base + a), *(x_base + b));
            using OrderType = decltype(res);
            if constexpr (std::is_same_v<OrderType, bool> ||
                is_boolean_v<OrderType>)
                return !bool(res);
            else
            {
                static_assert(std::is_signed_v<OrderType>,
                    WL_ERROR_ORDER_PRED_TYPE);
                return res < OrderType(0);
            }
        };
        if (size_t(-n) > x_size)
            throw std::logic_error(WL_ERROR_ORDERING_OUT_OF_RANGE);
        else if (size_t(-n) == x_size)
            std::sort(indices.rbegin(), indices.rend(), order);
        else
        {
            std::partial_sort(indices.rbegin(), indices.rbegin() + (-n),
                indices.rend(), order);
            indices.erase(indices.begin(), indices.end() - (-n));
        }
    }
    return ndarray<Ret, 1u>(ret_dims, std::move(indices));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto ordering(const X& x, const int64_t n)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    if constexpr (XR == 1u)
    {
        using XV = value_type_t<X>;
        if constexpr (is_complex_v<XV> || is_boolean_v<XV> ||
            is_string_view_v<XV>)
        {
            return ordering<Ret>(x, n, [](const auto& a, const auto& b)
                { return _order_scalar(a, b) > 0; });
        }
        else
        {
            static_assert(is_real_v<XV>, WL_ERROR_BAD_COMPARE);
            return ordering<Ret>(x, n, std::less<>{});
        }
    }
    else
    {
        return ordering<Ret>(x, n,
            [](const auto& a, const auto& b)
            { return _order_array(a, b, dim_checked{}) > 0; });
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X, typename Pred>
auto ordering(const X& x, all_type, Pred pred)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return ordering<Ret>(x, x.dims()[0], pred);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto ordering(const X& x)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return ordering<Ret>(x, x.dims()[0]);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X>
auto ordering(const X& x, all_type)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return ordering<Ret>(x, x.dims()[0]);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename T, typename Pred>
auto _sort_simple(ndarray<T, 1u>&& a, Pred pred)
{
    WL_THROW_IF_ABORT()
    std::sort(a.begin(), a.end(), pred);
    return std::move(a);
}
template<typename T, typename Pred>
auto _sort_simple(const ndarray<T, 1u>& a, Pred pred)
{
    auto copy = a;
    WL_THROW_IF_ABORT()
    std::sort(copy.begin(), copy.end(), pred);
    return copy;
}
template<typename X, typename Pred>
auto sort(X&& x, Pred pred)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    if constexpr (XR == 1u)
    {
        auto&& valx = val(std::forward<decltype(x)>(x));
        using OrderType = remove_cvref_t<
            decltype(pred(*valx.begin(), *valx.begin()))>;
        if constexpr (std::is_same_v<OrderType, bool> ||
            is_boolean_v<OrderType>)
            return _sort_simple(valx, pred);
        else
        {
            static_assert(std::is_signed_v<OrderType>,
                WL_ERROR_ORDER_PRED_TYPE);
            return _sort_simple(valx, [=](const auto& a, const auto& b)
                { return pred(a, b) > OrderType(0); });
        }
    }
    else
    {
        const auto& valx = val(std::forward<decltype(x)>(x));
        const auto x_size = valx.dims()[0];
        const auto order_indices = ordering(valx, x_size, pred);
        const auto order_data = order_indices.data();
        const auto x_base = valx.template view_begin<1u>() - 1;
        ndarray<value_type_t<XT>, XR> ret(valx.dims());
        auto ret_iter = ret.template view_begin<1u>();
        for (size_t i = 0u; i < x_size; ++i, ++ret_iter)
            (*(x_base + order_data[i])).copy_to(ret_iter.begin());
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto sort(X&& x)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    if constexpr (XR == 1u)
    {
        using XV = value_type_t<XT>;
        if constexpr (is_complex_v<XV> || is_boolean_v<XV> ||
            is_string_view_v<XV>)
        {
            return sort(std::forward<decltype(x)>(x),
                [](const auto& a, const auto& b)
                { return _order_scalar(a, b) > 0; });
        }
        else
        {
            static_assert(is_real_v<XV>, WL_ERROR_BAD_COMPARE);
            return sort(std::forward<decltype(x)>(x), std::less<>{});
        }
    }
    else
    {
        return sort(std::forward<decltype(x)>(x),
            [](const auto& a, const auto& b)
            { return _order_array(a, b, dim_checked{}) > 0; });
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Pred>
auto ordered_q(const X& x, Pred pred)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto& copy = allows<view_category::Array>(x);
    const auto copy_length = copy.dims()[0];
    auto in_order = [=](const auto& a, const auto& b)
    {
        WL_THROW_IF_ABORT()
        using OrderType = remove_cvref_t<decltype(pred(a, b))>;
        if constexpr (std::is_same_v<OrderType, bool> ||
            is_boolean_v<OrderType>)
            return bool(pred(a, b));
        else
            return pred(a, b) >= OrderType(0);
    };
    auto iter = copy.template view_begin<1u>();
    auto ret = true;
    for (size_t i = 1u; ret && i < copy_length; ++i, ++iter)
        ret = ret && in_order(*iter, *(iter + 1));
    return boolean(ret);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto ordered_q(const X& x)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto& copy = allows<view_category::Array>(x);
    const auto copy_length = copy.dims()[0];
    auto in_order = [=](const auto& a, const auto& b)
    {
        WL_THROW_IF_ABORT()
        constexpr auto AR = array_rank_v<remove_cvref_t<decltype(a)>>;
        if constexpr (AR == 0u)
            return _order_scalar(a, b) >= 0;
        else
            return _order_array(a, b, dim_checked{}) >= 0;
    };
    auto iter = copy.template view_begin<1u>();
    auto ret = true;
    for (size_t i = 1u; ret && i < copy_length; ++i, ++iter)
        ret = ret && in_order(*iter, *(iter + 1));
    return boolean(ret);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto append(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto YR = array_rank_v<YT>;
    using XV = value_type_t<XT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u, WL_ERROR_APPEND_RANK);
    static_assert(is_convertible_v<YV, XV>, WL_ERROR_JOIN_VALUE_TYPE);
    if constexpr (is_movable_v<X&&>)
    {
        x.append(std::forward<decltype(y)>(y));
        return std::move(x);
    }
    else if constexpr (XR == 1u)
    {
        ndarray<XV, XR> ret(std::array<size_t, 1u>{x.dims()[0] + 1u});
        const auto ret_iter = ret.data();
        x.copy_to(ret_iter);
        *(ret_iter + x.size()) = std::forward<decltype(y)>(y);
        return ret;
    }
    else
    {
        const auto x_elem_dims = utils::dims_take<2u, XR>(x.dims());
        if (!utils::check_dims(x_elem_dims, y.dims()))
            throw std::logic_error(WL_ERROR_INSERT_ELEM_DIMS);
        ndarray<XV, XR> ret(utils::dims_join(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, x_elem_dims));
        const auto ret_iter = ret.data();
        x.copy_to(ret_iter);
        y.copy_to(ret_iter + x.size());
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto prepend(X&& x, Y&& y)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto YR = array_rank_v<YT>;
    using XV = value_type_t<XT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u, WL_ERROR_APPEND_RANK);
    static_assert(is_convertible_v<YV, XV>, WL_ERROR_JOIN_VALUE_TYPE);
    if constexpr (XR == 1u)
    {
        ndarray<XV, XR> ret(std::array<size_t, 1u>{x.dims()[0] + 1u});
        const auto ret_iter = ret.data();
        *ret_iter = std::forward<decltype(y)>(y);
        x.copy_to(ret_iter + 1);
        return ret;
    }
    else
    {
        const auto x_elem_dims = utils::dims_take<2u, XR>(x.dims());
        if (!utils::check_dims(x_elem_dims, y.dims()))
            throw std::logic_error(WL_ERROR_INSERT_ELEM_DIMS);
        ndarray<XV, XR> ret(utils::dims_join(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, x_elem_dims));
        const auto ret_iter = ret.data();
        y.copy_to(ret_iter);
        x.copy_to(ret_iter + y.size());
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename XV, size_t XR, typename Y>
auto append_to(ndarray<XV, XR>& x, Y&& y) -> const auto&
{
    WL_TRY_BEGIN()
    using YT = remove_cvref_t<Y>;
    constexpr auto YR = array_rank_v<YT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u, WL_ERROR_APPEND_RANK);
    static_assert(is_convertible_v<YV, XV>, WL_ERROR_JOIN_VALUE_TYPE);
    x.append(std::forward<decltype(y)>(y));
    return x;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename XV, size_t XR, typename Y>
auto prepend_to(ndarray<XV, XR>& x, Y&& y) -> const auto&
{
    WL_TRY_BEGIN()
    using YT = remove_cvref_t<Y>;
    constexpr auto YR = array_rank_v<YT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u, WL_ERROR_APPEND_RANK);
    static_assert(is_convertible_v<YV, XV>, WL_ERROR_JOIN_VALUE_TYPE);
    if constexpr (XR == 1u)
    {
        const auto x_size = x.size();
        const auto new_size = x_size + 1u;
        x.uninitialized_resize(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, new_size);
        const auto x_iter = x.data();
        std::move_backward(x_iter, x_iter + x_size, x_iter + new_size);
        *x_iter = std::forward<decltype(y)>(y);
    }
    else
    {
        const auto x_elem_dims = utils::dims_take<2u, XR>(x.dims());
        if (!utils::check_dims(x_elem_dims, y.dims()))
            throw std::logic_error(WL_ERROR_INSERT_ELEM_DIMS);
        const auto x_size = x.size();
        const auto y_size = y.size();
        const auto new_size = x_size + y_size;
        x.uninitialized_resize(utils::dims_join(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, x_elem_dims), new_size);
        const auto x_iter = x.data();
        std::move_backward(x_iter, x_iter + x_size, x_iter + new_size);
        y.copy_to(x_iter);
    }
    return x;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<size_t Level, size_t Rank, typename Arg>
auto _join_dims_by_args_impl(
    const std::array<size_t, Rank>& dims, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        return arg.size() == 0u ? size_t(0) :
            arg.size() * _join_dims_by_args_impl<Level>(dims, arg.get(0));
    }
    else
    {
        static_assert(array_rank_v<Arg> == Rank, WL_ERROR_JOIN_RANK);
        if (arg.size() > 0u)
        {
            const auto leading_check = utils::check_dims<Level - 1u>(
                dims.data(), arg.dims().data());
            const auto trailing_check = utils::check_dims<Rank - Level>(
                dims.data() + Level, arg.dims().data() + Level);
            if (!leading_check || !trailing_check)
                throw std::logic_error(WL_ERROR_INSERT_ELEM_DIMS);
            return arg.dims()[Level - 1];
        }
        else
            return size_t(0);
    }
}
template<size_t Level, size_t Rank, typename... Args>
void _join_dims_by_args(std::array<size_t, Rank>& dims, const Args&... args)
{
    dims[Level - 1] += (size_t(0) + ... +
        _join_dims_by_args_impl<Level>(dims, args));
}
template<size_t Level, size_t Rank, typename Iter, typename Arg>
void _join_copy_leveln(Iter& ret_base, size_t stride, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        if (arg.size() == 0)
            return;
        auto arg_dims = arg.get(0).dims();
        const auto leading_size = utils::size_of_dims(
            utils::dims_take<1u, Level - 1u>(arg_dims));
        const auto trailing_size = utils::size_of_dims(
            utils::dims_take<Level, Rank>(arg_dims));
        for (size_t k = 0u; k < arg.size(); ++k, ret_base += trailing_size)
        {
            const auto& arg_k = arg.get(k);
            auto arg_iter = arg_k.begin();
            auto ret_iter = ret_base;
            for (size_t i = 0u; i < leading_size; ++i, ret_iter += stride)
            {
                WL_IGNORE_DEPENDENCIES
                for (size_t j = 0u; j < trailing_size; ++j, ++arg_iter)
                    ret_iter[j] = *arg_iter;
            }
        }
    }
    else if (arg.size() > 0u)
    {
        const auto arg_dims = arg.dims();
        const auto leading_size = utils::size_of_dims(
            utils::dims_take<1u, Level - 1u>(arg_dims));
        const auto trailing_size = utils::size_of_dims(
            utils::dims_take<Level, Rank>(arg_dims));
        if constexpr (Arg::category == view_category::General)
        {
            auto ret_iter = ret_base;
            size_t j = 0u;
            arg.for_each([&](const auto& x)
                {
                    ret_iter[j] = x;
                    if ((++j) >= trailing_size)
                    {
                        j = 0u;
                        ret_iter += stride;
                    }
                });
        }
        else
        {
            auto arg_iter = arg.begin();
            auto ret_iter = ret_base;
            for (size_t i = 0u; i < leading_size; ++i, ret_iter += stride)
            {
                WL_IGNORE_DEPENDENCIES
                for (size_t j = 0u; j < trailing_size; ++j, ++arg_iter)
                    ret_iter[j] = *arg_iter;
            }
        }
        ret_base += trailing_size;
    }
}
template<typename Iter, typename Arg>
void _join_copy_level1(Iter& iter, const Arg& arg)
{
    if constexpr (is_argument_pack_v<Arg>)
    {
        for (size_t i = 0; i < arg.size(); ++i)
            _join_copy_level1(iter, arg.get(i));
    }
    else
    {
        arg.copy_to(iter);
        iter += arg.size();
    }
}
template<int64_t I, typename First, typename... Rest>
auto join(const_int<I>, First&& first, Rest&&... rest)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    using FirstType = remove_cvref_t<First>;
    if constexpr (is_argument_pack_v<FirstType>)
    {
        if (first.size() == 0u)
        {
            if constexpr (sizeof...(rest) > 0u)
                return join(const_int<I>{},
                    std::forward<decltype(rest)>(rest)...);
            else
            {
                using ItemType = value_type_t<FirstType>;
                constexpr auto rank = array_rank_v<ItemType>;
                using ValueType = std::conditional_t<
                    rank == 0u, ItemType, value_type_t<ItemType>>;
                return ndarray<value_type_t<ItemType>, rank>{};
            }
        }
        else
            return join(const_int<I>{}, first.get(0), first.get_pack(1),
                std::forward<decltype(rest)>(rest)...);
    }
    else
    {
        constexpr auto rank = array_rank_v<FirstType>;
        static_assert(1u <= Level && Level <= rank, WL_ERROR_BAD_LEVEL);
        auto ret_dims = first.dims();
        _join_dims_by_args<Level>(ret_dims, rest...);
        ndarray<value_type_t<FirstType>, rank> ret(ret_dims);
        if constexpr (Level == 1u)
        {
            auto ret_iter = ret.data();
            _join_copy_level1(ret_iter, first);
            [[maybe_unused]] const auto& _1 = (
                _join_copy_level1(ret_iter, rest), ..., 0);
        }
        else
        {
            auto ret_iter = ret.data();
            auto stride = utils::size_of_dims(
                utils::dims_take<Level, rank>(ret_dims));
            _join_copy_leveln<Level, rank>(ret_iter, stride, first);
            [[maybe_unused]] const auto& _1 = (
                _join_copy_leveln<Level, rank>(ret_iter, stride, rest), ..., 0);
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename First, typename... Rest>
auto join(First&& first, Rest&&... rest)
{
    WL_TRY_BEGIN()
    return join(const_int<1>{}, std::forward<decltype(first)>(first),
        std::forward<decltype(rest)>(rest)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename First, typename... Rest>
auto set_union(const First& first, const Rest&... rest)
{
    WL_TRY_BEGIN()
    auto scalar_less = [](const auto& x, const auto& y)
    {
        return _order_scalar(x, y) == int64_t(1);
    };
    auto copy = join(first, rest...);
    using T = value_type_t<decltype(copy)>;
    constexpr auto R = array_rank_v<decltype(copy)>;
    const auto copy_length = copy.dims()[0];
    const auto copy_data = copy.data();
    if constexpr (R == 1u)
    {
        std::sort(copy_data, copy_data + copy_length, scalar_less);
        const auto copy_end = std::unique(
            copy_data, copy_data + copy_length);
        const auto union_size = size_t(copy_end - copy_data);
        copy.uninitialized_resize(std::array<size_t, 1u>{union_size});
        return copy;
    }
    else
    {
        const auto item_dims = utils::dims_take<2, R>(copy.dims());
        const auto item_size = utils::size_of_dims(item_dims);
        auto idx = range(size_t(0), copy_length - 1u);
        auto idx_begin = idx.data();
        auto sort_pred = [=](size_t a, size_t b)
        {
            WL_THROW_IF_ABORT()
            auto a_iter = copy_data + a * item_size;
            auto b_iter = copy_data + b * item_size;
            return std::lexicographical_compare(
                a_iter, a_iter + item_size,
                b_iter, b_iter + item_size,
                scalar_less);
        };
        std::sort(idx_begin, idx_begin + copy_length, sort_pred);
        auto unique_pred = [=](size_t a, size_t b)
        {
            auto a_iter = copy_data + a * item_size;
            auto b_iter = copy_data + b * item_size;
            return std::equal(a_iter, a_iter + item_size, b_iter);
        };
        const auto idx_end = std::unique(
            idx_begin, idx_begin + copy_length, unique_pred);
        const auto union_size = size_t(idx_end - idx_begin);
        ndarray<T, R> ret(utils::dims_join(
            std::array<size_t, 1u>{union_size}, item_dims));
        const auto copy_base = copy.template view_begin<1u>();
        auto ret_iter = ret.template view_begin<1u>();
        for (size_t i = 0u; i < union_size; ++i, ++ret_iter)
            (*(copy_base + idx_begin[i])).copy_to(ret_iter.begin());
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto _rotate_impl(const X& x, int64_t n)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<X>;
    const auto item_count = x.dims()[0];
    if (item_count == 0u)
        return ndarray<XV, XR>(x.dims());
    const auto& valx = allows<view_category::Regular>(x);
    if (n >= int64_t(item_count))
        n = n % item_count;
    else if (n <= -int64_t(item_count))
        n = -((-n) % item_count);
    if (n == 0)
        return allows<view_category::Array>(valx);
    const auto item_size = utils::size_of_dims(
        utils::dims_take<2u, XR>(valx.dims()));
    auto x_iter = valx.begin();
    ndarray<XV, XR> ret(valx.dims());
    const auto ret_iter = ret.data();
    
    if (n >= 0)
    { // rotate right
        const auto size1 = size_t(n) * item_size;
        const auto size2 = ret.size() - size1;
        auto iter2 = ret_iter + size1;
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0; i < size2; ++i, ++iter2, ++x_iter)
            *iter2 = *x_iter;
        auto iter1 = ret_iter;
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0; i < size1; ++i, ++iter1, ++x_iter)
            *iter1 = *x_iter;
    }
    else
    { // rotate left
        const auto size1 = size_t(-n) * item_size;
        const auto size2 = ret.size() - size1;
        auto iter1 = ret_iter + size2;
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0; i < size1; ++i, ++iter1, ++x_iter)
            *iter1 = *x_iter;
        auto iter2 = ret_iter;
        WL_IGNORE_DEPENDENCIES
        for (size_t i = 0; i < size2; ++i, ++iter2, ++x_iter)
            *iter2 = *x_iter;
    }
    return ret;
}
template<typename X, typename N>
auto rotate_left(const X& x, const N& n)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<N>, WL_ERROR_COUNTING_ARG);
    return _rotate_impl(x, -int64_t(n));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename N>
auto rotate_right(const X& x, const N& n)
{
    WL_TRY_BEGIN()
    static_assert(is_integral_v<N>, WL_ERROR_COUNTING_ARG);
    return _rotate_impl(x, int64_t(n));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto rotate_left(const X& x)
{
    WL_TRY_BEGIN()
    return _rotate_impl(x, -1);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto rotate_right(const X& x)
{
    WL_TRY_BEGIN()
    return _rotate_impl(x, 1);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<size_t I, size_t Level, typename Ret, typename Iter, typename IsEqual>
void _position_impl(const std::array<size_t, Level>& pos_dims,
    ndarray<Ret, 2u>& ret, ndarray<Ret, 1u>& pos_idx, Ret* const idx_base,
    Iter& x_iter, IsEqual is_equal)
{
    const auto dim = pos_dims[I];
    if constexpr (I + 1u < Level)
    {
        for (size_t i = 1u; i <= dim; ++i)
        {
            idx_base[I] = i;
            _position_impl<I + 1u, Level, Ret>(
                pos_dims, ret, pos_idx, idx_base, x_iter, is_equal);
        }
    }
    else
    {
        for (size_t i = 1u; i <= dim; ++i, ++x_iter)
        {
            if (is_equal(*x_iter))
            {
                idx_base[I] = i;
                ret.append(pos_idx, dim_checked{});
            }
        }
    }
}
template<typename Ret = int64_t, typename X, typename Y, int64_t I>
auto position(const X& x, const Y& y, const_int<I>)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(1u <= Level && Level + YR == XR, WL_ERROR_BAD_LEVEL);
    const auto& valx = allows<view_category::Array>(x);
    const auto& valy = allows<view_category::Simple>(y);
    return position(valx, varg_tag{},
        [&](const auto& a) { return equal(a, valy); }, const_int<Level>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X, typename Function, int64_t I>
auto position(const X& x, varg_tag, Function f, const_int<I>)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, WL_ERROR_BAD_LEVEL);
    
    const auto& valx = allows<view_category::Array>(x);
    if constexpr (Level == 1u)
    {
        auto x_iter = valx.template view_begin<1u>();
        const auto size = valx.dims()[0];
        ndarray<Ret, 1u> ret;
        for (size_t i = 1u; i <= size; ++i, ++x_iter)
        {
            if (f(*x_iter))
                ret.append(Ret(i), dim_checked{});
        }
        const auto ret_size = ret.size();
        return ndarray<Ret, 2u>(std::array<size_t, 2u>{ret_size, 1u},
            std::move(ret).data_vector());
    }
    else
    {
        auto x_iter = valx.template view_begin<Level>();
        const auto pos_dims = utils::dims_take<1u, Level>(valx.dims());
        ndarray<Ret, 2u> ret(std::array<size_t, 2u>{0u, Level});
        ndarray<Ret, 1u> pos_idx(std::array<size_t, 1u>{Level});
        Ret* const idx_base = pos_idx.data();
        _position_impl<0u, Level, Ret>(
            pos_dims, ret, pos_idx, idx_base, x_iter, f);
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Ret = int64_t, typename X, typename Y>
auto position(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR > YR, WL_ERROR_POSITION_RANK);
    return position(x, y, const_int<XR - YR>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Function, int64_t I>
auto cases(const X& x, varg_tag, Function f, const_int<I>)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, WL_ERROR_BAD_LEVEL);
    using XV = value_type_t<X>;
    if constexpr (XR == Level)
    {
        using RT = remove_cvref_t<decltype(f(XV{}))>;
        static_assert(is_boolean_v<RT>, WL_ERROR_PRED_TYPE);
        ndarray<XV, 1u> ret;
        x.for_each([&](const auto& a) {
            if (f(a)) ret.append(a, dim_checked{}); });
        return ret;
    }
    else
    {
        const auto& valx = allows<view_category::Array>(x);
        auto view_iter = valx.template view_begin<Level>();
        const auto view_end = valx.template view_end<Level>();
        using RT = remove_cvref_t<decltype(f(*view_iter))>;
        static_assert(is_boolean_v<RT>, WL_ERROR_PRED_TYPE);
        ndarray<XV, XR - Level + 1u> ret;
        for (; view_iter != view_end; ++view_iter)
        {
            if (f(*view_iter))
                ret.append(*view_iter, dim_checked{});
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y, int64_t I>
auto cases(const X& x, const Y& y, const_int<I>)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(1u <= Level && Level <= XR, WL_ERROR_BAD_LEVEL);
    if constexpr (XR != YR + Level)
    {
        return ndarray<value_type_t<X>, XR - Level + 1u>{};
    }
    else
    {
        auto same_dims = true;
        if constexpr (YR > 0u)
            same_dims = utils::check_dims<YR>(
                x.dims().data() + Level, y.dims().data());
        if (!same_dims)
        {
            return ndarray<value_type_t<X>, XR - Level + 1u>{};
        }
        else
        {
            const auto& valy = allows<view_category::Simple>(y);
            return cases(x, varg_tag{},
                [&](const auto& a) { return same_q(a, valy, dim_checked{}); },
                const_int<Level>{});
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto cases(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(YR < XR, WL_ERROR_POSITION_RANK);
    return cases(x, y, const_int<XR - YR>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Function>
auto delete_cases(const X& x, varg_tag, Function f)
{
    WL_TRY_BEGIN()
    static_assert(array_rank_v<X> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    return cases(x, varg_tag{},
        [=](auto&& a) {return !f(std::forward<decltype(a)>(a)); },
        const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Function, int64_t I>
auto delete_cases(const X& x, varg_tag, Function f, const_int<I>)
{
    WL_TRY_BEGIN()
    static_assert(I == 1, WL_ERROR_DELETE_CASES_LEVEL);
    return delete_cases(x, varg_tag{}, f);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto delete_cases(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(XR == YR + 1u, WL_ERROR_DELETE_CASES_LEVEL);
    auto same_dims = true;
    if constexpr (YR > 0u)
        same_dims = utils::check_dims<YR>(
            x.dims().data() + 1, y.dims().data());
    if (!same_dims)
    {
        return val(x);
    }
    else
    {
        const auto& valy = allows<view_category::Simple>(y);
        return cases(x, varg_tag{},
            [&](const auto& a) { return unsame_q(a, valy, dim_checked{}); },
            const_int<1>{});
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y, int64_t I>
auto delete_cases(const X& x, const Y& y, const_int<I>)
{
    WL_TRY_BEGIN()
    static_assert(I == 1, WL_ERROR_DELETE_CASES_LEVEL);
    return delete_cases(x, y);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Function, int64_t I>
auto member_q(const X& x, varg_tag, Function f, const_int<I>)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, WL_ERROR_BAD_LEVEL);
    using XV = value_type_t<X>;
    if constexpr (XR == Level)
    {
        using RT = remove_cvref_t<decltype(f(XV{}))>;
        static_assert(is_boolean_v<RT>, WL_ERROR_PRED_TYPE);
        auto ret = false;
        x.for_each([&](const auto& a) {
            ret = ret || f(a); return ret; });
        return boolean(ret);
    }
    else
    {
        const auto& valx = allows<view_category::Array>(x);
        auto view_iter = valx.template view_begin<Level>();
        const auto view_end = valx.template view_end<Level>();
        using RT = remove_cvref_t<decltype(f(*view_iter))>;
        static_assert(is_boolean_v<RT>, WL_ERROR_PRED_TYPE);
        auto ret = false;
        for (; view_iter != view_end && !ret; ++view_iter)
            ret = ret || f(*view_iter);
        return boolean(ret);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y, int64_t I>
auto member_q(const X& x, const Y& y, const_int<I>)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(1u <= Level && Level <= XR, WL_ERROR_BAD_LEVEL);
    if constexpr (XR != YR + Level)
    {
        return const_false;
    }
    else
    {
        auto same_dims = true;
        if constexpr (YR > 0u)
            same_dims = utils::check_dims<YR>(
                x.dims().data() + Level, y.dims().data());
        if (!same_dims)
        {
            return const_false;
        }
        else
        {
            const auto& valy = allows<view_category::Simple>(y);
            return member_q(x, varg_tag{},
                [&](const auto& a) { return same_q(a, valy, dim_checked{}); },
                const_int<Level>{});
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto member_q(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(YR < XR, WL_ERROR_POSITION_RANK);
    return member_q(x, y, const_int<XR - YR>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Function, int64_t I>
auto free_q(const X& x, varg_tag, Function f, const_int<I>)
{
    WL_TRY_BEGIN()
    return !member_q(x, varg_tag{}, f, const_int<I>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y, int64_t I>
auto free_q(const X& x, const Y& y, const_int<I>)
{
    WL_TRY_BEGIN()
    return !member_q(x, y, const_int<I>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto free_q(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    return !member_q(x, y);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<bool ScalarY, typename X, typename Y>
void _insert_impl2(const X* WL_RESTRICT src_ptr, X* WL_RESTRICT dst_ptr,
    const Y* WL_RESTRICT y_ptr, const int64_t* WL_RESTRICT pos_ptr,
    const size_t x_size, const size_t y_size, const size_t pos_size)
{
    WL_THROW_IF_ABORT()
    size_t last_offset = 0u;
    size_t this_offset = 0u;
    for (size_t p = 0; p < pos_size; ++p, ++pos_ptr)
    {
        this_offset = size_t(ScalarY ? (*pos_ptr) : (*pos_ptr) * y_size);
        const auto copy_size = this_offset - last_offset;
        last_offset = this_offset;
        for (size_t i = 0; i < copy_size; ++i, ++src_ptr, ++dst_ptr)
            *dst_ptr = *src_ptr;
        if constexpr (ScalarY)
        {
            *dst_ptr = *y_ptr;
            ++dst_ptr;
        }
        else
        {
            for (size_t i = 0; i < y_size; ++i, ++dst_ptr)
                *dst_ptr = y_ptr[i];
        }
    }
    const auto copy_size = x_size - last_offset;
    for (size_t i = 0; i < copy_size; ++i, ++src_ptr, ++dst_ptr)
        *dst_ptr = *src_ptr;
}
template<typename X, typename Y>
auto _insert_impl1(X&& x, const Y& y, ndarray<int64_t, 1u> pos)
{
    const auto pos_size = pos.size();
    if (pos_size == 1u)
    {
        return _insert_impl1(std::forward<decltype(x)>(x), y, *(pos.data()));
    }
    else
    {
        using XT = remove_cvref_t<X>;
        using XV = value_type_t<XT>;
        constexpr auto XR = array_rank_v<XT>;
        constexpr auto YR = array_rank_v<Y>;
        pos.for_each([d0 = x.dims()[0] + 1u](auto& a){
            a = int64_t(convert_index(a, d0)); });
        auto* pos_begin = pos.data();
        auto* pos_end = pos_begin + pos.size();
        auto* pos_mid = std::is_sorted_until(pos_begin, pos_end);
        if (pos_mid != pos_end)
        {
            std::sort(pos_mid, pos_end);
            std::inplace_merge(pos_begin, pos_mid, pos_end);
        }
        const auto& valx = allows<view_category::Simple>(x);
        auto ret_dims = valx.dims();
        ret_dims[0] += pos_size;
        auto ret = ndarray<XV, XR>(ret_dims);
        if constexpr (YR == 0u)
        {
            _insert_impl2<true>(
                valx.data(), ret.data(), &y, pos.data(),
                valx.size(), 1u, pos.size());
        }
        else
        {
            const auto& valy = cast<ndarray<XV, YR>>(y);
            _insert_impl2<false>(
                valx.data(), ret.data(), valy.data(), pos.data(),
                valx.size(), valy.size(), pos.size());
        }
        return ret;
    }
}
template<typename X, typename Y>
auto _insert_impl1(X&& x, const Y& y, int64_t pos)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto YR = array_rank_v<Y>;
    pos = int64_t(convert_index(pos, x.dims()[0] + 1u));
    const auto& valx = allows<view_category::Simple>(x);
    auto ret_dims = valx.dims();
    ret_dims[0] += 1u;
    auto ret = ndarray<XV, XR>(ret_dims);
    if constexpr (YR == 0u)
    {
        _insert_impl2<true>(
            valx.data(), ret.data(), &y, &pos,
            valx.size(), 1u, 1u);
    }
    else
    {
        const auto& valy = cast<ndarray<XV, YR>>(y);
        _insert_impl2<false>(
            valx.data(), ret.data(), valy.data(), &pos,
            valx.size(), valy.size(), 1u);
    }
    return ret;
}
template<typename X, typename Y, typename Pos>
auto insert(X&& x, const Y& y, Pos&& pos)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<remove_cvref_t<X>>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR == YR + 1u, WL_ERROR_INSERT_RANK);
    using PosT = remove_cvref_t<Pos>;
    using PosV = value_type_t<PosT>;
    
    if constexpr (YR >= 1u)
    {
        if (!utils::check_dims<YR>(x.dims().data() + 1, y.dims().data()))
            throw std::logic_error(WL_ERROR_INSERT_ELEM_DIMS);
    }
    if constexpr (array_rank_v<PosT> == 2u && is_integral_v<PosV>)
    {
        const auto& pos_dims = pos.dims();
        if (pos_dims[1] != 1u)
            throw std::logic_error(WL_ERROR_INSERT_POS_DIMS);
        if (pos_dims[0] == 0u)
            return val(x);
        else
        {
            auto ins_pos = ndarray<int64_t, 1u>(
                std::array<size_t, 1>{pos.size()});
            pos.copy_to(ins_pos.data());
            return _insert_impl1(
                std::forward<decltype(x)>(x), y, std::move(ins_pos));
        }
    }
    else if constexpr (is_integral_v<PosT>)
    {
        return _insert_impl1(std::forward<decltype(x)>(x), y, pos);
    }
    else
    {
        static_assert(always_false_v<Pos>, WL_ERROR_INSERT_POS_FORM);
        return val(x);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<bool ScalarY, typename X>
void _delete_impl2(const X* WL_RESTRICT src_ptr, X* WL_RESTRICT dst_ptr,
    const int64_t* WL_RESTRICT pos_ptr,
    const size_t x_size, const size_t y_size, const size_t pos_size)
{
    WL_THROW_IF_ABORT()
    size_t last_offset = 0u;
    size_t this_offset = 0u;
    for (size_t p = 0; p < pos_size; ++p, ++pos_ptr)
    {
        this_offset = size_t(ScalarY ? (*pos_ptr) : (*pos_ptr) * y_size);
        const auto copy_size = this_offset - last_offset;
        last_offset = this_offset;
        for (size_t i = 0; i < copy_size; ++i, ++src_ptr, ++dst_ptr)
            *dst_ptr = *src_ptr;
        src_ptr += ScalarY ? size_t(1u) : y_size;
    }
    const auto copy_size = x_size - last_offset;
    for (size_t i = 0; i < copy_size; ++i, ++src_ptr, ++dst_ptr)
        *dst_ptr = *src_ptr;
}
template<typename X>
auto _delete_impl1(X&& x, ndarray<int64_t, 1u> pos)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    bool delete_single = false;
    size_t pos_unique_size = 0;
    if (pos.size() == 1u)
    {
        delete_single = true;
    }
    else
    {
        pos.for_each([d0 = x.dims()[0]](auto& a){
            a = int64_t(convert_index(a, d0)); });
        auto* pos_begin = pos.data();
        auto* pos_end = pos_begin + pos.size();
        auto* pos_mid = std::is_sorted_until(pos_begin, pos_end);
        if (pos_mid != pos_end)
        {
            std::sort(pos_mid, pos_end);
            std::inplace_merge(pos_begin, pos_mid, pos_end);
        }
        auto* pos_unique_end = std::unique(pos_begin, pos_end);
        if (pos_unique_end == pos_begin + 1)
            delete_single = true;
        else
            pos_unique_size = pos_unique_end - pos_begin;
    }
    if (delete_single)
    {
        return _delete_impl1(std::forward<decltype(x)>(x), *(pos.data()));
    }
    else
    {
        const auto& valx = allows<view_category::Simple>(x);
        auto ret_dims = valx.dims();
        ret_dims[0] -= pos_unique_size;
        auto ret = ndarray<XV, XR>(ret_dims);
        WL_THROW_IF_ABORT()
        if constexpr (XR == 1u)
            _delete_impl2<true>(valx.data(), ret.data(), pos.data(),
                valx.size(), 1u, pos_unique_size);
        else
            _delete_impl2<false>(
                valx.data(), ret.data(), pos.data(), valx.size(),
                utils::size_of_dims<XR - 1u>(x.dims().data() + 1u),
                pos_unique_size);
        return ret;
    }
}
template<typename X>
auto _delete_impl1(X&& x, int64_t pos)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    pos = int64_t(convert_index(pos, x.dims()[0]));
    const auto& valx = allows<view_category::Simple>(x);
    auto ret_dims = valx.dims();
    ret_dims[0] -= 1u;
    auto ret = ndarray<XV, XR>(ret_dims);
    if constexpr (XR == 1u)
        _delete_impl2<true>(valx.data(), ret.data(), &pos,
            valx.size(), 1u, 1u);
    else
        _delete_impl2<false>(valx.data(), ret.data(), &pos, valx.size(),
            utils::size_of_dims<XR - 1u>(x.dims().data() + 1u), 1u);
    return ret;
}
template<typename X, typename Pos>
auto delete_(X&& x, Pos&& pos)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<remove_cvref_t<X>>;
    using PosT = remove_cvref_t<Pos>;
    using PosV = value_type_t<PosT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    if constexpr (array_rank_v<PosT> == 2u && is_integral_v<PosV>)
    {
        const auto& pos_dims = pos.dims();
        if (pos_dims[1] != 1u)
            throw std::logic_error(WL_ERROR_INSERT_POS_DIMS);
        if (pos_dims[0] == 0u)
            return val(x);
        else
        {
            auto del_pos = ndarray<int64_t, 1u>(
                std::array<size_t, 1>{pos.size()});
            pos.copy_to(del_pos.data());
            return _delete_impl1(
                std::forward<decltype(x)>(x), std::move(del_pos));
        }
    }
    else if constexpr (is_integral_v<PosT>)
    {
        return _delete_impl1(std::forward<decltype(x)>(x), pos);
    }
    else
    {
        static_assert(always_false_v<Pos>, WL_ERROR_INSERT_POS_FORM);
        return val(x);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Spec>
auto _take_get_spec(const Spec& spec)
{
    if constexpr (std::is_same_v<all_type, Spec>)
    {
        return const_all;
    }
    else if constexpr (is_integral_v<Spec>)
    {
        if constexpr (std::is_unsigned_v<Spec>)
            return make_span(const_all, spec);
        else if (spec > Spec(0))
            return make_span(int64_t(1), int64_t(spec));
        else
            return make_span(int64_t(spec), int64_t(-1));
    }
    else if constexpr (array_rank_v<Spec> == 1u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto spec_size = spec.size();
        if (!(1u <= spec_size && spec_size <= 3u))
            throw std::logic_error(WL_ERROR_TAKE_SPEC_LIST_LENGTH);
        std::array<value_type_t<Spec>, 3u> spec_data;
        spec.copy_to(spec_data.data());
        if (spec_size == 1u)
            return make_span(
                spec_data[0], spec_data[0], value_type_t<Spec>(1));
        else if (spec_size == 2u)
            return make_span(
                spec_data[0], spec_data[1], value_type_t<Spec>(1));
        else
            return make_span(spec_data[0], spec_data[1], spec_data[2]);
    }
    else
    {
        static_assert(always_false_v<Spec>, WL_ERROR_TAKE_SPEC_TYPE);
    }
}
template<typename Spec>
auto _drop_get_spec(const Spec& spec)
{
    if constexpr (std::is_same_v<none_type, Spec>)
    {
        return const_all;
    }
    else if constexpr (is_integral_v<Spec>)
    {
        if constexpr (std::is_unsigned_v<Spec>)
            return make_span(complement_span_tag{}, const_all, spec);
        else if (spec > Spec(0))
            return make_span(complement_span_tag{},
                int64_t(1), int64_t(spec));
        else
            return make_span(complement_span_tag{},
                int64_t(spec), int64_t(-1));
    }
    else if constexpr (array_rank_v<Spec> == 1u &&
        is_integral_v<value_type_t<Spec>>)
    {
        const auto spec_size = spec.size();
        if (!(1u <= spec_size && spec_size <= 3u))
            throw std::logic_error(WL_ERROR_TAKE_SPEC_LIST_LENGTH);
        std::array<value_type_t<Spec>, 3u> spec_data;
        spec.copy_to(spec_data.data());
        if (spec_size == 1u)
            return make_span(complement_span_tag{},
                spec_data[0], spec_data[0], value_type_t<Spec>(1));
        else if (spec_size == 2u)
            return make_span(complement_span_tag{},
                spec_data[0], spec_data[1], value_type_t<Spec>(1));
        else
            return make_span(complement_span_tag{},
                spec_data[0], spec_data[1], spec_data[2]);
    }
    else
    {
        static_assert(always_false_v<Spec>, WL_ERROR_TAKE_SPEC_TYPE);
    }
}
template<typename X, typename... Specs>
auto take(const X& x, const Specs&... specs)
{
    WL_TRY_BEGIN()
    return val(part(x, _take_get_spec(specs)...));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename... Specs>
auto drop(const X& x, const Specs&... specs)
{
    WL_TRY_BEGIN()
    return val(part(x, _drop_get_spec(specs)...));
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<size_t I, typename XV, size_t Level, size_t RR>
void _partition_copy_single(XV*& WL_RESTRICT dst, const XV* WL_RESTRICT src,
    const std::array<size_t, RR>& ret_dims,
    const std::array<size_t, Level>& strides)
{
    if constexpr (I == 0u)
    {
        WL_THROW_IF_ABORT()
    }
    if constexpr (I + 1u == Level)
    {
        const auto size = strides[Level - 1u];
        utils::restrict_copy_n(src, size, dst, no_check_abort_tag{});
        dst += size;
    }
    else if constexpr (I + 1u < Level)
    {
        const auto size = int64_t(ret_dims[Level + I]);
        for (int64_t i = 0; i < size; ++i)
            _partition_copy_single<I + 1u>(
                dst, src + i * strides[I], ret_dims, strides);
    }
    else
    {
        static_assert(always_false_v<XV>, WL_ERROR_INTERNAL);
    }
}
template<size_t I, typename XV, size_t Level, size_t RR>
void _partition_copy_batch(XV*& WL_RESTRICT dst, const XV* WL_RESTRICT src,
    const std::array<size_t, RR>& ret_dims,
    const std::array<size_t, Level>& strides,
    const std::array<size_t, Level>& d_strides)
{
    const auto size = ret_dims[I];
    for (size_t i = 0; i < size; ++i)
    {
        if constexpr (I + 1u == Level)
            _partition_copy_single<0u>(
                dst, src + i * d_strides[I], ret_dims, strides);
        else if constexpr (I + 1u < Level)
            _partition_copy_batch<I + 1u>(
                dst, src + i * d_strides[I], ret_dims, strides, d_strides);
        else
            static_assert(always_false_v<XV>, WL_ERROR_INTERNAL);
    }
}
template<typename XV, size_t XR, size_t Level>
auto _partition_impl(const XV* const x_data, std::array<size_t, XR> x_dims,
    std::array<size_t, Level> n, std::array<size_t, Level> d)
{
    // Max[0,Floor[(s+d-n)/d]]
    static_assert(1u <= Level && Level <= XR, WL_ERROR_INTERNAL);
    constexpr auto RR = XR + Level;
    std::array<size_t, RR> ret_dims;
    for (size_t i = 0; i < Level; ++i)
    {
        int64_t m = (x_dims[i] + d[i] - n[i]) / d[i];
        if (m <= 0)
            m = 0;
        ret_dims[i] = size_t(m);
        ret_dims[Level + i] = n[i];
    }
    for (size_t i = Level; i < XR; ++i)
        ret_dims[Level + i] = x_dims[i];
    ndarray<XV, RR> ret(ret_dims);
    if (ret.size() == 0u)
        return ret;
    std::array<size_t, Level> strides;
    std::array<size_t, Level> d_strides;
    size_t stride = 1u;
    for (int64_t i = XR - 1; i >= int64_t(Level); --i)
    {
        stride *= x_dims[i];
    }
    for (int64_t i = int64_t(Level) - 1; i >= 0; --i)
    {
        strides[i] = stride;
        d_strides[i] = stride * d[i];
        stride *= x_dims[i];
    }
    strides[Level - 1u] *= ret_dims[2u * Level - 1u];
    auto* ret_data = ret.data();
    _partition_copy_batch<0u>(ret_data, x_data, ret_dims, strides, d_strides);
    return ret;
}
template<size_t Level, typename Spec>
auto _partition_get_spec(const Spec& spec)
{
    std::array<size_t, Level> spec_array;
    if constexpr (array_rank_v<Spec> == 1u)
    {
        static_assert(is_integral_v<value_type_t<Spec>>,
            WL_ERROR_PARTITION_SPEC);
        if (spec.size() != Level)
            throw std::logic_error(WL_ERROR_PARTITION_SPEC_LENGTH);
        if (!all_true(spec,
            [](const auto& a) { return boolean(a > decltype(a)(0)); }))
            throw std::logic_error(WL_ERROR_PARTITION_NEGATIVE_SPEC);
        spec.copy_to(spec_array.data());
    }
    else
    {
        static_assert(array_rank_v<Spec> == 0u && is_integral_v<Spec>,
            WL_ERROR_PARTITION_SPEC);
        if (spec <= Spec(0))
            throw std::logic_error(WL_ERROR_PARTITION_NEGATIVE_SPEC);
        spec_array.fill(spec);
    }
    return spec_array;
}
template<typename X, int64_t I, typename N, typename D>
auto partition(const X& x, const_int<I>, const N& n, const D& d)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    constexpr auto Level = size_t(I);
    static_assert(1u <= I && I <= XR, WL_ERROR_PARTITION_LEVEL);
    const auto& valx = allows<view_category::Simple>(x);
    const auto n_array = _partition_get_spec<Level>(n);
    const auto d_array = _partition_get_spec<Level>(d);
    return _partition_impl(valx.data(), valx.dims(), n_array, d_array);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, int64_t I, typename N>
auto partition(const X& x, const_int<I>, const N& n)
{
    return partition(x, const_int<I>{}, n, n);
}
template<typename X, typename N, typename D>
auto partition(const X& x, const N& n, const D& d)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    if constexpr (array_rank_v<N> == 0u && array_rank_v<D> == 0u)
    {
        return partition(x, const_int<1>{}, n, d);
    }
    else
    {
        constexpr auto Level = XR;
        if constexpr (array_rank_v<N> == 1u)
            if (n.size() != XR)
                throw std::logic_error(WL_ERROR_PARTITION_DEFAULT_LEVEL);
        if constexpr (array_rank_v<D> == 1u)
            if (d.size() != XR)
                throw std::logic_error(WL_ERROR_PARTITION_DEFAULT_LEVEL);
        return partition(x, const_int<int64_t(XR)>{}, n, d);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename N>
auto partition(const X& x, const N& n)
{
    return partition(x, n, n);
}
}
namespace wl
{
template<typename F>
struct function;
template<typename Ret, typename... Args>
struct function<Ret(Args...)>
{
    std::function<Ret(const Args&...)> f_;
    function()
    {
        f_ = [](const Args&...) { return Ret{}; };
    }
    template<typename F>
    explicit function(F f)
    {
        static_assert(std::is_same_v<Ret, remove_cvref_t<decltype(
            f(std::declval<const Args&>()...))>>);
        f_ = f;
    }
    template<typename F>
    auto& operator=(F f)
    {
        static_assert(std::is_same_v<Ret, remove_cvref_t<decltype(
            f(std::declval<const Args&>()...))>>);
        f_ = f;
        return *this;
    }
    template<typename... Any>
    auto operator()(Any&&... any) -> decltype(auto)
    {
        static_assert(sizeof...(Any) == sizeof...(Args));
        static_assert((std::is_same_v<remove_cvref_t<Any>, Args> && ...));
        return f_(std::forward<decltype(any)>(any)...);
    }
};
template<typename ArgIter, bool HasStride>
struct argument_pack
{
    using value_type = remove_cvref_t<decltype(*std::declval<ArgIter>())>;
    const ArgIter iter_;
    const size_t size_;
    const size_t stride_;
    argument_pack(ArgIter iter, size_t size, size_t stride = 1u) :
        iter_{iter}, size_{size}, stride_{stride}
    {
    }
    size_t size() const
    {
        return this->size_;
    }
    auto get(size_t i) const
    {
        if (i >= size_)
            throw std::logic_error(WL_ERROR_ARGPACK_OUT_OF_RANGE);
        if constexpr (HasStride)
            return *(this->iter_ + i * stride_);
        else
            return *(this->iter_ + i);
    }
    auto get(size_t i, dim_checked) const
    {
        if constexpr (HasStride)
            return *(this->iter_ + i * stride_);
        else
            return *(this->iter_ + i);
    }
    auto get_pack(size_t i) const
    {
        if (i > size_)
            throw std::logic_error(WL_ERROR_ARGPACK_OUT_OF_RANGE);
        if constexpr (HasStride)
            return argument_pack(iter_ + i * stride_, size_ - i, stride_);
        else
            return argument_pack(iter_ + i, size_ - i);
    }
};
template<typename Normal, typename Variadic>
struct variadic
{
    Normal nf_;
    Variadic vf_;
    variadic(Normal nf, Variadic vf) :
        nf_{std::move(nf)}, vf_{std::move(vf)}
    {
    }
    template<typename... Args>
    auto operator()(Args&&... args) const -> decltype(auto)
    {
        return nf_(std::forward<decltype(args)>(args)...);
    }
    template<typename Arg, typename = std::enable_if_t<
        std::is_invocable_v<Variadic, Arg&&> ||
        !is_argument_pack_v<remove_cvref_t<Arg>>>>
    auto operator()(Arg&& arg) const -> decltype(auto)
    {
        if constexpr (is_argument_pack_v<remove_cvref_t<Arg>>)
            return vf_(std::forward<decltype(arg)>(arg));
        else
            return nf_(std::forward<decltype(arg)>(arg));
    }
};
template<typename T, typename Args>
struct _tuple_append;
template<typename T, typename... Args>
struct _tuple_append<T, std::tuple<Args...>>
{
    using type = std::tuple<Args..., T>;
};
template<typename T, typename Args>
using _tuple_append_t = typename _tuple_append<T, Args>::type;
template<typename Fn, typename T, typename Args, typename = void>
struct _apply_nargs : _apply_nargs<Fn, T, _tuple_append_t<T, Args>>
{
    static_assert(std::tuple_size_v<Args> <= MaximumArgCount, 
        WL_ERROR_INTERNAL);
};
template<typename Fn, typename T, typename... Args>
struct _apply_nargs<Fn, T, std::tuple<Args...>,
    std::void_t<std::invoke_result_t<Fn, Args...>>> :
    std::integral_constant<size_t, sizeof...(Args)> {};
template<typename Fn, typename T>
constexpr auto _apply_nargs_v = _apply_nargs<Fn, T, std::tuple<>>::value;
template<typename Function, typename Iter, size_t... Is>
auto _apply_fixed_impl(Function f, Iter iter, ptrdiff_t step,
    std::index_sequence<Is...>)
{
    return f(*(iter + step * Is)...);
}
template<typename Function, typename Iter, size_t... Is>
auto _apply_fixed_impl(Function f, Iter iter, std::index_sequence<Is...>)
{
    return f(*(iter + Is)...);
}
template<typename Function, typename X, int64_t I>
auto apply(Function f, const X& x, const_int<I>)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 1u, WL_ERROR_REQUIRE_ARRAY);
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(0 <= Level && Level < int64_t(R), WL_ERROR_BAD_LEVEL);
    const auto& valx = val(std::forward<decltype(x)>(x));
    auto x_iter = valx.template view_begin<Level + 1u>();
    const auto argc = valx.dims()[Level];
    const auto apply_dims = utils::dims_take<1u, Level>(valx.dims());
    if constexpr (is_variadic_function_v<Function>)
    {
        using PackType = argument_pack<decltype(x_iter)>;
        using RT = remove_cvref_t<decltype(f(std::declval<PackType>()))>;
        if constexpr (Level == 0u)
        {
            return f(PackType(x_iter, argc));
        }
        else if constexpr (array_rank_v<RT> == 0u)
        {
            ndarray<RT, Level> ret(apply_dims);
            auto ret_iter = ret.begin();
            auto ret_size = ret.size();
            for (size_t i = 0u; i < ret_size; ++i, ++ret_iter, x_iter += argc)
                *ret_iter = f(PackType(x_iter, argc));
            return ret;
        }
        else
        {
            auto first_item = f(PackType(x_iter, argc));
            const auto item_dims = first_item.dims();
            ndarray<value_type_t<RT>, Level + array_rank_v<RT>> ret(
                utils::dims_join(apply_dims, item_dims));
            auto ret_iter = ret.template view_begin<Level>();
            first_item.copy_to(ret_iter.begin());
            x_iter += argc;
            ++ret_iter;
            for (size_t i = 1; i < utils::size_of_dims(apply_dims);
                ++i, x_iter += argc, ++ret_iter)
            {
                auto item = f(PackType(x_iter, argc));
                if (!utils::check_dims(item.dims(), item_dims))
                    throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
                item.copy_to(ret_iter.begin());
            }
            return ret;
        }
    }
    else
    {
        constexpr auto function_nargs =
            _apply_nargs_v<Function, remove_cvref_t<decltype(*x_iter)>>;
        if (function_nargs > argc)
            throw std::logic_error(WL_ERROR_ARGPACK_OUT_OF_RANGE);
        constexpr auto index_seq = std::make_index_sequence<function_nargs>{};
        using RT = remove_cvref_t<decltype(
            _apply_fixed_impl(f, x_iter, index_seq))>;
        if constexpr (Level == 0u)
        {
            return _apply_fixed_impl(f, x_iter, index_seq);
        }
        else if constexpr (array_rank_v<RT> == 0u)
        {
            ndarray<RT, Level> ret(apply_dims);
            auto ret_iter = ret.begin();
            auto ret_size = ret.size();
            for (size_t i = 0u; i < ret_size; ++i, ++ret_iter, x_iter += argc)
                *ret_iter = _apply_fixed_impl(f, x_iter, index_seq);
            return ret;
        }
        else
        {
            auto first_item = _apply_fixed_impl(f, x_iter, index_seq);
            const auto item_dims = first_item.dims();
            ndarray<value_type_t<RT>, Level + array_rank_v<RT>> ret(
                utils::dims_join(apply_dims, item_dims));
            auto ret_iter = ret.template view_begin<Level>();
            first_item.copy_to(ret_iter.begin());
            x_iter += argc;
            ++ret_iter;
            for (size_t i = 1; i < utils::size_of_dims(apply_dims);
                ++i, x_iter += argc, ++ret_iter)
            {
                auto item = _apply_fixed_impl(f, x_iter, index_seq);
                if (!utils::check_dims(item.dims(), item_dims))
                    throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
                item.copy_to(ret_iter.begin());
            }
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto apply(Function f, X&& x)
{
    WL_TRY_BEGIN()
    return apply(f, std::forward<decltype(x)>(x), const_int<0>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename T, size_t R, typename Function>
auto _select_impl(const ndarray<T, R>& a, Function f)
{
    static_assert(R >= 2u, WL_ERROR_INTERNAL);
    ndarray<T, R> ret;
    auto view_iter = a.template view_begin<1u>();
    auto view_end = a.template view_end<1u>();
    size_t item_size = view_iter.size();
    for (; view_iter != view_end; ++view_iter)
    {
        auto out = f(*view_iter);
        static_assert(is_boolean_v<decltype(out)>, WL_ERROR_PRED_TYPE);
        if (out)
            ret.append(*view_iter);
    }
    return ret;
}
template<typename X, typename Function>
auto select(X&& x, Function f)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<XT>;
    if constexpr (array_rank_v<XT> == 1u)
    {
        std::vector<XV> ret;
        using RT = remove_cvref_t<decltype(f(XV{}))>;
        static_assert(is_boolean_v<RT>, WL_ERROR_PRED_TYPE);
        x.for_each([&](const auto& a) { if (f(a)) ret.push_back(a); });
        return ndarray<value_type_t<XT>, 1u>(
            std::array<size_t, 1u>{ret.size()}, std::move(ret));
    }
    else
        return _select_impl(std::forward<decltype(x)>(x).to_array(), f);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Function, int64_t I>
auto count(const X& x, varg_tag, Function f, const_int<I>)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, WL_ERROR_BAD_LEVEL);
    int64_t item_count = 0;
    if constexpr (XR == Level)
    {
        using RT = remove_cvref_t<decltype(f(value_type_t<X>{}))>;
        static_assert(is_boolean_v<RT>, WL_ERROR_PRED_TYPE);
        x.for_each([&](const auto& a) { if (f(a)) ++item_count; });
    }
    else
    {
        const auto& valx = allows<view_category::Array>(x);
        auto view_iter = valx.template view_begin<Level>();
        const auto view_end = valx.template view_end<Level>();
        using RT = remove_cvref_t<decltype(f(*view_iter))>;
        static_assert(is_boolean_v<RT>, WL_ERROR_PRED_TYPE);
        for (; view_iter != view_end; ++view_iter)
        {
            if (f(*view_iter))
                ++item_count;
        }
    }
    return item_count;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Function>
auto count(const X& x, varg_tag, Function f)
{
    return count(x, varg_tag{}, f, const_int<1>{});
}
template<typename X, typename Y, int64_t I>
auto count(const X& x, const Y& y, const_int<I>)
{
    WL_TRY_BEGIN()
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(1u <= Level && Level + YR == XR, WL_ERROR_BAD_LEVEL);
    int64_t item_count = 0;
    const auto& valy = allows<view_category::Array>(y);
    if constexpr (YR == 0u)
    {
        x.for_each([&](const auto& a) { if (equal(a, valy)) ++item_count; });
    }
    else
    {
        const auto& valx = allows<view_category::Array>(x);
        auto view_iter = valx.template view_begin<Level>();
        const auto view_end = valx.template view_end<Level>();
        for (; view_iter != view_end; ++view_iter)
        {
            if (equal(*view_iter, valy))
                ++item_count;
        }
    }
    return item_count;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y>
auto count(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    return count(x, y, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<size_t Level, typename Function, typename T, size_t R>
auto _map_impl(Function f, const ndarray<T, R>& a)
{
    static_assert(1 <= Level && Level <= R, WL_ERROR_BAD_LEVEL);
    auto a_iter = a.template view_begin<Level>();
    using RT = remove_cvref_t<decltype(f(*a_iter))>;
    const auto map_dims = utils::dims_take<1, Level>(a.dims());
    if constexpr (array_rank_v<RT> == 0u)
    {
        ndarray<RT, Level> ret(map_dims);
        ret.for_each([&](auto& r) { r = f(*a_iter); ++a_iter; });
        return ret;
    }
    else
    {
        auto first_item = f(*a_iter);
        const auto item_dims = first_item.dims();
        ndarray<value_type_t<RT>, Level + array_rank_v<RT>> ret(
            utils::dims_join(map_dims, item_dims));
        auto ret_iter = ret.template view_begin<Level>();
        first_item.copy_to(ret_iter.begin());
        ++a_iter;
        ++ret_iter;
        for (size_t i = 1; i < utils::size_of_dims(map_dims);
            ++i, ++a_iter, ++ret_iter)
        {
            auto item = f(*a_iter);
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
            item.copy_to(ret_iter.begin());
        }
        return ret;
    }
}
template<typename Function, typename X, int64_t I>
auto map(Function f, X&& x, const_int<I>)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 1, WL_ERROR_REQUIRE_ARRAY);
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(1 <= Level && Level <= int64_t(R), WL_ERROR_BAD_LEVEL);
    if constexpr (R == size_t(Level))
    {
        using RT = remove_cvref_t<decltype(f(value_type_t<XT>{}))>;
        if constexpr (array_rank_v<RT> == 0u)
            return utils::listable_function(f, std::forward<decltype(x)>(x));
        else
            return _map_impl<Level>(f, x.to_array());
    }
    else
        return _map_impl<Level>(f, x.to_array());
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto map(Function f, X&& x)
{
    WL_TRY_BEGIN()
    return map(f, std::forward<decltype(x)>(x), const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, int64_t I>
auto scan(Function f, X&& x, const_int<I>)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 1, WL_ERROR_REQUIRE_ARRAY);
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(1 <= Level && Level <= int64_t(R), WL_ERROR_BAD_LEVEL);
    const auto& valx = val(std::forward<decltype(x)>(x));
    auto x_iter = valx.template view_begin<Level>();
    const auto x_end = valx.template view_end<Level>();
    for (; x_iter != x_end; ++x_iter)
        f(*x_iter);
    return const_null;
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto scan(Function f, X&& x)
{
    WL_TRY_BEGIN()
    return scan(f, std::forward<decltype(x)>(x), const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, size_t R, typename... Iters>
auto _map_thread_impl2(Function f, const std::array<size_t, R>& map_dims,
    Iters... iters)
{
    using RT = remove_cvref_t<decltype(f(*iters...))>;
    if constexpr (array_rank_v<RT> == 0u)
    {
        ndarray<RT, 1u> ret(map_dims);
        const auto ret_size = ret.size();
        auto ret_iter = ret.begin();
        for (size_t i = 0u; i < ret_size; ++i, ++ret_iter, (++iters, ...))
            *ret_iter = f(*iters...);
        return ret;
    }
    else
    {
        auto first_item = f(*iters...);
        const auto item_dims = first_item.dims();
        ndarray<value_type_t<RT>, R + array_rank_v<RT>> ret(
            utils::dims_join(map_dims, item_dims));
        const auto ret_size = utils::size_of_dims(map_dims);
        auto ret_iter = ret.template view_begin<R>();
        first_item.copy_to(ret_iter.begin());
        [[maybe_unused]] const auto& _1 = (++iters, ...);
        ++ret_iter;
        for (size_t i = 1; i < ret_size; ++i, ++ret_iter, (++iters, ...))
        {
            auto item = f(*iters...);
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
            item.copy_to(ret_iter.begin());
        }
        return ret;
    }
}
template<size_t I, typename Function, typename Arg1, typename... Args>
auto _map_thread_impl1(Function f, const Arg1& arg1, const Args&... args)
{
    const auto dims = utils::dims_take<1u, I>(arg1.dims());
    if (!(utils::check_dims(dims, utils::dims_take<1, I>(args.dims())) && ...))
        throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
    return _map_thread_impl2(f, dims, arg1.template view_begin<size_t(I)>(),
        args.template view_begin<size_t(I)>()...);
}
template<typename Function, int64_t I, typename... Args>
auto map_thread(Function f, const_int<I>, varg_tag, Args&&... args)
{
    WL_TRY_BEGIN()
    static_assert(1 <= I, WL_ERROR_BAD_LEVEL);
    static_assert(((array_rank_v<remove_cvref_t<Args>> >= size_t(I)) && ...),
        WL_ERROR_MAP_THREAD_LEVEL);
    if constexpr (sizeof...(Args) == 0u)
    {
        using RT = remove_cvref_t<decltype(f())>;
        constexpr auto RR = array_rank_v<RT>;
        if constexpr (RR > 0u)
            return ndarray<value_type_t<RT>, RR + size_t(I)>{};
        else
            return ndarray<RT, size_t(I)>{};
    }
    else
    {
        return _map_thread_impl1<size_t(I)>(f,
            val(std::forward<decltype(args)>(args))...);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename... Args>
auto map_thread(Function f, varg_tag, Args&&... args)
{
    WL_TRY_BEGIN()
    return map_thread(f, const_int<1>{}, varg_tag{},
        std::forward<decltype(args)>(args)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, int64_t I>
auto map_thread(Function f, X&& x, const_int<I>)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 2u, WL_ERROR_REQUIRE_ARRAY_RANK"two or higher.");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(1 <= Level && Level < int64_t(R), WL_ERROR_BAD_LEVEL);
    const auto& valx = val(std::forward<decltype(x)>(x));
    const auto map_dims = utils::dims_take<2u, Level + 1u>(valx.dims());
    const auto pack_size = valx.dims()[0];
    const auto pack_stride = utils::size_of_dims(map_dims);
    auto x_iter = valx.template view_begin<Level + 1u>();
    if constexpr (is_variadic_function_v<Function>)
    {
        using PackType = argument_pack<decltype(x_iter), true>;
        using RT = remove_cvref_t<decltype(f(std::declval<PackType>()))>;
        if constexpr (array_rank_v<RT> == 0u)
        {
            ndarray<RT, Level> ret(map_dims);
            auto ret_iter = ret.begin();
            const auto ret_size = ret.size();
            for (size_t i = 0; i < ret_size; ++i, ++ret_iter, ++x_iter)
                *ret_iter = f(PackType(x_iter, pack_size, pack_stride));
            return ret;
        }
        else
        {
            auto first_item = f(PackType(x_iter, pack_size, pack_stride));
            const auto item_dims = first_item.dims();
            ndarray<value_type_t<RT>, Level + array_rank_v<RT>> ret(
                utils::dims_join(map_dims, item_dims));
            auto ret_iter = ret.template view_begin<Level>();
            first_item.copy_to(ret_iter.begin());
            ++x_iter;
            ++ret_iter;
            for (size_t i = 1; i < pack_stride; ++i, ++x_iter, ++ret_iter)
            {
                auto item = f(PackType(x_iter, pack_size, pack_stride));
                if (!utils::check_dims(item.dims(), item_dims))
                    throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
                item.copy_to(ret_iter.begin());
            }
            return ret;
        }
    }
    else
    {
        constexpr auto function_nargs =
            _apply_nargs_v<Function, remove_cvref_t<decltype(*x_iter)>>;
        if (function_nargs > pack_size)
            throw std::logic_error(WL_ERROR_ARGPACK_OUT_OF_RANGE);
        constexpr auto index_seq = std::make_index_sequence<function_nargs>{};
        using RT = remove_cvref_t<decltype(
            _apply_fixed_impl(f, x_iter, pack_stride, index_seq))>;
        if constexpr (array_rank_v<RT> == 0u)
        {
            ndarray<RT, Level> ret(map_dims);
            auto ret_iter = ret.begin();
            const auto ret_size = ret.size();
            for (size_t i = 0; i < ret_size; ++i, ++ret_iter, ++x_iter)
                *ret_iter = _apply_fixed_impl(
                    f, x_iter, pack_stride, index_seq);
            return ret;
        }
        else
        {
            auto first_item = _apply_fixed_impl(
                f, x_iter, pack_stride, index_seq);
            const auto item_dims = first_item.dims();
            ndarray<value_type_t<RT>, Level + array_rank_v<RT>> ret(
                utils::dims_join(map_dims, item_dims));
            auto ret_iter = ret.template view_begin<Level>();
            first_item.copy_to(ret_iter.begin());
            ++x_iter;
            ++ret_iter;
            for (size_t i = 1; i < pack_stride; ++i, ++x_iter, ++ret_iter)
            {
                auto item = _apply_fixed_impl(
                    f, x_iter, pack_stride, index_seq);
                if (!utils::check_dims(item.dims(), item_dims))
                    throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
                item.copy_to(ret_iter.begin());
            }
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto map_thread(Function f, X&& x)
{
    WL_TRY_BEGIN()
    return map_thread(f, std::forward<decltype(x)>(x), const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto nest(Function f, X&& x, const int64_t n)
{
    WL_TRY_BEGIN()
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        WL_ERROR_NEST_TYPE);
    constexpr auto rank = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error(WL_ERROR_ITERATION_NEGATIVE);
    if (n == 0)
        return cast<XT2>(std::forward<decltype(x)>(x));
    else // n >= 1
    {
        auto ret = cast<XT2>(f(std::forward<decltype(x)>(x)));
        for (size_t i = 1u; i < size_t(n); ++i)
        {
            if constexpr (rank == 0u)
                ret = cast<XT2>(f(ret));
            else
            {
                auto temp = val(f(std::move(ret)));
                ret = std::move(temp);
            }
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto nest_list(Function f, X&& x, const int64_t n)
{
    WL_TRY_BEGIN()
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        WL_ERROR_NEST_TYPE);
    constexpr auto rank = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error(WL_ERROR_ITERATION_NEGATIVE);
    if constexpr (rank == 0u)
    {
        ndarray<XT2, 1u> ret(std::array<size_t, 1u>{size_t(n) + 1u});
        auto ret_iter = ret.begin();
        *ret_iter = cast<XT2>(std::forward<decltype(x)>(x));
        for (size_t i = 1; i <= size_t(n); ++i, ++ret_iter)
            *(ret_iter + 1) = cast<XT2>(f(*ret_iter));
        return ret;
    }
    else
    {
        using XV2 = value_type_t<XT2>;
        auto item_dims = x.dims();
        auto ret_dims = utils::dims_join(
            std::array<size_t, 1u>{size_t(n) + 1u}, item_dims);
        if (n == 0)
        {
            auto item = cast<XT2>(std::forward<decltype(x)>(x));
            return ndarray<XV2, rank + 1u>(
                ret_dims, std::move(item).data_vector());
        }
        else // n >= 1
        {
            ndarray<XV2, rank + 1u> ret(ret_dims);
            auto view_iter = ret.template view_begin<1u>();
            x.copy_to(view_iter.begin());
            ++view_iter;
            auto item = val(f(std::forward<decltype(x)>(x)));
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
            item.copy_to(view_iter.begin());
            ++view_iter;
            for (size_t i = 2; i <= size_t(n); ++i, ++view_iter)
            {
                auto temp = val(f(std::move(item)));
                item = std::move(temp);
                if (!utils::check_dims(item.dims(), item_dims))
                    throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
                item.copy_to(view_iter.begin());
            }
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename T, size_t R>
struct _nest_while_queue
{
    static constexpr auto _is_scalar = (R == 0u);
    using _elem_t = std::conditional_t<_is_scalar, T, ndarray<T, R>>;
    const size_t size_;
    size_t elements_count_;
    size_t current_;
    std::vector<_elem_t> queue_;
    _nest_while_queue(size_t size) :
        size_{size}, elements_count_{0u}, current_{0u}, queue_(size)
    {
    }
    template<typename X>
    void push(X&& x)
    {
        using XT = remove_cvref_t<X>;
        static_assert(array_rank_v<XT> == R, WL_ERROR_NEST_TYPE);
        ++current_;
        ++elements_count_;
        if (current_ == size_)
            current_ = 0u;
        auto& dst = queue_[current_];
        if constexpr (_is_scalar)
        {
            static_assert(is_convertible_v<XT, T>, WL_ERROR_NEST_TYPE);
            dst = cast<_elem_t>(std::forward<decltype(x)>(x));
        }
        else
        {
            using XV = value_type_t<XT>;
            static_assert(is_convertible_v<XV, T>, WL_ERROR_NEST_TYPE);
            if constexpr (std::is_same_v<XV, T> && is_movable_v<X&&>)
                dst = std::move(x);
            else
            {
                dst.uninitialized_resize(x.dims(), x.size());
                x.copy_to(dst.data());
            }
        }
    }
    auto last() const & -> const auto&
    {
        return queue_[current_];
    }
    auto last() && -> auto&&
    {
        return std::move(queue_[current_]);
    }
    auto get(size_t i) const & -> const auto&
    {
        return queue_.at(current_ - i + (i > current_ ? size_ : size_t(0)));
    }
    auto get(size_t i) && -> auto&&
    {
        if (i >= elements_count_)
            throw std::logic_error(WL_ERROR_NEST_WHILE_OFFSET);
        return std::move(
            queue_.at(current_ - i + (i > current_ ? size_ : size_t(0))));
    }
    template<size_t N, typename Fn, size_t... Is>
    auto _apply_test_impl(Fn&& fn, std::index_sequence<Is...>) const
    {
        auto res = std::forward<decltype(fn)>(fn)(
            queue_[current_ - (N - Is - 1u) +
                ((N - Is - 1u) > current_ ? size_ : size_t(0))]...);
        static_assert(is_boolean_v<decltype(res)>, WL_ERROR_PRED_TYPE);
        return res;
    }
    template<size_t N, typename Fn>
    auto apply_test(Fn&& fn) const
    {
        return _apply_test_impl<N>(std::forward<decltype(fn)>(fn),
            std::make_index_sequence<N>{});
    }
};
template<typename Function, typename X, typename Test, int64_t N>
auto nest_while(Function f, X&& x, Test test, const_int<N>,
    const int64_t input_max = const_int_infinity,
    const int64_t offset = 0u)
{
    WL_TRY_BEGIN()
    static_assert(0 <= N && N <= MaximumArgCount, WL_ERROR_LARGE_ARGC);
    constexpr auto num_args = size_t(N);
    constexpr auto effective_num_args = std::max(num_args, size_t(1));
    const auto history_size = size_t(std::max(int64_t(num_args), -offset + 1));
    const auto max_steps = std::max(input_max, int64_t(0));
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        WL_ERROR_NEST_TYPE);
    constexpr auto XR = array_rank_v<XT0>;
    if (max_steps < int64_t(effective_num_args))
    {
        if (int64_t(max_steps + offset) < 0)
            throw std::logic_error(WL_ERROR_NEST_WHILE_OFFSET);
        return nest(f, std::forward<decltype(x)>(x), max_steps + offset);
    }
    else if (effective_num_args == 1u && offset >= 0)
    {
        auto ret = cast<XT2>(std::forward<decltype(x)>(x));
        bool continue_flag = false;
        if constexpr (num_args == 0u)
            continue_flag = test();
        else if constexpr (num_args == 1u)
            continue_flag = test(ret);
        if (continue_flag)
        {
            WL_CHECK_ABORT_LOOP_BEGIN(max_steps)
                for (auto i = _loop_zero; i < _loop_size; ++i)
                {
                    if constexpr (XR == 0u)
                        ret = cast<XT2>(f(ret));
                    else
                    {
                        auto temp = val(f(std::move(ret)));
                        set(ret, std::move(temp));
                    }
                    if constexpr (num_args == 0u)
                        continue_flag = test();
                    else if constexpr (num_args == 1u)
                        continue_flag = test(ret);
                    if (!continue_flag)
                        goto nest_while_exit_no_queue;
                }
            WL_CHECK_ABORT_LOOP_END()
        }
nest_while_exit_no_queue:
        if (offset == 0u)
            return ret;
        else
            return nest(f, ret, offset);
    }
    else
    {
        using XV = std::conditional_t<XR == 0u, XT2, value_type_t<XT2>>;
        _nest_while_queue<XV, XR> queue(history_size);
        queue.push(std::forward<decltype(x)>(x));
        
        for (int64_t i = 1; i < int64_t(num_args); ++i)
        {
            queue.push(f(queue.last()));
        }
        bool continue_flag = queue.template apply_test<num_args>(test);
        if (continue_flag)
        {
            WL_CHECK_ABORT_LOOP_BEGIN(max_steps - effective_num_args + 1)
                for (auto i = _loop_zero; i < _loop_size; ++i)
                {
                    queue.push(f(queue.last()));
                    continue_flag = queue.template apply_test<num_args>(test);
                    if (!continue_flag)
                        goto nest_while_exit_queue;
                }
            WL_CHECK_ABORT_LOOP_END()
        }
nest_while_exit_queue:
        const auto reverse_offset = int64_t(effective_num_args) - 1 - offset;
        if (offset <= 0)
            return std::move(queue).get(size_t(-offset));
        else
            return nest(f, std::move(queue).last(), offset);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, typename Test>
auto nest_while(Function f, X&& x, Test test)
{
    WL_TRY_BEGIN()
    return nest_while(f, std::forward<decltype(x)>(x), test, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Test, typename Iter, size_t... Is>
auto _nest_while_list_apply_test(Test test, Iter iter,
    std::index_sequence<Is...>)
{
    constexpr auto N = sizeof...(Is);
    return test(*(iter + int64_t(Is - N + 1u))...);
}
template<typename Function, typename X, typename Test, int64_t N>
auto nest_while_list(Function f, X&& x, Test test, const_int<N>,
    const int64_t input_max = const_int_infinity,
    const int64_t offset = 0u)
{
    WL_TRY_BEGIN()
    static_assert(0 <= N && N <= MaximumArgCount, WL_ERROR_LARGE_ARGC);
    const auto max_steps = std::max(input_max, int64_t(0));
    constexpr auto num_args = size_t(N);
    constexpr auto effective_num_args = std::max(num_args, size_t(1));
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        WL_ERROR_NEST_TYPE);
    constexpr auto XR = array_rank_v<XT0>;
    if (max_steps < int64_t(effective_num_args))
    {
        if (int64_t(max_steps + offset) < 0)
            throw std::logic_error(WL_ERROR_NEST_WHILE_LIST_OFFSET);
        return nest_list(f, std::forward<decltype(x)>(x), max_steps + offset);
    }
    else if constexpr (XR == 0u)
    {
        ndarray<XT2, 1u> ret;
        auto last = cast<XT2>(std::forward<decltype(x)>(x));
        ret.append(last);
        int64_t i = 1;
        for (; i < int64_t(num_args); ++i)
        {
            auto temp = f(std::move(last));
            last = std::move(temp);
            ret.append(last);
        }
        bool continue_flag = false;
        do
        {
            WL_THROW_IF_ABORT()
            if constexpr (num_args == 0u)
                continue_flag = test();
            else if constexpr (num_args == 1u)
                continue_flag = test(last);
            else
                continue_flag = _nest_while_list_apply_test(test,
                    ret.begin() + i - 1, std::make_index_sequence<num_args>{});
            if (!continue_flag)
                break;
            if (i++ > max_steps)
                break;
            auto temp = f(std::move(last));
            last = std::move(temp);
            ret.append(last);
        } while (true);
        if (offset > 0)
        {
            for (int64_t i = 0; i < offset; ++i)
            {
                auto temp = f(std::move(last));
                last = std::move(temp);
                ret.append(last);
            }
        }
        else if (offset < 0)
        {
            const auto new_dim0 = int64_t(offset + ret.dims()[0]);
            if (new_dim0 <= 0)
                throw std::logic_error(WL_ERROR_NEST_WHILE_LIST_OFFSET);
            ret.uninitialized_resize(
                std::array<size_t, 1u>{size_t(new_dim0)}, new_dim0);
        }
        return ret;
    }
    else
    {
        using XV = value_type_t<XT2>;
        ndarray<XV, XR + 1u> ret;
        auto last = cast<XT2>(std::forward<decltype(x)>(x));
        const auto item_dims = last.dims();
        ret.append(last);
        int64_t i = 1;
        for (; i < int64_t(num_args); ++i)
        {
            auto temp = val(f(std::move(last)));
            last = std::move(temp);
            if (!utils::check_dims(item_dims, last.dims()))
                throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
            ret.append(last);
        }
        bool continue_flag = false;
        do
        {
            WL_THROW_IF_ABORT()
            if constexpr (num_args == 0u)
                continue_flag = test();
            else if constexpr (false && num_args == 1u)
                continue_flag = test(last);
            else
                continue_flag = _nest_while_list_apply_test(test,
                    ret.template view_begin<1u>() + i - 1,
                    std::make_index_sequence<num_args>{});
            if (!continue_flag)
                break;
            if (i++ > max_steps)
                break;
            auto temp = val(f(std::move(last)));
            last = std::move(temp);
            if (!utils::check_dims(item_dims, last.dims()))
                throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
            ret.append(last);
        } while (true);
        if (offset > 0)
        {
            for (int64_t i = 0; i < offset; ++i)
            {
                auto temp = val(f(std::move(last)));
                last = std::move(temp);
                if (!utils::check_dims(item_dims, last.dims()))
                    throw std::logic_error(WL_ERROR_LIST_ELEM_DIMS);
                ret.append(last);
            }
        }
        else if (offset < 0)
        {
            const auto new_dim0 = int64_t(offset + ret.dims()[0]);
            if (new_dim0 <= 0)
                throw std::logic_error(WL_ERROR_NEST_WHILE_LIST_OFFSET);
            auto new_dims = ret.dims();
            new_dims[0] = size_t(new_dim0);
            ret.uninitialized_resize(new_dims, size_t(new_dim0) * last.size());
        }
        return ret;
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, typename Test>
auto nest_while_list(Function f, X&& x, Test test)
{
    WL_TRY_BEGIN()
    return nest_while_list(
        f, std::forward<decltype(x)>(x), test, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, typename YIter, typename YInc>
auto _fold_single_impl(Function f, X&& x, YIter y_iter, YInc y_inc,
    const size_t n)
{
    auto nest_f = [&](auto&& arg)
    {
        auto item = f(arg, *y_iter);
        y_inc(y_iter);
        return item;
    };
    return nest(nest_f, std::forward<decltype(x)>(x), n);
}
template<typename Function, typename X, typename YIter, typename YInc>
auto _fold_list_impl(Function f, X&& x, YIter y_iter, YInc y_inc,
    const size_t n)
{
    auto nest_f = [&](auto&& arg)
    {
        auto item = f(arg, *y_iter);
        y_inc(y_iter);
        return item;
    };
    return nest_list(nest_f, std::forward<decltype(x)>(x), n);
}
template<bool List, bool FoldL, typename Function, typename Y>
auto _fold_impl1(Function f, Y&& y)
{
    static_assert(array_rank_v<remove_cvref_t<Y>> >= 1u,
        WL_ERROR_REQUIRE_ARRAY);
    const size_t n = y.dims()[0];
    if (n < 1u) throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);
    const auto& valy = val(std::forward<decltype(y)>(y));
    if constexpr (List)
    {
        if constexpr (FoldL)
            return _fold_list_impl(f, *valy.template view_begin<1u>(),
                valy.template view_begin<1u>() + 1,
                [](auto& iter) { ++iter; }, n - 1u);
        else
            return _fold_list_impl(f, *(valy.template view_end<1u>() - 1),
                valy.template view_end<1u>() - 2,
                [](auto& iter) { --iter; }, n - 1u);
    }
    else
    {
        if constexpr (FoldL)
            return _fold_single_impl(f, *valy.template view_begin<1u>(),
                valy.template view_begin<1u>() + 1,
                [](auto& iter) { ++iter; }, n - 1u);
        else
            return _fold_single_impl(f, *(valy.template view_end<1u>() - 1),
                valy.template view_end<1u>() - 2,
                [](auto& iter) { --iter; }, n - 1u);
    }
}
template<bool List, bool FoldL, typename Function, typename X, typename Y>
auto _fold_impl1(Function f, X&& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<YT> >= 1u, WL_ERROR_REQUIRE_ARRAY);
    const auto& valy = val(std::forward<decltype(y)>(y));
    const size_t n = valy.dims()[0];
    if constexpr (List)
    {
        if constexpr (FoldL)
            return _fold_list_impl(f, std::forward<decltype(x)>(x),
                valy.template view_begin<1u>(),
                [](auto& iter) { ++iter; }, n);
        else
            return _fold_list_impl(f, std::forward<decltype(x)>(x),
                valy.template view_end<1u>() - 1,
                [](auto& iter) { --iter; }, n);
    }
    else
    {
        if constexpr (FoldL)
            return _fold_single_impl(f, std::forward<decltype(x)>(x),
                valy.template view_begin<1u>(),
                [](auto& iter) { ++iter; }, n);
        else
            return _fold_single_impl(f, std::forward<decltype(x)>(x),
                valy.template view_end<1u>() - 1,
                [](auto& iter) { --iter; }, n);
    }
}
template<typename Function, typename... Any>
auto fold(Function f, Any&&... any)
{
    WL_TRY_BEGIN()
    return _fold_impl1<false, true>(f, std::forward<decltype(any)>(any)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename... Any>
auto foldr(Function f, Any&&... any)
{
    WL_TRY_BEGIN()
    return _fold_impl1<false, false>(f, std::forward<decltype(any)>(any)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename... Any>
auto fold_list(Function f, Any&&... any)
{
    WL_TRY_BEGIN()
    return _fold_impl1<true, true>(f, std::forward<decltype(any)>(any)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename... Any>
auto foldr_list(Function f, Any&&... any)
{
    WL_TRY_BEGIN()
    return _fold_impl1<true, false>(f, std::forward<decltype(any)>(any)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, typename Pred>
auto fixed_point(Function f, X&& x, const int64_t max, varg_tag, Pred pred)
{
    WL_TRY_BEGIN()
    return nest_while(f, std::forward<decltype(x)>(x),
        [=](const auto& a, const auto& b)
        {
            auto same = pred(a, b);
            static_assert(is_boolean_v<decltype(same)>, WL_ERROR_PRED_TYPE);
            return !same;
        }, const_int<2>{}, max);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, typename Pred>
auto fixed_point(Function f, X&& x, varg_tag, Pred pred)
{
    WL_TRY_BEGIN()
    return fixed_point(f, std::forward<decltype(x)>(x),
        const_int_infinity, varg_tag{}, pred);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto fixed_point(Function f, X&& x, const int64_t max)
{
    WL_TRY_BEGIN()
    return nest_while(f, std::forward<decltype(x)>(x),
        [=](const auto& a, const auto& b) { return unequal(a, b); },
        const_int<2>{}, max);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto fixed_point(Function f, X&& x)
{
    WL_TRY_BEGIN()
    return fixed_point(f, std::forward<decltype(x)>(x), const_int_infinity);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, typename Pred>
auto fixed_point_list(Function f, X&& x, const int64_t max,
    varg_tag, Pred pred)
{
    WL_TRY_BEGIN()
    return nest_while_list(f, std::forward<decltype(x)>(x),
        [=](const auto& a, const auto& b)
        {
            auto same = pred(a, b);
            static_assert(is_boolean_v<decltype(same)>, WL_ERROR_PRED_TYPE);
            return !same;
        }, const_int<2>{}, max);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X, typename Pred>
auto fixed_point_list(Function f, X&& x, varg_tag, Pred pred)
{
    WL_TRY_BEGIN()
    return fixed_point_list(f, std::forward<decltype(x)>(x),
        const_int_infinity, varg_tag{}, pred);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto fixed_point_list(Function f, X&& x, const int64_t max)
{
    WL_TRY_BEGIN()
    return nest_while_list(f, std::forward<decltype(x)>(x),
        [=](const auto& a, const auto& b) { return unequal(a, b); },
        const_int<2>{}, max);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Function, typename X>
auto fixed_point_list(Function f, X&& x)
{
    WL_TRY_BEGIN()
    return fixed_point_list(f, std::forward<decltype(x)>(x),
        const_int_infinity);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename Arg>
auto identity(Arg&& arg) -> decltype(auto)
{
    WL_TRY_BEGIN()
    return val(arg);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<bool Reverse, typename... Fs>
struct composite_function
{
    static constexpr auto N = sizeof...(Fs);
    std::tuple<Fs...> fs_;
    composite_function(Fs... fs) :
        fs_{std::make_tuple(std::move(fs)...)}
    {
    }
    template<typename... Args>
    auto operator()(Args&&... args) const -> decltype(auto)
    {
        return _apply<0u>(std::forward<decltype(args)>(args)...);
    }
    template<size_t I, typename... Args>
    auto _apply(Args&&... args) const -> decltype(auto)
    {
        if constexpr (I + 1u < N)
            return std::get<(Reverse ? (N - 1u - I) : I)>(fs_)(
                _apply<I + 1u>(std::forward<decltype(args)>(args)...));
        else
            return std::get<(Reverse ? (N - 1u - I) : I)>(fs_)(
                std::forward<decltype(args)>(args)...);
    }
};
template<bool Reverse, typename... Fn>
auto _composition_impl(Fn&&... fn)
{
    static constexpr auto N = sizeof...(Fn);
    constexpr auto is_variadic = is_variadic_function_v<
        std::tuple_element_t<N - 1u, std::tuple<remove_cvref_t<Fn>...>>>;
    auto ret = composite_function<Reverse, remove_cvref_t<Fn>...>(
        std::forward<decltype(fn)>(fn)...);
    if constexpr (is_variadic)
        return variadic<decltype(ret), decltype(ret)>(ret, ret);
    else
        return ret;
}
template<bool Reverse>
auto _composition_impl()
{
    return [](auto&& arg) -> decltype(auto)
    {
        return std::forward<decltype(arg)>(arg);
    };
}
template<typename... Fn>
auto composition(Fn&&... fn) -> decltype(auto)
{
    WL_TRY_BEGIN()
    return _composition_impl<false>(std::forward<decltype(fn)>(fn)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename... Fn>
auto right_composition(Fn&&... fn) -> decltype(auto)
{
    WL_TRY_BEGIN()
    return _composition_impl<true>(std::forward<decltype(fn)>(fn)...);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Test, int64_t I>
auto all_true(X&& x, Test test, const_int<I>)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(1 <= I && I <= XR, WL_ERROR_BAD_LEVEL);
    const auto& valx = val(std::forward<decltype(x)>(x));
    auto x_iter = valx.template view_begin<I>();
    static_assert(is_boolean_v<remove_cvref_t<decltype(test(*x_iter))>>,
        WL_ERROR_PRED_TYPE);
    auto ret = true;
    const auto outer_size = utils::size_of_dims<I>(valx.dims().data());
    WL_CHECK_ABORT_LOOP_BEGIN(outer_size)
        for (auto i = _loop_zero; i < _loop_size && ret; ++i, ++x_iter)
            ret = ret && test(*x_iter);
    WL_CHECK_ABORT_LOOP_END()
    return boolean(ret);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Test>
auto all_true(X&& x, Test test)
{
    WL_TRY_BEGIN()
    return all_true(std::forward<decltype(x)>(x), test, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Test, int64_t I>
auto any_true(X&& x, Test test, const_int<I>)
{
    WL_TRY_BEGIN()
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(1 <= I && I <= XR, WL_ERROR_BAD_LEVEL);
    const auto& valx = val(std::forward<decltype(x)>(x));
    auto x_iter = valx.template view_begin<I>();
    static_assert(is_boolean_v<remove_cvref_t<decltype(test(*x_iter))>>,
        WL_ERROR_PRED_TYPE);
    auto ret = false;
    const auto outer_size = utils::size_of_dims<I>(valx.dims().data());
    WL_CHECK_ABORT_LOOP_BEGIN(outer_size)
        for (auto i = _loop_zero; i < _loop_size && !ret; ++i, ++x_iter)
            ret = ret || test(*x_iter);
    WL_CHECK_ABORT_LOOP_END()
    return boolean(ret);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Test>
auto any_true(X&& x, Test test)
{
    WL_TRY_BEGIN()
    return any_true(std::forward<decltype(x)>(x), test, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Test, int64_t I>
auto none_true(X&& x, Test test, const_int<I>)
{
    WL_TRY_BEGIN()
    return !any_true(std::forward<decltype(x)>(x), test, const_int<I>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Test>
auto none_true(X&& x, Test test)
{
    WL_TRY_BEGIN()
    return none_true(std::forward<decltype(x)>(x), test, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Test>
auto vector_q(X&& x, Test test)
{
    WL_TRY_BEGIN()
    if constexpr (array_rank_v<remove_cvref_t<X>> == 0u)
        return const_false;
    else
        return all_true(std::forward<decltype(x)>(x), test, const_int<1>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto vector_q(X&& x)
{
    WL_TRY_BEGIN()
    return boolean(array_rank_v<remove_cvref_t<X>> == 1u);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Test>
auto matrix_q(X&& x, Test test)
{
    WL_TRY_BEGIN()
    if constexpr (array_rank_v<remove_cvref_t<X>> <= 1u)
        return const_false;
    else
        return all_true(std::forward<decltype(x)>(x), test, const_int<2>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto matrix_q(X&& x)
{
    WL_TRY_BEGIN()
    return boolean(array_rank_v<remove_cvref_t<X>> == 2u);
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
template<typename Z, typename X, typename Y>
void _dot_vv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K)
{
    auto z = Z(0);
    for (size_t k = 0u; k < K; ++k)
        z += Z(px[k]) * Z(py[k]);
    *pz += z;
}
template<typename Z, typename X, typename Y>
void _dot_mv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K)
{
    WL_THROW_IF_ABORT()
    for (size_t m = 0u; m < M; ++m)
        _dot_vv(pz + m, px + m * K, py, K);
}
template<typename Z, typename X, typename Y>
auto _dot_vm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const size_t N)
{
    WL_THROW_IF_ABORT()
    for (size_t k = 0u; k < K; k += 1)
    {
        const auto xk = Z(px[k]);
        const auto* WL_RESTRICT pyk = py + k * N;
        for (size_t n = 0u; n < N; ++n)
            pz[n] += xk * Z(pyk[n]);
    }
}
template<typename Z, typename X, typename Y>
auto _dot_mm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, const size_t N)
{
    for (size_t m = 0; m < M; ++m)
        _dot_vm(pz + m * N, px + m * K, py, K, N);
}
template<typename X, typename Y>
auto dot(const X& x, const Y& y)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X> && is_numerical_type_v<Y>,
        WL_ERROR_NUMERIC_ONLY);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR >= 1u && YR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    using XV = value_type_t<X>;
    using YV = value_type_t<Y>;
    using C = common_type_t<XV, YV>;
    WL_THROW_IF_ABORT()
    const auto& valx = allows<view_category::Simple>(x);
    const auto& valy = allows<view_category::Simple>(y);
    const auto* px = valx.data();
    const auto* py = valy.data();
    const auto K = valx.dims()[XR - 1u];
    if (K != valy.dims()[0])
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);
    if constexpr (XR == 1u)
    {
        if constexpr (YR == 1u)
        {
            auto z = C(0);
            _dot_vv(&z, px, py, K);
            return z;
        }
        else
        {
            const auto ret_dims = utils::dims_take<2u, YR>(valy.dims());
            ndarray<C, YR - 1u> ret(ret_dims, C(0));
            const auto N = ret.size();
            auto* pz = ret.data();
            _dot_vm(pz, px, py, K, N);
            return ret;
        }
    }
    else
    {
        if constexpr (YR == 1u)
        {
            const auto ret_dims = utils::dims_take<1u, XR - 1u>(valx.dims());
            ndarray<C, XR - 1u> ret(ret_dims, C(0));
            const auto M = ret.size();
            auto* pz = ret.data();
            _dot_mv(pz, px, py, M, K);
            return ret;
        }
        else
        {
            const auto M_dims = utils::dims_take<1u, XR - 1u>(valx.dims());
            const auto N_dims = utils::dims_take<2u, YR>(valy.dims());
            const auto M = utils::size_of_dims(M_dims);
            const auto N = utils::size_of_dims(N_dims);
            ndarray<C, XR + YR - 2u> ret(
                utils::dims_join(M_dims, N_dims), C(0));
            auto* pz = ret.data();
            _dot_mm(pz, px, py, M, K, N);
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename Y, typename Z, typename... Rest>
auto dot(const X& x, const Y& y, const Z& z, const Rest&... rest)
{
    return dot(dot(x, y), z, rest...);
}
template<typename C, typename X, typename Y, typename F>
void _inner_f(C* WL_RESTRICT pc, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const ptrdiff_t dy, size_t K, F f)
{
    if (dy != 1)
    {
        WL_CHECK_ABORT_LOOP_BEGIN(K)
            for (auto k = _loop_begin; k < _loop_end; ++k)
                pc[k] = f(px[k], py[k * dy]);
        WL_CHECK_ABORT_LOOP_END()
    }
    else
    {
        WL_CHECK_ABORT_LOOP_BEGIN(K)
            for (auto k = _loop_begin; k < _loop_end; ++k)
                pc[k] = f(px[k], py[k]);
        WL_CHECK_ABORT_LOOP_END()
    }
}
template<typename C, typename X, typename Y, typename F, typename G>
auto _inner_vv(const X* WL_RESTRICT px, const Y* WL_RESTRICT py,
    const size_t K, F f, G g)
{
    ndarray<C, 1u> c(std::array<size_t, 1u>{K});
    auto pc = c.data();
    auto pack = argument_pack<C*, false>(pc, K);
    _inner_f(pc, px, py, 1, K, f);
    return g(pack);
}
template<typename C, typename Z, typename X, typename Y, typename F, typename G>
void _inner_mv(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, F f, G g)
{
    ndarray<C, 1u> c(std::array<size_t, 1u>{K});
    auto pc = c.data();
    auto pack = argument_pack<C*, false>(pc, K);
    for (size_t m = 0; m < M; ++m, ++pz, px += K)
    {
        _inner_f(pc, px, py, 1, K, f);
        *pz = g(pack);
    }
}
template<typename C, typename Z, typename X, typename Y, typename F, typename G>
void _inner_vm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const size_t N, F f, G g)
{
    ndarray<C, 1u> c(std::array<size_t, 1u>{K});
    auto pc = c.data();
    auto pack = argument_pack<C*, false>(pc, K);
    for (size_t n = 0; n < N; ++n, ++pz, ++py)
    {
        _inner_f(pc, px, py, N, K, f);
        *pz = g(pack);
    }
}
template<typename C, typename Z, typename X, typename Y, typename F, typename G>
void _inner_mm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t M, const size_t K, const size_t N,
    F f, G g)
{
    ndarray<C, 1u> c(std::array<size_t, 1u>{K});
    auto pc = c.data();
    auto pack = argument_pack<C*, false>(pc, K);
    for (size_t m = 0; m < M; ++m, px += K)
    {
        for (size_t n = 0; n < N; ++n, ++pz)
        {
            _inner_f(pc, px, py + n, N, K, f);
            *pz = g(pack);
        }
    }
}
template<typename F, typename X, typename Y, typename G>
auto inner(F f, const X& x, const Y& y, G g)
{
    WL_TRY_BEGIN()
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR >= 1u && YR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(is_variadic_function_v<G>, WL_ERROR_REQUIRE_VARIADIC);
    using XV = value_type_t<X>;
    using YV = value_type_t<Y>;
    using C = remove_cvref_t<decltype(f(XV{}, YV{}))>;
    using Z = remove_cvref_t<decltype(
        g(std::declval<argument_pack<C*, false>>()))>;
    const auto& valx = allows<view_category::Simple>(x);
    const auto& valy = allows<view_category::Simple>(y);
    const auto* px = valx.data();
    const auto* py = valy.data();
    const auto K = valx.dims()[XR - 1u];
    if (K != valy.dims()[0])
        throw std::logic_error(WL_ERROR_REQUIRE_NON_EMPTY);
    if constexpr (XR == 1u)
    {
        if constexpr (YR == 1u)
        {
            return _inner_vv<C>(px, py, K, f, g);
        }
        else
        {
            const auto ret_dims = utils::dims_take<2u, YR>(valy.dims());
            ndarray<Z, YR - 1u> ret(ret_dims);
            const auto N = ret.size();
            auto* pz = ret.data();
            _inner_vm<C>(pz, px, py, K, N, f, g);
            return ret;
        }
    }
    else
    {
        if constexpr (YR == 1u)
        {
            const auto ret_dims = utils::dims_take<1u, XR - 1u>(valx.dims());
            ndarray<Z, XR - 1u> ret(ret_dims);
            const auto M = ret.size();
            auto* pz = ret.data();
            _inner_mv<C>(pz, px, py, M, K, f, g);
            return ret;
        }
        else
        {
            const auto M_dims = utils::dims_take<1u, XR - 1u>(valx.dims());
            const auto N_dims = utils::dims_take<2u, YR>(valy.dims());
            const auto M = utils::size_of_dims(M_dims);
            const auto N = utils::size_of_dims(N_dims);
            ndarray<Z, XR + YR - 2u> ret(utils::dims_join(M_dims, N_dims));
            auto* pz = ret.data();
            _inner_mm<C>(pz, px, py, M, K, N, f, g);
            return ret;
        }
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<size_t Level, size_t R>
void _tr_dims(size_t& ret_stride, size_t& ret_tr_size,
    const std::array<size_t, R>& x_dims)
{
    static_assert(1 <= Level && Level <= R, WL_ERROR_INTERNAL);
    size_t stride = 0u;
    size_t inner_size = 1u;
    size_t tr_size = size_t(-1);
    for (auto i = ptrdiff_t(Level - 1u); i >= 0; --i)
    {
        const size_t dim = x_dims[i];
        stride += inner_size;
        inner_size *= dim;
        if (dim < tr_size)
            tr_size = dim;
    }
    ret_stride = stride;
    ret_tr_size = tr_size;
}
template<typename X, typename F, int64_t I>
auto tr(const X& x, F f, const_int<I>)
{
    WL_TRY_BEGIN()
    static_assert(is_numerical_type_v<X>, WL_ERROR_NUMERIC_ONLY);
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, WL_ERROR_REQUIRE_ARRAY);
    static_assert(0 < I && I <= int64_t(XR), WL_ERROR_BAD_LEVEL);
    static_assert(is_variadic_function_v<F>, WL_ERROR_REQUIRE_VARIADIC);
    using XV = value_type_t<X>;
    constexpr auto Level = size_t(I);
    if constexpr (Level == XR)
    {
        using RV = remove_cvref_t<decltype(
            f(std::declval<argument_pack<XV*, true>>()))>;
        if (x.size() == 0u)
        {
            return RV();
        }
        else
        {
            const auto& valx = allows<view_category::Simple>(x);
            size_t stride = 0u;
            size_t tr_size = 0u;
            _tr_dims<XR>(stride, tr_size, valx.dims());
            const auto pack = argument_pack<const XV*, true>(
                valx.data(), tr_size, stride);
            return f(pack);
        }
    }
    else
    {
        const auto& valx = allows<view_category::Array>(x);
        auto x_iter = valx.template view_begin<Level>();
        using PackType = argument_pack<decltype(x_iter), true>;
        using RT = remove_cvref_t<decltype(f(std::declval<PackType>()))>;
        size_t stride = 0u;
        size_t tr_size = 0u;
        _tr_dims<Level>(stride, tr_size, valx.dims());
        const auto pack = PackType(x_iter, tr_size, stride);
        return f(pack);
    }
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X, typename F>
auto tr(const X& x, F f)
{
    WL_TRY_BEGIN()
    return tr(x, f, const_int<array_rank_v<X>>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
template<typename X>
auto tr(const X& x)
{
    WL_TRY_BEGIN()
    return tr(x, WL_FUNCTION(plus), const_int<array_rank_v<X>>{});
    WL_TRY_END(__func__, __FILE__, __LINE__)
}
}
namespace wl
{
}
