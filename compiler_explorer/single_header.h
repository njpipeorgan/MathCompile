#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <complex>
#include <type_traits>
#include <string>
#include <tuple>
#include <limits>
#include <immintrin.h>
#include <chrono>
#include <exception>
#include <iterator>
#include <optional>
#include <vector>
#include <cassert>
#include <array>
#include <numeric>
#include <variant>
#include <cmath>
#include <random>
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
using string = std::string;
struct void_type;
struct all_type;
struct boolean;
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
template<typename T>
constexpr auto is_integral_v = std::is_integral_v<T>;
template<typename T>
constexpr auto is_float_v = std::is_floating_point_v<T>;
template<typename T>
constexpr auto is_string_v = std::is_same_v<T, string>;
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
constexpr auto is_numerical_type_v = is_arithmetic_v<value_type_t<T>>;
template<typename T>
constexpr auto is_boolean_type_v = is_boolean_v<value_type_t<T>>;
template<typename T>
constexpr auto is_string_type_v = is_string_v<value_type_t<T>>;
template<typename T>
constexpr auto is_value_type_v = is_arithmetic_v<T> || is_array_v<T> ||
is_array_view_v<T> || is_boolean_v<T> || is_string_v<T> ||
std::is_same_v<T, void_type> || std::is_same_v<T, all_type>;
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
        (is_float_v<T> && is_float_v<U>);
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
template<typename F>
struct is_variadic_function : std::false_type {};
template<typename Normal, typename Variadic>
struct is_variadic_function<variadic<Normal, Variadic>> : std::true_type {};
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
struct varg_tag
{
};
template<size_t>
struct rank_tag
{
};
struct loop_break
{
};
struct dim_checked
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
namespace literal
{
}
}
namespace wl
{
constexpr auto const_null  = void_type{};
constexpr auto const_all   = all_type{};
constexpr auto const_i     = complex<double>(0.f, 1.f);
constexpr auto const_true  = boolean(true);
constexpr auto const_false = boolean(false);
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
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
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
#elif defined(__INTEL_COMPILER)
#  define WL_INLINE __forceinline
#  define WL_IGNORE_DEPENDENCIES __pragma(ivdep)
#  define WL_RESTRICT __restrict
#  pragma warning (disable:1011)
#elif defined(__clang__)
#  define WL_INLINE __attribute__((always_inline))
#  define WL_IGNORE_DEPENDENCIES _Pragma("ivdep")
#  define WL_RESTRICT __restrict__
#elif defined(__GNUC__)
#  define WL_INLINE __attribute__((always_inline))
#  define WL_IGNORE_DEPENDENCIES _Pragma("ivdep")
#  define WL_RESTRICT __restrict__
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
}
namespace wl
{
struct scalar_indexer; // I
struct all_indexer;    // A
struct unit_indexer;   // U
struct step_indexer;   // S
struct list_indexer;   // L
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
template<typename Y, typename XV, size_t XR>
auto cast(ndarray<XV, XR>&& x) -> decltype(auto)
{
    static_assert(is_convertible_v<ndarray<XV, XR>, Y>, "badcast");
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
    static_assert(is_convertible_v<ndarray<XV, XR>, Y>, "badcast");
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
        static_assert(is_convertible_v<ndarray<XV, XR>, Y>, "badcast");
        if constexpr (std::is_same_v<XV, YV>)
            return x.to_array();
        else
        {
            ndarray<YV, XR> ret(x.dims());
            x.copy_to(ret.begin());
            return ret;
        }
    }
    else
    {
        static_assert(is_real_v<X>, "badargtype");
        static_assert(is_convertible_v<X, Y>, "badcast");
        return Y(x);
    }
}
template<typename Y, typename X>
auto cast(const complex<X>& x)
{
    static_assert(is_complex_v<Y>, "badcast");
    return std::complex<value_type_t<Y>>(x);
}
template<typename Y>
auto cast(const boolean& x)
{
    static_assert(is_boolean_v<Y>, "badcast");
    return x;
}
template<typename Y>
auto cast(const std::string& x)
{
    static_assert(is_string_v<Y>, "badcast");
    return x;
}
template<typename Y>
auto cast(std::string&& x)
{
    static_assert(is_string_v<Y>, "badcast");
    return std::move(x);
}
namespace utils
{
inline auto _get_time()
{
    auto now = std::chrono::high_resolution_clock::now();
    return now.time_since_epoch().count();
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
    static_assert(1u <= I1 && 1u <= I2, "internal");
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
        static_assert(1u <= I1 && I1 <= R && 1u <= I2 && I2 <= R, "internal");
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
        if (!((dims[Is] >= 0) && ...))
            throw std::logic_error("baddims");
        return (size_t(dims[Is]) * ...);
    }
}
template<size_t R, typename Dims>
auto size_of_dims(const Dims* dims)
{
    static_assert(is_integral_v<Dims>, "internal");
    if constexpr (R == 0u)
        return size_t(1);
    else
        return _size_of_dims_impl(dims, std::make_index_sequence<R>{});
}
template<size_t R, typename Dims>
auto size_of_dims(const std::array<Dims, R>& dims)
{
    static_assert(is_integral_v<Dims>, "internal");
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
    static_assert(R >= 1u, "internal");
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
    static_assert(R == sizeof...(Is), "internal");
    size_t pos = 0u;
    _linear_position_impl<0u>(dims, pos, is...);
    return pos;
}
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
        static_assert(x_rank == y_rank, "badrank");
        if (!utils::check_dims(x.dims(), y.dims()))
            throw std::logic_error("baddims");
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
    const auto size = args.size();                                  \
    for (size_t i = 1u; i < size; ++i)                              \
        ret = name(std::move(ret), args.get(i));                    \
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
        if (x.size() == 0u)                                         \
            return cast<remove_cvref_t<                             \
                decltype(_variadic_##name(x))>>(name());            \
        else                                                        \
            return _variadic_##name(x);                             \
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
template<typename X>
auto _lzcnt(X x)
{
    static_assert(std::is_unsigned_v<X>, "internal");
#if defined(__LZCNT__)
    return _lzcnt_u64(uint64_t(x));
#elif defined(__POPCNT__)
    uint64_t y = int64_t(x);
    y |= (y >> 1);
    y |= (y >> 2);
    y |= (y >> 4);
    if constexpr (sizeof(X) >= 2) y |= (y >> 8);
    if constexpr (sizeof(X) >= 4) y |= (y >> 16);
    if constexpr (sizeof(X) >= 8) y |= (y >> 32);
    return _mm_popcnt_u64(~y);
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
template<typename XIter, typename YIter>
WL_INLINE void copy_n(XIter x_iter, size_t n, YIter y_iter)
{
    WL_IGNORE_DEPENDENCIES
    for (size_t i = 0u; i < n; ++i, ++x_iter, ++y_iter)
        *y_iter = *x_iter;
}
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
    if constexpr (std::is_same_v<IndexType, cidx>)
    {
        if (idx.value() < dim)
            return idx.value();
        else
            throw std::logic_error("badindex");
    }
    else if constexpr (std::is_unsigned_v<IndexType>)
    {
        if (1u <= idx && idx <= dim)
            return size_t(idx - 1u);
        else
            throw std::logic_error("badindex");
    }
    else
    {
        static_assert(is_integral_v<IndexType>, "internal");
        ptrdiff_t pos_idx = idx >= 0 ?
            idx : idx + ptrdiff_t(dim) + 1;
        if (1 <= pos_idx && pos_idx <= ptrdiff_t(dim))
            return size_t(pos_idx - 1u);
        else
            throw std::logic_error("badindex");
    }
    return 0u;
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
    static_assert(default_begin || is_integral_v<Begin>, "badargtype");
    static_assert(default_end || is_integral_v<End>, "badargtype");
    static_assert(default_step || is_integral_v<Step>, "badargtype");
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
                //check out-of-bound
                if (begin < 1 || begin > ptrdiff_t(dim))
                    throw std::logic_error("badindex");
            }
            if constexpr (!default_end)
            {
                if constexpr (std::is_unsigned_v<End>)
                    end = ptrdiff_t(this->end_);
                else if (this->end_ >= 0)
                    end = ptrdiff_t(this->end_);
                else // this->end_ < 0
                    end = ptrdiff_t(this->end_) + ptrdiff_t(dim + 1u);
                //check out-of-bound
                if (end < 1 || end > ptrdiff_t(dim))
                    throw std::logic_error("badindex");
            }
            if constexpr (!default_step)
            {
                step = step_;
                if (step == 0)
                    throw std::logic_error("badvalue");
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
auto make_indexer(const span<Begin, End, Step>& s, size_t dim)
{
    return s.to_indexer(dim);
}
template<typename IndexType, size_t Rank>
auto make_indexer(const ndarray<IndexType, Rank>& list, size_t dim)
{
    static_assert(Rank == 1u, "badargtype");
    static_assert(is_integral_v<IndexType>, "badidxtype");
    return list_indexer(list.begin(), list.end(), dim);
}
template<typename IndexType>
auto make_indexer(const IndexType& index, size_t dim)
{
    if constexpr (std::is_same_v<IndexType, all_type>)
        return all_indexer(dim);
    else if constexpr (std::is_same_v<IndexType, cidx>)
        return scalar_indexer(index, dim);
    else
    {
        static_assert(is_integral_v<IndexType>, "badidxtype");
        return scalar_indexer(index, dim);
    }
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
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        this->_initialize<0u>(specs...);
        this->data_ = base_data + 
            utils::linear_position(base_dims, specs.offset()...);
    }
    template<size_t Level, typename Spec1, typename... Specs>
    void _initialize(const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level < ArrayRank - ViewRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->_initialize<Level + 1u>(specs...);
        }
        else
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>, "internal");
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
        return ptrdiff_t(other.pointer_ - this->pointer_) / this->stride_;
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
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
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
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->_initialize<Level + 1>(dims, specs...);
        }
        else if constexpr (Level < ArrayRank - StrideRank)
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>, "internal");
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
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
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
    static_assert(ViewRank == std::tuple_size_v<IndexersTuple>, "internal");
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
    static constexpr auto _has_last_stride = (StrideRank != 0) ||
        std::is_same_v<
        std::tuple_element_t<ViewRank - 1u, IndexersTuple>,
        step_indexer>;
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
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
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
            if constexpr (std::is_same_v<remove_cvref_t<Spec1>, step_indexer>)
                this->strides_[ViewLevel] = spec1.stride();
            else
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
                for (const auto& i : indexer.indices())
                {
                    _for_each_impl<ViewLevel + 1u>(
                        index + i, break_flag, f, iters...);
                    if (check_break && break_flag)
                        break;
                }
            else
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                {
                    _for_each_impl<ViewLevel + 1u>(
                        index + i, break_flag, f, iters...);
                    if (check_break && break_flag)
                        break;
                }
        }
        else
        {
            const auto last_stride = _has_last_stride ? 
                this->strides_[ViewLevel] : ptrdiff_t(1);
            if constexpr (std::is_same_v<Indexer, list_indexer>)
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
            else
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
template<typename Any>
auto val(Any&& any) -> decltype(auto)
{
    if constexpr (is_array_view_v<remove_cvref_t<Any>>)
        return std::forward<decltype(any)>(any).to_array();
    else
        return std::forward<decltype(any)>(any);
}
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
        static_assert(View::category == view_category::Regular, "internal");
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
#define WL_SMALL_ARRAY_SIZE 1024
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
        if (!is_static_)
            data_.dynamic_.~dynamic_t();
    }
    explicit _small_vector(size_t size) : size_{size}
    {
        bool is_static = (size_ <= N);
        if (is_static)
            std::uninitialized_default_construct_n(static_begin(), size);
        else
            new(&data_.dynamic_) dynamic_t(size);
        this->is_static_ = is_static;
    }
    _small_vector(size_t size, const T& val) : size_{size}
    {
        bool is_static = (size_ <= N);
        if (is_static)
            std::uninitialized_fill_n(static_begin(), size, val);
        else
            new(&data_.dynamic_) dynamic_t(size, val);
        this->is_static_ = is_static;
    }
    template<typename FwdIter>
    _small_vector(FwdIter begin, FwdIter end) : size_{size_t(end - begin)}
    {
        bool is_static = (size_ <= N);
        if (is_static)
            std::uninitialized_copy_n(begin, size_, static_begin());
        else
            new(&data_.dynamic_) dynamic_t(begin, end);
        this->is_static_ = is_static;
    }
    explicit _small_vector(const dynamic_t& other) : size_{other.size_}
    {
        bool is_static = (size_ <= N);
        if (is_static)
            std::uninitialized_copy(
                other.begin(), other.end(), static_begin());
        else
            new(&data_.dynamic_) dynamic_t(other);
        this->is_static_ = is_static;
    }
    explicit _small_vector(dynamic_t&& other) : size_{other.size_}
    {
        new(&data_.dynamic_) dynamic_t(std::move(other));
        this->is_static_ = false;
    }
    _small_vector(const _small_vector& other) : size_{other.size_}
    {
        if (other.size_ <= N)
        {
            std::uninitialized_copy(
                other.begin(), other.end(), this->static_begin());
            this->is_static_ = true;
        }
        else
        {
            new(&data_.dynamic_) dynamic_t(other.data_.dynamic_);
            this->is_static_ = false;
        }
    }
    _small_vector(_small_vector&& other) : size_{other.size_}
    {
        if (other.is_static_)
            std::uninitialized_move(
                other.static_begin(), other.static_end(), static_begin());
        else
            new(&data_.dynamic_) dynamic_t(std::move(other.data_.dynamic_));
        this->is_static_ = other.is_static_;
    }
    _small_vector& operator=(const _small_vector& other)
    {
        this->destory();
        if (other.size_ <= N)
        {
            std::uninitialized_copy(
                other.begin(), other.end(), this->static_begin());
            this->is_static_ = true;
        }
        else
        {
            new(&data_.dynamic_) dynamic_t(other.data_.dynamic_);
            this->is_static_ = false;
        }
        this->size_ = other.size_;
        return *this;
    }
    _small_vector& operator=(_small_vector&& other)
    {
        this->destory();
        if (other.is_static_)
            std::uninitialized_move(
                other.static_begin(), other.static_end(), static_begin());
        else
            new(&data_.dynamic_) dynamic_t(std::move(other.data_.dynamic_));
        this->is_static_ = other.is_static_;
        this->size_ = other.size_;
        return *this;
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
    void destroy_static() { std::destroy(static_begin(), static_end()); }
    void destroy_dynamic() { data_.dynamic_.~dynamic_t(); }
    void destroy()
    {
        if (is_static_)
            destroy_static();
        else
            destroy_dynamic();
    }
    //void to_dynamic()
    //{
    //    if (is_static_)
    //    {
    //        if constexpr (std::is_trivially_copyable_v<T>)
    //        {
    //            dynamic_t new_data(static_begin(), static_end());
    //            new(&data_.dynamic_) dynamic_t(std::move(new_data));
    //        }
    //        else
    //        {
    //            dynamic_t new_data(size_);
    //            std::move(static_begin(), static_end(), new_data.data());
    //            std::destroy(static_begin(), static_end());
    //            new(&data_.dynamic_) dynamic_t(std::move(new_data));
    //        }
    //        is_static_ = false;
    //    }
    //}
    void resize(size_t new_size)
    {
        const auto prev_size = size_;
        if (!is_static_)
        {
            data_.dynamic_.resize(new_size);
            if (new_size < prev_size)
                data_.dynamic_.shrink_to_fit();
        }
        else if (new_size > N)
        {
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
    static_assert(1u <= R && R <= MaximumArrayRank, "badrank");
    static_assert(std::is_same_v<T, remove_cvref_t<T>>, "internal");
    static_assert(is_arithmetic_v<T> || is_boolean_v<T> || is_string_v<T>,
        "badargtype");
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
    ndarray(const std::array<DimsT, rank>& dims, const T& val) :
        data_(utils::size_of_dims(dims), val)
    {
        std::copy(dims.begin(), dims.end(), this->dims_.data());
    }
    template<typename DimsT>
    ndarray(const std::array<DimsT, rank>& dims) :
        data_(utils::size_of_dims(dims))
    {
        std::copy(dims.begin(), dims.end(), this->dims_.data());
    }
    template<typename FwdIter>
    ndarray(std::array<size_t, rank> dims, FwdIter begin, FwdIter end) :
        dims_{dims}, data_(begin, end)
    {
    }
    ndarray(std::array<size_t, rank> dims, _data_t&& movable) :
        dims_{dims}, data_(std::move(movable))
    {
    }
    ndarray(std::array<size_t, rank> dims, std::vector<T>&& movable) :
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
        static_assert(Level <= R, "internal");
        return utils::size_of_dims<R - Level>(this->dims_.data());
    }
    // uses one-based indexing
    template<size_t Level>
    size_t dimension() const
    {
        static_assert(1 <= Level && Level <= R, "internal");
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
        static_assert(Level <= R, "internal");
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
        static_assert(Level <= R, "internal");
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
            this->dims_ = utils::dims_join(
                std::array<size_t, 1u>{1u}, x.dims());
            this->data_ = cast<ndarray<T, R - 1u>>(
                std::forward<decltype(x)>(x)).data_vector();
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
        static_assert(array_rank_v<remove_cvref_t<X>> == R - 1u, "badrank");
        if constexpr (R > 1u)
        {
            if (this->size() > 0u && !utils::check_dims<R - 1u>(
                this->dims_ptr() + 1, x.dims().data()))
                throw std::logic_error("baddims");
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
template<typename X>
auto n(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto x_rank = array_rank_v<XT>;
    if constexpr (x_rank == 0)
    {
        static_assert(is_arithmetic_v<XT>, "badargtype");
        if constexpr (is_integral_v<XT>)
            return double(x);
        else
            return x;
    }
    else
    {
        using XV = value_type_t<XT>;
        if constexpr (is_integral_v<XV>)
        {
            ndarray<decltype(n(XV{})), x_rank> ret(x.dims());
            x.for_each(
                [](const auto& src, auto& dst) { dst = n(src); },
                ret.begin());
            return ret;
        }
        else
            return std::forward<decltype(x)>(x);
    }
}
#define WL_DEFINE_ROUNDING_FUNCTION(name, stdname)                      \
template<typename X>                                                    \
auto name(X&& x)                                                        \
{                                                                       \
    using XT = remove_cvref_t<X>;                                       \
    constexpr auto x_rank = array_rank_v<XT>;                           \
    if constexpr (x_rank == 0)                                          \
    {                                                                   \
        static_assert(is_arithmetic_v<XT>, "badargtype");               \
        if constexpr (is_integral_v<XT>)                                \
            return x;                                                   \
        if constexpr (is_float_v<XT>)                                   \
            return int64_t(std::stdname(x));                            \
        else if constexpr (is_complex_v<XT>)                            \
            return XT(name(std::real(x)), name(std::imag(x)));          \
    }                                                                   \
    else                                                                \
    {                                                                   \
        using XVT = typename XT::value_type;                            \
        if constexpr (is_integral_v<XVT>)                               \
            return std::forward<decltype(x)>(x);                        \
        else                                                            \
        {                                                               \
            ndarray<decltype(name(XVT{})), x_rank> ret(x.dims());       \
            x.for_each(                                                 \
                [](const auto& src, auto& dst) { dst = name(src); },    \
                ret.begin());                                           \
            return ret;                                                 \
        }                                                               \
    }                                                                   \
}
WL_DEFINE_ROUNDING_FUNCTION(round, round)
WL_DEFINE_ROUNDING_FUNCTION(ceiling, ceil)
WL_DEFINE_ROUNDING_FUNCTION(floor, floor)
WL_DEFINE_ROUNDING_FUNCTION(integer_part, trunc)
template<typename X>
auto fractional_part(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto x_rank = array_rank_v<XT>;
    if constexpr (x_rank == 0)
    {
        static_assert(is_arithmetic_v<XT>, "badargtype");
        if constexpr (is_integral_v<XT>)
            return double(0);
        else if constexpr (is_float_v<XT>)
            return x - std::trunc(x);
        else
            return XT(fractional_part(x.real()), fractional_part(x.imag()));
    }
    else
    {
        using XVT = typename XT::value_type;
        if constexpr (is_integral_v<XVT>)
        {
            return ndarray<double, x_rank>(x.dims());
        }
        else if constexpr (is_movable_v<X&&>)
        { // movable
            ndarray<XVT, x_rank> ret(std::move(x));
            ret.for_each([](auto& src) { src = fractional_part(src); });
            return ret;
        }
        else
        {
            ndarray<XVT, x_rank> ret(x.dims());
            x.for_each(
                [](const auto& src, auto& dst) { dst = fractional_part(src); },
                ret.begin());
            return ret;
        }
    }
}
template<typename X>
auto abs(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    if constexpr (std::is_unsigned_v<XV>)
        return val(std::forward<decltype(x)>(x));
    else
        return utils::listable_function([](auto x) { return std::abs(x); },
            std::forward<decltype(x)>(x));
}
template<typename X>
auto ramp(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    if constexpr (std::is_unsigned_v<XV>)
        return val(std::forward<decltype(x)>(x));
    else
    {
        auto pure = [](auto x)
        {
            using XV = decltype(x);
            return x >= XV(0) ? x : XV(0);
        };
        return utils::listable_function(pure, std::forward<decltype(x)>(x));
    }
}
template<typename X, typename Y>
boolean greater(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return boolean(x > y);
}
template<typename X, typename Y>
boolean less(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return boolean(x < y);
}
template<typename X, typename Y>
boolean greater_equal(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return boolean(x >= y);
}
template<typename X, typename Y>
boolean less_equal(const X& x, const Y& y)
{
    static_assert(is_real_v<X> && is_real_v<Y>, "badargtype");
    return boolean(x <= y);
}
template<typename X, typename Y>
boolean equal(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR == YR, "badrank");
    if constexpr (XR == 0u)
    {
        static_assert((is_arithmetic_v<X> && is_arithmetic_v<X>) ||
            (is_value_type_v<X> && std::is_same_v<X, Y>), "badargtype");
        if constexpr (is_complex_v<X>)
            return boolean(x == cast<X>(y));
        else if constexpr (is_complex_v<Y>)
            return boolean(cast<Y>(x) == y);
        else
            return boolean(x == y);
    }
    else
    {
        if (!utils::check_dims(x.dims(), y.dims()))
            return const_false;
        if constexpr (X::category != view_category::General)
        {
            auto equal_flag = true;
            y.for_each([&](const auto& a, const auto& b)
                {
                    equal_flag = equal(a, b);
                    return !equal_flag;
                }, x.begin());
            return boolean(equal_flag);
        }
        else if constexpr (Y::category != view_category::General)
        {
            return equal(y, x);
        }
        else
        {
            if constexpr (sizeof(value_type_t<X>) < sizeof(value_type_t<Y>))
                return equal(x.to_array(), y);
            else
                return equal(x, y.to_array());
        }
    }
}
template<typename X, typename Y>
boolean unequal(const X& x, const Y& y)
{
    return !equal(x, y);
}
template<typename X, typename Y>
boolean same_q(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    constexpr auto same_type = (XR == 0u) ? std::is_same_v<X, Y> :
        std::is_same_v<value_type_t<X>, value_type_t<Y>>;
    if constexpr (XR == YR && same_type)
        return equal(x, y);
    else
        return const_false;
}
template<typename X, typename Y>
boolean unsame_q(const X& x, const Y& y)
{
    return !same_q(x, y);
}
template<typename X, typename Y>
auto mod(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, "badargtype");
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
}
template<typename X, typename Y>
auto quotient(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, "badargtype");
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
}
template<typename Ret = int64_t, typename X>
auto sign(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
        {
            return x / std::abs(x);
        }
        else
        {
            if (x == XV(0))
                return Ret(0);
            else if (x > XV(0))
                return Ret(1);
            else
                return Ret(-1);
        }
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename Ret = int64_t, typename X, typename Y, typename N>
auto integer_digits(const X& x, const Y& y, const N& n)
{
    static_assert(is_integral_v<X> && is_integral_v<Y>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    constexpr auto fixed_length = !std::is_same_v<N, void_type>;
    if (y < Y(0)) throw std::logic_error("badargv");
    auto ux = (x >= X(0)) ? uint64_t(x) : uint64_t(-x);
    auto uy = uint64_t(y);
    ndarray<Ret, 1u> ret;
    if constexpr (fixed_length)
    {
        static_assert(is_integral_v<N>, "badargtype");
        if (n < N(0)) throw std::logic_error("badargv");
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
                for (; ret_iter != ret_end; ++ret_iter)
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
        auto ret_begin = ret.data();
        auto ret_end = ret_begin + ret.size();
        std::reverse(ret_begin, ret_end);
    }
    return ret;
}
template<typename Ret = int64_t, typename X, typename Y>
auto integer_digits(const X& x, const Y& y)
{
    return integer_digits<Ret>(x, y, const_null);
}
template<typename Ret = int64_t, typename X>
auto integer_digits(const X& x)
{
    return integer_digits<Ret>(x, uint64_t(10), const_null);
}
template<typename X>
auto positive(const X& x)
{
    static_assert(is_numerical_type_v<X>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        return boolean(x > XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X>
auto negative(const X& x)
{
    static_assert(is_numerical_type_v<X>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        return boolean(x < XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X>
auto non_positive(const X& x)
{
    static_assert(is_numerical_type_v<X>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        return boolean(x <= XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X>
auto non_negative(const X& x)
{
    static_assert(is_numerical_type_v<X>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        return boolean(x >= XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X, typename L>
auto clip(X&& x, const L& limit)
{
    static_assert(array_rank_v<L> == 1, "badargtype");
    if (limit.size() != 2u) throw std::logic_error("baddims");
    std::array<value_type_t<L>, 2u> limit_pair;
    limit.copy_to(limit_pair.data());
    return clip(std::forward<decltype(x)>(x), varg_tag{},
        limit_pair[0], limit_pair[1]);
}
template<typename X, typename L, typename VL>
auto clip(X&& x, const L& limit, const VL& vlimit)
{
    static_assert(array_rank_v<L> == 1, "badargtype");
    static_assert(array_rank_v<VL> == 1, "badargtype");
    if (limit.size() != 2u) throw std::logic_error("baddims");
    if (vlimit.size() != 2u) throw std::logic_error("baddims");
    std::array<value_type_t<L>, 2u> limit_pair;
    std::array<value_type_t<VL>, 2u> vlimit_pair;
    limit.copy_to(limit_pair.data());
    vlimit.copy_to(vlimit_pair.data());
    return clip(std::forward<decltype(x)>(x), varg_tag{},
        limit_pair[0], limit_pair[1], vlimit_pair[0], vlimit_pair[1]);
}
template<typename X>
auto clip(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        if (x < XV(0))
            return XV(0);
        else if (x > XV(1))
            return XV(1);
        else
            return x;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X, typename Min, typename Max>
auto clip(X&& x, varg_tag, Min min, Max max)
{
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    static_assert(is_real_v<Min> && is_real_v<Max>, "badargtype");
    using C = common_type_t<value_type<XT>, Min, Max>;
    const auto cmin = cast<C>(min);
    const auto cmax = cast<C>(max);
    auto pure = [=](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        auto cx = cast<C>(x);
        if (cx < cmin)
            return cmin;
        else if (cx > cmax)
            return cmax;
        else
            return cx;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X, typename Min, typename Max, typename VMin, typename VMax>
auto clip(X&& x, varg_tag, Min min, Max max, VMin vmin, VMax vmax)
{
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_real_v<Min> && is_real_v<Max>, "badargtype");
    static_assert(is_real_v<VMin> && is_real_v<VMax>, "badargtype");
    using C = common_type_t<value_type_t<XT>, Min, Max, VMin, VMax>;
    const auto cmin = cast<C>(min);
    const auto cmax = cast<C>(max);
    const auto cvmin = cast<C>(vmin);
    const auto cvmax = cast<C>(vmax);
    auto pure = [=](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        auto cx = cast<C>(x);
        if (cx < cmin)
            return cvmin;
        else if (cx > cmax)
            return cvmax;
        else
            return cx;
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename Ret = int64_t, typename X>
auto unitize(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        return Ret(int8_t(x != XV(0)));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename Ret = int64_t, typename X>
auto unit_step(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        if (x >= XV(0))
            return Ret(1);
        else
            return Ret(0);
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
constexpr auto min()
{
    return const_real_infinity;
}
template<typename X>
auto min(const X& x)
{
    if constexpr (is_argument_pack_v<X>)
    {
        using XV = value_type_t<value_type_t<X>>;
        const auto pack_size = x.size();
        auto ret = std::numeric_limits<XV>::max();
        for (size_t i = 0u; i < pack_size; ++i)
            ret = std::min(ret, min(x.get(i)));
        return ret;
    }
    else if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_real_v<X>, "badargtype");
        return x;
    }
    else
    {
        static_assert(is_real_v<value_type_t<X>>, "badargtype");
        using XV = value_type_t<X>;
        auto ret = std::numeric_limits<XV>::max();
        x.for_each([&](const auto& a) { ret = std::min(ret, a); });
        return ret;
    }
}
template<typename X1, typename X2, typename... Xs>
auto min(const X1& x1, const X2& x2, const Xs&... xs)
{
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
}
constexpr auto max()
{
    return -const_real_infinity;
}
template<typename X>
auto max(const X& x)
{
    if constexpr (is_argument_pack_v<X>)
    {
        using XV = value_type_t<value_type_t<X>>;
        const auto pack_size = x.size();
        auto ret = std::numeric_limits<XV>::min();
        for (size_t i = 0u; i < pack_size; ++i)
            ret = std::max(ret, max(x.get(i)));
        return ret;
    }
    else if constexpr (array_rank_v<X> == 0u)
    {
        static_assert(is_real_v<X>, "badargtype");
        return x;
    }
    else
    {
        static_assert(is_real_v<value_type_t<X>>, "badargtype");
        using XV = value_type_t<X>;
        auto ret = std::numeric_limits<XV>::min();
        x.for_each([&](const auto& a) { ret = std::max(ret, a); });
        return ret;
    }
}
template<typename X1, typename X2, typename... Xs>
auto max(const X1& x1, const X2& x2, const Xs&... xs)
{
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
}
template<typename X, typename Y>
auto chop(X&& x, const Y& y)
{
    static_assert(is_real_v<Y>, "badargtype");
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    constexpr auto XR = array_rank_v<XT>;
    using XV = std::conditional_t<XR == 0u, XT, value_type_t<XT>>;
    if constexpr (is_integral_v<XV>)
        return std::forward<decltype(x)>(x);
    else
    {
        auto pure =[lim = cast<value_type_t<XV>>(y)](const auto& x)
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
}
}
namespace wl
{
template<typename T, bool HasVariable>
struct step_iterator
{
    static constexpr bool has_variable = HasVariable;
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
        return begin_ + step_ * T(i);
    }
};
template<typename Array>
struct list_iterator
{
    static constexpr bool has_variable = true;
    using Iter = remove_cvref_t<decltype(
        std::declval<Array>().template view_begin<1u>())>;
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
        "badargtype");
    using T = common_type_t<Begin, Step>;
    if (step == Step(0))
        throw std::logic_error("badvalue");
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
    return make_step_iterator<true>(begin, end, step);
}
template<typename Begin, typename End>
auto var_iterator(Begin begin, End end)
{
    return make_step_iterator<true>(begin, end, int8_t(1));
}
template<typename Begin, typename End>
auto var_iterator(End end)
{
    return make_step_iterator<true>(int8_t(1), end, int8_t(1));
}
template<typename Any>
auto var_iterator(Any&& any)
{
    using Type = remove_cvref_t<Any>;
    if constexpr (is_real_v<Type>)
    {
        if constexpr (is_integral_v<Type>)
            return make_step_iterator<true>(Type(1), any, Type(1));
        else
            return make_step_iterator<true>(int64_t(1), any, int64_t(1));
    }
    else
    {
        static_assert(array_rank_v<Type> == 1u, "badargtype");
        if constexpr (is_movable_v<Any&&>)
            return list_iterator<Type>(std::move(any));
        else
        {
            auto copy = allows<view_category::Array>(
                std::forward<decltype(any)>(any));
            return list_iterator<decltype(copy)>(std::move(copy));
        }
    }
}
template<typename Any>
auto iterator(Any&& any)
{
    static_assert(is_real_v<remove_cvref_t<Any>>, "badargtype");
    return make_step_iterator<false>(int8_t(1), any, int8_t(1));
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
    static_assert(is_convertible_v<common_type_t<X, Y>, X>, "badcast"); \
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
    using XType = remove_cvref_t<X>;                                        \
    using YType = remove_cvref_t<Y>;                                        \
    constexpr auto x_rank = array_rank_v<XType>;                            \
    constexpr auto y_rank = array_rank_v<YType>;                            \
    static_assert(std::is_lvalue_reference_v<X&&> ||                        \
        is_array_view_v<XType>, "badassign");                               \
    if constexpr (x_rank > 0)                                               \
    {                                                                       \
        if constexpr (y_rank == 0)                                          \
        {                                                                   \
            x.for_each([y](auto& dst) { _scalar_##name(dst, y); });         \
        }                                                                   \
        else                                                                \
        {                                                                   \
            static_assert(x_rank == y_rank, "badrank");                     \
            if (!utils::check_dims<x_rank>(x.dims_ptr(), y.dims_ptr()))     \
                throw std::logic_error("baddims");                          \
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
        static_assert(y_rank == 0, "badrank");                              \
        _scalar_##name(x, y);                                               \
    }                                                                       \
    return std::forward<decltype(x)>(x);                                    \
}
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(add_to)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(subtract_from)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(times_by)
WL_DEFINE_MUTABLE_ARITHMETIC_FUNCTION(divide_by)
template<typename X>
auto pre_increment(X&& x) -> decltype(auto)
{
    return add_to(std::forward<decltype(x)>(x), int64_t(1));
}
template<typename X>
auto pre_decrement(X&& x) -> decltype(auto)
{
    return subtract_from(std::forward<decltype(x)>(x), int64_t(1));
}
template<typename X>
auto increment(X&& x)
{
    auto valx = val(x);
    pre_increment(std::forward<decltype(x)>(x));
    return valx;
}
template<typename X>
auto decrement(X&& x)
{
    auto valx = val(x);
    pre_decrement(std::forward<decltype(x)>(x));
    return valx;
}
template<typename X>
auto minus(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    return utils::listable_function([](const auto& x) { return -x; },
        std::forward<decltype(x)>(x));
}
template<typename Iter, bool HasStride>
auto _variadic_plus(const argument_pack<Iter, HasStride>& args)
{
    auto ret = val(args.get(0));
    const auto size = args.size();
    for (size_t i = 1u; i < size; ++i)
        add_to(ret, args.get(i));
    return ret;
}
template<typename X, typename Y>
auto plus(X&& x, Y&& y)
{
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(plus)
    {
        static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
        static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
        return utils::listable_function(_scalar_plus,
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(plus, int64_t(0))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(plus)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(plus)
template<typename X, typename Y>
auto subtract(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    return utils::listable_function(_scalar_subtract,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
template<typename Iter, bool HasStride>
auto _variadic_times(const argument_pack<Iter, HasStride>& args)
{
    auto ret = val(args.get(0));
    const auto size = args.size();
    for (size_t i = 1u; i < size; ++i)
        times_by(ret, args.get(i));
    return ret;
}
template<typename X, typename Y>
auto times(X&& x, Y&& y)
{
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(times)
    {
        static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
        static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
        return utils::listable_function(_scalar_times,
            std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(times, int64_t(1))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(times)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(times)
template<typename X, typename Y>
auto divide(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    return utils::listable_function(_scalar_divide,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
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
auto branch_if(const boolean cond, A&& a, B&& b)
{
    using AT = decltype(val(std::declval<A&&>()()));
    using BT = decltype(val(std::declval<B&&>()()));
    static_assert(_branch_type_check<AT, BT>::value, "badrettype");
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
}
template<typename... Conds>
auto _which_conditions(Conds&&... conds)
{
    static_assert(std::conjunction_v<std::is_same<
        remove_cvref_t<decltype(conds())>, boolean>...>, "badargtype");
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
            throw std::logic_error("badargc");
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
    static_assert(_branch_type_check<
        decltype(val(std::declval<Cases&&>()()))...>::value, "badrettype");
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
}
}
namespace wl
{
template<typename Skip, typename Fn, typename First, typename... Rest>
auto _clause_impl(Skip& skip_flag,
    Fn fn, const First& first, const Rest&... rest)
{
    if constexpr (sizeof...(Rest) == 0)
    {
        for (size_t i = 0; i < first.length(); ++i)
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
    static_assert(sizeof...(Iters) >= 1, "internal");
    wl::void_type skip_flag;    // skip flag is not used
    _clause_impl(skip_flag, fn, iters...);
    return wl::void_type{};
}
template<typename Fn, typename... Iters>
auto clause_break_do(Fn fn, const Iters&... iters)
{
    static_assert(sizeof...(Iters) >= 1, "internal");
    wl::void_type skip_flag;    // skip flag is not used
    try
    {
        _clause_impl(skip_flag, fn, iters...);
    }
    catch (const loop_break&)
    {
    }
    return wl::void_type{};
}
template<typename Fn, typename... Iters>
auto clause_table(Fn fn, const Iters&... iters)
{
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, "internal");
    using InnerType = remove_cvref_t<decltype(fn(iters[0]...))>;
    static_assert(is_numerical_type_v<InnerType>, "badargtype");
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);
    if (outer_size == 0u)
    {
        if constexpr (is_arithmetic_v<InnerType>)
            return ndarray<InnerType, outer_rank>(outer_dims);
        else
            throw std::logic_error("badvalue");
    }
    else
    {
        if constexpr (is_arithmetic_v<InnerType>)
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
            auto first_item = fn(iters[0]...);
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
                        throw std::logic_error("baddims");
                    item.copy_to(ret_iter.begin());
                    ++ret_iter;
                },
                iters...);
            return ret;
        }
    }
}
template<typename Fn, typename... Iters>
auto clause_sum(Fn fn, const Iters&... iters)
{
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, "internal");
    using InnerType = remove_cvref_t<decltype(fn(iters[0]...))>;
    static_assert(is_numerical_type_v<InnerType>, "badargtype");
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);
    if (outer_size == 0u)
    {
        if constexpr (is_arithmetic_v<InnerType>)
            return InnerType{};
        else
            throw std::logic_error("badvalue");
    }
    else
    {
        auto ret = fn(iters[0]...);
        bool skip_flag = true;      // skip flag is not used
        _clause_impl(skip_flag,
            [&](const auto&... args)
            {
                auto item = fn(args...);
                if constexpr (array_rank_v<InnerType> > 0u)
                {
                    if (!utils::check_dims(ret.dims(), item.dims()))
                        throw std::logic_error("baddims");
                }
                add_to(ret, item);
            },
            iters...);
        return ret;
    }
}
template<typename Fn, typename... Iters>
auto clause_product(Fn fn, const Iters&... iters)
{
    constexpr auto outer_rank = sizeof...(iters);
    static_assert(outer_rank >= 1u, "internal");
    using InnerType = remove_cvref_t<decltype(fn(iters[0]...))>;
    static_assert(is_numerical_type_v<InnerType>, "badargtype");
    auto outer_dims = std::array<size_t, outer_rank>{iters.length()...};
    auto outer_size = utils::size_of_dims(outer_dims);
    if (outer_size == 0u)
    {
        if constexpr (is_arithmetic_v<InnerType>)
            return InnerType(int8_t(1));
        else
            throw std::logic_error("badvalue");
    }
    else
    {
        auto ret = fn(iters[0]...);
        bool skip_flag = true;      // skip flag is not used
        _clause_impl(skip_flag,
            [&](const auto&... args)
            {
                auto item = fn(args...);
                if constexpr (array_rank_v<InnerType> > 0u)
                {
                    if (!utils::check_dims(ret.dims(), item.dims()))
                        throw std::logic_error("baddims");
                }
                times_by(ret, item);
            },
            iters...);
        return ret;
    }
}
}
namespace wl
{
template<typename Re, typename Im>
auto make_complex(const Re& re, const Im& im)
{
    static_assert(is_real_v<Re> && is_real_v<Im>, "badargtype");
    using C = common_type_t<Re, Im>;
    using T = std::conditional_t<std::is_same_v<C, float>, float, double>;
    return complex<T>(T(re), T(im));
}
template<typename X>
auto re(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_re = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x.real();
        else
            return x;
    };
    return utils::listable_function(scalar_re, std::forward<decltype(x)>(x));
}
template<typename X>
auto im(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_im = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return x.imag();
        else
            return XV(0);
    };
    return utils::listable_function(scalar_im, std::forward<decltype(x)>(x));
}
template<typename X>
auto arg(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_arg = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_complex_v<XV>)
            return std::arg(x);
        else if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return double(0);
            else
                return x >= X(0) ? double(0) : const_pi;
        }
        else
            return x >= X(0) ? X(0) : X(const_pi);
    };
    return utils::listable_function(scalar_arg, std::forward<decltype(x)>(x));
}
template<typename X>
auto conjugate(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
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
}
template<typename X>
auto re_im(X&& x)
{
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    constexpr auto rank = array_rank_v<XT>;
    if constexpr (rank == 0)
    {
        ndarray<XT, 1u> ret(std::array<size_t, 1u>{2});
        ret[0] = re(x);
        ret[1] = im(x);
    }
    else
    {
        using XV = typename XT::value_type;
        auto dims = utils::dims_join(x.dims(), std::array<size_t, 1u>{2});
        ndarray<XV, rank + 1u> ret(dims);
        auto iter = ret.begin();
        x.for_each([&](const auto& a) { *iter++ = re(a), *iter++ = im(a); });
        return ret;
    }
}
template<typename X>
auto abs_arg(X&& x)
{
    using XT = remove_cvref_t<X>;
    static_assert(is_numerical_type_v<XT>, "badargtype");
    constexpr auto rank = array_rank_v<XT>;
    if constexpr (rank == 0)
    {
        using T = decltype(arg(XT{}));
        ndarray<T, 1u> ret(std::array<size_t, 1u>{2});
        ret[0] = T(abs(x));
        ret[1] = T(arg(x));
    }
    else
    {
        using XV = typename XT::value_type;
        using T = decltype(arg(XV{}));
        auto dims = utils::dims_join(x.dims(), std::array<size_t, 1u>{2});
        ndarray<T, rank + 1u> ret(dims);
        auto iter = ret.begin();
        x.for_each([&](const auto& a)
            { *iter++ = T(abs(a)), *iter++ = T(arg(a)); });
        return ret;
    }
}
}
namespace wl
{
#define WL_DEFINE_UNARY_MATH_FUNCTION(name, expr)                           \
template<typename X>                                                        \
WL_INLINE auto name(X&& x)                                                  \
{                                                                           \
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");    \
    return utils::listable_function([](auto x) {                            \
            using PX = promote_integral_t<decltype(x)>;                     \
            return expr;                                                    \
        }, std::forward<decltype(x)>(x));                                   \
}
#define WL_DEFINE_BINARY_MATH_FUNCTION(name, expr)                          \
template<typename X, typename Y>                                            \
WL_INLINE auto name(X&& x, Y&& y)                                           \
{                                                                           \
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");    \
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");    \
    return utils::listable_function([](auto x, auto y) {                    \
            using PX = promote_integral_t<decltype(x)>;                     \
            using PY = promote_integral_t<decltype(y)>;                     \
            using PC = promote_integral_t<                                  \
                common_type_t<decltype(x), decltype(y)>>;                   \
            return expr;                                                    \
        }, std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));     \
}
template<typename X>
WL_INLINE auto _scalar_square(const X x)
{
    return X(x * x);
}
WL_DEFINE_UNARY_MATH_FUNCTION(log, std::log(x))
WL_DEFINE_BINARY_MATH_FUNCTION(log, std::log(PC(y)) / std::log(PC(x)))
WL_DEFINE_UNARY_MATH_FUNCTION(log2, PX(1.4426950408889634074) * std::log(x))
WL_DEFINE_UNARY_MATH_FUNCTION(log10, PX(0.43429448190325182765) * std::log(x))
WL_DEFINE_UNARY_MATH_FUNCTION(exp, std::exp(x))
WL_DEFINE_UNARY_MATH_FUNCTION(sqrt, std::sqrt(x))
WL_DEFINE_BINARY_MATH_FUNCTION(power, std::pow(x, y))
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
WL_DEFINE_UNARY_MATH_FUNCTION(sinc, (x == PX(0) ? PX(1) : std::sin(x) / P(x)))
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
WL_DEFINE_UNARY_MATH_FUNCTION(gamma, std::tgamma(x))
WL_DEFINE_UNARY_MATH_FUNCTION(log_gamma, std::lgamma(x))
WL_DEFINE_UNARY_MATH_FUNCTION(erf, std::erf(x))
WL_DEFINE_UNARY_MATH_FUNCTION(erfc, std::erfc(x))
WL_DEFINE_BINARY_MATH_FUNCTION(beta, std::beta(x, y))
WL_DEFINE_UNARY_MATH_FUNCTION(zeta, std::riemann_zeta(x))
}
namespace wl
{
template<typename X>
auto even_q(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        if constexpr (is_integral_v<XV>)
            return boolean((x & XV(1)) == XV(0));
        else
            return boolean(std::fmod(x, XV(2)) == XV(0));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X>
auto odd_q(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        if constexpr (is_integral_v<XV>)
            return boolean((x & XV(1)) == XV(1));
        else
            return boolean(std::fmod(x, XV(2)) == std::copysign(XV(1), x));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X, typename Y>
auto divisible(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    auto pure = [](const auto& x, const auto& y)
    {
        using XV = remove_cvref_t<decltype(x)>;
        using YV = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XV> && is_real_v<YV>, "badargtype");
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
    auto lzcnt = utils::_lzcnt(n);
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
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_fibonacci(x));
            else if (x >= XV(0))
                return XV(_fibonacci(x));
            else
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
}
template<typename X>
auto lucas_l(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto pure = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        if constexpr (is_integral_v<XV>)
        {
            if constexpr (std::is_unsigned_v<XV>)
                return XV(_lucas_l(x));
            else if (x >= XV(0))
                return XV(_lucas_l(x));
            else
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
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bool_and)
    {
        static_assert(is_boolean_v<remove_cvref_t<X>> &&
            is_boolean_v<remove_cvref_t<Y>>, "badargtype");
        return x && y;
    }
}
template<typename Iter, bool HasStride>
auto _variadic_bool_and(const argument_pack<Iter, HasStride>& args)
{
    boolean ret = val(args.get(0));
    const auto size = args.size();
    for (size_t i = 1u; ret && i < size; ++i)
        ret = bool_and(ret, args.get(i));
    return ret;
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bool_and, const_true)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bool_and)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bool_and)
template<typename X, typename Y>
auto bool_or(X&& x, Y&& y)
{
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bool_or)
    {
        static_assert(is_boolean_v<remove_cvref_t<X>> &&
            is_boolean_v<remove_cvref_t<Y>>, "badargtype");
        return x || y;
    }
}
template<typename Iter, bool HasStride>
auto _variadic_bool_or(const argument_pack<Iter, HasStride>& args)
{
    boolean ret = val(args.get(0));
    const auto size = args.size();
    for (size_t i = 1u; !ret && i < size; ++i)
        ret = bool_or(ret, args.get(i));
    return ret;
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bool_or, const_false)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bool_or)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bool_or)
template<typename X, typename Y>
auto bool_xor(X&& x, Y&& y)
{
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bool_xor)
    {
        static_assert(is_boolean_v<remove_cvref_t<X>> &&
            is_boolean_v<remove_cvref_t<Y>>, "badargtype");
        return x ^ y;
    }
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_VARIADIC(bool_xor)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bool_xor, const_false)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bool_xor)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bool_xor)
template<typename... Args>
auto bool_nand(Args&&... args)
{
    return !bool_and(std::forward<decltype(args)>(args)...);
}
template<typename... Args>
auto bool_nor(Args&&... args)
{
    return !bool_or(std::forward<decltype(args)>(args)...);
}
template<typename... Args>
auto bool_xnor(Args&&... args)
{
    return !bool_xor(std::forward<decltype(args)>(args)...);
}
template<typename Ret = int64_t, typename X>
auto boole(X&& x)
{
    static_assert(is_boolean_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto pure = [](boolean x) { return Ret(x); };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X>
auto bit_not(X&& x)
{
    auto pure = [](auto x)
    {
        using XV = decltype(x);
        static_assert(is_integral_v<XV>, "badargtype");
        return XV(~x);
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename Ret = int64_t, typename X>
auto bit_length(X&& x)
{
    auto pure = [](auto x)
    {
        using XV = decltype(x);
        static_assert(is_integral_v<XV>, "badargtype");
        if constexpr (std::is_unsigned_v<XV>)
            return Ret(64) - Ret(utils::_lzcnt(x));
        else
            return Ret(64) - Ret(utils::_lzcnt(
                std::make_unsigned_t<XV>(x >= XV(0) ? x : ~x)));
    };
    return utils::listable_function(pure, std::forward<decltype(x)>(x));
}
template<typename X, typename Y>
auto bit_and(X&& x, Y&& y)
{
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bit_and)
    {
        using XV = remove_cvref_t<X>;
        using YV = remove_cvref_t<Y>;
        static_assert(is_integral_v<XV> && is_integral_v<YV>, "badargtype");
        using C = common_type_t<XV, YV>;
        return C(C(x) & C(y));
    }
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_VARIADIC(bit_and)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bit_and, int64_t(-1))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bit_and)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bit_and)
template<typename X, typename Y>
auto bit_or(X&& x, Y&& y)
{
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bit_or)
    {
        using XV = remove_cvref_t<X>;
        using YV = remove_cvref_t<Y>;
        static_assert(is_integral_v<XV> && is_integral_v<YV>, "badargtype");
        using C = common_type_t<XV, YV>;
        return C(C(x) | C(y));
    }
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_VARIADIC(bit_or)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bit_or, int64_t(0))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bit_or)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bit_or)
template<typename X, typename Y>
auto bit_xor(X&& x, Y&& y)
{
    WL_VARIADIC_FUNCTION_DEFAULT_IF_PARAMETER_PACK(bit_xor)
    {
        using XV = remove_cvref_t<X>;
        using YV = remove_cvref_t<Y>;
        static_assert(is_integral_v<XV> && is_integral_v<YV>, "badargtype");
        using C = common_type_t<XV, YV>;
        return C(C(x) ^ C(y));
    }
}
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_VARIADIC(bit_xor)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NULLARY(bit_xor, int64_t(0))
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_UNARY(bit_xor)
WL_VARIADIC_FUNCTION_DEFINE_DEFAULT_NARY(bit_xor)
template<bool ShiftLeft, typename X, typename Y>
auto _bit_shift_impl(X&& x, Y&& y)
{
    auto pure = [](auto x, auto y)
    {
        using XV = decltype(x);
        using YV = decltype(y);
        static_assert(is_integral_v<XV> && is_integral_v<YV>,
            "badargtype");
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
}
template<typename X, typename Y>
auto bit_shift_left(X&& x, Y&& y)
{
    return _bit_shift_impl<true>(
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
template<typename X, typename Y>
auto bit_shift_right(X&& x, Y&& y)
{
    return _bit_shift_impl<false>(
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
}
namespace wl
{
extern std::default_random_engine global_random_engine;
namespace distribution
{
constexpr auto _min = [](const auto x, const auto y)
{
    return x < y ? x : y;
};
constexpr auto _max = [](const auto x, const auto y)
{
    return x < y ? y : x;
};
template<typename T>
struct uniform
{
    static_assert(is_real_v<T>, "internal");
    using value_type = T;
    static constexpr size_t rank = 0;
    using _dist_type = std::conditional_t<is_integral_v<T>,
        std::uniform_int_distribution<T>,
        std::uniform_real_distribution<T>>;
    _dist_type dist_;
    uniform(T a, T b) :
        dist_(_min(a, b), _max(a, b))
    {
    }
    size_t size() const
    {
        return 1u;
    }
    T operator()()
    {
        return dist_(global_random_engine);
    }
};
template<typename T>
struct uniform<complex<T>>
{
    static_assert(is_float_v<T>, "internal");
    using value_type = complex<T>;
    static constexpr size_t rank = 0;
    std::uniform_real_distribution<T> dist_re_;
    std::uniform_real_distribution<T> dist_im_;
    uniform(const complex<T>& a, const complex<T>& b) :
        dist_re_(_min(a.real(), b.real()), _max(a.real(), b.real())),
        dist_im_(_min(a.imag(), b.imag()), _max(a.imag(), b.imag()))
    {
    }
    size_t size() const
    {
        return 1u;
    }
    complex<T> operator()()
    {
        return complex<T>(
            dist_re_(global_random_engine),
            dist_im_(global_random_engine));
    }
};
template<typename T>
struct normal
{
    static_assert(is_float_v<T>, "internal");
    using value_type = T;
    static constexpr size_t rank = 0;
    std::normal_distribution<T> dist_;
    normal(T mean, T stddev) :
        dist_(mean, stddev)
    {
    }
    size_t size() const
    {
        return 1u;
    }
    T operator()()
    {
        return dist_(global_random_engine);
    }
};
}
template<typename Dist, typename... Dims>
auto _random_variate_impl(Dist dist, const Dims&... dims)
{
    static_assert(all_is_integral_v<Dims...>, "badargtype");
    using T = typename Dist::value_type;
    constexpr size_t R1 = Dist::rank;
    constexpr size_t R2 = sizeof...(dims);
    constexpr size_t R = R1 + R2;
    static_assert(R1 == 0u, "internal");
    if constexpr (R2 == 0u)
        return dist();
    else
    {
        wl::ndarray<T, R> x(std::array<int64_t, R>{int64_t(dims)...});
        x.for_each([&](auto& v) { v = dist(); });
        return x;
    }
}
template<typename Min, typename Max, typename... Dims>
auto random_integer(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(all_is_integral_v<Min, Max>, "badargtype");
    using T = common_type_t<Min, Max>;
    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
}
template<typename Max, typename... Dims>
auto random_integer(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_integral_v<Max>, "badargtype");
    return random_integer(Max{}, max, varg_tag{}, dims...);
}
template<typename Min, typename Max, typename... Dims>
auto random_real(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_real_v<Min> && is_real_v<Max>, "badargtype");
    using C = common_type_t<Min, Max>;
    using T = std::conditional_t<is_integral_v<C>, double, C>;
    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
}
template<typename Max, typename... Dims>
auto random_real(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_real_v<Max>, "badargtype");
    return random_real(Max{}, max, varg_tag{}, dims...);
}
template<typename Min, typename Max, typename... Dims>
auto random_complex(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_arithmetic_v<Min> && is_arithmetic_v<Max>, "badargtype");
    using C = common_type_t<value_type_t<Min>, value_type_t<Max>>;
    using T = std::conditional_t<std::is_same_v<C, float>,
        complex<float>, complex<double>>;
    auto dist = distribution::uniform<T>(cast<T>(min), cast<T>(max));
    return _random_variate_impl(dist, dims...);
}
template<typename Max, typename... Dims>
auto random_complex(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_arithmetic_v<Max>, "badargtype");
    return random_complex(Max{}, max, varg_tag{}, dims...);
}
template<typename Array>
auto random_choice(const Array& x)
{
    constexpr auto XR = array_rank_v<Array>;
    static_assert(XR >= 1u, "badrank");
    using XV = value_type_t<Array>;
    const auto& valx = allows<view_category::Regular>(x);
    const auto x_iter = valx.begin();
    auto dist = std::uniform_int_distribution<size_t>(0u, x.dims()[0] - 1u);
    if constexpr (XR == 1u)
    {
        return *(valx.begin() + dist(global_random_engine));
    }
    else
    {
        auto item_dims = utils::dims_take<2u, XR>(x.dims());
        auto item_size = utils::size_of_dims(item_dims);
        ndarray<XV, XR - 1u> ret(item_dims);
        utils::copy_n(valx.begin() + item_size * dist(global_random_engine),
            item_size, ret.data());
        return ret;
    }
}
template<typename Array, size_t OuterRank>
auto _random_choice_impl(const Array& x,
    const std::array<size_t, OuterRank>& outer_dims)
{
    constexpr auto XR = array_rank_v<Array>;
    static_assert(XR >= 1u, "badrank");
    using XV = value_type_t<Array>;
    const auto& valx = allows<view_category::Regular>(x);
    const auto x_iter = valx.begin();
    auto dist = std::uniform_int_distribution<size_t>(0u, x.dims()[0] - 1u);
    const auto outer_size = utils::size_of_dims(outer_dims);
    if constexpr (XR == 1u)
    {
        ndarray<XV, OuterRank> ret(outer_dims);
        auto ret_iter = ret.data();
        for (size_t i = 0; i < outer_size; ++i, ++ret_iter)
            *ret_iter = *(x_iter + dist(global_random_engine));
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
            utils::copy_n(x_iter + item_size * dist(global_random_engine),
                item_size, base_iter);
        return ret;
    }
}
template<typename Array, typename... Dims>
auto random_choice(const Array& x, varg_tag, const Dims&... dims)
{
    static_assert(all_is_integral_v<Dims...>, "badargtype");
    if (!((dims > 0) && ...))
        throw std::logic_error("baddims");
    return _random_choice_impl(x,
        std::array<size_t, sizeof...(Dims)>{size_t(dims)...});
}
}
namespace wl
{
template<typename Dst, typename Src>
auto set(Dst&& dst, Src&& src) -> decltype(auto)
{
    using DstType = remove_cvref_t<Dst>;
    using SrcType = remove_cvref_t<Src>;
    constexpr auto dst_rank = array_rank_v<DstType>;
    constexpr auto src_rank = array_rank_v<SrcType>;
    using DstValue = value_type_t<DstType>;
    using SrcValue = value_type_t<SrcType>;
    static_assert(std::is_lvalue_reference_v<Dst&&> ||
        is_array_view_v<DstType>, "badassign");
    if constexpr (src_rank == 0u)
    {
        if constexpr (dst_rank == 0u)
        { // scalar -> scalar
            static_assert(is_convertible_v<SrcType, DstType>, "badargtype");
            dst = DstType(src);
            return std::forward<decltype(src)>(src);
        }
        else
        { // scalar -> ndarray / array_view
            static_assert(is_convertible_v<SrcType, DstValue>, "badargtype");
            dst.copy_from(make_scalar_view_iterator(DstValue(src)));
            return std::forward<decltype(src)>(src);
        }
    }
    else
    {
        static_assert(dst_rank == src_rank, "badrank");
        static_assert(is_convertible_v<SrcValue, DstValue>, "badargtype");
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
                throw std::logic_error("baddims");
            if (!has_aliasing(src, dst))
            {
                if constexpr (SrcType::category != view_category::General)
                    dst.copy_from(src.begin());
                else if (DstType::category != view_category::General)
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
        static_assert(array_rank_v<ItemType> == R, "badrank");
        using ValueType = value_type_t<ItemType>;
        static_assert(is_convertible_v<ValueType, T>, "badargtype");
        const auto size = first.size();
        for (size_t i = 0u; i < size; ++i, ++ret_iter)
        {
            const auto& item = first.get(i);
            if (!utils::check_dims(item.dims(), dims))
                throw std::logic_error("baddims");
            item.copy_to(ret_iter.begin());
        }
    }
    else
    {
        static_assert(array_rank_v<FirstType> == R, "badrank");
        using ValueType = value_type_t<FirstType>;
        static_assert(is_convertible_v<ValueType, T>, "badargtype");
        if (!utils::check_dims(first.dims(), dims))
            throw std::logic_error("baddims");
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
        static_assert(is_convertible_v<ItemType, T>, "badargtype");
        const auto size = first.size();
        for (size_t i = 0; i < size; ++i, ++ret_iter)
            *ret_iter = cast<T>(first.get(i));
    }
    else
    {
        static_assert(is_convertible_v<FirstType, T>, "badargtype");
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
                constexpr auto rank = array_rank_v<ItemType> +1u;
                using ValueType = std::conditional_t<
                    rank == 0u, ItemType, value_type_t<ItemType>>;
                return ndarray<value_type_t<ItemType>, rank>{};
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
            ndarray<FirstType, 1u> ret(std::array<size_t, 1u>{dim0});
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
            ndarray<FirstValueType, rank> ret(dims);
            auto ret_iter = ret.template view_begin<1u>();
            _copy_list_array_elements(ret_iter, first.dims(),
                std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...);
            return ret;
        }
    }
}
template<typename T, typename... Dims>
auto constant_array(const T& val, varg_tag, const Dims&... dims)
{
    constexpr auto val_rank = array_rank_v<T>;
    constexpr auto rep_rank = sizeof...(dims);
    constexpr auto all_rank = val_rank + rep_rank;
    if constexpr (val_rank > 0)
    {
        using ValueType = typename T::value_type;
        std::array<int64_t, all_rank> all_dims{int64_t(dims)...};
        std::copy_n(val.dims().begin(), val_rank, all_dims.data() + rep_rank);
        ndarray<ValueType, all_rank> ret(all_dims);
        size_t val_size = val.size();
        if constexpr (T::category == view_category::Array ||
            T::category == view_category::Simple)
        {
            auto iter = ret.begin();
            auto end = iter + ret.size();
            for (; iter != end; iter += val_size)
                val.copy_to(iter);
        }
        else
        {
            auto buffer = val.to_array();
            auto iter = ret.begin();
            auto end = iter + ret.size();
            for (; iter != end; iter += val_size)
                buffer.copy_to(iter);
        }
        return ret;
    }
    else
    {
        ndarray<T, all_rank> ret(
            std::array<int64_t, all_rank>{int64_t(dims)...}, val);
        return ret;
    }
}
template<size_t>
using make_all_type = all_type;
template<typename Array, typename... Indexers>
auto _part_impl3(Array&& a, Indexers&&... indexers) -> decltype(auto)
{
    using ArrayType = remove_cvref_t<Array>;
    using ValueType = typename ArrayType::value_type;
    constexpr auto is_const = array_is_const_v<Array&&>;
    static_assert(sizeof...(Indexers) <= array_rank_v<ArrayType>, "internal");
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
    using ArrayType = remove_cvref_t<Array>;
    static_assert(is_value_type_v<ArrayType>, "badargtype");
    constexpr auto R = array_rank_v<ArrayType>;
    static_assert(sizeof...(Specs) <= R, "badargc");
    if constexpr (R == 0)
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
}
template<typename Array>
auto first(Array&& a)
{
    static_assert(array_rank_v<remove_cvref_t<Array>> >= 1, "badargtype");
    return part(a, cidx(0));
}
template<typename Array>
auto last(Array&& a)
{
    static_assert(array_rank_v<remove_cvref_t<Array>> >= 1, "badargtype");
    return part(a, int64_t(-1));
}
template<typename Array>
auto most(Array&& a)
{
    using AT = remove_cvref_t<Array>;
    constexpr auto AR = array_rank_v<AT>;
    static_assert(AR >= 1, "badargtype");
    const auto size = a.dims()[0];
    if (size <= 1u)
        throw std::logic_error("baddims");
    return part(a, make_span(const_all, int64_t(-2)));
}
template<typename Array>
auto rest(Array&& a)
{
    using AT = remove_cvref_t<Array>;
    static_assert(array_rank_v<AT> >= 1, "badargtype");
    const auto size = a.dims()[0];
    if (size <= 1u)
        throw std::logic_error("baddims");
    return part(a, make_span(int64_t(2), const_all));
}
template<typename Begin, typename End, typename Step>
auto range(Begin begin, End end, Step step)
{
    static_assert(is_real_v<Begin> && is_real_v<End> && is_real_v<Step>,
        "badargtype");
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
                auto ret_iter = ret.begin();
                for (size_t i = 0; i < length; ++i, begin += step, ++ret_iter)
                    *ret_iter = begin;
                return ret;
            }
        }
        else // step = 0
            throw std::logic_error("badvalue");
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
                auto ret_iter = ret.begin();
                for (size_t i = 0; i < length; ++i, ++ret_iter)
                    *ret_iter = T(begin + i * step);
                return ret;
            }
        }
        else // step = 0
            throw std::logic_error("badvalue");
    }
}
template<typename Begin, typename End>
auto range(Begin begin, End end)
{
    static_assert(is_real_v<Begin> && is_real_v<End>, "badargtype");
    return range(begin, end, int8_t(1));
}
template<typename End>
auto range(End end)
{
    static_assert(is_real_v<End>, "badargtype");
    return range(End(1), end, int8_t(1));
}
template<typename Array, int64_t I1, int64_t I2>
auto total(const Array& a, const_int<I1>, const_int<I2>)
{
    constexpr auto rank = array_rank_v<Array>;
    static_assert(rank >= 1, "badargtype");
    using ValueType = value_type_t<Array>;
    constexpr int64_t L1 = I1 >= 0 ? I1 : I1 + int64_t(rank) + 1;
    constexpr int64_t L2 = I2 >= 0 ? I2 : I2 + int64_t(rank) + 1;
    static_assert(1 <= L1 && L1 <= L2 && L2 <= int64_t(rank), "badlevel");
    if constexpr (Array::category == view_category::General)
        return total(a.to_array(), const_int<I1>{}, const_int<I2>{});
    else
    {
        if constexpr (L1 == 1)
        {
            if constexpr (L2 == rank)
            {
                const auto inter_size = a.size();
                auto a_iter = a.begin();
                auto ret = ValueType{};
                for (size_t j = 0; j < inter_size; ++j, ++a_iter)
                    ret += *a_iter;
                return ret;
            }
            else
            {
                auto ret_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                ndarray<ValueType, rank - L2> ret(ret_dims);
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
                ndarray<ValueType, L1 - 1> ret(ret_dims);
                const auto outer_size = ret.size();
                const auto inter_size = utils::size_of_dims(
                    utils::dims_take<L1, L2>(a.dims()));
                auto a_iter = a.begin();
                auto ret_iter = ret.begin();
                for (size_t i = 0; i < outer_size; ++i, ++ret_iter)
                {
                    auto sum = ValueType{};
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
                ndarray<ValueType, rank - (L2 - L1 + 1)> ret(ret_dims);
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
}
template<typename Array, int64_t I>
auto total(const Array& a, const_int<I>)
{
    return total(a, const_int<1>{}, const_int<I>{});
}
template<typename Array>
auto total(const Array& a)
{
    return total(a, const_int<1>{}, const_int<1>{});
}
template<typename Array>
auto mean(const Array& a)
{
    return divide(total(a), a.template dimension<1>());
}
template<typename Ret = int64_t, typename Array>
auto dimensions(const Array& a)
{
    static_assert(is_integral_v<Ret>, "badrettype");
    constexpr auto rank = array_rank_v<Array>;
    if constexpr (rank == 0u)
        return ndarray<Ret, 1u>{};
    else
        return ndarray<Ret, 1u>(std::array<size_t, 1u>{rank},
            a.dims_ptr(), a.dims_ptr() + rank);
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
    using XT = remove_cvref_t<X>;
    constexpr auto rank = array_rank_v<XT>;
    static_assert(rank >= 1, "badargtype");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(rank) + 1;
    static_assert(1 <= Level && Level <= int64_t(rank), "badlevel");
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
}
template<typename X>
auto reverse(X&& x)
{
    return reverse(std::forward<decltype(x)>(x), const_int<1>{});
}
template<typename X, typename Pad, typename... Dims>
auto array_reshape(X&& x, const Pad& padding, varg_tag, const Dims&... dims)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    static_assert(array_rank_v<XT> >= 1, "badargtype");
    static_assert(all_is_integral_v<Dims...>, "badargtype");
    constexpr auto rank = sizeof...(dims);
    static_assert(rank >= 1, "badrank");
    ndarray<XV, rank> ret(std::array<int64_t, rank>{int64_t(dims)...});
    const size_t x_size = x.size();
    const size_t ret_size = ret.size();
    if (x_size <= ret_size)
    {
        x.copy_to(ret.begin());
        if constexpr (!std::is_same_v<Pad, void_type>)
        {
            static_assert(is_convertible_v<Pad, XV>, "badargtype");
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
}
template<typename X, typename... Dims>
auto array_reshape(X&& x, varg_tag, const Dims&... dims)
{
    static_assert(array_rank_v<remove_cvref_t<X>> >= 1, "badargtype");
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
        throw std::logic_error("baddims")), ...);
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
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto NL = sizeof...(Is);
    static_assert(1 <= NL && NL <= XR, "badargtype");
    static_assert(_is_valid_transpose<XR, size_t(Is)...>::value, "badlevel");
    return _transpose_impl(val(x), void_type{},
        typename _padded_transpose_levels<XR, Is...>::type{},
        std::make_index_sequence<XR>{});
}
template<typename X>
auto transpose(const X& x)
{
    return transpose(x, const_int<2>{}, const_int<1>{});
}
template<typename X>
auto conjugate_transpose(const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 2u, "badrank");
    using XV = value_type_t<X>;
    if constexpr (is_real_v<XV>)
        return transpose(x, const_int<2>{}, const_int<1>{});
    else
    {
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
    static_assert(1 <= MaxLevel && MaxLevel <= XR, "badlevel");
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
    constexpr auto XR = array_rank_v<remove_cvref_t<X>>;
    constexpr auto MaxLevel = _flatten_max_level<Levels...>::value;
    static_assert(1 <= MaxLevel && MaxLevel <= XR, "badlevel");
    return _flatten_impl1<MaxLevel>(std::forward<decltype(x)>(x),
        std::make_index_sequence<XR - MaxLevel>{}, Levels{}...);
}
template<typename X>
auto flatten(X&& x)
{
    using XT = remove_cvref_t<X>;
    using XV = value_type_t<XT>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(1 <= XR, "badlevel");
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
}
inline int64_t _order_scalar(const boolean& x, const boolean& y)
{
    return x == y ? int64_t(0) : x ? int64_t(-1) : int64_t(1);
}
template<typename X>
int64_t _order_scalar(const complex<X>& x, const complex<X>& y)
{
    return x.real() == y.real() ? _order_scalar(x.imag(), y.imag()) :
        (x.real() < y.real() ? int64_t(1) : int64_t(-1));
}
template<typename X>
int64_t _order_scalar(const X& x, const X& y)
{
    return x == y ? int64_t(0) : x < y ? int64_t(1) : int64_t(-1);
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
    static_assert(XR == YR, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    if constexpr (XR == 0)
    {
        static_assert(std::is_same_v<X, Y>, "badargtype");
        return Ret(_order_scalar(x, y));
    }
    else
    {
        static_assert(std::is_same_v<value_type_t<X>, value_type_t<Y>>,
            "badargtype");
        return Ret(_order_array(x, y));
    }
}
template<typename Ret = int64_t, typename X, typename Pred>
auto ordering(const X& x, const int64_t n, Pred pred)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    static_assert(is_integral_v<Ret>, "badrettype");
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
            auto res = pred(*(x_base + a), *(x_base + b));
            using OrderType = decltype(res);
            if constexpr (std::is_same_v<OrderType, bool> ||
                is_boolean_v<OrderType>)
                return bool(res);
            else
            {
                static_assert(std::is_signed_v<OrderType>, "badfunctype");
                return res > OrderType(0);
            }
        };
        if (size_t(n) > x_size)
            throw std::logic_error("badargv");
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
            auto res = pred(*(x_base + a), *(x_base + b));
            using OrderType = decltype(res);
            if constexpr (std::is_same_v<OrderType, bool> ||
                is_boolean_v<OrderType>)
                return !bool(res);
            else
            {
                static_assert(std::is_signed_v<OrderType>, "badfunctype");
                return res < OrderType(0);
            }
        };
        if (size_t(-n) > x_size)
            throw std::logic_error("badargv");
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
}
template<typename Ret = int64_t, typename X>
auto ordering(const X& x, const int64_t n)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    if constexpr (XR == 1u)
    {
        using XV = value_type_t<X>;
        if constexpr (is_complex_v<XV> || is_boolean_v<XV>)
        {
            return ordering<Ret>(x, n, [](const auto& a, const auto& b)
                { return _order_scalar(a, b) > 0; });
        }
        else
        {
            static_assert(is_real_v<XV>, "badargtype");
            return ordering<Ret>(x, n, std::less<>{});
        }
    }
    else
    {
        return ordering<Ret>(x, n,
            [](const auto& a, const auto& b)
            { return _order_array(a, b, dim_checked{}) > 0; });
    }
}
template<typename Ret = int64_t, typename X, typename Pred>
auto ordering(const X& x, all_type, Pred pred)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    return ordering<Ret>(x, x.dims()[0], pred);
}
template<typename Ret = int64_t, typename X>
auto ordering(const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    return ordering<Ret>(x, x.dims()[0]);
}
template<typename Ret = int64_t, typename X>
auto ordering(const X& x, all_type)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    return ordering<Ret>(x, x.dims()[0]);
}
template<typename T, typename Pred>
auto _sort_simple(ndarray<T, 1u>&& a, Pred pred)
{
    std::sort(a.begin(), a.end(), pred);
    return std::move(a);
}
template<typename T, typename Pred>
auto _sort_simple(const ndarray<T, 1u>& a, Pred pred)
{
    auto copy = a;
    std::sort(copy.begin(), copy.end(), pred);
    return copy;
}
template<typename X, typename Pred>
auto sort(X&& x, Pred pred)
{
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, "badargtype");
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
            static_assert(std::is_signed_v<OrderType>, "badfunctype");
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
}
template<typename X>
auto sort(X&& x)
{
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, "badargtype");
    if constexpr (XR == 1u)
    {
        using XV = value_type_t<XT>;
        if constexpr (is_complex_v<XV> || is_boolean_v<XV>)
        {
            return sort(std::forward<decltype(x)>(x),
                [](const auto& a, const auto& b)
                { return _order_scalar(a, b) > 0; });
        }
        else
        {
            static_assert(is_real_v<XV>, "badargtype");
            return sort(std::forward<decltype(x)>(x), std::less<>{});
        }
    }
    else
    {
        return sort(std::forward<decltype(x)>(x),
            [](const auto& a, const auto& b)
            { return _order_array(a, b, dim_checked{}) > 0; });
    }
}
template<typename X, typename Pred>
auto ordered_q(const X& x, Pred pred)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    const auto& copy = allows<view_category::Array>(x);
    const auto copy_length = copy.dims()[0];
    auto in_order = [=](const auto& a, const auto& b)
    {
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
}
template<typename X>
auto ordered_q(const X& x)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badargtype");
    const auto& copy = allows<view_category::Array>(x);
    const auto copy_length = copy.dims()[0];
    auto in_order = [=](const auto& a, const auto& b)
    {
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
}
template<typename X, typename Y>
auto append(X&& x, Y&& y)
{
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto YR = array_rank_v<YT>;
    using XV = value_type_t<XT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u && is_convertible_v<YV, XV>, "badargtype");
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
            throw std::logic_error("baddims");
        ndarray<XV, XR> ret(utils::dims_join(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, x_elem_dims));
        const auto ret_iter = ret.data();
        x.copy_to(ret_iter);
        y.copy_to(ret_iter + x.size());
        return ret;
    }
}
template<typename X, typename Y>
auto prepend(X&& x, Y&& y)
{
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    constexpr auto XR = array_rank_v<XT>;
    constexpr auto YR = array_rank_v<YT>;
    using XV = value_type_t<XT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u && is_convertible_v<YV, XV>, "badargtype");
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
            throw std::logic_error("baddims");
        ndarray<XV, XR> ret(utils::dims_join(
            std::array<size_t, 1u>{x.dims()[0] + 1u}, x_elem_dims));
        const auto ret_iter = ret.data();
        y.copy_to(ret_iter);
        x.copy_to(ret_iter + y.size());
        return ret;
    }
}
template<typename XV, size_t XR, typename Y>
auto append_to(ndarray<XV, XR>& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    constexpr auto YR = array_rank_v<YT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u && is_convertible_v<YV, XV>, "badargtype");
    x.append(std::forward<decltype(y)>(y));
}
template<typename XV, size_t XR, typename Y>
auto prepend_to(ndarray<XV, XR>& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    constexpr auto YR = array_rank_v<YT>;
    using YV = std::conditional_t<YR == 0u, YT, value_type_t<YT>>;
    static_assert(XR == YR + 1u && is_convertible_v<YV, XV>, "badargtype");
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
            throw std::logic_error("baddims");
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
        static_assert(array_rank_v<Arg> == Rank, "badrank");
        if (arg.size() > 0u)
        {
            const auto leading_check = utils::check_dims<Level - 1u>(
                dims.data(), arg.dims().data());
            const auto trailing_check = utils::check_dims<Rank - Level>(
                dims.data() + Level, arg.dims().data() + Level);
            if (!leading_check || !trailing_check)
                throw std::logic_error("baddims");
            return arg.dims()[Level - 1];
        }
        else
            return size_t(0);
    }
}
template<size_t Level, size_t Rank, typename... Args>
void _join_dims_by_args(std::array<size_t, Rank>& dims, const Args&... args)
{
    dims[Level - 1] += (_join_dims_by_args_impl<Level>(dims, args) + ...);
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
        static_assert(1u <= Level && Level <= rank, "badlevel");
        auto ret_dims = first.dims();
        _join_dims_by_args<Level>(ret_dims, rest...);
        ndarray<value_type_t<FirstType>, rank> ret(ret_dims);
        if constexpr (Level == 1u)
        {
            auto ret_iter = ret.data();
            _join_copy_level1(ret_iter, first);
            (_join_copy_level1(ret_iter, rest), ...);
        }
        else
        {
            auto ret_iter = ret.data();
            auto stride = utils::size_of_dims(
                utils::dims_take<Level, rank>(ret_dims));
            _join_copy_leveln<Level, rank>(ret_iter, stride, first);
            (_join_copy_leveln<Level, rank>(ret_iter, stride, rest), ...);
        }
        return ret;
    }
}
template<typename First, typename... Rest>
auto join(First&& first, Rest&&... rest)
{
    return join(const_int<1>{}, std::forward<decltype(first)>(first),
        std::forward<decltype(rest)>(rest)...);
}
template<typename First, typename... Rest>
auto set_union(const First& first, const Rest&... rest)
{
    constexpr auto R = array_rank_v<First>;
    static_assert(((R == array_rank_v<Rest>) && ... && (R >= 1u)), "badrank");
    using T = value_type_t<First>;
    static_assert((std::is_same_v<T, value_type_t<Rest>> && ...), "badargtype");
    auto scalar_less = [](const auto& x, const auto& y)
    {
        return _order_scalar(x, y) == int64_t(1);
    };
    auto copy = join(first, rest...);
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
}
template<typename X>
auto _rotate_impl(const X& x, int64_t n)
{
    constexpr auto XR = array_rank_v<X>;
    static_assert(XR >= 1u, "badrank");
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
    static_assert(is_integral_v<N>, "badargtype");
    return _rotate_impl(x, -int64_t(n));
}
template<typename X, typename N>
auto rotate_right(const X& x, const N& n)
{
    static_assert(is_integral_v<N>, "badargtype");
    return _rotate_impl(x, int64_t(n));
}
template<typename X>
auto rotate_left(const X& x)
{
    return _rotate_impl(x, -1);
}
template<typename X>
auto rotate_right(const X& x)
{
    return _rotate_impl(x, 1);
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
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(1u <= Level && Level + YR == XR, "badlevel");
    const auto& valx = allows<view_category::Array>(x);
    const auto& valy = allows<view_category::Simple>(y);
    return position(valx, varg_tag{},
        [&](const auto& a) { return equal(a, valy); }, const_int<Level>{});
}
template<typename Ret = int64_t, typename X, typename Function, int64_t I>
auto position(const X& x, varg_tag, Function f, const_int<I>)
{
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, "badlevel");
    
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
}
template<typename Ret = int64_t, typename X, typename Y>
auto position(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR > YR, "badrank");
    return position(x, y, const_int<XR - YR>{});
}
template<typename X, typename Function, int64_t I>
auto cases(const X& x, varg_tag, Function f, const_int<I>)
{
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, "badlevel");
    using XV = value_type_t<X>;
    if constexpr (XR == Level)
    {
        using RT = remove_cvref_t<decltype(f(XV{}))>;
        static_assert(is_boolean_v<RT>, "badfunctype");
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
        static_assert(is_boolean_v<RT>, "badfunctype");
        ndarray<XV, XR - Level + 1u> ret;
        for (; view_iter != view_end; ++view_iter)
        {
            if (f(*view_iter))
                ret.append(*view_iter, dim_checked{});
        }
        return ret;
    }
}
template<typename X, typename Y, int64_t I>
auto cases(const X& x, const Y& y, const_int<I>)
{
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, "badlevel");
    if constexpr (XR == YR + Level)
    {
        const auto& valy = allows<view_category::Simple>(y);
        return cases(x, [&](const auto& a) { return same_q(a, valy); });
    }
    else
    {
        return ndarray<value_type_t<X>, XR - Level + 1u>{};
    }
}
template<typename X, typename Y>
auto cases(const X& x, const Y& y)
{
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(YR < XR, "badlevel");
    return cases(x, y, const_int<XR - YR>{});
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
    for (size_t m = 0u; m < M; ++m)
        _dot_vv(pz + m, px + m * K, py, K);
}
template<typename Z, typename X, typename Y>
auto _dot_vm(Z* WL_RESTRICT pz, const X* WL_RESTRICT px,
    const Y* WL_RESTRICT py, const size_t K, const size_t N)
{
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
    static_assert(is_numerical_type_v<X> && is_numerical_type_v<Y>,
        "badargtype");
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(XR >= 1u && YR >= 1u);
    using XV = value_type_t<X>;
    using YV = value_type_t<Y>;
    using C = common_type_t<XV, YV>;
    const auto& valx = allows<view_category::Simple>(x);
    const auto& valy = allows<view_category::Simple>(y);
    const auto* px = valx.data();
    const auto* py = valy.data();
    const auto K = valx.dims()[XR - 1u];
    if (K != valy.dims()[0])
        throw std::logic_error("baddims");
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
}
}
namespace wl
{
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
            throw std::logic_error("badargc");
        if constexpr (HasStride)
            return *(this->iter_ + i * stride_);
        else
            return *(this->iter_ + i);
    }
    auto get_pack(size_t i) const
    {
        if (i > size_)
            throw std::logic_error("badargc");
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
    template<typename Arg>
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
    static_assert(std::tuple_size_v<Args> <= MaximumArgCount, "internal");
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
    //static_assert(is_variadic_function_v<Function>, "badargtype");
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 1u, "badrank");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(0 <= Level && Level < int64_t(R), "badlevel");
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
                    throw std::logic_error("baddims");
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
            throw std::logic_error("badargc");
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
                    throw std::logic_error("baddims");
                item.copy_to(ret_iter.begin());
            }
            return ret;
        }
    }
}
template<typename Function, typename X>
auto apply(Function f, X&& x)
{
    return apply(f, std::forward<decltype(x)>(x), const_int<0>{});
}
template<typename T, size_t R, typename Function>
auto _select_impl(const ndarray<T, R>& a, Function f)
{
    static_assert(R >= 2u, "internal");
    ndarray<T, R> ret;
    auto view_iter = a.template view_begin<1u>();
    auto view_end = a.template view_end<1u>();
    size_t item_size = view_iter.size();
    for (; view_iter != view_end; ++view_iter)
    {
        auto out = f(*view_iter);
        static_assert(is_boolean_v<decltype(out)>, "badfunctype");
        if (out)
            ret.append(*view_iter);
    }
    return ret;
}
template<typename X, typename Function>
auto select(X&& x, Function f)
{
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> >= 1u, "badrank");
    if constexpr (array_rank_v<XT> == 1u)
    {
        std::vector<value_type_t<XT>> ret;
        x.for_each([&](const auto& a)
            {
                auto out = f(a);
                static_assert(is_boolean_v<decltype(out)>, "badfunctype");
                if (out) ret.push_back(a);
            });
        return ndarray<value_type_t<XT>, 1u>(
            std::array<size_t, 1u>{ret.size()}, std::move(ret));
    }
    else
        return _select_impl(std::forward<decltype(x)>(x).to_array(), f);
}
template<typename X, typename Function, int64_t I>
auto count(const X& x, varg_tag, Function f, const_int<I>)
{
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    static_assert(1u <= Level && Level <= XR, "badlevel");
    size_t item_count = 0;
    if constexpr (XR == Level)
    {
        using RT = remove_cvref_t<decltype(f(value_type_t<X>{}))>;
        static_assert(is_boolean_v<RT>, "badfunctype");
        x.for_each([&](const auto& a) { if (f(a)) ++item_count; });
    }
    else
    {
        const auto& valx = allows<view_category::Array>(x);
        auto view_iter = valx.template view_begin<Level>();
        const auto view_end = valx.template view_end<Level>();
        using RT = remove_cvref_t<decltype(f(*view_iter))>;
        static_assert(is_boolean_v<RT>, "badfunctype");
        for (; view_iter != view_end; ++view_iter)
        {
            if (f(*view_iter))
                ++item_count;
        }
    }
    return item_count;
}
template<typename X, typename Function>
auto count(const X& x, varg_tag, Function f)
{
    return count(x, varg_tag{}, f, const_int<1>{});
}
template<typename X, typename Y, int64_t I>
auto count(const X& x, const Y& y, const_int<I>)
{
    constexpr auto Level = I > 0 ? size_t(I) : size_t(0);
    constexpr auto XR = array_rank_v<X>;
    constexpr auto YR = array_rank_v<Y>;
    static_assert(1u <= Level && Level + YR == XR, "badlevel");
    size_t item_count = 0;
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
}
template<typename X, typename Y>
auto count(const X& x, const Y& y)
{
    return count(x, y, const_int<1>{});
}
template<size_t Level, typename Function, typename T, size_t R>
auto _map_impl(Function f, const ndarray<T, R>& a)
{
    static_assert(1 <= Level && Level <= R, "badlevel");
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
                throw std::logic_error("baddims");
            item.copy_to(ret_iter.begin());
        }
        return ret;
    }
}
template<typename Function, typename X, int64_t I>
auto map(Function f, X&& x, const_int<I>)
{
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 1, "badrank");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(1 <= Level && Level <= int64_t(R), "badlevel");
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
}
template<typename Function, typename X>
auto map(Function f, X&& x)
{
    return map(f, std::forward<decltype(x)>(x), const_int<1>{});
}
template<typename Function, typename X, int64_t I>
auto scan(Function f, X&& x, const_int<I>)
{
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 1, "badrank");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(1 <= Level && Level <= int64_t(R), "badlevel");
    const auto& valx = val(std::forward<decltype(x)>(x));
    auto x_iter = valx.template view_begin<Level>();
    const auto x_end = valx.template view_end<Level>();
    for (; x_iter != x_end; ++x_iter)
        f(*x_iter);
    return const_null;
}
template<typename Function, typename X>
auto scan(Function f, X&& x)
{
    return scan(f, std::forward<decltype(x)>(x), const_int<1>{});
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
        (++iters, ...);
        ++ret_iter;
        for (size_t i = 1; i < ret_size; ++i, ++ret_iter, (++iters, ...))
        {
            auto item = f(*iters...);
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error("baddims");
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
        throw std::logic_error("baddims");
    return _map_thread_impl2(f, dims, arg1.template view_begin<size_t(I)>(),
        args.template view_begin<size_t(I)>()...);
}
template<typename Function, int64_t I, typename... Args>
auto map_thread(Function f, const_int<I>, varg_tag, Args&&... args)
{
    static_assert(sizeof...(Args) >= 1u, "badargc");
    static_assert(1 <= I, "badlevel");
    static_assert(((array_rank_v<remove_cvref_t<Args>> >= size_t(I)) && ...),
        "badargtype");
    return _map_thread_impl1<size_t(I)>(f,
        val(std::forward<decltype(args)>(args))...);
}
template<typename Function, typename... Args>
auto map_thread(Function f, varg_tag, Args&&... args)
{
    return map_thread(f, const_int<1>{}, varg_tag{},
        std::forward<decltype(args)>(args)...);
}
template<typename Function, typename X, int64_t I>
auto map_thread(Function f, X&& x, const_int<I>)
{
    using XT = remove_cvref_t<X>;
    constexpr auto R = array_rank_v<XT>;
    static_assert(R >= 2u, "badrank");
    constexpr int64_t Level = I >= 0 ? I : I + int64_t(R) + 1;
    static_assert(1 <= Level && Level < int64_t(R), "badlevel");
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
                    throw std::logic_error("baddims");
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
            throw std::logic_error("badargc");
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
                    throw std::logic_error("baddims");
                item.copy_to(ret_iter.begin());
            }
            return ret;
        }
    }
}
template<typename Function, typename X>
auto map_thread(Function f, X&& x)
{
    return map_thread(f, std::forward<decltype(x)>(x), const_int<1>{});
}
template<typename Function, typename X>
auto nest(Function f, X&& x, const int64_t n)
{
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        "badargtype");
    constexpr auto rank = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error("badargv");
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
}
template<typename Function, typename X>
auto nest_list(Function f, X&& x, const int64_t n)
{
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        "badargtype");
    constexpr auto rank = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error("badargv");
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
                throw std::logic_error("baddims");
            item.copy_to(view_iter.begin());
            ++view_iter;
            for (size_t i = 2; i <= size_t(n); ++i, ++view_iter)
            {
                auto temp = val(f(std::move(item)));
                item = std::move(temp);
                if (!utils::check_dims(item.dims(), item_dims))
                    throw std::logic_error("baddims");
                item.copy_to(view_iter.begin());
            }
            return ret;
        }
    }
}
template<typename T, size_t R>
struct _nest_while_queue
{
    static constexpr auto _is_scalar = (R == 0u);
    using _elem_t = std::conditional_t<_is_scalar, T, ndarray<T, R>>;
    const size_t size_;
    size_t current_;
    std::vector<_elem_t> queue_;
    _nest_while_queue(size_t size) :
        size_{size}, current_ {0u}, queue_(size)
    {
    }
    template<typename X>
    void push(X&& x)
    {
        using XT = remove_cvref_t<X>;
        static_assert(array_rank_v<XT> == R, "badrank");
        ++current_;
        if (current_ == size_)
            current_ = 0u;
        auto& dst = queue_[current_];
        if constexpr (_is_scalar)
        {
            static_assert(is_convertible_v<XT, T>, "badargtype");
            dst = std::forward<decltype(x)>(x);
        }
        else
        {
            using XV = value_type_t<XT>;
            static_assert(is_convertible_v<XV, T>, "badargtype");
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
        return std::move(
            queue_.at(current_ - i + (i > current_ ? size_ : size_t(0))));
    }
    template<typename Fn, size_t... Is>
    auto _apply_test_impl(Fn&& fn, std::index_sequence<Is...>) const
    {
        auto res = std::forward<decltype(fn)>(fn)(
            queue_[current_ - Is + (Is > current_ ? size_ : size_t(0))]...);
        static_assert(is_boolean_v<decltype(res)>, "badfunctype");
        return res;
    }
    template<size_t N, typename Fn>
    auto apply_test(Fn&& fn) const
    {
        return _apply_test_impl(std::forward<decltype(fn)>(fn),
            std::make_index_sequence<N>{});
    }
};
template<typename Function, typename X, typename Test, int64_t N>
auto nest_while(Function f, X&& x, Test test, const_int<N>,
    const int64_t input_max = const_int_infinity,
    const int64_t offset = 0u)
{
    static_assert(0 <= N && N <= MaximumArgCount, "badargv");
    const auto history_size = size_t(std::max(N, -offset + 1));
    const auto max_steps = std::max(input_max, int64_t(0));
    constexpr auto num_args = size_t(N);
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        "badargtype");
    constexpr auto XR = array_rank_v<XT0>;
    if (max_steps < num_args)
    {
        return nest(f, std::forward<decltype(x)>(x), max_steps);
    }
    else if (num_args <= 1u && offset == 0u)
    {
        auto ret = cast<XT2>(std::forward<decltype(x)>(x));
        bool continue_flag = false;
        if constexpr (num_args == 0u)
            continue_flag = test();
        else if constexpr (num_args == 1u)
            continue_flag = test(ret);
        for (int64_t i = 1; i <= max_steps && continue_flag; ++i)
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
        }
        return ret;
    }
    else
    {
        using XV = std::conditional_t<XR == 0u, XT2, value_type_t<XT2>>;
        _nest_while_queue<XV, XR> queue(history_size);
        queue.push(std::forward<decltype(x)>(x));
        int64_t i = 1;
        for (; i < int64_t(num_args); ++i)
        {
            queue.push(f(queue.last()));
        }
        bool continue_flag = queue.template apply_test<num_args>(test);
        for (; i <= max_steps && continue_flag; ++i)
        {
            queue.push(f(queue.last()));
            continue_flag = queue.template apply_test<num_args>(test);
        }
        if (offset <= -i)
            throw std::logic_error("badargv");
        else if (offset <= 0)
            return std::move(queue).get(size_t(-offset));
        else
            return nest(f, std::move(queue).last(), offset);
    }
}
template<typename Function, typename X, typename Test>
auto nest_while(Function f, X&& x, Test test)
{
    return nest_while(f, std::forward<decltype(x)>(x), test, const_int<1>{});
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
    const int64_t input_max = const_int_infinity)
{
    static_assert(0 <= N && N <= MaximumArgCount, "badargv");
    const auto max_steps = std::max(input_max, int64_t(0));
    constexpr auto num_args = size_t(N);
    using XT0 = remove_cvref_t<decltype(val(x))>;
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    static_assert(is_convertible_v<XT0, XT2> && std::is_same_v<XT1, XT2>,
        "badargtype");
    constexpr auto XR = array_rank_v<XT0>;
    if (max_steps < num_args)
    {
        return nest_list(f, std::forward<decltype(x)>(x), max_steps);
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
        if constexpr (num_args == 0u)
            continue_flag = test();
        else if constexpr (num_args == 1u)
            continue_flag = test(last);
        else
            continue_flag = _nest_while_list_apply_test(test,
                ret.begin() + i - 1, std::make_index_sequence<num_args>{});
        for (; i <= max_steps && continue_flag; ++i)
        {
            auto temp = f(std::move(last));
            last = std::move(temp);
            ret.append(last);
            if constexpr (num_args == 0u)
                continue_flag = test();
            else if constexpr (num_args == 1u)
                continue_flag = test(last);
            else
                continue_flag = _nest_while_list_apply_test(test,
                    ret.begin() + i, std::make_index_sequence<num_args>{});
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
                throw std::logic_error("baddims");
            ret.append(last);
        }
        bool continue_flag = false;
        if constexpr (num_args == 0u)
            continue_flag = test();
        else if constexpr (false && num_args == 1u)
            continue_flag = test(last);
        else
            continue_flag = _nest_while_list_apply_test(test,
                ret.template view_begin<1u>() + i - 1,
                std::make_index_sequence<num_args>{});
        for (; i <= max_steps && continue_flag; ++i)
        {
            auto temp = val(f(std::move(last)));
            last = std::move(temp);
            if (!utils::check_dims(item_dims, last.dims()))
                throw std::logic_error("baddims");
            ret.append(last);
            if constexpr (num_args == 0u)
                continue_flag = test();
            else if constexpr (false && num_args == 1u)
                continue_flag = test(last);
            else
                continue_flag = _nest_while_list_apply_test(test,
                    ret.template view_begin<1u>() + i,
                    std::make_index_sequence<num_args>{});
        }
        return ret;
    }
}
template<typename Function, typename X, typename Test>
auto nest_while_list(Function f, X&& x, Test test)
{
    return nest_while_list(
        f, std::forward<decltype(x)>(x), test, const_int<1>{});
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
    static_assert(array_rank_v<remove_cvref_t<Y>> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if (n < 1u) throw std::logic_error("baddims");
    const auto& valy = val(std::forward<decltype(y)>(y));
    if constexpr (List)
    {
        if constexpr (FoldL)
            return _fold_list_impl(f, *valy.template view_begin<1u>(),
                valy.template view_begin<1u>(),
                [](auto& iter) { ++iter; }, n);
        else
            return _fold_list_impl(f, *(--(valy.template view_end<1u>())),
                --(valy.template view_end<1u>()),
                [](auto& iter) { --iter; }, n);
    }
    else
    {
        if constexpr (FoldL)
            return _fold_single_impl(f, *valy.template view_begin<1u>(),
                valy.template view_begin<1u>(),
                [](auto& iter) { ++iter; }, n);
        else
            return _fold_single_impl(f, *(--(valy.template view_end<1u>())),
                --(valy.template view_end<1u>()),
                [](auto& iter) { --iter; }, n);
    }
}
template<bool List, bool FoldL, typename Function, typename X, typename Y>
auto _fold_impl1(Function f, X&& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<YT> >= 1u, "badargtype");
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
                --(valy.template view_end<1u>()),
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
                --(valy.template view_end<1u>()),
                [](auto& iter) { --iter; }, n);
    }
}
template<typename Function, typename... Any>
auto fold(Function f, Any&&... any)
{
    return _fold_impl1<false, true>(f, std::forward<decltype(any)>(any)...);
}
template<typename Function, typename... Any>
auto foldr(Function f, Any&&... any)
{
    return _fold_impl1<false, false>(f, std::forward<decltype(any)>(any)...);
}
template<typename Function, typename... Any>
auto fold_list(Function f, Any&&... any)
{
    return _fold_impl1<true, true>(f, std::forward<decltype(any)>(any)...);
}
template<typename Function, typename... Any>
auto foldr_list(Function f, Any&&... any)
{
    return _fold_impl1<true, false>(f, std::forward<decltype(any)>(any)...);
}
template<typename Function, typename X, typename Pred>
auto fixed_point(Function f, X&& x, const int64_t max, varg_tag, Pred pred)
{
    return nest_while(f, std::forward<decltype(x)>(x),
        [=](const auto& a, const auto& b)
        {
            auto same = pred(a, b);
            static_assert(is_boolean_v<decltype(same)>, "badfunctype");
            return !same;
        }, const_int<2>{}, max);
}
template<typename Function, typename X, typename Pred>
auto fixed_point(Function f, X&& x, varg_tag, Pred pred)
{
    return fixed_point(f, std::forward<decltype(x)>(x),
        const_int_infinity, varg_tag{}, pred);
}
template<typename Function, typename X>
auto fixed_point(Function f, X&& x, const int64_t max)
{
    return nest_while(f, std::forward<decltype(x)>(x),
        [=](const auto& a, const auto& b) { return unequal(a, b); },
        const_int<2>{}, max);
}
template<typename Function, typename X>
auto fixed_point(Function f, X&& x)
{
    return fixed_point(f, std::forward<decltype(x)>(x), const_int_infinity);
}
template<typename Function, typename X, typename Pred>
auto fixed_point_list(Function f, X&& x, const int64_t max,
    varg_tag, Pred pred)
{
    return nest_while_list(f, std::forward<decltype(x)>(x),
        [=](const auto& a, const auto& b)
        {
            auto same = pred(a, b);
            static_assert(is_boolean_v<decltype(same)>, "badfunctype");
            return !same;
        }, const_int<2>{}, max);
}
template<typename Function, typename X, typename Pred>
auto fixed_point_list(Function f, X&& x, varg_tag, Pred pred)
{
    return fixed_point_list(f, std::forward<decltype(x)>(x),
        const_int_infinity, varg_tag{}, pred);
}
template<typename Function, typename X>
auto fixed_point_list(Function f, X&& x, const int64_t max)
{
    return nest_while_list(f, std::forward<decltype(x)>(x),
        [=](const auto& a, const auto& b) { return unequal(a, b); },
        const_int<2>{}, max);
}
template<typename Function, typename X>
auto fixed_point_list(Function f, X&& x)
{
    return fixed_point_list(f, std::forward<decltype(x)>(x),
        const_int_infinity);
}
template<typename Arg>
auto identity(Arg&& arg) -> decltype(auto)
{
    return std::forward<decltype(arg)>(arg);
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
    return _composition_impl<false>(std::forward<decltype(fn)>(fn)...);
}
template<typename... Fn>
auto right_composition(Fn&&... fn) -> decltype(auto)
{
    return _composition_impl<true>(std::forward<decltype(fn)>(fn)...);
}
template<typename X, typename Test, int64_t I>
auto all_true(X&& x, Test test, const_int<I>)
{
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, "badrank");
    static_assert(1 <= I && I <= XR, "badlevel");
    const auto& valx = val(std::forward<decltype(x)>(x));
    auto x_iter = valx.template view_begin<I>();
    const auto x_end = valx.template view_end<I>();
    static_assert(is_boolean_v<remove_cvref_t<decltype(test(*x_iter))>>,
        "badfunctype");
    auto ret = true;
    for (; ret && x_iter != x_end; ++x_iter)
        ret = ret && test(*x_iter);
    return boolean(ret);
}
template<typename X, typename Test>
auto all_true(X&& x, Test test)
{
    return all_true(std::forward<decltype(x)>(x), test, const_int<1>{});
}
template<typename X, typename Test, int64_t I>
auto any_true(X&& x, Test test, const_int<I>)
{
    using XT = remove_cvref_t<X>;
    constexpr auto XR = array_rank_v<XT>;
    static_assert(XR >= 1u, "badrank");
    static_assert(1 <= I && I <= XR, "badlevel");
    const auto& valx = val(std::forward<decltype(x)>(x));
    auto x_iter = valx.template view_begin<I>();
    const auto x_end = valx.template view_end<I>();
    static_assert(is_boolean_v<remove_cvref_t<decltype(test(*x_iter))>>,
        "badfunctype");
    auto ret = false;
    for (; !ret && x_iter != x_end; ++x_iter)
        ret = ret || test(*x_iter);
    return boolean(ret);
}
template<typename X, typename Test>
auto any_true(X&& x, Test test)
{
    return any_true(std::forward<decltype(x)>(x), test, const_int<1>{});
}
template<typename X, typename Test, int64_t I>
auto none_true(X&& x, Test test, const_int<I>)
{
    return !any_true(std::forward<decltype(x)>(x), test, const_int<I>{});
}
template<typename X, typename Test>
auto none_true(X&& x, Test test)
{
    return none_true(std::forward<decltype(x)>(x), test, const_int<1>{});
}
template<typename X, typename Test>
auto vector_q(X&& x, Test test)
{
    if constexpr (array_rank_v<remove_cvref_t<X>> == 0u)
        return const_false;
    else
        return all_true(std::forward<decltype(x)>(x), test, const_int<1>{});
}
template<typename X>
auto vector_q(X&& x)
{
    return boolean(array_rank_v<remove_cvref_t<X>> == 1u);
}
template<typename X, typename Test>
auto matrix_q(X&& x, Test test)
{
    if constexpr (array_rank_v<remove_cvref_t<X>> <= 1u)
        return const_false;
    else
        return all_true(std::forward<decltype(x)>(x), test, const_int<2>{});
}
template<typename X>
auto matrix_q(X&& x)
{
    return boolean(array_rank_v<remove_cvref_t<X>> == 2u);
}
}
