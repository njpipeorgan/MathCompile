#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <complex>
#include <type_traits>
#include <string>
#include <tuple>
#include <exception>
#include <iterator>
#include <optional>
#include <vector>
#include <cassert>
#include <array>
#include <numeric>
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
template<typename...>
constexpr auto always_false_v = false;
template<typename T> struct _type_to_index : std::integral_constant<int, -1> {};
template<> struct _type_to_index<double> : std::integral_constant<int, 1> {};
template<> struct _type_to_index<float> : std::integral_constant<int, 2> {};
template<> struct _type_to_index<uint64_t> : std::integral_constant<int, 3> {};
template<> struct _type_to_index<int64_t> : std::integral_constant<int, 4> {};
template<> struct _type_to_index<uint32_t> : std::integral_constant<int, 5> {};
template<> struct _type_to_index<int32_t> : std::integral_constant<int, 6> {};
template<> struct _type_to_index<uint16_t> : std::integral_constant<int, 7> {};
template<> struct _type_to_index<int16_t> : std::integral_constant<int, 8> {};
template<> struct _type_to_index<uint8_t> : std::integral_constant<int, 9> {};
template<> struct _type_to_index<int8_t> : std::integral_constant<int, 10> {};
template<int I> struct _index_to_type { using type = void; };
template<> struct _index_to_type<1> { using type = double; };
template<> struct _index_to_type<2> { using type = float; };
template<> struct _index_to_type<3> { using type = uint64_t; };
template<> struct _index_to_type<4> { using type = int64_t; };
template<> struct _index_to_type<5> { using type = uint32_t; };
template<> struct _index_to_type<6> { using type = int32_t; };
template<> struct _index_to_type<7> { using type = uint16_t; };
template<> struct _index_to_type<8> { using type = int16_t; };
template<> struct _index_to_type<9> { using type = uint8_t; };
template<> struct _index_to_type<10> { using type = int8_t; };
template<typename... Ts>
struct common_type;
template<typename T, typename U>
struct common_type<T, U> :
    _index_to_type<std::min(_type_to_index<T>::value,
        _type_to_index<U>::value)> {};
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
struct is_convertible
{
    static constexpr bool value = is_complex_v<U> || is_integral_v<T> ||
        (is_float_v<T> && is_float_v<U>);
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
namespace literal
{
inline auto operator ""_i(unsigned long long i)
{
    return int64_t(i);
}
inline auto operator ""_r(long double r)
{
    return double(r);
}
inline auto operator ""_s(const char* x, size_t)
{
    return std::string(x);
}
}
}
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
namespace utils
{
#define WL_FUNCTION(name) ([](auto&&... args) { return name(std::forward<decltype(args)>(args)...); })
#define WL_PASS(var) std::forward<decltype(var)>(var)
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
template<size_t... Is>
auto _size_of_dims_impl(const size_t* dims, std::index_sequence<Is...>)
{
    return (dims[Is] * ...);
}
template<size_t R>
auto size_of_dims(const size_t* dims)
{
    if constexpr (R == 0u)
        return size_t(1);
    else
        return _size_of_dims_impl(dims, std::make_index_sequence<R>{});
}
template<size_t R>
auto size_of_dims(const std::array<size_t, R>& dims)
{
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
    static_assert(R >= 1u, "internal");
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
template<typename IndexType>
size_t convert_index(const IndexType& idx, const size_t& dim)
{
    static_assert(is_integral_v<IndexType>, "badidxtype");
    if constexpr (std::is_unsigned_v<IndexType>)
    {
        if (1u <= idx && idx <= dim)
            return size_t(idx - 1u);
        else
            throw std::logic_error("badindex");
    }
    else
    {
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
    template<typename IndexType>
    scalar_indexer(IndexType index, size_t dim) :
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
    else
    {
        static_assert(is_integral_v<IndexType>, "badidxtype");
        return scalar_indexer(index, dim);
    }
}
struct level_iter_tag {};
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
    using _dims_t = std::array<size_t, ViewRank>;
    const T* const identifier_;
    pointer_type data_;
    _dims_t dims_;
    size_t size_;
    simple_view(level_iter_tag, 
        array_ref_type base, ptrdiff_t offset, const size_t* dims) :
        identifier_{base.data()}, data_{base.data() + offset}
    {
        std::copy_n(dims, ViewRank, this->dims_.begin());
        this->size_ = utils::size_of_dims(this->dims_);
    }
    template<typename... Specs>
    simple_view(array_ref_type base, const Specs&... specs) :
        identifier_{base.data()}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        size_t offset = 0u;
        this->_initialize<0u>(offset, base.dims_ptr(), specs...);
        this->data_ = base.data() + offset;
    }
    template<size_t Level, typename Spec1, typename... Specs>
    void _initialize(size_t& offset, const size_t* dims,
        const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level > 0)
            offset *= dims[Level];
        offset += spec1.offset();
        if constexpr (Level < ArrayRank - ViewRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->_initialize<Level + 1u>(offset, dims, specs...);
        }
        else
        {
            static_assert(!std::is_same_v<Spec1, scalar_indexer>, "internal");
            constexpr size_t ViewLevel = Level - (ArrayRank - ViewRank);
            this->dims_[ViewLevel] = spec1.size();
            this->size_ *= this->dims_[ViewLevel];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(offset, dims, specs...);
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
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i)
        {
            if constexpr (std::is_same_v<bool, decltype(f(*ptr, *iters...))>)
            {
                if (f(*ptr++, (*iters++)...))
                    break;
            }
            else
                f(*ptr++, (*iters++)...);
        }
    }
    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        auto ptr = this->data_;
        for (size_t i = 0u; i < this->size_; ++i)
            *iter++ = *ptr++;
    }
    template<typename FwdIter>
    void copy_from(FwdIter iter) const
    {
        auto ptr = this->data_;
        for (size_t i = 0u; i < this->size_; ++i)
            *ptr++ = *iter++;
    }
    auto to_array() const
    {
        return ndarray<T, ViewRank>(this->dims_,
            this->begin(), this->begin() + this->size());
    }
    auto apply_pointer_offset(ptrdiff_t diff)
    {
        this->data_ += diff;
        return *this;
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
    using _dims_t = std::array<size_t, ViewRank>;
    const T* const identifier_;
    pointer_type data_;
    ptrdiff_t stride_;
    _dims_t dims_;
    size_t size_;
    template<typename... Specs>
    regular_view(array_ref_type base, const Specs&... specs) :
        identifier_{base.data()}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        this->stride_ = 1;
        size_t offset = 0u;
        this->_initialize<0u>(offset, base.dims_ptr(), specs...);
        this->data_ = base.data() + offset;
    }
    template<size_t Level, typename Spec1, typename... Specs>
    auto _initialize(size_t& offset, const size_t* dims,
        const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level > 0)
            offset *= dims[Level];
        offset += spec1.offset();
        if constexpr (Level < ArrayRank - ViewRank - StrideRank)
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->_initialize<Level + 1>(offset, dims, specs...);
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
                _initialize<Level + 1u>(offset, dims, specs...);
        }
        else
        {
            static_assert(std::is_same_v<Spec1, scalar_indexer>, "internal");
            this->stride_ *= dims[Level];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u>(offset, dims, specs...);
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
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i, ptr += this->stride_)
        {
            if constexpr (std::is_same_v<bool, decltype(f(*ptr, *iters...))>)
            {
                if (f(*ptr, (*iters++)...))
                    break;
            }
            else
                f(*ptr, (*iters++)...);
        }
    }
    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size_; ++i, ptr += this->stride_)
            *iter++ = *ptr;
    }
    template<typename FwdIter>
    void copy_from(FwdIter iter) const
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size_; ++i, ptr += this->stride_)
            *ptr = *iter++;
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
    using _dims_t = std::array<size_t, ViewRank>;
    using _strides_t = std::array<ptrdiff_t, ViewRank>;
    using _indexers_tuple = IndexersTuple;
    static constexpr auto _has_last_stride = (StrideRank != 0) ||
        std::is_same_v<
        std::tuple_element_t<ViewRank - 1u, IndexersTuple>,
        step_indexer>;
    const T* const identifier_;
    pointer_type data_;
    _strides_t strides_;
    _indexers_tuple indexers_;
    _dims_t dims_;
    size_t size_;
    template<typename... Specs>
    general_view(array_ref_type base, const Specs&... specs) :
        identifier_{base.data()}, size_{1u}
    {
        static_assert(sizeof...(Specs) == ArrayRank, "internal");
        size_t offset = 0u;
        this->_initialize<0u, 0u>(offset, base.dims_ptr(), specs...);
        this->data_ = base.data() + offset;
    }
    template<size_t Level, size_t ViewLevel, typename Spec1, typename... Specs>
    auto _initialize(size_t& offset, const size_t* dims,
        const Spec1& spec1, const Specs&... specs)
    {
        if constexpr (Level > 0)
            offset *= dims[Level];
        offset += spec1.offset();
        if constexpr (std::is_same_v<Spec1, scalar_indexer>)
        {
            if constexpr (ViewLevel > 0u)
                this->strides_[ViewLevel - 1] *= dims[Level];
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u, ViewLevel>(offset, dims, specs...);
        }
        else
        {
            if constexpr (ViewLevel > 0u)
                this->strides_[ViewLevel - 1] *= dims[Level];
            if constexpr (std::is_same_v<Spec1, step_indexer>)
                this->strides_[ViewLevel] = spec1.stride();
            else
                this->strides_[ViewLevel] = 1;
            this->dims_[ViewLevel] = spec1.size();
            this->size_ *= this->dims_[ViewLevel];
            std::get<ViewLevel>(this->indexers_) = spec1;
            if constexpr (Level < ArrayRank - 1u)
                _initialize<Level + 1u, ViewLevel + 1u>(
                    offset, dims, specs...);
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
                        if (f(this->data_[(index + i) * last_stride], (*iters++)...))
                            break;
                    }
                    else
                        f(this->data_[(index + i) * last_stride], (*iters++)...);
                }
            else
                for (size_t i = 0; i < this->dims_[ViewLevel]; ++i)
                {
                    if constexpr (check_break)
                    {
                        if (f(this->data_[(index + i) * last_stride], (*iters++)...))
                            break;
                    }
                    else
                        f(this->data_[(index + i) * last_stride], (*iters++)...);
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
auto val(Any&& any)
{
    if constexpr (is_array_view_v<remove_cvref_t<Any>>)
        return std::forward<decltype(any)>(any).to_array();
    else
        return std::forward<decltype(any)>(any);
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
    if constexpr (std::is_same_v<typename Dst::value_type,
        typename Src::value_type>)
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
template<typename T, size_t R>
struct ndarray
{
    static_assert(1u <= R && R <= 1024u, "badrank");
    static_assert(std::is_same_v<T, remove_cvref_t<T>>, "internal");
    static_assert(is_arithmetic_v<T> || is_boolean_v<T> || is_string_v<T>, 
        "badargtype");
    using value_type = T;
    static constexpr auto rank = R;
    using _dims_t = std::array<size_t, R>;
    static constexpr auto category = view_category::Array;
    _dims_t dims_;
    std::vector<T> data_;
    ndarray() : dims_{{0u}}, data_{}
    {
    }
    template<typename DimsT>
    ndarray(std::array<DimsT, rank> dims, const T& val = T{}) :
        dims_{}, data_(_input_dims_size(dims), val)
    {
        std::copy(dims.begin(), dims.end(), dims_.begin());
    }
    template<typename DimsT>
    ndarray(ndarray<DimsT, 1> dims, const T& val = T{}) :
        dims_{}, data_(_input_dims_size(dims), val)
    {
        std::copy(dims.begin(), dims.end(), dims_.begin());
    }
    template<typename FwdIter>
    ndarray(std::array<size_t, rank> dims, FwdIter begin, FwdIter end) :
        dims_{dims}, data_(begin, end)
    {
    }
    ndarray(std::array<size_t, rank> dims, std::vector<T>&& movable) :
        dims_{dims}, data_(std::move(movable))
    {
    }
    template<typename Dims>
    static size_t _input_dims_size(const Dims& dims)
    {
        static_assert(is_integral_v<typename Dims::value_type>, "badargtype");
        if (dims.size() != rank)
            throw std::logic_error("baddims");
        size_t size = 1u;
        for (const auto& d : dims)
        {
            if constexpr (std::is_signed_v<typename Dims::value_type>)
                if (d < 0)
                    throw std::logic_error("baddims");
            size *= d;
        }
        return size;
    }
    size_t size() const
    {
        return data_.size();
    }
    template<size_t Level>
    size_t partial_size() const
    {
        static_assert(Level <= R, "internal");
        if constexpr (Level == R)
            return 1u;
        else
            return this->dims_[Level] * this->partial_size<Level + 1u>();
    }
    // uses one-based indexing
    template<size_t Level>
    size_t dimension() const
    {
        static_assert(1 <= Level && Level <= R, "internal");
        return this->dims_[Level - 1];
    }
    auto dims() const
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
    auto data_vector() const & -> const std::vector<T>&
    {
        return this->data_;
    }
    auto data_vector() & -> std::vector<T>&
    {
        return this->data_;
    }
    auto data_vector() && -> std::vector<T>&&
    {
        return std::move(this->data_);
    }
    auto identifier() const
    {
        return this->data();
    }
    T& operator[](size_t i)
    {
        return data_[i];
    }
    const T& operator[](size_t i) const
    {
        return data_[i];
    }
    auto begin() { return data_.begin(); }
    auto end() { return data_.end(); }
    auto begin() const { return data_.begin(); }
    auto end() const { return data_.end(); }
    template<size_t Level>
    auto view_begin()
    {
        static_assert(Level <= R, "internal");
        if constexpr (Level == R)
            return this->begin();
        else
            return simple_view<T, R, R - Level, false>(level_iter_tag{}, 
                *this, 0, this->dims_.data() + Level);
    }
    template<size_t Level>
    auto view_end()
    {
        if constexpr (Level == R)
            return this->end();
        else
            return this->view_begin<Level>().apply_pointer_offset(
                this->size());
    }
    template<size_t Level>
    auto view_begin() const
    {
        static_assert(Level <= R, "internal");
        if constexpr (Level == R)
            return this->begin();
        else
            return simple_view<T, R, R - Level, true>(level_iter_tag{}, 
                *this, 0, this->dims_.data() + Level);
    }
    template<size_t Level>
    auto view_end() const
    {
        if constexpr (Level == R)
            return this->end();
        else
            return this->view_begin<Level>().apply_pointer_offset(
                this->size());
    }
    void resize(size_t new_dim0, ptrdiff_t size_diff)
    {
        assert(size_diff ==
            ptrdiff_t((new_dim0 - dims_[0]) * partial_size<1u>()));
        this->data_.resize(data_.size() + size_diff);
        this->dims_[0] = new_dim0;
    }
    template<size_t Level, typename... Is>
    void linear_pos_impl(size_t& pos,
        const size_t& i1, const Is&... is) const
    {
        pos += i1;
        if constexpr (Level < R - 1)
            pos *= this->dims_[Level + 1u];
        if constexpr (Level < R - 1)
            linear_pos_impl<Level + 1u>(pos, is...);
    }
    template<typename... Is>
    size_t linear_pos(const Is&... is) const
    {
        size_t pos = 0u;
        linear_pos_impl<0u>(pos, is...);
        return pos;
    }
    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters)
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i)
        {
            if constexpr (std::is_same_v<bool, decltype(f(*ptr, *iters...))>)
            {
                if (f(*ptr++, (*iters++)...))
                    break;
            }
            else
                f(*ptr++, (*iters++)...);
        }
    }
    template<typename Function, typename... Iters>
    void for_each(Function f, Iters... iters) const
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i)
        {
            if constexpr (std::is_same_v<bool, decltype(f(*ptr, *iters...))>)
            {
                if (f(*ptr++, (*iters++)...))
                    break;
            }
            else
                f(*ptr++, (*iters++)...);
        }
    }
    template<typename FwdIter>
    void copy_to(FwdIter iter) const
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i)
            *iter++ = *ptr++;
    }
    template<typename FwdIter>
    void copy_from(FwdIter iter) &
    {
        auto ptr = this->data();
        for (size_t i = 0u; i < this->size(); ++i)
            *ptr++ = *iter++;
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
template<typename T, typename X>
auto cast(X&& x)
{
    static_assert(is_real_v<remove_cvref_t<X>>, "internal");
    return static_cast<T>(x);
}
template<typename T, typename X>
auto cast(const complex<X>& x)
{
    if constexpr (is_real_v<T>)
        return std::real(x);
    else
        return std::complex<value_type_t<T>>(x);
}
template<typename T, typename X>
auto cast(complex<X>&& x)
{
    if constexpr (is_real_v<T>)
        return std::real(x);
    else
        return std::complex<value_type_t<T>>(x);
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
    constexpr auto x_rank = array_rank_v<XT>;
    if constexpr (x_rank == 0)
    {
        static_assert(is_arithmetic_v<XT>, "badargtype");
        if constexpr (std::is_unsigned_v<XT>)
            return x;
        else
            return std::abs(x);
    }
    else
    {
        using XVT = typename XT::value_type;
        if constexpr (std::is_unsigned_v<XT>)
            return std::forward<decltype(x)>(x);
        else if constexpr (is_real_v<XVT>&& is_movable_v<X&&>)
        { // movable
            ndarray<XVT, x_rank> ret(std::move(x));
            ret.for_each([](auto& src) { src = abs(src); });
            return ret;
        }
        else
        {
            ndarray<decltype(abs(XVT{})), x_rank > ret(x.dims());
            x.for_each(
                [](const auto& src, auto& dst) { dst = abs(src); },
                ret.begin());
            return ret;
        }
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
    constexpr auto x_rank = array_rank_v<X>;
    constexpr auto y_rank = array_rank_v<Y>;
    static_assert(x_rank == y_rank, "badrank");
    if constexpr (x_rank == 0)
    {
        static_assert((is_arithmetic_v<X> && is_arithmetic_v<X>) ||
            (is_value_type_v<X>, std::is_same_v<X, Y>), "badargtype");
        if constexpr (is_complex_v<X>)
            return boolean(x == X(y));
        else if constexpr (is_complex_v<Y>)
            return boolean(Y(x) == y);
        else
            return boolean(x == y);
    }
    else
    {
        if (!utils::check_dims<x_rank>(x.dims_ptr(), y.dims_ptr()))
            return false;
        if constexpr (X::category != view_category::General)
        {
            boolean equal_flag = true;
            y.for_each([&](const auto& a, const auto& b)
                {
                    equal_flag = equal(a, b);
                    return !equal_flag;
                }, x.begin());
            return equal_flag;
        }
        else if constexpr (Y::category != view_category::General)
        {
            return equal(y, x);
        }
        else
        {
            if constexpr (sizeof(typename X::value_type) <
                sizeof(typename Y::value_type))
                return equal(x.to_array, y);
            else
                return equal(x, y.to_array);
        }
    }
}
template<typename X, typename Y>
boolean unequal(const X& x, const Y& y)
{
    return !equal(x, y);
}
template<typename X, typename Y>
auto mod(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    auto scalar_mod = [](const auto& x, const auto& y)
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
                C rem = xi % yi;
                if (std::is_signed_v<C> && (rem != 0))
                    rem += yi & ((xi ^ yi) >> (8u * sizeof(C) - 1u));
                return rem;
            }
        }
        else
        {
            using C = common_type_t<XV, YV>;
            if (y == 0)
                return C(0);
            else
            {
                auto xi = C(x);
                auto yi = C(y);
                C rem = std::fmod(xi, yi);
                if (rem != 0 && ((xi > 0) ^ (yi > 0)))
                    rem += yi;
                return rem;
            }
        }
    };
    return utils::listable_function(scalar_mod,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
template<typename Ret, typename X>
auto sign(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto scalar_sign = [](const auto& x)
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
    return utils::listable_function(scalar_sign,
        std::forward<decltype(x)>(x));
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
    auto scalar_clip = [](const auto& x)
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
    return utils::listable_function(scalar_clip,
        std::forward<decltype(x)>(x));
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
    auto scalar_clip = [=](const auto& x)
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
    return utils::listable_function(scalar_clip,
        std::forward<decltype(x)>(x));
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
    auto scalar_clip = [=](const auto& x)
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
    return utils::listable_function(scalar_clip,
        std::forward<decltype(x)>(x));
}
template<typename Ret, typename X>
auto unitize(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto scalar_unitize = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        return Ret(int8_t(x != XV(0)));
    };
    return utils::listable_function(scalar_unitize,
        std::forward<decltype(x)>(x));
}
template<typename Ret, typename X>
auto unit_step(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_arithmetic_v<Ret>, "badrettype");
    auto scalar_sign = [](const auto& x)
    {
        using XV = remove_cvref_t<decltype(x)>;
        static_assert(is_real_v<XV>, "badargtype");
        if (x >= XV(0))
            return Ret(1);
        else
            return Ret(0);
    };
    return utils::listable_function(scalar_sign,
        std::forward<decltype(x)>(x));
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
template<typename T>
struct list_iterator
{
    static constexpr bool has_variable = true;
    std::vector<T> values_;
    list_iterator(std::vector<T>&& values) : values_{values}
    {
    }
    size_t length() const
    {
        return values_.size();
    }
    auto operator[](size_t i) const
    {
        return values_[i];
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
            size_t length = wl::integer_part(diff / ptrdiff_t(step)) + 1u;
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
        if constexpr (is_movable_v<Type&&>)
            return list_iterator(std::move(any.data_));
        else
        {
            std::vector<typename Type::value_type> buffer(any.size());
            any.copy_to(buffer.begin());
            return list_iterator(std::move(buffer));
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
auto _scalar_plus = [](const auto& x, const auto& y)
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
            return cast<C>(x) + cast<C>(y);
    }
};
auto _scalar_subtract = [](const auto& x, const auto& y)
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
            return cast<C>(x) - cast<C>(y);
    }
};
auto _scalar_times = [](const auto& x, const auto& y)
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
            return cast<C>(x) * cast<C>(y);
    }
};
auto _scalar_divide = [](const auto& x, const auto& y)
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
template<typename X>
auto minus(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    return utils::listable_function([](const auto& x) { return -x; },
        std::forward<decltype(x)>(x));
}
template<typename X, typename Y>
auto plus(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    return utils::listable_function(_scalar_plus,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
template<typename X>
auto plus(X&& x)
{
    return std::forward<decltype(x)>(x);
}
template<typename X1, typename X2, typename X3, typename... Xs>
auto plus(X1&& x1, X2&& x2, X3&& x3, Xs&&... xs)
{
    return plus(plus(std::forward<decltype(x1)>(x1),
        std::forward<decltype(x2)>(x2)),
        std::forward<decltype(x3)>(x3),
        std::forward<decltype(xs)>(xs)...);
}
template<typename X, typename Y>
auto subtract(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    return utils::listable_function(_scalar_subtract,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
template<typename X, typename Y>
auto times(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    return utils::listable_function(_scalar_times,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
template<typename X>
auto times(X&& x)
{
    return std::forward<decltype(x)>(x);
}
template<typename X1, typename X2, typename X3, typename... Xs>
auto times(X1&& x1, X2&& x2, X3&& x3, Xs&&... xs)
{
    return times(times(std::forward<decltype(x1)>(x1),
        std::forward<decltype(x2)>(x2)),
        std::forward<decltype(x3)>(x3),
        std::forward<decltype(xs)>(xs)...);
}
template<typename X, typename Y>
auto divide(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    return utils::listable_function(_scalar_divide,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
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
    using YType = remove_cvref_t<X>;                                        \
    constexpr auto x_rank = array_rank_v<XType>;                            \
    constexpr auto y_rank = array_rank_v<YType>;                            \
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
            if constexpr (YType::category != view_category::General)        \
                x.for_each([](auto& dst, const auto& src)                   \
            { _scalar_##name(dst, src); }, y.begin());                      \
            else if constexpr (XType::category != view_category::General)   \
                y.for_each([](const auto& src, auto& dst)                   \
            { _scalar_##name(dst, src); }, x.begin());                      \
            else /* general_view += general_view */                         \
            {                                                               \
                std::vector<typename Y::value_type> buffer(y.size());       \
                y.copy_to(buffer.begin());                                  \
                x.for_each([](auto& dst, const auto& src)                   \
                { _scalar_##name(dst, src); }, buffer.begin());             \
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
}
namespace wl
{
template<typename A, typename B>
auto branch_if(bool cond, A&& a, B&& b)
{
    using AType = remove_cvref_t<decltype(a())>;
    using BType = remove_cvref_t<decltype(b())>;
    if constexpr (is_value_type_v<AType>)
    {
        static_assert(std::is_same_v<AType, BType>, "badargtype");
        if (cond)
            return std::forward<decltype(a)>(a)();
        else
            return std::forward<decltype(b)>(b)();
    }
    else // "if" returns a function
    {
        return
            [cond,
            a = std::forward<decltype(a)>(a)(),
            b = std::forward<decltype(b)>(b)()](auto&&... args)
        {
            if (cond)
                return a(std::forward<decltype(args)>(args)...);
            else
                return b(std::forward<decltype(args)>(args)...);
        };
    }
}
template<typename X, typename Y>
auto native_if(boolean cond, X&& x, Y&& y)
{
    using XT = remove_cvref_t<X>;
    using YT = remove_cvref_t<Y>;
    if constexpr (std::is_same_v<XT, YT>)
    {
        if (cond)
            return std::forward<decltype(x)>(x);
        else
            return std::forward<decltype(y)>(y);
    }
    else
    {
        constexpr auto XR = array_rank_v<XT>;
        constexpr auto YR = array_rank_v<YT>;
        static_assert(XR == YR && XR >= 1u && 
            std::is_same_v<value_type_t<XT>, value_type_t<YT>>, "badargtype");
        if (cond)
            return std::forward<decltype(x)>(x).to_array();
        else
            return std::forward<decltype(y)>(y).to_array();
    }
}
}
namespace wl
{
template<typename Skip, typename Break, typename Fn, typename First, typename... Rest>
auto _clause_impl(Skip& skip_flag, Break& break_flag, Fn fn,
    const First& first, const Rest&... rest)
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
                _clause_impl(skip_flag, break_flag,
                    [&](const auto&... args) { return fn(arg1, args...); },
                    rest...);
            }
            else
                _clause_impl(skip_flag, break_flag, fn, rest...);
        }
    }
}
template<typename Fn, typename... Iters>
auto clause_do(Fn fn, const Iters&... iters)
{
    static_assert(sizeof...(Iters) >= 1, "internal");
    wl::void_type skip_flag;    // skip flag is not used
    wl::void_type break_flag;   // break flag is not used
    _clause_impl(skip_flag, break_flag, fn, iters...);
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
            wl::void_type break_flag;   // break flag is not used
            _clause_impl(skip_flag, break_flag,
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
            ret_iter.step_forward();
            bool skip_flag = true;
            wl::void_type break_flag;   // break flag is not used
            _clause_impl(skip_flag, break_flag,
                [&](const auto&... args)
                {
                    auto item = fn(args...);
                    if (!utils::check_dims(ret_iter.dims(), item.dims()))
                        throw std::logic_error("baddims");
                    item.copy_to(ret_iter.begin());
                    ret_iter.step_forward();
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
        bool skip_flag = true;
        wl::void_type break_flag;   // break flag is not used
        _clause_impl(skip_flag, break_flag,
            [&](const auto&... args)
            {
                auto item = fn(args...);
                if (!utils::check_dims(ret.dims(), item.dims()))
                    throw std::logic_error("baddims");
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
        bool skip_flag = true;
        wl::void_type break_flag;   // break flag is not used
        _clause_impl(skip_flag, break_flag,
            [&](const auto&... args)
            {
                auto item = fn(args...);
                if (!utils::check_dims(ret.dims(), item.dims()))
                    throw std::logic_error("baddims");
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
#define WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(name, expr)                     \
template<typename X>                                                        \
auto name(X&& x)                                                            \
{                                                                           \
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");    \
    return utils::listable_function([](const auto& x) { return expr; },     \
        std::forward<decltype(x)>(x));                                      \
}
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(log, std::log(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(exp, std::exp(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sqrt, std::sqrt(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sin, std::sin(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(cos, std::cos(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(tan, std::tan(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(cot, divide(int8_t(1), std::tan(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sec, divide(int8_t(1), std::cos(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(csc, divide(int8_t(1), std::sin(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arcsin, std::asin(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccos, std::acos(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arctan, std::atan(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccot, std::atan(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arcsec, std::acos(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccsc, std::asin(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sinh, std::sinh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(cosh, std::cosh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(tanh, std::tanh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(coth, divide(int8_t(1), std::tanh(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(sech, divide(int8_t(1), std::cosh(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(csch, divide(int8_t(1), std::sinh(x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arcsinh, std::asinh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccosh, std::acosh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arctanh, std::atanh(x))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccoth, std::atanh(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arcsech, std::acosh(divide(int8_t(1), x)))
WL_DEFINE_UNARY_ELEMENTARY_FUNCTION(arccsch, std::asinh(divide(int8_t(1), x)))
template<typename Base, typename X>
auto log(Base&& b, X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<Base>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    auto scalar_log = [](const auto& b, const auto& x)
    {
        return divide(std::log(x), std::log(b));
    };
    return utils::listable_function(scalar_log, 
        std::forward<decltype(b)>(b), std::forward<decltype(x)>(x));
}
template<typename X>
auto log2(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    constexpr auto log2_inv = double(1.4426950408889634074);
    auto scalar_log2 = [=](const auto& x)
    {
        if constexpr (is_complex_v<remove_cvref_t<decltype(x)>>)
            return log2_inv * std::log(x);
        else
            return std::log2(x);
    };
    return utils::listable_function(scalar_log2, 
        std::forward<decltype(x)>(x));
}
template<typename X>
auto log10(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    constexpr auto log10_inv = double(0.43429448190325182765);
    auto scalar_log10 = [=](const auto& x)
    {
        if constexpr (is_complex_v<remove_cvref_t<decltype(x)>>)
            return log10_inv * std::log(x);
        else
            return std::log10(x);
    };
    return utils::listable_function(scalar_log10, 
        std::forward<decltype(x)>(x));
}
template<typename X, typename Power>
auto power(X&& x, Power&& p)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Power>>, "badargtype");
    auto scalar_power = [](const auto& x, const auto& p)
    {
        return std::pow(x, p);
    };
    return utils::listable_function(scalar_power,
        std::forward<decltype(x)>(x), std::forward<decltype(p)>(p));
}
template<typename X, typename Y>
auto arctan(X&& x, Y&& y)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    static_assert(is_numerical_type_v<remove_cvref_t<Y>>, "badargtype");
    auto scalar_arctan = [](const auto& x, const auto& y)
    {
        using XT = remove_cvref_t<decltype(x)>;
        using YT = remove_cvref_t<decltype(y)>;
        static_assert(is_real_v<XT> && is_real_v<YT>, "badargtype");
        return std::atan2(x, y);
    };
    return utils::listable_function(scalar_arctan,
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
}
template<typename X>
auto sinc(X&& x)
{
    static_assert(is_numerical_type_v<remove_cvref_t<X>>, "badargtype");
    constexpr auto log2_inv = double(1.4426950408889634074);
    auto scalar_sinc = [=](const auto& x)
    {
        using XT = remove_cvref_t<decltype(x)>;
        using RT = decltype(divide(std::sin(x), x));
        if (x == XT(0))
            return RT(1.0);
        else
            return divide(std::sin(x), x);
    };
    return utils::listable_function(scalar_sinc,
        std::forward<decltype(x)>(x));
}
}
namespace wl
{
#define WL_DEFINE_UNARY_BOOLEAN_FUNCTION(name, expr)                    \
template<typename X>                                                    \
auto name(X&& x)                                                        \
{                                                                       \
    static_assert(is_boolean_type_v<remove_cvref_t<X>>, "badargtype");  \
    return utils::listable_function([](boolean x) { return expr; },     \
        std::forward<decltype(x)>(x));                                  \
}
#define WL_DEFINE_BINARY_BOOLEAN_FUNCTION(name, expr)                   \
template<typename X, typename Y>                                        \
auto name(X&& x, Y&& y)                                                 \
{                                                                       \
    static_assert(is_boolean_type_v<remove_cvref_t<X>> &&               \
        is_boolean_type_v<remove_cvref_t<Y>>, "badargtype");            \
    return utils::listable_function(                                    \
        [](boolean x, boolean y) { return expr; },                      \
        std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));    \
}
WL_DEFINE_UNARY_BOOLEAN_FUNCTION(bool_not, !x)
WL_DEFINE_UNARY_BOOLEAN_FUNCTION(boole, int64_t(x))
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_and, x && y)
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_or, x || y)
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_xor, x ^ y)
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_nand, !(x && y))
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_nor, !(x || y))
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(bool_xnor, !(x ^ y))
WL_DEFINE_BINARY_BOOLEAN_FUNCTION(implies, !x || y)
}
namespace wl
{
extern std::default_random_engine global_random_engine;
namespace distribution
{
auto _min = [](const auto& x, const auto& y) { return x < y ? x : y; };
auto _max = [](const auto& x, const auto& y) { return x < y ? y : x; };
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
    template<typename Ptr>
    void operator()(Ptr ptr)
    {
        *ptr = dist_(global_random_engine);
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
    template<typename Ptr>
    void operator()(Ptr ptr)
    {
        ptr->real() = dist_re_(global_random_engine);
        ptr->imag() = dist_im_(global_random_engine);
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
    template<typename Ptr>
    void operator()(Ptr ptr)
    {
        *ptr = dist_(global_random_engine);
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
    static_assert(R1 <= 1, "internal");
    if constexpr (R1 == 0u)
    {
        if constexpr (R2 == 0u)
        {
            return dist();
        }
        else
        {
            wl::ndarray<T, R> x(std::array<int64_t, R>{int64_t(dims)...});
            for (auto& val : x)
            {
                val = dist();
            }
            return x;
        }
    }
    else if constexpr (R2 == 0u)
    {
        size_t dimN = dist.size();
        ndarray<T, 1u> x(std::array<int64_t, 1u>{int64_t(dimN)});
        dist(x.begin());
        return x;
    }
    else
    {
        size_t dimN = dist.size();
        ndarray<T, R> x(std::array<int64_t, R>{int64_t(dims)..., int64_t(dimN)});
        auto x_iter = x.begin();
        auto x_end = x.end();
        for (; x_iter += dimN; x_iter != x_end)
        {
            dist(x_iter);
        }
        return x;
    }
}
template<typename Min, typename Max, typename... Dims>
auto random_integer(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_integral_v<Min> && is_integral_v<Max>, "badargtype");
    using T = decltype(plus(min, max));
    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
}
template<typename Max, typename... Dims>
auto random_integer(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_integral_v<Max>, "badargtype");
    return random_integer(Max(0), max, varg_tag{}, dims...);
}
template<typename Min, typename Max, typename... Dims>
auto random_real(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_real_v<Min> && is_real_v<Max>, "badargtype");
    using T = decltype(plus(plus(min, max), float{}));
    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
}
template<typename Max, typename... Dims>
auto random_real(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_real_v<Max>, "badargtype");
    return random_real(Max(0), max, varg_tag{}, dims...);
}
template<typename Min, typename Max, typename... Dims>
auto random_complex(const Min& min, const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_arithmetic_v<Min> && is_arithmetic_v<Max>, "badargtype");
    using C = decltype(plus(min, max));
    using T = std::conditional_t<std::is_same_v<C, complex<float>>, complex<float>, complex<double>>;
    auto dist = distribution::uniform<T>(T(min), T(max));
    return _random_variate_impl(dist, dims...);
}
template<typename Max, typename... Dims>
auto random_complex(const Max& max, varg_tag, const Dims&... dims)
{
    static_assert(is_arithmetic_v<Max>, "badargtype");
    return random_complex(Max{}, max, varg_tag{}, dims...);
}
}
namespace wl
{
template<typename Dst, typename Src>
auto set(Dst&& dst, Src&& src)
{
    using DstType = remove_cvref_t<Dst>;
    using SrcType = remove_cvref_t<Src>;
    constexpr auto dst_rank = array_rank_v<DstType>;
    constexpr auto src_rank = array_rank_v<SrcType>;
    using DstValue = value_type_t<DstType>;
    using SrcValue = value_type_t<SrcType>;
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
        if constexpr (is_array_v<DstType>)
        {
            static_assert(std::is_same_v<SrcValue, DstValue>, "badargtype");
            if (!has_aliasing(src, dst))
                dst = std::forward<decltype(src)>(src).to_array();
            else
                dst = std::forward<decltype(src)>(src);
        }
        else
        {
            static_assert(is_convertible_v<SrcValue, DstValue>, "badargtype");
            if (!utils::check_dims(src.dims(), dst.dims()))
                throw std::logic_error("baddims");
            if (!has_aliasing(src, dst))
            {
                if constexpr (SrcType::category != view_category::General)
                    std::forward<decltype(dst)>(dst).copy_from(src.begin());
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
template<typename T, size_t R, size_t N, size_t I, typename Iter, typename Tuple>
void _copy_list_array_elements(
    Iter dst, const size_t* dims, size_t size, Tuple&& elems)
{
    if constexpr (I < N)
    {
        using SrcType = remove_cvref_t<decltype(std::get<I>(elems))>;
        static_assert(array_rank_v<SrcType> == R, "badrank");
        using SrcValueType = typename SrcType::value_type;
        static_assert(is_convertible_v<SrcValueType, T>, "badargtype");
        auto elem = std::get<I>(std::forward<decltype(elems)>(elems));
        if (!utils::check_dims<R>(elem.dims_ptr(), dims))
            throw std::logic_error("baddims");
        std::move(elem).copy_to(dst);
    }
    if constexpr (I < N - 1)
        _copy_list_array_elements<T, R, N, I + 1>(dst + size, dims, size,
            std::forward<decltype(elems)>(elems));
}
template<typename T, size_t N, size_t I, typename Iter, typename Tuple>
void _copy_list_scalar_elements(Iter dst, Tuple&& elems)
{
    if constexpr (I < N)
    {
        using SrcType = remove_cvref_t<decltype(std::get<I>(elems))>;
        static_assert(is_convertible_v<SrcType, T>, "badargtype");
        *dst = std::get<I>(elems);
    }
    if constexpr (I < N - 1)
        _copy_list_scalar_elements<T, N, I + 1>(dst + 1,
            std::forward<decltype(elems)>(elems));
}
template<typename First, typename... Rest>
auto list(First&& first, Rest&&... rest)
{
    using FirstType = remove_cvref_t<First>;
    constexpr auto first_rank = array_rank_v<FirstType>;
    constexpr auto dim0 = sizeof...(rest) + 1u;
    if constexpr (first_rank == 0)
    {
        ndarray<FirstType, 1u> ret(std::array<size_t, 1u>{dim0});
        _copy_list_scalar_elements<FirstType, dim0, 0u>(
            ret.begin(),
            std::make_tuple(std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...));
        return ret;
    }
    else
    {
        constexpr auto rank = first_rank + 1u;
        std::array<size_t, rank> dims;
        dims[0] = dim0;
        std::copy_n(first.dims().begin(), first_rank, dims.data() + 1);
        using FirstValueType = typename FirstType::value_type;
        ndarray<FirstValueType, rank> ret(dims);
        size_t item_size = first.size();
        _copy_list_array_elements<FirstValueType, first_rank, dim0, 0u>(
            ret.begin(), dims.data() + 1, item_size,
            std::make_tuple(std::forward<decltype(first)>(first),
                std::forward<decltype(rest)>(rest)...));
        return ret;
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
    static_assert(sizeof...(Indexers) <= array_rank_v<ArrayType>, "badargc");
    using return_type = typename view_detail::view_type<Indexers...>::
        template return_type<ValueType, is_const, Indexers...>;
    if constexpr (is_array_view_v<return_type>)
        return return_type(a, std::forward<decltype(indexers)>(indexers)...);
    else
        return a.data()[a.linear_pos(indexers.offset()...)];
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
    else if constexpr (ArrayType::category != view_category::Array)
        return part(a.to_array(), std::forward<decltype(specs)>(specs)...);
    else
    {
        return _part_impl1<R>(std::forward<decltype(a)>(a),
            std::make_index_sequence<R - sizeof...(Specs)>{},
            std::forward<decltype(specs)>(specs)...);
    }
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
                for (size_t i = 0; i < length; ++i, begin += step)
                    ret[i] = begin;
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
                for (size_t i = 0; i < length; ++i)
                    ret[i] = T(begin + i * step);
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
                auto ret = ValueType{};
                a.for_each([&](const auto& val) { ret += val; });
                return ret;
            }
            else
            {
                auto ret_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                ndarray<ValueType, rank - L2> ret(ret_dims);
                const auto inter_size = utils::size_of_dims(
                    utils::dims_take<L1, L2>(a.dims()));
                const auto inner_size = ret.size();
                auto iter = a.begin();
                for (size_t j = 0; j < inter_size; ++j)
                    for (size_t k = 0; k < inner_size; ++k)
                        ret[k] += *iter++;
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
                auto iter = a.begin();
                for (size_t i = 0; i < outer_size; ++i)
                    for (size_t j = 0; j < inter_size; ++j)
                        ret[i] += *iter++;
                return ret;
            }
            else
            {
                auto outer_dims = utils::dims_take<1, L1 - 1>(a.dims());
                auto inter_dims = utils::dims_take<L1, L2>(a.dims());
                auto inner_dims = utils::dims_take<L2 + 1, rank>(a.dims());
                auto ret_dims = utils::dims_join(outer_dims, inner_dims);
                ndarray<ValueType, rank - (L2 - L1 + 1)> ret(ret_dims);
                auto iter = a.begin();
                const auto outer_size = utils::size_of_dims(outer_dims);
                const auto inter_size = utils::size_of_dims(inter_dims);
                const auto inner_size = utils::size_of_dims(inner_dims);
                for (size_t i = 0; i < outer_size; ++i)
                    for (size_t j = 0; j < inter_size; ++j)
                        for (size_t k = 0; k < inner_size; ++k)
                            ret[i * inner_size + k] += *iter++;
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
template<typename Array>
auto dimensions(const Array& a)
{
    constexpr auto rank = array_rank_v<Array>;
    if constexpr (rank == 0)
        return ndarray<int64_t, 1>();
    else
        return ndarray<int64_t, 1>(std::array<size_t, 1>{rank},
            a.dims_ptr(), a.dims_ptr() + rank);
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
        return std::move(x);
    }
    else
    {
        auto x2 = x.to_array();
        _reverse_inplace(x2, outer_size, inter_size, inner_size);
        return std::move(x2);
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
}
namespace wl
{
template<typename T, size_t R, typename Function>
auto _select_impl(const ndarray<T, R>& a, Function f)
{
    static_assert(R >= 2u, "internal");
    auto ret_dims = a.dims();
    ret_dims[0] = 0u;
    ndarray<T, R> ret(ret_dims);
    size_t item_count = 0;
    auto view_iter = a.template view_begin<1u>();
    auto view_end = a.template view_end<1u>();
    size_t item_size = view_iter.size();
    for (; !view_iter.view_pos_equal(view_end); view_iter.step_forward())
    {
        auto out = f(view_iter);
        static_assert(is_boolean_v<decltype(out)>, "badfunctype");
        if (out)
        {
            ret.resize(++item_count, item_size);
            view_iter.copy_to(ret.begin() + (item_count - 1u) * item_size);
        }
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
template<typename T, size_t R, typename Function>
auto _count_impl(const ndarray<T, R>& a, Function f)
{
    static_assert(R >= 2u, "internal");
    size_t item_count = 0;
    auto view_iter = a.template view_begin<1u>();
    auto view_end = a.template view_end<1u>();
    for (; !view_iter.view_pos_equal(view_end); view_iter.step_forward())
    {
        auto out = f(view_iter);
        static_assert(is_boolean_v<decltype(out)>, "badfunctype");
        if (out) ++item_count;
    }
    return item_count;
}
template<typename X, typename Function>
auto count(X&& x, Function f)
{
    using XT = remove_cvref_t<X>;
    static_assert(array_rank_v<XT> >= 1u, "badrank");
    if constexpr (array_rank_v<XT> == 1u)
    {
        size_t item_count = 0;
        x.for_each([&](const auto& a)
            {
                auto out = f(a);
                static_assert(is_boolean_v<decltype(out)>, "badfunctype");
                if (out) ++item_count;
            });
        return item_count;
    }
    else
        return _count_impl(std::forward<decltype(x)>(x).to_array(), f);
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
        for (size_t i = 0; i < utils::size_of_dims(map_dims);
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
template<typename Function, typename X>
auto nest(Function f, X&& x, const int64_t n)
{
    using XT0 = remove_cvref_t<X>;
    constexpr auto rank0 = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error("badargv");
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    if constexpr (rank0 == 0)
    {
        static_assert(is_convertible_v<XT0, XT2> &&
            is_convertible_v<XT1, XT2>, "badargtype");
        if (n == 0)
            return cast<XT2>(x);
        else // n >= 1
        {
            auto ret = cast<XT2>(f(std::forward<decltype(x)>(x)));
            for (size_t i = 1u; i < size_t(n); ++i)
                ret = cast<XT2>(f(ret));
            return ret;
        }
    }
    else
    {
        constexpr auto rank1 = array_rank_v<XT1>;
        constexpr auto rank2 = array_rank_v<XT2>;
        using XV0 = value_type_t<XT0>;
        using XV1 = value_type_t<XT1>;
        using XV2 = value_type_t<XT2>;
        static_assert(rank0 == rank2 && rank1 == rank2, "badargtype");
        static_assert(is_convertible_v<XV0, XV2> &&
            is_convertible_v<XV1, XV2>, "badargtype");
        if (n == 0)
        {
            if constexpr (rank0 == rank2 && std::is_same_v<XV0, XV2>)
                return val(std::forward<decltype(x)>(x));
            else
            {
                ndarray<XV2, rank2> ret(x.dims());
                x.copy_to(ret.begin());
                return ret;
            }
        }
        else // n >= 1
        {
            auto ret = val(f(std::forward<decltype(x)>(x)));
            for (size_t i = 1u; i < size_t(n); ++i)
            {
                auto temp = val(f(std::move(ret)));
                set(ret, std::move(temp));
            }
            return ret;
        }
    }
}
template<typename Function, typename X>
auto nest_list(Function f, X&& x, const int64_t n)
{
    using XT0 = remove_cvref_t<X>;
    constexpr auto rank0 = array_rank_v<XT0>;
    if (n < 0) throw std::logic_error("badargv");
    using XT1 = remove_cvref_t<decltype(val(f(std::declval<X&&>())))>;
    using XT2 = remove_cvref_t<decltype(val(f(std::declval<XT1&&>())))>;
    if constexpr (rank0 == 0)
    {
        static_assert(is_convertible_v<XT0, XT2> &&
            is_convertible_v<XT1, XT2>, "badargtype");
        ndarray<XT2, 1u> ret(std::array<size_t, 1u>{size_t(n) + 1u});
        ret[0] = cast<XT2>(x);
        for (size_t i = 1; i <= size_t(n); ++i)
            ret[i] = cast<XT2>(f(ret[i - 1]));
        return ret;
    }
    else
    {
        constexpr auto rank1 = array_rank_v<XT1>;
        constexpr auto rank2 = array_rank_v<XT2>;
        using XV0 = value_type_t<XT0>;
        using XV1 = value_type_t<XT1>;
        using XV2 = value_type_t<XT2>;
        static_assert(rank0 == rank2 && rank1 == rank2, "badargtype");
        static_assert(is_convertible_v<XV0, XV2> &&
            is_convertible_v<XV1, XV2>, "badargtype");
        auto item_dims = x.dims();
        auto ret_dims = utils::dims_join(
            std::array<size_t, 1u>{size_t(n) + 1u}, item_dims);
        if (n == 0)
        {
            if constexpr (std::is_same_v<XT0, XT2>)
            {
                return ndarray<XV2, rank2 + 1u>(ret_dims,
                    std::forward<decltype(x)>(x).data_vector());
            }
            else
            {
                ndarray<XV2, rank2 + 1u> ret(ret_dims);
                x.copy_to(ret.begin());
                return ret;
            }
        }
        else // n >= 1
        {
            ndarray<XV2, rank2 + 1u> ret(ret_dims);
            auto view_iter = ret.template view_begin<1u>();
            x.copy_to(view_iter.begin());
            ++view_iter;
            auto item = val(f(std::forward<decltype(x)>(x)));
            if (!utils::check_dims(item.dims(), item_dims))
                throw std::logic_error("baddims");
            item.copy_to(view_iter.begin());
            ++view_iter;
            for (int64_t i = 1; i < n; ++i, ++view_iter)
            {
                auto temp = val(f(std::move(item)));
                set(item, std::move(temp));
                if (!utils::check_dims(item.dims(), item_dims))
                    throw std::logic_error("baddims");
                item.copy_to(view_iter.begin());
            }
            return ret;
        }
    }
}
template<bool FoldL, typename Function, typename X, typename YIter>
auto _fold_impl(Function f, X&& x, YIter y_iter, const size_t n)
{
    using XT0 = remove_cvref_t<X>;
    using XV0 = value_type_t<XT0>;
    constexpr auto rank0 = array_rank_v<XT0>;
    using XT1 = remove_cvref_t<decltype(f(std::declval<X&&>(), *y_iter))>;
    using XT2 = remove_cvref_t<decltype(f(std::declval<XT1&&>(), *y_iter))>;
    if constexpr (rank0 == 0)
    {
        static_assert(is_convertible_v<XT0, XT2> && 
            is_convertible_v<XT1, XT2>, "badargtype");
        if (n == 0)
            return cast<XT2>(std::forward<decltype(x)>(x));
        else // n >= 1
        {
            auto ret = cast<XT2>(
                f(std::forward<decltype(x)>(x), *y_iter));
            for (size_t i = 1u; i < n; ++i)
            {
                if constexpr (FoldL)
                    ++y_iter;
                else
                    --y_iter;
                ret = cast<XT2>(f(ret, *y_iter));
            }
            return ret;
        }
    }
    else
    {
        constexpr auto rank1 = array_rank_v<XT1>;
        constexpr auto rank2 = array_rank_v<XT2>;
        using XV0 = value_type_t<XT0>;
        using XV1 = value_type_t<XT1>;
        using XV2 = value_type_t<XT2>;
        static_assert(rank0 == rank2 && rank1 == rank2, "badargtype");
        static_assert(is_convertible_v<XV0, XV2> &&
            is_convertible_v<XV1, XV2>, "badargtype");
        if (n == 0)
        {
            if constexpr (rank0 == rank2 && std::is_same_v<XV0, XV2>)
                return val(std::forward<decltype(x)>(x));
            else
            {
                ndarray<XV2, rank2> ret(x.dims());
                x.copy_to(ret.begin());
                return ret;
            }
        }
        else // n >= 1
        {
            auto ret = val(f(std::forward<decltype(x)>(x), *y_iter));
            for (size_t i = 1u; i < n; ++i)
            {
                if constexpr (FoldL)
                    ++y_iter;
                else
                    --y_iter;
                auto temp = val(f(std::move(ret), *y_iter));
                set(ret, std::move(temp));
            }
            return ret;
        }
    }
}
template<typename Function, typename X, typename Y>
auto fold(Function f, X&& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<Y> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if constexpr (is_array_v<YT>)
    {
        return _fold_impl<true>(f, std::forward<decltype(x)>(x),
            y.template view_begin<1u>(), y.dims()[0]);
    }
    else
    {
        auto valy = val(std::forward<decltype(y)>(y));
        return _fold_impl<true>(f, std::forward<decltype(x)>(x),
            valy.template view_begin<1u>(), valy.dims()[0]);
    }
}
template<typename Function, typename X, typename Y>
auto foldr(Function f, X&& x, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<Y> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if constexpr (is_array_v<YT>)
    {
        return _fold_impl<false>(f, std::forward<decltype(x)>(x),
            --(y.template view_end<1u>()), y.dims()[0]);
    }
    else
    {
        auto valy = val(std::forward<decltype(y)>(y));
        return _fold_impl<false>(f, std::forward<decltype(x)>(x),
            --(valy.template view_end<1u>()), valy.dims()[0]);
    }
}
template<typename Function, typename Y>
auto fold(Function f, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<Y> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if (n < 1u)
        throw std::logic_error("baddims");
    if constexpr (is_array_v<YT>)
    {
        auto y_first = y.template view_begin<1u>();
        auto y_rest = y.template view_begin<1u>();
        return _fold_impl<true>(f, *y_first, ++y_rest, n - 1u);
    }
    else
    {
        auto valy = val(std::forward<decltype(y)>(y));
        auto y_first = valy.template view_begin<1u>();
        auto y_rest = valy.template view_begin<1u>();
        return _fold_impl<true>(f, *y_first, ++y_rest, n - 1u);
    }
}
template<typename Function, typename Y>
auto foldr(Function f, Y&& y)
{
    using YT = remove_cvref_t<Y>;
    static_assert(array_rank_v<Y> >= 1u, "badargtype");
    const size_t n = y.dims()[0];
    if (n < 1u)
        throw std::logic_error("baddims");
    if constexpr (is_array_v<YT>)
    {
        auto y_first = --(y.template view_end<1u>());
        auto y_rest = --(y.template view_end<1u>());
        return _fold_impl<false>(f, *y_first, --y_rest, n - 1u);
    }
    else
    {
        auto valy = val(std::forward<decltype(y)>(y));
        auto y_first = --(valy.template view_end<1u>());
        auto y_rest = --(valy.template view_end<1u>());
        return _fold_impl<false>(f, *y_first, --y_rest, n - 1u);
    }
}
}
namespace wl
{
}
