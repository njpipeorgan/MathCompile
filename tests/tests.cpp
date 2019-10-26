#include <cassert>

#include "../src/math_compile.h"

using namespace wl::literal;
std::default_random_engine wl::global_random_engine;

#define WL_TEST_ASSERT_HAS_TYPE(val, type) static_assert(std::is_same_v<wl::remove_cvref_t<decltype(val)>, type>, "")

void arithmetic_test()
{
    auto x1 = wl::plus(int16_t(2), uint8_t(2));
    WL_TEST_ASSERT_HAS_TYPE(x1, int16_t);
    assert(x1 == int16_t(4));

    auto x2 = wl::subtract(wl::make_complex(3.f, 2.f), uint16_t(2));
    WL_TEST_ASSERT_HAS_TYPE(x2, wl::complex<float>);
    assert(x2 == wl::make_complex(1.f, 2.f));

    auto x3 = wl::times(int16_t(2), float(2));
    WL_TEST_ASSERT_HAS_TYPE(x3, float);
    assert(x3 == float(4.0));

    auto x4 = wl::divide(int16_t(2), uint16_t(2));
    WL_TEST_ASSERT_HAS_TYPE(x4, double);
    assert(x4 == double(1.0));

    // plus and times are variadic
    auto x5 = wl::plus(2, 3, 4, 5);
    assert(x5 == 14);

    auto x6 = wl::times(2, 3, 4, 5);
    assert(x6 == 120);

    std::vector<int> args{2, 3, 4, 5};
    auto x7 = wl::plus(1, wl::argument_pack<int*>(&args[0], args.size(), 1), 6);
    assert(x7 == 21);

    auto x8 = wl::times(1, wl::argument_pack<int*>(&args[0], args.size(), 1), 6);
    assert(x8 == 720);

    // listable
    auto x9 = wl::plus(wl::list(1, 2, 3), wl::list(10, 20, 30));
    assert(wl::equal(x9, wl::list(11, 22, 33)));

    auto x10 = wl::divide(wl::list(1, 2, 3), wl::list(10, 20, 30));
    assert(wl::equal(x10, wl::list(0.1, 0.1, 0.1)));
}

int main(int argc, char* argv[])
{
    arithmetic_test();

    return 0;
}
