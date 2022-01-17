#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    constexpr nterm<int> root("root");
    constexpr nterm<int> x("x");

    constexpr parser p1(
        root,
        terms('1'),
        nterms(root, x),
        rules(
            root(x),
            root(root, x) >= [](int sum, int v){ return sum + v; },
            x('1') >= val(1)
        )
    );
}

TEST_CASE("left recurrence", "[recurrence]")
{
    auto result1 = test::p1.parse(cstring_buffer("1"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 1);

    auto result2 = test::p1.parse(cstring_buffer("11"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 2);

    auto result3 = test::p1.parse(cstring_buffer("111"));
    REQUIRE(result3.has_value());
    REQUIRE(result3.value() == 3);
}

namespace test
{
    constexpr parser p2(
        root,
        terms('1'),
        nterms(root, x),
        rules(
            root(x),
            root(x, root) >= [](int v, int sum){ return sum + v; },
            x('1') >= val(1)
        )
    );
}

TEST_CASE("right recurrence", "[recurrence]")
{
    auto result1 = test::p2.parse(cstring_buffer("1"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 1);

    auto result2 = test::p2.parse(cstring_buffer("11"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 2);

    auto result3 = test::p2.parse(cstring_buffer("111"));
    REQUIRE(result3.has_value());
    REQUIRE(result3.value() == 3);
}

namespace test
{
    constexpr parser p3(
        root,
        terms('1'),
        nterms(root, x),
        rules(
            root() >= val(0),
            root(root, x) >= [](int v, int sum){ return sum + v; },
            x('1') >= val(1)
        )
    );
}

TEST_CASE("left recurrence, empty", "[recurrence]")
{
    auto result1 = test::p3.parse(cstring_buffer("1"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 1);

    auto result2 = test::p3.parse(cstring_buffer("11"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 2);

    auto result3 = test::p3.parse(cstring_buffer("111"));
    REQUIRE(result3.has_value());
    REQUIRE(result3.value() == 3);

    auto result4 = test::p3.parse(cstring_buffer(""));
    REQUIRE(result4.has_value());
    REQUIRE(result4.value() == 0);
}

namespace test
{
    constexpr parser p4(
        root,
        terms('1'),
        nterms(root, x),
        rules(
            root() >= val(0),
            root(x, root) >= [](int sum, int v){ return sum + v; },
            x('1') >= val(1)
        )
    );
}

TEST_CASE("right recurrence, empty", "[recurrence]")
{
    auto result1 = test::p4.parse(cstring_buffer("1"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 1);

    auto result2 = test::p4.parse(cstring_buffer("11"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 2);

    auto result3 = test::p4.parse(cstring_buffer("111"));
    REQUIRE(result3.has_value());
    REQUIRE(result3.value() == 3);

    auto result4 = test::p4.parse(cstring_buffer(""));
    REQUIRE(result4.has_value());
    REQUIRE(result4.value() == 0);
}
