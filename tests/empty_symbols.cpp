#include <catch2/catch.hpp>
#include <ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

constexpr nterm<int> root("root");
constexpr nterm<int> x("x");
constexpr nterm<int> y("y");

constexpr parser p1(
    root,
    terms('1'),
    nterms(root, x, y),
    rules(
        root(x, y) >= [](int v1, int v2){ return v1 + v2; },
        x('1') >= val(1),
        y() >= val(0),
        y('1') >= val(1)
    )
);

TEST_CASE("empty symbols", "[empty symbols]")
{
    auto result1 = p1.parse(cstring_buffer("1 1"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 2);

    auto result2 = p1.parse(cstring_buffer("1"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 1);
}
