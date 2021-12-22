#include <catch2/catch.hpp>
#include <ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

constexpr nterm<int> root("root");

constexpr parser p(
    root,
    terms('*'),
    nterms(root),
    rules(
        root('*') >= val(1),
        root(root, '*') >= [](int sum, skip){ return sum + 1; }
    )
);

constexpr auto result = p.parse(cstring_buffer("****"));
constexpr int v = result.value();

TEST_CASE("constexpr", "[compile time]")
{
    REQUIRE(result.has_value());
    REQUIRE(v == 4);
}
