#include <catch2/catch.hpp>
#include <ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    constexpr nterm<int> root("root");
    constexpr nterm<int> list("list");

    constexpr parser p(
        root,
        terms('x', ';', 'y'),
        nterms(root, list),
        rules(
            root(list, ';') >= _e1,
            root(error, ';') >= val(-1),
            list() >= val(0),
            list(list, 'x') >= [](int sum, skip){ return sum + 1; }
        )
    );
}

TEST_CASE("error recovery", "[error recovery]")
{
    auto result = test::p.parse(cstring_buffer("xxxxx;"));
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 5);

    auto result_e = test::p.parse(cstring_buffer("xxxyxx;"));
    REQUIRE(result_e.has_value());
    REQUIRE(result_e.value() == -1);
}
