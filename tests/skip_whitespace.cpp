#include <catch2/catch.hpp>
#include <ctpg.hpp>
#include <sstream>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    constexpr nterm<int> root("root");

    constexpr parser p1(
        root,
        terms('0', '1', '2'),
        nterms(root),
        rules(
            root('0', '1', '2') >= val(42)
        )
    );
}

TEST_CASE("skip whitespace", "[skip whitespace]")
{
    auto result1 = test::p1.parse(cstring_buffer(" 0  1  2 "));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 42);

    auto result2 = test::p1.parse(cstring_buffer("012"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 42);

    std::stringstream stream;
    auto result3 = test::p1.parse(parse_options{}.set_skip_whitespace(false), cstring_buffer(" 0 12"), stream);
    REQUIRE(!result3.has_value());
    REQUIRE(stream.str() == std::string("[1:1] PARSE: Unexpected character:  \n"));
}
