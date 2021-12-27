#include <catch2/catch.hpp>
#include <ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    constexpr nterm<int> root("root");

    constexpr parser single_char_parser(
        root,
        terms('x'),
        nterms(root),
        rules(
            root('x') >= val(1)
        )
    );
}

TEST_CASE("single char", "[simple cases]")
{
    auto result = test::single_char_parser.parse(cstring_buffer("x"));
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 1);
}

TEST_CASE("single char fail", "[simple cases]")
{
    auto result = test::single_char_parser.parse(cstring_buffer("z"));
    REQUIRE(!result.has_value());
}

namespace test
{
    constexpr parser single_string_parser(
        root,
        terms("gogogo_ctpg"),
        nterms(root),
        rules(
            root("gogogo_ctpg") >= val(1)
        )
    );
}

TEST_CASE("single string", "[simple cases]")
{
    auto result = test::single_string_parser.parse(cstring_buffer("gogogo_ctpg"));
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 1);
}

TEST_CASE("single string fail", "[simple cases]")
{
    auto result = test::single_string_parser.parse(cstring_buffer("what the heck"));
    REQUIRE(!result.has_value());
}

namespace test
{
    constexpr char pattern[] = "[1-9][0-9]+";
    constexpr regex_term<pattern> number("number");
    constexpr parser single_regex_parser(
        root,
        terms(number),
        nterms(root),
        rules(
            root(number) >= val(1)
        )
    );
}

TEST_CASE("single regex", "[simple cases]")
{
    auto result1 = test::single_regex_parser.parse(cstring_buffer("19"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 1);

    auto result2 = test::single_regex_parser.parse(cstring_buffer("19123123"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 1);
}

TEST_CASE("single regex fail", "[simple cases]")
{
    auto result = test::single_regex_parser.parse(cstring_buffer("what the heck"));
    REQUIRE(!result.has_value());
}
