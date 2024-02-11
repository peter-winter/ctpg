#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>
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

TEST_CASE("simple grammar p1", "[simple grammar]")
{
    auto result1 = test::p1.parse(cstring_buffer(" 0  1  2 "));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 42);

    auto result2 = test::p1.parse(cstring_buffer("012"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 42);

    std::stringstream stream;
    auto result3 = test::p1.parse(cstring_buffer("0123"), stream);
    REQUIRE(!result3.has_value());
    REQUIRE(stream.str() == std::string("[1:4] PARSE: Unexpected character: 3\n"));

    stream.str("");
    auto result4 = test::p1.parse(cstring_buffer("0120"), stream);
    REQUIRE(!result4.has_value());
    REQUIRE(stream.str() == std::string("[1:4] PARSE: Syntax error: Unexpected term 0\n"));
}

namespace test
{
    constexpr nterm<int> c("c");
    constexpr nterm<int> d("d");

    constexpr parser p2(
        root,
        terms('0', '1', '2'),
        nterms(root, c, d),
        rules(
            root(c, d) >= [](int cv, int dv){ return cv + dv; },
            c('0') >= val(0),
            d('1') >= val(1),
            d('2') >= val(2)
        )
    );
}

TEST_CASE("simple grammar p2", "[simple grammar]")
{
    auto result1 = test::p2.parse(cstring_buffer("0 1"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 1);

    auto result2 = test::p2.parse(cstring_buffer("0 2"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 2);

    std::stringstream stream;
    auto result3 = test::p2.parse(cstring_buffer("013"), stream);
    REQUIRE(!result3.has_value());
    REQUIRE(stream.str() == std::string("[1:3] PARSE: Unexpected character: 3\n"));

    stream.str("");
    auto result4 = test::p2.parse(cstring_buffer("012"), stream);
    REQUIRE(!result4.has_value());
    REQUIRE(stream.str() == std::string("[1:3] PARSE: Syntax error: Unexpected term 2\n"));

    stream.str("");
    auto result5 = test::p2.parse(cstring_buffer("1"), stream);
    REQUIRE(!result5.has_value());
    REQUIRE(stream.str() == std::string("[1:1] PARSE: Syntax error: Unexpected term 1\n"));
}
