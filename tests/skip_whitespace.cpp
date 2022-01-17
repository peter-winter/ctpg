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

TEST_CASE("skip newline", "[skip whitespace]")
{
  auto result1 = test::p1.parse(cstring_buffer("\n0\n1\n2\n"));
  REQUIRE(result1.has_value());
  REQUIRE(result1.value() == 42);

  auto result2 = test::p1.parse(cstring_buffer("012"));
  REQUIRE(result2.has_value());
  REQUIRE(result2.value() == 42);

  std::stringstream stream3;
  auto result3 = test::p1.parse(parse_options{}.set_skip_whitespace(false), cstring_buffer("\n0\n12"), stream3);
  REQUIRE(!result3.has_value());
  REQUIRE(stream3.str() == std::string("[1:1] PARSE: Unexpected character: \n\n"));

  std::stringstream stream4;
  auto result4 = test::p1.parse(parse_options{}.set_skip_newline(false), cstring_buffer("\n0\n12"), stream4);
  REQUIRE(!result4.has_value());
  REQUIRE(stream4.str() == std::string("[1:1] PARSE: Unexpected character: \n\n"));
}
