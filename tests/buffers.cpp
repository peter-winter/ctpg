#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>

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

TEST_CASE("cstring buffer", "[buffers]")
{
    auto result = test::p1.parse(cstring_buffer(" 0  1  2 "));
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 42);
}

TEST_CASE("string buffer", "[buffers]")
{
    std::string txt(" 0  1  2 ");
    string_buffer buf(std::move(txt));
    auto result = test::p1.parse(buf);
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 42);
}

TEST_CASE("string_view buffer, lvalue", "[buffers]")
{
    std::string_view txt(" 0  1  2 ");
    string_view_buffer buf(txt);
    auto result = test::p1.parse(buf);
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 42);
}

TEST_CASE("string_view buffer, rvalue", "[buffers]")
{
    string_view_buffer buf(std::string_view(" 0  1  2 "));
    auto result = test::p1.parse(buf);
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 42);
}
