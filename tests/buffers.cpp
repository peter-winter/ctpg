#include <catch2/catch_test_macros.hpp>
#include <ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

constexpr nterm<int> root("root");

constexpr parser p1(
    root,
    terms('0', '1', '2'),
    nterms(root),
    rules(
        root('0', '1', '2') >= val(42)
    )
);

TEST_CASE("cstring buffer", "[buffers]")
{
    auto result = p1.parse(cstring_buffer(" 0  1  2 "));
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 42);
}

TEST_CASE("string buffer", "[buffers]")
{
    std::string txt(" 0  1  2 ");
    string_buffer buf(std::move(txt));
    auto result = p1.parse(buf);
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 42);
}
