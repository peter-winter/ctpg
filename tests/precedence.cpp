#include <catch2/catch.hpp>
#include <ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

constexpr nterm<int> expr("expr");

constexpr char_term plus('+', 0, associativity::ltor);
constexpr char_term minus('-', 0, associativity::ltor);
constexpr char_term mul('*', 1, associativity::ltor);
constexpr char_term hash('#', 2, associativity::rtol);

constexpr parser p(
    expr,
    terms('1', '2', '3', '+', '*', '-', '#', '(', ')'),
    nterms(expr),
    rules(
        expr('1') >= val(1),
        expr('2') >= val(2),
        expr('3') >= val(3),
        expr(expr, '+', expr) >= [](int x, skip, int y){ return x + y; },
        expr(expr, '-', expr) >= [](int x, skip, int y){ return x - y; },
        expr(expr, '*', expr) >= [](int x, skip, int y){ return x * y; },
        expr(expr, '#', expr) >= [](int x, skip, int y){ return x - y; },
        expr('-', expr)[3] >=  [](skip, int x){ return -x; },
        expr('(', expr, ')') >= _e2
    )
);

TEST_CASE("proof of concept", "[precedence]")
{
    auto result1 = p.parse(cstring_buffer("1+1"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 2);

    auto result2 = p.parse(cstring_buffer("1-1"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 0);

    auto result3 = p.parse(cstring_buffer("1*2"));
    REQUIRE(result3.has_value());
    REQUIRE(result3.value() == 2);

    auto result4 = p.parse(cstring_buffer("1#3"));
    REQUIRE(result4.has_value());
    REQUIRE(result4.value() == -2);

    auto result5 = p.parse(cstring_buffer("-2"));
    REQUIRE(result5.has_value());
    REQUIRE(result5.value() == -2);

    auto result6 = p.parse(cstring_buffer("(2)"));
    REQUIRE(result6.has_value());
    REQUIRE(result6.value() == 2);
}

TEST_CASE("precedence cases", "[precedence]")
{
    auto result1 = p.parse(cstring_buffer("2+2*2"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 6);

    auto result2 = p.parse(cstring_buffer("(2+2)*2"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 8);

    auto result3 = p.parse(cstring_buffer("2-2*2"));
    REQUIRE(result3.has_value());
    REQUIRE(result3.value() == -2);

    auto result4 = p.parse(cstring_buffer("(2-2)*2"));
    REQUIRE(result4.has_value());
    REQUIRE(result4.value() == 0);

    auto result5 = p.parse(cstring_buffer("-3+2"));
    REQUIRE(result5.has_value());
    REQUIRE(result5.value() == -1);

    auto result6 = p.parse(cstring_buffer("-(3+2)"));
    REQUIRE(result6.has_value());
    REQUIRE(result6.value() == -5);

    auto result7 = p.parse(cstring_buffer("-3#2"));
    REQUIRE(result7.has_value());
    REQUIRE(result7.value() == -5);

    auto result8 = p.parse(cstring_buffer("-(3#2)"));
    REQUIRE(result8.has_value());
    REQUIRE(result8.value() == -1);

    auto result9 = p.parse(cstring_buffer("3#2#1"));
    REQUIRE(result9.has_value());
    REQUIRE(result9.value() == 2);
}
