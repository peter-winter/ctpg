#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>

namespace test
{
    constexpr char pattern_single_char[] = "s";
    constexpr char pattern_concat_single_chars[] = "ss";
    constexpr char pattern_escaped_char[] = R"(\+)";
    constexpr char pattern_escaped_char_hex_0[] = R"(\x)";
    constexpr char pattern_escaped_char_hex_1_digit1[] = R"(\x2)";
    constexpr char pattern_escaped_char_hex_1_digit2[] = R"(\xa)";
    constexpr char pattern_escaped_char_hex_1_digit3[] = R"(\xA)";
    constexpr char pattern_escaped_char_hex_2_digit1[] = R"(\x20)";
    constexpr char pattern_escaped_char_hex_2_digit2[] = R"(\xaf)";
    constexpr char pattern_escaped_char_hex_2_digit3[] = R"(\xAD)";
}

TEST_CASE("singe char", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_single_char> r;

    REQUIRE( r.match("s") );
    REQUIRE( !r.match("ss") );
    REQUIRE( !r.match("") );
}

TEST_CASE("concat singe chars", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_concat_single_chars> r;

    REQUIRE( r.match("ss") );
    REQUIRE( !r.match("sss") );
    REQUIRE( !r.match("s") );
    REQUIRE( !r.match("") );
}

TEST_CASE("escaped char", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_escaped_char> r;

    REQUIRE( r.match("+") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("++") );
    REQUIRE( !r.match("\\+") );
}

TEST_CASE("escaped char hex 0", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_escaped_char_hex_0> r;

    REQUIRE( r.match("\0") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("0") );
}

TEST_CASE("escaped char hex 1 digit", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_escaped_char_hex_1_digit1> r1;

    REQUIRE( r1.match("\x2") );
    REQUIRE( !r1.match("\x7") );
    REQUIRE( !r1.match("") );
    REQUIRE( !r1.match("x2") );

    constexpr ctpg::regex::expr<test::pattern_escaped_char_hex_1_digit2> r2;

    REQUIRE( r2.match("\xa") );
    REQUIRE( !r2.match("\xb") );
    REQUIRE( !r2.match("") );
    REQUIRE( !r2.match("xa") );

    constexpr ctpg::regex::expr<test::pattern_escaped_char_hex_1_digit3> r3;

    REQUIRE( r3.match("\xa") );
    REQUIRE( !r3.match("\xb") );
    REQUIRE( !r3.match("") );
    REQUIRE( !r3.match("xA") );
}

TEST_CASE("escaped char hex 2 digit", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_escaped_char_hex_2_digit1> r1;

    REQUIRE( r1.match("\x20") );
    REQUIRE( !r1.match("\x7") );
    REQUIRE( !r1.match("") );
    REQUIRE( !r1.match("x20") );

    constexpr ctpg::regex::expr<test::pattern_escaped_char_hex_2_digit2> r2;

    REQUIRE( r2.match("\xaf") );
    REQUIRE( !r2.match("\xbf") );
    REQUIRE( !r2.match("") );
    REQUIRE( !r2.match("xaf") );

    constexpr ctpg::regex::expr<test::pattern_escaped_char_hex_2_digit3> r3;

    REQUIRE( r3.match("\xad") );
    REQUIRE( !r3.match("\xbf") );
    REQUIRE( !r3.match("") );
    REQUIRE( !r3.match("xad") );
}
