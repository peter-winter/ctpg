#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>

namespace test
{
    constexpr char pattern_any_char[] = ".";
    constexpr char pattern_set[] = "[abc]";
    constexpr char pattern_inverted_set[] = "[^abc]";
    constexpr char pattern_range_set[] = "[a-z]";
    constexpr char pattern_inverted_range_set[] = "[^a-z]";
    constexpr char pattern_complex_set[] = "[a-zA-Z_0-9]";
    constexpr char pattern_set_escaping_only_rbracket[] = R"([[{}()|+*?^.\]])";
    constexpr char pattern_set_strange_range_and_minus[] = "[--Z-]";
}


TEST_CASE("any single char", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_any_char> r;

    REQUIRE( r.match("\x20") );
    REQUIRE( r.match("a") );
    REQUIRE( r.match("B") );
    REQUIRE( r.match("0") );

    REQUIRE( !r.match("\x20\x34") );
    REQUIRE( !r.match("aa") );
    REQUIRE( !r.match("BB") );
    REQUIRE( !r.match("000") );
    REQUIRE( !r.match("") );
}

TEST_CASE("set", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_set> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("b") );
    REQUIRE( r.match("c") );

    REQUIRE( !r.match("abc") );
    REQUIRE( !r.match("ab") );
    REQUIRE( !r.match("x") );
    REQUIRE( !r.match("000") );
    REQUIRE( !r.match("") );
}

TEST_CASE("inverted set", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_inverted_set> r;

    REQUIRE( !r.match("a") );
    REQUIRE( !r.match("b") );
    REQUIRE( !r.match("c") );

    REQUIRE( r.match("x") );
    REQUIRE( r.match("y") );
    REQUIRE( r.match("z") );
    REQUIRE( r.match("\x20") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("aa") );
}

TEST_CASE("range set", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_range_set> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("b") );
    REQUIRE( r.match("z") );

    REQUIRE( !r.match("0") );
    REQUIRE( !r.match("A") );
    REQUIRE( !r.match("Z") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("aa") );
}

TEST_CASE("inverted range set", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_inverted_range_set> r;

    REQUIRE( !r.match("a") );
    REQUIRE( !r.match("b") );
    REQUIRE( !r.match("z") );

    REQUIRE( r.match("0") );
    REQUIRE( r.match("A") );
    REQUIRE( r.match("Z") );
    REQUIRE( r.match("\x20") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("aa") );
}

TEST_CASE("complex set", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_complex_set> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("b") );
    REQUIRE( r.match("z") );
    REQUIRE( r.match("0") );
    REQUIRE( r.match("A") );
    REQUIRE( r.match("Z") );
    REQUIRE( r.match("_") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("aa") );
}

TEST_CASE("set escaping rbracket only", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_set_escaping_only_rbracket> r;

    REQUIRE( r.match("[") );
    REQUIRE( r.match("]") );
    REQUIRE( r.match("(") );
    REQUIRE( r.match(")") );
    REQUIRE( r.match("{") );
    REQUIRE( r.match("}") );
    REQUIRE( r.match("|") );
    REQUIRE( r.match("+") );
    REQUIRE( r.match("*") );
    REQUIRE( r.match("^") );
    REQUIRE( r.match(".") );
    REQUIRE( r.match("?") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("aa") );
    REQUIRE( !r.match("x") );
}

TEST_CASE("set range and minus", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_set_strange_range_and_minus> r;

    REQUIRE( r.match("-") );
    REQUIRE( r.match("Z") );
    REQUIRE( r.match("0") );
    REQUIRE( !r.match("a") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("aa") );
}
