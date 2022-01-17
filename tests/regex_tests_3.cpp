#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>

namespace test
{
    constexpr char pattern_star[] = "a*";
    constexpr char pattern_plus[] = "a+";
    constexpr char pattern_opt[] = "a?";
    constexpr char pattern_times[] = "a{5}";
    constexpr char pattern_alt[] = "a|b";
    constexpr char pattern_groupping[] = "(a|b)*";
    constexpr char pattern_precedence[] = "a|bc*";

    constexpr char pattern_number[] = "0|[1-9][0-9]*";
}

TEST_CASE("star", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_star> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("aaa") );
    REQUIRE( r.match("aaaaa") );
    REQUIRE( r.match("") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("b") );
    REQUIRE( !r.match("bbaa") );
}

TEST_CASE("plus", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_plus> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("aaa") );
    REQUIRE( r.match("aaaaa") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("b") );
    REQUIRE( !r.match("bbaa") );
}

TEST_CASE("opt", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_opt> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("") );
    REQUIRE( !r.match("aaa") );
    REQUIRE( !r.match("aaaaa") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("b") );
    REQUIRE( !r.match("bbaa") );
}

TEST_CASE("times", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_times> r;

    REQUIRE( r.match("aaaaa") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("aaa") );
    REQUIRE( !r.match("aaaa") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("b") );
    REQUIRE( !r.match("bbaa") );
}

TEST_CASE("alt", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_alt> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("b") );
    REQUIRE( !r.match("aaa") );
    REQUIRE( !r.match("aaaaa") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("c") );
    REQUIRE( !r.match("bbaa") );
}

TEST_CASE("groupping", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_groupping> r;

    REQUIRE( r.match("abaabaa") );
    REQUIRE( r.match("") );
    REQUIRE( r.match("aaba") );
    REQUIRE( r.match("aaaa") );
    REQUIRE( r.match("b") );
    REQUIRE( r.match("bbaa") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("c") );
    REQUIRE( !r.match("sdf") );
}

TEST_CASE("precedence", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_precedence> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("b") );
    REQUIRE( r.match("bc") );
    REQUIRE( r.match("bcc") );
    REQUIRE( r.match("bcccc") );
    REQUIRE( !r.match("bcbc") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("accccc") );
}

TEST_CASE("number", "[regex term]")
{
    constexpr ctpg::regex::expr<test::pattern_number> r;

    REQUIRE( r.match("0") );
    REQUIRE( r.match("1") );
    REQUIRE( r.match("120") );
    REQUIRE( !r.match("01") );
    REQUIRE( !r.match("ss") );
    REQUIRE( !r.match("") );
}
