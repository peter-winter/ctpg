#define CATCH_CONFIG_MAIN

#include <catch2/catch.hpp>
#include "../ctpg.hpp"

constexpr char pattern1[] = "s";
constexpr ctpg::regex::expr<pattern1> r1;

constexpr char pattern2[] = "ss";
constexpr ctpg::regex::expr<pattern2> r2;

TEST_CASE("singe char", "[regex term]")
{
    REQUIRE( r1.match("s") );
    REQUIRE( !r1.match("ss") );
    REQUIRE( !r1.match("") );
}

TEST_CASE("concat singe chars", "[regex term]")
{
    REQUIRE( r2.match("ss") );
    REQUIRE( !r2.match("sss") );
    REQUIRE( !r2.match("s") );
    REQUIRE( !r2.match("") );
}
