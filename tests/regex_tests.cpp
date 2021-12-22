#include <catch2/catch.hpp>
#include <ctpg.hpp>


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
constexpr char pattern_any_char[] = ".";
constexpr char pattern_set[] = "[abc]";
constexpr char pattern_inverted_set[] = "[^abc]";
constexpr char pattern_range_set[] = "[a-z]";
constexpr char pattern_inverted_range_set[] = "[^a-z]";
constexpr char pattern_complex_set[] = "[a-zA-Z_0-9]";
constexpr char pattern_set_escaping_only_rbracket[] = R"([[{}()|+*?^.\]])";
constexpr char pattern_set_strange_range_and_minus[] = "[--Z-]";
constexpr char pattern_star[] = "a*";
constexpr char pattern_plus[] = "a+";
constexpr char pattern_opt[] = "a?";
constexpr char pattern_times[] = "a{5}";
constexpr char pattern_alt[] = "a|b";
constexpr char pattern_groupping[] = "(a|b)*";
constexpr char pattern_precedence[] = "a|bc*";

TEST_CASE("singe char", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_single_char> r;

    REQUIRE( r.match("s") );
    REQUIRE( !r.match("ss") );
    REQUIRE( !r.match("") );
}

TEST_CASE("concat singe chars", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_concat_single_chars> r;

    REQUIRE( r.match("ss") );
    REQUIRE( !r.match("sss") );
    REQUIRE( !r.match("s") );
    REQUIRE( !r.match("") );
}

TEST_CASE("escaped char", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_escaped_char> r;

    REQUIRE( r.match("+") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("++") );
    REQUIRE( !r.match("\\+") );
}

TEST_CASE("escaped char hex 0", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_escaped_char_hex_0> r;

    REQUIRE( r.match("\0") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("0") );
}

TEST_CASE("escaped char hex 1 digit", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_escaped_char_hex_1_digit1> r1;

    REQUIRE( r1.match("\x2") );
    REQUIRE( !r1.match("\x7") );
    REQUIRE( !r1.match("") );
    REQUIRE( !r1.match("x2") );

    constexpr ctpg::regex::expr<pattern_escaped_char_hex_1_digit2> r2;

    REQUIRE( r2.match("\xa") );
    REQUIRE( !r2.match("\xb") );
    REQUIRE( !r2.match("") );
    REQUIRE( !r2.match("xa") );

    constexpr ctpg::regex::expr<pattern_escaped_char_hex_1_digit3> r3;

    REQUIRE( r3.match("\xa") );
    REQUIRE( !r3.match("\xb") );
    REQUIRE( !r3.match("") );
    REQUIRE( !r3.match("xA") );
}

TEST_CASE("escaped char hex 2 digit", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_escaped_char_hex_2_digit1> r1;

    REQUIRE( r1.match("\x20") );
    REQUIRE( !r1.match("\x7") );
    REQUIRE( !r1.match("") );
    REQUIRE( !r1.match("x20") );

    constexpr ctpg::regex::expr<pattern_escaped_char_hex_2_digit2> r2;

    REQUIRE( r2.match("\xaf") );
    REQUIRE( !r2.match("\xbf") );
    REQUIRE( !r2.match("") );
    REQUIRE( !r2.match("xaf") );

    constexpr ctpg::regex::expr<pattern_escaped_char_hex_2_digit3> r3;

    REQUIRE( r3.match("\xad") );
    REQUIRE( !r3.match("\xbf") );
    REQUIRE( !r3.match("") );
    REQUIRE( !r3.match("xad") );
}

TEST_CASE("any single char", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_any_char> r;

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
    constexpr ctpg::regex::expr<pattern_set> r;

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
    constexpr ctpg::regex::expr<pattern_inverted_set> r;

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
    constexpr ctpg::regex::expr<pattern_range_set> r;

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

TEST_CASE("complex set", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_complex_set> r;

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
    constexpr ctpg::regex::expr<pattern_set_escaping_only_rbracket> r;

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
    constexpr ctpg::regex::expr<pattern_set_strange_range_and_minus> r;

    REQUIRE( r.match("-") );
    REQUIRE( r.match("Z") );
    REQUIRE( r.match("0") );
    REQUIRE( !r.match("a") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("") );
    REQUIRE( !r.match("aa") );
}

TEST_CASE("star", "[regex term]")
{
    constexpr ctpg::regex::expr<pattern_star> r;

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
    constexpr ctpg::regex::expr<pattern_plus> r;

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
    constexpr ctpg::regex::expr<pattern_opt> r;

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
    constexpr ctpg::regex::expr<pattern_times> r;

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
    constexpr ctpg::regex::expr<pattern_alt> r;

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
    constexpr ctpg::regex::expr<pattern_groupping> r;

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
    constexpr ctpg::regex::expr<pattern_precedence> r;

    REQUIRE( r.match("a") );
    REQUIRE( r.match("b") );
    REQUIRE( r.match("bc") );
    REQUIRE( r.match("bcc") );
    REQUIRE( r.match("bcccc") );
    REQUIRE( !r.match("bcbc") );
    REQUIRE( !r.match("\x20") );
    REQUIRE( !r.match("accccc") );
}
