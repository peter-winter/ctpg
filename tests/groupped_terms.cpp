#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    constexpr nterm root(use_value<int>, "root");
    
    constexpr char_term c('c');

    constexpr const char p[] = "[0-9]*";
    struct tag{};
    constexpr typed_term t(regex_term(pattern<p>, "d"), create<tag>{});

    constexpr string_term s("ala");
    
    constexpr groupped_term g("g", c, t, s);

    struct visitor
    {
        int operator()(char){ return 1; }
        int operator()(std::string_view){ return 2; }
        int operator()(tag){ return 3; }
    };

    constexpr parser single_term_group_parser(
        root,
        terms(g),
        nterms(root),
        rules(
            root(g) >= [](const auto& v){ return std::visit(visitor{}, v.get_value()); }
        )
    );
}

TEST_CASE("single groupped term", "[groupped terms]")
{
    auto result = test::single_term_group_parser.parse(cstring_buffer("c"));
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 1);
    result = test::single_term_group_parser.parse(cstring_buffer("9099"));
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 3);
    result = test::single_term_group_parser.parse(cstring_buffer("ala"));
    REQUIRE(result.has_value());
    REQUIRE(result.value() == 2);
    result = test::single_term_group_parser.parse(cstring_buffer("e"));
    REQUIRE(!result.has_value());
}