#include <catch2/catch.hpp>
#include <ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    struct thing{};
    struct other_thing{};

    constexpr nterm<int> root("root");

    constexpr typed_term t(char_term('@'), [](auto){ return thing{}; });
    constexpr typed_term ot(string_term("##"), [](auto){ return other_thing{}; });

    struct foo
    {
        int operator ()(thing) const { return 1; }
        int operator ()(other_thing) const { return 2; }
    };

    constexpr parser p(
        root,
        terms(t, ot),
        nterms(root),
        rules(
            root(t) >= foo{},
            root(ot) >= foo{}
        )
    );
}

TEST_CASE("typed terms", "[typed terms]")
{
    auto result1 = test::p.parse(cstring_buffer("@"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 1);

    auto result2 = test::p.parse(cstring_buffer("##"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 2);
}
