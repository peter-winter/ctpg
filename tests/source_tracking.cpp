#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    constexpr nterm<std::vector<ctpg::size32_t>> root("root");

    auto foo(term_value<char> c1, term_value<char> c2, term_value<char> c3)
    {
        return std::vector<ctpg::size32_t>{c1.get_sp().column, c2.get_sp().column, c3.get_sp().column};
    }

    constexpr parser p1(
        root,
        terms('x'),
        nterms(root),
        rules(
            root('x', 'x', 'x') >= foo
        )
    );
}

TEST_CASE("source tracking", "[source tracking]")
{
    auto result1 = test::p1.parse(cstring_buffer(" x x x "));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == std::vector<ctpg::size32_t>{2, 4, 6});
}
