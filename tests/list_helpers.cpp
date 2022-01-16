#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>
#include <vector>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    struct copyable_thing
    {};

    constexpr nterm<std::vector<copyable_thing>> c_root("c_root");
    constexpr nterm<copyable_thing> ct("ct");

    constexpr parser p1(
        c_root,
        terms('*'),
        nterms(c_root, ct),
        rules(
            c_root() >= create<std::vector<copyable_thing>>{},
            c_root(c_root, ct) >= push_back{},
            ct('*') >= create<copyable_thing>{}
        )
    );
}

TEST_CASE("push back", "[list helpers]")
{
    auto result = test::p1.parse(cstring_buffer("****"));

    REQUIRE(result.has_value());
    REQUIRE(result.value().size() == 4);
}

namespace test
{
    struct non_copyable_thing
    {
        non_copyable_thing(const non_copyable_thing&) = delete;
        non_copyable_thing& operator = (const non_copyable_thing&) = delete;

        non_copyable_thing(non_copyable_thing&&) = default;
        non_copyable_thing& operator = (non_copyable_thing&&) = default;
    };

    constexpr nterm<std::vector<non_copyable_thing>> nc_root("nc_root");
    constexpr nterm<non_copyable_thing> nct("nct");

    constexpr parser p2(
        nc_root,
        terms('*'),
        nterms(nc_root, nct),
        rules(
            nc_root() >= create<std::vector<non_copyable_thing>>{},
            nc_root(nc_root, nct) >= emplace_back{},
            nct('*') >= create<non_copyable_thing>{}
        )
    );
}

TEST_CASE("emplace back", "[list helpers]")
{
    auto result = test::p2.parse(cstring_buffer("****"));

    REQUIRE(result.has_value());
    REQUIRE(result.value().size() == 4);
}

namespace test {
    constexpr parser p3(
        c_root,
        terms('*', ','),
        nterms(c_root, ct),
        rules(
            c_root(ct) >= construct<std::vector<copyable_thing>, 1>{},
            c_root(c_root, ',', ct) >= push_back<1, 3>{},
            ct('*') >= create<copyable_thing>{}
        )
    );
}

TEST_CASE("construct", "[list helpers]")
{
    auto result = test::p3.parse(cstring_buffer("*,*,*,*"));

    REQUIRE(result.has_value());
    REQUIRE(result.value().size() == 4);
}