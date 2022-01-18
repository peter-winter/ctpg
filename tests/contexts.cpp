#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    constexpr nterm<int> root("root");

    constexpr parser p1(
        root,
        terms('+', '1'),
        nterms(root),
        rules(
            root('+') >>= [](auto context, auto) { return ++context; },
            root('1') >= [](auto) { return 1; }
        )
    );
}

TEST_CASE("non-contextual reductors with context", "[contexts]")
{
    auto result1 = test::p1.context_parse(5, cstring_buffer("1"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 1);

    auto result2 = test::p1.context_parse(10, cstring_buffer("1"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 1);
}

TEST_CASE("value context", "[contexts]")
{
    auto result1 = test::p1.context_parse(5, cstring_buffer("+"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 6);

    int context = 10;
    auto result2 = test::p1.context_parse(context, cstring_buffer("+"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 11);
    REQUIRE(context == 10);
}

namespace test
{
    struct mutable_non_copyable {
        mutable_non_copyable(int data): data(data) {}
        mutable_non_copyable(const mutable_non_copyable&) = delete;
        mutable_non_copyable& operator=(const mutable_non_copyable&) = delete;

        int data = 0;
    };

    constexpr parser p2(
        root,
        terms('$'),
        nterms(root),
        rules(
            root('$') >>= [](const auto &context, auto) { return context.data; }
        )
    );
}

TEST_CASE("const reference context", "[contexts]")
{
    const test::mutable_non_copyable context1(5);
    auto result1 = test::p2.context_parse(context1, cstring_buffer("$"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 5);

    const test::mutable_non_copyable context2(10);
    auto result2 = test::p2.context_parse(context2, cstring_buffer("$"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 10);
}

namespace test
{
    constexpr parser p3(
        root,
        terms('+'),
        nterms(root),
        rules(
            root('+') >>= [](auto &context, auto) { return ++context.data; }
        )
    );
}

TEST_CASE("non-const reference context", "[contexts]")
{
    test::mutable_non_copyable context1(5);
    auto result1 = test::p3.context_parse(context1, cstring_buffer("+"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 6);
    REQUIRE(context1.data == 6);

    auto result2 = test::p3.context_parse(context1, cstring_buffer("+"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 7);
    REQUIRE(context1.data == 7);
}
