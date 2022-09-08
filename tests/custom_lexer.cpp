#include <catch2/catch.hpp>
#include <ctpg/ctpg.hpp>
#include <sstream>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

namespace test
{
    class custom_lexer
    {
    public:
        template<typename Iterator, typename ErrorStream>
        constexpr auto match(
            match_options options,
            source_point sp,
            Iterator start,
            Iterator end,
            ErrorStream& error_stream)
        {
            if (start == end)
                return not_recognized(start);

            // 'a' -> 0
            // 'b' -> 1
            // "bla" -> 2
            if (*start == 'a')
                return recognized(0, 1, options, sp, start, error_stream);
            Iterator tmp = start;
            if (*tmp++ == 'b')
            {
                if (tmp != end && *tmp++ == 'l')
                    if (tmp != end && *tmp++ == 'a')
                        return recognized(2, 3, options, sp, start, error_stream);
                return recognized(1, 1, options, sp, start, error_stream);
            }


            return not_recognized(start);
        }

    private:
        template<typename Iterator, typename ErrorStream>
        constexpr auto recognized(
            size16_t idx,
            size_t len,
            match_options options,
            source_point sp,
            Iterator start,
            ErrorStream& error_stream)
        {
            if (options.verbose)
                error_stream << sp << " LEXER MATCH: Recognized " << idx << " \n";
            Iterator term_end = start;
            term_end += len;
            sp.update(start, term_end);
            return recognized_term(term_end, idx);
        }

        template<typename Iterator>
        constexpr auto not_recognized(Iterator start) const
        {
            return recognized_term(start);
        }
    };

    constexpr nterm<int> root("root");

    constexpr custom_term bla("ct", [](auto) { return no_type{}; });

    constexpr parser p(
        root,
        terms('a', 'b', bla),
        nterms(root),
        rules(
            root('a') >= val(0),
            root('b') >= val(1),
            root(bla) >= val(42)
        ),
        use_lexer<custom_lexer>{}
    );
}

TEST_CASE("custom lexer", "[custom lexer]")
{
    auto result1 = test::p.parse(cstring_buffer("a"));
    REQUIRE(result1.has_value());
    REQUIRE(result1.value() == 0);

    auto result2 = test::p.parse(cstring_buffer("b"));
    REQUIRE(result2.has_value());
    REQUIRE(result2.value() == 1);

    auto result3 = test::p.parse(cstring_buffer("bla"));
    REQUIRE(result3.has_value());
    REQUIRE(result3.value() == 42);

    std::stringstream stream;
    auto result4 = test::p.parse(cstring_buffer("!"), stream);
    REQUIRE(!result4.has_value());
    REQUIRE(stream.str() == std::string("[1:1] PARSE: Unexpected character: !\n"));
}
