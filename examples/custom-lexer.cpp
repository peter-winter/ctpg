#include <ctpg/ctpg.hpp>
#include <iostream>

using namespace ctpg;
using namespace ctpg::buffers;
using namespace ctpg::ftors;

class int_lexer
{
public:
    constexpr int_lexer()
    {}

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
        if (*start >= '0' && *start <= '9')
        {
            // idx == 1, recognized 'number' term
            return recognized(1, options, sp, start, error_stream);
        }
        if (*start == ',')
        {
            // idx == 0, recognized 'comma' term
            return recognized(0, options, sp, start, error_stream);
        }
        return not_recognized(start);
    }

private:
    template<typename Iterator, typename ErrorStream>
    constexpr auto recognized(
        size16_t idx,
        match_options options,
        source_point sp,
        Iterator start,
        ErrorStream& error_stream)
    {
        Iterator term_end = start;
        if (options.verbose)
            error_stream << sp << " LEXER MATCH: Recognized " << idx << " \n";

        // always increase term_end by one, since all terms have length == 1
        sp.update(start, ++term_end);
        return recognized_term(term_end, idx);
    }

    template<typename Iterator>
    constexpr auto not_recognized(Iterator start) const
    {
        return recognized_term(start);
    }
};

constexpr nterm<int> list("list");

constexpr custom_term number("number", [](auto sv){ return int(sv[0]) - '0';} );
constexpr custom_term comma(",", create<no_type>{} );

constexpr parser p(
    list,
    terms(comma, number),       // comma == 0, number == 1
    nterms(list),
    rules(
        list(number),
        list(list, comma, number)
            >= [](int sum, skip, int x){ return sum + x; }
    ),
    use_lexer<int_lexer>{}
);

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        constexpr char example_text[] = "1, 2, 3";
        constexpr auto cres = p.parse(cstring_buffer(example_text));
        std::cout << cres.value() << std::endl;
        return 0;
    }

    auto res = p.parse(parse_options{}.set_verbose(), string_buffer(argv[1]), std::cerr);
    bool success = res.has_value();
    if (success)
        std::cout << res.value() << std::endl;
    return success ? 0 : -1;
}
