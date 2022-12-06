#include <ctpg/ctpg.hpp>
#include <iostream>

using namespace ctpg;
using namespace ctpg::buffers;
using namespace ctpg::ftors;

constexpr int get_int(std::string_view sv)
{
    int sum = 0;
    for (size_t i = 0; i < sv.size(); ++i)
    {
        sum *= 10;
        int digit = sv[i] - '0';
        sum += digit;
    }
    return sum;
}

constexpr nterm<int> expr("expr");

constexpr string_term o_or("||", 1, associativity::ltor);
constexpr string_term o_and("&&", 2, associativity::ltor);
constexpr char_term o_bit_or('|', 3, associativity::ltor);
constexpr char_term o_bit_xor('^', 4, associativity::ltor);
constexpr char_term o_bit_and('&', 5, associativity::ltor);
constexpr string_term o_eq("==", 6, associativity::ltor);
constexpr string_term o_neq("!=", 6, associativity::ltor);
constexpr char_term o_plus('+', 7, associativity::ltor);
constexpr char_term o_minus('-', 7, associativity::ltor);
constexpr char_term o_mul('*', 8, associativity::ltor);
constexpr string_term o_pow("**", 8, associativity::ltor);
constexpr char_term o_div('/', 8, associativity::ltor);
constexpr char_term o_mod('%', 8, associativity::ltor);

constexpr char number_pattern[] = "[1-9][0-9]*";
constexpr regex_term<number_pattern> number("number");

constexpr parser p(
    expr,
    terms('0', number, o_plus, o_minus, o_mul, o_div, '(', ')', '~', "&&", "||", "**", "==", "!=", '%', '^', '&', '|', '!'),
    nterms(expr),
    rules(
        expr(expr, '+', expr) >= [](auto e1, skip, auto e2){ return e1 + e2; },
        expr(expr, '-', expr) >= [](auto e1, skip, auto e2){ return e1 - e2; },
        expr(expr, '*', expr) >= [](auto e1, skip, auto e2){ return e1 * e2; },
        expr(expr, '/', expr) >= [](auto e1, skip, auto e2){ return e1 / e2; },
        expr(expr, "**", expr) >= [](auto e1, skip, auto e2){ int res = 1; for(int i = 0; i < e2; ++i) res *= e1; return res; },
        expr(expr, "&&", expr) >= [](auto e1, skip, auto e2){ return e1 && e2; },
        expr(expr, "||", expr) >= [](auto e1, skip, auto e2){ return e1 || e2; },
        expr(expr, "==", expr) >= [](auto e1, skip, auto e2){ return e1 == e2; },
        expr(expr, "!=", expr) >= [](auto e1, skip, auto e2){ return e1 != e2; },
        expr(expr, '%', expr) >= [](auto e1, skip, auto e2){ return e1 % e2; },
        expr(expr, '^', expr) >= [](auto e1, skip, auto e2){ return e1 ^ e2; },
        expr(expr, '&', expr) >= [](auto e1, skip, auto e2){ return e1 & e2; },
        expr(expr, '|', expr) >= [](auto e1, skip, auto e2){ return e1 | e2; },
        expr('-', expr)[9] >= [](skip, int e) { return -e; },
        expr('!', expr)[9] >= [](skip, int e) { return !e; },
        expr('~', expr)[9] >= [](skip, int e) { return ~e; },
        expr('(', expr, ')') >= _e2,
        expr(number) >= [](const auto& sv){ return get_int(sv); },
        expr('0') >= val(0)
    )
);

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        p.write_diag_str(std::cout);
        return 0;
    }

    auto res = p.parse(parse_options{}.set_verbose(), string_buffer(argv[1]), std::cerr);
    if (res.has_value())
    {
        int rv = res.value();
        std::cout << "runtime parse: " << rv << std::endl;
    }
    return 0;
}
