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

using exprs_type = std::vector<int>;

constexpr nterm<int> expr("expr");
constexpr nterm<exprs_type> exprs("exprs");

constexpr char_term o_plus('+', 1, associativity::ltor);
constexpr char_term o_minus('-', 1, associativity::ltor);
constexpr char_term o_mul('*', 2, associativity::ltor);
constexpr char_term o_div('/', 2, associativity::ltor);

constexpr char number_pattern[] = "[1-9][0-9]*";
constexpr regex_term<number_pattern> number("number");

constexpr parser p(
    exprs,
    terms(number, o_plus, o_minus, o_mul, o_div, '(', ')', ';'),
    nterms(exprs, expr),
    rules(
        exprs() >= create<exprs_type>{},
        exprs(exprs, expr, ';') >= push_back<1, 2>{},
        exprs(exprs, error, ';') >= _e1,
        expr(expr, '+', expr) >= [](int x1, skip, int x2){ return x1 + x2; },
        expr(expr, '-', expr) >= [](int x1, skip, int x2){ return x1 - x2; },
        expr(expr, '*', expr) >= [](int x1, skip, int x2){ return x1 * x2; },
        expr(expr, '/', expr) >= [](int x1, skip, int x2){ return x1 / x2; },
        expr('-', expr)[3] >= [](char, int x) { return -x; },
        expr('(', expr, ')') >= _e2,
        expr(number) >= [](const auto& sv){ return get_int(sv); }
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
        for (int x : res.value())
            std::cout << "runtime parse: " << x << std::endl;
    }
    return 0;
}
