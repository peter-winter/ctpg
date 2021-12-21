#include <ctpg/ctpg.hpp>
#include <iostream>

using namespace ctpg;
using namespace ctpg::buffers;
using namespace ctpg::ftors;

struct plus_op {};
struct minus_op {};
struct mul_op {};
struct div_op {};

struct calculator
{
    constexpr int operator()(int x1, plus_op, int x2) const
    {
        return x1 + x2;
    }

    constexpr int operator()(int x1, minus_op, int x2) const
    {
        return x1 - x2;
    }

    constexpr int operator()(int x1, mul_op, int x2) const
    {
        return x1 * x2;
    }

    constexpr int operator()(int x1, div_op, int x2) const
    {
        return x1 / x2;
    }

    constexpr int operator()(minus_op, int x) const
    {
        return -x;
    }
};

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

constexpr typed_term o_plus(char_term('+', 1, associativity::ltor), create<plus_op>{});
constexpr typed_term o_minus(char_term('-', 1, associativity::ltor), create<minus_op>{});
constexpr typed_term o_mul(char_term('*', 2, associativity::ltor), create<mul_op>{});
constexpr typed_term o_div(char_term('/', 2, associativity::ltor), create<div_op>{});

constexpr char number_pattern[] = "[1-9][0-9]*";
constexpr typed_term number(regex_term<number_pattern>("number"), get_int);

constexpr calculator c;

constexpr parser p(
    expr,
    terms(number, o_plus, o_minus, o_mul, o_div, '(', ')'),
    nterms(expr),
    rules(
        expr(expr, o_plus, expr) >= c,
        expr(expr, o_minus, expr) >= c,
        expr(expr, o_mul, expr) >= c,
        expr(expr, o_div, expr) >= c,
        expr(o_minus, expr)[3] >= c,
        expr('(', expr, ')') >= _e2,
        expr(number)
    )
);

int main(int argc, char* argv[])
{
    if (argc != 2)
        return -1;

    auto res = p.parse(parse_options{}.set_verbose(), string_buffer(argv[1]), std::cerr);
    if (res.has_value())
    {
        int rv = res.value();
        std::cout << "runtime parse: " << rv << std::endl;
    }
    return 0;
}
