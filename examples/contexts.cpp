#include <ctpg/ctpg.hpp>
#include <iostream>
#include <unordered_map>
#include <string>
#include <utility>

using namespace ctpg;
using namespace ctpg::buffers;
using namespace ctpg::ftors;

using predefined_variables = std::unordered_map<std::string_view, int>;

constexpr nterm<int> expr("expr");

constexpr char_term o_plus('+', 1, associativity::ltor);

constexpr char variable_pattern[] = "[a-zA-Z_][a-zA-Z_0-9]*";
constexpr regex_term<variable_pattern> variable("variable");

constexpr parser p(
    expr,
    terms(variable, o_plus),
    nterms(expr),
    rules(
        expr(expr, o_plus, expr) >= [](auto left, auto, auto right)
        {
            return left + right;
        },
        expr(variable) >>= [](const auto& variables, const auto& ident)
        {
            return variables.count(ident) == 0 ? 0 : variables.at(ident);
        }
    )
);

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        p.write_diag_str(std::cout);
        return 0;
    }

    const predefined_variables variables({{"var0", 10}, {"var1", 20}, {"var2", 30}});

    auto res = p.context_parse(variables, parse_options{}.set_verbose(), string_buffer(argv[1]), std::cerr);
    if (res.has_value())
    {
        int rv = res.value();
        std::cout << "runtime parse: " << rv << std::endl;
    }
    return 0;
}
