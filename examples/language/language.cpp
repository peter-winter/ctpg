/*
A simple language parser, demonstrates how to set up a language parser with a
abstract syntax tree builder.

Look at the CMakeLists.txt to see how to integrate CTPG into your CMake projects

Build in a separate build direcotry:
mkdir build
cd build
cmake ..
make

Run (in this directory):

./build/language "$(cat ./samples/1)"

When no input string as an argument is provided program prints a parser diagnostic message.
*/

// The header file normally sits in its own directory, somewhere like: /usr/local/include/ctpg/ctpg.hpp
// but this time CMake dealt with it so there is no need for #include <ctpg/ctpg.hpp>
#include <ctpg.hpp>

#include "ast_builder.hpp"

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

ast_builder b;

struct none{};

constexpr nterm<none> code("code");
constexpr nterm<abstracts::class_defs> class_defs("class_defs");
constexpr nterm<abstracts::class_def> class_def("class_def");
constexpr nterm<abstracts::class_header> class_header("class_header");

constexpr const char id_pattern[] = "[A-Za-z_][0-9A-Za-z_]*";
constexpr regex_term<id_pattern> id("id");

template<typename T>
auto get_sp(term_value<T> tv)
{
    return b.source_point(tv.get_sp().line, tv.get_sp().column);
}

constexpr parser p(
    code,
    terms("class", id, '{', '}'),
    nterms(code, class_defs, class_def, class_header),
    rules(
        code(class_defs)
            >= [](auto&& list) { b.add_defs(std::move(list)); return none{}; } ,
        class_defs()
            >= create<abstracts::class_defs>{},
        class_defs(class_defs, class_def)
            >= emplace_back{},
        class_def(class_header, '{', '}')
            >= [](auto&& h, char, char){ return b.class_def(std::move(h)); },
        class_header("class", id)
            >= [](auto kw, auto id){ return b.class_header(get_sp(kw), id); }
    )
);

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        p.write_diag_str(std::cout);
        return 0;
    }

    string_buffer buffer (argv[1]);
    auto res = p.parse(parse_options{}.set_verbose(), buffer, std::cerr);
    bool success = res.has_value();
    b.dump(std::cout);
    return success ? 0 : -1;
}
