#include <iostream>
#include <sstream>

#include "../ctpg.hpp"

using namespace ctpg;

constexpr char pattern[] = R"([0-9a-zA-Z_]+)";
constexpr regex::expr<pattern> r;

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        r.write_diag_str(std::cout);
        return 0;
    }
    bool m = r.match(match_options{}.set_verbose(), buffers::string_buffer(argv[1]), std::cout);
    std::cout << (m ? "runtime match" : "runtime match fail") << std::endl;
    return 0;
}
