/*
HTML parser
It only deals with matching tags parsing for simplification.
Texts and attributes are a trivial task to add
This example shows how to take advantage of the fact that the parser is defined in c++.
Here both terms and rules are generated from tag names
*/
#include <ctpg/ctpg.hpp>

#include <iostream>
#include <vector>

using namespace ctpg;
using namespace ctpg::ftors;
using namespace ctpg::buffers;

struct html_block;
using list_type = std::vector<html_block>;

class html_block
{
public:
    html_block(std::string_view tag, list_type&& list):
        name(tag), inner_blocks(std::move(list))
    {}

private:
    std::string name;
    list_type inner_blocks;
};

template<std::size_t N>
struct tag
{
    constexpr tag(const char (&name)[N])
    {
        open_tag_str[0] = '<';
        for (auto i = 0u; i < N - 1; ++i)   // skip terminating 0
            open_tag_str[i + 1] = name[i];
        open_tag_str[N] = '>';

        close_tag_str[0] = '<';
        close_tag_str[1] = '/';
        for (auto i = 0u; i < N - 1; ++i)   // skip terminating 0
            close_tag_str[i + 2] = name[i];
        close_tag_str[N + 1] = '>';
    }

    constexpr const auto& get_open_tag_str() const { return open_tag_str; }
    constexpr const auto& get_close_tag_str() const { return close_tag_str; }

    char open_tag_str[N + 2] = {0};
    char close_tag_str[N + 3] = {0};
};

constexpr auto tag_tuple = std::make_tuple(
    tag("div"),
    tag("span"),
    tag("a"),
    tag("button")
);

constexpr nterm<html_block> block("block");
constexpr nterm<list_type> block_list("list");

template<typename Tags>
constexpr auto create_terms(Tags tags)
{
    return std::apply(
        [](auto... tag)
        {
            return ctpg::terms(tag.get_open_tag_str()..., tag.get_close_tag_str()...);
        },
        tags
    );
}

template<typename Tags>
constexpr auto create_rules(Tags tags)
{
    return std::apply(
        [](auto... tag)
        {
            // create rule in the form:
            //      block("<tag>" block_list "</tag>") for each tag
            // and add last two rules not depending on tags
            return ctpg::rules(
                block(tag.get_open_tag_str(), block_list, tag.get_close_tag_str())
                    >= [](auto tag, auto&& list, skip){ return html_block(tag, std::move(list)); }...,
                block_list()
                    >= create<list_type>{},
                block_list(block_list, block)
                    >= [](auto&& list, auto&& block){ list.emplace_back(std::move(block)); return std::move(list); }
            );
        },
        tags
    );
}

constexpr parser p(
    block,                      // root
    create_terms(tag_tuple),    // terms
    nterms(block, block_list),  // nonterms
    create_rules(tag_tuple)     // rules
);

int main(int argc, char const *argv[])
{
    if (argc != 2)
    {
        p.write_diag_str(std::cout);
        return 0;
    }

    auto res = p.parse(parse_options{}.set_verbose(), string_buffer(argv[1]), std::cerr);
    return 0;
}
