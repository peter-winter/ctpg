#ifndef AST_BUILDER_H
#define AST_BUILDER_H

#include <vector>

#include "abstracts.hpp"

namespace ab = abstracts;

class ast_builder
{
public:
    auto source_point(int line, int column) const
    {
        // abstracts provide their own source point data,
        // just so the abstract classes don't have the dependency on ctpg
        return ab::source_point{line, column};
    }

    ab::class_def class_def(const ab::class_header& header) const
    {
        return ab::class_def(header.get_sp(), header.get_name());
    }

    ab::class_header class_header(ab::source_point sp, std::string_view name) const
    {
        return ab::class_header(sp, name);
    }

    void add_defs(std::vector<ab::class_def>&& classes)
    {
        this->classes = std::move(classes);
    }

    template<typename Stream>
    void dump(Stream& s)
    {
        s << std::endl << std::endl;
        for (const auto&cl : classes)
        {
            s << "class: " << cl.get_name() << " at [" << cl.get_sp().line << ":" << cl.get_sp().column << "]" << std::endl;
        }
    }

private:
    std::vector<ab::class_def> classes;
};

#endif
