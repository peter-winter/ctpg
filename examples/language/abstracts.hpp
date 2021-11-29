#ifndef ABSTRACTS_H
#define ABSTRACTS_H

#include <vector>
#include <string_view>
#include <iostream>

namespace abstracts
{
    // abstracts provide their own source point data,
    // just so the abstract classes don't have the dependency on ctpg
    struct source_point
    {
        int line;
        int column;
    };

    class source_object
    {
    public:
        source_object(source_point sp):
            sp(sp)
        {}
        source_object(const source_object&) = delete;
        source_object& operator = (const source_object&) = delete;
        source_object(source_object&&) = default;
        source_object& operator = (source_object&&) = default;

        source_point get_sp() const { return sp; }

    private:
        source_point sp;
    };

    class class_header : public source_object
    {
    public:
        class_header(source_point sp, std::string_view name):
            source_object(sp), name(name)
        {}
        class_header(class_header&&) = default;
        class_header& operator = (class_header&&) = default;

        std::string_view get_name() const { return name; }

    private:
        std::string_view name;
    };

    class class_def : public source_object
    {
    public:
        class_def(source_point sp, std::string_view name):
            source_object(sp), name(name)
        {}
        class_def(class_def&&) = default;
        class_def& operator = (class_def&&) = default;

        std::string_view get_name() const { return name; }

    private:
        std::string_view name;
    };

    using class_defs = std::vector<class_def>;
}

#endif
