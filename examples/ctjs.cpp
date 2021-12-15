/*
Compile Time JSON Parser example
*/

#include "../ctpg.hpp"

#include <iostream>
#include <variant>

template<typename = void> // just so it doesn't complain it's not a constexpr function
constexpr void invalid_unicode_sequence()
{
    throw std::runtime_error("invalid string: surrogate [\\ud800-\\udbff] must be followed by [\\udc00-\\udfff]");
}

namespace ctjs
{
    template<typename... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template<typename... Ts> overloaded(Ts...) -> overloaded<Ts...>;

    using namespace ctpg;

    constexpr char number_pattern[] = R"_(\-?(0|[1-9][0-9]*)(\.[0-9]+)?((e|E)(\+|\-)[0-9]+)?)_";
    constexpr char string_pattern[] = R"_("([^\\"\x00-\x1F]|\\[\\"/bfnrt]|\\u[0-9A-Fa-f]{4})*")_";

    using idx_t = std::size_t;
    struct string_info { idx_t start; idx_t end; };
    struct array_tag { idx_t size; };
    struct object_tag { idx_t size; };

    using chunk = std::variant<
        std::nullptr_t,
        bool,
        double,
        string_info,
        array_tag,
        object_tag
    >;

    class chunks_builder
    {
    public:
        constexpr chunks_builder(chunk* chunks, char* string_memory):
            chunks(chunks), string_memory(string_memory)
        {}

        constexpr idx_t get_size() const { return size; }

        template<typename T>
        constexpr idx_t add_new_chunk_and_increase(T arg)
        {
            chunks[idx++] = chunk(arg);
            size++;
            return idx;
        }

        template<typename T>
        constexpr idx_t set_size(idx_t idx, idx_t size)
        {
            std::get<T>(chunks[idx]).size = size;
            return idx;
        }

        constexpr double to_js_number(std::string_view sv)
        {
            auto it = sv.begin();
            auto end = sv.end();

            double sign = 1.0;
            if (*it == '-')
            {
                sign = -1.0;
                it++;
            }

            auto get_int = [end](auto& it, char limit1, char limit2, char limit3, char limit4)
            {
                std::uint64_t res = 0;
                while (true)
                {
                    if (it == end || *it == limit1 || *it == limit2 || *it == limit3 || *it == limit4)
                        break;
                    res *= 10;
                    res += *it - '0';
                    ++it;
                }
                return res;
            };
            std::uint64_t dec = get_int(it, '.', 'E', 'e', '\0');
            if (*it == '.')
                ++it;
            auto it_before_frac = it;
            std::uint64_t frac = get_int(it, 'E', 'e', '\0', '\0');
            std::uint64_t frac_digits = it - it_before_frac;

            std::uint64_t exp = 0;
            bool exp_minus = false;
            if (*it == 'e' || *it == 'E')
            {
                ++it;
                exp_minus = *it++ == '-';
                exp = get_int(it, '\0', '\0', '\0', '\0');
            }

            double frac_d = double(frac);
            for (size_t i = 0; i < frac_digits; ++i)
                frac_d /= 10.0;

            double result = sign * (dec + frac_d);
            for (size_t i = 0; i < exp; ++i)
                result = exp_minus ? result / 10 : result * 10;
            return result;
        }

        constexpr string_info add_js_string(std::string_view sv)
        {
            idx_t old_string_memory_idx = string_memory_idx;

            for (size_t i = 1; i < sv.size() - 1; ++i)
            {
                char c = sv[i];
                if (c == '\\')
                {
                    c = sv[++i];
                    if (c == 'u')
                    {
                        ++i;
                        std::uint32_t cp = hex_digits_to_codepoint(sv.substr(i));
                        i += 4;
                        if (cp >= 0xd800 && cp <= 0xdbff)
                        {
                            if (sv[i] == '\\' && sv[++i] == 'u')
                            {
                                ++i;
                                std::uint32_t cp2 = hex_digits_to_codepoint(sv.substr(i));
                                i += 4;
                                if (cp2 >= 0xdc00 && cp2 <= 0xdfff)
                                {
                                    cp = (cp << 10) + cp2 - 0x35fdc00u;
                                }
                                else
                                    invalid_unicode_sequence();
                            }
                            else
                                invalid_unicode_sequence();
                        }
                        if (cp <= 0x80)
                            string_memory[string_memory_idx++] = char(cp);
                        else if (cp <= 0x7ff)
                        {
                            string_memory[string_memory_idx++] = char((cp >> 6) | 0xc0);
                            string_memory[string_memory_idx++] = char((cp & 0x3f) | 0x80);
                        }
                        else if (cp <= 0xffff)
                        {
                            string_memory[string_memory_idx++] = char((cp >> 12) | 0xe0);
                            string_memory[string_memory_idx++] = char(((cp >> 6) & 0x3f) | 0x80);
                            string_memory[string_memory_idx++] = char((cp & 0x3f) | 0x80);
                        }
                        else
                        {
                            string_memory[string_memory_idx++] = char((cp >> 18) | 0xf0);
                            string_memory[string_memory_idx++] = char(((cp >> 12) & 0x3f) | 0x80);
                            string_memory[string_memory_idx++] = char(((cp >> 6) & 0x3f) | 0x80);
                            string_memory[string_memory_idx++] = char((cp & 0x3f) | 0x80);
                        }
                    }
                    else
                    {
                        if (c == 'b')
                            c = '\x08';
                        else if (c == 't')
                            c = '\x09';
                        else if (c == 'n')
                            c = '\x0a';
                        else if (c == 'f')
                            c = '\x0c';
                        else if (c == 'r')
                            c = '\x0d';
                        string_memory[string_memory_idx++] = c;
                    }
                }
                else
                    string_memory[string_memory_idx++] = c;
            }
            return string_info{ old_string_memory_idx, string_memory_idx };
        }

    private:
        constexpr std::uint32_t hex_digits_to_codepoint(std::string_view sv)
        {
            auto f = [](char d) -> std::uint32_t
            {
                if (d >= 'A' && d <= 'F')
                    return std::uint32_t(10 + d - 'A');
                else if (d >= 'a' && d <= 'f')
                    return std::uint32_t(10 + d - 'a');
                else
                    return std::uint32_t(d - '0');
            };
            return (f(sv[0]) << 12) | (f(sv[1]) << 8) | (f(sv[2]) << 4 )| f(sv[3]);
        }

        idx_t idx = 0;
        idx_t string_memory_idx = 0;
        chunk* chunks;
        char* string_memory;
        idx_t size = 0;

    };

    constexpr auto create_parser(chunks_builder &b)
    {
        constexpr regex_term<number_pattern> js_number("js_number");
        constexpr regex_term<string_pattern> js_string("js_string");

        constexpr nterm<idx_t> js_value("js_value");
        constexpr nterm<idx_t> js_object("js_object");
        constexpr nterm<idx_t> js_object_start("js_object_start");
        constexpr nterm<idx_t> js_object_elements("js_object_elements");
        constexpr nterm<idx_t> js_object_element("js_object_element");
        constexpr nterm<idx_t> js_array("js_array");
        constexpr nterm<idx_t> js_array_start("js_array_start");
        constexpr nterm<idx_t> js_array_elements("js_array_elements");

        return parser(
            js_object,
            terms(js_number, js_string, "true", "false", "null", '[', ']', ',', '{', '}', ':'),
            nterms(js_object, js_object_start, js_array, js_array_start, js_value, js_array_elements, js_object_elements, js_object_element),
            rules(
                js_value(js_number)
                    >= [&b](auto sv){ return b.add_new_chunk_and_increase(b.to_js_number(sv)); },
                js_value(js_string)
                    >= [&b](auto sv){ return b.add_new_chunk_and_increase(b.add_js_string(sv)); },
                js_value("true")
                    >= [&b](auto sv){ return b.add_new_chunk_and_increase(true); },
                js_value("false")
                    >= [&b](auto sv){ return b.add_new_chunk_and_increase(false);},
                js_value("null")
                    >= [&b](auto sv){ return b.add_new_chunk_and_increase(nullptr); },
                js_value(js_array),
                js_value(js_object),
                js_array_start('[')
                    >= [&b](skip){ return b.add_new_chunk_and_increase(array_tag{ 0 }) - 1; }, // return start of array idx
                js_array(js_array_start, js_array_elements, ']')
                    >= [&b](auto arr_idx, auto arr_size, skip){ return b.set_size<array_tag>(arr_idx, arr_size); },
                js_array('[', ']')
                    >= [&b](skip, skip){ return b.add_new_chunk_and_increase(array_tag{ 0 }); },
                js_object_start('{')
                    >= [&b](skip){ return b.add_new_chunk_and_increase(object_tag{ 0 }) - 1; }, // return start of object idx
                js_object(js_object_start, js_object_elements, '}')
                    >= [&b](auto obj_idx, auto obj_size, skip){ return b.set_size<object_tag>(obj_idx, obj_size); },
                js_object('{', '}')
                    >= [&b](skip, skip){ return b.add_new_chunk_and_increase(object_tag{ 0 }); },
                js_array_elements(js_value)
                    >= [](skip){ return 1; },
                js_array_elements(js_array_elements, ',', js_value)
                    >= [](auto arr_size, skip, skip) { return arr_size + 1; },
                js_object_elements(js_object_element)
                    >= [](skip){ return 1; },
                js_object_elements(js_object_elements, ',', js_object_element)
                    >= [](auto obj_size, skip, skip) { return obj_size + 1; },
                js_object_element(js_string, ':', js_value)
                    >= [&b](auto sv, skip, skip) { return b.add_new_chunk_and_increase(b.add_js_string(sv)); }
            )
        );
    }

    template<idx_t N>
    class json_string
    {
    public:
        constexpr json_string(std::string_view str)
        {
            for (auto i = 0u; i < str.size(); ++i)
                arr[i] = str.at(i);
        }

        constexpr const char* c_str() const
        {
            return arr;
        }

    private:
        char arr[N] = {};
    };

    template<idx_t N>
    class json_value
    {
    public:
        constexpr json_value(const chunk* chunks, const char* string_memory):
            chunks(chunks), string_memory(string_memory)
        {}

        constexpr auto operator [](idx_t idx) const
        {
            idx_t arr_size = std::get<array_tag>(chunks[0]).size;
            if (idx >= arr_size)
                throw std::runtime_error("out of array range");

            idx_t element_idx = 1;
            for (auto i = 0u; i < idx; ++i)
            {
                element_idx += value_size(element_idx);
            }
            const chunk* element_start = chunks + element_idx;
            return json_value<N>(element_start, string_memory);
        }

        template<idx_t KeySize>
        constexpr auto operator [](const char (&key)[KeySize]) const
        {
            idx_t obj_size = std::get<object_tag>(chunks[0]).size;
            idx_t idx = 0;
            for (auto i = 0u; i < obj_size; ++i)
            {
                ++idx;
                const chunk* element_start = chunks + idx;
                idx += value_size(idx);
                std::string_view stored_key = get_string(idx);
                if (stored_key == std::string_view(key))
                    return json_value<N>(element_start, string_memory);
            }
            throw std::runtime_error("no element with the key");
        }

        constexpr auto as_string() const { return json_string<N>(get_string(0)); }
        constexpr bool as_bool() const { return std::get<bool>(chunks[0]); }
        constexpr double as_number() const { return std::get<double>(chunks[0]); }
        constexpr std::nullptr_t as_null() const { return std::get<std::nullptr_t>(chunks[0]); }

    private:
        constexpr idx_t value_size(idx_t element_idx) const
        {
            idx_t result = std::visit(overloaded {
                [](auto) -> idx_t { return 1u; },
                [this, element_idx](array_tag a)
                {
                    idx_t total_size = 1;
                    for (auto i = 0u; i < a.size; ++i)
                        total_size += value_size(element_idx + total_size);
                    return total_size;
                },
                [this, element_idx](object_tag o)
                {
                    idx_t total_size = 1;
                    for (auto i = 0u; i < o.size; ++i)
                    {
                        total_size += value_size(element_idx + total_size);
                        total_size++;
                    }
                    return total_size;
                }
            }, chunks[element_idx]);
            return result;
        }

        constexpr std::string_view get_string(idx_t idx) const
        {
            auto info = std::get<string_info>(chunks[idx]);
            return std::string_view(string_memory + info.start, info.end - info.start);
        }

        const chunk* chunks;
        const char* string_memory;
    };

    template<idx_t N>
    class json
    {
    public:
        constexpr json(const char (&txt) [N])
        {
            parse(txt);
        }

        template<idx_t KeySize>
        constexpr auto operator [](const char (&key)[KeySize]) const
        {
            return json_value<N>(chunks, string_memory)[key];
        }

        template<typename Stream>
        void dump_chunks(Stream& s) const
        {
            for (const auto& v: chunks)
            {
                std::visit(overloaded {
                    [&s](std::nullptr_t) { s << "null" << ' '; },
                    [&s](double d) { s << d << ' '; },
                    [&s](bool b) { s << (b ? "true" : "false") << ' '; },
                    [&s, this](string_info si) { s << "\"" << std::string_view(&(string_memory[si.start]), si.end - si.start) << "\" "; },
                    [&s](array_tag a) { s << "[" << a.size << "] "; },
                    [&s](object_tag o) { s << "{" << o.size << "} "; }
                }, v);
            }
            s << std::endl;
        }

    private:
        constexpr void parse(const char (&txt) [N])
        {
            chunks_builder b(chunks, string_memory);
            auto js_parser = create_parser(b);

            auto res = js_parser.parse(buffers::cstring_buffer(txt));
            if (!res.has_value())
                throw std::runtime_error("parse error");
            size = b.get_size();
        }

        char string_memory[N] = {};
        chunk chunks[N];
        idx_t size = 0;
    };
}

int main(int argc, char* argv[])
{
    constexpr const char txt[] = R"({"a":[1,2,3], "a1":{}, "a2":[], "b":true, "c":false, "d":null, "e":"ala", "f":{"x":1.2}})";
    constexpr ctjs::json js(txt);

    //js.dump_chunks(std::cout);


    constexpr auto a0 = js["a"][0].as_number();
    constexpr auto a1 = js["a"][1].as_number();
    constexpr auto a2 = js["a"][2].as_number();
    static_assert(a0 == 1 && a1 == 2 && a2 == 3);

    constexpr auto b = js["b"].as_bool();
    constexpr auto c = js["c"].as_bool();
    static_assert(b && !c);

    constexpr auto d = js["d"].as_null();

    constexpr auto e = js["e"].as_string();

    constexpr auto f = js["f"]["x"].as_number();
    static_assert(f == 1.2);

    std::cout << e.c_str() << std::endl;
    return 0;
}
