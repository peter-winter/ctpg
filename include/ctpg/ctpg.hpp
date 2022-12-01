#ifndef CTPG_H
#define CTPG_H

#include <utility>
#include <type_traits>
#include <cstdint>
#include <limits>
#include <tuple>
#include <algorithm>
#include <vector>
#include <optional>
#include <variant>
#include <string_view>
#include <ostream>

namespace ctpg
{

using size_t = std::size_t;
using size8_t = std::uint8_t;
using size16_t = std::uint16_t;
using size32_t = std::uint32_t;

template<size_t N>
using str_table = const char* [N];

constexpr size_t uninitialized = size_t(-1);
constexpr size16_t uninitialized16 = size16_t(-1);
constexpr size32_t uninitialized32 = size32_t(-1);

namespace meta
{
    template<typename T>
    struct contains_type
    {};

    template<typename... T>
    struct unique_type_list : contains_type<T>...
    {
        using variant_type = std::variant<T...>;

        template<typename New>
        struct insert
        {
            static unique_type_list<T...> foo(contains_type<New>*);
            static unique_type_list<T..., New> foo(...);

            using type = decltype(foo(std::declval<unique_type_list<T...>*>()));
        };

        template<typename New>
        typename insert<New>::type operator + (New);
    };

    template<typename... T>
    struct unique_types_variant
    {
        using unique_list = decltype((std::declval<unique_type_list<>>() + ... + std::declval<T>()));
        using type = typename unique_list::variant_type;
    };

    template<typename... T>
    using unique_types_variant_t = typename unique_types_variant<T...>::type;

    template<typename... T>
    constexpr int sum_size = (0 + ... + sizeof(T));

    template<typename T>
    constexpr size_t distinct_values_count = 1 << (sizeof(T) * 8);

    constexpr size_t distinct_chars_count = distinct_values_count<char>;

    template<size_t First, size_t... Rest>
    struct max
    {
        static const size_t value = std::max(First, max<Rest...>::value);
    };

    template<size_t X>
    struct max<X>
    {
        static const size_t value = X;
    };

    template<size_t... X>
    constexpr size_t max_v = max<X...>::value;

    template<size_t... X>
    constexpr size_t count_zeros = (0 + ... + (X == 0 ? 1 : 0));
}

namespace stdex
{
    template<typename T>
    struct is_cvector_compatible : std::bool_constant<std::is_default_constructible_v<T> && std::is_trivially_destructible_v<T>>
    {};

    template<typename T, std::size_t N, typename = void>
    class cvector
    {};

    template<typename T, std::size_t N>
    class cvector<T, N, std::enable_if_t<is_cvector_compatible<T>::value>>
    {
    public:
        using size_type = std::size_t;

        constexpr cvector() :
            the_data{}, current_size(0)
        {}

        constexpr cvector(const T& arg, size_t count):
            the_data{}, current_size(0)
        {
            for (size_t i = 0; i < count; ++i)
                push_back(arg);
        }

    private:
        template<typename Derived>
        struct iterator_base
        {
            using it_type = Derived;
            constexpr it_type* cast() { return static_cast<it_type*>(this); }
            constexpr const it_type* cast() const { return static_cast<const it_type*>(this); }

            constexpr bool operator == (const it_type& other) const { return cast()->ptr == other.ptr; }
            constexpr bool operator != (const it_type& other) const { return cast()->ptr != other.ptr; }
            constexpr it_type operator - (size_type amount) const { return it_type{ cast()->ptr - amount }; }
            constexpr size_type operator - (const it_type& other) const { return size_type(cast()->ptr - other.ptr); }
            constexpr it_type operator + (size_type amount) const { return it_type{ cast()->ptr + amount }; }
            constexpr it_type operator ++(int) { it_type it{ cast()->ptr }; ++(cast()->ptr); return it; }
            constexpr it_type& operator ++() { ++(cast()->ptr); return *cast(); }
            constexpr bool operator > (const it_type& other) const { return cast()->ptr > other.ptr; }
            constexpr bool operator < (const it_type& other) const { return cast()->ptr < other.ptr; }
        };

    public:
        struct iterator : iterator_base<iterator>
        {
            T* ptr;
            constexpr iterator(T* ptr) : ptr(ptr) {}
            constexpr T& operator *() const { return *ptr; }
        };

        struct const_iterator : iterator_base<const_iterator>
        {
            const T* ptr;
            constexpr const_iterator(const T* ptr) : ptr(ptr) {}
            constexpr const T& operator *() const { return *ptr; }
        };

        constexpr const T* data() const { return the_data; }
        constexpr T* data() { return the_data; }
        constexpr size_type size() const { return current_size; }
        constexpr bool empty() const { return current_size == 0; }
        constexpr void reserve(size_type) const {};
        constexpr const T& operator[](size_type idx) const { return the_data[idx]; }
        constexpr T& operator[](size_type idx) { return the_data[idx]; }
        constexpr void push_back(const T& v) { the_data[current_size++] = v; }
        constexpr void emplace_back(T&& v) { the_data[current_size++] = std::move(v); }
        constexpr const T& front() const { return the_data[0]; }
        constexpr T& front() { return the_data[0]; }
        constexpr T& back() { return the_data[current_size - 1]; }
        constexpr const T& back() const { return the_data[current_size - 1]; }
        constexpr const_iterator begin() const { return const_iterator(the_data); }
        constexpr const_iterator end() const { return const_iterator(the_data + current_size); }
        constexpr iterator begin() { return iterator(the_data); }
        constexpr iterator end() { return iterator(the_data + current_size); }
        constexpr void clear() { current_size = 0; }
        constexpr void pop_back() { current_size--; }
        constexpr iterator erase(iterator first, iterator last)
        {
            if (!(first < last))
                return end();
            auto from = first < begin() ? begin() : first;
            auto to = last > end() ? end() : last;
            size_type diff = to - from;
            iterator it = to;
            while (!(it == end()))
            {
                *from = std::move(*it);
                ++from; ++it;
            }
            current_size -= diff;

            return end();
        }

    private:
        T the_data[N];
        size_type current_size;
    };

    template<std::size_t N>
    class cbitset
    {
    public:
        using size_type = std::size_t;
        using underlying_type = std::uint64_t;

        constexpr cbitset& set(size_type idx)
        {
            check_idx(idx);
            data[idx / underlying_size] |= (underlying_type(1) << (idx % underlying_size));
            return *this;
        }

        constexpr cbitset& set(size_type idx, bool value)
        {
            check_idx(idx);
            data[idx / underlying_size] ^= (-!!value ^ data[idx / underlying_size]) & (underlying_type(1) << (idx % underlying_size));
            return *this;
        }

        constexpr cbitset& reset(size_type idx)
        {
            check_idx(idx);
            data[idx / underlying_size] &= ~(underlying_type(1) << (idx % underlying_size));
            return *this;
        }

        constexpr cbitset& flip(size_type idx)
        {
            check_idx(idx);
            data[idx / underlying_size] ^= (underlying_type(1) << (idx % underlying_size));
            return *this;
        }

        constexpr bool test(size_type idx) const
        {
            check_idx(idx);
            return (data[idx / underlying_size] >> (idx % underlying_size)) & underlying_type(1);
        }

        constexpr cbitset& flip()
        {
            for (auto& d : data)
                d = ~d;
            return *this;
        }

        constexpr cbitset& set()
        {
            for (auto& d : data)
                d = underlying_type(-1);
            return *this;
        }

        constexpr cbitset& reset()
        {
            for (auto& d : data)
                d = underlying_type(0);
            return *this;
        }

        constexpr size_type size() const
        {
            return N;
        }

    private:
        constexpr void check_idx(size_type idx) const
        {
            if (idx >= N)
                throw std::runtime_error("Index access out of range");
        }

        static const size_type underlying_size = sizeof(underlying_type) * 8;
        static const size_type underlying_count = (N / underlying_size) + ((N % underlying_size) ? 1 : 0);
        underlying_type data[underlying_count] = {};
    };

    template<typename T>
    struct is_cqueue_compatible : std::bool_constant<std::is_default_constructible_v<T> && std::is_trivially_destructible_v<T>>
    {};

    template<typename T, std::size_t N, typename = void>
    class cqueue
    {};

    template<typename T, std::size_t N>
    class cqueue<T, N, std::enable_if_t<is_cqueue_compatible<T>::value>>
    {
    public:
        constexpr cqueue():
            data{}, start{0}, end{0}, size_{0}
        {}

        constexpr void push(const T& v)
        {
            if (size_ >= N)
                throw std::runtime_error("Pushing out of range");

            data[end++] = v;
            if (end == N)
                end = 0;
            size_++;
        }

        constexpr void pop()
        {
            if (empty())
                throw std::runtime_error("Pop on empty");

            start++;
            if (start == N)
                start = 0;
            size_--;
        }

        constexpr T& top()
        {
            if (empty())
                throw std::runtime_error("Top on empty");
            return data[start];
        }

        constexpr const T& top() const
        {
            return std::as_const(*this).top();
        }

        constexpr bool empty() const
        {
            return size_ == 0;
        }

        constexpr std::size_t size() const
        {
            return size_;
        }

    private:
        T data[N];
        std::size_t start;
        std::size_t end;
        std::size_t size_;
    };

    template<typename Container, typename Pred>
    constexpr Container& sort(Container& c, Pred p)
    {
        bool swap = true;
        while (swap)
        {
            swap = false;
            for (auto i = 0u; i < std::size(c) - 1; i++)
            {
                if (p(c[i + 1], c[i]))
                {
                    auto x = c[i];
                    c[i] = c[i + 1];
                    c[i + 1] = x;
                    swap = true;
                }
            }
        }
        return c;
    }
}

namespace utils
{
    template<typename T, typename... Args>
    constexpr T construct_default(Args...)
    {
        return T();
    }

    template<typename T, size_t... I>
    constexpr void copy_array(T *a1, const T* a2, std::index_sequence<I...>)
    {
        (void(a1[I] = a2[I]), ...);
    }

    constexpr size_t char_to_idx(char c)
    {
        return static_cast<size_t>(static_cast<unsigned char>(c)) & 0xff;
    }

    constexpr char idx_to_char(size_t idx)
    {
        return static_cast<char>(static_cast<unsigned char>(idx & 0xff));
    }

    constexpr bool is_printable(char c)
    {
        return c >= 0x20 && c <= 0x7e;
    }

    constexpr bool is_hex_digit(char c)
    {
        return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }

    constexpr bool is_dec_digit(char c)
    {
        return c >= '0' && c <= '9';
    }

    class char_names
    {
    public:
        const static size_t name_size = 5;

        constexpr char_names()
        {
            for (size_t i = 0; i < meta::distinct_chars_count; ++i)
            {
                char d[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
                if (idx_to_char(i) > 32 && idx_to_char(i) < 127)
                {
                    arr[i][0] = idx_to_char(i);
                    arr[i][1] = 0;
                }
                else
                {
                    arr[i][0] = '\\';
                    arr[i][1] = 'x';
                    arr[i][2] = d[i / 16];
                    arr[i][3] = d[i % 16];
                    arr[i][4] = 0;
                }
            }
        }

        constexpr const char* name(char c) const { return arr[char_to_idx(c)]; }

    private:
        char arr[meta::distinct_chars_count][name_size] = {};
    };

    constexpr char_names c_names = {};

    constexpr bool str_equal(const char* str1, const char* str2)
    {
        if ((str1 == nullptr) || (str2 == nullptr))
            throw std::runtime_error("null pointers not allowed here");
        while (*str1 == *str2)
        {
            if (*str1 == 0)
                return true;
            str1++; str2++;
        }
        return false;
    }

    template<size_t N>
    constexpr size_t find_str(const str_table<N>& table, const char* str)
    {
        size_t res = 0;
        for (const auto& n : table)
        {
            if (str_equal(n, str))
                return res;
            res++;
        }
        if (res == N)
            throw std::runtime_error("string not found");
        return uninitialized;
    }

    constexpr size_t find_char(char c, const char* str)
    {
        size_t i = 0;
        while (*str)
        {
            if (*str == c)
                return i;
            str++; i++;
        }
        return uninitialized;
    }

    constexpr std::size_t str_len(const char* str)
    {
        std::size_t i = 0;
        const char* p = str;
        while (*p) { ++i; ++p; }
        return i;
    }

    struct slice
    {
        size32_t start;
        size32_t n;
    };

    template<typename T>
    struct fake_table
    {
        T operator[](size_t) const { return val; }
        T val;
    };
}

namespace ftors
{
    template<size_t>
    struct ignore
    {
        template<typename T>
        constexpr ignore(T&&){}
    };

    template<size_t X, typename = std::make_index_sequence<X - 1>>
    class element
    {};

    template<size_t X, size_t... I>
    class element<X, std::index_sequence<I...>>
    {
    public:
        template<typename First, typename... Rest>
        constexpr decltype(auto) operator ()(ignore<I>..., First&& arg, Rest&&...) const
        {
            return std::forward<First>(arg);
        }
    };

    constexpr element<1> _e1;
    constexpr element<2> _e2;
    constexpr element<3> _e3;
    constexpr element<4> _e4;
    constexpr element<5> _e5;
    constexpr element<6> _e6;
    constexpr element<7> _e7;
    constexpr element<8> _e8;
    constexpr element<9> _e9;

    template<typename T>
    class val
    {
    public:
        constexpr val(T&& v):
            v(std::forward<T>(v))
        {}

        template<typename... Args>
        constexpr auto operator ()(Args&&...) const
        {
            return v;
        }

    private:
        T v;
    };

    template<typename T>
    val(T&&) -> val<std::decay_t<T>>;

    template<typename T>
    class create
    {
    public:
        template<typename... Args>
        constexpr auto operator ()(Args&&...) const
        {
            return T{};
        }
    };

    template<typename T, std::size_t FromIdx = 1, typename = std::make_index_sequence<FromIdx - 1>>
    struct construct
    {};

    template<
        typename T,
        std::size_t FromIdx,
        std::size_t... Skip
    >
    struct construct<
        T,
        FromIdx,
        std::index_sequence<Skip...>>
    {
        template<typename Arg, typename... Rest>
        constexpr auto operator()(ignore<Skip>..., Arg&& arg, Rest&&...) const
        {
            return T{std::forward<Arg>(arg)};
        }
    };

    template<
        std::size_t ContIdx = 1,
        std::size_t ArgIdx = 2,
        typename = std::make_index_sequence<std::min(ContIdx, ArgIdx) - 1>,
        typename = std::make_index_sequence<std::max(ContIdx, ArgIdx) - std::min(ContIdx, ArgIdx) - 1>,
        bool container_first = ContIdx < ArgIdx
    >
    struct emplace_back
    {};

    template<
        std::size_t ContIdx,
        std::size_t ArgIdx,
        std::size_t... Skip1,
        std::size_t... Skip2
    >
    struct emplace_back<
        ContIdx,
        ArgIdx,
        std::index_sequence<Skip1...>,
        std::index_sequence<Skip2...>,
        true>
    {
        template<typename Container, typename Arg, typename... Rest>
        constexpr decltype(auto) operator()(ignore<Skip1>..., Container &&container, ignore<Skip2>..., Arg&& arg, Rest&&...) const
        {
            container.emplace_back(std::move(arg));
            return std::move(container);
        }
    };

    template<
        std::size_t ContIdx,
        std::size_t ArgIdx,
        std::size_t... Skip1,
        std::size_t... Skip2
    >
    struct emplace_back<
        ContIdx,
        ArgIdx,
        std::index_sequence<Skip1...>,
        std::index_sequence<Skip2...>,
        false>
    {
        template<typename Container, typename Arg, typename... Rest>
        constexpr decltype(auto) operator()(ignore<Skip1>..., Arg&& arg, ignore<Skip2>..., Container &&container, Rest&&...) const
        {
            container.emplace_back(std::move(arg));
            return std::move(container);
        }
    };

    template<
        std::size_t ContIdx = 1,
        std::size_t ArgIdx = 2,
        typename = std::make_index_sequence<std::min(ContIdx, ArgIdx) - 1>,
        typename = std::make_index_sequence<std::max(ContIdx, ArgIdx) - std::min(ContIdx, ArgIdx) - 1>,
        bool container_first = ContIdx < ArgIdx
    >
    struct push_back
    {};

    template<
        std::size_t ContIdx,
        std::size_t ArgIdx,
        std::size_t... Skip1,
        std::size_t... Skip2
    >
    struct push_back<
        ContIdx,
        ArgIdx,
        std::index_sequence<Skip1...>,
        std::index_sequence<Skip2...>,
        true>
    {
        template<typename Container, typename Arg, typename... Rest>
        constexpr decltype(auto) operator()(ignore<Skip1>..., Container &&container, ignore<Skip2>..., const Arg& arg, Rest&&...) const
        {
            container.push_back(arg);
            return std::move(container);
        }
    };

    template<
        std::size_t ContIdx,
        std::size_t ArgIdx,
        std::size_t... Skip1,
        std::size_t... Skip2
    >
    struct push_back<
        ContIdx,
        ArgIdx,
        std::index_sequence<Skip1...>,
        std::index_sequence<Skip2...>,
        false>
    {
        template<typename Container, typename Arg, typename... Rest>
        constexpr decltype(auto) operator()(ignore<Skip1>..., const Arg& arg, ignore<Skip2>..., Container &&container, Rest&&...) const
        {
            container.push_back(arg);
            return std::move(container);
        }
    };
}

namespace buffers
{
    template<size_t N>
    class cstring_buffer
    {
    public:
        template<size_t N1>
        constexpr cstring_buffer(const char(&source)[N1])
        {
            utils::copy_array(data, source, std::make_index_sequence<N1>{});
        }

        struct iterator
        {
            const char* ptr;

            constexpr char operator *() const { return *ptr; }
            constexpr iterator& operator ++() { ++ptr; return *this; }
            constexpr iterator operator ++(int) { iterator i(*this); ++ptr; return i; }
            constexpr bool operator == (const iterator& other) const { return ptr == other.ptr; }
            constexpr bool operator != (const iterator& other) const { return ptr != other.ptr; }
            constexpr iterator& operator += (size_t len) { ptr += len; return *this; }
            constexpr iterator operator + (size_t len) { iterator i(*this); i.ptr += len; return i; }
        };

        constexpr iterator begin() const { return iterator{ data }; }
        constexpr iterator end() const { return iterator{ data + N - 1 }; }
        constexpr std::string_view get_view(iterator start, iterator end) const { return std::string_view(start.ptr, end.ptr - start.ptr); }

    private:
        char data[N] = { 0 };
    };

    template<size_t N>
    cstring_buffer(const char(&)[N])->cstring_buffer<N>;

    class string_buffer
    {
    public:
        string_buffer(std::string&& str):
            str(std::move(str))
        {}

        string_buffer(const char* str):
            str(str)
        {}

        auto begin() const { return str.cbegin(); }
        auto end() const { return str.cend(); }

        using iterator = std::string::const_iterator;

        std::string_view get_view(iterator start, iterator end) const
        {
            return std::string_view(str.data() + (start - str.begin()), end - start);
        }

    private:
        std::string str;
    };

    template<typename Buffer>
    using iterator_t = typename Buffer::iterator;
}

template<typename ValueType>
class nterm
{
public:
    using value_type = ValueType;

    constexpr nterm(const char* name) :
        name(name)
    {
        if (name[0] == 0)
            throw std::runtime_error("empty name not allowed");
    }

    constexpr const char* get_name() const { return name; }

    template<typename... Args>
    constexpr auto operator()(Args&&... args) const;

private:
    const char* name;
};

enum class associativity { no_assoc, ltor, rtol };

struct source_point
{
    size32_t line = 1;
    size32_t column = 1;

    template<typename Iterator>
    constexpr void update(Iterator start, Iterator end)
    {
        while (!(start == end))
        {
            if (*start == '\n')
            {
                ++line;
                column = 1;
            }
            else
                ++column;
            ++start;
        }
    }

    friend std::ostream& operator << (std::ostream& o, const source_point& sp);
};

inline std::ostream& operator << (std::ostream& o, const source_point& sp)
{
    o << "[" << sp.line << ":" << sp.column << "]";
    return o;
}

template<typename VT>
class term_value
{
public:
    constexpr term_value(VT v, source_point sp):
        value(v), sp{ sp }
    {}

    constexpr operator VT() const { return value; }
    constexpr size32_t get_line() const { return sp.line; }
    constexpr size32_t get_column() const { return sp.column; }
    constexpr const VT& get_value() const { return value; }
    constexpr source_point get_sp() const { return sp; }

private:
    VT value;
    source_point sp;
};

class term
{
public:
    constexpr term(int precedence = 0, associativity a = associativity::no_assoc) :
        precedence(precedence), ass(a)
    {}

    constexpr associativity get_associativity() const { return ass; }
    constexpr int get_precedence() const { return precedence; }

protected:
    int precedence;
    associativity ass;
};

namespace detail
{
    constexpr std::string_view pass_sv(const std::string_view& sv) { return sv; }
    constexpr char first_sv_char(const std::string_view& sv) { return sv[0]; }
}

class char_term : public term
{
public:
    using internal_value_type = char;

    static const size_t dfa_size = 2;
    static const bool is_trivial = true;

    constexpr char_term(char c, int precedence = 0, associativity a = associativity::no_assoc):
        term(precedence, a), c(c)
    {
        utils::copy_array(id, utils::c_names.name(c), std::make_index_sequence<utils::char_names::name_size>{});
    }

    constexpr const char* get_id() const { return id; }
    constexpr const char* get_name() const { return get_id(); }
    constexpr char get_char() const { return c; }
    constexpr char get_data() const { return c; }

    constexpr const auto& get_ftor() const { return detail::first_sv_char; }

private:
    char c;
    char id[utils::char_names::name_size] = {};
};

template<size_t DataSize>
class string_term : public term
{
public:
    using internal_value_type = std::string_view;
    static const size_t dfa_size = (DataSize - 1) * 2;
    static const bool is_trivial = true;

    constexpr string_term(const char (&str)[DataSize], int precedence = 0, associativity a = associativity::no_assoc):
        term(precedence, a)
    {
        utils::copy_array(data, str, std::make_index_sequence<DataSize>{});
    }

    constexpr const char* get_id() const { return data; }
    constexpr const char* get_name() const { return get_id(); }
    constexpr const auto& get_data() const { return data; }

    constexpr const auto& get_ftor() const { return detail::pass_sv; }

private:
    char data[DataSize] = {};
};

template<typename Term, typename Ftor>
class typed_term
{
public:
    using internal_value_type = std::invoke_result_t<Ftor, std::string_view>;
    static const size_t dfa_size = Term::dfa_size;
    static const bool is_trivial = Term::is_trivial;

    constexpr typed_term(Term t, Ftor f):
        term(t), ftor(f)
    {}

    constexpr const char* get_id() const { return term.get_id(); }
    constexpr const char* get_name() const { return term.get_name(); }
    constexpr decltype(auto) get_data() const { return term.get_data(); }

    constexpr associativity get_associativity() const { return term.get_associativity(); }
    constexpr int get_precedence() const { return term.get_precedence(); }

    using ftor_type = Ftor;

    constexpr const ftor_type& get_ftor() const { return ftor; }

private:
    Term term;
    ftor_type ftor;
};

template<typename Ftor>
class custom_term : public term
{
public:
    using internal_value_type = std::invoke_result_t<Ftor, std::string_view>;
    static const size_t dfa_size = 0;
    static const bool is_trivial = false;

    constexpr custom_term(const char* custom_name, Ftor ftor, int precedence = 0, associativity a = associativity::no_assoc):
        term(precedence, a), custom_name(custom_name), ftor(ftor)
    {}

    constexpr const char* get_id() const { return get_name(); }
    constexpr const char* get_name() const { return custom_name; }

    using ftor_type = Ftor;

    constexpr const ftor_type& get_ftor() const { return ftor; }

private:
    const char* custom_name;
    Ftor ftor;
};

struct error_recovery_token
{
    constexpr static const char* get_name() { return "<error_recovery_token>"; }
    constexpr static const char* get_id() { return get_name(); }
};

constexpr error_recovery_token error;

template<typename T, typename Enable = void>
struct value_type
{};

template<typename T>
struct value_type<T, std::enable_if_t<std::is_base_of_v<term, T>>>
{
    using type = term_value<typename T::internal_value_type>;
};

template<typename ValueType>
struct value_type<nterm<ValueType>>
{
    using type = ValueType;
};

template<typename Term, typename Ftor>
struct value_type<typed_term<Term, Ftor>>
{
    using type = term_value<typename typed_term<Term, Ftor>::internal_value_type>;
};

struct no_type {};

template<>
struct value_type<error_recovery_token>
{
    using type = no_type;
};

template<typename T>
using value_type_t = typename value_type<T>::type;

struct parse_options
{
    constexpr parse_options& set_verbose(bool val = true) { verbose = val; return *this; }
    constexpr parse_options& set_skip_whitespace(bool val = true) { skip_whitespace = val; return *this; }
    constexpr parse_options& set_skip_newline(bool val = true) { skip_newline = val; return *this; }

    bool verbose = false;
    bool skip_whitespace = true;
    bool skip_newline = true;
};

struct match_options
{
    bool verbose = false;
    constexpr match_options& set_verbose(bool val = true) { verbose = val; return *this; }
};

namespace detail
{
    struct no_stream
    {
        template<typename T>
        constexpr const no_stream& operator <<(T&&) const { return *this; }
    };
}

struct recognized_term
{
    constexpr recognized_term() = default;

    constexpr recognized_term(size16_t term_idx, size_t len):
        term_idx(term_idx), len(len)
    {}

    size16_t term_idx = uninitialized16;
    size_t len = uninitialized16;
};

namespace regex
{
    using conflicted_terms = size16_t[4];

    constexpr void add_conflicted_term(conflicted_terms& ts, size16_t t)
    {
        for (size_t i = 0; i < 4; ++i)
            if (ts[i] == uninitialized16)
            {
                ts[i] = t;
                break;
            }
    }

    struct dfa_state
    {
        constexpr dfa_state()
        {
            for (auto& t : transitions)
                t = uninitialized16;
        }

        size8_t start_state = 0;
        size8_t end_state = 0;
        size8_t unreachable = 0;
        conflicted_terms conflicted_recognition = { uninitialized16, uninitialized16, uninitialized16, uninitialized16 };
        static const size_t transitions_size = meta::distinct_values_count<char>;
        size16_t transitions[transitions_size] = {};
    };

    struct char_range
    {
        constexpr char_range(char c):
            start(c), end(c)
        {}

        constexpr char_range(char c1, char c2):
            start(c1), end(c2)
        {}
        char start;
        char end;
    };

    constexpr char hex_digits_to_char(char d1, char d2)
    {
        auto dd = [](char d) -> char
        {
            if (d >= 'A' && d <= 'F')
                return char(10) + d - 'A';
            else if (d >= 'a' && d <= 'f')
                return char(10) + d - 'a';
            else
                return d - '0';
        };
        return dd(d1) * 16 + dd(d2);
    }

    class char_subset
    {
    public:
        constexpr char_subset() = default;

        template<size_t N>
        constexpr char_subset(const char (&str)[N])
        {
            for (size_t i = 0; i < N - 1; ++i)  // ignore trailing 0, so N - 1
                data.set(utils::char_to_idx(str[i]));
        }

        constexpr char_subset(char_range r)
        {
            add_range(r);
        }

        constexpr char_subset& add_range(char_range r)
        {
            for (size_t i = utils::char_to_idx(r.start); i <= utils::char_to_idx(r.end); ++i)
                data.set(i);
            return *this;
        }

        constexpr bool test(size_t idx) const { return data.test(idx); }
        constexpr size_t size() const { return data.size(); }
        constexpr char_subset& set() { data.set(); return *this; }
        constexpr char_subset& flip() { data.flip(); return *this; }
        constexpr char_subset& set(size_t idx) { data.set(idx); return *this; }

    private:
        stdex::cbitset<meta::distinct_values_count<char>> data = {};
    };

    template<size_t N>
    using dfa = stdex::cvector<dfa_state, N>;

    class dfa_size_analyzer
    {
    public:
        using slice = utils::slice;

        constexpr slice primary_char(char) { return prim(); }
        constexpr slice primary_subset(char_subset&&) { return prim(); }

        constexpr slice star(slice s) { return s; }
        constexpr slice plus(slice s) { return s; }
        constexpr slice opt(slice s) { return s; }
        constexpr slice cat(slice s1, slice s2) { return add(s1, s2); }
        constexpr slice alt(slice s1, slice s2) { return add(s1, s2); }

        constexpr slice rep(slice s, size32_t n)
        {
            if (n == 0)
                return s;
            size += (s.n * (n - 1));
            return slice{ s.start, s.n * n };
        }

    private:
        constexpr slice prim() { auto old = size; size += 2; return slice{ old, 2 }; }
        constexpr slice add(slice s1, slice s2) { return slice{ s1.start, s1.n + s2.n }; }

        size32_t size = 0;
    };

    template<size_t N>
    class dfa_builder
    {
    public:
        constexpr dfa_builder(dfa<N>& sm):
            sm(sm)
        {}

        using slice = utils::slice;

        constexpr dfa_state& transition(dfa_state& from, const char_subset& s)
        {
            for (size_t i = 0; i < s.size(); ++i)
                if (s.test(i))
                {
                    from.transitions[i] = size16_t(sm.size());
                }
            sm.push_back(dfa_state());
            return sm.back();
        }

        constexpr dfa_state& transition(dfa_state& from, char c)
        {
            from.transitions[utils::char_to_idx(c)] = size16_t(sm.size());
            sm.push_back(dfa_state());
            return sm.back();
        }

        constexpr slice primary_char(char c)
        {
            return primary_subset(char_subset(char_range(c)));
        }

        constexpr slice primary_subset(const char_subset& s)
        {
            size_t old_size = sm.size();
            sm.push_back(dfa_state());
            sm.back().start_state = 1;
            dfa_state& to = transition(sm.back(), s);
            to.end_state = 1;
            return slice{ size32_t(old_size), 2 };
        }

        constexpr slice star(slice s)
        {
            size_t b = s.start;
            sm[b].end_state = 1;
            for (size_t i = s.start; i < s.start + s.n; ++i)
            {
                if (sm[i].end_state)
                    merge(i, b);
            }
            return s;
        }

        constexpr slice plus(slice s)
        {
            size_t b = s.start;
            for (size_t i = s.start; i < s.start + s.n; ++i)
            {
                if (sm[i].end_state)
                    merge(i, b, true);
            }
            return s;
        }

        constexpr slice opt(slice s)
        {
            sm[s.start].end_state = 1;
            return s;
        }

        constexpr slice rep(slice s, size32_t n)
        {
            if (n == 0)
            {
                for (size_t j = s.start; j < s.start + s.n; ++j)
                {
                    if (sm[j].start_state)
                    {
                        for (auto& t : sm[j].transitions)
                            t = uninitialized16;
                        sm[j].end_state = 1;
                        sm[j].start_state = 0;
                    }
                    else
                        sm[j].unreachable = 1;
                }
                return s;
            }

            for (size_t i = 0; i < n - 1; ++i)
            {
                for (size_t j = s.start; j < s.start + s.n; ++j)
                {
                    sm.push_back(sm[j]);
                    auto& st = sm.back();
                    for (auto& t : st.transitions)
                    {
                        if (t != uninitialized16)
                            t += size16_t(s.n * (i + 1));
                    }
                }
            }
            slice whole = s;
            for (size_t i = 0; i < n - 1; ++i)
            {
                cat(whole, slice{ whole.start + whole.n, s.n });
                whole = slice{ whole.start, whole.n + s.n};
            }
            return whole;
        }

        constexpr slice cat(slice s1, slice s2)
        {
            size_t b = s2.start;
            for (size_t i = s1.start; i < s1.start + s1.n; ++i)
            {
                if (sm[i].end_state)
                    merge(i, b, false, true);
            }
            return slice{ s1.start, s1.n + s2.n };
        }

        constexpr slice alt(slice s1, slice s2)
        {
            size_t b1 = s1.start;
            size_t b2 = s2.start;
            merge(b1, b2, true, true);
            return slice{ s1.start, s1.n + s2.n };
        }

        constexpr void mark_end_states(slice s, size16_t idx)
        {
            for (size_t i = s.start; i < s.start + s.n; ++i)
            {
                mark_end_state(sm[i], idx);
            }
        }

        constexpr size_t size() const { return sm.size(); }
    private:
        constexpr void merge(size_t to, size_t from, bool keep_end_state = false, bool mark_from_as_unreachable = false)
        {
            if (to == from)
                return;
            dfa_state& s_from = sm[from];
            dfa_state& s_to = sm[to];
            s_from.start_state = 0;
            if (keep_end_state)
                s_to.end_state = s_to.end_state || s_from.end_state;
            else
                s_to.end_state = s_from.end_state;

            s_from.unreachable = mark_from_as_unreachable ? 1 : 0;

            for (size_t i = 0; i < dfa_state::transitions_size; ++i)
            {
                size16_t& tr_from = s_from.transitions[i];
                if (tr_from == uninitialized16)
                    continue;
                size16_t& tr_to = s_to.transitions[i];
                if (tr_to == uninitialized16)
                {
                    tr_to = tr_from;
                    sm[tr_to].unreachable = 0;
                }
                else
                    merge(tr_to, tr_from, keep_end_state, mark_from_as_unreachable);
            }

            auto& cr = s_from.conflicted_recognition;
            for (size_t j = 0; j < 4; ++j)
            {
                size16_t term_idx = cr[j];
                if (term_idx != uninitialized16)
                    mark_end_state(s_to, term_idx);
                else
                    break;
            }
        }

        constexpr void mark_end_state(dfa_state& s, size16_t idx)
        {
            if (!s.end_state)
                return;
            add_conflicted_term(s.conflicted_recognition, idx);
        }

        dfa<N>& sm;
    };

    template<size_t N>
    struct regex_pattern_data
    {
        const char (&pattern)[N];
    };

    template<size_t N, typename Parser>
    constexpr void add_term_data_to_dfa(char c, dfa_builder<N>& b, const Parser&, size16_t idx)
    {
        using slice = utils::slice;

        slice prev{0, size32_t(b.size())};
        slice new_sl = b.primary_char(c);
        b.mark_end_states(new_sl, idx);
        b.alt(prev, new_sl);
    }

    template<size_t N, typename Parser, size_t DataSize>
    constexpr void add_term_data_to_dfa(const char (&str)[DataSize], dfa_builder<N>& b, const Parser&, size16_t idx)
    {
        using slice = utils::slice;
        slice prev{0, size32_t(b.size())};

        slice whole = b.primary_char(str[0]);
        for (size_t i = 1; i < DataSize - 1; ++i)
        {
            slice char_sl = b.primary_char(str[i]);
            whole = b.cat(whole, char_sl);
        }

        b.mark_end_states(whole, idx);
        b.alt(prev, whole);
    }

    template<size_t N, typename Parser, size_t PatternSize>
    constexpr void add_term_data_to_dfa(
        const regex_pattern_data<PatternSize>& pattern_data,
        dfa_builder<N>& b,
        const Parser& p,
        size16_t idx)
    {
        using slice = utils::slice;
        detail::no_stream s{};
        std::optional<slice> res = p.parse(
            parse_options{}.set_skip_whitespace(false),
            buffers::cstring_buffer(pattern_data.pattern),
            s
        );

        if (res.has_value())
        {
            slice prev{0, size32_t(b.size())};
            b.mark_end_states(res.value(), idx);
            b.alt(prev, res.value());
        }
        else
            throw std::runtime_error("Regex parse error");
    }

    template<size_t N, typename Iterator, typename ErrorStream>
    constexpr auto dfa_match(
        const dfa<N>& sm,
        match_options options,
        source_point sp,
        Iterator start,
        Iterator end,
        ErrorStream& error_stream)
    {
        size16_t state_idx = 0;
        recognized_term rt;
        size_t len = 0;
        while (true)
        {
            const dfa_state& state = sm[state_idx];
            size16_t rec_idx = state.conflicted_recognition[0];
            if (rec_idx != uninitialized16)
            {
                rt.len = len;
                rt.term_idx = rec_idx;

                if (options.verbose)
                {
                    error_stream << sp << " REGEX MATCH: Recognized " << rec_idx << "\n";
                }
            }
            
            if (start == end)
                break;

            size16_t tr = state.transitions[utils::char_to_idx(*start)];
            if (tr == uninitialized16)
            {
                break;
            }

            state_idx = tr;

            if (options.verbose)
            {
                error_stream << sp << " REGEX MATCH: Current char " << utils::c_names.name(*start) << "\n";
                error_stream << sp << " REGEX MATCH: New state " << state_idx << "\n";
            }
            sp.update(start, start + 1);
            ++start;
            ++len;
        }
        return rt;
    }

    template<typename Stream, typename StrTable>
    constexpr void write_dfa_state_diag_str(const dfa_state& st, Stream& s, size16_t idx, const StrTable& term_names)
    {
        s << "STATE " << idx;
        if (st.unreachable)
        {
            s << " (unreachable) \n";
            return;
        }

        if (st.end_state)
            s << " recognized ";
        size16_t term_idx = st.conflicted_recognition[0];
        if (term_idx != uninitialized16)
            s << term_names[term_idx];
        s << "   ";

        auto f_range = [&s](const auto& r, size16_t state_idx)
        {
            if (r.size() > 2)
            {
                s << "[";
                s << utils::c_names.name(r.front());
                s << " - ";
                s << utils::c_names.name(r.back());
                s << "] -> " << state_idx << "  ";
            }
            else
            {
                for (char c : r)
                {
                    s << utils::c_names.name(c);
                    s << " -> " << state_idx << "  ";
                }
            }
        };

        stdex::cvector<char, dfa_state::transitions_size> tmp;
        size16_t prev = uninitialized16;
        for (size_t i = 0; i < dfa_state::transitions_size + 1; ++i)
        {
            size16_t to = (i == dfa_state::transitions_size ? uninitialized16 : st.transitions[i]);
            if (to == prev && to != uninitialized16)
                tmp.push_back(utils::idx_to_char(i));
            else
            {
                if (prev != uninitialized16)
                {
                    f_range(tmp, prev);
                    tmp.clear();
                }
                if (to != uninitialized16)
                    tmp.push_back(utils::idx_to_char(i));
            }
            prev = to;
        }
        s << "\n";
    }

    template<size_t N, typename Stream, typename StrTable>
    constexpr void write_dfa_diag_str(const dfa<N>& sm, Stream& stream, const StrTable& term_names)
    {
        for (size16_t i = 0; i < sm.size(); ++i)
            write_dfa_state_diag_str(sm[i], stream, i, term_names);
    }

    template<size_t N, typename Stream>
    constexpr void write_dfa_diag_str(const dfa<N>& sm, Stream& stream)
    {
        write_dfa_diag_str(sm, stream, utils::fake_table<const char*>{""});
    }

    template<typename Builder>
    constexpr auto create_regex_parser(Builder& b);
}

namespace detail
{
    template<typename Arg>
    constexpr decltype(auto) make_term(Arg&& arg)
    {
        return std::forward<Arg>(arg);
    }

    constexpr auto make_term(char c)
    {
        return char_term(c);
    }

    template<size_t N>
    constexpr auto make_term(const char (&str)[N])
    {
        return string_term<N>(str);
    }

    template<typename ValueType>
    struct fake_root
    {
        using value_type = ValueType;

        constexpr auto operator()(const nterm<ValueType>& nt) const;

        constexpr static const char* get_name() { return "##"; };
    };

    struct eof
    {
        constexpr static const char* get_name() { return "<eof>"; }
    };

    template<bool RequiresContext, typename F, typename L, typename...R>
    class rule
    {
    public:
        using f_type = F;
        static const size_t n = sizeof...(R);

        constexpr rule(L l, std::tuple<R...> r) :
            f(nullptr), l(l), r(r), precedence(0)
        {}

        template<typename F1>
        constexpr rule(F1&& f, L l, std::tuple<R...> r) :
            f(std::move(f)), l(l), r(r), precedence(0)
        {}

        template<typename F1>
        constexpr rule(F1&& f, L l, std::tuple<R...> r, int precedence) :
            f(std::move(f)), l(l), r(r), precedence(precedence)
        {}

        constexpr auto operator[](int prec)
        {
            return rule<RequiresContext, F, L, R...>(std::move(f), l, r, prec);
        }

        template<typename F1>
        constexpr auto operator >= (F1&& f)
        {
            return rule<false, std::decay_t<F1>, L, R...>(std::move(f), l, r, precedence);
        }

        template<typename F1>
        constexpr auto operator >>= (F1&& f)
        {
            return rule<true, std::decay_t<F1>, L, R...>(std::move(f), l, r, precedence);
        }

        constexpr const F& get_f() const { return f; }
        constexpr const L& get_l() const { return l; }
        constexpr const auto& get_r() const { return r; }
        constexpr int get_precedence() const { return precedence; }

    private:
        F f;
        L l;
        std::tuple<R...> r;
        int precedence;
    };

    template<typename L, typename... R>
    rule(L l, std::tuple<R...> r) -> rule<false, std::nullptr_t, L, R...>;

    template<typename Arg>
    constexpr auto make_rule_item(Arg&& arg)
    {
        return make_term(arg);
    }

    template<typename ValueType>
    constexpr auto make_rule_item(const nterm<ValueType>& nt)
    {
        return nt;
    }

    template<typename ValueType>
    constexpr auto fake_root<ValueType>::operator()(const nterm<ValueType>& nt) const
    {
        return rule(*this, std::make_tuple(nt));
    }
}

template<typename ValueType>
template<typename... Args>
constexpr auto nterm<ValueType>::operator()(Args&&... args) const
{
    return detail::rule(
        *this,
        std::make_tuple(detail::make_rule_item(args)...)
    );
}

namespace detail
{
    constexpr size_t value_stack_initial_capacity = 1 << 10;
    constexpr size_t cursor_stack_initial_capacity = 1 << 10;

    template<typename Context, typename ValueVariantType, typename RuleTupleType, size_t RuleCount>
    struct value_reductors
    {
        constexpr value_reductors(const RuleTupleType& rule_tuple): rule_tuple(rule_tuple) {
            init_reductors(rule_tuple, std::make_index_sequence<std::tuple_size_v<RuleTupleType>>{});
        }

        constexpr ValueVariantType invoke(Context&& context, size_t i, ValueVariantType* args) const {
            return reductors[i](std::forward<Context>(context), rule_tuple, args);
        }

    private:
        template<size_t... I>
        constexpr void init_reductors(const RuleTupleType& rule_tuple, std::index_sequence<I...>)
        {
            (void(init_nth_reductor<I>(std::get<I>(rule_tuple))), ...);
        }

        template<size_t Nr, bool RequiresContext, typename F, typename L, typename... R>
        constexpr void init_nth_reductor(const detail::rule<RequiresContext, F, L, R...>&)
        {
            reductors[Nr] = &reduce_value<Nr, RequiresContext, F, value_type_t<L>, value_type_t<R>...>;
        }

        template<bool RequiresContext, typename F, typename LValueType, typename... RValueType, size_t... I>
        constexpr static LValueType reduce_value_impl([[maybe_unused]] Context&& ctx, [[maybe_unused]] const F& f, ValueVariantType* start, std::index_sequence<I...>)
        {
            if constexpr (std::is_same_v<F, std::nullptr_t>)
                return LValueType(std::get<RValueType>(std::move(*(start + I)))...);
            else
            {
                if constexpr (RequiresContext)
                    return LValueType(f(std::forward<Context>(ctx), std::get<RValueType>(std::move(*(start + I)))...));
                else
                    return LValueType(f(std::get<RValueType>(std::move(*(start + I)))...));
            }
        }

        template<size_t RuleIdx, bool RequiresContext, typename F, typename LValueType, typename... RValueType>
        constexpr static ValueVariantType reduce_value(Context&& ctx, const RuleTupleType& rules, ValueVariantType* start)
        {
            return ValueVariantType(
                reduce_value_impl<RequiresContext, F, LValueType, RValueType...>(
                    std::forward<Context>(ctx), std::get<RuleIdx>(rules).get_f(), start, std::index_sequence_for<RValueType...>{})
            );
        }

        const RuleTupleType& rule_tuple;

        using value_reductor = ValueVariantType(*)(Context&&, const RuleTupleType&, ValueVariantType*);
        value_reductor reductors[RuleCount] = {};
    };

    template<typename CursorStack, typename ValueStack, typename ErrorStream, typename Iterator, typename ValueReductors>
    struct parse_state
    {
        constexpr parse_state(
            CursorStack& cursor_stack,
            ValueStack& value_stack,
            ErrorStream& error_stream,
            parse_options options,
            Iterator buffer_begin,
            Iterator buffer_end,
            const ValueReductors& reductors):
            cursor_stack(cursor_stack),
            value_stack(value_stack),
            error_stream(error_stream),
            options(options),
            current_sp{1, 1},
            current_it(buffer_begin),
            current_end_it(buffer_begin),
            buffer_end(buffer_end),
            reductors(reductors),
            current_term_idx(uninitialized16),
            recovery_mode(false),
            consume_mode(false)
        {
            cursor_stack.reserve(cursor_stack_initial_capacity);
            value_stack.reserve(value_stack_initial_capacity);
        }

        constexpr void enter_recovery_mode() { recovery_mode = true; }
        constexpr void leave_recovery_mode() { recovery_mode = false; }
        constexpr void enter_consume_mode() { consume_mode = true; }
        constexpr void leave_consume_mode() { consume_mode = false; }
        constexpr bool in_recovery_mode() const { return recovery_mode; }
        constexpr bool in_consume_mode() const { return consume_mode; }

        using iterator = Iterator;

        CursorStack& cursor_stack;
        ValueStack& value_stack;
        ErrorStream& error_stream;
        parse_options options;
        source_point current_sp;
        iterator current_it;
        iterator current_end_it;
        iterator buffer_end;
        const ValueReductors& reductors;
        size16_t current_term_idx;
        bool recovery_mode;
        bool consume_mode;
    };

    template<typename Buffer, size_t EmptyRulesCount>
    struct parse_table_cursor_stack_type
    {
        using type = std::vector<size16_t>;
    };

    template<size_t N, size_t EmptyRulesCount>
    struct parse_table_cursor_stack_type<buffers::cstring_buffer<N>, EmptyRulesCount>
    {
        using type = stdex::cvector<size16_t, N + EmptyRulesCount + 1>;
    };

    template<typename Buffer, size_t EmptyRulesCount>
    using parse_table_cursor_stack_type_t = typename parse_table_cursor_stack_type<Buffer, EmptyRulesCount>::type;

    template<typename Buffer, size_t EmptyRulesCount, typename ValueVariantType, typename = void>
    struct parser_value_stack_type
    {
        using type = std::vector<ValueVariantType>;
    };

    template<size_t N, size_t EmptyRulesCount, typename ValueVariantType>
    struct parser_value_stack_type<
        buffers::cstring_buffer<N>,
        EmptyRulesCount,
        ValueVariantType,
        std::enable_if_t<!stdex::is_cvector_compatible<ValueVariantType>::value>
    >
    {
        using type = std::vector<ValueVariantType>;
    };

    template<size_t N, size_t EmptyRulesCount, typename ValueVariantType>
    struct parser_value_stack_type<
        buffers::cstring_buffer<N>,
        EmptyRulesCount,
        ValueVariantType,
        std::enable_if_t<stdex::is_cvector_compatible<ValueVariantType>::value>
    >
    {
        using type = stdex::cvector<ValueVariantType, N + EmptyRulesCount + 1>;
    };

    template<typename Buffer, size_t EmptyRulesCount, typename ValueVariantType>
    using parser_value_stack_type_t = typename parser_value_stack_type<Buffer, EmptyRulesCount, ValueVariantType>::type;
}


namespace detail
{
    struct deduce_max_states
    {
        template<size_t TermCount, size_t... RuleSizes>
        static const size_t value = (0 + ... + (RuleSizes + 1)) * TermCount;
    };
}

template<typename Lexer>
struct use_lexer
{
    using type = Lexer;
};

struct use_generated_lexer
{
    using type = no_type;
};

template<typename Root, typename Terms, typename NTerms, typename Rules, typename LexerUsage>
class parser
{};

template<typename RootValueType, typename... Terms, typename... NTerms, typename... Rules, typename LexerUsage>
class parser<
    nterm<RootValueType>,
    std::tuple<Terms...>,
    std::tuple<NTerms...>,
    std::tuple<Rules...>,
    LexerUsage
>
{
    using term_tuple_type = std::tuple<Terms...>;
    using nterm_tuple_type = std::tuple<NTerms...>;
    using rule_tuple_type = std::tuple<Rules...>;
    using root_nterm_type = nterm<RootValueType>;
    using root_value_type = RootValueType;

    static const bool generate_lexer = std::is_same_v<LexerUsage, use_generated_lexer>;
    using lexer_type = typename LexerUsage::type;

public:
    constexpr parser(
        root_nterm_type grammar_root,
        term_tuple_type terms,
        nterm_tuple_type nterms,
        rule_tuple_type&& rules,
        LexerUsage):
        parser(grammar_root, terms, nterms, std::move(rules))
    {}

    constexpr parser(
        root_nterm_type grammar_root,
        term_tuple_type terms,
        nterm_tuple_type nterms,
        rule_tuple_type&& rules):
        term_tuple(terms),
        nterm_tuple(nterms),
        rule_tuple(std::move(rules))
    {
        auto seq_for_terms = std::make_index_sequence<std::tuple_size_v<term_tuple_type>>{};
        analyze_nterms(std::make_index_sequence<std::tuple_size_v<nterm_tuple_type>>{});
        analyze_nterm(detail::fake_root<value_type_t<root_nterm_type>>{});
        analyze_terms(seq_for_terms);
        analyze_eof();
        analyze_error_recovery_token();
        analyze_rules(std::make_index_sequence<std::tuple_size_v<rule_tuple_type>>{}, grammar_root);
        analyze_states();

        create_lexer(seq_for_terms);
    }

    template<typename Buffer>
    constexpr std::optional<root_value_type> parse(const Buffer& buffer) const
    {
        detail::no_stream error_stream;
        return parse(buffer, error_stream);
    }

    template<typename Context, typename Buffer>
    constexpr std::optional<root_value_type> context_parse(Context&& ctx, const Buffer& buffer) const
    {
        detail::no_stream error_stream;
        return context_parse(std::forward<Context>(ctx), buffer, error_stream);
    }

    template<typename Buffer, typename ErrorStream>
    constexpr std::optional<root_value_type> parse(const Buffer& buffer, ErrorStream& error_stream) const
    {
        return parse(parse_options{}, buffer, error_stream);
    }

    template<typename Context, typename Buffer, typename ErrorStream>
    constexpr std::optional<root_value_type> context_parse(Context&& ctx, const Buffer& buffer, ErrorStream& error_stream) const
    {
        return context_parse(std::forward<Context>(ctx), parse_options{}, buffer, error_stream);
    }

    template<typename Buffer, typename ErrorStream>
    constexpr std::optional<root_value_type> parse(parse_options options, const Buffer& buffer, ErrorStream& error_stream) const
    {
        return context_parse(no_type{}, options, buffer, error_stream);
    }

    template<typename Context, typename Buffer, typename ErrorStream>
    constexpr std::optional<root_value_type> context_parse(Context&& ctx, parse_options options, const Buffer& buffer, ErrorStream& error_stream) const
    {
        detail::parser_value_stack_type_t<Buffer, empty_rules_count, value_variant_type> value_stack{};
        detail::parse_table_cursor_stack_type_t<Buffer, empty_rules_count> cursor_stack{};

        detail::value_reductors<Context, value_variant_type, rule_tuple_type, rule_count> reductors(rule_tuple);
        detail::parse_state ps(cursor_stack, value_stack, error_stream, options, buffer.begin(), buffer.end(), reductors);

        ps.cursor_stack.push_back(0);

        std::optional<root_value_type> root_value;

        while (true)
        {
            size16_t cursor = ps.cursor_stack.back();

            auto t_idx = get_current_term(ps);
            if (t_idx == uninitialized16)
                break;

            const auto& entry = parse_table[cursor][get_parse_table_idx(true, t_idx)];

            if (entry.kind == parse_table_entry_kind::error)
            {
                if (ps.in_consume_mode())
                {
                    if (!consume_term_recovering(ps))
                        break;
                    continue;
                }
                if (!ps.in_recovery_mode())
                {
                    syntax_error(ps);
                    enter_recovery_mode(ps);
                }
                if (!pop_stacks(ps))
                    break;
                continue;
            }
            else
            {
                if (ps.in_consume_mode())
                    leave_consume_mode(ps);
            }

            if (entry.kind == parse_table_entry_kind::shift)
            {
                shift(ps, buffer.get_view(ps.current_it, ps.current_end_it), t_idx, entry.shift);
                consume_term(ps);
            }
            else if (entry.kind == parse_table_entry_kind::shift_error_recovery_token)
            {
                shift_recovery_token(ps, entry.shift);
                leave_recovery_mode(ps);
                enter_consume_mode(ps);
            }
            else if (entry.kind == parse_table_entry_kind::reduce)
                reduce(std::forward<Context>(ctx), ps, entry.reduce);
            else if (entry.kind == parse_table_entry_kind::rr_conflict)
                rr_conflict(std::forward<Context>(ctx), ps, entry.reduce);
            else if (entry.kind == parse_table_entry_kind::success)
            {
                root_value = std::optional(std::move(success(ps)));
                break;
            }
        }

        return root_value;
    }

    template<typename Stream>
    constexpr void write_diag_str(Stream& s) const
    {
        s << "PARSER" << "\n" << "\n";

        s << "Parser Object size: " << sizeof(*this) << "\n\nRULES\n\n";

        for (size16_t i = 0; i < rule_count; ++i)
        {
            s << i << "    ";
            write_rule_diag_str(s, i);
            s << "\n";
        }

        s << "\nSTATES\n\n";
        for (size16_t i = 0; i < state_count; ++i)
        {
            write_state_diag_str(s, i);
            s << "\n";
        }

        if constexpr (generate_lexer)
        {
            s << "\n" << "LEXICAL ANALYZER" << "\n" << "\n";
            regex::write_dfa_diag_str(lexer_sm, s, term_names);
        }
    }

private:
    static const size_t max_rule_element_count = meta::max_v<1, Rules::n...>;
    static const size16_t eof_idx = sizeof...(Terms);
    static const size16_t error_recovery_token_idx = sizeof...(Terms) + 1;
    static const size_t term_count = sizeof...(Terms) + 2;
    static const size16_t fake_root_idx = sizeof...(NTerms);
    static const size_t nterm_count = sizeof...(NTerms) + 1;
    static const size_t root_rule_idx = sizeof...(Rules);
    static const size_t rule_count = sizeof...(Rules) + 1;
    static const size_t empty_rules_count = meta::count_zeros<Rules::n...>;
    static const size_t situation_size = max_rule_element_count + 1;
    static const size_t situation_address_space_size = rule_count * situation_size * term_count;
    static const size_t max_states = detail::deduce_max_states::template value<term_count, Rules::n..., 1>;
    static const size_t lexer_dfa_size = generate_lexer ? (0 + ... + Terms::dfa_size) : 1;

    using value_variant_type = meta::unique_types_variant_t<
        std::nullptr_t,
        no_type,
        value_type_t<NTerms>...,
        value_type_t<Terms>...
    >;

    using term_subset = stdex::cbitset<term_count>;
    using nterm_subset = stdex::cbitset<nterm_count>;
    using right_side_slice_subset = stdex::cbitset<situation_size * rule_count>;
    using state = stdex::cbitset<situation_address_space_size>;
    using situation_set = stdex::cbitset<situation_address_space_size>;

    struct situation_queue_entry
    {
        size16_t state_idx;
        size32_t idx;
    };

    using situation_queue_t = stdex::cqueue<
        situation_queue_entry,
        max_states * max_states
    >;

    struct symbol
    {
        constexpr symbol() :
            term(false), idx(uninitialized16)
        {}

        constexpr symbol(bool term, size16_t idx) :
            term(term), idx(idx)
        {}

        constexpr size16_t get_parse_table_idx() const
        {
            return parser::get_parse_table_idx(term, idx);
        }

        bool term;
        size16_t idx;
    };

    struct situation_info
    {
        size16_t rule_info_idx;
        size16_t after;
        size16_t t;
    };

    struct rule_info
    {
        size16_t l_idx;
        size16_t r_idx;
        size16_t r_elements;
    };

    enum class parse_table_entry_kind : size16_t { error, success, shift, shift_error_recovery_token, reduce, rr_conflict };

    constexpr static bool is_shift(parse_table_entry_kind kind)
    {
        return kind == parse_table_entry_kind::shift || kind == parse_table_entry_kind::shift_error_recovery_token;
    }

    struct parse_table_entry
    {
        parse_table_entry_kind kind = parse_table_entry_kind::error;
        size16_t shift = uninitialized16;
        size16_t reduce = uninitialized16;

        bool has_shift = false;
        bool has_reduce = false;

        constexpr void set_shift(size16_t value) { shift = value; has_shift = true; }
        constexpr void set_reduce(size16_t value) { reduce = value; has_reduce = true; }
    };

    constexpr static size16_t get_parse_table_idx(bool term, size16_t idx)
    {
        return term ? nterm_count + idx : idx;
    }

    constexpr void analyze_eof()
    {
        term_names[eof_idx] = detail::eof::get_name();
        term_ids[eof_idx] = detail::eof::get_name();
        term_precedences[eof_idx] = 0;
        term_associativities[eof_idx] = associativity::no_assoc;
    }

    constexpr void analyze_error_recovery_token()
    {
        term_names[error_recovery_token_idx] = error_recovery_token::get_name();
        term_ids[error_recovery_token_idx] = error_recovery_token::get_name();
        term_precedences[error_recovery_token_idx] = 0;
        term_associativities[error_recovery_token_idx] = associativity::no_assoc;
    }

    template<size16_t TermIdx, typename Term>
    constexpr void analyze_term(const Term& t)
    {
        term_precedences[TermIdx] = t.get_precedence();
        term_associativities[TermIdx] = t.get_associativity();
        term_names[TermIdx] = t.get_name();
        term_ids[TermIdx] = t.get_id();
        term_ftors[TermIdx] = string_view_to_term_value<TermIdx>;
    }

    template<typename ValueType>
    constexpr void analyze_nterm(const nterm<ValueType>& nt, size16_t idx)
    {
        nterm_names[idx] = nt.get_name();
    }

    template<typename ValueType>
    constexpr void analyze_nterm(detail::fake_root<ValueType>)
    {
        nterm_names[fake_root_idx] = detail::fake_root<ValueType>::get_name();
    }

    template<typename Term>
    constexpr auto make_symbol(const Term& t) const
    {
        return symbol{ true, size16_t(utils::find_str(term_ids, t.get_id())) };
    }

    template<typename ValueType>
    constexpr auto make_symbol(const nterm<ValueType>& nt) const
    {
        return symbol{ false, size16_t(utils::find_str(nterm_names, nt.get_name())) };
    }

    template<size_t... I>
    constexpr void analyze_terms(std::index_sequence<I...>)
    {
        (void(analyze_term<I>(std::get<I>(term_tuple))), ...);
    }

    template<size_t... I>
    constexpr void analyze_nterms(std::index_sequence<I...>)
    {
        (void(analyze_nterm(std::get<I>(nterm_tuple), I)), ...);
    }

    template<size_t... I>
    constexpr void analyze_rules(std::index_sequence<I...>, const root_nterm_type& root)
    {
        (void(analyze_rule<I>(std::get<I>(rule_tuple), std::make_index_sequence<Rules::n>{})), ...);
        analyze_rule<root_rule_idx>(detail::fake_root<value_type_t<root_nterm_type>>{}(root), std::index_sequence<0>{});
        stdex::sort(rule_infos, [](const auto& ri1, const auto& ri2) { return ri1.l_idx < ri2.l_idx; });
        make_nterm_rule_slices();
    }

    constexpr void make_nterm_rule_slices()
    {
        size16_t nt = 0;
        for (size16_t i = 0u; i < rule_count; ++i)
        {
            if (nt != rule_infos[i].l_idx)
            {
                nt = rule_infos[i].l_idx;
                nterm_rule_slices[nt].start = i;
                nterm_rule_slices[nt].n = 1;
            }
            else
                nterm_rule_slices[nt].n++;
        }
    }

    constexpr size16_t calculate_rule_last_term(size16_t rule_idx, size16_t rule_size) const
    {
        for (int i = int(rule_size - 1); i >= 0; --i)
        {
            const auto& s = right_sides[rule_idx][i];
            if (!s.term)
                continue;
            return s.idx;
        }
        return uninitialized16;
    }

    constexpr int calculate_rule_precedence(int precedence, size16_t rule_idx) const
    {
        if (precedence != 0)
            return precedence;
        size16_t last_term_idx = rule_last_terms[rule_idx];
        if (last_term_idx != uninitialized16)
            return term_precedences[last_term_idx];
        return 0;
    }

    constexpr associativity calculate_rule_associativity(size16_t rule_idx) const
    {
        size16_t last_term_idx = rule_last_terms[rule_idx];
        if (last_term_idx != uninitialized16)
            return term_associativities[last_term_idx];
        return associativity::no_assoc;
    }

    template<size_t Nr, bool RequiresContext, typename F, typename L, typename... R, size_t... I>
    constexpr void analyze_rule(const detail::rule<RequiresContext, F, L, R...>& r, std::index_sequence<I...>)
    {
        size16_t l_idx = size16_t(utils::find_str(nterm_names, r.get_l().get_name()));
        (void(right_sides[Nr][I] = make_symbol(std::get<I>(r.get_r()))), ...);
        constexpr size16_t rule_elements_count = size16_t(sizeof...(R));
        rule_infos[Nr] = { l_idx, size16_t(Nr), rule_elements_count };
        rule_last_terms[Nr] = calculate_rule_last_term(Nr, rule_elements_count);
        rule_precedences[Nr] = calculate_rule_precedence(r.get_precedence(), Nr);
        rule_associativities[Nr] = calculate_rule_associativity(Nr);
    }

    struct state_analyze_info
    {
        situation_queue_t situation_queue = { };
        stdex::cvector<size32_t, situation_address_space_size> closures[situation_address_space_size] = {};
        stdex::cbitset<situation_address_space_size> done_closures = {};
        term_subset situation_first_after[situation_address_space_size] = { };
        situation_set situation_first_after_analyzed = {};
        term_subset right_side_slice_first[situation_size * rule_count] = {};
        right_side_slice_subset right_side_slice_first_analyzed = {};
        nterm_subset nterm_empty = { };
        term_subset nterm_first[nterm_count] = { };
        nterm_subset nterm_empty_analyzed = { };
        nterm_subset nterm_first_analyzed = { };
    };

    constexpr size32_t make_situation_idx(situation_info a) const
    {
        return a.rule_info_idx * situation_size * term_count + a.after * term_count + a.t;
    }

    constexpr situation_info make_situation_info(size32_t idx) const
    {
        size16_t t = size16_t(idx % term_count);
        idx /= term_count;
        size16_t after = size16_t(idx % situation_size);
        size16_t rule_info_idx = size16_t(idx / situation_size);
        return situation_info{ rule_info_idx, after, t };
    }

    constexpr void add_term_subset(term_subset& dest, const term_subset& source)
    {
        for (size_t i = 0u; i < term_count; ++i)
            dest.set(i, dest.test(i) || source.test(i));
    }

    constexpr const term_subset& make_right_side_slice_first(const rule_info& ri, size_t start, state_analyze_info& sai)
    {
        size_t right_side_slice_idx = max_rule_element_count * ri.r_idx + start;
        auto& res = sai.right_side_slice_first[right_side_slice_idx];

        if (sai.right_side_slice_first_analyzed.test(right_side_slice_idx))
            return res;
        sai.right_side_slice_first_analyzed.set(right_side_slice_idx);

        for (size_t i = start; i < ri.r_elements; ++i)
        {
            const symbol& s = right_sides[ri.r_idx][i];
            if (s.term)
            {
                res.set(s.idx);
                break;
            }
            add_term_subset(res, make_nterm_first(s.idx, sai));
            if (!make_nterm_empty(s.idx, sai))
                break;
        }
        return res;
    }

    constexpr const term_subset& make_nterm_first(size16_t nt, state_analyze_info& sai)
    {
        if (sai.nterm_first_analyzed.test(nt))
            return sai.nterm_first[nt];
        sai.nterm_first_analyzed.set(nt);

        const utils::slice& s = nterm_rule_slices[nt];
        for (size_t i = 0u; i < s.n; ++i)
        {
            const rule_info& ri = rule_infos[s.start + i];
            add_term_subset(sai.nterm_first[nt], make_right_side_slice_first(ri, 0, sai));
        }
        return sai.nterm_first[nt];
    }

    constexpr bool is_right_side_slice_empty(const rule_info& ri, size_t start, state_analyze_info& sai)
    {
        for (size_t i = start; i < ri.r_elements; ++i)
        {
            const symbol& s = right_sides[ri.r_idx][i];
            if (s.term)
                return false;
            if (!make_nterm_empty(s.idx, sai))
                return false;
        }
        return true;
    }

    constexpr bool is_right_side_empty(const rule_info& ri, state_analyze_info& sai)
    {
        return is_right_side_slice_empty(ri, 0, sai);
    }

    constexpr bool make_nterm_empty(size16_t nt, state_analyze_info& sai)
    {
        if (sai.nterm_empty_analyzed.test(nt))
            return sai.nterm_empty.test(nt);
        sai.nterm_empty_analyzed.set(nt);

        const utils::slice& s = nterm_rule_slices[nt];
        for (size_t i = 0u; i < s.n; ++i)
        {
            if (is_right_side_empty(rule_infos[s.start + i], sai))
            {
                return (sai.nterm_empty.set(nt), true);
            }
        }
        return (sai.nterm_empty.reset(nt), false);
    }

    constexpr const term_subset& make_situation_first_after(const situation_info& addr, size32_t idx, state_analyze_info& sai)
    {
        if (sai.situation_first_after_analyzed.test(idx))
            return sai.situation_first_after[idx];
        sai.situation_first_after_analyzed.set(idx);

        const rule_info& ri = rule_infos[addr.rule_info_idx];
        add_term_subset(sai.situation_first_after[idx], make_right_side_slice_first(ri, addr.after + 1, sai));
        if (is_right_side_slice_empty(ri, addr.after + 1, sai))
            sai.situation_first_after[idx].set(addr.t);
        return sai.situation_first_after[idx];
    }

    constexpr void analyze_states()
    {
        situation_info root_situation_info{ root_rule_idx, 0, eof_idx };
        size32_t idx = make_situation_idx(root_situation_info);
        state_count = 1;

        state_analyze_info sai;

        add_situation_to_state(0, idx, sai);

        while (!sai.situation_queue.empty())
        {
            const auto& entry = sai.situation_queue.top();
            sai.situation_queue.pop();

            situation_closure(entry.state_idx, entry.idx, sai);
            situation_transition(entry.state_idx, entry.idx, sai);
        }
    }

    constexpr void add_situation_to_state(size16_t state_idx, size32_t idx, state_analyze_info& sai)
    {
        state& s = states[state_idx];
        if (!s.test(idx))
        {
            s.set(idx);
            sai.situation_queue.push({ state_idx, idx });
        }
    }

    constexpr void situation_closure(size16_t state_idx, size32_t idx, state_analyze_info& sai)
    {
        if (sai.done_closures.test(idx))
        {
            for(auto i = 0u; i < sai.closures[idx].size(); ++i)
            {
                add_situation_to_state(state_idx, sai.closures[idx][i], sai);
            }
            return;
        }

        sai.done_closures.set(idx);

        situation_info addr = make_situation_info(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        if (addr.after >= ri.r_elements)
            return;

        const symbol& s = right_sides[ri.r_idx][addr.after];
        if (!s.term)
        {
            size16_t nt = s.idx;
            const term_subset& first = make_situation_first_after(addr, idx, sai);
            const utils::slice& sl = nterm_rule_slices[nt];
            for (auto i = 0u; i < sl.n; ++i)
            {
                for (size16_t t = 0; t < term_count; ++t)
                {
                    if (first.test(t))
                    {
                        size32_t new_s_idx = make_situation_idx(situation_info{ size16_t(sl.start + i), 0, t });
                        add_situation_to_state(state_idx, new_s_idx, sai);
                        sai.closures[idx].push_back(new_s_idx);
                    }
                }
            }
        }
    }

    constexpr auto solve_conflict(size16_t rule_info_idx, size16_t term_idx) const
    {
        size16_t rule_idx = rule_infos[rule_info_idx].r_idx;
        int r_p = rule_precedences[rule_idx];
        int t_p = term_precedences[term_idx];
        if (r_p > t_p)
            return parse_table_entry_kind::reduce;

        if (r_p == t_p)
        {
            if (rule_associativities[rule_idx] == associativity::ltor)
                return parse_table_entry_kind::reduce;
        }
        return parse_table_entry_kind::shift;
    }

    constexpr void situation_transition(size16_t state_idx, size32_t idx, state_analyze_info& sai)
    {
        situation_info addr = make_situation_info(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        bool reduction = addr.after >= ri.r_elements;

        const auto& s = right_sides[ri.r_idx][addr.after];
        size16_t symbol_idx = reduction ? get_parse_table_idx(true, addr.t) : s.get_parse_table_idx();
        auto& entry = parse_table[state_idx][symbol_idx];
        if (reduction)
        {
            if (ri.r_idx == root_rule_idx)
            {
                entry.kind = parse_table_entry_kind::success;
            }
            else if (entry.has_shift)
            {
                entry.kind = solve_conflict(addr.rule_info_idx, addr.t);
            }
            else if (entry.has_reduce)
            {
                entry.kind = parse_table_entry_kind::rr_conflict;
            }
            else
            {
                entry.kind = parse_table_entry_kind::reduce;
            }
            entry.set_reduce(size16_t(addr.rule_info_idx));
            return;
        }

        situation_info new_addr = situation_info{ addr.rule_info_idx, size16_t(addr.after + 1), addr.t };
        size32_t new_idx = make_situation_idx(new_addr);

        if (entry.has_shift)
        {
            add_situation_to_state(entry.shift, new_idx, sai);
            return;
        }

        size16_t new_state_idx = uninitialized16;
        for (size16_t i = 0; i < state_count; ++i)
        {
            if (states[i].test(new_idx))
            {
                new_state_idx = i;
                break;
            }
        }
        if (new_state_idx == uninitialized16)
        {
            new_state_idx = state_count;
            ++state_count;
        }

        if (entry.has_reduce)
        {
            entry.kind = solve_conflict(entry.reduce, s.idx);
        }
        else
        {
            entry.kind = parse_table_entry_kind::shift;
        };
        entry.set_shift(new_state_idx);

        if (entry.kind == parse_table_entry_kind::shift && s.idx == error_recovery_token_idx)
            entry.kind = parse_table_entry_kind::shift_error_recovery_token;

        add_situation_to_state(new_state_idx, new_idx, sai);
    }

    constexpr const char* get_symbol_name(const symbol& s) const
    {
        return s.term ? term_names[s.idx] : nterm_names[s.idx];
    }

    template<typename Stream>
    constexpr void write_rule_diag_str(Stream& s, size16_t rule_info_idx) const
    {
        const rule_info& ri = rule_infos[rule_info_idx];
        s << nterm_names[ri.l_idx] << " <- ";
        if (ri.r_elements > 0)
            s << get_symbol_name(right_sides[ri.r_idx][0]);
        if constexpr (max_rule_element_count > 1)
        {
            for (size_t i = 1u; i < ri.r_elements; ++i)
            {
                s << " " << get_symbol_name(right_sides[ri.r_idx][i]);
            }
        }
    }

    template<typename Stream>
    constexpr void write_situation_diag_str(Stream& s, size32_t idx) const
    {
        const situation_info addr = make_situation_info(idx);
        const rule_info& ri = rule_infos[addr.rule_info_idx];
        s << nterm_names[ri.l_idx] << " <- ";
        for (size_t i = 0u; i < addr.after; ++i)
        {
            s << get_symbol_name(right_sides[ri.r_idx][i]) << " ";
        }
        s << ". ";
        for (size_t i = addr.after; i < ri.r_elements; ++i)
        {
            s << get_symbol_name(right_sides[ri.r_idx][i]) << " ";
        }
        s << "==> " << term_names[addr.t];
    }

    template<typename Stream>
    constexpr void write_state_diag_str(Stream& s, size16_t idx) const
    {
        s << "STATE " << idx << "\n";

        for (size32_t i = 0u; i < situation_address_space_size; ++i)
        {
            if (states[idx].test(i))
            {
                write_situation_diag_str(s, i);
                s << "\n";
            }
        }

        s << "\n";

        for (size_t i = 0; i < nterm_count; ++i)
        {
            const auto& entry = parse_table[idx][i];
            if (is_shift(entry.kind))
                s << "On " << nterm_names[i] << " go to " << entry.shift << "\n";
        }
        for (size_t i = nterm_count; i < nterm_count + term_count; ++i)
        {
            const auto& entry = parse_table[idx][i];
            if (entry.kind == parse_table_entry_kind::error)
                continue;

            size_t term_idx = i - nterm_count;
            s << "On " << term_names[term_idx];
            if (entry.kind == parse_table_entry_kind::success)
                s << " success \n";
            else if (entry.kind == parse_table_entry_kind::reduce && entry.has_shift)
                s << " shift to " << entry.shift << " S/R CONFLICT, prefer reduce(" << rule_infos[entry.reduce].r_idx << ") over shift\n";
            else if (is_shift(entry.kind) && entry.has_reduce)
                s << " shift to " << entry.shift << " S/R CONFLICT, prefer shift over reduce(" << rule_infos[entry.reduce].r_idx << ")\n";
            else if (is_shift(entry.kind))
                s << " shift to " << entry.shift << "\n";
            else if (entry.kind == parse_table_entry_kind::reduce)
                s << " reduce using (" << rule_infos[entry.reduce].r_idx << ")\n";
            else if (entry.kind == parse_table_entry_kind::rr_conflict)
                s << " R/R CONFLICT - !!! FIX IT !!! \n";
        }
    }

    template<size16_t TermIdx>
    constexpr static value_variant_type string_view_to_term_value(const term_tuple_type& term_tuple, const std::string_view& sv, source_point sp)
    {
        const auto &t = std::get<TermIdx>(term_tuple);
        using term_value_type = value_type_t<std::tuple_element_t<TermIdx, term_tuple_type>>;
        return value_variant_type(term_value_type(t.get_ftor()(sv), sp));
    }

    template<typename ParseState>
    constexpr void shift_recovery_token(ParseState& ps, size16_t new_cursor_value) const
    {
        if (ps.options.verbose)
            ps.error_stream << ps.current_sp << " PARSE: Shift to " << new_cursor_value << ", term: " << term_names[error_recovery_token_idx] << "\n";
        ps.cursor_stack.push_back(new_cursor_value);

        ps.value_stack.emplace_back(term_value(no_type{}, ps.current_sp));
    }

    template<typename ParseState>
    constexpr void shift(ParseState& ps, const std::string_view& sv, size16_t term_idx, size16_t new_cursor_value) const
    {
        if (ps.options.verbose)
            ps.error_stream << ps.current_sp << " PARSE: Shift to " << new_cursor_value << ", term: " << sv << "\n";
        ps.cursor_stack.push_back(new_cursor_value);

        const auto& ftor = term_ftors[term_idx];
        ps.value_stack.emplace_back(ftor(term_tuple, sv, ps.current_sp));
    }

    template<typename Context, typename ParseState>
    constexpr void reduce(Context&& ctx, ParseState& ps, size16_t rule_info_idx) const
    {
        const auto& ri = rule_infos[rule_info_idx];
        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Reduced using rule " << ri.r_idx << "  ";
            write_rule_diag_str(ps.error_stream, rule_info_idx);
            ps.error_stream << "\n";
        }

        ps.cursor_stack.erase(ps.cursor_stack.end() - ri.r_elements, ps.cursor_stack.end());
        size16_t new_cursor_value = parse_table[ps.cursor_stack.back()][ri.l_idx].shift;

        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Go to " << new_cursor_value << "\n";
        }

        ps.cursor_stack.push_back(new_cursor_value);
        value_variant_type* start = ps.value_stack.data() + ps.value_stack.size() - ri.r_elements;
        value_variant_type lvalue(ps.reductors.invoke(std::forward<Context>(ctx), ri.r_idx, start));
        ps.value_stack.erase(ps.value_stack.end() - ri.r_elements, ps.value_stack.end());
        ps.value_stack.emplace_back(std::move(lvalue));
    }

    template<typename Context, typename ParseState>
    constexpr void rr_conflict(Context&& ctx, ParseState& ps, size16_t rule_idx) const
    {
        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: R/R conflict encountered \n";
        }
        reduce(std::forward<Context>(ctx), ps, rule_idx);
    }

    template<typename ParseState>
    constexpr bool pop_stacks(ParseState& ps) const
    {
        ps.cursor_stack.pop_back();
        if (ps.value_stack.size() != 0)
            ps.value_stack.pop_back();

        if (ps.cursor_stack.size() == 0)
        {
            if (ps.options.verbose)
            {
                ps.error_stream << ps.current_sp << " PARSE: Could not recover from error \n";
            }
            return false;
        }

        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Recovering to state " << ps.cursor_stack.back() << "\n";
        }

        return true;
    }

    template<typename ParseState>
    constexpr void syntax_error(ParseState& ps) const
    {
        ps.error_stream << ps.current_sp << " PARSE: Syntax error: " <<
            "Unexpected '" << term_names[ps.current_term_idx] << "'" << "\n";
    }

    template<typename ParseState>
    constexpr root_value_type& success(ParseState& ps) const
    {
        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Success \n";
        }
        return std::get<root_value_type>(ps.value_stack.front());
    }

    template<typename ParseState>
    constexpr void consume_term(ParseState& ps) const
    {
        ps.current_sp.update(ps.current_it, ps.current_end_it);
        ps.current_it = ps.current_end_it;
    }

    template<typename ParseState>
    constexpr void enter_recovery_mode(ParseState& ps) const
    {
        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Entering recovery mode \n";
        }
        ps.enter_recovery_mode();
    }

    template<typename ParseState>
    constexpr void leave_recovery_mode(ParseState& ps) const
    {
        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Leaving recovery mode \n";
        }
        ps.leave_recovery_mode();
    }

    template<typename ParseState>
    constexpr void enter_consume_mode(ParseState& ps) const
    {
        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Entering consume mode \n";
        }
        ps.enter_consume_mode();
    }

    template<typename ParseState>
    constexpr void leave_consume_mode(ParseState& ps) const
    {
        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Leaving consume mode \n";
        }
        ps.leave_consume_mode();
    }

    template<typename ParseState>
    constexpr bool consume_term_recovering(ParseState& ps) const
    {
        if (ps.current_term_idx == eof_idx)
            return false;
        if (ps.options.verbose)
        {
            ps.error_stream << ps.current_sp << " PARSE: Recovery, consuming term " << term_names[ps.current_term_idx] << " \n";
        }
        consume_term(ps);
        return true;
    }

    template<typename ParseState>
    constexpr size16_t get_current_term(ParseState& ps) const
    {
        if (ps.in_recovery_mode())
            return error_recovery_token_idx;

        if (ps.current_it != ps.current_end_it)
            return ps.current_term_idx;

        if (ps.options.skip_whitespace)
        {
            auto after_ws = skip_whitespace(ps);
            ps.current_sp.update(ps.current_it, after_ws);
            ps.current_it = after_ws;
        }

        if (ps.current_it == ps.buffer_end)
        {
            ps.current_term_idx = eof_idx;
            trace_recognized_term(ps);
            return eof_idx;
        }

        recognized_term res;
        match_options opts;
        opts.set_verbose(ps.options.verbose);

        if constexpr (generate_lexer)
        {
            res = regex::dfa_match(lexer_sm, opts, ps.current_sp, ps.current_it, ps.buffer_end, ps.error_stream);
        }
        else
        {
            lexer_type custom_lexer;
            res = custom_lexer.match(opts, ps.current_sp, ps.current_it, ps.buffer_end, ps.error_stream);
        }

        ps.current_term_idx = res.term_idx;
        ps.current_end_it = ps.current_it + res.len;

        if (ps.current_term_idx == uninitialized16)
        {
            unexpected_char(ps);
            return uninitialized16;
        }
        else
        {
            trace_recognized_term(ps);
        }

        return ps.current_term_idx;
    }

    template<typename ParseState>
    constexpr auto skip_whitespace(ParseState& ps) const
    {
        constexpr char space_chars_newline[] = { 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x20, 0x00 };
        constexpr char space_chars_no_newline[] = { 0x09, 0x0b, 0x0c, 0x0d, 0x20, 0x00 };

        const auto& space_chars = ps.options.skip_newline
                                     ? space_chars_newline
                                     : space_chars_no_newline;

        auto start = ps.current_it;
        while (true)
        {
            if (start == ps.buffer_end)
                break;
            if (utils::find_char(*start, space_chars) == uninitialized)
                break;
            ++start;
        }
        return start;
    }

    template<typename ParseState>
    constexpr void unexpected_char(ParseState& ps) const
    {
        ps.error_stream << ps.current_sp << " PARSE: Unexpected character: " << *ps.current_it << "\n";
    }

    template<typename ParseState>
    constexpr void trace_recognized_term(ParseState& ps) const
    {
        if (ps.options.verbose)
            ps.error_stream << ps.current_sp << " PARSE: Recognized " << term_names[ps.current_term_idx] << " \n";
    }

    struct no_parser{};

    template<size_t... I>
    constexpr void create_lexer(std::index_sequence<I...>)
    {
        if constexpr (generate_lexer)
        {
            regex::dfa_builder<lexer_dfa_size> b(lexer_sm);
            constexpr bool trivial_lexer = (true && ... && std::tuple_element_t<I, term_tuple_type>::is_trivial);
            if constexpr (trivial_lexer)
                (void(regex::add_term_data_to_dfa(std::get<I>(term_tuple).get_data(), b, no_parser{}, size16_t(I))), ...);
            else
            {
                auto p = regex::create_regex_parser(b);
                (void(regex::add_term_data_to_dfa(std::get<I>(term_tuple).get_data(), b, p, size16_t(I))), ...);
            }
        }
    }

    str_table<term_count> term_names = { };
    str_table<term_count> term_ids = { };
    str_table<nterm_count> nterm_names = { };
    symbol right_sides[rule_count][max_rule_element_count] = { };
    rule_info rule_infos[rule_count] = { };
    utils::slice nterm_rule_slices[nterm_count] = { };
    state states[max_states] = { };
    parse_table_entry parse_table[max_states][term_count + nterm_count] = {};
    size16_t state_count = 0;
    int term_precedences[term_count] = { };
    associativity term_associativities[term_count] = { };
    int rule_precedences[rule_count] = { };
    associativity rule_associativities[rule_count] = { };
    size16_t rule_last_terms[rule_count] = { };
    term_tuple_type term_tuple;
    nterm_tuple_type nterm_tuple;
    rule_tuple_type rule_tuple;

    using string_view_to_term_value_t = value_variant_type(*)(const term_tuple_type&, const std::string_view&, source_point);
    string_view_to_term_value_t term_ftors[term_count] = {};

    using dfa_type = regex::dfa<lexer_dfa_size>;
    dfa_type lexer_sm = {};
};

template<typename Root, typename Terms, typename NTerms, typename Rules>
parser(Root, Terms, NTerms, Rules&&) -> parser<Root, Terms, NTerms, Rules, use_generated_lexer>;

template<typename Root, typename Terms, typename NTerms, typename Rules, typename LexerUsage>
parser(Root, Terms, NTerms, Rules&&, LexerUsage) -> parser<Root, Terms, NTerms, Rules, LexerUsage>;

template<typename... Terms>
constexpr auto terms(const Terms&... terms)
{
    return std::make_tuple(detail::make_term(terms)...);
}

template<typename... NTerms>
constexpr auto nterms(NTerms... nterms)
{
    return std::make_tuple(nterms...);
}

template<typename... Rules>
constexpr auto rules(Rules&&... rules)
{
    return std::make_tuple(std::move(rules)...);
}

struct skip
{
    template<typename T>
    constexpr skip(T&&) {}
};

namespace regex
{
    class regex_lexer
    {
    public:
        constexpr regex_lexer()
        {
            //0              1             2 3 4 5 6 7 8 9
            //regex_digit_09 regex_primary * + ? | ( ) { }

            specials[utils::char_to_idx('*')] = 2;
            specials[utils::char_to_idx('+')] = 3;
            specials[utils::char_to_idx('?')] = 4;
            specials[utils::char_to_idx('|')] = 5;
            specials[utils::char_to_idx('(')] = 6;
            specials[utils::char_to_idx(')')] = 7;
            specials[utils::char_to_idx('{')] = 8;
            specials[utils::char_to_idx('}')] = 9;
        }

        template<typename Iterator, typename ErrorStream>
        constexpr auto match(
            match_options options,
            source_point sp,
            Iterator start,
            Iterator end,
            ErrorStream& error_stream)
        {
            if (start == end)
                return recognized_term{};

            char c = *start;

            auto res = [&](size16_t idx, size_t len)
            { return recognized(idx, len, options, sp, error_stream); };

            if (specials[utils::char_to_idx(c)] != 0)
                return res(specials[utils::char_to_idx(c)], 1);

            if (utils::is_dec_digit(c))
                return res(0, 1);

            size_t len = 0;
            bool ok = match_primary(start, end, len);
            if (ok)
                return res(1, len);

            return recognized_term{};
        }

    private:
        size16_t specials[meta::distinct_chars_count] = {};

        template<typename Iterator>
        constexpr bool match_primary(Iterator start, Iterator end, size_t& len)
        {
            len = 0;
            bool ok = match_escaped(start, end, len);
            if (!ok)
                return false;
            if (len != 0)
                return true;

            ok = match_range(start, end, len);
            if (!ok)
                return false;
            if (len != 0)
                return true;

            ok = utils::is_printable(*start);
            if (ok)
                len = 1;
            return ok;
        }

        template<typename Iterator>
        constexpr bool match_range(Iterator start, Iterator end, size_t& len)
        {
            char c = *start;
            if (c == '[')
            {
                len = 1;
                ++start;
                if (start == end)
                {
                    len = 0;
                    return false;
                }
                c = *start;
                if (c == '^')
                {
                    len = 2;
                    ++start;
                }
                if (start == end)
                {
                    len = 0;
                    return false;
                }
                while (start != end && *start != ']')
                {
                    size_t item_len = 0;
                    bool ok = match_range_item(start, end, item_len);
                    if (!ok)
                    {
                        len = 0;
                        return false;
                    }
                    len += item_len;
                    start += item_len;
                }
                if (start == end)
                {
                    len = 0;
                    return false;
                }
                len++;
            }
            return true;
        }

        template<typename Iterator>
        constexpr bool match_range_item(Iterator start, Iterator end, size_t& len)
        {
            len = 0;
            bool ok = match_escaped(start, end, len);
            if (!ok)
                return false;

            if (len == 0)
            {
                ok = utils::is_printable(*start);
                if (!ok)
                    return false;
                len = 1;
            }

            start += len;

            if (*start == '-')
            {
                len++;

                ++start;
                if (start == end || *start == ']')
                    return false;

                size_t range_end_len = 0;
                bool escaped_ok = match_escaped(start, end, range_end_len);
                if (!escaped_ok)
                    return false;

                if (range_end_len == 0)
                {
                    ok = utils::is_printable(*start);
                    if (!ok)
                        return false;
                    len += 1;
                }
                else
                    len += range_end_len;
            }

            return ok;
        }

        template<typename Iterator>
        constexpr bool match_escaped(Iterator start, Iterator end, size_t& len)
        {
            char c = *start;

            if (c == '\\')
            {
                ++start;
                if (start == end)
                    return false;
                c = *start;
                len = 2;
                if (c == 'x')
                {
                    ++start;
                    if (start == end)
                        return true;
                    c = *start;
                    if (!utils::is_hex_digit(c))
                        return true;
                    ++start;
                    len = 3;
                    if (start == end)
                        return true;
                    c = *start;
                    if (!utils::is_hex_digit(c))
                        return true;
                    len = 4;
                    return true;
                }
                return utils::is_printable(c);
            }
            return true;
        }

        template<typename ErrorStream>
        constexpr auto recognized(
            size16_t idx,
            size_t len,
            match_options options,
            source_point sp,
            ErrorStream& error_stream)
        {
            if (options.verbose)
                error_stream << sp << " LEXER MATCH: Recognized " << idx << " \n";
            return recognized_term(idx, len);
        }
    };

    constexpr char regex_char(std::string_view sv, size_t& len)
    {
        if (sv[0] == '\\')
        {
            if (sv[1] == 'x')
            {
                if (sv.size() == 2 || !utils::is_hex_digit(sv[2]))
                {
                    len = 2;
                    return 0;
                }
                else if (sv.size() == 3 || !utils::is_hex_digit(sv[3]))
                {
                    len = 3;
                    return hex_digits_to_char('0', sv[2]);
                }
                else
                {
                    len = 4;
                    return hex_digits_to_char(sv[2], sv[3]);
                }
            }
            else
            {
                len = 2;
                return sv[1];
            }
        }
        else
        {
            len = 1;
            return sv[0];
        }
    }

    constexpr char_subset string_view_to_subset(std::string_view sv)
    {
        char_subset cs;

        if (sv[0] == '.')
            cs.flip();
        else if (sv[0] == '[')
        {
            bool flip = false;
            size_t i = 1;
            if (sv[1] == '^')
            {
                i++;
                flip = true;
            }
            while (sv[i] != ']')
            {
                size_t len = 0;
                char c1 = regex_char(sv.substr(i), len);
                i += len;
                if (sv[i] == '-')
                {
                    ++i;
                    char c2 = regex_char(sv.substr(i), len);
                    cs.add_range(char_range{c1, c2});
                }
                else
                    cs.set(utils::char_to_idx(c1));
            }
            if (flip)
                cs.flip();
        }
        else
        {
            size_t len = 0;
            cs.set(utils::char_to_idx(regex_char(sv, len)));
        }

        return cs;
    }

    template<typename Builder>
    constexpr auto create_regex_parser(Builder& b)
    {
        using slice = utils::slice;
        using namespace ftors;

        constexpr nterm<slice> expr("expr");
        constexpr nterm<slice> alt("alt");
        constexpr nterm<slice> concat("concat");
        constexpr nterm<slice> q_expr("q_expr");
        constexpr nterm<slice> primary("primary");
        constexpr nterm<size32_t> number("number");

        constexpr custom_term regex_digit_09("regex_digit_09", [](auto sv) { return size32_t(sv[0]) - '0'; });
        constexpr custom_term regex_primary("regex_primary", string_view_to_subset);

        return parser(
            expr,
            terms(regex_digit_09, regex_primary, '*', '+', '?', '|', '(', ')', '{', '}'),
            nterms(expr, alt, concat, q_expr, primary, number),
            rules(
                number(regex_digit_09),
                number(number, regex_digit_09) >= [](size32_t n, size32_t x){ return n * 10 + x; },
                primary(regex_digit_09) >= [&b](size32_t number){ return b.primary_char(char(number + '0')); },
                primary(regex_primary) >= [&b](const auto& s){ return b.primary_subset(s); },
                primary('(', expr, ')') >= _e2,
                q_expr(primary),
                q_expr(primary, '*') >= [&b](slice s, skip) { return b.star(s); },
                q_expr(primary, '+') >= [&b](slice s, skip) { return b.plus(s); },
                q_expr(primary, '?') >= [&b](slice s, skip) { return b.opt(s); },
                q_expr(primary, '{', number, '}') >= [&b](slice s, skip, size32_t n, skip) { return b.rep(s, n); },
                concat(q_expr),
                concat(concat, q_expr) >= [&b](slice s1, slice s2) { return b.cat(s1, s2); },
                alt(concat),
                alt(alt, '|', alt) >= [&b](slice s1, skip, slice s2) { return b.alt(s1, s2); },
                expr(alt)
            ),
            use_lexer<regex_lexer>{}
        );
    }

    template<size_t N>
    constexpr size32_t analyze_dfa_size(const char (&pattern)[N])
    {
        dfa_size_analyzer a;
        auto p = create_regex_parser(a);
        buffers::cstring_buffer buffer(pattern);
        detail::no_stream s{};
        auto res = p.parse(
            parse_options{}.set_skip_whitespace(false),
            buffer,
            s);
        if (!res.has_value())
            throw std::runtime_error("invalid regex");
        return res.value().n;
    }

    template<typename Stream>
    constexpr void write_regex_parser_diag_msg(Stream& s)
    {
        regex::dfa_size_analyzer a;
        auto p = regex::create_regex_parser(a);
        p.write_diag_str(s);
    }

    template<auto& Pattern>
    class expr
    {
    public:
        static const size32_t dfa_size = analyze_dfa_size(Pattern);

        constexpr expr()
        {
            dfa_builder<dfa_size> b(sm);
            auto p = create_regex_parser(b);
            detail::no_stream stream{};
            auto s = p.parse(
                parse_options{}.set_skip_whitespace(false),
                buffers::cstring_buffer(Pattern),
                stream
            );
            if (!s.has_value())
                throw std::runtime_error("invalid regex");
            b.mark_end_states(s.value(), 0);
        }

        template<typename Stream>
        constexpr void debug_parse(Stream& s) const
        {
            regex::dfa_size_analyzer a;
            auto p = regex::create_regex_parser(a);
            p.parse(
                parse_options{}.set_skip_whitespace(false).set_verbose(),
                buffers::cstring_buffer(Pattern),
                s
            );
        }

        template<size_t N>
        constexpr bool match(const char (&str)[N]) const
        {
            return match(buffers::cstring_buffer(str));
        }

        template<typename Buffer>
        constexpr bool match(const Buffer& buf) const
        {
            detail::no_stream s;
            return match(buf, s);
        }

        template<typename Buffer, typename Stream>
        constexpr bool match(const Buffer& buf, Stream& s) const
        {
            return match(match_options{}, buf, s);
        }

        template<typename Buffer, typename Stream>
        constexpr bool match(match_options opts, const Buffer& buf, Stream& s) const
        {
            auto res = dfa_match(sm, opts, source_point{}, buf.begin(), buf.end(), s);
            auto end = buf.begin() + res.len;
            if (res.term_idx == 0 && end == buf.end())
                return true;
            else
            {
                if (res.term_idx == 0)
                    s << "Leftover text after recognition: " << buf.get_view(end, buf.end()) << "\n";
                else
                    s << "Unexpected char: " << utils::c_names.name(*end) << "\n";
                return false;
            }
        }

        template<typename Stream>
        constexpr void write_diag_str(Stream& stream) const
        {
            regex::write_dfa_diag_str(sm, stream);
        }

    private:
        dfa<dfa_size> sm;
    };
}

template<auto& Pattern>
class regex_term : public term
{
public:
    using internal_value_type = std::string_view;

    static const size_t dfa_size = regex::analyze_dfa_size(Pattern);
    static const bool is_trivial = false;

    static const size_t pattern_size = std::size(Pattern);

    constexpr regex_term(associativity a = associativity::no_assoc) :
        regex_term(nullptr, 0, a)
    {}

    constexpr regex_term(int precedence = 0, associativity a = associativity::no_assoc) :
        regex_term(nullptr, precedence, a)
    {}

    constexpr regex_term(const char *custom_name, int precedence = 0, associativity a = associativity::no_assoc) :
        term(precedence, a),
        custom_name(custom_name)
    {
        id[0] = 'r';
        id[1] = '_';
        utils::copy_array(&id[2], Pattern, std::make_index_sequence<pattern_size>{});
    }

    constexpr const char* get_name() const { return custom_name ? custom_name : id; }
    constexpr const char* get_id() const { return id; }
    constexpr auto get_data() const { return regex::regex_pattern_data<pattern_size>{ Pattern }; }

    constexpr const auto& get_ftor() const { return detail::pass_sv; }

private:
    char id[pattern_size + 2] = {};
    const char* custom_name = nullptr;
};

} // namespace ctpg

#endif
