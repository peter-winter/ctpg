cmake_minimum_required(VERSION 3.12)
project(ctpg-examples LANGUAGES CXX)

find_package(ctpg 1 REQUIRED)

set(examples
    ctjs
    custom-lexer
    error-recovery
    html
    json-parser
    readme-example
    regex-test
    simple-expr-parser
    source-tracking
    typed-terms
)

foreach (example IN LISTS examples)
    add_executable(${example} ${example}.cpp)
    target_link_libraries(${example} PRIVATE ctpg::ctpg)
endforeach ()
