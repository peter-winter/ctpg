cmake_minimum_required(VERSION 3.14)
project(ctpg-examples LANGUAGES CXX)

option(CTPG_WARNING_FLAGS "When ON, enable all warnings and promote warnings to errors." ON)

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
    if (CTPG_WARNING_FLAGS)
        if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
            target_compile_options(${example} PRIVATE /W4 /WX 
                /wd4459  # declaration of 'identifier' hides global declaration
            )
        else ()
            target_compile_options(${example} PRIVATE -Wall -Wextra -pedantic -Werror)
        endif ()
    endif ()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        target_compile_options(${example} PRIVATE -fconstexpr-steps=1000000000)
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        target_compile_options(${example} PRIVATE -fconstexpr-ops-limit=1000000000)
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        target_compile_options(${example} PRIVATE /EHsc /constexpr:steps1000000000)
    endif()
    
endforeach ()
