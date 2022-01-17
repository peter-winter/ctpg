cmake_minimum_required(VERSION 3.14)
project(
    "ctpg"
    VERSION 1.3.6
    DESCRIPTION "Compile Time Parser Generator. A single header library turning c++ code into LR(1) parser with finite state machine lexer in compile time."
    HOMEPAGE_URL "https://github.com/peter-winter/ctpg"
    LANGUAGES CXX
)

# CMake module dependencies

include(CMakePackageConfigHelpers)
include(CTest)
include(GNUInstallDirs)

# CTPG specific options

string(COMPARE EQUAL "${CMAKE_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}" is_top_level)

option(CTPG_ENABLE_INSTALL "Include project-specific install rules" "${is_top_level}")
option(CTPG_ENABLE_TESTS "Build test binaries" "${BUILD_TESTING}")

if (NOT DEFINED CTPG_INSTALL_CMAKEDIR)
    set(CTPG_INSTALL_CMAKEDIR "${CMAKE_INSTALL_DATAROOTDIR}/ctpg/cmake"
        CACHE STRING "Install path for ctpg CMake files")
endif ()

option(CTPG_WARNING_FLAGS "When ON, enable all warnings and promote warnings to errors." "${is_top_level}")

# CTPG header-only library

add_library(ctpg INTERFACE)
add_library(ctpg::ctpg ALIAS ctpg)
target_include_directories(ctpg INTERFACE "$<BUILD_INTERFACE:${ctpg_SOURCE_DIR}/include>")
target_compile_features(ctpg INTERFACE cxx_std_17)

# Test

if (CTPG_ENABLE_TESTS)
    find_package(Catch2 2 REQUIRED)

    add_executable(testbin
        tests/main.cpp
        tests/regex_tests_1.cpp
        tests/regex_tests_2.cpp
        tests/regex_tests_3.cpp
        tests/simple_cases.cpp
        tests/simple_grammar.cpp
        tests/empty_symbols.cpp
        tests/recurrence.cpp
        tests/precedence.cpp
        tests/compile_time.cpp
        tests/list_helpers.cpp
        tests/skip_whitespace.cpp
        tests/typed_terms.cpp
        tests/source_tracking.cpp
        tests/error_recovery.cpp
        tests/buffers.cpp
    )

    target_link_libraries(testbin PRIVATE Catch2::Catch2 ctpg::ctpg)
    if (CTPG_WARNING_FLAGS)
        if (MSVC)
            target_compile_options(testbin PRIVATE /W4 /WX)
        else ()
            target_compile_options(testbin PRIVATE -Wall -Wextra -pedantic -Werror)
        endif ()
    endif ()

    include(Catch)
    catch_discover_tests(testbin)
endif ()

# Install

if (NOT CTPG_ENABLE_INSTALL)
    return()
endif ()

install(
    TARGETS ctpg
    EXPORT ctpg-targets
    INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)

install(
    EXPORT ctpg-targets
    DESTINATION "${CTPG_INSTALL_CMAKEDIR}"
    NAMESPACE ctpg::
    FILE ctpg-targets.cmake
    COMPONENT ctpg
)

install(
    DIRECTORY "${PROJECT_SOURCE_DIR}/include/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
    COMPONENT ctpg
)

configure_package_config_file(
    "${PROJECT_SOURCE_DIR}/cmake/ctpg-config.cmake.in"
    "${PROJECT_BINARY_DIR}/ctpg-config.cmake"
    INSTALL_DESTINATION "${CTPG_INSTALL_CMAKEDIR}"
)

write_basic_package_version_file(
    "ctpg-config-version.cmake"
    COMPATIBILITY SameMajorVersion
    ARCH_INDEPENDENT
)

install(
    FILES "${PROJECT_BINARY_DIR}/ctpg-config.cmake"
    "${PROJECT_BINARY_DIR}/ctpg-config-version.cmake"
    DESTINATION "${CTPG_INSTALL_CMAKEDIR}"
    COMPONENT ctpg
)
