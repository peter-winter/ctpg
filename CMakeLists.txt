cmake_minimum_required(VERSION 3.12)

project("ctpg" VERSION 1.3.4
     DESCRIPTION "Compile Time Parser Generator. A single header library turning c++ code into LR(1) parser with finite state machine lexer in compile time."
     HOMEPAGE_URL "https://github.com/peter-winter/ctpg")

include(GNUInstallDirs)

if (MSVC)
    add_compile_options(/W4 /WX)
else()
    add_compile_options(-Wall -Wextra -pedantic -Werror)
endif()


add_library(${PROJECT_NAME} INTERFACE)

target_include_directories(
  ${PROJECT_NAME}
  INTERFACE
  $<BUILD_INTERFACE:${${PROJECT_NAME}_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

target_compile_features(${PROJECT_NAME} INTERFACE cxx_std_17)

# Test
find_package(Catch2 REQUIRED)

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

target_link_libraries(testbin Catch2::Catch2)
target_include_directories(testbin PRIVATE ${PROJECT_SOURCE_DIR}/include)
target_compile_features(testbin PUBLIC cxx_std_17)

include(CTest)
include(Catch)
catch_discover_tests(testbin)
enable_testing()

# Install
install(TARGETS ${PROJECT_NAME}
        EXPORT ${PROJECT_NAME}_Targets
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

include(CMakePackageConfigHelpers)

write_basic_package_version_file("${PROJECT_NAME}ConfigVersion.cmake"
                                 VERSION ${PROJECT_VERSION}
                                 COMPATIBILITY SameMajorVersion)
if(NOT INCLUDE_INSTALL_DIR)
  set(INCLUDE_INSTALL_DIR include/${PROJECT_NAME})
endif()

configure_package_config_file(
  "${PROJECT_SOURCE_DIR}/cmake/${PROJECT_NAME}Config.cmake.in"
  "${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
  INSTALL_DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake
  PATH_VARS INCLUDE_INSTALL_DIR)

install(EXPORT ${PROJECT_NAME}_Targets
        FILE ${PROJECT_NAME}Targets.cmake
        NAMESPACE ${PROJECT_NAME}::
        DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake)

install(FILES "${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
              "${PROJECT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
        DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME})
