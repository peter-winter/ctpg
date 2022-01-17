cmake_minimum_required(VERSION 3.14)
project(language LANGUAGES CXX)

find_package(ctpg 1 REQUIRED)

add_executable(language language.cpp)
target_link_libraries(language PRIVATE ctpg::ctpg)
