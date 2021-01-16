cmake_minimum_required(VERSION 3.15)

project(SonicPi)

set(APP_ROOT ${CMAKE_CURRENT_LIST_DIR})

add_subdirectory(api)

add_subdirectory(gui)

