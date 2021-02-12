cmake_minimum_required(VERSION 3.13)

message(STATUS " CMakeLists: Sonic Pi")

# Sonic Pi Requires a C++ 17 Compiler to build it
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

#set (QT_OLD_API 1)

if(APPLE)
  set(CMAKE_OSX_DEPLOYMENT_TARGET '10.13')
endif()

set(APP_ROOT ${CMAKE_CURRENT_LIST_DIR})

# Different triplet for windows
if (WIN32)
    set(VCPKG_TARGET_TRIPLET x64-windows-static-md CACHE STRING "triplet")
endif()

# vcpkg toolchain - Must be declared before project
set(CMAKE_TOOLCHAIN_FILE ${APP_ROOT}/vcpkg/scripts/buildsystems/vcpkg.cmake CACHE STRING "Vcpkg toolchain file")

project(SonicPi)

list(APPEND CMAKE_MODULE_PATH ${APP_ROOT}/cmake)
include(cmake/utils.cmake)
include(cmake/common.cmake)

configure_file(${APP_ROOT}/cmake/config.h.cmake ${CMAKE_BINARY_DIR}/config.h)

add_subdirectory(api)
add_subdirectory(api-tests)
add_subdirectory(gui)
