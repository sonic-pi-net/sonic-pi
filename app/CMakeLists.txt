cmake_minimum_required(VERSION 3.13)
set(APP_ROOT ${CMAKE_CURRENT_LIST_DIR})

# Read canonical version from /VERSION (one repo-root file, also read by
# Ruby runtime). Strip any dev/RC suffix so PROJECT_VERSION stays X.Y.Z
# for CMake/macOS bundle plumbing; the full string goes to config.h.
file(READ "${APP_ROOT}/../VERSION" SONIC_PI_VERSION)
string(STRIP "${SONIC_PI_VERSION}" SONIC_PI_VERSION)
string(REGEX MATCH "^[0-9]+\\.[0-9]+\\.[0-9]+" SONIC_PI_VERSION_NUMERIC "${SONIC_PI_VERSION}")

# Different triplet for windows
if (WIN32)
    if(CMAKE_SYSTEM_PROCESSOR STREQUAL "ARM64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64")
        set(VCPKG_TARGET_TRIPLET arm64-windows-static-md CACHE STRING "triplet")
    else()
        set(VCPKG_TARGET_TRIPLET x64-windows-static-md CACHE STRING "triplet")
    endif()
endif()

# vcpkg toolchain - Must be declared before project

if (WIN32 OR APPLE)
  set(CMAKE_TOOLCHAIN_FILE ${APP_ROOT}/vcpkg/scripts/buildsystems/vcpkg.cmake CACHE STRING "Vcpkg toolchain file")
endif()

project(SonicPi)

message(STATUS "CMakeLists: Sonic Pi")



# Sonic Pi Requires a C++ 17 Compiler to build it
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT ${PROJECT_NAME})

if(APPLE)
  # Must stay in sync with LSMinimumSystemVersion in mac-release-03-info-plist.sh
  # and the MACOSX_DEPLOYMENT_TARGET export in mac-build-all.sh. The release
  # pipeline's stage 08 compat audit fails if any bundled Mach-O's minos
  # exceeds this floor.
  set(CMAKE_OSX_DEPLOYMENT_TARGET "14")
endif()


if (NOT APP_INSTALL_ROOT)
    set(APP_INSTALL_ROOT ${APP_ROOT})
endif()

list(APPEND CMAKE_MODULE_PATH ${APP_ROOT}/cmake)
include(cmake/utils.cmake)
include(cmake/common.cmake)

configure_file(${APP_ROOT}/cmake/config.h.cmake ${CMAKE_BINARY_DIR}/config.h)

add_subdirectory(external)
# Vendored Spout2 — Windows-only sibling to external/Syphon-Framework.
if(WIN32)
    add_subdirectory(external/Spout2)
endif()
add_subdirectory(api)
add_subdirectory(api-tests)
add_subdirectory(gui)

option(BUILD_SPOUT_SMOKETEST "Build tools/spout-receiver smoke-test (Windows)" OFF)
if(WIN32 AND BUILD_SPOUT_SMOKETEST)
    add_subdirectory(tools/spout-receiver)
endif()
