# This CMakeLists builds all the external libraries required for a Tau.

# This is provided for the convinience of devs just wanting to work on Tau
# directly without building the full GUI.

# For the full Sonic Pi build, use the scripts in /app to not just build the
# extensions referenced here but also other extensions required by the GUI.

cmake_minimum_required(VERSION 3.13)

message(STATUS "External NIF Builder")

project(ExternalsBuilder)

option(USE_SYSTEM_LIBS "Use system libraries instead of bundled libraries when building" OFF)

message(STATUS "USE_SYSTEM_LIBS: ${USE_SYSTEM_LIBS}")

include(GNUInstallDirs)
include(ExternalProject)

set(CMAKE_OSX_DEPLOYMENT_TARGET '10.15')

# sp_midi
ExternalProject_Add(sp_midi
    PREFIX sp_midi-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/../../../external/sp_midi
    INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/priv
    CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_SOURCE_DIR}/priv
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
        -DUSE_SYSTEM_RTMIDI=${USE_SYSTEM_LIBS}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config Release
    )

ExternalProject_Add(sp_link
    PREFIX sp_link-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/../../../external/sp_link
    INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/priv
    CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_SOURCE_DIR}/priv
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config Release
    )