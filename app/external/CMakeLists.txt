# This CMakeLists builds all the external libraries required for a Sonic Pi
cmake_minimum_required(VERSION 3.13)

message(STATUS "Externals Builder")

project(ExternalsBuilder)

option(USE_SYSTEM_LIBS "Use system libraries instead of bundled libraries when building" OFF)

message(STATUS "USE_SYSTEM_LIBS: ${USE_SYSTEM_LIBS}")

# Set default build type for sp_midi and sp_link
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release")
endif()
message(STATUS "CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")

include(GNUInstallDirs)
include(ExternalProject)

set(CMAKE_OSX_DEPLOYMENT_TARGET "13")

# sp_midi
ExternalProject_Add(sp_midi
    PREFIX sp_midi-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/sp_midi
    INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../server/beam/tau/priv
    CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_SOURCE_DIR}/../server/beam/tau/priv
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
        -DUSE_SYSTEM_RTMIDI=${USE_SYSTEM_LIBS}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config ${CMAKE_BUILD_TYPE}
    )

ExternalProject_Add(sp_link
    PREFIX sp_link-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/sp_link
    INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../server/beam/tau/priv
    CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_SOURCE_DIR}/../server/beam/tau/priv
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config ${CMAKE_BUILD_TYPE}
    )

if(NOT USE_SYSTEM_LIBS)
    find_package(SndFile CONFIG REQUIRED)

    ExternalProject_Add(aubio-project
        PREFIX aubio-prefix
        SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/aubio-0.4.9

        INSTALL_DIR ${CMAKE_BINARY_DIR}/aubio-package

        CMAKE_ARGS
            -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/aubio-package
            # propagate vcpkg information
            -DVCPKG_TARGET_TRIPLET=${VCPKG_TARGET_TRIPLET}
            -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
        )
endif()

# Build aubio_onset for server
add_executable(aubio_onset
    ${CMAKE_CURRENT_LIST_DIR}/aubio/aubioonset.c
    ${CMAKE_CURRENT_LIST_DIR}/aubio/utils.c)
if(USE_SYSTEM_LIBS)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(aubio REQUIRED IMPORTED_TARGET aubio)
    target_link_libraries(aubio_onset
        PRIVATE
        PkgConfig::aubio)
else()
    add_dependencies(aubio_onset aubio-project)
    target_include_directories(aubio_onset PRIVATE ${CMAKE_BINARY_DIR}/aubio-package/include)
    target_link_directories(aubio_onset PRIVATE ${CMAKE_BINARY_DIR}/aubio-package/lib)
    target_link_libraries(aubio_onset
        PRIVATE
        aubio
        SndFile::sndfile)
endif()
if(UNIX)
    target_link_libraries(aubio_onset
        PRIVATE
        m)
endif()

# Kick off a generation by making a dummy/empty project
add_library(ExternalsBuilder STATIC externals.cpp)

# Dependency ensures the externals are built
add_dependencies(ExternalsBuilder aubio_onset)

# Copy prebuilt files
add_custom_command(
    TARGET aubio_onset POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:aubio_onset> ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
)

if(WIN32)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64-bit
      add_custom_command(
        TARGET ExternalsBuilder PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/../../prebuilt/windows/x64/ ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
      )
  elseif(CMAKE_SIZEOF_VOID_P EQUAL 4) # 32-bit
      add_custom_command(
        TARGET ExternalsBuilder PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/../../prebuilt/windows/x86/ ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
      )
  endif()
elseif(${CMAKE_SYSTEM_NAME} MATCHES Darwin) # macOS
  add_custom_command(
    TARGET ExternalsBuilder PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/../../prebuilt/macos/universal/ ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
    COMMAND ${CMAKE_COMMAND} -E create_symlink supercollider/Resources/scsynth ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/scsynth
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/supercollider/Resources/extra-plugins/ ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/supercollider/Resources/plugins/
    COMMAND ${CMAKE_COMMAND} -E rm -rf ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/supercollider/Resources/extra-plugins
  )
endif()
