# This CMakeLists builds all the external libraries required for a Sonic Pi
cmake_minimum_required(VERSION 3.13)
set(APP_ROOT ${CMAKE_CURRENT_LIST_DIR})

# vcpkg toolchain - Must be declared before project

if (WIN32 OR APPLE)
  set(CMAKE_TOOLCHAIN_FILE ${APP_ROOT}/../vcpkg/scripts/buildsystems/vcpkg.cmake CACHE STRING "Vcpkg toolchain file")
endif()

message(STATUS "Externals Builder")

project(ExternalsBuilder)


# Set default build type for the external projects
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release")
endif()
message(STATUS "CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")

include(GNUInstallDirs)
include(ExternalProject)

set(CMAKE_OSX_DEPLOYMENT_TARGET "12")


if (WIN32 OR APPLE)
    # vcpkg's libsndfile port has shipped CMake config under both
    # share/SndFile/SndFileConfig.cmake and share/sndfile/sndfile-config.cmake
    # at different times; NAMES makes the lookup tolerant of either casing.
    find_package(SndFile NAMES SndFile sndfile CONFIG REQUIRED)

    ExternalProject_Add(aubio-project
        PREFIX aubio-prefix
        SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/aubio-0.4.9

        INSTALL_DIR ${CMAKE_BINARY_DIR}/aubio-package

        CMAKE_ARGS
            -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/aubio-package
            # propagate vcpkg information
            -DVCPKG_TARGET_TRIPLET=${VCPKG_TARGET_TRIPLET}
            -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
            # a nested project starts from a bare cache, so hardening and
            # reproducibility flags have to be handed down explicitly
            -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
            -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
        )
endif()

# Build aubio_onset for server
add_executable(aubio_onset
    ${CMAKE_CURRENT_LIST_DIR}/aubio/aubioonset.c
    ${CMAKE_CURRENT_LIST_DIR}/aubio/utils.c)
if (WIN32 OR APPLE)
    add_dependencies(aubio_onset aubio-project)
    target_include_directories(aubio_onset PRIVATE ${CMAKE_BINARY_DIR}/aubio-package/include)
    target_link_directories(aubio_onset PRIVATE ${CMAKE_BINARY_DIR}/aubio-package/lib)
    target_link_libraries(aubio_onset
        PRIVATE
        aubio
        SndFile::sndfile)
else()
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(aubio REQUIRED IMPORTED_TARGET aubio)
    target_link_libraries(aubio_onset
        PRIVATE
        PkgConfig::aubio)
endif()

if(UNIX)
    target_link_libraries(aubio_onset
        PRIVATE
        m)
endif()

# SuperSonic's system-library switches (used by distribution packagers) and
# the FetchContent offline controls only act inside the nested build, so
# forward any that were set on this configure.
set(SUPERSONIC_FORWARD_ARGS "")
foreach(opt
    SUPERSONIC_SYSTEM_JUCE
    SUPERSONIC_SYSTEM_SNDFILE
    SUPERSONIC_SYSTEM_BOOST
    SUPERSONIC_CARGO_OFFLINE
    FETCHCONTENT_FULLY_DISCONNECTED
    FETCHCONTENT_SOURCE_DIR_ABLETONLINK)
    if(DEFINED ${opt})
        list(APPEND SUPERSONIC_FORWARD_ARGS "-D${opt}=${${opt}}")
    endif()
endforeach()

# SuperSonic — BUILD_ALWAYS ensures source changes are picked up
# without needing to manually delete stamp files.
ExternalProject_Add(supersonic
    PREFIX supersonic-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/supersonic
    DOWNLOAD_COMMAND ""
    BUILD_ALWAYS ON
    CMAKE_ARGS
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DBUILD_TESTS=OFF
        -DBUILD_NIF=OFF
        # Published in OS-facing binary metadata (Windows version resource,
        # macOS embedded Info.plist) so consent prompts name Sonic Pi.
        "-DSUPERSONIC_PRODUCT_NAME=Sonic Pi - SuperSonic"
        # a nested project starts from a bare cache, so hardening and
        # reproducibility flags have to be handed down explicitly
        -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
        -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
        ${SUPERSONIC_FORWARD_ARGS}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config ${CMAKE_BUILD_TYPE} --target SuperSonic
    INSTALL_COMMAND ""
)

# Kick off a generation by making a dummy/empty project
add_library(ExternalsBuilder STATIC externals.cpp)

# Dependency ensures the externals are built
add_dependencies(ExternalsBuilder aubio_onset supersonic)

# Copy prebuilt files
add_custom_command(
    TARGET aubio_onset POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:aubio_onset> ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
)

# Copy SuperSonic binary to server/native/ after every build. It ships under
# a Sonic Pi name because the OS identifies the process by its executable —
# firewall consent prompts, Activity Monitor, ps, and PipeWire's ALSA-bridge
# node name all display it — and users should see Sonic Pi there.
ExternalProject_Get_Property(supersonic BINARY_DIR)
if(WIN32)
    set(SUPERSONIC_BUILD_NAME "SuperSonic.exe")
    set(SUPERSONIC_NATIVE_NAME "Sonic Pi - SuperSonic.exe")
elseif(APPLE)
    set(SUPERSONIC_BUILD_NAME "SuperSonic")
    set(SUPERSONIC_NATIVE_NAME "Sonic Pi - SuperSonic")
else()
    set(SUPERSONIC_BUILD_NAME "SuperSonic")
    set(SUPERSONIC_NATIVE_NAME "sonic-pi-supersonic")
endif()
add_custom_target(copy_supersonic ALL
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${BINARY_DIR}/SuperSonic_artefacts/${CMAKE_BUILD_TYPE}/${SUPERSONIC_BUILD_NAME}
        ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/${SUPERSONIC_NATIVE_NAME}
    # MdaPiano sample table — an optional SuperSonic asset, not compiled into the
    # engine. Copied next to the binary so the :piano synth works in the bundle;
    # if absent the engine just plays :piano silent.
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_CURRENT_LIST_DIR}/supersonic/assets/piano/piano_wavetable.dat
        ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/piano_wavetable.dat
    COMMENT "Copying SuperSonic binary + piano wavetable to server/native/"
    DEPENDS supersonic
)

# Keep the PDB next to the shipped engine exe so a WER crash dump from this
# build can be symbolicated. Not harvested into the MSI. Same file each build
# config — dbghelp resolves it by the GUID in the exe's debug directory and
# looks it up by the embedded name (SuperSonic.pdb, case-insensitive), which
# is independent of the exe's shipped filename.
if(WIN32)
    add_custom_command(TARGET copy_supersonic POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${BINARY_DIR}/SuperSonic_artefacts/${CMAKE_BUILD_TYPE}/SuperSonic.pdb
            ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/supersonic.pdb
        COMMENT "Copying SuperSonic PDB to server/native/"
        VERBATIM)
endif()

# Refresh the bundled MSVC runtime DLLs alongside the engine exe on every build.
# app/server/native/ is gitignored and persists across builds, so when the
# developer's MSVC toolchain updates (e.g. 14.29 -> 14.44) the freshly compiled
# binary picks up the new headers while stale bundled DLLs remain. Since the
# EXE directory wins DLL search order, supersonic loads the old MSVCP140.dll
# and crashes inside std::mutex::lock (mtx_do_lock AV) on first thread spawn.
if(WIN32)
    set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP TRUE)
    include(InstallRequiredSystemLibraries)
    add_custom_command(TARGET copy_supersonic POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
            ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
        COMMENT "Refreshing bundled MSVC runtime DLLs"
        VERBATIM)
endif()

