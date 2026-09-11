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

# clockwork's distro switches (system zlib and stb, cargo offline, an
# installed bridge directory — the ones packaging/debian/rules sets) and the
# FetchContent offline controls only act inside the nested build, so forward
# any that were set on this configure.
set(SUPERSONIC_FORWARD_ARGS "")
foreach(opt
    CLOCKWORK_SYSTEM_ZLIB
    CLOCKWORK_SYSTEM_STB
    CLOCKWORK_CARGO_OFFLINE
    CLOCKWORK_PLUGIN_BRIDGE_DIR
    FETCHCONTENT_FULLY_DISCONNECTED
    FETCHCONTENT_SOURCE_DIR_ABLETONLINK)
    if(DEFINED ${opt})
        list(APPEND SUPERSONIC_FORWARD_ARGS "-D${opt}=${${opt}}")
    endif()
endforeach()

# SuperSonic — BUILD_ALWAYS ensures source changes are picked up
# without needing to manually delete stamp files.
# SOURCE_DIR is the supersonic submodule (see cmake/supersonic.cmake, which
# also names the engine and its bridge). It is scsynth hosted on clockwork
# (AGPL-3.0-or-later, or a commercial licence — see clockwork/LICENSE), which
# is what brings VST3 hosting with it, and it builds its own `clockwork`
# submodule: a recursive checkout is required, not just a shallow one.
#
# The option names are clockwork's: the product name is CLOCKWORK_PRODUCT_NAME
# and plugin hosting is CLOCKWORK_PLUGINS.
include(${CMAKE_CURRENT_LIST_DIR}/../cmake/supersonic.cmake)

# VST3 + CLAP hosting: the whole reason for the engine swap, so ON here and
# named rather than inherited. It is an option, not a constant, because the
# SDKs are fetched at configure time and Debian ships neither: the package
# build (packaging/debian/rules) turns it off, and with it goes the plugin
# bridge — the copy step below knows.
option(CLOCKWORK_PLUGINS "Host VST3 and CLAP plugins in the engine (fetches the SDKs at configure time)" ON)
#
# PREFIX and BINARY_DIR are deliberately SHORT, and on Windows that is not
# cosmetic. Left to itself ExternalProject builds under
# external/supersonic-prefix/src/supersonic-build — 47 characters — and the
# engine's own build then nests FetchContent under that: the VST3 SDK's
# populate stamp lands at
#   .../_deps/vst3_pluginterfaces-subbuild/vst3_pluginterfaces-populate-prefix
#   /src/vst3_pluginterfaces-populate-stamp/<config>/...-mkdir
# which reached 263 characters from a checkout in a home directory and failed
# the whole build with MSB4023, "exceeds the OS max path limit". MAX_PATH is
# 260, long paths are off by default on Windows, and MSBuild enforces the
# limit itself regardless. Those 47 characters become 4 here, which buys back
# the margin for everyone — the clone's own path is the part we do not
# control.
ExternalProject_Add(supersonic
    PREFIX ${CMAKE_BINARY_DIR}/ss
    SOURCE_DIR ${SUPERSONIC_SOURCE_DIR}
    BINARY_DIR ${CMAKE_BINARY_DIR}/ss/b
    DOWNLOAD_COMMAND ""
    BUILD_ALWAYS ON
    CMAKE_ARGS
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DBUILD_TESTS=OFF
        "-DCLOCKWORK_PRODUCT_NAME=${SUPERSONIC_PRODUCT_NAME}"
        # VST3 + CLAP hosting (the option above).
        -DCLOCKWORK_PLUGINS=${CLOCKWORK_PLUGINS}
        # Ableton Link. Sonic Pi has always shipped it (link_sync, the peer
        # count, the GUI's tempo scrub), and clockwork's default is OFF: an
        # engine built without it changed tempo as a session of one.
        -DCLOCKWORK_LINK=ON
        # The AUDIO half of Link, which is a separate opt-in and defaults OFF:
        # CLOCKWORK_LINK alone gives tempo, transport and peers (the clock's
        # LinkSession) and compiles the peer-audio renderer and endpoint out
        # entirely. Sonic Pi's Link audio streams widget has no engine behind it
        # without this, and the failure is silent — the streams simply never
        # appear, on a build whose banner used to claim Link-Audio anyway.
        -DCLOCKWORK_LINK_AUDIO=ON
        "-DCLOCKWORK_PLUGIN_BRIDGE_NAME=${SUPERSONIC_BRIDGE_NAME}"
        # The app icon, for the bridge's editor windows and the taskbar.
        "-DCLOCKWORK_PRODUCT_ICON=${CMAKE_CURRENT_SOURCE_DIR}/../gui/images/app.ico"
        # How macOS knows the bridge: what OBS's application capture and the
        # Syphon publisher (gui/platform/syphon_publisher.mm) select it by.
        -DCLOCKWORK_PLUGIN_BRIDGE_BUNDLE_ID=net.sonic-pi.plugins
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
# The binary sits directly in the build dir, not under SuperSonic_artefacts/.
# That path was JUCE's: the old project built the host through JUCE's
# add_executable wrapper, which interposes an artefacts directory. clockwork
# links the engine with a plain add_executable, so the binary lands where
# CMake normally puts one.
#
# "Where CMake normally puts one" is not one place, though. The nested build
# inherits this generator, and a multi-config generator — the Visual Studio one
# Sonic Pi uses on Windows — puts its artefacts under a per-config
# subdirectory, while Ninja and Makefiles put them straight in the build dir.
# Ask which shape this is rather than assuming, and name the config the
# ExternalProject is actually built with above (--config ${CMAKE_BUILD_TYPE}),
# so the two cannot drift apart.
if(CMAKE_CONFIGURATION_TYPES)
    set(SUPERSONIC_OUT_DIR "${BINARY_DIR}/${CMAKE_BUILD_TYPE}")
else()
    set(SUPERSONIC_OUT_DIR "${BINARY_DIR}")
endif()
#
# The piano wavetable is OURS (external/piano — see its README). The engine
# carries no table at all: MdaPiano is handed one at runtime through
# /supersonic/piano/wavetable, from a buffer the client loaded like any
# sample, so which table the piano plays is the client's business. The studio
# does that at boot (Studio#load_piano_wavetable), reading the file from
# beside the engine binary, which is what this copy puts there.
# Still guarded: a tree without the asset configures, and :piano is silent.
set(_ss_piano "${CMAKE_CURRENT_LIST_DIR}/piano/piano_wavetable.dat")
if(EXISTS "${_ss_piano}")
    set(_ss_piano_cmd COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${_ss_piano}" ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/piano_wavetable.dat)
else()
    set(_ss_piano_cmd "")
endif()

# The bridge is built under its shipped name already (clockwork sets
# OUTPUT_NAME from CLOCKWORK_PLUGIN_BRIDGE_NAME) and lands beside the engine binary,
# so it is copied as it is. It must sit beside the engine in server/native/
# too: that is where the engine looks for it. On macOS it is an app bundle,
# "Sonic Pi - Plugins.app", so the process has a bundle identifier (OBS's
# application capture and the Syphon publisher select it by that); the
# directory is copied whole, and a flat binary left by an older build is
# removed so the engine cannot pick the stale one.
#
# No plugins, no bridge: clockwork only builds it under CLOCKWORK_PLUGINS, so a
# build with hosting off (the Debian package) has nothing to copy.
if(WIN32)
    set(SUPERSONIC_BRIDGE_FILE "${SUPERSONIC_BRIDGE_NAME}.exe")
elseif(APPLE)
    set(SUPERSONIC_BRIDGE_FILE "${SUPERSONIC_BRIDGE_NAME}.app")
else()
    set(SUPERSONIC_BRIDGE_FILE "${SUPERSONIC_BRIDGE_NAME}")
endif()
if(NOT CLOCKWORK_PLUGINS)
    set(_ss_bridge_cmd "")
elseif(APPLE)
    set(_ss_bridge_cmd
        COMMAND ${CMAKE_COMMAND} -E rm -f
            "${CMAKE_CURRENT_SOURCE_DIR}/../server/native/${SUPERSONIC_BRIDGE_NAME}"
        COMMAND ${CMAKE_COMMAND} -E copy_directory
            "${SUPERSONIC_OUT_DIR}/${SUPERSONIC_BRIDGE_FILE}"
            "${CMAKE_CURRENT_SOURCE_DIR}/../server/native/${SUPERSONIC_BRIDGE_FILE}")
else()
    set(_ss_bridge_cmd
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${SUPERSONIC_OUT_DIR}/${SUPERSONIC_BRIDGE_FILE}"
            "${CMAKE_CURRENT_SOURCE_DIR}/../server/native/${SUPERSONIC_BRIDGE_FILE}")
endif()

add_custom_target(copy_supersonic ALL
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${SUPERSONIC_OUT_DIR}/${SUPERSONIC_BUILD_NAME}
        ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/${SUPERSONIC_NATIVE_NAME}
    ${_ss_bridge_cmd}
    ${_ss_piano_cmd}
    COMMENT "Copying SuperSonic, the plugin bridge and the piano wavetable to server/native/"
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
            ${SUPERSONIC_OUT_DIR}/SuperSonic.pdb
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

