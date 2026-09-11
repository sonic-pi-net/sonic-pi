cmake_minimum_required(VERSION 3.15)
project(sonic-pi-api VERSION 0.0.0.1)

set(API_ROOT "${CMAKE_CURRENT_LIST_DIR}")
set(VENDOR_DIR "${API_ROOT}/vendor")

set(KISSFFT_TEST OFF CACHE BOOL "Disable kissfft tests" FORCE)
# Tools (fft, fastconv, psdpng) — Sonic Pi only links the library.
# psdpng pulls a pkg_check_modules(libpng) call which CMake 4.x makes
# a hard error when pkg-config isn't installed (Windows default).
set(KISSFFT_TOOLS OFF CACHE BOOL "Disable kissfft tools" FORCE)
set(REPROC++ ON CACHE BOOL "Build reproc++" FORCE)
# Sonic Pi links reproc directly; its headers, static libs and cmake/pkgconfig
# files have no place in any install tree.
set(REPROC_INSTALL OFF CACHE BOOL "Disable reproc install rules" FORCE)

# Update these if vendored versions change.
set(KISSFFT_VERSION "kissfft-131.1.0" CACHE INTERNAL "Version string for KissFFT")
set(REPROC_VERSION "reproc-14.2.5" CACHE INTERNAL "Version string for Reproc")
set(PLATFORMFOLDERS_VERSION "PlatformFolders-4.2.0" CACHE INTERNAL "Version string for PlatformFolders")
set(KISSNET_VERSION "kissnet-master-34b751b" CACHE INTERNAL "Version string for Kissnet")
set(GHC_FILESYSTEM_VERSION "ghc_filesystem" CACHE INTERNAL "Version string for ghc_filesystem")
set(TLSF_VERSION "TLSF-2.4.6" CACHE INTERNAL "Version string for TLSF")

if(WIN32)
    # KissFFT DLL path. $<CONFIG> is resolved at build time, so this
    # works correctly under multi-config generators (Visual Studio)
    # where CMAKE_BUILD_TYPE is empty at configure time.
    set(API_VENDORED_KISSFFT_DLL "${CMAKE_SOURCE_DIR}/build/${KISSFFT_VERSION}/$<CONFIG>/kissfft-float.dll" PARENT_SCOPE)
endif()

# Specify unique binary directories for each vendored project.
# PlatformFolders is only needed for the Windows home directory lookup; the
# other platforms read the password database directly.
if(WIN32)
    add_subdirectory("${VENDOR_DIR}/${PLATFORMFOLDERS_VERSION}" "${CMAKE_BINARY_DIR}/${PLATFORMFOLDERS_VERSION}")
endif()
add_subdirectory("${VENDOR_DIR}/${REPROC_VERSION}" "${CMAKE_BINARY_DIR}/${REPROC_VERSION}")
add_subdirectory("${VENDOR_DIR}/${KISSFFT_VERSION}" "${CMAKE_BINARY_DIR}/${KISSFFT_VERSION}")

add_library(ghc_filesystem INTERFACE)
target_include_directories(ghc_filesystem INTERFACE "${VENDOR_DIR}/ghc_filesystem/include")

add_library(kissnet INTERFACE)
target_include_directories(kissnet INTERFACE "${VENDOR_DIR}/${KISSNET_VERSION}")

set(API_SRC
    ${API_ROOT}/src/sonicpi_api.cpp
    ${API_ROOT}/src/string_utils.cpp
    ${API_ROOT}/src/file_utils.cpp

    ${API_ROOT}/include/api/string_utils.h
    ${API_ROOT}/include/api/file_utils.h
    ${API_ROOT}/include/api/logger.h

    ${VENDOR_DIR}/${TLSF_VERSION}/src/tlsf.c
)

set(OSC_SRC
    ${API_ROOT}/src/osc/osc_handler.cpp
    ${API_ROOT}/src/osc/osc_sender.cpp
    ${API_ROOT}/src/osc/tcp_osc_sender.cpp
    ${API_ROOT}/src/osc/osc_server.cpp
    ${API_ROOT}/src/osc/tcp_osc_server.cpp
    ${API_ROOT}/src/osc/udp_osc_server.cpp

    ${API_ROOT}/include/api/osc/osc_handler.h
    ${API_ROOT}/include/api/osc/osc_sender.h
    ${API_ROOT}/include/api/osc/osc_server.h
    ${API_ROOT}/include/api/osc/tcp_osc_server.h
    ${API_ROOT}/include/api/osc/udp_osc_server.h
    ${API_ROOT}/include/api/osc/osc_pkt.hh
    ${API_ROOT}/include/api/osc/udp.hh
)

set(BUFFER_SRC
    ${API_ROOT}/src/audio/audio_processor.cpp
    ${API_ROOT}/include/api/audio/audio_processor.h
    ${API_ROOT}/include/api/audio/shm_audio_buffer.hpp
    ${API_ROOT}/include/api/audio/server_shm.hpp
)

# Create the static library target
include(${CMAKE_CURRENT_LIST_DIR}/../cmake/supersonic.cmake)

add_library(${PROJECT_NAME} STATIC ${API_SRC} ${OSC_SRC} ${BUFFER_SRC})
add_library(SonicPi::API ALIAS ${PROJECT_NAME})



# Include directories
target_include_directories(${PROJECT_NAME}
    PUBLIC
        ${API_ROOT}/include

        # The shm segment layout + reader come straight from the engine tree
        # (the api/audio shm headers are thin shims over these) — one source
        # of truth, no hand-synced mirror to drift. The layout is checked
        # against the engine's self-described geometry when the segment is
        # opened, so a stale reader fails loudly rather than reading garbage.
        # clockwork/src: the substrate's own tree inside the engine checkout,
        # where shm_segment.hpp lives. dsp/scsynth: SuperSonic's own, where
        # node_tree.h says what the guest publishes in its window.
        ${SUPERSONIC_SOURCE_DIR}/clockwork/src
        ${SUPERSONIC_SOURCE_DIR}/dsp/scsynth

        ${VENDOR_DIR}/${GHC_FILESYSTEM_VERSION}/include
        ${VENDOR_DIR}/${KISSNET_VERSION}
        ${VENDOR_DIR}/${REPROC_VERSION}/reproc++/include
        ${VENDOR_DIR}/${KISSFFT_VERSION}

    PRIVATE
        ${VENDOR_DIR}/${TLSF_VERSION}/src
)

# Link to the vendored targets
target_link_libraries(${PROJECT_NAME}
    PUBLIC
        reproc
        reproc++
        kissfft
)

if(WIN32)
    target_include_directories(${PROJECT_NAME} PUBLIC ${VENDOR_DIR}/${PLATFORMFOLDERS_VERSION})
    target_link_libraries(${PROJECT_NAME} PUBLIC sago::platform_folders)
endif()

# Platform-specific link libraries
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    target_link_libraries(${PROJECT_NAME}
        PUBLIC
            stdc++fs
            rt
    )
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    # Windows-specific libraries, if any
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    # macOS-specific libraries, if any
endif()

target_compile_definitions(${PROJECT_NAME} PUBLIC
    # The segment is named "<product>_<port>" by the engine, and the reader
    # spells the product the same way or opens nothing.
    #
    # HARD-CODED to match the engine's COMPILED product name. The engine gets
    # its name via ExternalProject_Add's -DCLOCKWORK_PRODUCT_NAME, but the
    # spaces in SUPERSONIC_PRODUCT_NAME ("Sonic Pi - SuperSonic") do not survive
    # that hand-off, so the engine falls back to its own default,
    # "SuperSonic". The reader must spell it identically or the shm
    # client attaches to nothing and every shm panel (metrics, scope, node
    # tree) is blank. This is a stopgap: the durable fix is to carry the segment
    # identity through the connect handshake so no side reconstructs it from a
    # build-time constant.
    CLOCKWORK_PRODUCT_NAME="SuperSonic"
    _CRT_SECURE_NO_WARNINGS
    _WINSOCK_DEPRECATED_NO_WARNINGS
    WIN32_LEAN_AND_MEAN
)

# Organize source groups (optional)
source_group("Api" FILES ${API_SRC})
source_group("Api\\Osc" FILES ${OSC_SRC})
source_group("Api\\Audio" FILES ${BUFFER_SRC})
