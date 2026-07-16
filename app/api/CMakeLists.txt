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
add_subdirectory("${VENDOR_DIR}/${PLATFORMFOLDERS_VERSION}" "${CMAKE_BINARY_DIR}/${PLATFORMFOLDERS_VERSION}")
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
    ${API_ROOT}/include/api/audio/relative_ptr.hpp
)

# Create the static library target
add_library(${PROJECT_NAME} STATIC ${API_SRC} ${OSC_SRC} ${BUFFER_SRC})
add_library(SonicPi::API ALIAS ${PROJECT_NAME})



# Include directories
target_include_directories(${PROJECT_NAME}
    PUBLIC
        ${API_ROOT}/include

        # The shm segment layout + reader come straight from supersonic (the
        # api/audio shm headers are thin shims over these) — one source of
        # truth, no hand-synced mirror to drift.
        ${API_ROOT}/../external/supersonic
        ${API_ROOT}/../external/supersonic/src

        ${VENDOR_DIR}/${GHC_FILESYSTEM_VERSION}/include
        ${VENDOR_DIR}/${KISSNET_VERSION}
        ${VENDOR_DIR}/${REPROC_VERSION}/reproc++/include
        ${VENDOR_DIR}/${PLATFORMFOLDERS_VERSION}
        ${VENDOR_DIR}/${KISSNET_VERSION}
        ${VENDOR_DIR}/${KISSFFT_VERSION}

    PRIVATE
        ${VENDOR_DIR}/${TLSF_VERSION}/src
)

# Link to the vendored targets
target_link_libraries(${PROJECT_NAME}
    PUBLIC
        sago::platform_folders
        reproc
        reproc++
        kissfft
)

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
    _CRT_SECURE_NO_WARNINGS
    _WINSOCK_DEPRECATED_NO_WARNINGS
    WIN32_LEAN_AND_MEAN
)

# Organize source groups (optional)
source_group("Api" FILES ${API_SRC})
source_group("Api\\Osc" FILES ${OSC_SRC})
source_group("Api\\Audio" FILES ${BUFFER_SRC})
