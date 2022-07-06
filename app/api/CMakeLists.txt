project(sonic-pi-api VERSION 0.0.0.1)

set(API_ROOT ${CMAKE_CURRENT_LIST_DIR})

set(API_SRC
    ${API_ROOT}/src/sonicpi_api.cpp
    ${API_ROOT}/src/string_utils.cpp
    ${API_ROOT}/src/file_utils.cpp

    ${API_ROOT}/include/api/string_utils.h
    ${API_ROOT}/include/api/file_utils.h
    ${API_ROOT}/include/api/logger.h

    ${APP_ROOT}/external/TLSF-2.4.6/src/tlsf.c
)

set(OSC_SRC
    # Osc
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
    ${API_ROOT}/include/api/audio/scope_buffer.hpp
    ${API_ROOT}/include/api/audio/server_shm.hpp)

add_library(${PROJECT_NAME} STATIC ${API_SRC} ${OSC_SRC} ${BUFFER_SRC})
add_library(SonicPi::API ALIAS ${PROJECT_NAME})

# For finding home path, etc.
find_package(platform_folders CONFIG REQUIRED)

# For running child processes
find_package(reproc CONFIG REQUIRED)
find_package(reproc++ CONFIG REQUIRED)

if (USE_SYSTEM_LIBS)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(kissfft REQUIRED IMPORTED_TARGET kissfft-float)
else()
    find_package(unofficial-kissfft CONFIG REQUIRED)
endif()

target_include_directories(${PROJECT_NAME}
    PUBLIC
        ${API_ROOT}/include
        ${APP_ROOT}/external/ghc_filesystem/include
        ${APP_ROOT}/external/kissnet-master-34b751b
    PRIVATE
        ${APP_ROOT}/external
        ${APP_ROOT}/external/scsynth-boost-1.74.0
        ${APP_ROOT}/external/TLSF-2.4.6/src

    )

target_link_libraries(${PROJECT_NAME}
    PRIVATE
        sago::platform_folders
        reproc
        reproc++
)

if (USE_SYSTEM_LIBS)
    target_link_libraries(${PROJECT_NAME}
        PRIVATE
            PkgConfig::kissfft
    )
else()
    target_link_libraries(${PROJECT_NAME}
        PRIVATE
            unofficial::kissfft::kissfft
    )
endif()

# Windows
if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
find_package(crossguid CONFIG REQUIRED)
target_link_libraries(${PROJECT_NAME}
    PRIVATE
        crossguid
    )
endif()

# Linux
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
find_package(crossguid CONFIG REQUIRED)
target_link_libraries(${PROJECT_NAME}
    PUBLIC
        stdc++fs
        rt
    PRIVATE
        crossguid
    )
endif()

# Mac
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
target_link_libraries(${PROJECT_NAME}
    PRIVATE
    )
endif()

target_compile_definitions(${PROJECT_NAME} PUBLIC _CRT_SECURE_NO_WARNINGS _WINSOCK_DEPRECATED_NO_WARNINGS BOOST_DATE_TIME_NO_LIB WIN32_LEAN_AND_MEAN)

source_group ("Api" FILES ${API_SRC})
source_group ("Api\\Osc" FILES ${OSC_SRC})
source_group ("Api\\Audio" FILES ${BUFFER_SRC})
