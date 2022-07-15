project (sp_link)
cmake_minimum_required (VERSION 3.0)

set(CMAKE_VERBOSE_MAKEFILE ON)

if(NOT MSVC)
    if(APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -stdlib=libc++")
    else(APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
    endif(APPLE)
endif(NOT MSVC)


# If we need to change something based on this running on CI, we can use if(DEFINED ENV{GITHUB_ACTION})
if(APPLE)
    set(ERLANG_INCLUDE_PATH "/usr/local/lib/erlang/usr/include" CACHE PATH "Path to erlang includes")
elseif(UNIX)
    set(ERLANG_INCLUDE_PATH "/usr/lib/erlang/usr/include" CACHE PATH "Path to erlang includes")
elseif(MSVC)
    if(DEFINED ENV{GITHUB_ACTION})
        set(ERLANG_INCLUDE_PATH "C:/Program Files/erl10.7/usr/include" CACHE PATH "Path to erlang includes")
    else()
        set(ERLANG_INCLUDE_PATH "C:/Program Files/erl-24.0/usr/include" CACHE PATH "Path to erlang includes")
    endif()
endif(APPLE)


# For Link
include(${PROJECT_SOURCE_DIR}/external_libs/link/AbletonLinkConfig.cmake)
#target_link_libraries(libsp_link Ableton::Link)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(WIN32)
 include_directories(${PROJECT_SOURCE_DIR}/external_libs/spdlog-1.8.2/include)
elseif(APPLE)
 include_directories(${PROJECT_SOURCE_DIR}/external_libs/spdlog-1.8.2/include)
else()
 include_directories(${PROJECT_SOURCE_DIR}/external_libs/spdlog-1.8.2/include)
endif()

set(sp_link_sources
    src/sp_link.cpp
    src/sp_link_nifs.cpp
    src/sp_link_nif_callbacks.cpp
)

# sp_link_sources
add_library(libsp_link SHARED ${sp_link_sources})
SET_TARGET_PROPERTIES(libsp_link PROPERTIES PREFIX "")

#check if armv7l architecture (Raspberry Pi OS 32bit) and add atomic linking if so
#if (${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "armv7l")
#    message(STATUS("linking atomic for armv7l architecture"))
#    target_link_libraries(libsp_link atomic)
#endif()

if(MSVC)
    add_definitions(-D_WIN32_WINNT=0x0600)
    include_directories(${ERLANG_INCLUDE_PATH})
    target_link_libraries(libsp_link Ableton::Link)
    # example test exe. Only under Windows, because on the others, the NIF functions are resolved when linked to the erlang VM, not on the library
    add_executable(sp_link_test src/sp_link_test.c)
    target_link_libraries(sp_link_test libsp_link)
elseif(APPLE)
    add_definitions(-DNDEBUG=1)
    include_directories(${ERLANG_INCLUDE_PATH})
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -undefined suppress -flat_namespace")
    set_target_properties(libsp_link PROPERTIES XCODE_ATTRIBUTE_CLANG_LINK_OBJC_RUNTIME "NO")
#    set(CMAKE_EXE_LINKER_FLAGS "-framework CoreMIDI -framework CoreAudio -framework CoreFoundation -framework Accelerate -framework QuartzCore -framework AudioToolbox -framework IOKit -framework DiscRecording -framework Cocoa")
    target_link_libraries(libsp_link Ableton::Link "-framework CoreMIDI -framework CoreAudio -framework CoreFoundation -framework Accelerate -framework QuartzCore -framework AudioToolbox -framework IOKit -framework DiscRecording -framework Cocoa")
elseif(UNIX)
    add_definitions(-DLINUX=1 -DNDEBUG=1)
    include_directories(${ERLANG_INCLUDE_PATH})
    target_link_libraries(libsp_link Ableton::Link)
endif(MSVC)

if(APPLE)
  install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/libsp_link.dylib DESTINATION ${CMAKE_INSTALL_PREFIX} RENAME libsp_link.so)
elseif(MSVC)
  install(TARGETS libsp_link RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})
else()
  install(TARGETS libsp_link LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX})
endif()
