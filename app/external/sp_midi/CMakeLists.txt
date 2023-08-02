project (sp_midi)
cmake_minimum_required (VERSION 3.0)

option(USE_SYSTEM_RTMIDI "Use system RtMidi library instead of built-in library (Linux only)" OFF)

set(CMAKE_VERBOSE_MAKEFILE ON)

if(NOT MSVC)
    if(APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -stdlib=libc++")
    else(APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
    endif(APPLE)
endif(NOT MSVC)


# If we need to change something based on this running on CI, we can use if(DEFINED ENV{GITHUB_ACTION})
if(WIN32)
  set(ERLANG_INCLUDE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/external_libs/erlang_headers/win_x86_64 CACHE STRING "Path to the Erlang header files for 64 bit Windows")
elseif(${CMAKE_SYSTEM_NAME} MATCHES Darwin) # macOS
  set(ERLANG_INCLUDE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/external_libs/erlang_headers/mac_x86_64 CACHE STRING "Path to the Erlang header files for 64 bit Intel Macs")
else()
  execute_process(
    COMMAND erl -noshell -eval "io:format(\"~s~n\", [filename:join([lists:concat([code:root_dir(), \"/erts-\", erlang:system_info(version)]), \"include\"])]), init:stop(0)."
    OUTPUT_VARIABLE FOUND_ERLANG_INCLUDE_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(ERLANG_INCLUDE_PATH ${FOUND_ERLANG_INCLUDE_PATH} CACHE STRING "Path to the Erlang header files")
  find_package(ALSA REQUIRED)
endif()

message(STATUS "ERLANG_INCLUDE_PATH: ${ERLANG_INCLUDE_PATH}")

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(WIN32)
    include_directories(${PROJECT_SOURCE_DIR}/external_libs/spdlog-1.8.2/include ${PROJECT_SOURCE_DIR}/external_libs/concurrentqueue ${PROJECT_SOURCE_DIR}/external_libs)
elseif(APPLE)
    include_directories(${PROJECT_SOURCE_DIR}/external_libs/spdlog-1.8.2/include ${PROJECT_SOURCE_DIR}/external_libs/concurrentqueue ${PROJECT_SOURCE_DIR}/external_libs)
else()
    include_directories(${PROJECT_SOURCE_DIR}/external_libs/spdlog-1.8.2/include ${PROJECT_SOURCE_DIR}/external_libs/concurrentqueue)
    if(NOT USE_SYSTEM_RTMIDI)
        include_directories(${PROJECT_SOURCE_DIR}/external_libs)
    endif(NOT USE_SYSTEM_RTMIDI)
endif()

set(sp_midi_sources
    src/sp_midi.cpp
    src/midiin.cpp
    src/midiout.cpp
    src/midicommon.cpp
    src/midisendprocessor.cpp
    src/utils.cpp
)

if(MSVC)
    list(APPEND sp_midi_sources ${PROJECT_SOURCE_DIR}/external_libs/rtmidi/RtMidi.cpp)
    add_definitions(-D__WINDOWS_MM__)
elseif(APPLE)
    list(APPEND sp_midi_sources ${PROJECT_SOURCE_DIR}/external_libs/rtmidi/RtMidi.cpp)
    add_definitions(-D__MACOSX_CORE__)
elseif(UNIX)
    if(NOT USE_SYSTEM_RTMIDI)
        list(APPEND sp_midi_sources ${PROJECT_SOURCE_DIR}/external_libs/rtmidi/RtMidi.cpp)
        add_definitions(-D__LINUX_ALSA__)
    endif(NOT USE_SYSTEM_RTMIDI)
endif(MSVC)

# sp_midi_sources
add_library(libsp_midi SHARED ${sp_midi_sources})
SET_TARGET_PROPERTIES(libsp_midi PROPERTIES PREFIX "")

#check if armv7l architecture (Raspberry Pi OS 32bit) and add atomic linking if so
if (${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "armv7l")
    message(STATUS("linking atomic for armv7l architecture"))
    target_link_libraries(libsp_midi atomic)
endif()

if(MSVC)
    add_definitions(-D_WIN32_WINNT=0x0600)
    include_directories(${ERLANG_INCLUDE_PATH})
    target_link_libraries(libsp_midi winmm)
    # example test exe. Only under Windows, because on the others, the NIF functions are resolved when linked to the erlang VM, not on the library
    add_executable(sp_midi_test src/sp_midi_test.c)
    target_link_libraries(sp_midi_test libsp_midi)
elseif(APPLE)
    add_definitions(-DNDEBUG=1)
    include_directories(${ERLANG_INCLUDE_PATH})
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -undefined suppress -flat_namespace")
    set_target_properties(libsp_midi PROPERTIES XCODE_ATTRIBUTE_CLANG_LINK_OBJC_RUNTIME "NO")
#    set(CMAKE_EXE_LINKER_FLAGS "-framework CoreMIDI -framework CoreAudio -framework CoreFoundation -framework Accelerate -framework QuartzCore -framework AudioToolbox -framework IOKit -framework DiscRecording -framework Cocoa")
    target_link_libraries(libsp_midi "-framework CoreMIDI -framework CoreAudio -framework CoreFoundation -framework Accelerate -framework QuartzCore -framework AudioToolbox -framework IOKit -framework DiscRecording -framework Cocoa")
elseif(UNIX)
    add_definitions(-DLINUX=1 -DNDEBUG=1)
    include_directories(${ERLANG_INCLUDE_PATH})
    target_link_libraries(libsp_midi pthread ${ALSA_LIBRARY} dl)
    if(USE_SYSTEM_RTMIDI)
        target_link_libraries(libsp_midi rtmidi)
    endif(USE_SYSTEM_RTMIDI)
endif(MSVC)

if(APPLE)
  install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/libsp_midi.dylib DESTINATION ${CMAKE_INSTALL_PREFIX} RENAME libsp_midi.so)
elseif(MSVC)
  install(TARGETS libsp_midi RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})
else()
  install(TARGETS libsp_midi LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX})
endif()
