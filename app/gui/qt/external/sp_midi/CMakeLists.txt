project (sp_midi)
cmake_minimum_required (VERSION 3.0)

set(CMAKE_VERBOSE_MAKEFILE ON)

if(NOT MSVC)
    if(APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -stdlib=libc++")
    else(APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
    endif(APPLE)
endif(NOT MSVC)

if(APPLE)
    set(ERLANG_INCLUDE_PATH "/usr/local/lib/erlang/usr/include" CACHE PATH "Path to erlang includes")
elseif(UNIX)
    find_package(ALSA REQUIRED)
elseif(MSVC)
    set(ERLANG_INCLUDE_PATH "C:/Program Files/erl-23.0/usr/include" CACHE PATH "Path to erlang includes")
endif(APPLE)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

#set(oscpack_path ${PROJECT_SOURCE_DIR}/external_libs/oscpack_1_1_0)
#add_subdirectory(${oscpack_path})
#don't include ${PROJECT_SOURCE_DIR}/external_libs globally as it affects linux builds where it is not wanted/ Add instead just for MSCV and APPLE builds
#include_directories( ${PROJECT_SOURCE_DIR}/external_libs/spdlog-0.11.0/include JuceLibraryCode JuceLibraryCode/modules ${PROJECT_SOURCE_DIR}/external_libs/cxxopts ${PROJECT_SOURCE_DIR}/external_libs)
include_directories(${PROJECT_SOURCE_DIR}/external_libs/spdlog-0.11.0/include JuceLibraryCode JuceLibraryCode/modules ${PROJECT_SOURCE_DIR}/external_libs/cxxopts)


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
    include_directories(${PROJECT_SOURCE_DIR}/external_libs)
    add_definitions(-D__WINDOWS_MM__)
endif(MSVC)

if(APPLE)
    list(APPEND sp_midi_sources ${PROJECT_SOURCE_DIR}/external_libs/rtmidi/RtMidi.cpp)
    include_directories(${PROJECT_SOURCE_DIR}/external_libs)
    add_definitions(-D__MACOSX_CORE__)
endif(APPLE)

if(APPLE)
    set(juce_sources
        JuceLibraryCode/include_juce_core.mm
        JuceLibraryCode/include_juce_events.mm
    )
else(APPLE)
    set(juce_sources
        JuceLibraryCode/include_juce_core.cpp
        JuceLibraryCode/include_juce_events.cpp
    )
endif(APPLE)

# sp_midi_sources
add_library(libsp_midi SHARED ${sp_midi_sources} ${juce_sources})
SET_TARGET_PROPERTIES(libsp_midi PROPERTIES PREFIX "")

#check if armv7l architecture (Raspberry Pi OS 32bit) and add atomic linking if so
if (${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "armv7l")
    message(STATUS("linking atomic for armv7l architecture"))
    target_link_libraries(libsp_midi atomic)
endif()

add_definitions(-DJUCE_ALSA_MIDI_NAME="sp_midi")
add_definitions(-DJUCE_APP_CONFIG_HEADER="AppConfig.h")

if(MSVC)
    add_definitions(-D_WIN32_WINNT=0x0600 -DJUCER_VS2015_78A5022=1 -DJUCE_APP_VERSION=1.0.0 -DJUCE_APP_VERSION_HEX=0x10000)
    include_directories(${ERLANG_INCLUDE_PATH})
    target_link_libraries(libsp_midi winmm)
    # example test exe. Only under Windows, because on the others, the NIF functions are resolved when linked to the erlnag VM, not on the library
    add_executable(sp_midi_test src/sp_midi_test.c)
    target_link_libraries(sp_midi_test libsp_midi)
elseif(APPLE)
    add_definitions(-DNDEBUG=1 -DJUCER_XCODE_MAC_F6D2F4CF=1 -DJUCE_APP_VERSION=1.0.0 -DJUCE_APP_VERSION_HEX=0x10000)
    include_directories(${ERLANG_INCLUDE_PATH})
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -undefined suppress -flat_namespace")
    set_target_properties(libsp_midi PROPERTIES XCODE_ATTRIBUTE_CLANG_LINK_OBJC_RUNTIME "NO")
#    set(CMAKE_EXE_LINKER_FLAGS "-framework CoreMIDI -framework CoreAudio -framework CoreFoundation -framework Accelerate -framework QuartzCore -framework AudioToolbox -framework IOKit -framework DiscRecording -framework Cocoa")
    target_link_libraries(libsp_midi "-framework CoreMIDI -framework CoreAudio -framework CoreFoundation -framework Accelerate -framework QuartzCore -framework AudioToolbox -framework IOKit -framework DiscRecording -framework Cocoa")
elseif(UNIX)
    add_definitions(-DLINUX=1 -DNDEBUG=1 -DJUCER_LINUX_MAKE_6D53C8B4=1 -DJUCE_APP_VERSION=1.0.0 -DJUCE_APP_VERSION_HEX=0x10000)
    target_link_libraries(libsp_midi pthread ${ALSA_LIBRARY} dl rtmidi)
endif(MSVC)
