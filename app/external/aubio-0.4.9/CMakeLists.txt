cmake_minimum_required(VERSION 3.13)
project(aubio C)

add_definitions(
    -DHAVE_STDLIB_H=1
    -DHAVE_STDIO_H=1
    -DHAVE_MATH_H=1
    -DHAVE_STRING_H=1
    -DHAVE_LIMITS_H=1
    -DHAVE_STDARG_H=1
    -DHAVE_ERRNO_H=1
    -DHAVE_C99_VARARGS_MACROS=1
    -DHAVE_SNDFILE=1
    -DHAVE_WAVREAD=1
    -D_CRT_SECURE_NO_WARNINGS=1
)

find_package(SndFile CONFIG REQUIRED)

include_directories(src)

file(GLOB_RECURSE SOURCES src/*.c)

if(WIN32 AND NOT MINGW)
    set_source_files_properties(src/io/sink_wavwrite.c PROPERTIES COMPILE_FLAGS /FIWinsock2.h)
endif()

add_library(aubio ${SOURCES})

target_include_directories(aubio PUBLIC ${SNDFILE_INCLUDE_DIRS})
target_link_libraries(aubio PUBLIC ${SNDFILE_LIBRARIES})

if(WIN32)
    target_link_libraries(aubio PUBLIC ws2_32)
endif()

install(
    TARGETS aubio
    INCLUDES DESTINATION include
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(
    DIRECTORY src/
    DESTINATION include/aubio
    FILES_MATCHING
    PATTERN "*.h"
    PATTERN "*_priv.h" EXCLUDE
    PATTERN "config.h" EXCLUDE
)
