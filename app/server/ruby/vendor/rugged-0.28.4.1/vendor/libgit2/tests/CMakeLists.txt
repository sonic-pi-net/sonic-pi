FIND_PACKAGE(PythonInterp)

IF(NOT PYTHONINTERP_FOUND)
  MESSAGE(FATAL_ERROR "Could not find a python interpeter, which is needed to build the tests. "
    "Make sure python is available, or pass -DBUILD_CLAR=OFF to skip building the tests")
ENDIF()

SET(CLAR_FIXTURES "${CMAKE_CURRENT_SOURCE_DIR}/resources/")
SET(CLAR_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
ADD_DEFINITIONS(-DCLAR_FIXTURE_PATH=\"${CLAR_FIXTURES}\")
ADD_DEFINITIONS(-DCLAR_TMPDIR=\"libgit2_tests\")
ADD_DEFINITIONS(-D_FILE_OFFSET_BITS=64)

# Ensure that we do not use deprecated functions internally
ADD_DEFINITIONS(-DGIT_DEPRECATE_HARD)

INCLUDE_DIRECTORIES(${CLAR_PATH} ${libgit2_BINARY_DIR}/src)
FILE(GLOB_RECURSE SRC_TEST ${CLAR_PATH}/*/*.c ${CLAR_PATH}/*/*.h)
SET(SRC_CLAR "main.c" "clar_libgit2.c" "clar_libgit2_trace.c" "clar_libgit2_timer.c" "clar.c")

IF(MSVC_IDE)
	LIST(APPEND SRC_CLAR "precompiled.c")
ENDIF()

ADD_CUSTOM_COMMAND(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/clar.suite
	COMMAND ${PYTHON_EXECUTABLE} generate.py -o "${CMAKE_CURRENT_BINARY_DIR}" -f -xonline -xstress -xperf .
	DEPENDS ${SRC_TEST}
	WORKING_DIRECTORY ${CLAR_PATH}
)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

SET_SOURCE_FILES_PROPERTIES(
	${CLAR_PATH}/clar.c
	PROPERTIES OBJECT_DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/clar.suite)

INCLUDE_DIRECTORIES(${LIBGIT2_INCLUDES})
INCLUDE_DIRECTORIES(SYSTEM ${LIBGIT2_SYSTEM_INCLUDES})

ADD_EXECUTABLE(libgit2_clar ${SRC_CLAR} ${SRC_TEST} ${LIBGIT2_OBJECTS})

SET_TARGET_PROPERTIES(libgit2_clar PROPERTIES C_STANDARD 90)
SET_TARGET_PROPERTIES(libgit2_clar PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${libgit2_BINARY_DIR})

TARGET_INCLUDE_DIRECTORIES(libgit2_clar PRIVATE ../src PUBLIC ../include)
TARGET_LINK_LIBRARIES(libgit2_clar ${LIBGIT2_LIBS})
IDE_SPLIT_SOURCES(libgit2_clar)

IF (MSVC_IDE)
	# Precompiled headers
	SET_TARGET_PROPERTIES(libgit2_clar PROPERTIES COMPILE_FLAGS "/Yuprecompiled.h /FIprecompiled.h")
	SET_SOURCE_FILES_PROPERTIES("precompiled.c" COMPILE_FLAGS "/Ycprecompiled.h")
ENDIF ()

FUNCTION(ADD_CLAR_TEST name)
	IF (NOT USE_LEAK_CHECKER STREQUAL "OFF")
		ADD_TEST(${name} "${libgit2_SOURCE_DIR}/script/${USE_LEAK_CHECKER}.sh" "${libgit2_BINARY_DIR}/libgit2_clar" ${ARGN})
	ELSE()
		ADD_TEST(${name} "${libgit2_BINARY_DIR}/libgit2_clar" ${ARGN})
	ENDIF()
ENDFUNCTION(ADD_CLAR_TEST)

ADD_CLAR_TEST(offline   -v -xonline)
ADD_CLAR_TEST(invasive  -v -score::ftruncate -sfilter::stream::bigfile -sodb::largefiles -siterator::workdir::filesystem_gunk -srepo::init -srepo::init::at_filesystem_root)
ADD_CLAR_TEST(online    -v -sonline)
ADD_CLAR_TEST(gitdaemon -v -sonline::push)
ADD_CLAR_TEST(ssh       -v -sonline::push -sonline::clone::ssh_cert -sonline::clone::ssh_with_paths -sonline::clone::path_whitespace_ssh)
ADD_CLAR_TEST(proxy     -v -sonline::clone::proxy)
