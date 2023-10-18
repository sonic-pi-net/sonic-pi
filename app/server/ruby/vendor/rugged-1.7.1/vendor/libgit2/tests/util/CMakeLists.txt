# util: the unit tests for libgit2's utility library

set(Python_ADDITIONAL_VERSIONS 3 2.7)
find_package(PythonInterp)

if(NOT PYTHONINTERP_FOUND)
	message(FATAL_ERROR "Could not find a python interpeter, which is needed to build the tests. "
	                     "Make sure python is available, or pass -DBUILD_TESTS=OFF to skip building the tests")
ENDIF()

set(CLAR_PATH "${libgit2_SOURCE_DIR}/tests/clar")
set(CLAR_FIXTURES "${libgit2_SOURCE_DIR}/tests/resources/")
set(TEST_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
add_definitions(-DCLAR_FIXTURE_PATH=\"${CLAR_FIXTURES}\")
add_definitions(-DCLAR_TMPDIR=\"libgit2_tests\")
add_definitions(-DCLAR_WIN32_LONGPATHS)
add_definitions(-D_FILE_OFFSET_BITS=64)

# Ensure that we do not use deprecated functions internally
add_definitions(-DGIT_DEPRECATE_HARD)

set(TEST_INCLUDES "${CLAR_PATH}" "${TEST_PATH}" "${CMAKE_CURRENT_BINARY_DIR}")
file(GLOB_RECURSE SRC_TEST ${TEST_PATH}/*.c ${TEST_PATH}/*.h ${TEST_PATH}/*/*.c ${TEST_PATH}/*/*.h)
file(GLOB_RECURSE SRC_CLAR ${CLAR_PATH}/*.c ${CLAR_PATH}/*.h)

if(MSVC_IDE)
	list(APPEND SRC_TEST "precompiled.c")
endif()

add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/clar.suite ${CMAKE_CURRENT_BINARY_DIR}/clar_suite.h
	COMMAND ${PYTHON_EXECUTABLE} ${CLAR_PATH}/generate.py -o "${CMAKE_CURRENT_BINARY_DIR}" -f .
	DEPENDS ${SRC_TEST}
	WORKING_DIRECTORY ${TEST_PATH})

set_source_files_properties(
	${CLAR_PATH}/clar.c
	PROPERTIES OBJECT_DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/clar.suite)

add_executable(util_tests ${SRC_CLAR} ${SRC_TEST} ${LIBGIT2_OBJECTS})

set_target_properties(util_tests PROPERTIES C_STANDARD 90)
set_target_properties(util_tests PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${libgit2_BINARY_DIR})

target_include_directories(util_tests PRIVATE ${TEST_INCLUDES} ${LIBGIT2_INCLUDES} ${LIBGIT2_DEPENDENCY_INCLUDES})
target_include_directories(util_tests SYSTEM PRIVATE ${LIBGIT2_SYSTEM_INCLUDES})
target_link_libraries(util_tests ${LIBGIT2_SYSTEM_LIBS})

ide_split_sources(util_tests)

#
# Old versions of gcc require us to declare our test functions; don't do
# this on newer compilers to avoid unnecessary recompilation.
#
if(CMAKE_COMPILER_IS_GNUCC AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 6.0)
	target_compile_options(util_tests PRIVATE -include "clar_suite.h")
endif()

if(MSVC_IDE)
	# Precompiled headers
	set_target_properties(util_tests PROPERTIES COMPILE_FLAGS "/Yuprecompiled.h /FIprecompiled.h")
	set_source_files_properties("precompiled.c" COMPILE_FLAGS "/Ycprecompiled.h")
endif()

enable_testing()

include(AddClarTest)
add_clar_test(util_tests util -v)
