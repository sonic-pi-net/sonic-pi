project(api-tests)

set(TESTS_ROOT ${CMAKE_CURRENT_LIST_DIR})

find_package(Catch2 CONFIG REQUIRED)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

list(APPEND TEST_SOURCES
    ${TESTS_ROOT}/CMakeLists.txt)

file(GLOB_RECURSE FOUND_TEST_SOURCES "${APP_ROOT}/*.test.cpp")

enable_testing()

set (TEST_SOURCES
    ${FOUND_TEST_SOURCES}
    ${TEST_SOURCES}
    )

add_executable(${PROJECT_NAME} ${TEST_SOURCES})

target_include_directories(${PROJECT_NAME} PRIVATE
    ${CMAKE_BINARY_DIR}
    include
    )


if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
find_package(Threads REQUIRED)
set(PLATFORM_LINKLIBS
    Threads::Threads
	)
endif()

target_link_libraries(${PROJECT_NAME}
    PRIVATE
        SonicPi::API
        Catch2::Catch2WithMain
        ${PLATFORM_LINKLIBS}
        ${CMAKE_THREAD_LIBS_INIT})

add_test(${PROJECT_NAME} ${PROJECT_NAME})

SOURCE_GROUP(tests REGULAR_EXPRESSION ".*.test.(cpp|h)+")
SOURCE_GROUP(tests FILES ${TEST_SOURCES})

