project(api-tests)

set(TESTS_ROOT ${CMAKE_CURRENT_LIST_DIR})
set(VENDOR_DIR "${TESTS_ROOT}/vendor")
set(APP_ROOT "${TESTS_ROOT}/../api")

add_subdirectory("${VENDOR_DIR}/Catch2-3.8.0")

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

# list(APPEND TEST_SOURCES ${TESTS_ROOT}/CMakeLists.txt)

file(GLOB_RECURSE FOUND_TEST_SOURCES "${APP_ROOT}/*.test.cpp")

enable_testing()

# Set TEST_SOURCES to the found test files.
set(TEST_SOURCES ${FOUND_TEST_SOURCES})

if(NOT TEST_SOURCES)
  message(WARNING "No test source files found in ${APP_ROOT}.")
endif()

if(TEST_SOURCES)
  add_executable(${PROJECT_NAME} ${TEST_SOURCES})

  target_include_directories(${PROJECT_NAME} PRIVATE
      ${CMAKE_BINARY_DIR}
      include
  )

  if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    find_package(Threads REQUIRED)
    set(PLATFORM_LINKLIBS Threads::Threads)
  endif()

  target_link_libraries(${PROJECT_NAME}
      PRIVATE
          SonicPi::API
          Catch2::Catch2WithMain
          ${PLATFORM_LINKLIBS}
          ${CMAKE_THREAD_LIBS_INIT}
  )

  if(WIN32)
    # Same UTF-8 active-code-page manifest as the GUI exe, so the tests
    # exercise the API under the identical narrow-string regime.
    target_sources(${PROJECT_NAME} PRIVATE ${TESTS_ROOT}/../gui/platform/utf8.manifest)
  endif()

  add_test(${PROJECT_NAME} ${PROJECT_NAME})

  SOURCE_GROUP(tests REGULAR_EXPRESSION ".*.test.(cpp|h)+")
  SOURCE_GROUP(tests FILES ${TEST_SOURCES})

  if(WIN32)
  add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${API_VENDORED_KISSFFT_DLL}
            $<TARGET_FILE_DIR:${PROJECT_NAME}>)

  endif()
endif()
