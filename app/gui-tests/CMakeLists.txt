project(gui-tests)

# Mirrors app/api-tests, but for the Qt GUI. The GUI is an executable, not a
# library, so the widgets under test are compiled straight into the test binary.
# The test main forces the offscreen Qt platform so it runs headless in CI; it
# asserts the accessibility data a screen reader consumes (QAccessibleInterface
# + announcement signals).

set(GUI_TESTS_ROOT ${CMAKE_CURRENT_LIST_DIR})
set(GUI_ROOT ${GUI_TESTS_ROOT}/../gui)
set(SP_APP_ROOT ${GUI_TESTS_ROOT}/..)

find_package(Qt6 COMPONENTS Core Gui Widgets Svg Test REQUIRED)

# Catch2 targets are created by the api-tests subdirectory (added earlier in
# app/CMakeLists.txt). Don't add_subdirectory it again — that would redefine
# the targets. If it's somehow absent, skip these tests rather than error.
if(NOT TARGET Catch2::Catch2)
  message(WARNING "Catch2 target not found; skipping gui-tests")
  return()
endif()

# Some tests include the generated completion metadata (completion_argkinds.gen.h),
# produced by qt-doc.rb during pre-translations. If it hasn't been generated yet,
# skip rather than fail with a cryptic missing-include error.
if(NOT EXISTS ${GUI_ROOT}/utils/completion_argkinds.gen.h)
  message(WARNING "completion_argkinds.gen.h not generated (run pre-translations); skipping gui-tests")
  return()
endif()

set(CMAKE_AUTOMOC ON)

add_executable(${PROJECT_NAME}
    ${GUI_TESTS_ROOT}/main.cpp
    ${GUI_TESTS_ROOT}/completionpopup_a11y.test.cpp
    ${GUI_TESTS_ROOT}/announcementpolicy.test.cpp
    ${GUI_TESTS_ROOT}/completion_context.test.cpp
    ${GUI_TESTS_ROOT}/arg_kind.test.cpp
    ${GUI_TESTS_ROOT}/argkinds_generated.test.cpp
    ${GUI_TESTS_ROOT}/completion_detect.test.cpp
    ${GUI_TESTS_ROOT}/completion_rank.test.cpp
    ${GUI_TESTS_ROOT}/tutorialdocs.test.cpp
    ${GUI_TESTS_ROOT}/tutorialwidgets_a11y.test.cpp
    ${GUI_TESTS_ROOT}/setbundle.test.cpp
    ${GUI_ROOT}/widgets/completionpopup.cpp
    ${GUI_ROOT}/utils/completion_context.cpp
    ${GUI_ROOT}/utils/reducedmotion.cpp
    ${GUI_ROOT}/utils/tutorialdocs.cpp
    ${GUI_ROOT}/widgets/tutorialwidgets.cpp
    ${GUI_ROOT}/utils/setbundle.cpp
)

target_include_directories(${PROJECT_NAME} PRIVATE
    ${GUI_ROOT}
    ${GUI_ROOT}/widgets
    ${GUI_ROOT}/QScintilla_src-2.14.1/src   # <Qsci/...> headers (declarations only)
    ${SP_APP_ROOT}/api/include              # api/audio/shm_audio_buffer.hpp via platform/macos.h
    ${SP_APP_ROOT}/external/supersonic      # the api/audio shm headers shim to supersonic's
    ${SP_APP_ROOT}/external/supersonic/src
    ${CMAKE_BINARY_DIR}
)

target_link_libraries(${PROJECT_NAME} PRIVATE
    Qt6::Core
    Qt6::Gui
    Qt6::Widgets
    Qt6::Svg
    Qt6::Test
    Catch2::Catch2
)

enable_testing()
add_test(NAME ${PROJECT_NAME} COMMAND ${PROJECT_NAME})
