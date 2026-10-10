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
if(NOT EXISTS ${GUI_ROOT}/utils/completion_argkinds.gen.h OR
   NOT EXISTS ${GUI_ROOT}/utils/completion_fnopts.gen.h OR
   NOT EXISTS ${GUI_ROOT}/utils/completion_optowners.gen.h)
  message(WARNING "completion metadata not generated (run pre-translations); skipping gui-tests")
  return()
endif()

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)   # the app's images and themes, compiled in below: a widget under test loads them as the app does

add_executable(${PROJECT_NAME}
    ${GUI_TESTS_ROOT}/main.cpp
    ${GUI_ROOT}/SonicPi.qrc
    ${GUI_TESTS_ROOT}/completionpopup_a11y.test.cpp
    ${GUI_TESTS_ROOT}/completionpopup_select.test.cpp
    ${GUI_TESTS_ROOT}/announcementpolicy.test.cpp
    ${GUI_TESTS_ROOT}/audiodevicepolicy.test.cpp
    ${GUI_TESTS_ROOT}/completion_context.test.cpp
    ${GUI_TESTS_ROOT}/arg_kind.test.cpp
    ${GUI_TESTS_ROOT}/argkinds_generated.test.cpp
    ${GUI_TESTS_ROOT}/completion_detect.test.cpp
    ${GUI_TESTS_ROOT}/completion_fnopts.test.cpp
    ${GUI_TESTS_ROOT}/completion_optowners.test.cpp
    ${GUI_TESTS_ROOT}/completion_rank.test.cpp
    ${GUI_TESTS_ROOT}/completion_trackparams.test.cpp
    ${GUI_TESTS_ROOT}/completion_trackparams_editor.test.cpp
    ${GUI_TESTS_ROOT}/completion_cuepaths_editor.test.cpp
    ${GUI_TESTS_ROOT}/tutorialdocs.test.cpp
    ${GUI_TESTS_ROOT}/tutorialwidgets_a11y.test.cpp
    ${GUI_TESTS_ROOT}/tutorialpane_clip.test.cpp
    ${GUI_TESTS_ROOT}/trackspanel_look.test.cpp
    ${GUI_TESTS_ROOT}/helprail_look.test.cpp
    ${GUI_TESTS_ROOT}/chevron_look.test.cpp
    ${GUI_TESTS_ROOT}/recorder_limits.test.cpp
    ${GUI_TESTS_ROOT}/recorder_audio_mix.test.cpp
    ${GUI_TESTS_ROOT}/helppanelmodel.test.cpp
    ${GUI_TESTS_ROOT}/sidecolumnmodel.test.cpp
    ${GUI_TESTS_ROOT}/nodetreemotion.test.cpp
    ${GUI_TESTS_ROOT}/nodetreegraph_look.test.cpp
    ${GUI_TESTS_ROOT}/loopscope_lifecycle.test.cpp
    ${GUI_TESTS_ROOT}/linkaudio_engine_ready.test.cpp
    ${GUI_TESTS_ROOT}/plugins.test.cpp
    ${GUI_TESTS_ROOT}/card_deck.test.cpp
    ${GUI_TESTS_ROOT}/quickstart_card.test.cpp
    ${GUI_TESTS_ROOT}/tutorial_cards.test.cpp
    ${GUI_ROOT}/widgets/nodetreegraph.cpp
    ${GUI_ROOT}/widgets/trackspanel.cpp
    ${GUI_ROOT}/widgets/quickstartpane.cpp
    ${GUI_ROOT}/widgets/codecard.cpp
    ${GUI_ROOT}/widgets/carddeck.cpp
    ${GUI_ROOT}/widgets/linkaudiostreamswidget.cpp
    ${GUI_ROOT}/widgets/linkvisibilitytoggle.cpp
    ${GUI_ROOT}/widgets/tutorialpane.cpp
    ${GUI_ROOT}/widgets/sonicpiscintilla.cpp
    ${GUI_ROOT}/widgets/sonicpilexer.cpp
    ${GUI_ROOT}/widgets/sonicpilog.cpp
    ${GUI_ROOT}/widgets/zoombar.cpp
    ${GUI_ROOT}/model/sonicpitheme.cpp
    ${GUI_ROOT}/widgets/editortoolbar.cpp
    ${GUI_ROOT}/widgets/findpopup.cpp
    ${GUI_ROOT}/widgets/chevronbutton.cpp
    ${GUI_TESTS_ROOT}/sampleheaderinfo.test.cpp
    ${GUI_TESTS_ROOT}/setbundle.test.cpp
    ${GUI_TESTS_ROOT}/splash_quit_policy.test.cpp
    ${GUI_ROOT}/widgets/splashwidget.cpp
    ${GUI_TESTS_ROOT}/stylesheet_invariants.test.cpp
    ${GUI_ROOT}/widgets/completionpopup.cpp
    ${GUI_ROOT}/utils/sampleheaderinfo.cpp
    ${GUI_ROOT}/utils/completion_context.cpp
    ${GUI_ROOT}/utils/reducedmotion.cpp
    ${GUI_ROOT}/utils/framepacer.cpp
    ${GUI_ROOT}/utils/tutorialdocs.cpp
    ${GUI_ROOT}/widgets/tutorialwidgets.cpp
    ${GUI_ROOT}/utils/setbundle.cpp
    ${GUI_ROOT}/utils/scintilla_api.cpp
)

# The Objective-C++ guard every system callback block goes through, and the
# lint that keeps it so: macOS only, with the frameworks the guard test
# provokes a real exception from. As in gui/, the .mm files go to the C++
# compiler, which reads Objective-C++ from the extension.
if(APPLE)
  target_sources(${PROJECT_NAME} PRIVATE
      ${GUI_TESTS_ROOT}/objc_guard.test.mm
      ${GUI_ROOT}/platform/objc_guard.mm)
  set_source_files_properties(${GUI_TESTS_ROOT}/objc_guard.test.mm ${GUI_ROOT}/platform/objc_guard.mm
      PROPERTIES COMPILE_FLAGS "-fobjc-arc")
  target_link_libraries(${PROJECT_NAME} PRIVATE "-framework Foundation" "-framework AVFoundation")
endif()

target_include_directories(${PROJECT_NAME} PRIVATE
# The Objective-C++ guard every system callback block goes through, and the
# lint that keeps it so: macOS only, with the frameworks the guard test
# provokes a real exception from. As in gui/, the .mm files go to the C++
# compiler, which reads Objective-C++ from the extension.
if(APPLE)
  target_sources(${PROJECT_NAME} PRIVATE
      ${GUI_TESTS_ROOT}/objc_guard.test.mm
      ${GUI_ROOT}/platform/objc_guard.mm)
  set_source_files_properties(${GUI_TESTS_ROOT}/objc_guard.test.mm ${GUI_ROOT}/platform/objc_guard.mm
      PROPERTIES COMPILE_FLAGS "-fobjc-arc")
  target_link_libraries(${PROJECT_NAME} PRIVATE "-framework Foundation" "-framework AVFoundation")
endif()

    ${GUI_ROOT}
    ${GUI_ROOT}/widgets
    ${GUI_ROOT}/QScintilla_src-2.14.1/src   # <Qsci/...> headers (declarations only)
    ${SP_APP_ROOT}/api/include              # the shm headers shim to the harness's, which sonic-pi-api exports
    ${CMAKE_BINARY_DIR}
)

target_compile_definitions(${PROJECT_NAME} PRIVATE
    SP_ROOT="${SP_APP_ROOT}/.."
    SONIC_PI_PLUGINS=$<BOOL:${CLOCKWORK_PLUGINS}>)   # as the GUI's (gui/CMakeLists.txt)

target_link_libraries(${PROJECT_NAME} PRIVATE
    sonic-pi-api
    Qt6::Core
    Qt6::Gui
    Qt6::Widgets
    Qt6::Svg
    Qt6::Test
    Catch2::Catch2
    QScintilla
)

enable_testing()
add_test(NAME ${PROJECT_NAME} COMMAND ${PROJECT_NAME})
