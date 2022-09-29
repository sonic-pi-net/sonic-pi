cmake_minimum_required(VERSION 3.2)

message(STATUS "CMakeLists: Sonic Pi Qt GUI")

if(APPLE)
  set(CMAKE_OSX_DEPLOYMENT_TARGET "10.15")
endif()

project("Sonic Pi"
  LANGUAGES CXX C
  DESCRIPTION "A code-based music creation and performance tool"
  VERSION 4.3.0
  HOMEPAGE_URL "https://sonic-pi.net"
)

set(QAPPLICATION_CLASS QApplication CACHE STRING "Inheritance class for SingleApplication")
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(QTAPP_ROOT ${CMAKE_CURRENT_LIST_DIR})

list(APPEND CMAKE_MODULE_PATH ${QTAPP_ROOT}/cmake)

# Set cmake prefix path so it can find the Qt libraries
if((${BUILD_32BIT}) AND (DEFINED ENV{QT_INSTALL_LOCATION32}))
  message(STATUS "Setting up a 32 bit Qt Build")
  message(STATUS "Setting prefix to user-defined Qt (32-bit) install location: $ENV{QT_INSTALL_LOCATION32}")
  set(CMAKE_PREFIX_PATH $ENV{QT_INSTALL_LOCATION32})
elseif(DEFINED ENV{QT_INSTALL_LOCATION})
  message(STATUS "Setting prefix to user-defined Qt install location: $ENV{QT_INSTALL_LOCATION}")
  set(CMAKE_PREFIX_PATH $ENV{QT_INSTALL_LOCATION})
elseif(APPLE AND EXISTS /usr/local/opt/qt@5)
  # Homebrew installs Qt into/usr/local/qt5
  message(STATUS "Setting prefix to Homebrew's Qt install location: /usr/local/opt/qt@5")
  set(CMAKE_PREFIX_PATH /usr/local/opt/qt@5)
endif()

# Qt Setup
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

set(QT_TOOLS_DIR ${CMAKE_PREFIX_PATH}/bin)

set_property(GLOBAL PROPERTY AUTOMOC_FOLDER Automoc)
set_property(GLOBAL PROPERTY AUTOGEN_TARGETS_FOLDER Automoc)

find_package(Qt6 QUIET)

if(Qt6_FOUND AND WITH_QT_GUI_WEBENGINE)
  find_package(Qt6 COMPONENTS Core Widgets Gui Concurrent Network OpenGLWidgets PrintSupport Xml Svg LinguistTools WebEngineWidgets REQUIRED)
  add_compile_definitions(WITH_WEBENGINE)
elseif(Qt6_FOUND)
  find_package(Qt6 COMPONENTS Core Widgets Gui Concurrent Network OpenGLWidgets PrintSupport Xml Svg LinguistTools REQUIRED)
elseif(WITH_QT_GUI_WEBENGINE)
  find_package(Qt5 COMPONENTS Core Widgets Gui Concurrent Network OpenGL PrintSupport Xml Svg LinguistTools WebEngineWidgets REQUIRED)
  add_compile_definitions(WITH_WEBENGINE)
else()
  find_package(Qt5 COMPONENTS Core Widgets Gui Concurrent Network OpenGL PrintSupport Xml Svg LinguistTools REQUIRED)
endif()

find_package(Threads REQUIRED)

if(USE_SYSTEM_LIBS)
  find_package(QScintilla REQUIRED)
else()
  # We build QScintilla as part of the main project to make it easy to debug/fix
  # It is not built as part of externals
  add_subdirectory(QScintilla_src-2.13.3)
endif()

add_subdirectory(vendor/SingleApplication)

set(APP_NAME sonic-pi)
set(MACOS_APP_NAME "Sonic Pi")

set(NEW_API_SOURCES
    ${QTAPP_ROOT}/qt_api_client.cpp
    ${QTAPP_ROOT}/qt_api_client.h
    ${QTAPP_ROOT}/visualizer/scope_window.cpp
    ${QTAPP_ROOT}/visualizer/scope_window.h
    )

set(EDITOR_SOURCES
    ${QTAPP_ROOT}/widgets/sonicpiscintilla.cpp
    ${QTAPP_ROOT}/widgets/sonicpilexer.cpp
    ${QTAPP_ROOT}/widgets/sonicpilexer.h
    ${QTAPP_ROOT}/widgets/sonicpiscintilla.h
    )

set(QT_SOURCES
    ${QTAPP_ROOT}/widgets/infowidget.cpp
    ${QTAPP_ROOT}/widgets/settingswidget.cpp
    ${QTAPP_ROOT}/widgets/bpmscrubwidget.cpp
    ${QTAPP_ROOT}/widgets/bpmscrubwidget.h
    ${QTAPP_ROOT}/mainwindow.cpp
    ${QTAPP_ROOT}/mainwindow.h
    ${QTAPP_ROOT}/model/sonicpitheme.cpp
    ${QTAPP_ROOT}/model/sonicpitheme.h
    ${QTAPP_ROOT}/widgets/infowidget.h
    ${QTAPP_ROOT}/widgets/settingswidget.h
    )

set(SOURCES
    ${QTAPP_ROOT}/main.cpp
    ${QTAPP_ROOT}/utils/scintilla_api.cpp
    ${QTAPP_ROOT}/utils/sonicpi_i18n.cpp
    ${QTAPP_ROOT}/widgets/sonicpilog.cpp
    ${QTAPP_ROOT}/widgets/sonicpilog.h
    ${QTAPP_ROOT}/widgets/sonicpicontext.cpp
    ${QTAPP_ROOT}/widgets/sonicpicontext.h
    ${QTAPP_ROOT}/widgets/sonicpimetro.cpp
    ${QTAPP_ROOT}/widgets/sonicpimetro.h
    ${QTAPP_ROOT}/widgets/timewarpedit.h
    ${QTAPP_ROOT}/widgets/timewarpedit.cpp
    ${QTAPP_ROOT}/widgets/sonicpieditor.cpp
    ${QTAPP_ROOT}/widgets/sonicpieditor.h
    ${QTAPP_ROOT}/utils/scintilla_api.h
    ${QTAPP_ROOT}/utils/sonicpi_i18n.h
    ${QTAPP_ROOT}/utils/ruby_help.h
    ${QTAPP_ROOT}/utils/lang_list.h
    ${QTAPP_ROOT}/model/settings.h
    )

  if(WITH_QT_GUI_WEBENGINE)
    set(SOURCES
      ${SOURCES}
      ${QTAPP_ROOT}/widgets/phxwebview.h
      ${QTAPP_ROOT}/widgets/phxwebview.cpp
      ${QTAPP_ROOT}/widgets/phxwidget.h
      ${QTAPP_ROOT}/widgets/phxwidget.cpp
      )
  endif()

SET(RESOURCES
    ${QTAPP_ROOT}/SonicPi.qrc
    ${QTAPP_ROOT}/help_files.qrc
    ${QTAPP_ROOT}/info_files.qrc
    ${QTAPP_ROOT}/SonicPi.rc
    ${QTAPP_ROOT}/images/app.icns
    )

SET(TS_FILES
    ${QTAPP_ROOT}/lang/sonic-pi_ar.ts
    ${QTAPP_ROOT}/lang/sonic-pi_bg.ts
    ${QTAPP_ROOT}/lang/sonic-pi_bn.ts
    ${QTAPP_ROOT}/lang/sonic-pi_bs.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ca.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ca@valencia.ts
    ${QTAPP_ROOT}/lang/sonic-pi_cs.ts
    ${QTAPP_ROOT}/lang/sonic-pi_da.ts
    ${QTAPP_ROOT}/lang/sonic-pi_de.ts
    ${QTAPP_ROOT}/lang/sonic-pi_el.ts
    ${QTAPP_ROOT}/lang/sonic-pi_en_AU.ts
    ${QTAPP_ROOT}/lang/sonic-pi_en_US.ts
    ${QTAPP_ROOT}/lang/sonic-pi_eo.ts
    ${QTAPP_ROOT}/lang/sonic-pi_es.ts
    ${QTAPP_ROOT}/lang/sonic-pi_et.ts
    ${QTAPP_ROOT}/lang/sonic-pi_eu.ts
    ${QTAPP_ROOT}/lang/sonic-pi_fa.ts
    ${QTAPP_ROOT}/lang/sonic-pi_fi.ts
    ${QTAPP_ROOT}/lang/sonic-pi_fr.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ga.ts
    ${QTAPP_ROOT}/lang/sonic-pi_gl.ts
    ${QTAPP_ROOT}/lang/sonic-pi_he.ts
    ${QTAPP_ROOT}/lang/sonic-pi_hi.ts
    ${QTAPP_ROOT}/lang/sonic-pi_hu.ts
    ${QTAPP_ROOT}/lang/sonic-pi_hy.ts
    ${QTAPP_ROOT}/lang/sonic-pi_id.ts
    ${QTAPP_ROOT}/lang/sonic-pi_is.ts
    ${QTAPP_ROOT}/lang/sonic-pi_it.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ja.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ka.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ko.ts
    ${QTAPP_ROOT}/lang/sonic-pi_nb.ts
    ${QTAPP_ROOT}/lang/sonic-pi_nl.ts
    ${QTAPP_ROOT}/lang/sonic-pi_pl.ts
    ${QTAPP_ROOT}/lang/sonic-pi_pt_BR.ts
    ${QTAPP_ROOT}/lang/sonic-pi_pt.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ro.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ru.ts
    ${QTAPP_ROOT}/lang/sonic-pi_si.ts
    ${QTAPP_ROOT}/lang/sonic-pi_sk.ts
    ${QTAPP_ROOT}/lang/sonic-pi_sl.ts
    ${QTAPP_ROOT}/lang/sonic-pi_sv.ts
    ${QTAPP_ROOT}/lang/sonic-pi_sw.ts
    ${QTAPP_ROOT}/lang/sonic-pi_th.ts
    ${QTAPP_ROOT}/lang/sonic-pi_tr.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ug.ts
    ${QTAPP_ROOT}/lang/sonic-pi_uk.ts
    ${QTAPP_ROOT}/lang/sonic-pi_vi.ts
    ${QTAPP_ROOT}/lang/sonic-pi_zh_HK.ts
    ${QTAPP_ROOT}/lang/sonic-pi_zh_TW.ts
    ${QTAPP_ROOT}/lang/sonic-pi_zh-Hans.ts
    ${QTAPP_ROOT}/lang/sonic-pi_zh.ts
)

# Enable tracy profiler on debug and rel with debug builds
if(WIN32)
    SET (SOURCES ${SOURCES} ${APP_ROOT}/external/tracy/TracyClient.cpp)
elseif(APPLE)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/macos.mm)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/macos.h)
endif()

set(ALL_SOURCES ${QT_SOURCES} ${SOURCES} ${EDITOR_SOURCES} ${OLD_API_SOURCES} ${NEW_API_SOURCES})

# Create the Qt version of the app
if (APPLE)
    # NOTE! The underscore here works around a subtle problem with  vcpkg toolchain and is required
  _add_executable(${APP_NAME} MACOSX_BUNDLE ${ALL_SOURCES} ${RESOURCES} )
else()
  add_executable(${APP_NAME} WIN32 ${ALL_SOURCES} ${RESOURCES}) # Win32 ignored on non-windows
endif()

# Translations: Convert the .ts files into .qm files
set_source_files_properties(${TS_FILES} PROPERTIES OUTPUT_LOCATION ${QTAPP_ROOT}/lang)
if (Qt6_FOUND)
  qt_add_lrelease(${APP_NAME} TS_FILES ${TS_FILES})
else()
  qt5_add_translation(QM_FILES ${TS_FILES})
  set_property(SOURCE ${APP_NAME} APPEND PROPERTY OBJECT_DEPENDS ${QM_FILES})
endif()

if (APPLE)
  set_target_properties(
    ${APP_NAME} PROPERTIES
    OUTPUT_NAME "Sonic Pi"
    MACOSX_RPATH TRUE
    XCODE_ATTRIBUTE_ENABLE_HARDENED_RUNTIME TRUE
    MACOSX_BUNDLE_BUNDLE_NAME ${PROJECT_NAME}
    MACOSX_BUNDLE_GUI_IDENTIFIER "net.sonic-pi.app"
    MACOSX_BUNDLE_INFO_STRING "A code-based music creation and performance tool"
    MACOSX_BUNDLE_ICON_FILE "app.icns"
    MACOSX_BUNDLE_LONG_VERSION_STRING ${PROJECT_VERSION}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION}
    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
    RESOURCE "images/app.icns"
    LINK_FLAGS "-framework AppKit"
    OUTPUT_NAME ${MACOS_APP_NAME}
    )
  file(
    GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/run-macdeployqt.sh"
    CONTENT "PATH=$PATH:/usr/local/opt/qt@5/bin macdeployqt \"$1\""
    )

  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND bash "${CMAKE_CURRENT_BINARY_DIR}/run-macdeployqt.sh" ${MACOS_APP_NAME}.app
    )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink ../../../../../../../app app
    POST_BUILD
    WORKING_DIRECTORY ${MACOS_APP_NAME}.app/Contents/Resources
    )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink ../../../../../../server server
    POST_BUILD
    WORKING_DIRECTORY ${MACOS_APP_NAME}.app/Contents/Resources
    )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink ../../../../../../../etc etc
    POST_BUILD
    WORKING_DIRECTORY ${MACOS_APP_NAME}.app/Contents/Resources
    )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND plutil -replace NSHighResolutionCapable -bool true ${MACOS_APP_NAME}.app/Contents/Info.plist
  )
endif()

message(STATUS "App Root: ${APP_ROOT}")

set_property(TARGET ${APP_NAME} PROPERTY POSITION_INDEPENDENT_CODE ON) # Qt requires this?
target_include_directories(${APP_NAME}
    PRIVATE
    ${QTAPP_ROOT}
    ${APP_ROOT}/external/scsynth-boost-1.74.0
    ${APP_ROOT}/external/TLSF-2.4.6/src
    ${APP_ROOT}/external
    ${QTAPP_ROOT}/osc
    ${QTAPP_ROOT}/model
    ${QTAPP_ROOT}/visualizer
    ${QTAPP_ROOT}/widgets
    ${QTAPP_ROOT}/Utils
    ${CMAKE_BINARY_DIR}
    include)

target_link_libraries(${APP_NAME}
    PRIVATE
    SonicPi::API
    QScintilla
    Threads::Threads
    SingleApplication::SingleApplication)

if(Qt5_FOUND)
  target_link_libraries(${APP_NAME}
  PRIVATE
  Qt5::Core
  Qt5::Gui
  Qt5::Widgets
  Qt5::OpenGL
  Qt5::Concurrent
  Qt5::Network)
else()
  target_link_libraries(${APP_NAME}
  PRIVATE
  Qt::Core
  Qt::Gui
  Qt::Widgets
  Qt::OpenGL
  Qt::Concurrent
  Qt::Network
  Qt::OpenGLWidgets)
endif()

if(WITH_QT_GUI_WEBENGINE)
  if(Qt5_FOUND)
    target_link_libraries(${APP_NAME}
    PRIVATE
    Qt5::WebEngineWidgets)
  else()
    target_link_libraries(${APP_NAME}
    PRIVATE
    Qt::WebEngineWidgets)
  endif()
endif()


# Compile options and OS specific libraries
if(WIN32)
    # Workaround Qt + MSVC 19 compile issue in release build.
    target_compile_options(${APP_NAME} PRIVATE $<$<CXX_COMPILER_ID:MSVC>:/wd4005 /W3 /D_CRT_SECURE_NO_WARNINGS /D_WINSOCK_DEPRECATED_NO_WARNINGS /DBOOST_DATE_TIME_NO_LIB>)
    target_compile_options(${APP_NAME} PRIVATE "$<$<CONFIG:DEBUG>:-DTRACY_ENABLE=1>")
    target_compile_options(${APP_NAME} PRIVATE "$<$<CONFIG:RELWITHDEBINFO>:-DTRACY_ENABLE=1>")
elseif(${CMAKE_SYSTEM_NAME} MATCHES Linux)
    # Link librt
    target_link_libraries(${APP_NAME} PRIVATE rt)
endif()

# Deploy Qt binaries to the output on windows, and copy the CRT to the release folder
if(WIN32)
    # Visual Studio
    set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP true)
    include(InstallRequiredSystemLibraries)
    file(COPY ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/Release)

    # Run winddeployqt if it can be found, to ensure installed dependencies
    add_custom_command(TARGET ${APP_NAME} POST_BUILD
        COMMAND ${CMAKE_PREFIX_PATH}/bin/windeployqt $<TARGET_FILE:${APP_NAME}>)
endif() # Win32

# Make convenient source groups in the IDE
source_group(SonicPi FILES ${SOURCES})
source_group(Osc FILES ${OSC_SOURCES})
source_group(Scintilla FILES ${EDITOR_SOURCES})
source_group(Qt FILES ${QT_SOURCES})
source_group(OldApi FILES ${OLD_API_SOURCES})
source_group(NewApi FILES ${NEW_API_SOURCES})

source_group(Qt\\AutoMoc FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_automoc.cpp )
source_group(Qt\\AutoMoc REGULAR_EXPRESSION "(mocs_*|qrc_.*|QtAwesome.*)" )
