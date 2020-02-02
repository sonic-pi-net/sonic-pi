cmake_minimum_required(VERSION 3.2)

message(STATUS " CMakeLists: Sonic Pi")

project(SonicPi
    LANGUAGES CXX C
    VERSION 3.2.0
    )

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_MODULE_PATH ${APP_ROOT}/cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(APP_ROOT ${CMAKE_CURRENT_LIST_DIR})
set(SONIC_PI_ROOT ${CMAKE_CURRENT_LIST_DIR}/../../..)

if(BUILD_32BIT)
set(CMAKE_PREFIX_PATH $ENV{QT_INSTALL_LOCATION32})
else()
set(CMAKE_PREFIX_PATH $ENV{QT_INSTALL_LOCATION})
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_CURRENT_LIST_DIR}/RelWithDebInfo)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_CURRENT_LIST_DIR}/RelWithDebInfo)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_CURRENT_LIST_DIR}/RelWithDebInfo)

CONFIGURE_FILE(${CMAKE_CURRENT_LIST_DIR}/cmake/config.h.cmake ${CMAKE_BINARY_DIR}/config.h)

# Qt Setup
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(QT_TOOLS_DIR ${CMAKE_PREFIX_PATH}/bin)

set_property(GLOBAL PROPERTY AUTOMOC_FOLDER Automoc)
set_property(GLOBAL PROPERTY AUTOGEN_TARGETS_FOLDER Automoc)

find_package(Qt5 COMPONENTS Core Widgets Gui Concurrent Network OpenGL PrintSupport Xml Svg REQUIRED)
find_package(Threads REQUIRED)

# We build scintilla as part of the main project to make it easy to debug/fix
add_subdirectory(external/QScintilla-2.11.4)

set(APP_NAME sonic-pi)

set(OSC_SOURCES
    ${APP_ROOT}/osc/oschandler.cpp
    ${APP_ROOT}/osc/oscsender.cpp
    ${APP_ROOT}/osc/sonic_pi_osc_server.cpp
    ${APP_ROOT}/osc/sonic_pi_udp_osc_server.cpp
    ${APP_ROOT}/osc/sonic_pi_tcp_osc_server.cpp
    ${APP_ROOT}/osc/oscpkt.hh
    ${APP_ROOT}/osc/udp.hh
    ${APP_ROOT}/osc/oschandler.h
    ${APP_ROOT}/osc/oscsender.h
    ${APP_ROOT}/osc/sonic_pi_osc_server.h
    ${APP_ROOT}/osc/sonic_pi_udp_osc_server.h
    ${APP_ROOT}/osc/sonic_pi_tcp_osc_server.h
    )

set(EDITOR_SOURCES
    ${APP_ROOT}/widgets/sonicpiscintilla.cpp
    ${APP_ROOT}/widgets/sonicpilexer.cpp
    ${APP_ROOT}/widgets/sonicpilexer.h
    ${APP_ROOT}/widgets/sonicpiscintilla.h
    )

set(QT_SOURCES
    ${APP_ROOT}/widgets/infowidget.cpp
    ${APP_ROOT}/widgets/settingswidget.cpp
    ${APP_ROOT}/mainwindow.cpp
    ${APP_ROOT}/mainwindow.h
    ${APP_ROOT}/model/sonicpitheme.cpp
    ${APP_ROOT}/model/sonicpitheme.h
    ${APP_ROOT}/widgets/infowidget.h
    ${APP_ROOT}/widgets/settingswidget.h
    )

set(SOURCES
    ${APP_ROOT}/visualizer/scope.cpp
    ${APP_ROOT}/visualizer/scope.h
    ${APP_ROOT}/visualizer/scope_buffer.hpp
    ${APP_ROOT}/visualizer/server_shm.hpp
    ${APP_ROOT}/main.cpp
    ${APP_ROOT}/utils/sonicpiapis.cpp
    ${APP_ROOT}/widgets/sonicpilog.cpp
    ${APP_ROOT}/widgets/sonicpilog.h
    ${APP_ROOT}/utils/sonicpiapis.h
    ${APP_ROOT}/utils/ruby_help.h
    ${APP_ROOT}/model/settings.h
    )

SET(RESOURCES
    ${APP_ROOT}/SonicPi.qrc
    ${APP_ROOT}/help_files.qrc
    ${APP_ROOT}/info_files.qrc
    ${APP_ROOT}/SonicPi.rc
    ${APP_ROOT}/images/app.icns
    )

set(ALL_SOURCES ${OSC_SOURCES} ${QT_SOURCES} ${SOURCES} ${EDITOR_SOURCES})

# Create the Qt version of the app
add_executable(${APP_NAME} WIN32 ${ALL_SOURCES} ${RESOURCES}) # Win32 ignored on non-windows
set_property(TARGET ${APP_NAME} PROPERTY POSITION_INDEPENDENT_CODE ON) # Qt requires this?
target_include_directories(${APP_NAME}
    PRIVATE
    ${APP_ROOT}
    ${APP_ROOT}/external/boost_1_72_0
    ${APP_ROOT}/osc
    ${APP_ROOT}/model
    ${APP_ROOT}/visualizer
    ${APP_ROOT}/widgets
    ${APP_ROOT}/Utils
    ${Qt5Widgets_INCLUDE_DIRS}
    ${Qt5Gui_INCLUDE_DIRS}
    ${Qt5Core_INCLUDE_DIRS}
    ${CMAKE_BINARY_DIR}
    include)

target_link_libraries(${APP_NAME}
    PRIVATE
    QScintilla
    Qt5::Core 
    Qt5::Gui 
    Qt5::Widgets
    Qt5::OpenGL
    Qt5::Concurrent
    Qt5::Network
    Threads::Threads)

# Compile options and OS specific libraries
if(WIN32)
    # Workaround Qt + MSVC 19 compile issue in release build.
    target_compile_options(${APP_NAME} PRIVATE $<$<CXX_COMPILER_ID:MSVC>:/wd4005 /W3 /D_CRT_SECURE_NO_WARNINGS /D_WINSOCK_DEPRECATED_NO_WARNINGS /DBOOST_DATE_TIME_NO_LIB>)
elseif(UNIX)
    # Link librt
    target_link_libraries(${APP_NAME} PRIVATE rt)
endif()

# Deploy Qt binaries to the output on windows
if(WIN32)
    message(STATUS "Copying required Qt libraries and binaries to output directory....")

    # Run winddeployqt if it can be found, to ensure installed dependencies
    find_program(WINDEPLOYQT_EXECUTABLE NAMES windeployqt HINTS ${QTDIR} ENV QTDIR PATH_SUFFIXES bin)
    add_custom_command(TARGET ${APP_NAME} POST_BUILD
        COMMAND ${WINDEPLOYQT_EXECUTABLE} $<TARGET_FILE:${APP_NAME}>)

    # Copy the built osmid files to the server
    add_custom_command(TARGET ${APP_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory ${SONIC_PI_ROOT}/app/server/native/osmid
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_LIST_DIR}/external/build/osmid-prefix/src/osmid-build/Release/m2o.exe ${SONIC_PI_ROOT}/app/server/native/osmid
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_LIST_DIR}/external/build/osmid-prefix/src/osmid-build/Release/o2m.exe ${SONIC_PI_ROOT}/app/server/native/osmid)
endif() # Win32

# Make convenient source groups in the IDE
source_group(SonicPi FILES ${SOURCES})
source_group(Osc FILES ${OSC_SOURCES})
source_group(Scintilla FILES ${EDITOR_SOURCES})
source_group(Qt FILES ${QT_SOURCES})

source_group(Qt\\AutoMoc FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_automoc.cpp ) 
source_group(Qt\\AutoMoc REGULAR_EXPRESSION "(mocs_*|qrc_.*|QtAwesome.*)" ) 
