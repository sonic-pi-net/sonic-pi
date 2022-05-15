project(sonic-pi-imgui VERSION 0.1.0.0)

set(APP_NAME ${PROJECT_NAME})

# Set additional project information
set(COMPANY "Sonic Pi")
set(COPYRIGHT "Copyright (c) 2021 Sonic Pi")
set(IDENTIFIER "com.sonic-pi-imgui")

set(APP_ROOT ${CMAKE_CURRENT_LIST_DIR})

list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_CURRENT_LIST_DIR}/cmake)
include(${CMAKE_CURRENT_LIST_DIR}/cmake/demo_common.cmake)

find_package(gl3w CONFIG REQUIRED)
find_package(SDL2 CONFIG REQUIRED)
find_package(fmt CONFIG REQUIRED)

if (USE_SYSTEM_LIBS)
    set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
    set(THREADS_PREFER_PTHREAD_FLAG TRUE)

    if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	find_package(Threads REQUIRED)
	set(PLATFORM_LINKLIBS
	    Threads::Threads
	    )
    endif()
endif()

set(SPIMGUI_APP_SOURCE
    ${APP_ROOT}/main.cpp
    ${APP_ROOT}/app.cpp
    ${APP_ROOT}/log_window.cpp
    ${APP_ROOT}/log_window.h 
    ${APP_ROOT}/scope_window.cpp
    ${APP_ROOT}/scope_window.h
    ${APP_ROOT}/main_menu.cpp
    ${APP_ROOT}/main_menu.h
    ${APP_ROOT}/edit_window.cpp
    ${APP_ROOT}/edit_window.h
    ${APP_ROOT}/cue_window.cpp
    ${APP_ROOT}/cue_window.h
    ${APP_ROOT}/midi_window.cpp
    ${APP_ROOT}/midi_window.h
    ${APP_ROOT}/CMakeLists.txt

    )

set(SPIMGUI_IMGUI_SOURCE
    ${APP_ROOT}/core/imconfig.h
    ${APP_ROOT}/core/imgui.cpp
    ${APP_ROOT}/core/imgui.h
    ${APP_ROOT}/core/imgui_demo.cpp
    ${APP_ROOT}/core/imgui_draw.cpp
    ${APP_ROOT}/core/imgui_internal.h
    ${APP_ROOT}/core/imgui_tables.cpp
    ${APP_ROOT}/core/imgui_widgets.cpp
    ${APP_ROOT}/core/imstb_rectpack.h
    ${APP_ROOT}/core/imstb_textedit.h
    ${APP_ROOT}/core/imstb_truetype.h

    ${APP_ROOT}/backends/imgui_impl_opengl3.cpp
    ${APP_ROOT}/backends/imgui_impl_opengl3.h
    ${APP_ROOT}/backends/imgui_impl_sdl.cpp
    ${APP_ROOT}/backends/imgui_impl_sdl.h
)

set(RESOURCE_FOLDER ${CMAKE_CURRENT_LIST_DIR}/res)
add_project_meta(META_FILES_TO_INCLUDE ${APP_ROOT}/res/app.manifest)

add_executable (${APP_NAME} 
    ${OS_BUNDLE}
    ${SPIMGUI_APP_SOURCE}
    ${SPIMGUI_IMGUI_SOURCE}
    ${META_FILES_TO_INCLUDE}
    )

target_include_directories(${APP_NAME}
    PRIVATE
    ${APP_ROOT}/core
    ${APP_ROOT}/backends
    ${CMAKE_CURRENT_LIST_DIR} 
    ${CMAKE_BINARY_DIR}
    )

# Extras for compiling on PI:  Use ES2
if (RASPBERRY_PI)
target_compile_definitions(${APP_NAME} PRIVATE 
	IMGUI_IMPL_OPENGL_ES2)

target_link_directories(${APP_NAME}
	PRIVATE
	 /opt/vc/lib
	 )
endif()

target_link_libraries (${APP_NAME}
    PRIVATE
    SonicPi::API
    fmt::fmt
    )

if (USE_SYSTEM_LIBS)
    target_link_libraries (${APP_NAME}
	PRIVATE
	gl3w
	SDL2::SDL2
	)
else()
    target_link_libraries (${APP_NAME}
	PRIVATE
	unofficial::gl3w::gl3w
	SDL2::SDL2main
	SDL2::SDL2-static
	)
endif()

target_link_libraries (${APP_NAME} 
    PRIVATE
    ${PLATFORM_LINKLIBS}
    )

if (WIN32)
    copy_existing_files(${PROJECT_NAME} "${RESOURCE_DEPLOY_FILES}" ${CMAKE_CURRENT_BINARY_DIR}/$(Configuration) )
endif()

source_group ("App" FILES ${SPIMGUI_APP_SOURCE})
source_group ("ImGui" FILES ${SPIMGUI_IMGUI_SOURCE})

