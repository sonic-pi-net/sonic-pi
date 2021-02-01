find_package(SDL2 REQUIRED)

macro(add_project_meta FILES_TO_INCLUDE)
if (NOT RESOURCE_FOLDER)
    set(RESOURCE_FOLDER ${CMAKE_CURRENT_BINARY_DIR}/../res)
endif()

if (NOT ICON_NAME)
    set(ICON_NAME AppIcon)
endif()

#set(RESOURCE_DEPLOY_FILES)

if (APPLE)
    set(ICON_FILE ${RESOURCE_FOLDER}/${ICON_NAME}.icns)
elseif (WIN32)
    set(ICON_FILE ${RESOURCE_FOLDER}/${ICON_NAME}.ico)
endif()

set(RESOURCE_DEPLOY_FILES ${RESOURCE_DEPLOY_FILES}
    ${RESOURCE_FOLDER}/Cousine-Regular.ttf
    ${RESOURCE_FOLDER}/imgui.ini
    ) 

if (WIN32)
    configure_file("${APP_ROOT}/cmake/windows_metafile.rc.in"
      "windows_metafile.rc"
    )
set(RES_FILES windows_metafile.rc ${APP_ROOT}/res/app.manifest)
endif()

if (APPLE)
    set_source_files_properties(${ICON_FILE} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
    set_source_files_properties(${RESOURCE_DEPLOY_FILES} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

    # Identify MacOS bundle
    set(MACOSX_BUNDLE_BUNDLE_NAME ${PROJECT_NAME})
    set(MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION})
    set(MACOSX_BUNDLE_LONG_VERSION_STRING ${PROJECT_VERSION})
    set(MACOSX_BUNDLE_SHORT_VERSION_STRING "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}")
    set(MACOSX_BUNDLE_COPYRIGHT ${COPYRIGHT})
    set(MACOSX_BUNDLE_GUI_IDENTIFIER ${IDENTIFIER})
    set(MACOSX_BUNDLE_ICON_FILE ${ICON_NAME})
endif()

if (APPLE)
    set(${FILES_TO_INCLUDE} ${ICON_FILE} ${RESOURCE_DEPLOY_FILES})
elseif (WIN32)
    set(${FILES_TO_INCLUDE} ${RES_FILES})
endif()

endmacro()

macro(init_os_bundle)
if (APPLE)
    set(OS_BUNDLE MACOSX_BUNDLE)
elseif (WIN32)
    set(OS_BUNDLE WIN32)
endif()
endmacro()

macro(fix_win_compiler)
if (MSVC)
    set_target_properties(${PROJECT_NAME} PROPERTIES
        WIN32_EXECUTABLE YES
        LINK_FLAGS "/ENTRY:mainCRTStartup"
    )
endif()
endmacro()

init_os_bundle()


