# Make a release build of osmid
include(ExternalProject)
ExternalProject_Add(osmid
    PREFIX osmid-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/osmid
    INSTALL_COMMAND "" 
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config Release
    )

add_subdirectory(QScintilla-2.11.4)

