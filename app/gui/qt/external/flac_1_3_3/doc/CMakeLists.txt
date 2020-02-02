cmake_minimum_required(VERSION 3.9)

find_package(Doxygen)

if (NOT DOXYGEN_FOUND)
    return()
endif()

option(BUILD_DOXYGEN "Enable API documentation building via Doxygen" ON)

if (NOT BUILD_DOXYGEN)
    return()
endif()

set(DOXYGEN_HTML_FOOTER doxygen.footer.html)
set(DOXYGEN_GENERATE_TAGFILE FLAC.tag)

doxygen_add_docs(FLAC-doxygen
    "${PROJECT_SOURCE_DIR}/include/FLAC"
    "${PROJECT_SOURCE_DIR}/include/FLAC++")

add_subdirectory(html)

install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html/"
    DESTINATION "${CMAKE_INSTALL_DOCDIR}/html/api")
