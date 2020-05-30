cmake_minimum_required(VERSION 2.8.12)
project(libogg)

# Required modules
include(GNUInstallDirs)
include(CheckIncludeFiles)
include(CMakePackageConfigHelpers)
include(CTest)

# Build options
option(BUILD_SHARED_LIBS "Build shared library" OFF)
if(APPLE)
    option(BUILD_FRAMEWORK "Build Framework bundle for OSX" OFF)
endif()

# Install options
option(INSTALL_DOCS "Install documentation" ON)
option(INSTALL_PKG_CONFIG_MODULE "Install ogg.pc file" ON)
option(INSTALL_CMAKE_PACKAGE_MODULE "Install CMake package configiguration module" ON)

# Extract project version from configure.ac
file(READ configure.ac CONFIGURE_AC_CONTENTS)
string(REGEX MATCH "AC_INIT\\(\\[libogg\\],\\[([0-9]*).([0-9]*).([0-9]*)" DUMMY ${CONFIGURE_AC_CONTENTS})
set(PROJECT_VERSION_MAJOR ${CMAKE_MATCH_1})
set(PROJECT_VERSION_MINOR ${CMAKE_MATCH_2})
set(PROJECT_VERSION_PATCH ${CMAKE_MATCH_3})
set(PROJECT_VERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH})

# Extract library version from configure.ac
string(REGEX MATCH "LIB_CURRENT=([0-9]*)" DUMMY ${CONFIGURE_AC_CONTENTS})
set(LIB_CURRENT ${CMAKE_MATCH_1})

string(REGEX MATCH "LIB_AGE=([0-9]*)" DUMMY ${CONFIGURE_AC_CONTENTS})
set(LIB_AGE ${CMAKE_MATCH_1})

string(REGEX MATCH "LIB_REVISION=([0-9]*)" DUMMY ${CONFIGURE_AC_CONTENTS})
set(LIB_REVISION ${CMAKE_MATCH_1})

math(EXPR LIB_SOVERSION "${LIB_CURRENT} - ${LIB_AGE}")
set(LIB_VERSION "${LIB_SOVERSION}.${LIB_AGE}.${LIB_REVISION}")


# Helper function to configure pkg-config files
function(configure_pkg_config_file pkg_config_file_in)
    set(prefix ${CMAKE_INSTALL_PREFIX})
    set(exec_prefix ${CMAKE_INSTALL_FULL_BINDIR})
    set(libdir ${CMAKE_INSTALL_FULL_LIBDIR})
    set(includedir ${CMAKE_INSTALL_FULL_INCLUDEDIR})
    set(VERSION ${PROJECT_VERSION})
    string(REPLACE ".in" "" pkg_config_file ${pkg_config_file_in})
    configure_file(${pkg_config_file_in} ${pkg_config_file} @ONLY)
endfunction()

message(STATUS "Configuring ${PROJECT_NAME} ${PROJECT_VERSION}")

# Configure config_type.h
check_include_files(inttypes.h INCLUDE_INTTYPES_H)
check_include_files(stdint.h INCLUDE_STDINT_H)
check_include_files(sys/types.h INCLUDE_SYS_TYPES_H)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(SIZE16 int16_t)
set(USIZE16 uint16_t)
set(SIZE32 int32_t)
set(USIZE32 uint32_t)
set(SIZE64 int64_t)
set(USIZE64 uint64_t)

include(CheckSizes)

configure_file(include/ogg/config_types.h.in include/ogg/config_types.h @ONLY)

set(OGG_HEADERS
    ${CMAKE_CURRENT_BINARY_DIR}/include/ogg/config_types.h
    include/ogg/ogg.h
    include/ogg/os_types.h
)

set(OGG_SOURCES
    src/bitwise.c
    src/framing.c
    src/crctable.h
)

if(WIN32 AND BUILD_SHARED_LIBS)
    list(APPEND OGG_SOURCES win32/ogg.def)
endif()

if(BUILD_FRAMEWORK)
    set(BUILD_SHARED_LIBS TRUE)
endif()

add_library(ogg ${OGG_HEADERS} ${OGG_SOURCES})
target_include_directories(ogg PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

set_target_properties(
    ogg PROPERTIES
    SOVERSION ${LIB_SOVERSION}
    VERSION ${LIB_VERSION}
    PUBLIC_HEADER "${OGG_HEADERS}"
)

if(BUILD_FRAMEWORK)
    set_target_properties(ogg PROPERTIES
        FRAMEWORK TRUE
        FRAMEWORK_VERSION ${PROJECT_VERSION}
        MACOSX_FRAMEWORK_IDENTIFIER org.xiph.ogg
        MACOSX_FRAMEWORK_SHORT_VERSION_STRING ${PROJECT_VERSION}
        MACOSX_FRAMEWORK_BUNDLE_VERSION ${PROJECT_VERSION}
        XCODE_ATTRIBUTE_INSTALL_PATH "@rpath"
        OUTPUT_NAME Ogg
    )
endif()

configure_pkg_config_file(ogg.pc.in)

install(TARGETS ogg
    EXPORT OggTargets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    FRAMEWORK DESTINATION ${CMAKE_INSTALL_PREFIX}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/ogg
)

if(INSTALL_CMAKE_PACKAGE_MODULE)
    set(CMAKE_INSTALL_CONFIGDIR ${CMAKE_INSTALL_LIBDIR}/cmake/Ogg)
    install(EXPORT OggTargets
        DESTINATION ${CMAKE_INSTALL_CONFIGDIR}
        NAMESPACE Ogg::
    )

    include(CMakePackageConfigHelpers)

    configure_package_config_file(${PROJECT_SOURCE_DIR}/cmake/OggConfig.cmake.in ${PROJECT_BINARY_DIR}/OggConfig.cmake
        INSTALL_DESTINATION ${CMAKE_INSTALL_CONFIGDIR}
        PATH_VARS CMAKE_INSTALL_FULL_INCLUDEDIR
    )

    write_basic_package_version_file(${PROJECT_BINARY_DIR}/OggConfigVersion.cmake
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY SameMajorVersion
    )

    install(FILES ${PROJECT_BINARY_DIR}/OggConfig.cmake ${PROJECT_BINARY_DIR}/OggConfigVersion.cmake
        DESTINATION ${CMAKE_INSTALL_CONFIGDIR}
    )
endif()

if(INSTALL_PKG_CONFIG_MODULE)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/ogg.pc
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
    )
endif()

if(INSTALL_DOCS)
    set(OGG_DOCS
        doc/framing.html
        doc/index.html
        doc/oggstream.html
        doc/ogg-multiplex.html
        doc/fish_xiph_org.png
        doc/multiplex1.png
        doc/packets.png
        doc/pages.png
        doc/stream.png
        doc/vorbisword2.png
        doc/white-ogg.png
        doc/white-xifish.png
        doc/rfc3533.txt
        doc/rfc5334.txt
        doc/skeleton.html
    )
    install(FILES ${OGG_DOCS} DESTINATION ${CMAKE_INSTALL_DOCDIR}/html)
    install(DIRECTORY doc/libogg DESTINATION ${CMAKE_INSTALL_DOCDIR}/html)
endif()

if(BUILD_TESTING)
    add_executable(test_bitwise src/bitwise.c ${OGG_HEADERS})
    target_compile_definitions(test_bitwise PRIVATE _V_SELFTEST)
    target_include_directories(test_bitwise PRIVATE
        include
        ${CMAKE_CURRENT_BINARY_DIR}/include
    )
    add_test(NAME test_bitwise COMMAND $<TARGET_FILE:test_bitwise>)

    add_executable(test_framing src/framing.c ${OGG_HEADERS})
    target_compile_definitions(test_framing PRIVATE _V_SELFTEST)
    target_include_directories(test_framing PRIVATE
        include
        ${CMAKE_CURRENT_BINARY_DIR}/include
    )
    add_test(NAME test_framing COMMAND $<TARGET_FILE:test_framing>)
endif()

set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
include(CPack)
