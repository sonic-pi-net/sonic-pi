cmake_minimum_required(VERSION 3.7.2)
project(ghcfilesystem)

if(POLICY CMP0110)
    cmake_policy(PUSH)
    cmake_policy(SET CMP0110 OLD)
endif()

include(CMakeDependentOption)

cmake_dependent_option(GHC_FILESYSTEM_BUILD_TESTING
    "Enable tests" ON
    "CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR" OFF)
cmake_dependent_option(GHC_FILESYSTEM_BUILD_EXAMPLES
    "Build examples" ON
    "CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR" OFF)
cmake_dependent_option(GHC_FILESYSTEM_WITH_INSTALL
    "With install target" ON
    "CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR" OFF)

if(NOT DEFINED CMAKE_CXX_STANDARD)
    set(CMAKE_CXX_STANDARD 11)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_EXTENSIONS OFF)
endif()
if(CMAKE_CXX_STANDARD LESS 11)
    message(FATAL_ERROR "CMAKE_CXX_STANDARD is less than 11, ghc::filesystem only works with C++11 and above.")
endif()

add_library(ghc_filesystem INTERFACE)
target_include_directories(ghc_filesystem INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)
target_compile_options(ghc_filesystem INTERFACE "$<$<C_COMPILER_ID:MSVC>:/utf-8>")
target_compile_options(ghc_filesystem INTERFACE "$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")

get_directory_property(hasParent PARENT_DIRECTORY)
if(NOT hasParent)
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/")
    include(GhcHelper)

    if(GHC_FILESYSTEM_BUILD_TESTING)
        enable_testing()
        add_subdirectory(test)
    endif()

    if(GHC_FILESYSTEM_BUILD_EXAMPLES)
        add_subdirectory(examples)
    endif()
endif()

if(GHC_FILESYSTEM_WITH_INSTALL)
    include(CMakePackageConfigHelpers)
    include(GNUInstallDirs)

    install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
    install(TARGETS ghc_filesystem EXPORT ghcFilesystemConfig)
    install(EXPORT ghcFilesystemConfig NAMESPACE ghcFilesystem:: DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/ghcFilesystem)
endif()

if(POLICY CMP0110)
    cmake_policy(POP)
endif()
