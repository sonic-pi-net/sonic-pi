# libgit2: the cross-platform, linkable library implementation of git.
# See `README.md` for build instructions.
#
# This top-level CMakeLists.txt sets up configuration options and
# determines which subprojects to build.

cmake_minimum_required(VERSION 3.5.1)

project(libgit2 VERSION "1.6.3" LANGUAGES C)

# Add find modules to the path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake")

#
# Build options
#

# Experimental features
option(EXPERIMENTAL_SHA256     "Enable experimental SHA256 support (for R&D/testing)"  OFF)

# Optional subsystems
option(BUILD_SHARED_LIBS       "Build Shared Library (OFF for Static)"                  ON)
option(BUILD_TESTS             "Build Tests using the Clar suite"                       ON)
option(BUILD_CLI               "Build the command-line interface"                       ON)
option(BUILD_EXAMPLES          "Build library usage example apps"                      OFF)
option(BUILD_FUZZERS           "Build the fuzz targets"                                OFF)

# Suggested functionality that may not be available on a per-platform basis
option(USE_THREADS             "Use threads for parallel processing when possible"      ON)
option(USE_NSEC                "Support nanosecond precision file mtimes and ctimes"    ON)

# Backend selection
option(USE_SSH                 "Link with libssh2 to enable SSH support"               OFF)
option(USE_HTTPS               "Enable HTTPS support. Can be set to a specific backend" ON)
option(USE_SHA1                "Enable SHA1. Can be set to CollisionDetection(ON)/HTTPS" ON)
option(USE_SHA256              "Enable SHA256. Can be set to HTTPS/Builtin" ON)
option(USE_GSSAPI              "Link with libgssapi for SPNEGO auth"      OFF)
   set(USE_HTTP_PARSER         "" CACHE STRING "Specifies the HTTP Parser implementation; either system or builtin.")
   set(REGEX_BACKEND           "" CACHE STRING "Regular expression implementation. One of regcomp_l, pcre2, pcre, regcomp, or builtin.")
option(USE_BUNDLED_ZLIB        "Use the bundled version of zlib. Can be set to one of Bundled(ON)/Chromium. The Chromium option requires a x86_64 processor with SSE4.2 and CLMUL" OFF)

# Debugging options
option(USE_LEAK_CHECKER        "Run tests with leak checker"                           OFF)
option(USE_STANDALONE_FUZZERS  "Enable standalone fuzzers (compatible with gcc)"       OFF)
option(DEBUG_POOL              "Enable debug pool allocator"                           OFF)
option(DEBUG_STRICT_ALLOC      "Enable strict allocator behavior"                      OFF)
option(DEBUG_STRICT_OPEN       "Enable path validation in open"                        OFF)

# Output options
option(SONAME                  "Set the (SO)VERSION of the target"                      ON)
   set(LIBGIT2_FILENAME        "git2" CACHE STRING "Name of the produced binary")
option(DEPRECATE_HARD          "Do not include deprecated functions in the library"    OFF)

# Compilation options
option(ENABLE_WERROR           "Enable compilation with -Werror"                       OFF)

if(UNIX)
	# NTLM client requires crypto libraries from the system HTTPS stack
	if(NOT USE_HTTPS)
		option(USE_NTLMCLIENT  "Enable NTLM support on Unix."                  OFF)
	else()
		option(USE_NTLMCLIENT  "Enable NTLM support on Unix."                   ON)
	endif()

	option(ENABLE_REPRODUCIBLE_BUILDS "Enable reproducible builds"                 OFF)
endif()

if(APPLE)
	option(USE_ICONV           "Link with and use iconv library"                    ON)
endif()

if(MSVC)
	# This option must match the settings used in your program, in particular if you
	# are linking statically
	option(STATIC_CRT          "Link the static CRT libraries"                      ON)

	# If you want to embed a copy of libssh2 into libgit2, pass a
	# path to libssh2
	option(EMBED_SSH_PATH      "Path to libssh2 to embed (Windows)"                OFF)

	# Enable leak checking using the debugging C runtime.
	option(WIN32_LEAKCHECK     "Enable leak reporting via crtdbg"                  OFF)
endif()

if(WIN32)
	# By default, libgit2 is built with WinHTTP.  To use the built-in
	# HTTP transport, invoke CMake with the "-DUSE_WINHTTP=OFF" argument.
	option(USE_WINHTTP         "Use Win32 WinHTTP routines"                         ON)
endif()

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()


# Modules

include(CheckLibraryExists)
include(CheckFunctionExists)
include(CheckSymbolExists)
include(CheckStructHasMember)
include(CheckPrototypeDefinition)
include(AddCFlagIfSupported)
include(FindPkgLibraries)
include(FindThreads)
include(FindStatNsec)
include(Findfutimens)
include(GNUInstallDirs)
include(IdeSplitSources)
include(FeatureSummary)
include(EnableWarnings)
include(DefaultCFlags)
include(ExperimentalFeatures)


#
# Subdirectories
#

add_subdirectory(src)

if(BUILD_TESTS)
	enable_testing()
	add_subdirectory(tests)
endif()

if(BUILD_EXAMPLES)
	add_subdirectory(examples)
endif()

if(BUILD_FUZZERS)
	if((BUILD_TESTS OR BUILD_EXAMPLES) AND NOT USE_STANDALONE_FUZZERS)
		message(FATAL_ERROR "Cannot build the fuzzer and the tests or examples together")
	endif()
	add_subdirectory(fuzzers)
endif()


# Export for people who use us as a dependency

if(NOT "${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_SOURCE_DIR}")
	set(LIBGIT2_DEPENDENCY_OBJECTS ${LIBGIT2_DEPENDENCY_OBJECTS} PARENT_SCOPE)
	set(LIBGIT2_SYSTEM_LIBS ${LIBGIT2_SYSTEM_LIBS} PARENT_SCOPE)
endif()


# Summary

feature_summary(WHAT ENABLED_FEATURES DESCRIPTION "Enabled features:")
feature_summary(WHAT DISABLED_FEATURES DESCRIPTION "Disabled features:")
