# CMake build script for the libgit2 project
#
# Building (out of source build):
# > mkdir build && cd build
# > cmake .. [-DSETTINGS=VALUE]
# > cmake --build .
#
# Testing:
# > ctest -V
#
# Install:
# > cmake --build . --target install

PROJECT(libgit2 C)
CMAKE_MINIMUM_REQUIRED(VERSION 3.5.1)
CMAKE_POLICY(SET CMP0015 NEW)
IF(POLICY CMP0051)
	CMAKE_POLICY(SET CMP0051 NEW)
ENDIF()
IF(POLICY CMP0042)
	CMAKE_POLICY(SET CMP0042 NEW)
ENDIF()
IF(POLICY CMP0054)
	CMAKE_POLICY(SET CMP0054 NEW)
ENDIF()

# Add find modules to the path
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${libgit2_SOURCE_DIR}/cmake/Modules/")

INCLUDE(CheckLibraryExists)
INCLUDE(CheckFunctionExists)
INCLUDE(CheckSymbolExists)
INCLUDE(CheckStructHasMember)
INCLUDE(CheckPrototypeDefinition) # Added in CMake 3.0
INCLUDE(AddCFlagIfSupported)
INCLUDE(FindPkgLibraries)
INCLUDE(FindThreads)
INCLUDE(FindStatNsec)
INCLUDE(IdeSplitSources)
INCLUDE(FeatureSummary)
INCLUDE(EnableWarnings)

# Build options
#
OPTION(SONAME				"Set the (SO)VERSION of the target"			 ON)
OPTION(BUILD_SHARED_LIBS		"Build Shared Library (OFF for Static)"			 ON)
OPTION(THREADSAFE			"Build libgit2 as threadsafe"				 ON)
OPTION(BUILD_CLAR			"Build Tests using the Clar suite"			 ON)
OPTION(BUILD_EXAMPLES			"Build library usage example apps"			OFF)
OPTION(BUILD_FUZZERS			"Build the fuzz targets"				OFF)
OPTION(TAGS				"Generate tags"						OFF)
OPTION(PROFILE				"Generate profiling information"			OFF)
OPTION(ENABLE_TRACE			"Enables tracing support"				ON)
OPTION(LIBGIT2_FILENAME			"Name of the produced binary"				OFF)
OPTION(USE_SSH				"Link with libssh2 to enable SSH support"		 ON)
OPTION(USE_HTTPS			"Enable HTTPS support. Can be set to a specific backend" ON)
OPTION(USE_SHA1				"Enable SHA1. Can be set to CollisionDetection(ON)/HTTPS/Generic" ON)
OPTION(USE_GSSAPI			"Link with libgssapi for SPNEGO auth"			OFF)
OPTION(USE_STANDALONE_FUZZERS		"Enable standalone fuzzers (compatible with gcc)"	OFF)
OPTION(USE_LEAK_CHECKER			"Run tests with leak checker"				OFF)
OPTION(VALGRIND				"Configure build for valgrind"				OFF)
OPTION(DEBUG_POOL			"Enable debug pool allocator"				OFF)
OPTION(ENABLE_WERROR			"Enable compilation with -Werror"			OFF)
OPTION(USE_BUNDLED_ZLIB    		"Use the bundled version of zlib"			OFF)
   SET(USE_HTTP_PARSER			"" CACHE STRING "Specifies the HTTP Parser implementation; either system or builtin.")
OPTION(DEPRECATE_HARD			"Do not include deprecated functions in the library"	OFF)
   SET(REGEX_BACKEND			"" CACHE STRING "Regular expression implementation. One of regcomp_l, pcre2, pcre, regcomp, or builtin.")

IF (UNIX)
	IF (NOT USE_HTTPS)
	    OPTION(USE_NTLMCLIENT		"Enable NTLM support on Unix."				OFF )
	ELSE()
	    OPTION(USE_NTLMCLIENT		"Enable NTLM support on Unix."				ON )
	ENDIF()
ENDIF()

IF (UNIX AND NOT APPLE)
	OPTION(ENABLE_REPRODUCIBLE_BUILDS "Enable reproducible builds"				OFF)
ENDIF()

IF (APPLE)
	OPTION(USE_ICONV		"Link with and use iconv library"			 ON)
ENDIF()

IF(MSVC)
	# This option must match the settings used in your program, in particular if you
	# are linking statically
	OPTION(STATIC_CRT		"Link the static CRT libraries"		 		 ON)

	# If you want to embed a copy of libssh2 into libgit2, pass a
	# path to libssh2
	OPTION(EMBED_SSH_PATH		"Path to libssh2 to embed (Windows)"			OFF)
ENDIF()


IF(WIN32)
	# By default, libgit2 is built with WinHTTP.  To use the built-in
	# HTTP transport, invoke CMake with the "-DWINHTTP=OFF" argument.
	OPTION(WINHTTP			"Use Win32 WinHTTP routines"				 ON)
ENDIF()

IF(MSVC)
	# Enable MSVC CRTDBG memory leak reporting when in debug mode.
	OPTION(MSVC_CRTDBG 		"Enable CRTDBG memory leak reporting"			OFF)
ENDIF()

FILE(STRINGS "${libgit2_SOURCE_DIR}/include/git2/version.h" GIT2_HEADER REGEX "^#define LIBGIT2_VERSION \"[^\"]*\"$")

STRING(REGEX REPLACE "^.*LIBGIT2_VERSION \"([0-9]+).*$" "\\1" LIBGIT2_VERSION_MAJOR "${GIT2_HEADER}")
STRING(REGEX REPLACE "^.*LIBGIT2_VERSION \"[0-9]+\\.([0-9]+).*$" "\\1" LIBGIT2_VERSION_MINOR  "${GIT2_HEADER}")
STRING(REGEX REPLACE "^.*LIBGIT2_VERSION \"[0-9]+\\.[0-9]+\\.([0-9]+).*$" "\\1" LIBGIT2_VERSION_REV "${GIT2_HEADER}")
SET(LIBGIT2_VERSION_STRING "${LIBGIT2_VERSION_MAJOR}.${LIBGIT2_VERSION_MINOR}.${LIBGIT2_VERSION_REV}")

FILE(STRINGS "${libgit2_SOURCE_DIR}/include/git2/version.h" GIT2_HEADER_SOVERSION REGEX "^#define LIBGIT2_SOVERSION [0-9]+$")
STRING(REGEX REPLACE "^.*LIBGIT2_SOVERSION ([0-9]+)$" "\\1" LIBGIT2_SOVERSION "${GIT2_HEADER_SOVERSION}")

IF (DEPRECATE_HARD)
	ADD_DEFINITIONS(-DGIT_DEPRECATE_HARD)
ENDIF()

# Platform specific compilation flags
IF (MSVC)
	IF (STDCALL)
		MESSAGE(FATAL_ERROR "The STDCALL option is no longer supported; libgit2 is now always built as a cdecl library.  If you're using PInvoke, please add the CallingConventions.Cdecl attribute for support.")
	ENDIF()

	ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
	ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
	ADD_DEFINITIONS(-D_CRT_NONSTDC_NO_DEPRECATE)

	STRING(REPLACE "/Zm1000" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")

	# /GF - String pooling
	# /MP - Parallel build
	SET(CMAKE_C_FLAGS "/GF /MP /nologo ${CMAKE_C_FLAGS}")

	# /Gd - explicitly set cdecl calling convention
	SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /Gd")

	IF (STATIC_CRT)
		SET(CRT_FLAG_DEBUG "/MTd")
		SET(CRT_FLAG_RELEASE "/MT")
	ELSE()
		SET(CRT_FLAG_DEBUG "/MDd")
		SET(CRT_FLAG_RELEASE "/MD")
	ENDIF()

	IF (MSVC_CRTDBG)
		SET(GIT_MSVC_CRTDBG 1)
		SET(CRT_FLAG_DEBUG "${CRT_FLAG_DEBUG}")
		SET(CMAKE_C_STANDARD_LIBRARIES "${CMAKE_C_STANDARD_LIBRARIES} Dbghelp.lib")
	ENDIF()

	# /Zi - Create debugging information
	# /Od - Disable optimization
	# /D_DEBUG - #define _DEBUG
	# /MTd - Statically link the multithreaded debug version of the CRT
	# /MDd - Dynamically link the multithreaded debug version of the CRT
	# /RTC1 - Run time checks
	SET(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Zi /Od /D_DEBUG /RTC1 ${CRT_FLAG_DEBUG}")

	# /DNDEBUG - Disables asserts
	# /MT - Statically link the multithreaded release version of the CRT
	# /MD - Dynamically link the multithreaded release version of the CRT
	# /O2 - Optimize for speed
	# /Oy - Enable frame pointer omission (FPO) (otherwise CMake will automatically turn it off)
	# /GL - Link time code generation (whole program optimization)
	# /Gy - Function-level linking
	SET(CMAKE_C_FLAGS_RELEASE "/DNDEBUG /O2 /Oy /GL /Gy ${CRT_FLAG_RELEASE}")

	# /Oy- - Disable frame pointer omission (FPO)
	SET(CMAKE_C_FLAGS_RELWITHDEBINFO "/DNDEBUG /Zi /O2 /Oy- /GL /Gy ${CRT_FLAG_RELEASE}")

	# /O1 - Optimize for size
	SET(CMAKE_C_FLAGS_MINSIZEREL "/DNDEBUG /O1 /Oy /GL /Gy ${CRT_FLAG_RELEASE}")

	# /IGNORE:4221 - Ignore empty compilation units
	SET(CMAKE_STATIC_LINKER_FLAGS "/IGNORE:4221")

	# /DYNAMICBASE - Address space load randomization (ASLR)
	# /NXCOMPAT - Data execution prevention (DEP)
	# /LARGEADDRESSAWARE - >2GB user address space on x86
	# /VERSION - Embed version information in PE header
	SET(CMAKE_EXE_LINKER_FLAGS "/DYNAMICBASE /NXCOMPAT /LARGEADDRESSAWARE /VERSION:${LIBGIT2_VERSION_MAJOR}.${LIBGIT2_VERSION_MINOR}")

	# /DEBUG - Create a PDB
	# /LTCG - Link time code generation (whole program optimization)
	# /OPT:REF /OPT:ICF - Fold out duplicate code at link step
	# /INCREMENTAL:NO - Required to use /LTCG
	# /DEBUGTYPE:cv,fixup - Additional data embedded in the PDB (requires /INCREMENTAL:NO, so not on for Debug)
	SET(CMAKE_EXE_LINKER_FLAGS_DEBUG "/DEBUG")
	SET(CMAKE_EXE_LINKER_FLAGS_RELEASE "/RELEASE /LTCG /OPT:REF /OPT:ICF /INCREMENTAL:NO")
	SET(CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO "/DEBUG /RELEASE /LTCG /OPT:REF /OPT:ICF /INCREMENTAL:NO /DEBUGTYPE:cv,fixup")
	SET(CMAKE_EXE_LINKER_FLAGS_MINSIZEREL "/RELEASE /LTCG /OPT:REF /OPT:ICF /INCREMENTAL:NO")

	# Same linker settings for DLL as EXE
	SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
	SET(CMAKE_SHARED_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG}")
	SET(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE}")
	SET(CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO "${CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO}")
	SET(CMAKE_SHARED_LINKER_FLAGS_MINSIZEREL "${CMAKE_EXE_LINKER_FLAGS_MINSIZEREL}")
ELSE ()
	IF (ENABLE_REPRODUCIBLE_BUILDS)
		SET(CMAKE_C_ARCHIVE_CREATE "<CMAKE_AR> Dqc <TARGET> <LINK_FLAGS> <OBJECTS>")
		SET(CMAKE_C_ARCHIVE_APPEND "<CMAKE_AR> Dq  <TARGET> <LINK_FLAGS> <OBJECTS>")
		SET(CMAKE_C_ARCHIVE_FINISH "<CMAKE_RANLIB> -D <TARGET>")
	ENDIF()

	SET(CMAKE_C_FLAGS "-D_GNU_SOURCE ${CMAKE_C_FLAGS}")

	ENABLE_WARNINGS(all)
	ENABLE_WARNINGS(extra)

	IF (CMAKE_SYSTEM_NAME MATCHES "(Solaris|SunOS)")
		SET(CMAKE_C_FLAGS "-D_POSIX_C_SOURCE=200112L -D__EXTENSIONS__ -D_POSIX_PTHREAD_SEMANTICS ${CMAKE_C_FLAGS}")
	ENDIF()

	SET(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -D_DEBUG -O0")

	IF (MINGW OR MSYS) # MinGW and MSYS always do PIC and complain if we tell them to
		STRING(REGEX REPLACE "-fPIC" "" CMAKE_SHARED_LIBRARY_C_FLAGS "${CMAKE_SHARED_LIBRARY_C_FLAGS}")
	ELSEIF (BUILD_SHARED_LIBS)
		ADD_C_FLAG_IF_SUPPORTED(-fvisibility=hidden)

		SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
	ENDIF ()

	IF (MINGW)
		# MinGW >= 3.14 uses the C99-style stdio functions
		# automatically, but forks like mingw-w64 still want
		# us to define this in order to use them
		ADD_DEFINITIONS(-D__USE_MINGW_ANSI_STDIO=1)
	ENDIF ()

	ENABLE_WARNINGS(documentation)
	DISABLE_WARNINGS(missing-field-initializers)
	ENABLE_WARNINGS(strict-aliasing)
	ENABLE_WARNINGS(strict-prototypes)
	ENABLE_WARNINGS(declaration-after-statement)
	ENABLE_WARNINGS(shift-count-overflow)
	ENABLE_WARNINGS(unused-const-variable)
	ENABLE_WARNINGS(unused-function)
	ENABLE_WARNINGS(int-conversion)

	# MinGW uses gcc, which expects POSIX formatting for printf, but
	# uses the Windows C library, which uses its own format specifiers.
	# Disable format specifier warnings.
	IF(MINGW)
		DISABLE_WARNINGS(format)
		DISABLE_WARNINGS(format-security)
	ELSE()
		ENABLE_WARNINGS(format)
		ENABLE_WARNINGS(format-security)
	ENDIF()

	IF("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
		DISABLE_WARNINGS(documentation-deprecated-sync)
	ENDIF()

	IF (PROFILE)
		SET(CMAKE_C_FLAGS "-pg ${CMAKE_C_FLAGS}")
		SET(CMAKE_EXE_LINKER_FLAGS "-pg ${CMAKE_EXE_LINKER_FLAGS}")
	ENDIF ()
ENDIF()

# Ensure that MinGW provides the correct header files.
IF (WIN32 AND NOT CYGWIN)
	ADD_DEFINITIONS(-DWIN32 -D_WIN32_WINNT=0x0600)
ENDIF()

IF( NOT CMAKE_CONFIGURATION_TYPES )
	# Build Debug by default
	IF (NOT CMAKE_BUILD_TYPE)
		SET(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
	ENDIF ()
ELSE()
	# Using a multi-configuration generator eg MSVC or Xcode
	# that uses CMAKE_CONFIGURATION_TYPES and not CMAKE_BUILD_TYPE
ENDIF()

IF(BUILD_FUZZERS AND NOT USE_STANDALONE_FUZZERS)
	# The actual sanitizer link target will be added when linking the fuzz
	# targets.
	SET(CMAKE_REQUIRED_FLAGS "-fsanitize=fuzzer-no-link")
	ADD_C_FLAG(-fsanitize=fuzzer-no-link)
	UNSET(CMAKE_REQUIRED_FLAGS)
ENDIF ()

ADD_SUBDIRECTORY(src)

# Tests
IF (NOT MSVC)
   IF (NOT BUILD_SHARED_LIBS)
       SET(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
   ENDIF()
ENDIF ()

IF (BUILD_CLAR)
	ENABLE_TESTING()
	ADD_SUBDIRECTORY(tests)
ENDIF ()

IF (TAGS)
	FIND_PROGRAM(CTAGS ctags)
	IF (NOT CTAGS)
		MESSAGE(FATAL_ERROR "Could not find ctags command")
	ENDIF ()

	FILE(GLOB_RECURSE SRC_ALL *.[ch])

	ADD_CUSTOM_COMMAND(
		OUTPUT tags
		COMMAND ${CTAGS} -a ${SRC_ALL}
		DEPENDS ${SRC_ALL}
	)
	ADD_CUSTOM_TARGET(
		do_tags ALL
		DEPENDS tags
	)
ENDIF ()

IF (BUILD_EXAMPLES)
	ADD_SUBDIRECTORY(examples)
ENDIF ()

IF(BUILD_FUZZERS)
	IF(NOT USE_STANDALONE_FUZZERS)
		IF(BUILD_EXAMPLES)
			MESSAGE(FATAL_ERROR "Cannot build the fuzzer targets and the examples together")
		ENDIF()
		IF(BUILD_CLAR)
			MESSAGE(FATAL_ERROR "Cannot build the fuzzer targets and the tests together")
		ENDIF()
	ENDIF()
	ADD_SUBDIRECTORY(fuzzers)
ENDIF()

FEATURE_SUMMARY(WHAT ENABLED_FEATURES DESCRIPTION "Enabled features:")
FEATURE_SUMMARY(WHAT DISABLED_FEATURES DESCRIPTION "Disabled features:")
