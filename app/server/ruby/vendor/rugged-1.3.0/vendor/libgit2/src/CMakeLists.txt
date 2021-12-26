add_library(git2internal OBJECT)
set_target_properties(git2internal PROPERTIES C_STANDARD 90)

IF(DEBUG_POOL)
	SET(GIT_DEBUG_POOL 1)
ENDIF()
ADD_FEATURE_INFO(debugpool GIT_DEBUG_POOL "debug pool allocator")

IF(DEBUG_STRICT_ALLOC)
	SET(GIT_DEBUG_STRICT_ALLOC 1)
ENDIF()
ADD_FEATURE_INFO(debugalloc GIT_DEBUG_STRICT_ALLOC "debug strict allocators")

IF(DEBUG_STRICT_OPEN)
	SET(GIT_DEBUG_STRICT_OPEN 1)
ENDIF()
ADD_FEATURE_INFO(debugopen GIT_DEBUG_STRICT_OPEN "path validation in open")

INCLUDE(PkgBuildConfig)
INCLUDE(SanitizeBool)

# This variable will contain the libraries we need to put into
# libgit2.pc's Requires.private. That is, what we're linking to or
# what someone who's statically linking us needs to link to.
SET(LIBGIT2_PC_REQUIRES "")
# This will be set later if we use the system's http-parser library or
# use iconv (OSX) and will be written to the Libs.private field in the
# pc file.
SET(LIBGIT2_PC_LIBS "")

SET(LIBGIT2_INCLUDES
	"${CMAKE_CURRENT_BINARY_DIR}"
	"${libgit2_SOURCE_DIR}/src"
	"${libgit2_SOURCE_DIR}/include")
SET(LIBGIT2_SYSTEM_INCLUDES "")
SET(LIBGIT2_LIBS "")

enable_warnings(missing-declarations)

# Enable tracing
IF(ENABLE_TRACE)
	SET(GIT_TRACE 1)
ENDIF()
ADD_FEATURE_INFO(tracing GIT_TRACE "tracing support")

IF (HAVE_FUTIMENS)
	SET(GIT_USE_FUTIMENS 1)
ENDIF ()
ADD_FEATURE_INFO(futimens GIT_USE_FUTIMENS "futimens support")

CHECK_PROTOTYPE_DEFINITION(qsort_r
	"void qsort_r(void *base, size_t nmemb, size_t size, void *thunk, int (*compar)(void *, const void *, const void *))"
	"" "stdlib.h" HAVE_QSORT_R_BSD)
IF (HAVE_QSORT_R_BSD)
	target_compile_definitions(git2internal PRIVATE HAVE_QSORT_R_BSD)
ENDIF()

CHECK_PROTOTYPE_DEFINITION(qsort_r
	"void qsort_r(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *, void *), void *arg)"
	"" "stdlib.h" HAVE_QSORT_R_GNU)
IF (HAVE_QSORT_R_GNU)
	target_compile_definitions(git2internal PRIVATE HAVE_QSORT_R_GNU)
ENDIF()

CHECK_FUNCTION_EXISTS(qsort_s HAVE_QSORT_S)
IF (HAVE_QSORT_S)
	target_compile_definitions(git2internal PRIVATE HAVE_QSORT_S)
ENDIF ()

# Find required dependencies

IF(WIN32)
	LIST(APPEND LIBGIT2_LIBS ws2_32)
ELSEIF(CMAKE_SYSTEM_NAME MATCHES "(Solaris|SunOS)")
	LIST(APPEND LIBGIT2_LIBS socket nsl)
	LIST(APPEND LIBGIT2_PC_LIBS "-lsocket" "-lnsl")
ELSEIF(CMAKE_SYSTEM_NAME MATCHES "Haiku")
	LIST(APPEND LIBGIT2_LIBS network)
	LIST(APPEND LIBGIT2_PC_LIBS "-lnetwork")
ENDIF()

CHECK_LIBRARY_EXISTS(rt clock_gettime "time.h" NEED_LIBRT)
IF(NEED_LIBRT)
	LIST(APPEND LIBGIT2_LIBS rt)
	LIST(APPEND LIBGIT2_PC_LIBS "-lrt")
ENDIF()

IF(THREADSAFE)
	LIST(APPEND LIBGIT2_LIBS ${CMAKE_THREAD_LIBS_INIT})
	LIST(APPEND LIBGIT2_PC_LIBS ${CMAKE_THREAD_LIBS_INIT})
ENDIF()
ADD_FEATURE_INFO(threadsafe THREADSAFE "threadsafe support")


if(WIN32 AND EMBED_SSH_PATH)
	file(GLOB SRC_SSH "${EMBED_SSH_PATH}/src/*.c")
	list(SORT SRC_SSH)
	target_sources(git2internal PRIVATE ${SRC_SSH})

	list(APPEND LIBGIT2_SYSTEM_INCLUDES "${EMBED_SSH_PATH}/include")
	file(WRITE "${EMBED_SSH_PATH}/src/libssh2_config.h" "#define HAVE_WINCNG\n#define LIBSSH2_WINCNG\n#include \"../win32/libssh2_config.h\"")
	set(GIT_SSH 1)
endif()

IF (WIN32 AND WINHTTP)
	SET(GIT_WINHTTP 1)

	# Since MinGW does not come with headers or an import library for winhttp,
	# we have to include a private header and generate our own import library
	IF (MINGW)
		ADD_SUBDIRECTORY("${libgit2_SOURCE_DIR}/deps/winhttp" "${libgit2_BINARY_DIR}/deps/winhttp")
		LIST(APPEND LIBGIT2_LIBS winhttp)
		LIST(APPEND LIBGIT2_INCLUDES "${libgit2_SOURCE_DIR}/deps/winhttp")
	ELSE()
		LIST(APPEND LIBGIT2_LIBS "winhttp")
		LIST(APPEND LIBGIT2_PC_LIBS "-lwinhttp")
	ENDIF ()

	LIST(APPEND LIBGIT2_LIBS "rpcrt4" "crypt32" "ole32")
	LIST(APPEND LIBGIT2_PC_LIBS "-lrpcrt4" "-lcrypt32" "-lole32")
ENDIF()

include(SelectHTTPSBackend)
include(SelectHashes)
target_sources(git2internal PRIVATE ${SRC_SHA1})

# Specify regular expression implementation
FIND_PACKAGE(PCRE)

IF(REGEX_BACKEND STREQUAL "")
	CHECK_SYMBOL_EXISTS(regcomp_l "regex.h;xlocale.h" HAVE_REGCOMP_L)

	IF(HAVE_REGCOMP_L)
		SET(REGEX_BACKEND "regcomp_l")
	ELSEIF(PCRE_FOUND)
		SET(REGEX_BACKEND "pcre")
	ELSE()
		SET(REGEX_BACKEND "builtin")
	ENDIF()
ENDIF()

IF(REGEX_BACKEND STREQUAL "regcomp_l")
	ADD_FEATURE_INFO(regex ON "using system regcomp_l")
	SET(GIT_REGEX_REGCOMP_L 1)
ELSEIF(REGEX_BACKEND STREQUAL "pcre2")
	FIND_PACKAGE(PCRE2)

	IF(NOT PCRE2_FOUND)
		MESSAGE(FATAL_ERROR "PCRE2 support was requested but not found")
	ENDIF()

	ADD_FEATURE_INFO(regex ON "using system PCRE2")
	SET(GIT_REGEX_PCRE2 1)

	LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${PCRE2_INCLUDE_DIRS})
	LIST(APPEND LIBGIT2_LIBS ${PCRE2_LIBRARIES})
	LIST(APPEND LIBGIT2_PC_REQUIRES "libpcre2-8")
ELSEIF(REGEX_BACKEND STREQUAL "pcre")
	ADD_FEATURE_INFO(regex ON "using system PCRE")
	SET(GIT_REGEX_PCRE 1)

	LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${PCRE_INCLUDE_DIRS})
	LIST(APPEND LIBGIT2_LIBS ${PCRE_LIBRARIES})
	LIST(APPEND LIBGIT2_PC_REQUIRES "libpcre")
ELSEIF(REGEX_BACKEND STREQUAL "regcomp")
	ADD_FEATURE_INFO(regex ON "using system regcomp")
	SET(GIT_REGEX_REGCOMP 1)
ELSEIF(REGEX_BACKEND STREQUAL "builtin")
	ADD_FEATURE_INFO(regex ON "using bundled PCRE")
	SET(GIT_REGEX_BUILTIN 1)

	ADD_SUBDIRECTORY("${libgit2_SOURCE_DIR}/deps/pcre" "${libgit2_BINARY_DIR}/deps/pcre")
	LIST(APPEND LIBGIT2_INCLUDES "${libgit2_SOURCE_DIR}/deps/pcre")
	LIST(APPEND LIBGIT2_OBJECTS $<TARGET_OBJECTS:pcre>)
ELSE()
	MESSAGE(FATAL_ERROR "The REGEX_BACKEND option provided is not supported")
ENDIF()

# Optional external dependency: http-parser
IF(USE_HTTP_PARSER STREQUAL "system")
	FIND_PACKAGE(HTTP_Parser)

	IF (HTTP_PARSER_FOUND AND HTTP_PARSER_VERSION_MAJOR EQUAL 2)
		LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${HTTP_PARSER_INCLUDE_DIRS})
		LIST(APPEND LIBGIT2_LIBS ${HTTP_PARSER_LIBRARIES})
		LIST(APPEND LIBGIT2_PC_LIBS "-lhttp_parser")
		ADD_FEATURE_INFO(http-parser ON "http-parser support (system)")
	ELSE()
		MESSAGE(FATAL_ERROR "http-parser support was requested but not found")
	ENDIF()
ELSE()
	MESSAGE(STATUS "http-parser version 2 was not found or disabled; using bundled 3rd-party sources.")
	ADD_SUBDIRECTORY("${libgit2_SOURCE_DIR}/deps/http-parser" "${libgit2_BINARY_DIR}/deps/http-parser")
	LIST(APPEND LIBGIT2_INCLUDES "${libgit2_SOURCE_DIR}/deps/http-parser")
	LIST(APPEND LIBGIT2_OBJECTS "$<TARGET_OBJECTS:http-parser>")
	ADD_FEATURE_INFO(http-parser ON "http-parser support (bundled)")
ENDIF()

# Optional external dependency: zlib
SanitizeBool(USE_BUNDLED_ZLIB)
IF(USE_BUNDLED_ZLIB STREQUAL ON)
	SET(USE_BUNDLED_ZLIB "Bundled")
ENDIF()

IF(USE_BUNDLED_ZLIB STREQUAL "OFF")
	FIND_PACKAGE(ZLIB)
	IF(ZLIB_FOUND)
		LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${ZLIB_INCLUDE_DIRS})
		LIST(APPEND LIBGIT2_LIBS ${ZLIB_LIBRARIES})
		IF(APPLE OR CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
			LIST(APPEND LIBGIT2_PC_LIBS "-lz")
		ELSE()
			LIST(APPEND LIBGIT2_PC_REQUIRES "zlib")
		ENDIF()
		ADD_FEATURE_INFO(zlib ON "using system zlib")
	ELSE()
		MESSAGE(STATUS "zlib was not found; using bundled 3rd-party sources." )
	ENDIF()
ENDIF()
IF(USE_BUNDLED_ZLIB STREQUAL "Chromium")
	ADD_SUBDIRECTORY("${libgit2_SOURCE_DIR}/deps/chromium-zlib" "${libgit2_BINARY_DIR}/deps/chromium-zlib")
	LIST(APPEND LIBGIT2_INCLUDES "${libgit2_SOURCE_DIR}/deps/chromium-zlib")
	LIST(APPEND LIBGIT2_OBJECTS $<TARGET_OBJECTS:chromium_zlib>)
	ADD_FEATURE_INFO(zlib ON "using (Chromium) bundled zlib")
ELSEIF(USE_BUNDLED_ZLIB OR NOT ZLIB_FOUND)
	ADD_SUBDIRECTORY("${libgit2_SOURCE_DIR}/deps/zlib" "${libgit2_BINARY_DIR}/deps/zlib")
	LIST(APPEND LIBGIT2_INCLUDES "${libgit2_SOURCE_DIR}/deps/zlib")
	LIST(APPEND LIBGIT2_OBJECTS $<TARGET_OBJECTS:zlib>)
	ADD_FEATURE_INFO(zlib ON "using bundled zlib")
ENDIF()

# Optional external dependency: libssh2
IF (USE_SSH)
	FIND_PKGLIBRARIES(LIBSSH2 libssh2)
	IF (NOT LIBSSH2_FOUND)
		FIND_PACKAGE(LibSSH2)
		SET(LIBSSH2_INCLUDE_DIRS ${LIBSSH2_INCLUDE_DIR})
		GET_FILENAME_COMPONENT(LIBSSH2_LIBRARY_DIRS "${LIBSSH2_LIBRARY}" DIRECTORY)
		SET(LIBSSH2_LIBRARIES ${LIBSSH2_LIBRARY})
		SET(LIBSSH2_LDFLAGS "-lssh2")
	ENDIF()
ENDIF()
IF (LIBSSH2_FOUND)
	SET(GIT_SSH 1)
	LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${LIBSSH2_INCLUDE_DIRS})
	LIST(APPEND LIBGIT2_LIBS ${LIBSSH2_LIBRARIES})
	LIST(APPEND LIBGIT2_PC_LIBS ${LIBSSH2_LDFLAGS})

	CHECK_LIBRARY_EXISTS("${LIBSSH2_LIBRARIES}" libssh2_userauth_publickey_frommemory "${LIBSSH2_LIBRARY_DIRS}" HAVE_LIBSSH2_MEMORY_CREDENTIALS)
	IF (HAVE_LIBSSH2_MEMORY_CREDENTIALS)
		SET(GIT_SSH_MEMORY_CREDENTIALS 1)
	ENDIF()
ELSE()
	MESSAGE(STATUS "LIBSSH2 not found. Set CMAKE_PREFIX_PATH if it is installed outside of the default search path.")
ENDIF()
ADD_FEATURE_INFO(SSH GIT_SSH "SSH transport support")

# Optional external dependency: ntlmclient
IF (USE_NTLMCLIENT)
	SET(GIT_NTLM 1)
	ADD_SUBDIRECTORY("${libgit2_SOURCE_DIR}/deps/ntlmclient" "${libgit2_BINARY_DIR}/deps/ntlmclient")
	LIST(APPEND LIBGIT2_INCLUDES "${libgit2_SOURCE_DIR}/deps/ntlmclient")
	LIST(APPEND LIBGIT2_OBJECTS "$<TARGET_OBJECTS:ntlmclient>")
ENDIF()
ADD_FEATURE_INFO(ntlmclient GIT_NTLM "NTLM authentication support for Unix")

# Optional external dependency: GSSAPI

INCLUDE(SelectGSSAPI)

# Optional external dependency: iconv
IF (USE_ICONV)
	FIND_PACKAGE(Iconv)
ENDIF()
IF (ICONV_FOUND)
	SET(GIT_USE_ICONV 1)
	LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${ICONV_INCLUDE_DIR})
	LIST(APPEND LIBGIT2_LIBS ${ICONV_LIBRARIES})
	LIST(APPEND LIBGIT2_PC_LIBS ${ICONV_LIBRARIES})
ENDIF()
ADD_FEATURE_INFO(iconv GIT_USE_ICONV "iconv encoding conversion support")


IF (THREADSAFE)
	IF (NOT WIN32)
		FIND_PACKAGE(Threads REQUIRED)
	ENDIF()

	SET(GIT_THREADS 1)
ENDIF()

IF (USE_NSEC)
	SET(GIT_USE_NSEC 1)
ENDIF()

IF (HAVE_STRUCT_STAT_ST_MTIM)
	SET(GIT_USE_STAT_MTIM 1)
ELSEIF (HAVE_STRUCT_STAT_ST_MTIMESPEC)
	SET(GIT_USE_STAT_MTIMESPEC 1)
ELSEIF (HAVE_STRUCT_STAT_ST_MTIME_NSEC)
	SET(GIT_USE_STAT_MTIME_NSEC 1)
ENDIF()

target_compile_definitions(git2internal PRIVATE _FILE_OFFSET_BITS=64)

# Collect sourcefiles
file(GLOB SRC_H
	"${libgit2_SOURCE_DIR}/include/git2.h"
	"${libgit2_SOURCE_DIR}/include/git2/*.h"
	"${libgit2_SOURCE_DIR}/include/git2/sys/*.h")
list(SORT SRC_H)
target_sources(git2internal PRIVATE ${SRC_H})

# On Windows use specific platform sources
if(WIN32 AND NOT CYGWIN)
	SET(WIN_RC "win32/git2.rc")

	file(GLOB SRC_OS win32/*.c win32/*.h)
	list(SORT SRC_OS)
	target_sources(git2internal PRIVATE ${SRC_OS})
elseif(AMIGA)
	target_compile_definitions(git2internal PRIVATE NO_ADDRINFO NO_READDIR_R NO_MMAP)
else()
	file(GLOB SRC_OS unix/*.c unix/*.h)
	list(SORT SRC_OS)
	target_sources(git2internal PRIVATE ${SRC_OS})
endif()

IF (USE_LEAK_CHECKER STREQUAL "valgrind")
	target_compile_definitions(git2internal PRIVATE VALGRIND)
ENDIF()

file(GLOB SRC_GIT2 *.c *.h
	allocators/*.c allocators/*.h
	streams/*.c streams/*.h
	transports/*.c transports/*.h
	xdiff/*.c xdiff/*.h)
list(SORT SRC_GIT2)
target_sources(git2internal PRIVATE ${SRC_GIT2})

IF(APPLE)
	# The old Secure Transport API has been deprecated in macOS 10.15.
	SET_SOURCE_FILES_PROPERTIES(streams/stransport.c PROPERTIES COMPILE_FLAGS -Wno-deprecated)
ENDIF()

# the xdiff dependency is not (yet) warning-free, disable warnings as
# errors for the xdiff sources until we've sorted them out
IF(MSVC)
	SET_SOURCE_FILES_PROPERTIES(xdiff/xdiffi.c PROPERTIES COMPILE_FLAGS -WX-)
	SET_SOURCE_FILES_PROPERTIES(xdiff/xutils.c PROPERTIES COMPILE_FLAGS -WX-)
ENDIF()

# Determine architecture of the machine
IF (CMAKE_SIZEOF_VOID_P EQUAL 8)
	SET(GIT_ARCH_64 1)
ELSEIF (CMAKE_SIZEOF_VOID_P EQUAL 4)
	SET(GIT_ARCH_32 1)
ELSEIF (CMAKE_SIZEOF_VOID_P)
	MESSAGE(FATAL_ERROR "Unsupported architecture (pointer size is ${CMAKE_SIZEOF_VOID_P} bytes)")
ELSE()
	MESSAGE(FATAL_ERROR "Unsupported architecture (CMAKE_SIZEOF_VOID_P is unset)")
ENDIF()

CONFIGURE_FILE(features.h.in git2/sys/features.h)

IDE_SPLIT_SOURCES(git2internal)
LIST(APPEND LIBGIT2_OBJECTS $<TARGET_OBJECTS:git2internal>)

TARGET_INCLUDE_DIRECTORIES(git2internal PRIVATE ${LIBGIT2_INCLUDES} PUBLIC ${libgit2_SOURCE_DIR}/include)
TARGET_INCLUDE_DIRECTORIES(git2internal SYSTEM PRIVATE ${LIBGIT2_SYSTEM_INCLUDES})

SET(LIBGIT2_OBJECTS ${LIBGIT2_OBJECTS} PARENT_SCOPE)
SET(LIBGIT2_INCLUDES ${LIBGIT2_INCLUDES} PARENT_SCOPE)
SET(LIBGIT2_SYSTEM_INCLUDES ${LIBGIT2_SYSTEM_INCLUDES} PARENT_SCOPE)
SET(LIBGIT2_LIBS ${LIBGIT2_LIBS} PARENT_SCOPE)

IF(XCODE_VERSION)
	# This is required for Xcode to actually link the libgit2 library
	# when using only object libraries.
	FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/dummy.c "")
	LIST(APPEND LIBGIT2_OBJECTS ${CMAKE_CURRENT_BINARY_DIR}/dummy.c)
ENDIF()

# Compile and link libgit2
ADD_LIBRARY(git2 ${WIN_RC} ${LIBGIT2_OBJECTS})
TARGET_LINK_LIBRARIES(git2 ${LIBGIT2_LIBS})

SET_TARGET_PROPERTIES(git2 PROPERTIES C_STANDARD 90)
SET_TARGET_PROPERTIES(git2 PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${libgit2_BINARY_DIR})
SET_TARGET_PROPERTIES(git2 PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${libgit2_BINARY_DIR})
SET_TARGET_PROPERTIES(git2 PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${libgit2_BINARY_DIR})

# Workaround for Cmake bug #0011240 (see http://public.kitware.com/Bug/view.php?id=11240)
# Win64+MSVC+static libs = linker error
IF(MSVC AND GIT_ARCH_64 AND NOT BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(git2 PROPERTIES STATIC_LIBRARY_FLAGS "/MACHINE:x64")
ENDIF()

IDE_SPLIT_SOURCES(git2)

if(SONAME)
	set_target_properties(git2 PROPERTIES VERSION ${libgit2_VERSION})
	set_target_properties(git2 PROPERTIES SOVERSION "${libgit2_VERSION_MAJOR}.${libgit2_VERSION_MINOR}")
	if(LIBGIT2_FILENAME)
		target_compile_definitions(git2 PRIVATE LIBGIT2_FILENAME=\"${LIBGIT2_FILENAME}\")
		set_target_properties(git2 PROPERTIES OUTPUT_NAME ${LIBGIT2_FILENAME})
	elseif(DEFINED LIBGIT2_PREFIX)
		set_target_properties(git2 PROPERTIES PREFIX "${LIBGIT2_PREFIX}")
	endif()
endif()

PKG_BUILD_CONFIG(NAME libgit2
	VERSION ${libgit2_VERSION}
	DESCRIPTION "The git library, take 2"
	LIBS_SELF git2
	PRIVATE_LIBS ${LIBGIT2_PC_LIBS}
	REQUIRES ${LIBGIT2_PC_REQUIRES}
)

IF (MSVC_IDE)
   # Precompiled headers
   SET_TARGET_PROPERTIES(git2 PROPERTIES COMPILE_FLAGS "/Yuprecompiled.h /FIprecompiled.h")
   SET_SOURCE_FILES_PROPERTIES(win32/precompiled.c COMPILE_FLAGS "/Ycprecompiled.h")
ENDIF ()

# Install
INSTALL(TARGETS git2
	RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
	LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
	ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
INSTALL(DIRECTORY ${libgit2_SOURCE_DIR}/include/git2 DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
INSTALL(FILES ${libgit2_SOURCE_DIR}/include/git2.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
