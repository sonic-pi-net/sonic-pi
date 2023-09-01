# The main libgit2 source tree: this CMakeLists.txt identifies platform
# support and includes the subprojects that make up core libgit2 support.

#
# Optional build configuration settings
#

if(DEPRECATE_HARD)
        add_definitions(-DGIT_DEPRECATE_HARD)
endif()

if(USE_LEAK_CHECKER STREQUAL "valgrind")
	add_definitions(-DVALGRIND)
endif()

#
# Optional debugging functionality
#

if(DEBUG_POOL)
	set(GIT_DEBUG_POOL 1)
endif()
add_feature_info(debugpool GIT_DEBUG_POOL "debug pool allocator")

if(DEBUG_STRICT_ALLOC)
	set(GIT_DEBUG_STRICT_ALLOC 1)
endif()
add_feature_info(debugalloc GIT_DEBUG_STRICT_ALLOC "debug strict allocators")

if(DEBUG_STRICT_OPEN)
	set(GIT_DEBUG_STRICT_OPEN 1)
endif()
add_feature_info(debugopen GIT_DEBUG_STRICT_OPEN "path validation in open")

#
# Optional feature enablement
#

include(SelectGSSAPI)
include(SelectHTTPSBackend)
include(SelectHashes)
include(SelectHTTPParser)
include(SelectRegex)
include(SelectSSH)
include(SelectWinHTTP)
include(SelectZlib)

#
# Platform support
#

# futimes/futimens

if(HAVE_FUTIMENS)
	set(GIT_USE_FUTIMENS 1)
endif ()
add_feature_info(futimens GIT_USE_FUTIMENS "futimens support")

# qsort

check_prototype_definition(qsort_r
	"void qsort_r(void *base, size_t nmemb, size_t size, void *thunk, int (*compar)(void *, const void *, const void *))"
	"" "stdlib.h" GIT_QSORT_R_BSD)

check_prototype_definition(qsort_r
	"void qsort_r(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *, void *), void *arg)"
	"" "stdlib.h" GIT_QSORT_R_GNU)

check_function_exists(qsort_s GIT_QSORT_S)

# random / entropy data

check_function_exists(getentropy GIT_RAND_GETENTROPY)
check_function_exists(getloadavg GIT_RAND_GETLOADAVG)

# determine architecture of the machine

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
	set(GIT_ARCH_64 1)
elseif(CMAKE_SIZEOF_VOID_P EQUAL 4)
	set(GIT_ARCH_32 1)
elseif(CMAKE_SIZEOF_VOID_P)
	message(FATAL_ERROR "Unsupported architecture (pointer size is ${CMAKE_SIZEOF_VOID_P} bytes)")
else()
	message(FATAL_ERROR "Unsupported architecture (CMAKE_SIZEOF_VOID_P is unset)")
endif()

# nanosecond mtime/ctime support

if(USE_NSEC)
	set(GIT_USE_NSEC 1)
endif()

# high-resolution stat support

if(HAVE_STRUCT_STAT_ST_MTIM)
	set(GIT_USE_STAT_MTIM 1)
elseif(HAVE_STRUCT_STAT_ST_MTIMESPEC)
	set(GIT_USE_STAT_MTIMESPEC 1)
elseif(HAVE_STRUCT_STAT_ST_MTIME_NSEC)
	set(GIT_USE_STAT_MTIME_NSEC 1)
endif()

# realtime support

check_library_exists(rt clock_gettime "time.h" NEED_LIBRT)
if(NEED_LIBRT)
	list(APPEND LIBGIT2_SYSTEM_LIBS rt)
	list(APPEND LIBGIT2_PC_LIBS "-lrt")
endif()

# platform libraries

if(WIN32)
	list(APPEND LIBGIT2_SYSTEM_LIBS ws2_32)
endif()

if(CMAKE_SYSTEM_NAME MATCHES "(Solaris|SunOS)")
	list(APPEND LIBGIT2_SYSTEM_LIBS socket nsl)
	list(APPEND LIBGIT2_PC_LIBS "-lsocket" "-lnsl")
endif()

if(CMAKE_SYSTEM_NAME MATCHES "Haiku")
	list(APPEND LIBGIT2_SYSTEM_LIBS gnu network)
	list(APPEND LIBGIT2_PC_LIBS "-lgnu -lnetwork")
endif()

if(AMIGA)
	add_definitions(-DNO_ADDRINFO -DNO_READDIR_R -DNO_MMAP)
endif()

# threads

if(USE_THREADS)
	if(NOT WIN32)
		find_package(Threads REQUIRED)
		list(APPEND LIBGIT2_SYSTEM_LIBS ${CMAKE_THREAD_LIBS_INIT})
		list(APPEND LIBGIT2_PC_LIBS ${CMAKE_THREAD_LIBS_INIT})
	endif()

	set(GIT_THREADS 1)
endif()
add_feature_info(threadsafe USE_THREADS "threadsafe support")

#
# Optional bundled features
#

# ntlmclient
if(USE_NTLMCLIENT)
	set(GIT_NTLM 1)
	add_subdirectory("${PROJECT_SOURCE_DIR}/deps/ntlmclient" "${PROJECT_BINARY_DIR}/deps/ntlmclient")
	list(APPEND LIBGIT2_DEPENDENCY_INCLUDES "${PROJECT_SOURCE_DIR}/deps/ntlmclient")
	list(APPEND LIBGIT2_DEPENDENCY_OBJECTS "$<TARGET_OBJECTS:ntlmclient>")
endif()
add_feature_info(ntlmclient GIT_NTLM "NTLM authentication support for Unix")

#
# Optional external dependencies

# iconv
if(USE_ICONV)
	find_package(Iconv)
endif()
if(ICONV_FOUND)
	set(GIT_USE_ICONV 1)
	list(APPEND LIBGIT2_SYSTEM_INCLUDES ${ICONV_INCLUDE_DIR})
	list(APPEND LIBGIT2_SYSTEM_LIBS ${ICONV_LIBRARIES})
	list(APPEND LIBGIT2_PC_LIBS ${ICONV_LIBRARIES})
endif()
add_feature_info(iconv GIT_USE_ICONV "iconv encoding conversion support")

#
# Include child projects
#

add_subdirectory(libgit2)
add_subdirectory(util)

if(BUILD_CLI)
	add_subdirectory(cli)
endif()

# re-export these to the root so that peer projects (tests, fuzzers,
# examples) can use them
set(LIBGIT2_INCLUDES ${LIBGIT2_INCLUDES} PARENT_SCOPE)
set(LIBGIT2_OBJECTS ${LIBGIT2_OBJECTS} PARENT_SCOPE)
set(LIBGIT2_DEPENDENCY_INCLUDES ${LIBGIT2_DEPENDENCY_INCLUDES} PARENT_SCOPE)
set(LIBGIT2_DEPENDENCY_OBJECTS ${LIBGIT2_DEPENDENCY_OBJECTS} PARENT_SCOPE)
set(LIBGIT2_SYSTEM_INCLUDES ${LIBGIT2_SYSTEM_INCLUDES} PARENT_SCOPE)
set(LIBGIT2_SYSTEM_LIBS ${LIBGIT2_SYSTEM_LIBS} PARENT_SCOPE)
