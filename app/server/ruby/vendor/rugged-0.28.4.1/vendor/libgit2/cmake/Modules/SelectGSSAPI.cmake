# Select the backend to use

# We try to find any packages our backends might use

FIND_PACKAGE(GSSAPI)
IF (CMAKE_SYSTEM_NAME MATCHES "Darwin")
	INCLUDE(FindGSSFramework)
ENDIF()

# Auto-select GSS backend
IF (USE_GSSAPI STREQUAL ON)
	IF (GSSFRAMEWORK_FOUND)
		SET(GSS_BACKEND "GSS.framework")
	ELSEIF(GSSAPI_FOUND)
		SET(GSS_BACKEND "gssapi")
	ELSE()
		MESSAGE(FATAL_ERROR "Unable to autodetect a usable GSS backend."
			"Please pass the backend name explicitly (-DUSE_GSS=backend)")
	ENDIF()
ELSEIF(USE_GSSAPI)
	# Backend was explicitly set
	SET(GSS_BACKEND ${USE_GSSAPI})
ELSE()
	SET(GSS_BACKEND NO)
ENDIF()

IF(GSS_BACKEND)
	# Check that we can find what's required for the selected backend
	IF (GSS_BACKEND STREQUAL "GSS.framework")
		IF (NOT GSSFRAMEWORK_FOUND)
			MESSAGE(FATAL_ERROR "Asked for GSS.framework backend, but it wasn't found")
		ENDIF()

		LIST(APPEND LIBGIT2_LIBS ${GSSFRAMEWORK_LIBRARIES})

		SET(GIT_GSSFRAMEWORK 1)
		ADD_FEATURE_INFO(SPNEGO GIT_GSSFRAMEWORK "SPNEGO authentication support (${GSS_BACKEND})")
	ELSEIF (GSS_BACKEND STREQUAL "gssapi")
		IF (NOT GSSAPI_FOUND)
			MESSAGE(FATAL_ERROR "Asked for gssapi GSS backend, but it wasn't found")
		ENDIF()

		LIST(APPEND LIBGIT2_LIBS ${GSSAPI_LIBRARIES})

		SET(GIT_GSSAPI 1)
		ADD_FEATURE_INFO(SPNEGO GIT_GSSAPI "SPNEGO authentication support (${GSS_BACKEND})")
	ELSE()
		MESSAGE(FATAL_ERROR "Asked for backend ${GSS_BACKEND} but it wasn't found")
	ENDIF()
ELSE()
	SET(GIT_GSSAPI 0)
	ADD_FEATURE_INFO(SPNEGO NO "")
ENDIF()
