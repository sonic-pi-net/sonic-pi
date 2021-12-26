INCLUDE(SanitizeBool)

# We try to find any packages our backends might use
FIND_PACKAGE(GSSAPI)
IF (CMAKE_SYSTEM_NAME MATCHES "Darwin")
	INCLUDE(FindGSSFramework)
ENDIF()

IF(USE_GSSAPI)
	# Auto-select GSS backend
	SanitizeBool(USE_GSSAPI)
	IF (USE_GSSAPI STREQUAL ON)
		IF (GSSFRAMEWORK_FOUND)
			SET(USE_GSSAPI "GSS.framework")
		ELSEIF(GSSAPI_FOUND)
			SET(USE_GSSAPI "gssapi")
		ELSE()
			MESSAGE(FATAL_ERROR "Unable to autodetect a usable GSS backend."
				"Please pass the backend name explicitly (-DUSE_GSS=backend)")
		ENDIF()
	ENDIF()

	# Check that we can find what's required for the selected backend
	IF (USE_GSSAPI STREQUAL "GSS.framework")
		IF (NOT GSSFRAMEWORK_FOUND)
			MESSAGE(FATAL_ERROR "Asked for GSS.framework backend, but it wasn't found")
		ENDIF()

		LIST(APPEND LIBGIT2_LIBS ${GSSFRAMEWORK_LIBRARIES})

		SET(GIT_GSSFRAMEWORK 1)
		ADD_FEATURE_INFO(SPNEGO GIT_GSSFRAMEWORK "SPNEGO authentication support (${USE_GSSAPI})")
	ELSEIF (USE_GSSAPI STREQUAL "gssapi")
		IF (NOT GSSAPI_FOUND)
			MESSAGE(FATAL_ERROR "Asked for gssapi GSS backend, but it wasn't found")
		ENDIF()

		LIST(APPEND LIBGIT2_LIBS ${GSSAPI_LIBRARIES})

		SET(GIT_GSSAPI 1)
		ADD_FEATURE_INFO(SPNEGO GIT_GSSAPI "SPNEGO authentication support (${USE_GSSAPI})")
	ELSE()
		MESSAGE(FATAL_ERROR "Asked for backend ${USE_GSSAPI} but it wasn't found")
	ENDIF()
ELSE()
	SET(GIT_GSSAPI 0)
	ADD_FEATURE_INFO(SPNEGO NO "SPNEGO authentication support")
ENDIF()
