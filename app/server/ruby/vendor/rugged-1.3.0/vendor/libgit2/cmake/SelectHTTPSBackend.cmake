INCLUDE(SanitizeBool)

# We try to find any packages our backends might use
FIND_PACKAGE(OpenSSL)
FIND_PACKAGE(mbedTLS)
IF (CMAKE_SYSTEM_NAME MATCHES "Darwin")
	FIND_PACKAGE(Security)
	FIND_PACKAGE(CoreFoundation)
ENDIF()

IF(USE_HTTPS)
	# Auto-select TLS backend
	SanitizeBool(USE_HTTPS)
	IF (USE_HTTPS STREQUAL ON)
		IF (SECURITY_FOUND)
			IF (SECURITY_HAS_SSLCREATECONTEXT)
				SET(USE_HTTPS "SecureTransport")
			ELSE()
				MESSAGE(STATUS "Security framework is too old, falling back to OpenSSL")
				SET(USE_HTTPS "OpenSSL")
			ENDIF()
		ELSEIF (WINHTTP)
			SET(USE_HTTPS "WinHTTP")
		ELSEIF(OPENSSL_FOUND)
			SET(USE_HTTPS "OpenSSL")
		ELSEIF(MBEDTLS_FOUND)
			SET(USE_HTTPS "mbedTLS")
		ELSE()
			MESSAGE(FATAL_ERROR "Unable to autodetect a usable HTTPS backend."
				"Please pass the backend name explicitly (-DUSE_HTTPS=backend)")
		ENDIF()
	ENDIF()

	# Check that we can find what's required for the selected backend
	IF (USE_HTTPS STREQUAL "SecureTransport")
		IF (NOT COREFOUNDATION_FOUND)
			MESSAGE(FATAL_ERROR "Cannot use SecureTransport backend, CoreFoundation.framework not found")
		ENDIF()
		IF (NOT SECURITY_FOUND)
			MESSAGE(FATAL_ERROR "Cannot use SecureTransport backend, Security.framework not found")
		ENDIF()
		IF (NOT SECURITY_HAS_SSLCREATECONTEXT)
			MESSAGE(FATAL_ERROR "Cannot use SecureTransport backend, SSLCreateContext not supported")
		ENDIF()

		SET(GIT_SECURE_TRANSPORT 1)
		LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${SECURITY_INCLUDE_DIR})
		LIST(APPEND LIBGIT2_LIBS ${COREFOUNDATION_LDFLAGS} ${SECURITY_LDFLAGS})
		LIST(APPEND LIBGIT2_PC_LIBS ${COREFOUNDATION_LDFLAGS} ${SECURITY_LDFLAGS})
	ELSEIF (USE_HTTPS STREQUAL "OpenSSL")
		IF (NOT OPENSSL_FOUND)
			MESSAGE(FATAL_ERROR "Asked for OpenSSL TLS backend, but it wasn't found")
		ENDIF()

		SET(GIT_OPENSSL 1)
		LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${OPENSSL_INCLUDE_DIR})
		LIST(APPEND LIBGIT2_LIBS ${OPENSSL_LIBRARIES})
		LIST(APPEND LIBGIT2_PC_LIBS ${OPENSSL_LDFLAGS})
		LIST(APPEND LIBGIT2_PC_REQUIRES "openssl")
	ELSEIF(USE_HTTPS STREQUAL "mbedTLS")
		IF (NOT MBEDTLS_FOUND)
			MESSAGE(FATAL_ERROR "Asked for mbedTLS backend, but it wasn't found")
		ENDIF()

		IF(NOT CERT_LOCATION)
			MESSAGE(STATUS "Auto-detecting default certificates location")
			IF(CMAKE_SYSTEM_NAME MATCHES Darwin)
				# Check for an Homebrew installation
				SET(OPENSSL_CMD "/usr/local/opt/openssl/bin/openssl")
			ELSE()
				SET(OPENSSL_CMD "openssl")
			ENDIF()
			EXECUTE_PROCESS(COMMAND ${OPENSSL_CMD} version -d OUTPUT_VARIABLE OPENSSL_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
			IF(OPENSSL_DIR)
				STRING(REGEX REPLACE "^OPENSSLDIR: \"(.*)\"$" "\\1/" OPENSSL_DIR ${OPENSSL_DIR})

				SET(OPENSSL_CA_LOCATIONS
					"ca-bundle.pem"             # OpenSUSE Leap 42.1
					"cert.pem"                  # Ubuntu 14.04, FreeBSD
					"certs/ca-certificates.crt" # Ubuntu 16.04
					"certs/ca.pem"              # Debian 7
				)
				FOREACH(SUFFIX IN LISTS OPENSSL_CA_LOCATIONS)
					SET(LOC "${OPENSSL_DIR}${SUFFIX}")
					IF(NOT CERT_LOCATION AND EXISTS "${OPENSSL_DIR}${SUFFIX}")
						SET(CERT_LOCATION ${LOC})
					ENDIF()
				ENDFOREACH()
			ELSE()
				MESSAGE(FATAL_ERROR "Unable to find OpenSSL executable. Please provide default certificate location via CERT_LOCATION")
			ENDIF()
		ENDIF()

		IF(CERT_LOCATION)
			IF(NOT EXISTS ${CERT_LOCATION})
				MESSAGE(FATAL_ERROR "Cannot use CERT_LOCATION=${CERT_LOCATION} as it doesn't exist")
			ENDIF()
			ADD_FEATURE_INFO(CERT_LOCATION ON "using certificates from ${CERT_LOCATION}")
			ADD_DEFINITIONS(-DGIT_DEFAULT_CERT_LOCATION="${CERT_LOCATION}")
		ENDIF()

		SET(GIT_MBEDTLS 1)
		LIST(APPEND LIBGIT2_SYSTEM_INCLUDES ${MBEDTLS_INCLUDE_DIR})
		LIST(APPEND LIBGIT2_LIBS ${MBEDTLS_LIBRARIES})
		# mbedTLS has no pkgconfig file, hence we can't require it
		# https://github.com/ARMmbed/mbedtls/issues/228
		# For now, pass its link flags as our own
		LIST(APPEND LIBGIT2_PC_LIBS ${MBEDTLS_LIBRARIES})
	ELSEIF (USE_HTTPS STREQUAL "WinHTTP")
		# WinHTTP setup was handled in the WinHTTP-specific block above
	ELSEIF (USE_HTTPS STREQUAL "OpenSSL-Dynamic")
		SET(GIT_OPENSSL 1)
		SET(GIT_OPENSSL_DYNAMIC 1)
		LIST(APPEND LIBGIT2_LIBS dl)
	ELSE()
		MESSAGE(FATAL_ERROR "Asked for backend ${USE_HTTPS} but it wasn't found")
	ENDIF()

	SET(GIT_HTTPS 1)
	ADD_FEATURE_INFO(HTTPS GIT_HTTPS "using ${USE_HTTPS}")
ELSE()
	SET(GIT_HTTPS 0)
	ADD_FEATURE_INFO(HTTPS NO "")
ENDIF()
