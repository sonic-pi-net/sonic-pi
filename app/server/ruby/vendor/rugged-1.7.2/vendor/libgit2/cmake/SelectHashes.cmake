# Select a hash backend

include(SanitizeBool)

# USE_SHA1=CollisionDetection(ON)/HTTPS/Generic/OFF
sanitizebool(USE_SHA1)
sanitizebool(USE_SHA256)

# sha1

if(USE_SHA1 STREQUAL ON)
	SET(USE_SHA1 "CollisionDetection")
elseif(USE_SHA1 STREQUAL "HTTPS")
	if(USE_HTTPS STREQUAL "SecureTransport")
		set(USE_SHA1 "CommonCrypto")
	elseif(USE_HTTPS STREQUAL "Schannel")
		set(USE_SHA1 "Win32")
	elseif(USE_HTTPS STREQUAL "WinHTTP")
		set(USE_SHA1 "Win32")
	elseif(USE_HTTPS)
		set(USE_SHA1 ${USE_HTTPS})
	else()
		set(USE_SHA1 "CollisionDetection")
	endif()
endif()

if(USE_SHA1 STREQUAL "CollisionDetection")
	set(GIT_SHA1_COLLISIONDETECT 1)
elseif(USE_SHA1 STREQUAL "OpenSSL")
	set(GIT_SHA1_OPENSSL 1)
elseif(USE_SHA1 STREQUAL "OpenSSL-Dynamic")
	set(GIT_SHA1_OPENSSL 1)
	set(GIT_SHA1_OPENSSL_DYNAMIC 1)
	list(APPEND LIBGIT2_SYSTEM_LIBS dl)
elseif(USE_SHA1 STREQUAL "CommonCrypto")
	set(GIT_SHA1_COMMON_CRYPTO 1)
elseif(USE_SHA1 STREQUAL "mbedTLS")
	set(GIT_SHA1_MBEDTLS 1)
elseif(USE_SHA1 STREQUAL "Win32")
	set(GIT_SHA1_WIN32 1)
else()
	message(FATAL_ERROR "Asked for unknown SHA1 backend: ${USE_SHA1}")
endif()

# sha256

if(USE_SHA256 STREQUAL ON AND USE_HTTPS)
	SET(USE_SHA256 "HTTPS")
elseif(USE_SHA256 STREQUAL ON)
	SET(USE_SHA256 "Builtin")
endif()

if(USE_SHA256 STREQUAL "HTTPS")
	if(USE_HTTPS STREQUAL "SecureTransport")
		set(USE_SHA256 "CommonCrypto")
	elseif(USE_HTTPS STREQUAL "Schannel")
		set(USE_SHA256 "Win32")
	elseif(USE_HTTPS STREQUAL "WinHTTP")
		set(USE_SHA256 "Win32")
	elseif(USE_HTTPS)
		set(USE_SHA256 ${USE_HTTPS})
	endif()
endif()

if(USE_SHA256 STREQUAL "Builtin")
	set(GIT_SHA256_BUILTIN 1)
elseif(USE_SHA256 STREQUAL "OpenSSL")
	set(GIT_SHA256_OPENSSL 1)
elseif(USE_SHA256 STREQUAL "OpenSSL-Dynamic")
	set(GIT_SHA256_OPENSSL 1)
	set(GIT_SHA256_OPENSSL_DYNAMIC 1)
	list(APPEND LIBGIT2_SYSTEM_LIBS dl)
elseif(USE_SHA256 STREQUAL "CommonCrypto")
	set(GIT_SHA256_COMMON_CRYPTO 1)
elseif(USE_SHA256 STREQUAL "mbedTLS")
	set(GIT_SHA256_MBEDTLS 1)
elseif(USE_SHA256 STREQUAL "Win32")
	set(GIT_SHA256_WIN32 1)
else()
	message(FATAL_ERROR "Asked for unknown SHA256 backend: ${USE_SHA256}")
endif()

# add library requirements
if(USE_SHA1 STREQUAL "OpenSSL" OR USE_SHA256 STREQUAL "OpenSSL")
	if(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
		list(APPEND LIBGIT2_PC_LIBS "-lssl")
	else()
		list(APPEND LIBGIT2_PC_REQUIRES "openssl")
	endif()
endif()

if(USE_SHA1 STREQUAL "mbedTLS" OR USE_SHA256 STREQUAL "mbedTLS")
	list(APPEND LIBGIT2_SYSTEM_INCLUDES ${MBEDTLS_INCLUDE_DIR})
	list(APPEND LIBGIT2_SYSTEM_LIBS ${MBEDTLS_LIBRARIES})
	# mbedTLS has no pkgconfig file, hence we can't require it
	# https://github.com/ARMmbed/mbedtls/issues/228
	# For now, pass its link flags as our own
	list(APPEND LIBGIT2_PC_LIBS ${MBEDTLS_LIBRARIES})
endif()

# notify feature enablement

add_feature_info(SHA1 ON "using ${USE_SHA1}")
add_feature_info(SHA256 ON "using ${USE_SHA256}")
