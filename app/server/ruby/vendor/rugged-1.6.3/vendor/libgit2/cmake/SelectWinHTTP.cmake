if(WIN32 AND USE_WINHTTP)
	set(GIT_WINHTTP 1)

	# Since MinGW does not come with headers or an import library for winhttp,
	# we have to include a private header and generate our own import library
	if(MINGW)
		add_subdirectory("${PROJECT_SOURCE_DIR}/deps/winhttp" "${PROJECT_BINARY_DIR}/deps/winhttp")
		list(APPEND LIBGIT2_SYSTEM_LIBS winhttp)
		list(APPEND LIBGIT2_DEPENDENCY_INCLUDES "${PROJECT_SOURCE_DIR}/deps/winhttp")
	else()
		list(APPEND LIBGIT2_SYSTEM_LIBS "winhttp")
		list(APPEND LIBGIT2_PC_LIBS "-lwinhttp")
	endif()

	list(APPEND LIBGIT2_SYSTEM_LIBS "rpcrt4" "crypt32" "ole32")
	list(APPEND LIBGIT2_PC_LIBS "-lrpcrt4" "-lcrypt32" "-lole32")
endif()
