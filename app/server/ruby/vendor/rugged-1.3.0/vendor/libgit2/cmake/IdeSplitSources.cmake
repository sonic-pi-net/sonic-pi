# This function splits the sources files up into their appropriate
# subdirectories.  This is especially useful for IDEs like Xcode and
# Visual Studio, so that you can navigate into the libgit2_clar project,
# and see the folders within the tests folder (instead of just seeing all
# source and tests in a single folder.)
FUNCTION(IDE_SPLIT_SOURCES target)
	IF(MSVC_IDE OR CMAKE_GENERATOR STREQUAL Xcode)
		GET_TARGET_PROPERTY(sources ${target} SOURCES)
		FOREACH(source ${sources})
			IF(source MATCHES ".*/")
				STRING(REPLACE ${libgit2_SOURCE_DIR}/ "" rel ${source})
				IF(rel)
					STRING(REGEX REPLACE "/([^/]*)$" "" rel ${rel})
					IF(rel)
						STRING(REPLACE "/" "\\\\" rel ${rel})
						SOURCE_GROUP(${rel} FILES ${source})
					ENDIF()
				ENDIF()
			ENDIF()
		ENDFOREACH()
	ENDIF()
ENDFUNCTION()
