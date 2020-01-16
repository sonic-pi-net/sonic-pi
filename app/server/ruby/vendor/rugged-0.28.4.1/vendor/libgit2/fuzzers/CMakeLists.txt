LINK_DIRECTORIES(${LIBGIT2_LIBDIRS})
INCLUDE_DIRECTORIES(${LIBGIT2_INCLUDES})
INCLUDE_DIRECTORIES(SYSTEM ${LIBGIT2_SYSTEM_INCLUDES})

IF(BUILD_FUZZERS AND NOT USE_STANDALONE_FUZZERS)
	ADD_C_FLAG(-fsanitize=fuzzer)
ENDIF ()

FILE(GLOB SRC_FUZZ RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *_fuzzer.c)
FOREACH(fuzz_target_src ${SRC_FUZZ})
	STRING(REPLACE ".c" "" fuzz_target_name ${fuzz_target_src})
	STRING(REPLACE "_fuzzer" "" fuzz_name ${fuzz_target_name})

	SET(${fuzz_target_name}_SOURCES ${fuzz_target_src} ${LIBGIT2_OBJECTS})
	IF(USE_STANDALONE_FUZZERS)
		LIST(APPEND ${fuzz_target_name}_SOURCES "standalone_driver.c")
	ENDIF()
	ADD_EXECUTABLE(${fuzz_target_name} ${${fuzz_target_name}_SOURCES})
	SET_TARGET_PROPERTIES(${fuzz_target_name} PROPERTIES C_STANDARD 90)
	TARGET_LINK_LIBRARIES(${fuzz_target_name} ${LIBGIT2_LIBS})

	ADD_TEST(${fuzz_target_name} "${CMAKE_CURRENT_BINARY_DIR}/${fuzz_target_name}" "${CMAKE_CURRENT_SOURCE_DIR}/corpora/${fuzz_name}")
ENDFOREACH()
