set(tests
  ampmod_test.cpp
  simd_binary_tests.cpp
  simd_horizontal_tests.cpp
  simd_math_tests.cpp
  simd_memory_tests.cpp
  simd_mix_tests.cpp
  simd_pan_tests.cpp
  simd_peak_tests.cpp
  simd_round_tests.cpp
  simd_ternary_tests.cpp
  simd_tests.cpp
  simd_unary_tests.cpp
  simd_unit_conversion_tests.cpp
  softclip_test.cpp
  vec_test.cpp
)

find_package(Boost)
include_directories(${Boost_INCLUDE_DIRS})

if (WIN32)
   add_definitions(-DNOMINMAX)
endif()

SET(EMULATOR "" CACHE STRING "use CPU emulator")
separate_arguments(EMULATOR UNIX_COMMAND "${EMULATOR}")

foreach(test ${tests})
  string(REPLACE .cpp "" test_name ${test} )
  add_executable(${test_name} ${test} ${headers})

  if(EMULATOR)
    add_test(NAME ${test_name}_run COMMAND ${EMULATOR} ${CMAKE_CURRENT_BINARY_DIR}/${test_name})
  else()
    add_test(${test_name}_run ${EXECUTABLE_OUTPUT_PATH}/${test_name})
  endif()
endforeach(test)
