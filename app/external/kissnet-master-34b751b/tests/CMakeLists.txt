if(MSVC)
  # Force to always compile with W4
  IF(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
    STRING(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  ELSE()
    string(APPEND CMAKE_CXX_FLAGS " /W4")
  ENDIF()
elseif(CMAKE_CXX_COMPILER_ID STREQUAL GNU)
  # Update if necessary
  string(APPEND CMAKE_CXX_FLAGS " -Wall -Wno-long-long -pedantic")
endif()

foreach(d acceptor endpoint error_handler loopback_tcp loopback_udp simple socket)
  add_subdirectory(${d})
endforeach()

set(WINSOCK_LIBRARIES)
if(WIN32)
set(WINSOCK_LIBRARIES wsock32 ws2_32 Iphlpapi)
endif()

foreach(t acceptor_test endpoint_test error_handler_test loopback_tcp_test loopback_udp_test simple_test socket_test)
  set_target_properties(${t} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
  target_link_libraries(${t} PRIVATE kissnet ${WINSOCK_LIBRARIES})
endforeach()

foreach(t endpoint_test error_handler_test loopback_tcp_test loopback_udp_test simple_test)
  add_test(NAME test:${t} COMMAND $<TARGET_FILE:${t}>)
  set_tests_properties(test:${t} PROPERTIES TIMEOUT 5)
endforeach()
