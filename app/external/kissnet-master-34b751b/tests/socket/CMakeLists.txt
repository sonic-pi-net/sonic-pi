add_executable(socket_test main.cpp)
find_package(Threads)
if(Threads_FOUND)
target_link_libraries(socket_test PRIVATE Threads::Threads)
endif()

if(NOT CI)
# this test can be shaky on CI, and for example if the website server is down the CI won't know that
add_test(NAME test:socket:ipv4
COMMAND $<TARGET_FILE:socket_test> 127.0.0.1 0.0.0.0)
set_tests_properties(test:socket:ipv4 PROPERTIES TIMEOUT 30 RESOURCE_LOCK port)

add_test(NAME test:socket:ipv6
COMMAND $<TARGET_FILE:socket_test> ::1 ::)
set_tests_properties(test:socket:ipv6 PROPERTIES TIMEOUT 30 RESOURCE_LOCK port)
endif()
