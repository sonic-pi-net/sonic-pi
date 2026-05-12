# spout-receiver: Windows-only smoke-test. Opt-in via BUILD_SPOUT_SMOKETEST.
# Links the same vendored Spout2 as the GUI.

add_executable(spout-receiver main.cpp)

target_link_libraries(spout-receiver PRIVATE spout_vendored)

target_compile_features(spout-receiver PRIVATE cxx_std_17)

# Match GUI's MSVC warning level.
if(MSVC)
    target_compile_options(spout-receiver PRIVATE
        /W3 /D_CRT_SECURE_NO_WARNINGS)
endif()
