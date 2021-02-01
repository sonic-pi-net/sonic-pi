
add_executable(fs_dir dir.cpp)
target_link_libraries(fs_dir ghc_filesystem)
if(CMAKE_CXX_COMPILER_ID MATCHES MSVC)
    target_compile_definitions(fs_dir PRIVATE _CRT_SECURE_NO_WARNINGS)
endif()
AddExecutableWithStdFS(std_fs_dir dir.cpp)

add_executable(fs_du du.cpp)
target_link_libraries(fs_du ghc_filesystem)
AddExecutableWithStdFS(std_fs_du du.cpp)

