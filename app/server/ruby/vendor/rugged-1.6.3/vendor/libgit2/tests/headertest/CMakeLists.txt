# Header file validation project: ensure that we do not publish any sloppy
# definitions in our headers and that a consumer can include <git2.dll>
# even when they have aggressive C90 warnings enabled.

add_executable(headertest headertest.c)
set_target_properties(headertest PROPERTIES C_STANDARD 90)
set_target_properties(headertest PROPERTIES C_EXTENSIONS OFF)
target_include_directories(headertest PRIVATE ${LIBGIT2_INCLUDES})

if (MSVC)
	target_compile_options(headertest PUBLIC /W4 /WX)
else()
	target_compile_options(headertest PUBLIC -Wall -Wextra -pedantic -Werror)
endif()
