#include "clar_libgit2.h"

static git_str _buf;

void test_str_splice__initialize(void) {
   git_str_init(&_buf, 16);
}

void test_str_splice__cleanup(void) {
   git_str_dispose(&_buf);
}

void test_str_splice__preprend(void)
{
	git_str_sets(&_buf, "world!");

	cl_git_pass(git_str_splice(&_buf, 0, 0, "Hello Dolly", strlen("Hello ")));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}

void test_str_splice__append(void)
{
	git_str_sets(&_buf, "Hello");

	cl_git_pass(git_str_splice(&_buf, git_str_len(&_buf), 0, " world!", strlen(" world!")));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}

void test_str_splice__insert_at(void)
{
	git_str_sets(&_buf, "Hell world!");

	cl_git_pass(git_str_splice(&_buf, strlen("Hell"), 0, "o", strlen("o")));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}

void test_str_splice__remove_at(void)
{
	git_str_sets(&_buf, "Hello world of warcraft!");

	cl_git_pass(git_str_splice(&_buf, strlen("Hello world"), strlen(" of warcraft"), "", 0));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}

void test_str_splice__replace(void)
{
	git_str_sets(&_buf, "Hell0 w0rld!");

	cl_git_pass(git_str_splice(&_buf, strlen("Hell"), strlen("0 w0"), "o wo", strlen("o wo")));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}

void test_str_splice__replace_with_longer(void)
{
	git_str_sets(&_buf, "Hello you!");

	cl_git_pass(git_str_splice(&_buf, strlen("Hello "), strlen("you"), "world", strlen("world")));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}

void test_str_splice__replace_with_shorter(void)
{
	git_str_sets(&_buf, "Brave new world!");

	cl_git_pass(git_str_splice(&_buf, 0, strlen("Brave new"), "Hello", strlen("Hello")));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}

void test_str_splice__truncate(void)
{
	git_str_sets(&_buf, "Hello world!!");

	cl_git_pass(git_str_splice(&_buf, strlen("Hello world!"), strlen("!"), "", 0));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}

void test_str_splice__dont_do_anything(void)
{
	git_str_sets(&_buf, "Hello world!");

	cl_git_pass(git_str_splice(&_buf, 3, 0, "Hello", 0));

	cl_assert_equal_s("Hello world!", git_str_cstr(&_buf));
}
