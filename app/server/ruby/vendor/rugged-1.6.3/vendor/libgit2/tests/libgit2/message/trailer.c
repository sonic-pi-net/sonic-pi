#include "clar_libgit2.h"

static void assert_trailers(const char *message, git_message_trailer *trailers)
{
	git_message_trailer_array arr;
	size_t i;

	int rc = git_message_trailers(&arr, message);

	cl_assert_equal_i(0, rc);

	for(i=0; i<arr.count; i++) {
		cl_assert_equal_s(arr.trailers[i].key, trailers[i].key);
		cl_assert_equal_s(arr.trailers[i].value, trailers[i].value);
	}

	cl_assert_equal_i(0, rc);

	git_message_trailer_array_free(&arr);
}

void test_message_trailer__simple(void)
{
	git_message_trailer trailers[] = {
		{"Signed-off-by", "foo@bar.com"},
		{"Signed-off-by", "someone@else.com"},
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"Signed-off-by: foo@bar.com\n"
		"Signed-off-by: someone@else.com\n"
	, trailers);
}

void test_message_trailer__no_whitespace(void)
{
	git_message_trailer trailers[] = {
		{"Key", "value"},
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"Key:value\n"
	, trailers);
}

void test_message_trailer__extra_whitespace(void)
{
	git_message_trailer trailers[] = {
		{"Key", "value"},
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"Key   :   value\n"
	, trailers);
}

void test_message_trailer__no_newline(void)
{
	git_message_trailer trailers[] = {
		{"Key", "value"},
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"Key: value"
	, trailers);
}

void test_message_trailer__not_last_paragraph(void)
{
	git_message_trailer trailers[] = {
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"Key: value\n"
		"\n"
		"More stuff\n"
	, trailers);
}

void test_message_trailer__conflicts(void)
{
	git_message_trailer trailers[] = {
		{"Key", "value"},
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"Key: value\n"
		"\n"
		"Conflicts:\n"
		"\tfoo.c\n"
	, trailers);
}

void test_message_trailer__patch(void)
{
	git_message_trailer trailers[] = {
		{"Key", "value"},
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"Key: value\n"
		"\n"
		"---\n"
		"More: stuff\n"
	, trailers);
}

void test_message_trailer__continuation(void)
{
	git_message_trailer trailers[] = {
		{"A", "b\n c"},
		{"D", "e\n f: g h"},
		{"I", "j"},
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"A: b\n"
		" c\n"
		"D: e\n"
		" f: g h\n"
		"I: j\n"
	, trailers);
}

void test_message_trailer__invalid(void)
{
	git_message_trailer trailers[] = {
		{"Signed-off-by", "some@one.com"},
		{"Another", "trailer"},
		{NULL, NULL},
	};

	assert_trailers(
		"Message\n"
		"\n"
		"Signed-off-by: some@one.com\n"
		"Not a trailer\n"
		"Another: trailer\n"
	, trailers);
}
