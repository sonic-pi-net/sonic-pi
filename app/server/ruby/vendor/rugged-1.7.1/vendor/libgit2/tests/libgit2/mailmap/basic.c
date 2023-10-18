#include "clar.h"
#include "clar_libgit2.h"

#include "common.h"
#include "mailmap.h"

static git_mailmap *mailmap = NULL;

const char TEST_MAILMAP[] =
	"Foo bar <foo@bar.com> <foo@baz.com>  \n"
	"Blatantly invalid line\n"
	"Foo bar <foo@bar.com> <foo@bal.com>\n"
	"<email@foo.com> <otheremail@foo.com>\n"
	"<email@foo.com> Other Name <yetanotheremail@foo.com>\n";

struct {
	const char *real_name;
	const char *real_email;
	const char *replace_name;
	const char *replace_email;
} expected[] = {
	{ "Foo bar", "foo@bar.com", NULL, "foo@baz.com" },
	{ "Foo bar", "foo@bar.com", NULL, "foo@bal.com" },
	{ NULL, "email@foo.com", NULL, "otheremail@foo.com" },
	{ NULL, "email@foo.com", "Other Name", "yetanotheremail@foo.com" }
};

void test_mailmap_basic__initialize(void)
{
	cl_git_pass(git_mailmap_from_buffer(
		&mailmap, TEST_MAILMAP, strlen(TEST_MAILMAP)));
}

void test_mailmap_basic__cleanup(void)
{
	git_mailmap_free(mailmap);
	mailmap = NULL;
}

void test_mailmap_basic__entry(void)
{
	size_t idx;
	const git_mailmap_entry *entry;

	/* Check that we have the expected # of entries */
	cl_assert_equal_sz(ARRAY_SIZE(expected), git_vector_length(&mailmap->entries));

	for (idx = 0; idx < ARRAY_SIZE(expected); ++idx) {
		/* Try to look up each entry and make sure they match */
		entry = git_mailmap_entry_lookup(
			mailmap, expected[idx].replace_name, expected[idx].replace_email);

		cl_assert(entry);
		cl_assert_equal_s(entry->real_name, expected[idx].real_name);
		cl_assert_equal_s(entry->real_email, expected[idx].real_email);
		cl_assert_equal_s(entry->replace_name, expected[idx].replace_name);
		cl_assert_equal_s(entry->replace_email, expected[idx].replace_email);
	}
}

void test_mailmap_basic__lookup_not_found(void)
{
	const git_mailmap_entry *entry = git_mailmap_entry_lookup(
		mailmap, "Whoever", "doesnotexist@fo.com");
	cl_assert(!entry);
}

void test_mailmap_basic__lookup(void)
{
	const git_mailmap_entry *entry = git_mailmap_entry_lookup(
		mailmap, "Typoed the name once", "foo@baz.com");
	cl_assert(entry);
	cl_assert_equal_s(entry->real_name, "Foo bar");
}

void test_mailmap_basic__empty_email_query(void)
{
	const char *name;
	const char *email;
	cl_git_pass(git_mailmap_resolve(
		&name, &email, mailmap, "Author name", "otheremail@foo.com"));
	cl_assert_equal_s(name, "Author name");
	cl_assert_equal_s(email, "email@foo.com");
}

void test_mailmap_basic__name_matching(void)
{
	const char *name;
	const char *email;
	cl_git_pass(git_mailmap_resolve(
		&name, &email, mailmap, "Other Name", "yetanotheremail@foo.com"));

	cl_assert_equal_s(name, "Other Name");
	cl_assert_equal_s(email, "email@foo.com");

	cl_git_pass(git_mailmap_resolve(
		&name, &email, mailmap,
		"Other Name That Doesn't Match", "yetanotheremail@foo.com"));
	cl_assert_equal_s(name, "Other Name That Doesn't Match");
	cl_assert_equal_s(email, "yetanotheremail@foo.com");
}
