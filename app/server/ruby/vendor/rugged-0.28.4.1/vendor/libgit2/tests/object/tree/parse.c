#include "clar_libgit2.h"
#include "tree.h"
#include "object.h"

#define OID1_HEX \
	"\xae\x90\xf1\x2e\xea\x69\x97\x29\xed\x24" \
	"\x55\x5e\x40\xb9\xfd\x66\x9d\xa1\x2a\x12"
#define OID1_STR "ae90f12eea699729ed24555e40b9fd669da12a12"

#define OID2_HEX \
	"\xe8\xbf\xe5\xaf\x39\x57\x9a\x7e\x48\x98" \
	"\xbb\x23\xf3\xa7\x6a\x72\xc3\x68\xce\xe6"
#define OID2_STR "e8bfe5af39579a7e4898bb23f3a76a72c368cee6"

typedef struct {
	const char *filename;
	uint16_t attr;
	const char *oid;
} expected_entry;

static void assert_tree_parses(const char *data, size_t datalen,
	expected_entry *expected_entries, size_t expected_nentries)
{
	git_tree *tree;
	size_t n;

	if (!datalen)
		datalen = strlen(data);
	cl_git_pass(git_object__from_raw((git_object **) &tree, data, datalen, GIT_OBJECT_TREE));

	cl_assert_equal_i(git_tree_entrycount(tree), expected_nentries);

	for (n = 0; n < expected_nentries; n++) {
		expected_entry *expected = expected_entries + n;
		const git_tree_entry *entry;
		git_oid oid;

		cl_git_pass(git_oid_fromstr(&oid, expected->oid));

		cl_assert(entry = git_tree_entry_byname(tree, expected->filename));
		cl_assert_equal_s(expected->filename, entry->filename);
		cl_assert_equal_i(expected->attr, entry->attr);
		cl_assert_equal_oid(&oid, entry->oid);
	}

	git_object_free(&tree->object);
}

static void assert_tree_fails(const char *data, size_t datalen)
{
	git_object *object;
	if (!datalen)
		datalen = strlen(data);
	cl_git_fail(git_object__from_raw(&object, data, datalen, GIT_OBJECT_TREE));
}

void test_object_tree_parse__single_blob_parses(void)
{
	expected_entry entries[] = {
		{ "foo", 0100644, OID1_STR },
	};
	const char data[] = "100644 foo\x00" OID1_HEX;

	assert_tree_parses(data, ARRAY_SIZE(data) - 1, entries, ARRAY_SIZE(entries));
}

void test_object_tree_parse__single_tree_parses(void)
{
	expected_entry entries[] = {
		{ "foo", 040000, OID1_STR },
	};
	const char data[] = "040000 foo\x00" OID1_HEX;

	assert_tree_parses(data, ARRAY_SIZE(data) - 1, entries, ARRAY_SIZE(entries));
}

void test_object_tree_parse__leading_filename_spaces_parse(void)
{
	expected_entry entries[] = {
		{ "       bar", 0100644, OID1_STR },
	};
	const char data[] = "100644        bar\x00" OID1_HEX;

	assert_tree_parses(data, ARRAY_SIZE(data) - 1, entries, ARRAY_SIZE(entries));
}

void test_object_tree_parse__multiple_entries_parse(void)
{
	expected_entry entries[] = {
		{ "bar", 0100644, OID1_STR },
		{ "foo", 040000,  OID2_STR },
	};
	const char data[] =
	    "100644 bar\x00" OID1_HEX
	    "040000 foo\x00" OID2_HEX;

	assert_tree_parses(data, ARRAY_SIZE(data) - 1, entries, ARRAY_SIZE(entries));
}

void test_object_tree_parse__invalid_mode_fails(void)
{
	const char data[] = "10x644 bar\x00" OID1_HEX;
	assert_tree_fails(data, ARRAY_SIZE(data) - 1);
}

void test_object_tree_parse__missing_mode_fails(void)
{
	const char data[] = " bar\x00" OID1_HEX;
	assert_tree_fails(data, ARRAY_SIZE(data) - 1);
}

void test_object_tree_parse__mode_doesnt_cause_oob_read(void)
{
	const char data[] = "100644 bar\x00" OID1_HEX;
	assert_tree_fails(data, 2);
	/*
	 * An oob-read would correctly parse the filename and
	 * later fail to parse the OID with a different error
	 * message
	 */
	cl_assert_equal_s(git_error_last()->message, "failed to parse tree: missing space after filemode");
}

void test_object_tree_parse__unreasonably_large_mode_fails(void)
{
	const char data[] = "10000000000000000000000000 bar\x00" OID1_HEX;
	assert_tree_fails(data, ARRAY_SIZE(data) - 1);
}

void test_object_tree_parse__missing_filename_separator_fails(void)
{
	const char data[] = "100644bar\x00" OID1_HEX;
	assert_tree_fails(data, ARRAY_SIZE(data) - 1);
}

void test_object_tree_parse__missing_filename_terminator_fails(void)
{
	const char data[] = "100644 bar" OID1_HEX;
	assert_tree_fails(data, ARRAY_SIZE(data) - 1);
}

void test_object_tree_parse__empty_filename_fails(void)
{
	const char data[] = "100644 \x00" OID1_HEX;
	assert_tree_fails(data, ARRAY_SIZE(data) - 1);
}

void test_object_tree_parse__trailing_garbage_fails(void)
{
	const char data[] = "100644 bar\x00" OID1_HEX "x";
	assert_tree_fails(data, ARRAY_SIZE(data) - 1);
}

void test_object_tree_parse__leading_space_fails(void)
{
	const char data[] = " 100644 bar\x00" OID1_HEX;
	assert_tree_fails(data, ARRAY_SIZE(data) - 1);
}

void test_object_tree_parse__truncated_oid_fails(void)
{
	const char data[] = " 100644 bar\x00" OID1_HEX;
	assert_tree_fails(data, ARRAY_SIZE(data) - 2);
}
