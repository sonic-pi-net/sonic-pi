#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "merge.h"
#include "merge_helpers.h"
#include "conflict_data.h"
#include "refs.h"
#include "futils.h"
#include "diff_xdiff.h"

#define TEST_REPO_PATH "merge-resolve"
#define TEST_INDEX_PATH TEST_REPO_PATH "/.git/index"

static git_repository *repo;
static git_index *repo_index;

/* Fixture setup and teardown */
void test_merge_files__initialize(void)
{
	git_config *cfg;

	repo = cl_git_sandbox_init(TEST_REPO_PATH);
	git_repository_index(&repo_index, repo);

	/* Ensure that the user's merge.conflictstyle doesn't interfere */
	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_string(cfg, "merge.conflictstyle", "merge"));
	git_config_free(cfg);
}

void test_merge_files__cleanup(void)
{
	git_index_free(repo_index);
	cl_git_sandbox_cleanup();
}

void test_merge_files__automerge_from_bufs(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_result result = {0};
	const char *expected = "Zero\n1\n2\n3\n4\n5\n6\n7\n8\n9\nTen\n";

	ancestor.ptr = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n";
	ancestor.size = strlen(ancestor.ptr);
	ancestor.path = "testfile.txt";
	ancestor.mode = 0100755;

	ours.ptr = "Zero\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n";
	ours.size = strlen(ours.ptr);
	ours.path = "testfile.txt";
	ours.mode = 0100755;

	theirs.ptr = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\nTen\n";
	theirs.size = strlen(theirs.ptr);
	theirs.path = "testfile.txt";
	theirs.mode = 0100755;

	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, 0));

	cl_assert_equal_i(1, result.automergeable);

	cl_assert_equal_s("testfile.txt", result.path);
	cl_assert_equal_i(0100755, result.mode);

	cl_assert_equal_i(strlen(expected), result.len);
	cl_assert_equal_strn(expected, result.ptr, result.len);

	git_merge_file_result_free(&result);
}

void test_merge_files__automerge_use_best_path_and_mode(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_result result = {0};
	const char *expected = "Zero\n1\n2\n3\n4\n5\n6\n7\n8\n9\nTen\n";

	ancestor.ptr = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n";
	ancestor.size = strlen(ancestor.ptr);
	ancestor.path = "testfile.txt";
	ancestor.mode = 0100755;

	ours.ptr = "Zero\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n";
	ours.size = strlen(ours.ptr);
	ours.path = "testfile.txt";
	ours.mode = 0100644;

	theirs.ptr = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\nTen\n";
	theirs.size = strlen(theirs.ptr);
	theirs.path = "theirs.txt";
	theirs.mode = 0100755;

	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, 0));

	cl_assert_equal_i(1, result.automergeable);

	cl_assert_equal_s("theirs.txt", result.path);
	cl_assert_equal_i(0100644, result.mode);

	cl_assert_equal_i(strlen(expected), result.len);
	cl_assert_equal_strn(expected, result.ptr, result.len);

	git_merge_file_result_free(&result);
}

void test_merge_files__conflict_from_bufs(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_result result = {0};

	const char *expected = "<<<<<<< testfile.txt\nAloha!\nOurs.\n=======\nHi!\nTheirs.\n>>>>>>> theirs.txt\n";
	size_t expected_len = strlen(expected);

	ancestor.ptr = "Hello!\nAncestor!\n";
	ancestor.size = strlen(ancestor.ptr);
	ancestor.path = "testfile.txt";
	ancestor.mode = 0100755;

	ours.ptr =  "Aloha!\nOurs.\n";
	ours.size = strlen(ours.ptr);
	ours.path = "testfile.txt";
	ours.mode = 0100644;

	theirs.ptr = "Hi!\nTheirs.\n";
	theirs.size = strlen(theirs.ptr);
	theirs.path = "theirs.txt";
	theirs.mode = 0100755;

	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, NULL));

	cl_assert_equal_i(0, result.automergeable);

	cl_assert_equal_s("theirs.txt", result.path);
	cl_assert_equal_i(0100644, result.mode);

	cl_assert_equal_i(expected_len, result.len);
	cl_assert_equal_strn(expected, result.ptr, expected_len);

	git_merge_file_result_free(&result);
}

void test_merge_files__automerge_from_index(void)
{
	git_merge_file_result result = {0};
	git_index_entry ancestor, ours, theirs;

	git_oid__fromstr(&ancestor.id, "6212c31dab5e482247d7977e4f0dd3601decf13b", GIT_OID_SHA1);
	ancestor.path = "automergeable.txt";
	ancestor.mode = 0100644;

	git_oid__fromstr(&ours.id, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", GIT_OID_SHA1);
	ours.path = "automergeable.txt";
	ours.mode = 0100755;

	git_oid__fromstr(&theirs.id, "058541fc37114bfc1dddf6bd6bffc7fae5c2e6fe", GIT_OID_SHA1);
	theirs.path = "newname.txt";
	theirs.mode = 0100644;

	cl_git_pass(git_merge_file_from_index(&result, repo,
		&ancestor, &ours, &theirs, 0));

	cl_assert_equal_i(1, result.automergeable);

	cl_assert_equal_s("newname.txt", result.path);
	cl_assert_equal_i(0100755, result.mode);

	cl_assert_equal_i(strlen(AUTOMERGEABLE_MERGED_FILE), result.len);
	cl_assert_equal_strn(AUTOMERGEABLE_MERGED_FILE, result.ptr, result.len);

	git_merge_file_result_free(&result);
}

void test_merge_files__automerge_whitespace_eol(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};
	const char *expected = "Zero\n1\n2\n3\n4\n5\n6\n7\n8\n9\nTen\n";

	ancestor.ptr = "0 \n1\n2\n3\n4\n5\n6\n7\n8\n9\n10 \n";
	ancestor.size = strlen(ancestor.ptr);
	ancestor.path = "testfile.txt";
	ancestor.mode = 0100755;

	ours.ptr = "Zero\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n";
	ours.size = strlen(ours.ptr);
	ours.path = "testfile.txt";
	ours.mode = 0100755;

	theirs.ptr = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\nTen\n";
	theirs.size = strlen(theirs.ptr);
	theirs.path = "testfile.txt";
	theirs.mode = 0100755;

	opts.flags |= GIT_MERGE_FILE_IGNORE_WHITESPACE_EOL;
	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, &opts));

	cl_assert_equal_i(1, result.automergeable);

	cl_assert_equal_s("testfile.txt", result.path);
	cl_assert_equal_i(0100755, result.mode);

	cl_assert_equal_i(strlen(expected), result.len);
	cl_assert_equal_strn(expected, result.ptr, result.len);

	git_merge_file_result_free(&result);
}

void test_merge_files__automerge_whitespace_change(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};
	const char *expected = "Zero\n1\n2\n3\n4\n5 XXX\n6 YYY\n7\n8\n9\nTen\n";

	ancestor.ptr = "0\n1\n2\n3\n4\n5 XXX\n6YYY\n7\n8\n9\n10\n";
	ancestor.size = strlen(ancestor.ptr);
	ancestor.path = "testfile.txt";
	ancestor.mode = 0100755;

	ours.ptr = "Zero\n1\n2\n3\n4\n5 XXX\n6 YYY\n7\n8\n9\n10\n";
	ours.size = strlen(ours.ptr);
	ours.path = "testfile.txt";
	ours.mode = 0100755;

	theirs.ptr = "0\n1\n2\n3\n4\n5 XXX\n6  YYY\n7\n8\n9\nTen\n";
	theirs.size = strlen(theirs.ptr);
	theirs.path = "testfile.txt";
	theirs.mode = 0100755;

	opts.flags |= GIT_MERGE_FILE_IGNORE_WHITESPACE_CHANGE;
	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, &opts));

	cl_assert_equal_i(1, result.automergeable);

	cl_assert_equal_s("testfile.txt", result.path);
	cl_assert_equal_i(0100755, result.mode);

	cl_assert_equal_i(strlen(expected), result.len);
	cl_assert_equal_strn(expected, result.ptr, result.len);

	git_merge_file_result_free(&result);
}

void test_merge_files__doesnt_add_newline(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};
	const char *expected = "Zero\n1\n2\n3\n4\n5 XXX\n6 YYY\n7\n8\n9\nTen";

	ancestor.ptr = "0\n1\n2\n3\n4\n5 XXX\n6YYY\n7\n8\n9\n10";
	ancestor.size = strlen(ancestor.ptr);
	ancestor.path = "testfile.txt";
	ancestor.mode = 0100755;

	ours.ptr = "Zero\n1\n2\n3\n4\n5 XXX\n6 YYY\n7\n8\n9\n10";
	ours.size = strlen(ours.ptr);
	ours.path = "testfile.txt";
	ours.mode = 0100755;

	theirs.ptr = "0\n1\n2\n3\n4\n5 XXX\n6  YYY\n7\n8\n9\nTen";
	theirs.size = strlen(theirs.ptr);
	theirs.path = "testfile.txt";
	theirs.mode = 0100755;

	opts.flags |= GIT_MERGE_FILE_IGNORE_WHITESPACE_CHANGE;
	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, &opts));

	cl_assert_equal_i(1, result.automergeable);

	cl_assert_equal_s("testfile.txt", result.path);
	cl_assert_equal_i(0100755, result.mode);

	cl_assert_equal_i(strlen(expected), result.len);
	cl_assert_equal_strn(expected, result.ptr, result.len);

	git_merge_file_result_free(&result);
}

void test_merge_files__skips_large_files(void)
{
	git_merge_file_input ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};

	ours.size = GIT_XDIFF_MAX_SIZE + 1;
	ours.path = "testfile.txt";
	ours.mode = 0100755;

	theirs.size = GIT_XDIFF_MAX_SIZE + 1;
	theirs.path = "testfile.txt";
	theirs.mode = 0100755;

	cl_git_pass(git_merge_file(&result, NULL, &ours, &theirs, &opts));

	cl_assert_equal_i(0, result.automergeable);

	git_merge_file_result_free(&result);
}

void test_merge_files__skips_binaries(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_result result = {0};

	ancestor.ptr = "ance\0stor\0";
	ancestor.size = 10;
	ancestor.path = "ancestor.txt";
	ancestor.mode = 0100755;

	ours.ptr = "foo\0bar\0";
	ours.size = 8;
	ours.path = "ours.txt";
	ours.mode = 0100755;

	theirs.ptr = "bar\0foo\0";
	theirs.size = 8;
	theirs.path = "theirs.txt";
	theirs.mode = 0100644;

	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, NULL));

	cl_assert_equal_i(0, result.automergeable);

	git_merge_file_result_free(&result);
}

void test_merge_files__handles_binaries_when_favored(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};

	ancestor.ptr = "ance\0stor\0";
	ancestor.size = 10;
	ancestor.path = "ancestor.txt";
	ancestor.mode = 0100755;

	ours.ptr = "foo\0bar\0";
	ours.size = 8;
	ours.path = "ours.txt";
	ours.mode = 0100755;

	theirs.ptr = "bar\0foo\0";
	theirs.size = 8;
	theirs.path = "theirs.txt";
	theirs.mode = 0100644;

	opts.favor = GIT_MERGE_FILE_FAVOR_OURS;
	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, &opts));

	cl_assert_equal_i(1, result.automergeable);

	cl_assert_equal_s("ours.txt", result.path);
	cl_assert_equal_i(0100755, result.mode);

	cl_assert_equal_i(ours.size, result.len);
	cl_assert(memcmp(result.ptr, ours.ptr, ours.size) == 0);

	git_merge_file_result_free(&result);
}

void test_merge_files__crlf_conflict_markers_for_crlf_files(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};

	const char *expected =
		"<<<<<<< file.txt\r\nThis file\r\ndoes, too.\r\n"
		"=======\r\nAnd so does\r\nthis one.\r\n>>>>>>> file.txt\r\n";
	size_t expected_len = strlen(expected);

	const char *expected_diff3 =
		"<<<<<<< file.txt\r\nThis file\r\ndoes, too.\r\n"
		"||||||| file.txt\r\nThis file has\r\nCRLF line endings.\r\n"
		"=======\r\nAnd so does\r\nthis one.\r\n>>>>>>> file.txt\r\n";
	size_t expected_diff3_len = strlen(expected_diff3);

	ancestor.ptr = "This file has\r\nCRLF line endings.\r\n";
	ancestor.size = 35;
	ancestor.path = "file.txt";
	ancestor.mode = 0100644;

	ours.ptr = "This file\r\ndoes, too.\r\n";
	ours.size = 23;
	ours.path = "file.txt";
	ours.mode = 0100644;

	theirs.ptr = "And so does\r\nthis one.\r\n";
	theirs.size = 24;
	theirs.path = "file.txt";
	theirs.mode = 0100644;

	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, &opts));
	cl_assert_equal_i(0, result.automergeable);
	cl_assert_equal_i(expected_len, result.len);
	cl_assert(memcmp(expected, result.ptr, expected_len) == 0);
	git_merge_file_result_free(&result);

	opts.flags |= GIT_MERGE_FILE_STYLE_DIFF3;
	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, &opts));
	cl_assert_equal_i(0, result.automergeable);
	cl_assert_equal_i(expected_diff3_len, result.len);
	cl_assert(memcmp(expected_diff3, result.ptr, expected_len) == 0);
	git_merge_file_result_free(&result);
}

void test_merge_files__conflicts_in_zdiff3(void)
{
	git_merge_file_input ancestor = GIT_MERGE_FILE_INPUT_INIT,
		ours = GIT_MERGE_FILE_INPUT_INIT,
		theirs = GIT_MERGE_FILE_INPUT_INIT;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};

	const char *expected_zdiff3 =
		"1,\nfoo,\nbar,\n" \
		"<<<<<<< file.txt\n" \
		"||||||| file.txt\n# add more here\n" \
		"=======\nquux,\nwoot,\n" \
		">>>>>>> file.txt\nbaz,\n3,\n";
	size_t expected_zdiff3_len = strlen(expected_zdiff3);

	ancestor.ptr = "1,\n# add more here\n3,\n";
	ancestor.size = strlen(ancestor.ptr);
	ancestor.path = "file.txt";
	ancestor.mode = 0100644;

	ours.ptr = "1,\nfoo,\nbar,\nbaz,\n3,\n";
	ours.size = strlen(ours.ptr);
	ours.path = "file.txt";
	ours.mode = 0100644;

	theirs.ptr = "1,\nfoo,\nbar,\nquux,\nwoot,\nbaz,\n3,\n";
	theirs.size = strlen(theirs.ptr);
	theirs.path = "file.txt";
	theirs.mode = 0100644;

	opts.flags |= GIT_MERGE_FILE_STYLE_ZDIFF3;
	cl_git_pass(git_merge_file(&result, &ancestor, &ours, &theirs, &opts));
	cl_assert_equal_i(0, result.automergeable);
	cl_assert_equal_i(expected_zdiff3_len, result.len);
	cl_assert(memcmp(expected_zdiff3, result.ptr, expected_zdiff3_len) == 0);
	git_merge_file_result_free(&result);
}
