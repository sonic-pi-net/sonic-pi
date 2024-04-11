#include "clar_libgit2.h"

#include "git2/sys/diff.h"

#include "delta.h"
#include "filebuf.h"
#include "repository.h"

static git_repository *repo;

void test_diff_binary__initialize(void)
{
}

void test_diff_binary__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void test_patch(
	const char *one,
	const char *two,
	const git_diff_options *opts,
	const char *expected)
{
	git_oid id_one, id_two;
	git_index *index = NULL;
	git_commit *commit_one, *commit_two = NULL;
	git_tree *tree_one, *tree_two;
	git_diff *diff;
	git_patch *patch;
	git_buf actual = GIT_BUF_INIT;

	cl_git_pass(git_oid__fromstr(&id_one, one, GIT_OID_SHA1));
	cl_git_pass(git_commit_lookup(&commit_one, repo, &id_one));
	cl_git_pass(git_commit_tree(&tree_one, commit_one));

	if (two) {
		cl_git_pass(git_oid__fromstr(&id_two, two, GIT_OID_SHA1));
		cl_git_pass(git_commit_lookup(&commit_two, repo, &id_two));
		cl_git_pass(git_commit_tree(&tree_two, commit_two));
	} else {
		cl_git_pass(git_repository_index(&index, repo));
		cl_git_pass(git_index_write_tree(&id_two, index));
		cl_git_pass(git_tree_lookup(&tree_two, repo, &id_two));
	}

	cl_git_pass(git_diff_tree_to_tree(&diff, repo, tree_one, tree_two, opts));

	cl_git_pass(git_patch_from_diff(&patch, diff, 0));
	cl_git_pass(git_patch_to_buf(&actual, patch));

	cl_assert_equal_s(expected, actual.ptr);

	git_buf_dispose(&actual);
	cl_git_pass(git_diff_print(diff, GIT_DIFF_FORMAT_PATCH, git_diff_print_callback__to_buf, &actual));

	cl_assert_equal_s(expected, actual.ptr);

	git_buf_dispose(&actual);
	git_patch_free(patch);
	git_diff_free(diff);
	git_tree_free(tree_one);
	git_tree_free(tree_two);
	git_commit_free(commit_one);
	git_commit_free(commit_two);
	git_index_free(index);
}

void test_diff_binary__add_normal(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/binary.bin b/binary.bin\n" \
		"new file mode 100644\n" \
		"index 0000000..bd474b2\n" \
		"Binary files /dev/null and b/binary.bin differ\n";

	repo = cl_git_sandbox_init("diff_format_email");
	test_patch(
		"873806f6f27e631eb0b23e4b56bea2bfac14a373",
		"897d3af16ca9e420cd071b1c4541bd2b91d04c8c",
		&opts,
		expected);
}

void test_diff_binary__add(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/binary.bin b/binary.bin\n" \
		"new file mode 100644\n" \
		"index 0000000000000000000000000000000000000000..bd474b2519cc15eab801ff851cc7d50f0dee49a1\n" \
		"GIT binary patch\n" \
		"literal 3\n" \
		"Kc${Nk-~s>u4FC%O\n"
		"\n" \
		"literal 0\n" \
		"Hc$@<O00001\n" \
		"\n";

	opts.flags = GIT_DIFF_SHOW_BINARY;
	opts.id_abbrev = GIT_OID_SHA1_HEXSIZE;

	repo = cl_git_sandbox_init("diff_format_email");
	test_patch(
		"873806f6f27e631eb0b23e4b56bea2bfac14a373",
		"897d3af16ca9e420cd071b1c4541bd2b91d04c8c",
		&opts,
		expected);
}

void test_diff_binary__modify_normal(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/binary.bin b/binary.bin\n" \
		"index bd474b2..9ac35ff 100644\n" \
		"Binary files a/binary.bin and b/binary.bin differ\n";

	repo = cl_git_sandbox_init("diff_format_email");
	test_patch(
		"897d3af16ca9e420cd071b1c4541bd2b91d04c8c",
		"8d7523f6fcb2404257889abe0d96f093d9f524f9",
		&opts,
		expected);
}

void test_diff_binary__modify(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/binary.bin b/binary.bin\n" \
		"index bd474b2519cc15eab801ff851cc7d50f0dee49a1..9ac35ff15cd8864aeafd889e4826a3150f0b06c4 100644\n" \
		"GIT binary patch\n" \
		"literal 5\n" \
		"Mc${NkU}WL~000&M4gdfE\n" \
		"\n" \
		"literal 3\n" \
		"Kc${Nk-~s>u4FC%O\n" \
		"\n";

	opts.flags = GIT_DIFF_SHOW_BINARY;

	repo = cl_git_sandbox_init("diff_format_email");
	test_patch(
		"897d3af16ca9e420cd071b1c4541bd2b91d04c8c",
		"8d7523f6fcb2404257889abe0d96f093d9f524f9",
		&opts,
		expected);
}

void test_diff_binary__delete_normal(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/binary.bin b/binary.bin\n" \
		"deleted file mode 100644\n" \
		"index bd474b2..0000000\n" \
		"Binary files a/binary.bin and /dev/null differ\n";

	repo = cl_git_sandbox_init("diff_format_email");
	test_patch(
		"897d3af16ca9e420cd071b1c4541bd2b91d04c8c",
		"873806f6f27e631eb0b23e4b56bea2bfac14a373",
		&opts,
		expected);
}

void test_diff_binary__delete(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/binary.bin b/binary.bin\n" \
		"deleted file mode 100644\n" \
		"index bd474b2519cc15eab801ff851cc7d50f0dee49a1..0000000000000000000000000000000000000000\n" \
		"GIT binary patch\n" \
		"literal 0\n" \
		"Hc$@<O00001\n" \
		"\n" \
		"literal 3\n" \
		"Kc${Nk-~s>u4FC%O\n" \
		"\n";

	opts.flags = GIT_DIFF_SHOW_BINARY;
	opts.id_abbrev = GIT_OID_SHA1_HEXSIZE;

	repo = cl_git_sandbox_init("diff_format_email");
	test_patch(
		"897d3af16ca9e420cd071b1c4541bd2b91d04c8c",
		"873806f6f27e631eb0b23e4b56bea2bfac14a373",
		&opts,
		expected);
}

void test_diff_binary__delta(void)
{
	git_index *index;
	git_str contents = GIT_STR_INIT;
	size_t i;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/songof7cities.txt b/songof7cities.txt\n" \
		"index 4210ffd5c390b21dd5483375e75288dea9ede512..cc84ec183351c9944ed90a619ca08911924055b5 100644\n" \
		"GIT binary patch\n" \
		"delta 198\n" \
		"zc$}LmI8{(0BqLQJI6p64AwNwaIJGP_Pa)Ye#M3o+qJ$<Jl;sX*mF<MGCYv&*L7AHu\n" \
		"zGA1*^gt?gYVN82wTbPO_W)+x<&1+cP;HrPHR>PQ;Y(X&QMK*C5^Br3bjG4d=XI^5@\n" \
		"JfH567LIG)KJdFSV\n" \
		"\n" \
		"delta 198\n" \
		"zc$}LmI8{(0BqLQJI6p64AwNwaIJGP_Pr*5}Br~;mqJ$<Jl;sX*mF<MGCYv&*L7AHu\n" \
		"zGA1*^gt?gYVN82wTbPO_W)+x<&1+cP;HrPHR>PQ;Y(X&QMK*C5^Br3bjG4d=XI^5@\n" \
		"JfH567LIF3FM2!Fd\n" \
		"\n";

	opts.flags = GIT_DIFF_SHOW_BINARY | GIT_DIFF_FORCE_BINARY;
	opts.id_abbrev = GIT_OID_SHA1_HEXSIZE;

	repo = cl_git_sandbox_init("renames");
	cl_git_pass(git_repository_index(&index, repo));

	cl_git_pass(git_futils_readbuffer(&contents, "renames/songof7cities.txt"));

	for (i = 0; i < contents.size - 6; i++) {
		if (strncmp(&contents.ptr[i], "Cities", 6) == 0)
			memcpy(&contents.ptr[i], "cITIES", 6);
	}

	cl_git_rewritefile("renames/songof7cities.txt", contents.ptr);
	cl_git_pass(git_index_add_bypath(index, "songof7cities.txt"));
	cl_git_pass(git_index_write(index));

	test_patch(
		"19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13",
		NULL,
		&opts,
		expected);

	git_index_free(index);
	git_str_dispose(&contents);
}

void test_diff_binary__delta_append(void)
{
	git_index *index;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/untimely.txt b/untimely.txt\n" \
		"index 9a69d960ae94b060f56c2a8702545e2bb1abb935..1111d4f11f4b35bf6759e0fb714fe09731ef0840 100644\n" \
		"GIT binary patch\n" \
		"delta 32\n" \
		"nc%1vf+QYWt3zLL@hC)e3Vu?a>QDRl4f_G*?PG(-ZA}<#J$+QbW\n" \
		"\n" \
		"delta 7\n" \
		"Oc%18D`@*{63ljhg(E~C7\n" \
		"\n";

	opts.flags = GIT_DIFF_SHOW_BINARY | GIT_DIFF_FORCE_BINARY;
	opts.id_abbrev = GIT_OID_SHA1_HEXSIZE;

	repo = cl_git_sandbox_init("renames");
	cl_git_pass(git_repository_index(&index, repo));

	cl_git_append2file("renames/untimely.txt", "Oh that crazy Kipling!\r\n");
	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));
	cl_git_pass(git_index_write(index));

	test_patch(
		"19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13",
		NULL,
		&opts,
		expected);

	git_index_free(index);
}

void test_diff_binary__empty_for_no_diff(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_oid id;
	git_commit *commit;
	git_tree *tree;
	git_diff *diff;
	git_str actual = GIT_STR_INIT;

	opts.flags = GIT_DIFF_SHOW_BINARY | GIT_DIFF_FORCE_BINARY;
	opts.id_abbrev = GIT_OID_SHA1_HEXSIZE;

	repo = cl_git_sandbox_init("renames");

	cl_git_pass(git_oid__fromstr(&id, "19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13", GIT_OID_SHA1));
	cl_git_pass(git_commit_lookup(&commit, repo, &id));
	cl_git_pass(git_commit_tree(&tree, commit));

	cl_git_pass(git_diff_tree_to_tree(&diff, repo, tree, tree, &opts));
	cl_git_pass(git_diff_print(diff, GIT_DIFF_FORMAT_PATCH, git_diff_print_callback__to_buf, &actual));

	cl_assert_equal_s("", actual.ptr);

	git_str_dispose(&actual);
	git_diff_free(diff);
	git_commit_free(commit);
	git_tree_free(tree);
}

void test_diff_binary__index_to_workdir(void)
{
	git_index *index;
	git_diff *diff;
	git_patch *patch;
	git_buf actual = GIT_BUF_INIT;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/untimely.txt b/untimely.txt\n" \
		"index 9a69d960ae94b060f56c2a8702545e2bb1abb935..1111d4f11f4b35bf6759e0fb714fe09731ef0840 100644\n" \
		"GIT binary patch\n" \
		"delta 32\n" \
		"nc%1vf+QYWt3zLL@hC)e3Vu?a>QDRl4f_G*?PG(-ZA}<#J$+QbW\n" \
		"\n" \
		"delta 7\n" \
		"Oc%18D`@*{63ljhg(E~C7\n" \
		"\n";

	opts.flags = GIT_DIFF_SHOW_BINARY | GIT_DIFF_FORCE_BINARY;
	opts.id_abbrev = GIT_OID_SHA1_HEXSIZE;

	repo = cl_git_sandbox_init("renames");
	cl_git_pass(git_repository_index(&index, repo));

	cl_git_append2file("renames/untimely.txt", "Oh that crazy Kipling!\r\n");

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, index, &opts));

	cl_git_pass(git_patch_from_diff(&patch, diff, 0));
	cl_git_pass(git_patch_to_buf(&actual, patch));

	cl_assert_equal_s(expected, actual.ptr);

	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));
	cl_git_pass(git_index_write(index));

	test_patch(
		"19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13",
		NULL,
		&opts,
		expected);

	git_buf_dispose(&actual);
	git_patch_free(patch);
	git_diff_free(diff);
	git_index_free(index);
}

static int print_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	git_str *buf = (git_str *)payload;

	GIT_UNUSED(delta);

	if (hunk)
		git_str_put(buf, hunk->header, hunk->header_len);

	if (line)
		git_str_put(buf, line->content, line->content_len);

	return git_str_oom(buf) ? -1 : 0;
}

void test_diff_binary__print_patch_from_diff(void)
{
	git_index *index;
	git_diff *diff;
	git_str actual = GIT_STR_INIT;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	const char *expected =
		"diff --git a/untimely.txt b/untimely.txt\n" \
		"index 9a69d960ae94b060f56c2a8702545e2bb1abb935..1111d4f11f4b35bf6759e0fb714fe09731ef0840 100644\n" \
		"GIT binary patch\n" \
		"delta 32\n" \
		"nc%1vf+QYWt3zLL@hC)e3Vu?a>QDRl4f_G*?PG(-ZA}<#J$+QbW\n" \
		"\n" \
		"delta 7\n" \
		"Oc%18D`@*{63ljhg(E~C7\n" \
		"\n";

	opts.flags = GIT_DIFF_SHOW_BINARY | GIT_DIFF_FORCE_BINARY;
	opts.id_abbrev = GIT_OID_SHA1_HEXSIZE;

	repo = cl_git_sandbox_init("renames");
	cl_git_pass(git_repository_index(&index, repo));

	cl_git_append2file("renames/untimely.txt", "Oh that crazy Kipling!\r\n");

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, index, &opts));

	cl_git_pass(git_diff_print(diff, GIT_DIFF_FORMAT_PATCH, print_cb, &actual));

	cl_assert_equal_s(expected, actual.ptr);

	git_str_dispose(&actual);
	git_diff_free(diff);
	git_index_free(index);
}

struct diff_data {
	char *old_path;
	git_oid old_id;
	git_str old_binary_base85;
	size_t old_binary_inflatedlen;
	git_diff_binary_t old_binary_type;

	char *new_path;
	git_oid new_id;
	git_str new_binary_base85;
	size_t new_binary_inflatedlen;
	git_diff_binary_t new_binary_type;
};

static int file_cb(
	const git_diff_delta *delta,
	float progress,
	void *payload)
{
	struct diff_data *diff_data = payload;

	GIT_UNUSED(progress);

	if (delta->old_file.path)
		diff_data->old_path = git__strdup(delta->old_file.path);

	if (delta->new_file.path)
		diff_data->new_path = git__strdup(delta->new_file.path);

	git_oid_cpy(&diff_data->old_id, &delta->old_file.id);
	git_oid_cpy(&diff_data->new_id, &delta->new_file.id);

	return 0;
}

static int binary_cb(
	const git_diff_delta *delta,
	const git_diff_binary *binary,
	void *payload)
{
	struct diff_data *diff_data = payload;

	GIT_UNUSED(delta);

	git_str_encode_base85(&diff_data->old_binary_base85,
		binary->old_file.data, binary->old_file.datalen);
	diff_data->old_binary_inflatedlen = binary->old_file.inflatedlen;
	diff_data->old_binary_type = binary->old_file.type;

	git_str_encode_base85(&diff_data->new_binary_base85,
		binary->new_file.data, binary->new_file.datalen);
	diff_data->new_binary_inflatedlen = binary->new_file.inflatedlen;
	diff_data->new_binary_type = binary->new_file.type;

	return 0;
}

static int hunk_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	void *payload)
{
	GIT_UNUSED(delta);
	GIT_UNUSED(hunk);
	GIT_UNUSED(payload);

	cl_fail("did not expect hunk callback");
	return 0;
}

static int line_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	GIT_UNUSED(delta);
	GIT_UNUSED(hunk);
	GIT_UNUSED(line);
	GIT_UNUSED(payload);

	cl_fail("did not expect line callback");
	return 0;
}

void test_diff_binary__blob_to_blob(void)
{
	git_index *index;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_blob *old_blob, *new_blob;
	git_oid old_id, new_id;
	struct diff_data diff_data = {0};

	opts.flags = GIT_DIFF_SHOW_BINARY | GIT_DIFF_FORCE_BINARY;
	opts.id_abbrev = GIT_OID_SHA1_HEXSIZE;

	repo = cl_git_sandbox_init("renames");
	cl_git_pass(git_repository_index__weakptr(&index, repo));

	cl_git_append2file("renames/untimely.txt", "Oh that crazy Kipling!\r\n");
	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));
	cl_git_pass(git_index_write(index));

	git_oid__fromstr(&old_id, "9a69d960ae94b060f56c2a8702545e2bb1abb935", GIT_OID_SHA1);
	git_oid__fromstr(&new_id, "1111d4f11f4b35bf6759e0fb714fe09731ef0840", GIT_OID_SHA1);

	cl_git_pass(git_blob_lookup(&old_blob, repo, &old_id));
	cl_git_pass(git_blob_lookup(&new_blob, repo, &new_id));

	cl_git_pass(git_diff_blobs(old_blob,
		"untimely.txt", new_blob, "untimely.txt", &opts,
		file_cb, binary_cb, hunk_cb, line_cb, &diff_data));

	cl_assert_equal_s("untimely.txt", diff_data.old_path);
	cl_assert_equal_oid(&old_id, &diff_data.old_id);
	cl_assert_equal_i(GIT_DIFF_BINARY_DELTA, diff_data.old_binary_type);
	cl_assert_equal_i(7, diff_data.old_binary_inflatedlen);
	cl_assert_equal_s("c%18D`@*{63ljhg(E~C7",
		diff_data.old_binary_base85.ptr);

	cl_assert_equal_s("untimely.txt", diff_data.new_path);
	cl_assert_equal_oid(&new_id, &diff_data.new_id);
	cl_assert_equal_i(GIT_DIFF_BINARY_DELTA, diff_data.new_binary_type);
	cl_assert_equal_i(32, diff_data.new_binary_inflatedlen);
	cl_assert_equal_s("c%1vf+QYWt3zLL@hC)e3Vu?a>QDRl4f_G*?PG(-ZA}<#J$+QbW",
		diff_data.new_binary_base85.ptr);

	git_blob_free(old_blob);
	git_blob_free(new_blob);

	git__free(diff_data.old_path);
	git__free(diff_data.new_path);

	git_str_dispose(&diff_data.old_binary_base85);
	git_str_dispose(&diff_data.new_binary_base85);
}
