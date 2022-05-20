#include "clar_libgit2.h"
#include "git2/sys/filter.h"
#include "buffer.h"

static git_repository *g_repo = NULL;

void test_filter_crlf__initialize(void)
{
	g_repo = cl_git_sandbox_init("crlf");

	cl_git_mkfile("crlf/.gitattributes",
		"*.txt text\n*.bin binary\n*.crlf text eol=crlf\n*.lf text eol=lf\n");

	cl_repo_set_bool(g_repo, "core.autocrlf", true);
}

void test_filter_crlf__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_filter_crlf__to_worktree(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_WORKTREE, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	in = "Some text\nRight here\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));

	cl_assert_equal_s("Some text\r\nRight here\r\n", out.ptr);

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_crlf__to_odb(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	in = "Some text\r\nRight here\r\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));

	cl_assert_equal_s("Some text\nRight here\n", out.ptr);

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_crlf__with_safecrlf(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_repo_set_bool(g_repo, "core.safecrlf", true);

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	/* Normalized \r\n succeeds with safecrlf */
	in = "Normal\r\nCRLF\r\nline-endings.\r\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_s("Normal\nCRLF\nline-endings.\n", out.ptr);

	/* Mix of line endings fails with safecrlf */
	in = "Mixed\nup\r\nLF\nand\r\nCRLF\nline-endings.\r\n";
	in_len = strlen(in);

	cl_git_fail(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_i(git_error_last()->klass, GIT_ERROR_FILTER);

	/* Normalized \n fails for autocrlf=true when safecrlf=true */
	in = "Normal\nLF\nonly\nline-endings.\n";
	in_len = strlen(in);

	cl_git_fail(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_i(git_error_last()->klass, GIT_ERROR_FILTER);

	/* String with \r but without \r\n does not fail with safecrlf */
	in = "Normal\nCR only\rand some more\nline-endings.\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_s("Normal\nCR only\rand some more\nline-endings.\n", out.ptr);

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_crlf__with_safecrlf_and_unsafe_allowed(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_repo_set_bool(g_repo, "core.safecrlf", true);

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, GIT_FILTER_ALLOW_UNSAFE));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	/* Normalized \r\n succeeds with safecrlf */
	in = "Normal\r\nCRLF\r\nline-endings.\r\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_s("Normal\nCRLF\nline-endings.\n", out.ptr);

	/* Mix of line endings fails with safecrlf, but allowed to pass */
	in = "Mixed\nup\r\nLF\nand\r\nCRLF\nline-endings.\r\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	/* TODO: check for warning */
	cl_assert_equal_s("Mixed\nup\nLF\nand\nCRLF\nline-endings.\n", out.ptr);

	/* Normalized \n fails with safecrlf, but allowed to pass */
	in = "Normal\nLF\nonly\nline-endings.\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	/* TODO: check for warning */
	cl_assert_equal_s("Normal\nLF\nonly\nline-endings.\n", out.ptr);

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_crlf__no_safecrlf(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	/* Normalized \r\n succeeds with safecrlf */
	in = "Normal\r\nCRLF\r\nline-endings.\r\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_s("Normal\nCRLF\nline-endings.\n", out.ptr);

	/* Mix of line endings fails with safecrlf */
	in = "Mixed\nup\r\nLF\nand\r\nCRLF\nline-endings.\r\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_s("Mixed\nup\nLF\nand\nCRLF\nline-endings.\n", out.ptr);

	/* Normalized \n fails with safecrlf */
	in = "Normal\nLF\nonly\nline-endings.\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_s("Normal\nLF\nonly\nline-endings.\n", out.ptr);

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_crlf__safecrlf_warn(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_repo_set_string(g_repo, "core.safecrlf", "warn");

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	/* Normalized \r\n succeeds with safecrlf=warn */
	in = "Normal\r\nCRLF\r\nline-endings.\r\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_s("Normal\nCRLF\nline-endings.\n", out.ptr);

	/* Mix of line endings succeeds with safecrlf=warn */
	in = "Mixed\nup\r\nLF\nand\r\nCRLF\nline-endings.\r\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	/* TODO: check for warning */
	cl_assert_equal_s("Mixed\nup\nLF\nand\nCRLF\nline-endings.\n", out.ptr);

	/* Normalized \n is reversible, so does not fail with safecrlf=warn */
	in = "Normal\nLF\nonly\nline-endings.\n";
	in_len = strlen(in);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));
	cl_assert_equal_s(in, out.ptr);

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}
