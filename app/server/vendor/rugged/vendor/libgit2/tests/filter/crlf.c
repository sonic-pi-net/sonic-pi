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
	git_buf in = { 0 }, out = { 0 };

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_WORKTREE, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	in.ptr = "Some text\nRight here\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));

#ifdef GIT_WIN32
	cl_assert_equal_s("Some text\r\nRight here\r\n", out.ptr);
#else
	cl_assert_equal_s("Some text\nRight here\n", out.ptr);
#endif

	git_filter_list_free(fl);
	git_buf_free(&out);
}

void test_filter_crlf__to_odb(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf in = { 0 }, out = { 0 };

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	in.ptr = "Some text\r\nRight here\r\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));

	cl_assert_equal_s("Some text\nRight here\n", out.ptr);

	git_filter_list_free(fl);
	git_buf_free(&out);
}

void test_filter_crlf__with_safecrlf(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf in = {0}, out = GIT_BUF_INIT;

	cl_repo_set_bool(g_repo, "core.safecrlf", true);

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	/* Normalized \r\n succeeds with safecrlf */
	in.ptr = "Normal\r\nCRLF\r\nline-endings.\r\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_s("Normal\nCRLF\nline-endings.\n", out.ptr);

	/* Mix of line endings fails with safecrlf */
	in.ptr = "Mixed\nup\r\nLF\nand\r\nCRLF\nline-endings.\r\n";
	in.size = strlen(in.ptr);

	cl_git_fail(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_i(giterr_last()->klass, GITERR_FILTER);

	/* Normalized \n is reversible, so does not fail with safecrlf */
	in.ptr = "Normal\nLF\nonly\nline-endings.\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_s(in.ptr, out.ptr);

	git_filter_list_free(fl);
	git_buf_free(&out);
}

void test_filter_crlf__with_safecrlf_and_unsafe_allowed(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf in = {0}, out = GIT_BUF_INIT;

	cl_repo_set_bool(g_repo, "core.safecrlf", true);

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, GIT_FILTER_OPT_ALLOW_UNSAFE));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	/* Normalized \r\n succeeds with safecrlf */
	in.ptr = "Normal\r\nCRLF\r\nline-endings.\r\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_s("Normal\nCRLF\nline-endings.\n", out.ptr);

	/* Mix of line endings fails with safecrlf, but allowed to pass */
	in.ptr = "Mixed\nup\r\nLF\nand\r\nCRLF\nline-endings.\r\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	/* TODO: check for warning */
	cl_assert_equal_s("Mixed\nup\nLF\nand\nCRLF\nline-endings.\n", out.ptr);

	/* Normalized \n fails with safecrlf, but allowed to pass */
	in.ptr = "Normal\nLF\nonly\nline-endings.\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	/* TODO: check for warning */
	cl_assert_equal_s("Normal\nLF\nonly\nline-endings.\n", out.ptr);

	git_filter_list_free(fl);
	git_buf_free(&out);
}

void test_filter_crlf__no_safecrlf(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf in = {0}, out = GIT_BUF_INIT;

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	/* Normalized \r\n succeeds with safecrlf */
	in.ptr = "Normal\r\nCRLF\r\nline-endings.\r\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_s("Normal\nCRLF\nline-endings.\n", out.ptr);

	/* Mix of line endings fails with safecrlf */
	in.ptr = "Mixed\nup\r\nLF\nand\r\nCRLF\nline-endings.\r\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_s("Mixed\nup\nLF\nand\nCRLF\nline-endings.\n", out.ptr);

	/* Normalized \n fails with safecrlf */
	in.ptr = "Normal\nLF\nonly\nline-endings.\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_s("Normal\nLF\nonly\nline-endings.\n", out.ptr);

	git_filter_list_free(fl);
	git_buf_free(&out);
}

void test_filter_crlf__safecrlf_warn(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf in = {0}, out = GIT_BUF_INIT;

	cl_repo_set_string(g_repo, "core.safecrlf", "warn");

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	/* Normalized \r\n succeeds with safecrlf=warn */
	in.ptr = "Normal\r\nCRLF\r\nline-endings.\r\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_s("Normal\nCRLF\nline-endings.\n", out.ptr);

	/* Mix of line endings succeeds with safecrlf=warn */
	in.ptr = "Mixed\nup\r\nLF\nand\r\nCRLF\nline-endings.\r\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	/* TODO: check for warning */
	cl_assert_equal_s("Mixed\nup\nLF\nand\nCRLF\nline-endings.\n", out.ptr);

	/* Normalized \n is reversible, so does not fail with safecrlf=warn */
	in.ptr = "Normal\nLF\nonly\nline-endings.\n";
	in.size = strlen(in.ptr);

	cl_git_pass(git_filter_list_apply_to_data(&out, fl, &in));
	cl_assert_equal_s(in.ptr, out.ptr);

	git_filter_list_free(fl);
	git_buf_free(&out);
}
