#include "clar_libgit2.h"
#include "crlf.h"
#include "path.h"

static git_repository *g_repo = NULL;
static git_str system_attr_path = GIT_STR_INIT;

void test_filter_systemattrs__initialize(void)
{
	git_buf system_path = GIT_BUF_INIT;

	g_repo = cl_git_sandbox_init("crlf");
	cl_must_pass(p_unlink("crlf/.gitattributes"));

	cl_git_pass(git_libgit2_opts(
		GIT_OPT_GET_SEARCH_PATH, GIT_CONFIG_LEVEL_SYSTEM, &system_path));
	cl_git_pass(git_str_joinpath(&system_attr_path,
		system_path.ptr, "gitattributes"));

	cl_git_mkfile(system_attr_path.ptr,
		"*.txt text\n"
		"*.bin binary\n"
		"*.crlf text eol=crlf\n"
		"*.lf text eol=lf\n");

	git_buf_dispose(&system_path);
}

void test_filter_systemattrs__cleanup(void)
{
	cl_must_pass(p_unlink(system_attr_path.ptr));
	git_str_dispose(&system_attr_path);

	cl_git_sandbox_cleanup();
}

void test_filter_systemattrs__reads_system_attributes(void)
{
	git_blob *blob;
	git_buf buf = { 0 };

	cl_git_pass(git_revparse_single(
		(git_object **)&blob, g_repo, "799770d")); /* all-lf */

	cl_assert_equal_s(ALL_LF_TEXT_RAW, git_blob_rawcontent(blob));

	cl_git_pass(git_blob_filter(&buf, blob, "file.bin", NULL));
	cl_assert_equal_s(ALL_LF_TEXT_RAW, buf.ptr);

	cl_git_pass(git_blob_filter(&buf, blob, "file.crlf", NULL));
	cl_assert_equal_s(ALL_LF_TEXT_AS_CRLF, buf.ptr);

	cl_git_pass(git_blob_filter(&buf, blob, "file.lf", NULL));
	cl_assert_equal_s(ALL_LF_TEXT_AS_LF, buf.ptr);

	git_buf_dispose(&buf);
	git_blob_free(blob);
}

void test_filter_systemattrs__disables_system_attributes(void)
{
	git_blob *blob;
	git_buf buf = { 0 };
	git_blob_filter_options opts = GIT_BLOB_FILTER_OPTIONS_INIT;

	opts.flags |= GIT_BLOB_FILTER_NO_SYSTEM_ATTRIBUTES;

	cl_git_pass(git_revparse_single(
		(git_object **)&blob, g_repo, "799770d")); /* all-lf */

	cl_assert_equal_s(ALL_LF_TEXT_RAW, git_blob_rawcontent(blob));

	cl_git_pass(git_blob_filter(&buf, blob, "file.bin", &opts));
	cl_assert_equal_s(ALL_LF_TEXT_RAW, buf.ptr);

	/* No attributes mean these are all treated literally */
	cl_git_pass(git_blob_filter(&buf, blob, "file.crlf", &opts));
	cl_assert_equal_s(ALL_LF_TEXT_RAW, buf.ptr);

	cl_git_pass(git_blob_filter(&buf, blob, "file.lf", &opts));
	cl_assert_equal_s(ALL_LF_TEXT_RAW, buf.ptr);

	git_buf_dispose(&buf);
	git_blob_free(blob);
}
