#include "clar_libgit2.h"
#include "crlf.h"

static git_repository *g_repo = NULL;

void test_filter_blob__initialize(void)
{
	g_repo = cl_git_sandbox_init("crlf");
	cl_git_mkfile("crlf/.gitattributes",
		"*.txt text\n*.bin binary\n"
		"*.crlf text eol=crlf\n"
		"*.lf text eol=lf\n"
		"*.ident text ident\n"
		"*.identcrlf ident text eol=crlf\n"
		"*.identlf ident text eol=lf\n");
}

void test_filter_blob__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_filter_blob__all_crlf(void)
{
	git_blob *blob;
	git_buf buf = { 0 };

	cl_git_pass(git_revparse_single(
		(git_object **)&blob, g_repo, "a9a2e891")); /* all-crlf */

	cl_assert_equal_s(ALL_CRLF_TEXT_RAW, git_blob_rawcontent(blob));

	cl_git_pass(git_blob_filtered_content(&buf, blob, "file.bin", 1));

	cl_assert_equal_s(ALL_CRLF_TEXT_RAW, buf.ptr);

	cl_git_pass(git_blob_filtered_content(&buf, blob, "file.crlf", 1));

	/* in this case, raw content has crlf in it already */
	cl_assert_equal_s(ALL_CRLF_TEXT_AS_CRLF, buf.ptr);

	cl_git_pass(git_blob_filtered_content(&buf, blob, "file.lf", 1));

	/* we never convert CRLF -> LF on platforms that have LF */
	cl_assert_equal_s(ALL_CRLF_TEXT_AS_CRLF, buf.ptr);

	git_buf_free(&buf);
	git_blob_free(blob);
}

void test_filter_blob__sanitizes(void)
{
	git_blob *blob;
	git_buf buf;

	cl_git_pass(git_revparse_single(
		(git_object **)&blob, g_repo, "e69de29")); /* zero-byte */

	cl_assert_equal_i(0, git_blob_rawsize(blob));
	cl_assert_equal_s("", git_blob_rawcontent(blob));

	memset(&buf, 0, sizeof(git_buf));
	cl_git_pass(git_blob_filtered_content(&buf, blob, "file.bin", 1));
	cl_assert_equal_sz(0, buf.size);
	cl_assert_equal_s("", buf.ptr);
	git_buf_free(&buf);

	memset(&buf, 0, sizeof(git_buf));
	cl_git_pass(git_blob_filtered_content(&buf, blob, "file.crlf", 1));
	cl_assert_equal_sz(0, buf.size);
	cl_assert_equal_s("", buf.ptr);
	git_buf_free(&buf);

	memset(&buf, 0, sizeof(git_buf));
	cl_git_pass(git_blob_filtered_content(&buf, blob, "file.lf", 1));
	cl_assert_equal_sz(0, buf.size);
	cl_assert_equal_s("", buf.ptr);
	git_buf_free(&buf);

	git_blob_free(blob);
}

void test_filter_blob__ident(void)
{
	git_oid id;
	git_blob *blob;
	git_buf buf = { 0 };

	cl_git_mkfile("crlf/test.ident", "Some text\n$Id$\nGoes there\n");
	cl_git_pass(git_blob_create_fromworkdir(&id, g_repo, "test.ident"));
	cl_git_pass(git_blob_lookup(&blob, g_repo, &id));
	cl_assert_equal_s(
		"Some text\n$Id$\nGoes there\n", git_blob_rawcontent(blob));
	git_blob_free(blob);

	cl_git_mkfile("crlf/test.ident", "Some text\n$Id: Any old just you want$\nGoes there\n");
	cl_git_pass(git_blob_create_fromworkdir(&id, g_repo, "test.ident"));
	cl_git_pass(git_blob_lookup(&blob, g_repo, &id));
	cl_assert_equal_s(
		"Some text\n$Id$\nGoes there\n", git_blob_rawcontent(blob));

	cl_git_pass(git_blob_filtered_content(&buf, blob, "filter.bin", 1));
	cl_assert_equal_s(
		"Some text\n$Id$\nGoes there\n", buf.ptr);

	cl_git_pass(git_blob_filtered_content(&buf, blob, "filter.identcrlf", 1));
	cl_assert_equal_s(
		"Some text\r\n$Id: 3164f585d548ac68027d22b104f2d8100b2b6845$\r\nGoes there\r\n", buf.ptr);

	cl_git_pass(git_blob_filtered_content(&buf, blob, "filter.identlf", 1));
	cl_assert_equal_s(
		"Some text\n$Id: 3164f585d548ac68027d22b104f2d8100b2b6845$\nGoes there\n", buf.ptr);

	git_buf_free(&buf);
	git_blob_free(blob);

}
