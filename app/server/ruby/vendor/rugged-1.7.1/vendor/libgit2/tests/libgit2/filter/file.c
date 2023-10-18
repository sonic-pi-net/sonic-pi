#include "clar_libgit2.h"
#include "git2/sys/filter.h"
#include "crlf.h"

static git_repository *g_repo = NULL;

void test_filter_file__initialize(void)
{
	git_reference *head_ref;
	git_commit *head;

	g_repo = cl_git_sandbox_init("crlf");

	cl_git_mkfile("crlf/.gitattributes",
		"*.txt text\n*.bin binary\n*.crlf text eol=crlf\n*.lf text eol=lf\n");

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	cl_git_pass(git_repository_head(&head_ref, g_repo));
	cl_git_pass(git_reference_peel((git_object **)&head, head_ref, GIT_OBJECT_COMMIT));
	cl_git_pass(git_reset(g_repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_commit_free(head);
	git_reference_free(head_ref);
}

void test_filter_file__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_filter_file__apply(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	cl_git_pass(git_filter_list_apply_to_file(&buf, fl, g_repo, "all-crlf"));
	cl_assert_equal_s("crlf\ncrlf\ncrlf\ncrlf\n", buf.ptr);

	git_buf_dispose(&buf);
	git_filter_list_free(fl);
}

struct buf_writestream {
	git_writestream base;
	git_str buf;
};

static int buf_writestream_write(git_writestream *s, const char *buf, size_t len)
{
	struct buf_writestream *stream = (struct buf_writestream *)s;
	return git_str_put(&stream->buf, buf, len);
}

static int buf_writestream_close(git_writestream *s)
{
	GIT_UNUSED(s);
	return 0;
}

static void buf_writestream_free(git_writestream *s)
{
	struct buf_writestream *stream = (struct buf_writestream *)s;
	git_str_dispose(&stream->buf);
}

void test_filter_file__apply_stream(void)
{
	git_filter_list *fl;
	git_filter *crlf;
	struct buf_writestream write_target = { {
		buf_writestream_write,
		buf_writestream_close,
		buf_writestream_free } };

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	crlf = git_filter_lookup(GIT_FILTER_CRLF);
	cl_assert(crlf != NULL);

	cl_git_pass(git_filter_list_push(fl, crlf, NULL));

	cl_git_pass(git_filter_list_stream_file(fl, g_repo, "all-crlf", &write_target.base));
	cl_assert_equal_s("crlf\ncrlf\ncrlf\ncrlf\n", write_target.buf.ptr);

	git_filter_list_free(fl);
	write_target.base.free(&write_target.base);
}
