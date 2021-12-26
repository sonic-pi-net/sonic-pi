#include "clar_libgit2.h"
#include "buffer.h"
#include "posix.h"
#include "path.h"
#include "futils.h"

static git_repository *repo;
static char textual_content[] = "libgit2\n\r\n\0";

void test_object_blob_fromstream__initialize(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
}

void test_object_blob_fromstream__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_object_blob_fromstream__multiple_write(void)
{
	git_oid expected_id, id;
	git_object *blob;
	git_writestream *stream;
	int i, howmany = 6;

	cl_git_pass(git_oid_fromstr(&expected_id, "321cbdf08803c744082332332838df6bd160f8f9"));

	cl_git_fail_with(GIT_ENOTFOUND,
			 git_object_lookup(&blob, repo, &expected_id, GIT_OBJECT_ANY));

	cl_git_pass(git_blob_create_from_stream(&stream, repo, NULL));

	for (i = 0; i < howmany; i++)
		cl_git_pass(stream->write(stream, textual_content, strlen(textual_content)));

	cl_git_pass(git_blob_create_from_stream_commit(&id, stream));
	cl_assert_equal_oid(&expected_id, &id);

	cl_git_pass(git_object_lookup(&blob, repo, &expected_id, GIT_OBJECT_BLOB));

	git_object_free(blob);
}

#define GITATTR "* text=auto\n" \
	"*.txt text\n" \
	"*.data binary\n"

static void write_attributes(git_repository *repo)
{
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&buf, git_repository_path(repo), "info"));
	cl_git_pass(git_buf_joinpath(&buf, git_buf_cstr(&buf), "attributes"));

	cl_git_pass(git_futils_mkpath2file(git_buf_cstr(&buf), 0777));
	cl_git_rewritefile(git_buf_cstr(&buf), GITATTR);

	git_buf_dispose(&buf);
}

static void assert_named_chunked_blob(const char *expected_sha, const char *fake_name)
{
	git_oid expected_id, id;
	git_writestream *stream;
	int i, howmany = 6;

	cl_git_pass(git_oid_fromstr(&expected_id, expected_sha));

	cl_git_pass(git_blob_create_from_stream(&stream, repo, fake_name));

	for (i = 0; i < howmany; i++)
		cl_git_pass(stream->write(stream, textual_content, strlen(textual_content)));

	cl_git_pass(git_blob_create_from_stream_commit(&id, stream));

	cl_assert_equal_oid(&expected_id, &id);
}

void test_object_blob_fromstream__creating_a_blob_from_chunks_honors_the_attributes_directives(void)
{
	write_attributes(repo);

	assert_named_chunked_blob("321cbdf08803c744082332332838df6bd160f8f9", "dummy.data");
	assert_named_chunked_blob("e9671e138a780833cb689753570fd10a55be84fb", "dummy.txt");
	assert_named_chunked_blob("e9671e138a780833cb689753570fd10a55be84fb", "dummy.dunno");
}
