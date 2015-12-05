#include "clar_libgit2.h"
#include "buffer.h"
#include "posix.h"
#include "path.h"
#include "fileops.h"

static git_repository *repo;
static char textual_content[] = "libgit2\n\r\n\0";

void test_object_blob_fromchunks__initialize(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
}

void test_object_blob_fromchunks__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static int text_chunked_source_cb(char *content, size_t max_length, void *payload)
{
	int *count;

	GIT_UNUSED(max_length);

	count = (int *)payload;
	(*count)--;

	if (*count == 0)
		return 0;

	strcpy(content, textual_content);
	return (int)strlen(textual_content);
}

void test_object_blob_fromchunks__can_create_a_blob_from_a_in_memory_chunk_provider(void)
{
	git_oid expected_oid, oid;
	git_object *blob;
	int howmany = 7;

	cl_git_pass(git_oid_fromstr(&expected_oid, "321cbdf08803c744082332332838df6bd160f8f9"));

	cl_git_fail_with(
		git_object_lookup(&blob, repo, &expected_oid, GIT_OBJ_ANY),
		GIT_ENOTFOUND);

	cl_git_pass(git_blob_create_fromchunks(&oid, repo, NULL, text_chunked_source_cb, &howmany));

	cl_git_pass(git_object_lookup(&blob, repo, &expected_oid, GIT_OBJ_ANY));
	cl_assert(git_oid_cmp(&expected_oid, git_object_id(blob)) == 0);

	git_object_free(blob);
}

void test_object_blob_fromchunks__doesnot_overwrite_an_already_existing_object(void)
{
	git_buf path = GIT_BUF_INIT;
	git_buf content = GIT_BUF_INIT;
	git_oid expected_oid, oid;
	int howmany = 7;

	cl_git_pass(git_oid_fromstr(&expected_oid, "321cbdf08803c744082332332838df6bd160f8f9"));

	cl_git_pass(git_blob_create_fromchunks(&oid, repo, NULL, text_chunked_source_cb, &howmany));

	/* Let's replace the content of the blob file storage with something else... */
	cl_git_pass(git_buf_joinpath(&path, git_repository_path(repo), "objects/32/1cbdf08803c744082332332838df6bd160f8f9"));
	cl_git_pass(p_unlink(git_buf_cstr(&path)));
	cl_git_mkfile(git_buf_cstr(&path), "boom");

	/* ...request a creation of the same blob... */
	howmany = 7;
	cl_git_pass(git_blob_create_fromchunks(&oid, repo, NULL, text_chunked_source_cb, &howmany));

	/* ...and ensure the content of the faked blob file hasn't been altered */
	cl_git_pass(git_futils_readbuffer(&content, git_buf_cstr(&path)));
	cl_assert(!git__strcmp("boom", git_buf_cstr(&content)));

	git_buf_free(&path);
	git_buf_free(&content);
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

	git_buf_free(&buf);
}

static void assert_named_chunked_blob(const char *expected_sha, const char *fake_name)
{
	git_oid expected_oid, oid;
	int howmany = 7;

	cl_git_pass(git_oid_fromstr(&expected_oid, expected_sha));

	cl_git_pass(git_blob_create_fromchunks(&oid, repo, fake_name, text_chunked_source_cb, &howmany));
	cl_assert(git_oid_cmp(&expected_oid, &oid) == 0);
}

void test_object_blob_fromchunks__creating_a_blob_from_chunks_honors_the_attributes_directives(void)
{
	write_attributes(repo);

	assert_named_chunked_blob("321cbdf08803c744082332332838df6bd160f8f9", "dummy.data");
	assert_named_chunked_blob("e9671e138a780833cb689753570fd10a55be84fb", "dummy.txt");
	assert_named_chunked_blob("e9671e138a780833cb689753570fd10a55be84fb", "dummy.dunno");
}

static int failing_chunked_source_cb(
	char *content, size_t max_length, void *payload)
{
	int *count = (int *)payload;

	GIT_UNUSED(max_length);

	(*count)--;
	if (*count == 0)
		return -1234;

	strcpy(content, textual_content);
	return (int)strlen(textual_content);
}

void test_object_blob_fromchunks__can_stop_with_error(void)
{
	git_oid expected_oid, oid;
	git_object *blob;
	int howmany = 7;

	cl_git_pass(git_oid_fromstr(
		&expected_oid, "321cbdf08803c744082332332838df6bd160f8f9"));

	cl_git_fail_with(
		git_object_lookup(&blob, repo, &expected_oid, GIT_OBJ_ANY),
		GIT_ENOTFOUND);

	cl_git_fail_with(git_blob_create_fromchunks(
		&oid, repo, NULL, failing_chunked_source_cb, &howmany), -1234);

	cl_git_fail_with(
		git_object_lookup(&blob, repo, &expected_oid, GIT_OBJ_ANY),
		GIT_ENOTFOUND);
}

