#include "clar_libgit2.h"
#include "index.h"

static const size_t index_entry_count = 109;
static const size_t index_entry_count_2 = 1437;
#define TEST_INDEX_PATH cl_fixture("testrepo.git/index")
#define TEST_INDEX2_PATH cl_fixture("gitgit.index")
#define TEST_INDEXBIG_PATH cl_fixture("big.index")
#define TEST_INDEXBAD_PATH cl_fixture("bad.index")


/* Suite data */
struct test_entry {
   size_t index;
   char path[128];
   git_off_t file_size;
   git_time_t mtime;
};

static struct test_entry test_entries[] = {
   {4, "Makefile", 5064, 0x4C3F7F33},
   {62, "tests/Makefile", 2631, 0x4C3F7F33},
   {36, "src/index.c", 10014, 0x4C43368D},
   {6, "git.git-authors", 2709, 0x4C3F7F33},
   {48, "src/revobject.h", 1448, 0x4C3F7FE2}
};

/* Helpers */
static void copy_file(const char *src, const char *dst)
{
	git_buf source_buf = GIT_BUF_INIT;
	git_file dst_fd;

	cl_git_pass(git_futils_readbuffer(&source_buf, src));

	dst_fd = git_futils_creat_withpath(dst, 0777, 0666); /* -V536 */
	if (dst_fd < 0)
		goto cleanup;

	cl_git_pass(p_write(dst_fd, source_buf.ptr, source_buf.size));

cleanup:
	git_buf_free(&source_buf);
	p_close(dst_fd);
}

static void files_are_equal(const char *a, const char *b)
{
	git_buf buf_a = GIT_BUF_INIT;
	git_buf buf_b = GIT_BUF_INIT;
	int pass;

	if (git_futils_readbuffer(&buf_a, a) < 0)
		cl_assert(0);

	if (git_futils_readbuffer(&buf_b, b) < 0) {
		git_buf_free(&buf_a);
		cl_assert(0);
	}

	pass = (buf_a.size == buf_b.size && !memcmp(buf_a.ptr, buf_b.ptr, buf_a.size));

	git_buf_free(&buf_a);
	git_buf_free(&buf_b);

	cl_assert(pass);
}


/* Fixture setup and teardown */
void test_index_tests__initialize(void)
{
}

void test_index_tests__empty_index(void)
{
   git_index *index;

   cl_git_pass(git_index_open(&index, "in-memory-index"));
   cl_assert(index->on_disk == 0);

   cl_assert(git_index_entrycount(index) == 0);
   cl_assert(git_vector_is_sorted(&index->entries));

   git_index_free(index);
}

void test_index_tests__default_test_index(void)
{
   git_index *index;
   unsigned int i;
   git_index_entry **entries;

   cl_git_pass(git_index_open(&index, TEST_INDEX_PATH));
   cl_assert(index->on_disk);

   cl_assert(git_index_entrycount(index) == index_entry_count);
   cl_assert(git_vector_is_sorted(&index->entries));

   entries = (git_index_entry **)index->entries.contents;

   for (i = 0; i < ARRAY_SIZE(test_entries); ++i) {
		git_index_entry *e = entries[test_entries[i].index];

		cl_assert_equal_s(e->path, test_entries[i].path);
		cl_assert_equal_i(e->mtime.seconds, test_entries[i].mtime);
		cl_assert_equal_i(e->file_size, test_entries[i].file_size);
   }

   git_index_free(index);
}

void test_index_tests__gitgit_index(void)
{
   git_index *index;

   cl_git_pass(git_index_open(&index, TEST_INDEX2_PATH));
   cl_assert(index->on_disk);

   cl_assert(git_index_entrycount(index) == index_entry_count_2);
   cl_assert(git_vector_is_sorted(&index->entries));
   cl_assert(index->tree != NULL);

   git_index_free(index);
}

void test_index_tests__find_in_existing(void)
{
   git_index *index;
   unsigned int i;

   cl_git_pass(git_index_open(&index, TEST_INDEX_PATH));

   for (i = 0; i < ARRAY_SIZE(test_entries); ++i) {
		size_t idx;

		cl_assert(!git_index_find(&idx, index, test_entries[i].path));
		cl_assert(idx == test_entries[i].index);
   }

   git_index_free(index);
}

void test_index_tests__find_in_empty(void)
{
   git_index *index;
   unsigned int i;

   cl_git_pass(git_index_open(&index, "fake-index"));

   for (i = 0; i < ARRAY_SIZE(test_entries); ++i) {
		cl_assert(GIT_ENOTFOUND == git_index_find(NULL, index, test_entries[i].path));
   }

   git_index_free(index);
}

void test_index_tests__find_prefix(void)
{
   git_index *index;
   const git_index_entry *entry;
   size_t pos;

   cl_git_pass(git_index_open(&index, TEST_INDEX_PATH));

   cl_git_pass(git_index_find_prefix(&pos, index, "src"));
   entry = git_index_get_byindex(index, pos);
   cl_assert(git__strcmp(entry->path, "src/block-sha1/sha1.c") == 0);

   cl_git_pass(git_index_find_prefix(&pos, index, "src/co"));
   entry = git_index_get_byindex(index, pos);
   cl_assert(git__strcmp(entry->path, "src/commit.c") == 0);

   cl_assert(GIT_ENOTFOUND == git_index_find_prefix(NULL, index, "blah"));

   git_index_free(index);
}

void test_index_tests__write(void)
{
   git_index *index;

   copy_file(TEST_INDEXBIG_PATH, "index_rewrite");

   cl_git_pass(git_index_open(&index, "index_rewrite"));
   cl_assert(index->on_disk);

   cl_git_pass(git_index_write(index));
   files_are_equal(TEST_INDEXBIG_PATH, "index_rewrite");

   git_index_free(index);

   p_unlink("index_rewrite");
}

void test_index_tests__sort0(void)
{
	/* sort the entires in an index */

   /*
   * TODO: This no longer applies:
   * index sorting in Git uses some specific changes to the way
   * directories are sorted.
   *
   * We need to specificially check for this by creating a new
   * index, adding entries in random order and then
   * checking for consistency
   */
}

void test_index_tests__sort1(void)
{
   /* sort the entires in an empty index */
   git_index *index;

   cl_git_pass(git_index_open(&index, "fake-index"));

   /* FIXME: this test is slightly dumb */
   cl_assert(git_vector_is_sorted(&index->entries));

   git_index_free(index);
}

static void cleanup_myrepo(void *opaque)
{
	GIT_UNUSED(opaque);
	cl_fixture_cleanup("myrepo");
}

void test_index_tests__add(void)
{
	git_index *index;
	git_filebuf file = GIT_FILEBUF_INIT;
	git_repository *repo;
	const git_index_entry *entry;
	git_oid id1;

	cl_set_cleanup(&cleanup_myrepo, NULL);

	/* Intialize a new repository */
	cl_git_pass(git_repository_init(&repo, "./myrepo", 0));

	/* Ensure we're the only guy in the room */
	cl_git_pass(git_repository_index(&index, repo));
	cl_assert(git_index_entrycount(index) == 0);

	/* Create a new file in the working directory */
	cl_git_pass(git_futils_mkpath2file("myrepo/test.txt", 0777));
	cl_git_pass(git_filebuf_open(&file, "myrepo/test.txt", 0, 0666));
	cl_git_pass(git_filebuf_write(&file, "hey there\n", 10));
	cl_git_pass(git_filebuf_commit(&file));

	/* Store the expected hash of the file/blob
	 * This has been generated by executing the following
	 * $ echo "hey there" | git hash-object --stdin
	 */
	cl_git_pass(git_oid_fromstr(&id1, "a8233120f6ad708f843d861ce2b7228ec4e3dec6"));

	/* Add the new file to the index */
	cl_git_pass(git_index_add_bypath(index, "test.txt"));

	/* Wow... it worked! */
	cl_assert(git_index_entrycount(index) == 1);
	entry = git_index_get_byindex(index, 0);

	/* And the built-in hashing mechanism worked as expected */
	cl_assert_equal_oid(&id1, &entry->id);

	/* Test access by path instead of index */
	cl_assert((entry = git_index_get_bypath(index, "test.txt", 0)) != NULL);
	cl_assert_equal_oid(&id1, &entry->id);

	git_index_free(index);
	git_repository_free(repo);
}

void test_index_tests__add_frombuffer(void)
{
	git_index *index;
	git_repository *repo;
        git_index_entry entry;
	const git_index_entry *returned_entry;

	git_oid id1;
	git_blob *blob;

	const char *content = "hey there\n";

	cl_set_cleanup(&cleanup_myrepo, NULL);

	/* Intialize a new repository */
	cl_git_pass(git_repository_init(&repo, "./myrepo", 0));

	/* Ensure we're the only guy in the room */
	cl_git_pass(git_repository_index(&index, repo));
	cl_assert(git_index_entrycount(index) == 0);

	/* Store the expected hash of the file/blob
	 * This has been generated by executing the following
	 * $ echo "hey there" | git hash-object --stdin
	 */
	cl_git_pass(git_oid_fromstr(&id1, "a8233120f6ad708f843d861ce2b7228ec4e3dec6"));

	/* Add the new file to the index */
	memset(&entry, 0x0, sizeof(git_index_entry));
	entry.mode = GIT_FILEMODE_BLOB;
	entry.path = "test.txt";
	cl_git_pass(git_index_add_frombuffer(index, &entry,
		content, strlen(content)));

	/* Wow... it worked! */
	cl_assert(git_index_entrycount(index) == 1);
	returned_entry = git_index_get_byindex(index, 0);

	/* And the built-in hashing mechanism worked as expected */
	cl_assert_equal_oid(&id1, &returned_entry->id);
	/* And mode is the one asked */
	cl_assert_equal_i(GIT_FILEMODE_BLOB, returned_entry->mode);

	/* Test access by path instead of index */
	cl_assert((returned_entry = git_index_get_bypath(index, "test.txt", 0)) != NULL);
	cl_assert_equal_oid(&id1, &returned_entry->id);

	/* Test the blob is in the repository */
	cl_git_pass(git_blob_lookup(&blob, repo, &id1));
	cl_assert_equal_s(
		content, git_blob_rawcontent(blob));
	git_blob_free(blob);

	git_index_free(index);
	git_repository_free(repo);
}

void test_index_tests__add_frombuffer_reset_entry(void)
{
	git_index *index;
	git_repository *repo;
        git_index_entry entry;
	const git_index_entry *returned_entry;
	git_filebuf file = GIT_FILEBUF_INIT;

	git_oid id1;
	git_blob *blob;
	const char *old_content = "here\n";
	const char *content = "hey there\n";

	cl_set_cleanup(&cleanup_myrepo, NULL);

	/* Intialize a new repository */
	cl_git_pass(git_repository_init(&repo, "./myrepo", 0));
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_futils_mkpath2file("myrepo/test.txt", 0777));
	cl_git_pass(git_filebuf_open(&file, "myrepo/test.txt", 0, 0666));
	cl_git_pass(git_filebuf_write(&file, old_content, strlen(old_content)));
	cl_git_pass(git_filebuf_commit(&file));

	/* Store the expected hash of the file/blob
	 * This has been generated by executing the following
	 * $ echo "hey there" | git hash-object --stdin
	 */
	cl_git_pass(git_oid_fromstr(&id1, "a8233120f6ad708f843d861ce2b7228ec4e3dec6"));

	cl_git_pass(git_index_add_bypath(index, "test.txt"));

	/* Add the new file to the index */
	memset(&entry, 0x0, sizeof(git_index_entry));
	entry.mode = GIT_FILEMODE_BLOB;
	entry.path = "test.txt";
	cl_git_pass(git_index_add_frombuffer(index, &entry,
		content, strlen(content)));

	/* Wow... it worked! */
	cl_assert(git_index_entrycount(index) == 1);
	returned_entry = git_index_get_byindex(index, 0);

	/* And the built-in hashing mechanism worked as expected */
	cl_assert_equal_oid(&id1, &returned_entry->id);
	/* And mode is the one asked */
	cl_assert_equal_i(GIT_FILEMODE_BLOB, returned_entry->mode);

	/* Test access by path instead of index */
	cl_assert((returned_entry = git_index_get_bypath(index, "test.txt", 0)) != NULL);
	cl_assert_equal_oid(&id1, &returned_entry->id);
	cl_assert_equal_i(0, returned_entry->dev);
	cl_assert_equal_i(0, returned_entry->ino);
	cl_assert_equal_i(0, returned_entry->uid);
	cl_assert_equal_i(0, returned_entry->uid);
	cl_assert_equal_i(10, returned_entry->file_size);

            /* Test the blob is in the repository */
	cl_git_pass(git_blob_lookup(&blob, repo, &id1));
	cl_assert_equal_s(content, git_blob_rawcontent(blob));
	git_blob_free(blob);

	git_index_free(index);
	git_repository_free(repo);
}

static void cleanup_1397(void *opaque)
{
	GIT_UNUSED(opaque);
	cl_git_sandbox_cleanup();
}

void test_index_tests__add_issue_1397(void)
{
	git_index *index;
	git_repository *repo;
	const git_index_entry *entry;
	git_oid id1;

	cl_set_cleanup(&cleanup_1397, NULL);

	repo = cl_git_sandbox_init("issue_1397");

	cl_repo_set_bool(repo, "core.autocrlf", true);

	/* Ensure we're the only guy in the room */
	cl_git_pass(git_repository_index(&index, repo));

	/* Store the expected hash of the file/blob
	 * This has been generated by executing the following
	 * $ git hash-object crlf_file.txt
	 */
	cl_git_pass(git_oid_fromstr(&id1, "8312e0889a9cbab77c732b6bc39b51a683e3a318"));

	/* Make sure the initial SHA-1 is correct */
	cl_assert((entry = git_index_get_bypath(index, "crlf_file.txt", 0)) != NULL);
	cl_assert_equal_oid(&id1, &entry->id);

	/* Update the index */
	cl_git_pass(git_index_add_bypath(index, "crlf_file.txt"));

	/* Check the new SHA-1 */
	cl_assert((entry = git_index_get_bypath(index, "crlf_file.txt", 0)) != NULL);
	cl_assert_equal_oid(&id1, &entry->id);

	git_index_free(index);
}

void test_index_tests__add_bypath_to_a_bare_repository_returns_EBAREPO(void)
{
	git_repository *bare_repo;
	git_index *index;

	cl_git_pass(git_repository_open(&bare_repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_repository_index(&index, bare_repo));

	cl_assert_equal_i(GIT_EBAREREPO, git_index_add_bypath(index, "test.txt"));

	git_index_free(index);
	git_repository_free(bare_repo);
}

static void add_invalid_filename(git_repository *repo, const char *fn)
{
	git_index *index;
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_repository_index(&index, repo));
	cl_assert(git_index_entrycount(index) == 0);

	git_buf_joinpath(&path, "./invalid", fn);

	cl_git_mkfile(path.ptr, NULL);
	cl_git_fail(git_index_add_bypath(index, fn));
	cl_must_pass(p_unlink(path.ptr));

	cl_assert(git_index_entrycount(index) == 0);

	git_buf_free(&path);
	git_index_free(index);
}

/* Test that writing an invalid filename fails */
void test_index_tests__add_invalid_filename(void)
{
	git_repository *repo;

	p_mkdir("invalid", 0700);

	cl_git_pass(git_repository_init(&repo, "./invalid", 0));
	cl_must_pass(p_mkdir("./invalid/subdir", 0777));

	/* cl_git_mkfile() needs the dir to exist */
	if (!git_path_exists("./invalid/.GIT"))
		cl_must_pass(p_mkdir("./invalid/.GIT", 0777));
	if (!git_path_exists("./invalid/.GiT"))
		cl_must_pass(p_mkdir("./invalid/.GiT", 0777));

	add_invalid_filename(repo, ".git/hello");
	add_invalid_filename(repo, ".GIT/hello");
	add_invalid_filename(repo, ".GiT/hello");
	add_invalid_filename(repo, "./.git/hello");
	add_invalid_filename(repo, "./foo");
	add_invalid_filename(repo, "./bar");
	add_invalid_filename(repo, "subdir/../bar");

	git_repository_free(repo);

	cl_fixture_cleanup("invalid");
}

static void replace_char(char *str, char in, char out)
{
	char *c = str;

	while (*c++)
		if (*c == in)
			*c = out;
}

static void write_invalid_filename(git_repository *repo, const char *fn_orig)
{
	git_index *index;
	git_oid expected;
	const git_index_entry *entry;
	git_buf path = GIT_BUF_INIT;
	char *fn;

	cl_git_pass(git_repository_index(&index, repo));
	cl_assert(git_index_entrycount(index) == 0);

	/*
	 * Sneak a valid path into the index, we'll update it
	 * to an invalid path when we try to write the index.
	 */
	fn = git__strdup(fn_orig);
	replace_char(fn, '/', '_');

	git_buf_joinpath(&path, "./invalid", fn);

	cl_git_mkfile(path.ptr, NULL);

	cl_git_pass(git_index_add_bypath(index, fn));

	cl_assert(entry = git_index_get_bypath(index, fn, 0));

	/* kids, don't try this at home */
	replace_char((char *)entry->path, '_', '/');

	/* write-tree */
	cl_git_fail(git_index_write_tree(&expected, index));

	p_unlink(path.ptr);

	cl_git_pass(git_index_remove_all(index, NULL, NULL, NULL));
	git_buf_free(&path);
	git_index_free(index);
	git__free(fn);
}

/* Test that writing an invalid filename fails */
void test_index_tests__write_invalid_filename(void)
{
	git_repository *repo;

	p_mkdir("invalid", 0700);

	cl_git_pass(git_repository_init(&repo, "./invalid", 0));

	write_invalid_filename(repo, ".git/hello");
	write_invalid_filename(repo, ".GIT/hello");
	write_invalid_filename(repo, ".GiT/hello");
	write_invalid_filename(repo, "./.git/hello");
	write_invalid_filename(repo, "./foo");
	write_invalid_filename(repo, "./bar");
	write_invalid_filename(repo, "foo/../bar");

	git_repository_free(repo);

	cl_fixture_cleanup("invalid");
}

void test_index_tests__honors_protect_filesystems(void)
{
	git_repository *repo;

	p_mkdir("invalid", 0700);

	cl_git_pass(git_repository_init(&repo, "./invalid", 0));

	cl_repo_set_bool(repo, "core.protectHFS", true);
	cl_repo_set_bool(repo, "core.protectNTFS", true);

	write_invalid_filename(repo, ".git./hello");
	write_invalid_filename(repo, ".git\xe2\x80\xad/hello");
	write_invalid_filename(repo, "git~1/hello");
	write_invalid_filename(repo, ".git\xe2\x81\xaf/hello");

	git_repository_free(repo);

	cl_fixture_cleanup("invalid");
}

void test_index_tests__remove_entry(void)
{
	git_repository *repo;
	git_index *index;

	p_mkdir("index_test", 0770);

	cl_git_pass(git_repository_init(&repo, "index_test", 0));
	cl_git_pass(git_repository_index(&index, repo));
	cl_assert(git_index_entrycount(index) == 0);

	cl_git_mkfile("index_test/hello", NULL);
	cl_git_pass(git_index_add_bypath(index, "hello"));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_index_read(index, true)); /* reload */
	cl_assert(git_index_entrycount(index) == 1);
	cl_assert(git_index_get_bypath(index, "hello", 0) != NULL);

	cl_git_pass(git_index_remove(index, "hello", 0));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_index_read(index, true)); /* reload */
	cl_assert(git_index_entrycount(index) == 0);
	cl_assert(git_index_get_bypath(index, "hello", 0) == NULL);

	git_index_free(index);
	git_repository_free(repo);
	cl_fixture_cleanup("index_test");
}

void test_index_tests__remove_directory(void)
{
	git_repository *repo;
	git_index *index;

	p_mkdir("index_test", 0770);

	cl_git_pass(git_repository_init(&repo, "index_test", 0));
	cl_git_pass(git_repository_index(&index, repo));
	cl_assert_equal_i(0, (int)git_index_entrycount(index));

	p_mkdir("index_test/a", 0770);
	cl_git_mkfile("index_test/a/1.txt", NULL);
	cl_git_mkfile("index_test/a/2.txt", NULL);
	cl_git_mkfile("index_test/a/3.txt", NULL);
	cl_git_mkfile("index_test/b.txt", NULL);

	cl_git_pass(git_index_add_bypath(index, "a/1.txt"));
	cl_git_pass(git_index_add_bypath(index, "a/2.txt"));
	cl_git_pass(git_index_add_bypath(index, "a/3.txt"));
	cl_git_pass(git_index_add_bypath(index, "b.txt"));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_index_read(index, true)); /* reload */
	cl_assert_equal_i(4, (int)git_index_entrycount(index));
	cl_assert(git_index_get_bypath(index, "a/1.txt", 0) != NULL);
	cl_assert(git_index_get_bypath(index, "a/2.txt", 0) != NULL);
	cl_assert(git_index_get_bypath(index, "b.txt", 0) != NULL);

	cl_git_pass(git_index_remove(index, "a/1.txt", 0));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_index_read(index, true)); /* reload */
	cl_assert_equal_i(3, (int)git_index_entrycount(index));
	cl_assert(git_index_get_bypath(index, "a/1.txt", 0) == NULL);
	cl_assert(git_index_get_bypath(index, "a/2.txt", 0) != NULL);
	cl_assert(git_index_get_bypath(index, "b.txt", 0) != NULL);

	cl_git_pass(git_index_remove_directory(index, "a", 0));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_index_read(index, true)); /* reload */
	cl_assert_equal_i(1, (int)git_index_entrycount(index));
	cl_assert(git_index_get_bypath(index, "a/1.txt", 0) == NULL);
	cl_assert(git_index_get_bypath(index, "a/2.txt", 0) == NULL);
	cl_assert(git_index_get_bypath(index, "b.txt", 0) != NULL);

	git_index_free(index);
	git_repository_free(repo);
	cl_fixture_cleanup("index_test");
}

void test_index_tests__preserves_case(void)
{
	git_repository *repo;
	git_index *index;
	const git_index_entry *entry;
	int index_caps;

	cl_set_cleanup(&cleanup_myrepo, NULL);

	cl_git_pass(git_repository_init(&repo, "./myrepo", 0));
	cl_git_pass(git_repository_index(&index, repo));

	index_caps = git_index_caps(index);

	cl_git_rewritefile("myrepo/test.txt", "hey there\n");
	cl_git_pass(git_index_add_bypath(index, "test.txt"));

	cl_git_pass(p_rename("myrepo/test.txt", "myrepo/TEST.txt"));
	cl_git_rewritefile("myrepo/TEST.txt", "hello again\n");
	cl_git_pass(git_index_add_bypath(index, "TEST.txt"));

	if (index_caps & GIT_INDEXCAP_IGNORE_CASE)
		cl_assert_equal_i(1, (int)git_index_entrycount(index));
	else
		cl_assert_equal_i(2, (int)git_index_entrycount(index));

	/* Test access by path instead of index */
	cl_assert((entry = git_index_get_bypath(index, "test.txt", 0)) != NULL);
	/* The path should *not* have changed without an explicit remove */
	cl_assert(git__strcmp(entry->path, "test.txt") == 0);

	cl_assert((entry = git_index_get_bypath(index, "TEST.txt", 0)) != NULL);
	if (index_caps & GIT_INDEXCAP_IGNORE_CASE)
		/* The path should *not* have changed without an explicit remove */
		cl_assert(git__strcmp(entry->path, "test.txt") == 0);
	else
		cl_assert(git__strcmp(entry->path, "TEST.txt") == 0);

	git_index_free(index);
	git_repository_free(repo);
}

void test_index_tests__elocked(void)
{
	git_repository *repo;
	git_index *index;
	git_filebuf file = GIT_FILEBUF_INIT;
	const git_error *err;
	int error;

	cl_set_cleanup(&cleanup_myrepo, NULL);

	cl_git_pass(git_repository_init(&repo, "./myrepo", 0));
	cl_git_pass(git_repository_index(&index, repo));

	/* Lock the index file so we fail to lock it */
	cl_git_pass(git_filebuf_open(&file, index->index_file_path, 0, 0666));
	error = git_index_write(index);
	cl_assert_equal_i(GIT_ELOCKED, error);

	err = giterr_last();
	cl_assert_equal_i(err->klass, GITERR_INDEX);

	git_filebuf_cleanup(&file);
	git_index_free(index);
	git_repository_free(repo);
}

void test_index_tests__reload_from_disk(void)
{
	git_repository *repo;
	git_index *read_index;
	git_index *write_index;

	cl_set_cleanup(&cleanup_myrepo, NULL);

	cl_git_pass(git_futils_mkdir("./myrepo", 0777, GIT_MKDIR_PATH));
	cl_git_mkfile("./myrepo/a.txt", "a\n");
	cl_git_mkfile("./myrepo/b.txt", "b\n");

	cl_git_pass(git_repository_init(&repo, "./myrepo", 0));
	cl_git_pass(git_repository_index(&write_index, repo));
	cl_assert_equal_i(false, write_index->on_disk);

	cl_git_pass(git_index_open(&read_index, write_index->index_file_path));
	cl_assert_equal_i(false, read_index->on_disk);

	/* Stage two new files against the write_index */
	cl_git_pass(git_index_add_bypath(write_index, "a.txt"));
	cl_git_pass(git_index_add_bypath(write_index, "b.txt"));

	cl_assert_equal_sz(2, git_index_entrycount(write_index));

	/* Persist the index changes to disk */
	cl_git_pass(git_index_write(write_index));
	cl_assert_equal_i(true, write_index->on_disk);

	/* Sync the changes back into the read_index */
	cl_assert_equal_sz(0, git_index_entrycount(read_index));

	cl_git_pass(git_index_read(read_index, true));
	cl_assert_equal_i(true, read_index->on_disk);

	cl_assert_equal_sz(2, git_index_entrycount(read_index));

	/* Remove the index file from the filesystem */
	cl_git_pass(p_unlink(write_index->index_file_path));

	/* Sync the changes back into the read_index */
	cl_git_pass(git_index_read(read_index, true));
	cl_assert_equal_i(false, read_index->on_disk);
	cl_assert_equal_sz(0, git_index_entrycount(read_index));

	git_index_free(read_index);
	git_index_free(write_index);
	git_repository_free(repo);
}

void test_index_tests__corrupted_extension(void)
{
	git_index *index;

	cl_git_fail_with(git_index_open(&index, TEST_INDEXBAD_PATH), GIT_ERROR);
}

void test_index_tests__reload_while_ignoring_case(void)
{
	git_index *index;
	unsigned int caps;

	cl_git_pass(git_index_open(&index, TEST_INDEX_PATH));
	cl_git_pass(git_vector_verify_sorted(&index->entries));

	caps = git_index_caps(index);
	cl_git_pass(git_index_set_caps(index, caps &= ~GIT_INDEXCAP_IGNORE_CASE));
	cl_git_pass(git_index_read(index, true));
	cl_git_pass(git_vector_verify_sorted(&index->entries));
	cl_assert(git_index_get_bypath(index, ".HEADER", 0));
	cl_assert_equal_p(NULL, git_index_get_bypath(index, ".header", 0));

	cl_git_pass(git_index_set_caps(index, caps | GIT_INDEXCAP_IGNORE_CASE));
	cl_git_pass(git_index_read(index, true));
	cl_git_pass(git_vector_verify_sorted(&index->entries));
	cl_assert(git_index_get_bypath(index, ".HEADER", 0));
	cl_assert(git_index_get_bypath(index, ".header", 0));

	git_index_free(index);
}

void test_index_tests__change_icase_on_instance(void)
{
	git_index *index;
	unsigned int caps;
	const git_index_entry *e;

	cl_git_pass(git_index_open(&index, TEST_INDEX_PATH));
	cl_git_pass(git_vector_verify_sorted(&index->entries));

	caps = git_index_caps(index);
	cl_git_pass(git_index_set_caps(index, caps &= ~GIT_INDEXCAP_IGNORE_CASE));
	cl_assert_equal_i(false, index->ignore_case);
	cl_git_pass(git_vector_verify_sorted(&index->entries));
	cl_assert(e = git_index_get_bypath(index, "src/common.h", 0));
	cl_assert_equal_p(NULL, e = git_index_get_bypath(index, "SRC/Common.h", 0));
	cl_assert(e = git_index_get_bypath(index, "COPYING", 0));
	cl_assert_equal_p(NULL, e = git_index_get_bypath(index, "copying", 0));

	cl_git_pass(git_index_set_caps(index, caps | GIT_INDEXCAP_IGNORE_CASE));
	cl_assert_equal_i(true, index->ignore_case);
	cl_git_pass(git_vector_verify_sorted(&index->entries));
	cl_assert(e = git_index_get_bypath(index, "COPYING", 0));
	cl_assert_equal_s("COPYING", e->path);
	cl_assert(e = git_index_get_bypath(index, "copying", 0));
	cl_assert_equal_s("COPYING", e->path);

	git_index_free(index);
}

void test_index_tests__can_lock_index(void)
{
	git_repository *repo;
	git_index *index;
	git_indexwriter one = GIT_INDEXWRITER_INIT,
		two = GIT_INDEXWRITER_INIT;

	repo = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_indexwriter_init(&one, index));

	cl_git_fail_with(GIT_ELOCKED, git_indexwriter_init(&two, index));
	cl_git_fail_with(GIT_ELOCKED, git_index_write(index));

	cl_git_pass(git_indexwriter_commit(&one));

	cl_git_pass(git_index_write(index));

	git_indexwriter_cleanup(&one);
	git_indexwriter_cleanup(&two);
	git_index_free(index);
	cl_git_sandbox_cleanup();
}
