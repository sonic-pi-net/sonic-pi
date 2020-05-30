#include "clar_libgit2.h"
#include "index.h"
#include "git2/sys/index.h"
#include "git2/repository.h"
#include "../reset/reset_helpers.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH "nsecs"

/* Fixture setup and teardown */
void test_index_nsec__initialize(void)
{
	repo = cl_git_sandbox_init("nsecs");
	git_repository_index(&repo_index, repo);
}

void test_index_nsec__cleanup(void)
{
	git_index_free(repo_index);
	repo_index = NULL;

	cl_git_sandbox_cleanup();
}

static bool try_create_file_with_nsec_timestamp(const char *path)
{
	struct stat st;
	int try;

	/* retry a few times to avoid nanos *actually* equal 0 race condition */
	for (try = 0; try < 3; try++) {
		cl_git_mkfile(path, "This is hopefully a file with nanoseconds!");

		cl_must_pass(p_stat(path, &st));

		if (st.st_ctime_nsec && st.st_mtime_nsec)
			return true;
	}

	return false;
}

/* try to determine if the underlying filesystem supports a resolution
 * higher than a single second.  (i'm looking at you, hfs+)
 */
static bool should_expect_nsecs(void)
{
	git_buf nsec_path = GIT_BUF_INIT;
	bool expect;

	git_buf_joinpath(&nsec_path, clar_sandbox_path(), "nsec_test");

	expect = try_create_file_with_nsec_timestamp(nsec_path.ptr);

	p_unlink(nsec_path.ptr);

	git_buf_dispose(&nsec_path);

	return expect;
}

static bool has_nsecs(void)
{
	const git_index_entry *entry;
	size_t i;
	bool has_nsecs = false;

	for (i = 0; i < git_index_entrycount(repo_index); i++) {
		entry = git_index_get_byindex(repo_index, i);

		if (entry->ctime.nanoseconds || entry->mtime.nanoseconds) {
			has_nsecs = true;
			break;
		}
	}

	return has_nsecs;
}

void test_index_nsec__has_nanos(void)
{
	cl_assert_equal_b(true, has_nsecs());
}

void test_index_nsec__staging_maintains_other_nanos(void)
{
	const git_index_entry *entry;
	bool expect_nsec, test_file_has_nsec;

	expect_nsec = should_expect_nsecs();
	test_file_has_nsec = try_create_file_with_nsec_timestamp("nsecs/a.txt");

	cl_assert_equal_b(expect_nsec, test_file_has_nsec);

	cl_git_pass(git_index_add_bypath(repo_index, "a.txt"));
	cl_git_pass(git_index_write(repo_index));

	cl_git_pass(git_index_write(repo_index));

	git_index_read(repo_index, 1);
	cl_assert_equal_b(true, has_nsecs());

	cl_assert((entry = git_index_get_bypath(repo_index, "a.txt", 0)));

	/* if we are writing nanoseconds to the index, expect them to be
	 * nonzero.
	 */
	if (expect_nsec) {
		cl_assert(entry->ctime.nanoseconds != 0);
		cl_assert(entry->mtime.nanoseconds != 0);
	} else {
		cl_assert_equal_i(0, entry->ctime.nanoseconds);
		cl_assert_equal_i(0, entry->mtime.nanoseconds);
	}
}

void test_index_nsec__status_doesnt_clear_nsecs(void)
{
	git_status_list *statuslist;

	cl_git_pass(git_status_list_new(&statuslist, repo, NULL));

	git_index_read(repo_index, 1);
	cl_assert_equal_b(true, has_nsecs());

	git_status_list_free(statuslist);
}
