#include "clar_libgit2.h"
#include "index.h"
#include "git2/sys/index.h"
#include "git2/repository.h"
#include "../reset/reset_helpers.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH "nsecs"

// Fixture setup and teardown
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

	cl_git_rewritefile("nsecs/a.txt", "This is file A");
	cl_git_pass(git_index_add_bypath(repo_index, "a.txt"));
	cl_git_pass(git_index_write(repo_index));

	cl_git_pass(git_index_write(repo_index));

	git_index_read(repo_index, 1);
	cl_assert_equal_b(true, has_nsecs());

	cl_assert((entry = git_index_get_bypath(repo_index, "a.txt", 0)));

	/* if we are writing nanoseconds to the index, expect them to be
	 * nonzero.  if we are *not*, expect that we truncated the entry.
	 */
#ifdef GIT_USE_NSEC
	cl_assert(entry->ctime.nanoseconds != 0);
	cl_assert(entry->mtime.nanoseconds != 0);
#else
	cl_assert_equal_i(0, entry->ctime.nanoseconds);
	cl_assert_equal_i(0, entry->mtime.nanoseconds);
#endif
}

void test_index_nsec__status_doesnt_clear_nsecs(void)
{
	git_status_list *statuslist;

	cl_git_pass(git_status_list_new(&statuslist, repo, NULL));

	git_index_read(repo_index, 1);
	cl_assert_equal_b(true, has_nsecs());

	git_status_list_free(statuslist);
}
