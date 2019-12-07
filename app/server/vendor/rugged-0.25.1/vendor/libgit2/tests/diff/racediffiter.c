/* This test exercises the problem described in
** https://github.com/libgit2/libgit2/pull/3568
** where deleting a directory during a diff/status
** operation can cause an access violation.
**
** The "test_diff_racediffiter__basic() test confirms
** the normal operation of diff on the given repo.
**
** The "test_diff_racediffiter__racy_rmdir() test
** uses the new diff progress callback to delete
** a directory (after the initial readdir() and
** before the directory itself is visited) causing
** the recursion and iteration to fail.
*/

#include "clar_libgit2.h"
#include "diff_helpers.h"

#define ARRAY_LEN(a) (sizeof(a) / sizeof(a[0]))

void test_diff_racediffiter__initialize(void)
{
}

void test_diff_racediffiter__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

typedef struct
{
	const char *path;
	git_delta_t t;

} basic_payload;

static int notify_cb__basic(
	const git_diff *diff_so_far,
	const git_diff_delta *delta_to_add,
	const char *matched_pathspec,
	void *payload)
{
	basic_payload *exp = (basic_payload *)payload;
	basic_payload *e;

	GIT_UNUSED(diff_so_far);
	GIT_UNUSED(matched_pathspec);

	for (e = exp; e->path; e++) {
		if (strcmp(e->path, delta_to_add->new_file.path) == 0) {
			cl_assert_equal_i(e->t, delta_to_add->status);
			return 0;
		}
	}
	cl_assert(0);
	return GIT_ENOTFOUND;
}

void test_diff_racediffiter__basic(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo = cl_git_sandbox_init("diff");
	git_diff *diff;

	basic_payload exp_a[] = {
		{ "another.txt", GIT_DELTA_MODIFIED },
		{ "readme.txt", GIT_DELTA_MODIFIED },
		{ "zzzzz/", GIT_DELTA_IGNORED },
		{ NULL, 0 }
	};

	cl_must_pass(p_mkdir("diff/zzzzz", 0777));

	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_RECURSE_UNTRACKED_DIRS;
	opts.notify_cb = notify_cb__basic;
	opts.payload = exp_a;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, &opts));

	git_diff_free(diff);
}


typedef struct {
	bool first_time;
	const char *dir;
	basic_payload *basic_payload;
} racy_payload;

static int notify_cb__racy_rmdir(
	const git_diff *diff_so_far,
	const git_diff_delta *delta_to_add,
	const char *matched_pathspec,
	void *payload)
{
	racy_payload *pay = (racy_payload *)payload;

	if (pay->first_time) {
		cl_must_pass(p_rmdir(pay->dir));
		pay->first_time = false;
	}

	return notify_cb__basic(diff_so_far, delta_to_add, matched_pathspec, pay->basic_payload);
}

void test_diff_racediffiter__racy(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo = cl_git_sandbox_init("diff");
	git_diff *diff;

	basic_payload exp_a[] = {
		{ "another.txt", GIT_DELTA_MODIFIED },
		{ "readme.txt", GIT_DELTA_MODIFIED },
		{ NULL, 0 }
	};

	racy_payload pay = { true, "diff/zzzzz", exp_a };

	cl_must_pass(p_mkdir("diff/zzzzz", 0777));

	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_RECURSE_UNTRACKED_DIRS;
	opts.notify_cb = notify_cb__racy_rmdir;
	opts.payload = &pay;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, &opts));

	git_diff_free(diff);
}
