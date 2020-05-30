#include "clar_libgit2.h"
#include "futils.h"
#include "stash_helpers.h"

struct callback_data
{
	char **oids;
	int invokes;
};

static git_repository *repo;
static git_signature *signature;
static git_oid stash_tip_oid;
struct callback_data data;

#define REPO_NAME "stash"

void test_stash_foreach__initialize(void)
{
	cl_git_pass(git_signature_new(
		&signature,
		"nulltoken",
		"emeric.fermas@gmail.com",
		1323847743, 60)); /* Wed Dec 14 08:29:03 2011 +0100 */

	memset(&data, 0, sizeof(struct callback_data));
}

void test_stash_foreach__cleanup(void)
{
	git_signature_free(signature);
	signature = NULL;

	git_repository_free(repo);
	repo = NULL;

	cl_git_pass(git_futils_rmdir_r(REPO_NAME, NULL, GIT_RMDIR_REMOVE_FILES));
}

static int callback_cb(
	size_t index,
	const char* message,
	const git_oid *stash_oid,
	void *payload)
{
	struct callback_data *data = (struct callback_data *)payload;

	GIT_UNUSED(index);
	GIT_UNUSED(message);

	cl_assert_equal_i(0, git_oid_streq(stash_oid, data->oids[data->invokes++]));

	return 0;
}

void test_stash_foreach__enumerating_a_empty_repository_doesnt_fail(void)
{
	char *oids[] = { NULL };

	data.oids = oids;

	cl_git_pass(git_repository_init(&repo, REPO_NAME, 0));

	cl_git_pass(git_stash_foreach(repo, callback_cb, &data));

	cl_assert_equal_i(0, data.invokes);
}

void test_stash_foreach__can_enumerate_a_repository(void)
{
	char *oids_default[] = {
		"493568b7a2681187aaac8a58d3f1eab1527cba84", NULL };

	char *oids_untracked[] = {
		"7f89a8b15c878809c5c54d1ff8f8c9674154017b",
		"493568b7a2681187aaac8a58d3f1eab1527cba84", NULL };

	char *oids_ignored[] = {
		"c95599a8fef20a7e57582c6727b1a0d02e0a5828",
		"7f89a8b15c878809c5c54d1ff8f8c9674154017b",
		"493568b7a2681187aaac8a58d3f1eab1527cba84", NULL };

	cl_git_pass(git_repository_init(&repo, REPO_NAME, 0));

	setup_stash(repo, signature);

	cl_git_pass(git_stash_save(
		&stash_tip_oid,
		repo,
		signature,
		NULL,
		GIT_STASH_DEFAULT));

	data.oids = oids_default;

	cl_git_pass(git_stash_foreach(repo, callback_cb, &data));
	cl_assert_equal_i(1, data.invokes);

	/* ensure stash_foreach operates with INCLUDE_UNTRACKED */
	cl_git_pass(git_stash_save(
		&stash_tip_oid,
		repo,
		signature,
		NULL,
		GIT_STASH_INCLUDE_UNTRACKED));

	data.oids = oids_untracked;
	data.invokes = 0;

	cl_git_pass(git_stash_foreach(repo, callback_cb, &data));
	cl_assert_equal_i(2, data.invokes);

	/* ensure stash_foreach operates with INCLUDE_IGNORED */
	cl_git_pass(git_stash_save(
		&stash_tip_oid,
		repo,
		signature,
		NULL,
		GIT_STASH_INCLUDE_IGNORED));

	data.oids = oids_ignored;
	data.invokes = 0;

	cl_git_pass(git_stash_foreach(repo, callback_cb, &data));
	cl_assert_equal_i(3, data.invokes);
}
