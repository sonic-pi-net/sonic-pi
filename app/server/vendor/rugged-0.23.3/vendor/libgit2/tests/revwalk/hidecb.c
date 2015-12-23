#include "clar_libgit2.h"
/*
*   a4a7dce [0] Merge branch 'master' into br2
|\
| * 9fd738e [1] a fourth commit
| * 4a202b3 [2] a third commit
* | c47800c [3] branch commit one
|/
* 5b5b025 [5] another commit
* 8496071 [4] testing
*/
static const char *commit_head = "a4a7dce85cf63874e984719f4fdd239f5145052f";

static const char *commit_strs[] = {
	"a4a7dce85cf63874e984719f4fdd239f5145052f", /* 0 */
	"9fd738e8f7967c078dceed8190330fc8648ee56a", /* 1 */
	"4a202b346bb0fb0db7eff3cffeb3c70babbd2045", /* 2 */
	"c47800c7266a2be04c571c04d5a6614691ea99bd", /* 3 */
	"8496071c1b46c854b31185ea97743be6a8774479", /* 4 */
	"5b5b025afb0b4c913b4c338a42934a3863bf3644", /* 5 */
};

#define commit_count 6

static git_oid commit_ids[commit_count];
static git_oid _head_id;
static git_repository *_repo;


void test_revwalk_hidecb__initialize(void)
{
	int i;

	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_oid_fromstr(&_head_id, commit_head));

	for (i = 0; i < commit_count; i++)
		cl_git_pass(git_oid_fromstr(&commit_ids[i], commit_strs[i]));

}

void test_revwalk_hidecb__cleanup(void)
{
	git_repository_free(_repo);
	_repo = NULL;
}

/* Hide all commits */
static int hide_every_commit_cb(const git_oid *commit_id, void *data)
{
	GIT_UNUSED(commit_id);
	GIT_UNUSED(data);

	return 1;
}

/* Do not hide anything */
static int hide_none_cb(const git_oid *commit_id, void *data)
{
	GIT_UNUSED(commit_id);
	GIT_UNUSED(data);

	return 0;
}

/* Hide some commits */
static int hide_commit_cb(const git_oid *commit_id, void *data)
{
	GIT_UNUSED(commit_id);
	GIT_UNUSED(data);

	return (git_oid_cmp(commit_id, &commit_ids[5]) == 0);
}

/* In payload data, pointer to a commit id is passed */
static int hide_commit_use_payload_cb(const git_oid *commit_id, void *data)
{
	git_oid *hide_commit_id = data;

	return (git_oid_cmp(commit_id, hide_commit_id) == 0);
}

void test_revwalk_hidecb__hide_all_cb(void)
{
	git_revwalk *walk;
	git_oid id;

	cl_git_pass(git_revwalk_new(&walk, _repo));
	cl_git_pass(git_revwalk_add_hide_cb(walk, hide_every_commit_cb, NULL));
	cl_git_pass(git_revwalk_push(walk, &_head_id));

	/* First call to git_revwalk_next should return GIT_ITEROVER */
	cl_assert_equal_i(GIT_ITEROVER, git_revwalk_next(&id, walk));

	git_revwalk_free(walk);
}


void test_revwalk_hidecb__hide_none_cb(void)
{
	git_revwalk *walk;
	int i, error;
	git_oid id;

	cl_git_pass(git_revwalk_new(&walk, _repo));
	cl_git_pass(git_revwalk_add_hide_cb(walk, hide_none_cb, NULL));
	cl_git_pass(git_revwalk_push(walk, &_head_id));

	/* It should return all 6 commits */
	i = 0;
	while ((error = git_revwalk_next(&id, walk)) == 0)
		i++;

	cl_assert_equal_i(i, 6);
	cl_assert_equal_i(error, GIT_ITEROVER);

	git_revwalk_free(walk);
}

void test_revwalk_hidecb__add_hide_cb_multiple_times(void)
{
	git_revwalk *walk;

	cl_git_pass(git_revwalk_new(&walk, _repo));
	cl_git_pass(git_revwalk_add_hide_cb(walk, hide_every_commit_cb, NULL));
	cl_git_fail(git_revwalk_add_hide_cb(walk, hide_every_commit_cb, NULL));

	git_revwalk_free(walk);
}

void test_revwalk_hidecb__add_hide_cb_during_walking(void)
{
	git_revwalk *walk;
	git_oid id;
	int error;

	cl_git_pass(git_revwalk_new(&walk, _repo));
	cl_git_pass(git_revwalk_push(walk, &_head_id));

	/* Start walking without adding hide callback */
	cl_git_pass(git_revwalk_next(&id, walk));

	/* Now add hide callback */
	cl_git_pass(git_revwalk_add_hide_cb(walk, hide_none_cb, NULL));

	/* walk should be reset */
	error = git_revwalk_next(&id, walk);
	cl_assert_equal_i(error, GIT_ITEROVER);

	git_revwalk_free(walk);
}

void test_revwalk_hidecb__hide_some_commits(void)
{
	git_revwalk *walk;
	git_oid id;
	int i, error;

	cl_git_pass(git_revwalk_new(&walk, _repo));
	cl_git_pass(git_revwalk_push(walk, &_head_id));

	/* Add hide callback */
	cl_git_pass(git_revwalk_add_hide_cb(walk, hide_commit_cb, NULL));

	i = 0;
	while ((error = git_revwalk_next(&id, walk)) == 0) {
		cl_assert_equal_oid(&commit_ids[i], &id);
		i++;
	}

	cl_assert_equal_i(i, 4);
	cl_assert_equal_i(error, GIT_ITEROVER);

	git_revwalk_free(walk);
}

void test_revwalk_hidecb__test_payload(void)
{
	git_revwalk *walk;
	git_oid id;
	int i, error;

	cl_git_pass(git_revwalk_new(&walk, _repo));
	cl_git_pass(git_revwalk_push(walk, &_head_id));

	/* Add hide callback, pass id of parent of initial commit as payload data */
	cl_git_pass(git_revwalk_add_hide_cb(walk, hide_commit_use_payload_cb, &commit_ids[5]));

	i = 0;
	while ((error = git_revwalk_next(&id, walk)) == 0) {
		cl_assert_equal_oid(&commit_ids[i], &id);
		i++;
	}

	/* walker should return four commits */
	cl_assert_equal_i(i, 4);
	cl_assert_equal_i(error, GIT_ITEROVER);

	git_revwalk_free(walk);
}

