#include "clar_libgit2.h"

void test_revwalk_simplify__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

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

static const char *expected_str[] = {
	"a4a7dce85cf63874e984719f4fdd239f5145052f", /* 0 */
	"c47800c7266a2be04c571c04d5a6614691ea99bd", /* 3 */
	"5b5b025afb0b4c913b4c338a42934a3863bf3644", /* 4 */
	"8496071c1b46c854b31185ea97743be6a8774479", /* 5 */
};

void test_revwalk_simplify__first_parent(void)
{
	git_repository *repo;
	git_revwalk *walk;
	git_oid id, expected[4];
	int i, error;

	for (i = 0; i < 4; i++) {
		git_oid_fromstr(&expected[i], expected_str[i]);
	}

	repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_revwalk_new(&walk, repo));

	git_oid_fromstr(&id, commit_head);
	cl_git_pass(git_revwalk_push(walk, &id));
	git_revwalk_sorting(walk, GIT_SORT_TOPOLOGICAL);
	git_revwalk_simplify_first_parent(walk);

	i = 0;
	while ((error = git_revwalk_next(&id, walk)) == 0) {
		cl_assert_equal_oid(&expected[i], &id);
		i++;
	}

	cl_assert_equal_i(i, 4);
	cl_assert_equal_i(error, GIT_ITEROVER);

	git_revwalk_free(walk);
}
