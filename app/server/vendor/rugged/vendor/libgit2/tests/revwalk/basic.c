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

static const char *commit_ids[] = {
	"a4a7dce85cf63874e984719f4fdd239f5145052f", /* 0 */
	"9fd738e8f7967c078dceed8190330fc8648ee56a", /* 1 */
	"4a202b346bb0fb0db7eff3cffeb3c70babbd2045", /* 2 */
	"c47800c7266a2be04c571c04d5a6614691ea99bd", /* 3 */
	"8496071c1b46c854b31185ea97743be6a8774479", /* 4 */
	"5b5b025afb0b4c913b4c338a42934a3863bf3644", /* 5 */
};

/* Careful: there are two possible topological sorts */
static const int commit_sorting_topo[][6] = {
	{0, 1, 2, 3, 5, 4}, {0, 3, 1, 2, 5, 4}
};

static const int commit_sorting_time[][6] = {
	{0, 3, 1, 2, 5, 4}
};

static const int commit_sorting_topo_reverse[][6] = {
	{4, 5, 3, 2, 1, 0}, {4, 5, 2, 1, 3, 0}
};

static const int commit_sorting_time_reverse[][6] = {
	{4, 5, 2, 1, 3, 0}
};

static const int commit_sorting_segment[][6] = {
	{1, 2, -1, -1, -1, -1}
};

#define commit_count 6
static const int result_bytes = 24;


static int get_commit_index(git_oid *raw_oid)
{
	int i;
	char oid[40];

	git_oid_fmt(oid, raw_oid);

	for (i = 0; i < commit_count; ++i)
		if (memcmp(oid, commit_ids[i], 40) == 0)
			return i;

	return -1;
}

static int test_walk_only(git_revwalk *walk,
		const int possible_results[][commit_count], int results_count)
{
	git_oid oid;
	int i;
	int result_array[commit_count];

	for (i = 0; i < commit_count; ++i)
		result_array[i] = -1;

	i = 0;
	while (git_revwalk_next(&oid, walk) == 0) {
		result_array[i++] = get_commit_index(&oid);
		/*{
			char str[41];
			git_oid_fmt(str, &oid);
			str[40] = 0;
			printf("  %d) %s\n", i, str);
		}*/
	}

	for (i = 0; i < results_count; ++i)
		if (memcmp(possible_results[i],
				result_array, result_bytes) == 0)
			return 0;

	return GIT_ERROR;
}

static int test_walk(git_revwalk *walk, const git_oid *root,
		int flags, const int possible_results[][6], int results_count)
{
	git_revwalk_sorting(walk, flags);
	git_revwalk_push(walk, root);

	return test_walk_only(walk, possible_results, results_count);
}

static git_repository *_repo = NULL;
static git_revwalk *_walk = NULL;
static const char *_fixture = NULL;

void test_revwalk_basic__initialize(void)
{
}

void test_revwalk_basic__cleanup(void)
{
	git_revwalk_free(_walk);

	if (_fixture)
		cl_git_sandbox_cleanup();
	else
		git_repository_free(_repo);

	_fixture = NULL;
	_repo = NULL;
	_walk = NULL;
}

static void revwalk_basic_setup_walk(const char *fixture)
{
	if (fixture) {
		_fixture = fixture;
		_repo = cl_git_sandbox_init(fixture);
	} else {
		cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));
	}

	cl_git_pass(git_revwalk_new(&_walk, _repo));
}

void test_revwalk_basic__sorting_modes(void)
{
	git_oid id;

	revwalk_basic_setup_walk(NULL);

	git_oid_fromstr(&id, commit_head);

	cl_git_pass(test_walk(_walk, &id, GIT_SORT_TIME, commit_sorting_time, 1));
	cl_git_pass(test_walk(_walk, &id, GIT_SORT_TOPOLOGICAL, commit_sorting_topo, 2));
	cl_git_pass(test_walk(_walk, &id, GIT_SORT_TIME | GIT_SORT_REVERSE, commit_sorting_time_reverse, 1));
	cl_git_pass(test_walk(_walk, &id, GIT_SORT_TOPOLOGICAL | GIT_SORT_REVERSE, commit_sorting_topo_reverse, 2));
}

void test_revwalk_basic__glob_heads(void)
{
	int i = 0;
	git_oid oid;

	revwalk_basic_setup_walk(NULL);

	cl_git_pass(git_revwalk_push_glob(_walk, "heads"));

	while (git_revwalk_next(&oid, _walk) == 0) {
		i++;
	}

	/* git log --branches --oneline | wc -l => 14 */
	cl_assert_equal_i(i, 14);
}

void test_revwalk_basic__glob_heads_with_invalid(void)
{
	int i;
	git_oid oid;

	revwalk_basic_setup_walk("testrepo");

	cl_git_mkfile("testrepo/.git/refs/heads/garbage", "not-a-ref");
	cl_git_pass(git_revwalk_push_glob(_walk, "heads"));

	for (i = 0; !git_revwalk_next(&oid, _walk); ++i)
		/* walking */;

	/* git log --branches --oneline | wc -l => 16 */
	cl_assert_equal_i(17, i);
}

void test_revwalk_basic__push_head(void)
{
	int i = 0;
	git_oid oid;

	revwalk_basic_setup_walk(NULL);

	cl_git_pass(git_revwalk_push_head(_walk));

	while (git_revwalk_next(&oid, _walk) == 0) {
		i++;
	}

	/* git log HEAD --oneline | wc -l => 7 */
	cl_assert_equal_i(i, 7);
}

void test_revwalk_basic__push_head_hide_ref(void)
{
	int i = 0;
	git_oid oid;

	revwalk_basic_setup_walk(NULL);

	cl_git_pass(git_revwalk_push_head(_walk));
	cl_git_pass(git_revwalk_hide_ref(_walk, "refs/heads/packed-test"));

	while (git_revwalk_next(&oid, _walk) == 0) {
		i++;
	}

	/* git log HEAD --oneline --not refs/heads/packed-test | wc -l => 4 */
	cl_assert_equal_i(i, 4);
}

void test_revwalk_basic__push_head_hide_ref_nobase(void)
{
	int i = 0;
	git_oid oid;

	revwalk_basic_setup_walk(NULL);

	cl_git_pass(git_revwalk_push_head(_walk));
	cl_git_pass(git_revwalk_hide_ref(_walk, "refs/heads/packed"));

	while (git_revwalk_next(&oid, _walk) == 0) {
		i++;
	}

	/* git log HEAD --oneline --not refs/heads/packed | wc -l => 7 */
	cl_assert_equal_i(i, 7);
}

/*
* $ git rev-list HEAD 5b5b02 ^refs/heads/packed-test
* a65fedf39aefe402d3bb6e24df4d4f5fe4547750
* be3563ae3f795b2b4353bcce3a527ad0a4f7f644
* c47800c7266a2be04c571c04d5a6614691ea99bd
* 9fd738e8f7967c078dceed8190330fc8648ee56a

* $ git log HEAD 5b5b02 --oneline --not refs/heads/packed-test | wc -l => 4
* a65fedf
* be3563a Merge branch 'br2'
* c47800c branch commit one
* 9fd738e a fourth commit
*/
void test_revwalk_basic__multiple_push_1(void)
{
	int i = 0;
	git_oid oid;

	revwalk_basic_setup_walk(NULL);

	cl_git_pass(git_revwalk_push_head(_walk));

	cl_git_pass(git_revwalk_hide_ref(_walk, "refs/heads/packed-test"));

	cl_git_pass(git_oid_fromstr(&oid, "5b5b025afb0b4c913b4c338a42934a3863bf3644"));
	cl_git_pass(git_revwalk_push(_walk, &oid));

	while (git_revwalk_next(&oid, _walk) == 0)
		i++;

	cl_assert_equal_i(i, 4);
}

/*
* Difference between test_revwalk_basic__multiple_push_1 and 
* test_revwalk_basic__multiple_push_2 is in the order reference
* refs/heads/packed-test and commit 5b5b02 are pushed. 
* revwalk should return same commits in both the tests.

* $ git rev-list 5b5b02 HEAD ^refs/heads/packed-test
* a65fedf39aefe402d3bb6e24df4d4f5fe4547750
* be3563ae3f795b2b4353bcce3a527ad0a4f7f644
* c47800c7266a2be04c571c04d5a6614691ea99bd
* 9fd738e8f7967c078dceed8190330fc8648ee56a

* $ git log 5b5b02 HEAD --oneline --not refs/heads/packed-test | wc -l => 4
* a65fedf
* be3563a Merge branch 'br2'
* c47800c branch commit one
* 9fd738e a fourth commit
*/
void test_revwalk_basic__multiple_push_2(void)
{
	int i = 0;
	git_oid oid;

	revwalk_basic_setup_walk(NULL);

	cl_git_pass(git_oid_fromstr(&oid, "5b5b025afb0b4c913b4c338a42934a3863bf3644"));
	cl_git_pass(git_revwalk_push(_walk, &oid));

	cl_git_pass(git_revwalk_hide_ref(_walk, "refs/heads/packed-test"));

	cl_git_pass(git_revwalk_push_head(_walk));

	while (git_revwalk_next(&oid, _walk) == 0)
		i++;

	cl_assert_equal_i(i, 4);
}

void test_revwalk_basic__disallow_non_commit(void)
{
	git_oid oid;

	revwalk_basic_setup_walk(NULL);

	cl_git_pass(git_oid_fromstr(&oid, "521d87c1ec3aef9824daf6d96cc0ae3710766d91"));
	cl_git_fail(git_revwalk_push(_walk, &oid));
}

void test_revwalk_basic__push_range(void)
{
	revwalk_basic_setup_walk(NULL);

	git_revwalk_reset(_walk);
	git_revwalk_sorting(_walk, 0);
	cl_git_pass(git_revwalk_push_range(_walk, "9fd738e~2..9fd738e"));
	cl_git_pass(test_walk_only(_walk, commit_sorting_segment, 1));
}

void test_revwalk_basic__push_mixed(void)
{
	git_oid oid;
	int i = 0;

	revwalk_basic_setup_walk(NULL);

	git_revwalk_reset(_walk);
	git_revwalk_sorting(_walk, 0);
	cl_git_pass(git_revwalk_push_glob(_walk, "tags"));

	while (git_revwalk_next(&oid, _walk) == 0) {
		i++;
	}

	/* git rev-list --count --glob=tags #=> 9 */
	cl_assert_equal_i(9, i);
}

void test_revwalk_basic__push_all(void)
{
	git_oid oid;
	int i = 0;

	revwalk_basic_setup_walk(NULL);

	git_revwalk_reset(_walk);
	git_revwalk_sorting(_walk, 0);
	cl_git_pass(git_revwalk_push_glob(_walk, "*"));

	while (git_revwalk_next(&oid, _walk) == 0) {
		i++;
	}

	/* git rev-list --count --all #=> 15 */
	cl_assert_equal_i(15, i);
}

/*
* $ git rev-list br2 master e908
* a65fedf39aefe402d3bb6e24df4d4f5fe4547750
* e90810b8df3e80c413d903f631643c716887138d
* 6dcf9bf7541ee10456529833502442f385010c3d
* a4a7dce85cf63874e984719f4fdd239f5145052f
* be3563ae3f795b2b4353bcce3a527ad0a4f7f644
* c47800c7266a2be04c571c04d5a6614691ea99bd
* 9fd738e8f7967c078dceed8190330fc8648ee56a
* 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
* 5b5b025afb0b4c913b4c338a42934a3863bf3644
* 8496071c1b46c854b31185ea97743be6a8774479
*/

void test_revwalk_basic__mimic_git_rev_list(void)
{
   git_oid oid;

   revwalk_basic_setup_walk(NULL);
   git_revwalk_sorting(_walk, GIT_SORT_TIME);

   cl_git_pass(git_revwalk_push_ref(_walk, "refs/heads/br2"));
   cl_git_pass(git_revwalk_push_ref(_walk, "refs/heads/master"));
   cl_git_pass(git_oid_fromstr(&oid, "e90810b8df3e80c413d903f631643c716887138d"));
   cl_git_pass(git_revwalk_push(_walk, &oid));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "e90810b8df3e80c413d903f631643c716887138d"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "6dcf9bf7541ee10456529833502442f385010c3d"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "a4a7dce85cf63874e984719f4fdd239f5145052f"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "c47800c7266a2be04c571c04d5a6614691ea99bd"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "9fd738e8f7967c078dceed8190330fc8648ee56a"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "4a202b346bb0fb0db7eff3cffeb3c70babbd2045"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "5b5b025afb0b4c913b4c338a42934a3863bf3644"));

   cl_git_pass(git_revwalk_next(&oid, _walk));
   cl_assert(!git_oid_streq(&oid, "8496071c1b46c854b31185ea97743be6a8774479"));

   cl_git_fail_with(git_revwalk_next(&oid, _walk), GIT_ITEROVER);
}
