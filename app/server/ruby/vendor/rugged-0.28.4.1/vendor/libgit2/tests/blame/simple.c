#include "blame_helpers.h"

static git_repository *g_repo;
static git_blame *g_blame;

void test_blame_simple__initialize(void)
{
	g_repo = NULL;
	g_blame = NULL;
}

void test_blame_simple__cleanup(void)
{
	git_blame_free(g_blame);
	git_repository_free(g_repo);
}

/*
 * $ git blame -s branch_file.txt
 *    orig line no                        final line no
 * commit   V  author       timestamp                 V
 * c47800c7 1 (Scott Chacon 2010-05-25 11:58:14 -0700 1
 * a65fedf3 2 (Scott Chacon 2011-08-09 19:33:46 -0700 2
 */
void test_blame_simple__trivial_testrepo(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo/.gitted")));
	cl_git_pass(git_blame_file(&g_blame, g_repo, "branch_file.txt", NULL));

	cl_assert_equal_i(2, git_blame_get_hunk_count(g_blame));
	check_blame_hunk_index(g_repo, g_blame, 0, 1, 1, 0, "c47800c7", "branch_file.txt");
	check_blame_hunk_index(g_repo, g_blame, 1, 2, 1, 0, "a65fedf3", "branch_file.txt");
}

/*
 * $ git blame -n b.txt
 *    orig line no                          final line no
 * commit    V  author     timestamp                  V
 * da237394  1 (Ben Straub 2013-02-12 15:11:30 -0800  1
 * da237394  2 (Ben Straub 2013-02-12 15:11:30 -0800  2
 * da237394  3 (Ben Straub 2013-02-12 15:11:30 -0800  3
 * da237394  4 (Ben Straub 2013-02-12 15:11:30 -0800  4
 * ^b99f7ac  1 (Ben Straub 2013-02-12 15:10:12 -0800  5
 * 63d671eb  6 (Ben Straub 2013-02-12 15:13:04 -0800  6
 * 63d671eb  7 (Ben Straub 2013-02-12 15:13:04 -0800  7
 * 63d671eb  8 (Ben Straub 2013-02-12 15:13:04 -0800  8
 * 63d671eb  9 (Ben Straub 2013-02-12 15:13:04 -0800  9
 * 63d671eb 10 (Ben Straub 2013-02-12 15:13:04 -0800 10
 * aa06ecca  6 (Ben Straub 2013-02-12 15:14:46 -0800 11
 * aa06ecca  7 (Ben Straub 2013-02-12 15:14:46 -0800 12
 * aa06ecca  8 (Ben Straub 2013-02-12 15:14:46 -0800 13
 * aa06ecca  9 (Ben Straub 2013-02-12 15:14:46 -0800 14
 * aa06ecca 10 (Ben Straub 2013-02-12 15:14:46 -0800 15
 */
void test_blame_simple__trivial_blamerepo(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture("blametest.git")));
	cl_git_pass(git_blame_file(&g_blame, g_repo, "b.txt", NULL));

	cl_assert_equal_i(4, git_blame_get_hunk_count(g_blame));
	check_blame_hunk_index(g_repo, g_blame, 0,  1, 4, 0, "da237394", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 1,  5, 1, 1, "b99f7ac0", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 2,  6, 5, 0, "63d671eb", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 3, 11, 5, 0, "aa06ecca", "b.txt");
}


/*
 * $ git blame -n 359fc2d -- include/git2.h
 *                     orig line no                                final line no
 * commit   orig path       V  author              timestamp                  V
 * d12299fe src/git.h       1 (Vicent Martí        2010-12-03 22:22:10 +0200  1
 * 359fc2d2 include/git2.h  2 (Edward Thomson      2013-01-08 17:07:25 -0600  2
 * d12299fe src/git.h       5 (Vicent Martí        2010-12-03 22:22:10 +0200  3
 * bb742ede include/git2.h  4 (Vicent Martí        2011-09-19 01:54:32 +0300  4
 * bb742ede include/git2.h  5 (Vicent Martí        2011-09-19 01:54:32 +0300  5
 * d12299fe src/git.h      24 (Vicent Martí        2010-12-03 22:22:10 +0200  6
 * d12299fe src/git.h      25 (Vicent Martí        2010-12-03 22:22:10 +0200  7
 * d12299fe src/git.h      26 (Vicent Martí        2010-12-03 22:22:10 +0200  8
 * d12299fe src/git.h      27 (Vicent Martí        2010-12-03 22:22:10 +0200  9
 * d12299fe src/git.h      28 (Vicent Martí        2010-12-03 22:22:10 +0200 10
 * 96fab093 include/git2.h 11 (Sven Strickroth     2011-10-09 18:37:41 +0200 11
 * 9d1dcca2 src/git2.h     33 (Vicent Martí        2011-02-07 10:35:58 +0200 12
 * 44908fe7 src/git2.h     29 (Vicent Martí        2010-12-06 23:03:16 +0200 13
 * a15c550d include/git2.h 14 (Vicent Martí        2011-11-16 14:09:44 +0100 14
 * 44908fe7 src/git2.h     30 (Vicent Martí        2010-12-06 23:03:16 +0200 15
 * d12299fe src/git.h      32 (Vicent Martí        2010-12-03 22:22:10 +0200 16
 * 44908fe7 src/git2.h     33 (Vicent Martí        2010-12-06 23:03:16 +0200 17
 * d12299fe src/git.h      34 (Vicent Martí        2010-12-03 22:22:10 +0200 18
 * 44908fe7 src/git2.h     35 (Vicent Martí        2010-12-06 23:03:16 +0200 19
 * 638c2ca4 src/git2.h     36 (Vicent Martí        2010-12-18 02:10:25 +0200 20
 * 44908fe7 src/git2.h     36 (Vicent Martí        2010-12-06 23:03:16 +0200 21
 * d12299fe src/git.h      37 (Vicent Martí        2010-12-03 22:22:10 +0200 22
 * 44908fe7 src/git2.h     38 (Vicent Martí        2010-12-06 23:03:16 +0200 23
 * 44908fe7 src/git2.h     39 (Vicent Martí        2010-12-06 23:03:16 +0200 24
 * bf787bd8 include/git2.h 25 (Carlos Martín Nieto 2012-04-08 18:56:50 +0200 25
 * 0984c876 include/git2.h 26 (Scott J. Goldman    2012-11-28 18:27:43 -0800 26
 * 2f8a8ab2 src/git2.h     41 (Vicent Martí        2011-01-29 01:56:25 +0200 27
 * 27df4275 include/git2.h 47 (Michael Schubert    2011-06-28 14:13:12 +0200 28
 * a346992f include/git2.h 28 (Ben Straub          2012-05-10 09:47:14 -0700 29
 * d12299fe src/git.h      40 (Vicent Martí        2010-12-03 22:22:10 +0200 30
 * 44908fe7 src/git2.h     41 (Vicent Martí        2010-12-06 23:03:16 +0200 31
 * 44908fe7 src/git2.h     42 (Vicent Martí        2010-12-06 23:03:16 +0200 32
 * 44908fe7 src/git2.h     43 (Vicent Martí        2010-12-06 23:03:16 +0200 33
 * 44908fe7 src/git2.h     44 (Vicent Martí        2010-12-06 23:03:16 +0200 34
 * 44908fe7 src/git2.h     45 (Vicent Martí        2010-12-06 23:03:16 +0200 35
 * 65b09b1d include/git2.h 33 (Russell Belfer      2012-02-02 18:03:43 -0800 36
 * d12299fe src/git.h      46 (Vicent Martí        2010-12-03 22:22:10 +0200 37
 * 44908fe7 src/git2.h     47 (Vicent Martí        2010-12-06 23:03:16 +0200 38
 * 5d4cd003 include/git2.h 55 (Carlos Martín Nieto 2011-03-28 17:02:45 +0200 39
 * 41fb1ca0 include/git2.h 39 (Philip Kelley       2012-10-29 13:41:14 -0400 40
 * 2dc31040 include/git2.h 56 (Carlos Martín Nieto 2011-06-20 18:58:57 +0200 41
 * 764df57e include/git2.h 40 (Ben Straub          2012-06-15 13:14:43 -0700 42
 * 5280f4e6 include/git2.h 41 (Ben Straub          2012-07-31 19:39:06 -0700 43
 * 613d5eb9 include/git2.h 43 (Philip Kelley       2012-11-28 11:42:37 -0500 44
 * d12299fe src/git.h      48 (Vicent Martí        2010-12-03 22:22:10 +0200 45
 * 111ee3fe include/git2.h 41 (Vicent Martí        2012-07-11 14:37:26 +0200 46
 * f004c4a8 include/git2.h 44 (Russell Belfer      2012-08-21 17:26:39 -0700 47
 * 111ee3fe include/git2.h 42 (Vicent Martí        2012-07-11 14:37:26 +0200 48
 * 9c82357b include/git2.h 58 (Carlos Martín Nieto 2011-06-17 18:13:14 +0200 49
 * d6258deb include/git2.h 61 (Carlos Martín Nieto 2011-06-25 15:10:09 +0200 50
 * b311e313 include/git2.h 63 (Julien Miotte       2011-07-27 18:31:13 +0200 51
 * 3412391d include/git2.h 63 (Carlos Martín Nieto 2011-07-07 11:47:31 +0200 52
 * bfc9ca59 include/git2.h 43 (Russell Belfer      2012-03-28 16:45:36 -0700 53
 * bf477ed4 include/git2.h 44 (Michael Schubert    2012-02-15 00:33:38 +0100 54
 * edebceff include/git2.h 46 (nulltoken           2012-05-01 13:57:45 +0200 55
 * 743a4b3b include/git2.h 48 (nulltoken           2012-06-15 22:24:59 +0200 56
 * 0a32dca5 include/git2.h 54 (Michael Schubert    2012-08-19 22:26:32 +0200 57
 * 590fb68b include/git2.h 55 (nulltoken           2012-10-04 13:47:45 +0200 58
 * bf477ed4 include/git2.h 45 (Michael Schubert    2012-02-15 00:33:38 +0100 59
 * d12299fe src/git.h      49 (Vicent Martí        2010-12-03 22:22:10 +0200 60
 */
void test_blame_simple__trivial_libgit2(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;
	git_object *obj;

	/* If we can't open the libgit2 repo or if it isn't a full repo
	 * with proper history, just skip this test */
	if (git_repository_open(&g_repo, cl_fixture("../..")) < 0)
		cl_skip();

	if (git_repository_is_shallow(g_repo))
		cl_skip();

	if (git_revparse_single(&obj, g_repo, "359fc2d") < 0)
		cl_skip();

	git_oid_cpy(&opts.newest_commit, git_object_id(obj));
	git_object_free(obj);

	cl_git_pass(git_blame_file(&g_blame, g_repo, "include/git2.h", &opts));

	check_blame_hunk_index(g_repo, g_blame,  0,  1, 1, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame,  1,  2, 1, 0, "359fc2d2", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame,  2,  3, 1, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame,  3,  4, 2, 0, "bb742ede", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame,  4,  6, 5, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame,  5, 11, 1, 0, "96fab093", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame,  6, 12, 1, 0, "9d1dcca2", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame,  7, 13, 1, 0, "44908fe7", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame,  8, 14, 1, 0, "a15c550d", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame,  9, 15, 1, 0, "44908fe7", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 10, 16, 1, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame, 11, 17, 1, 0, "44908fe7", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 12, 18, 1, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame, 13, 19, 1, 0, "44908fe7", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 14, 20, 1, 0, "638c2ca4", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 15, 21, 1, 0, "44908fe7", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 16, 22, 1, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame, 17, 23, 2, 0, "44908fe7", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 18, 25, 1, 0, "bf787bd8", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 19, 26, 1, 0, "0984c876", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 20, 27, 1, 0, "2f8a8ab2", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 21, 28, 1, 0, "27df4275", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 22, 29, 1, 0, "a346992f", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 23, 30, 1, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame, 24, 31, 5, 0, "44908fe7", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 25, 36, 1, 0, "65b09b1d", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 26, 37, 1, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame, 27, 38, 1, 0, "44908fe7", "src/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 28, 39, 1, 0, "5d4cd003", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 29, 40, 1, 0, "41fb1ca0", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 30, 41, 1, 0, "2dc31040", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 31, 42, 1, 0, "764df57e", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 32, 43, 1, 0, "5280f4e6", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 33, 44, 1, 0, "613d5eb9", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 34, 45, 1, 0, "d12299fe", "src/git.h");
	check_blame_hunk_index(g_repo, g_blame, 35, 46, 1, 0, "111ee3fe", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 36, 47, 1, 0, "f004c4a8", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 37, 48, 1, 0, "111ee3fe", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 38, 49, 1, 0, "9c82357b", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 39, 50, 1, 0, "d6258deb", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 40, 51, 1, 0, "b311e313", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 41, 52, 1, 0, "3412391d", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 42, 53, 1, 0, "bfc9ca59", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 43, 54, 1, 0, "bf477ed4", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 44, 55, 1, 0, "edebceff", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 45, 56, 1, 0, "743a4b3b", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 46, 57, 1, 0, "0a32dca5", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 47, 58, 1, 0, "590fb68b", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 48, 59, 1, 0, "bf477ed4", "include/git2.h");
	check_blame_hunk_index(g_repo, g_blame, 49, 60, 1, 0, "d12299fe", "src/git.h");
}

/* This was leading to segfaults on some systems during cache eviction. */
void test_blame_simple__trivial_libgit2_under_cache_pressure(void)
{
	ssize_t old_max_storage = git_cache__max_storage;
	git_cache__max_storage = 1024 * 1024;
	test_blame_simple__trivial_libgit2();
	git_cache__max_storage = old_max_storage;
}

/*
 * $ git blame -n b.txt -L 8
 *    orig line no                          final line no
 * commit    V  author     timestamp                  V
 * 63d671eb  8 (Ben Straub 2013-02-12 15:13:04 -0800  8
 * 63d671eb  9 (Ben Straub 2013-02-12 15:13:04 -0800  9
 * 63d671eb 10 (Ben Straub 2013-02-12 15:13:04 -0800 10
 * aa06ecca  6 (Ben Straub 2013-02-12 15:14:46 -0800 11
 * aa06ecca  7 (Ben Straub 2013-02-12 15:14:46 -0800 12
 * aa06ecca  8 (Ben Straub 2013-02-12 15:14:46 -0800 13
 * aa06ecca  9 (Ben Straub 2013-02-12 15:14:46 -0800 14
 * aa06ecca 10 (Ben Straub 2013-02-12 15:14:46 -0800 15
 */
void test_blame_simple__can_restrict_lines_min(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("blametest.git")));

	opts.min_line = 8;
	cl_git_pass(git_blame_file(&g_blame, g_repo, "b.txt", &opts));
	cl_assert_equal_i(2, git_blame_get_hunk_count(g_blame));
	check_blame_hunk_index(g_repo, g_blame, 0,  8, 3, 0, "63d671eb", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 1, 11, 5, 0, "aa06ecca", "b.txt");
}

/*
 * $ git blame -n b.txt -L ,6
 *    orig line no                          final line no
 * commit    V  author     timestamp                  V
 * da237394  1 (Ben Straub 2013-02-12 15:11:30 -0800  1
 * da237394  2 (Ben Straub 2013-02-12 15:11:30 -0800  2
 * da237394  3 (Ben Straub 2013-02-12 15:11:30 -0800  3
 * da237394  4 (Ben Straub 2013-02-12 15:11:30 -0800  4
 * ^b99f7ac  1 (Ben Straub 2013-02-12 15:10:12 -0800  5
 * 63d671eb  6 (Ben Straub 2013-02-12 15:13:04 -0800  6
 */
void test_blame_simple__can_restrict_lines_max(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("blametest.git")));

	opts.max_line = 6;
	cl_git_pass(git_blame_file(&g_blame, g_repo, "b.txt", &opts));
	cl_assert_equal_i(3, git_blame_get_hunk_count(g_blame));
	check_blame_hunk_index(g_repo, g_blame, 0,  1, 4, 0, "da237394", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 1,  5, 1, 1, "b99f7ac0", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 2,  6, 1, 0, "63d671eb", "b.txt");
}

/*
 * $ git blame -n b.txt -L 2,7
 *    orig line no                          final line no
 * commit   V  author     timestamp                 V
 * da237394 2 (Ben Straub 2013-02-12 15:11:30 -0800 2
 * da237394 3 (Ben Straub 2013-02-12 15:11:30 -0800 3
 * da237394 4 (Ben Straub 2013-02-12 15:11:30 -0800 4
 * ^b99f7ac 1 (Ben Straub 2013-02-12 15:10:12 -0800 5
 * 63d671eb 6 (Ben Straub 2013-02-12 15:13:04 -0800 6
 * 63d671eb 7 (Ben Straub 2013-02-12 15:13:04 -0800 7
 */
void test_blame_simple__can_restrict_lines_both(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("blametest.git")));

	opts.min_line = 2;
	opts.max_line = 7;
	cl_git_pass(git_blame_file(&g_blame, g_repo, "b.txt", &opts));
	cl_assert_equal_i(3, git_blame_get_hunk_count(g_blame));
	check_blame_hunk_index(g_repo, g_blame, 0,  2, 3, 0, "da237394", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 1,  5, 1, 1, "b99f7ac0", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 2,  6, 2, 0, "63d671eb", "b.txt");
}

void test_blame_simple__can_blame_huge_file(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("blametest.git")));

	cl_git_pass(git_blame_file(&g_blame, g_repo, "huge.txt", &opts));
	cl_assert_equal_i(2, git_blame_get_hunk_count(g_blame));
	check_blame_hunk_index(g_repo, g_blame, 0, 1,     65536, 0, "4eecfea", "huge.txt");
	check_blame_hunk_index(g_repo, g_blame, 1, 65537, 1,     0, "6653ff4", "huge.txt");
}

/*
 * $ git blame -n branch_file.txt be3563a..HEAD
 *    orig line no                          final line no
 * commit   V  author       timestamp                 V
 * ^be3563a 1 (Scott Chacon 2010-05-25 11:58:27 -0700 1) hi
 * a65fedf3 2 (Scott Chacon 2011-08-09 19:33:46 -0700 2) bye!
 */
void test_blame_simple__can_restrict_to_newish_commits(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));

	{
		git_object *obj;
		cl_git_pass(git_revparse_single(&obj, g_repo, "be3563a"));
		git_oid_cpy(&opts.oldest_commit, git_object_id(obj));
		git_object_free(obj);
	}

	cl_git_pass(git_blame_file(&g_blame, g_repo, "branch_file.txt", &opts));

	cl_assert_equal_i(2, git_blame_get_hunk_count(g_blame));
	check_blame_hunk_index(g_repo, g_blame, 0,  1, 1, 1, "be3563a", "branch_file.txt");
	check_blame_hunk_index(g_repo, g_blame, 1,  2, 1, 0, "a65fedf", "branch_file.txt");
}

void test_blame_simple__can_restrict_to_first_parent_commits(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;
	opts.flags |= GIT_BLAME_FIRST_PARENT;

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("blametest.git")));

	cl_git_pass(git_blame_file(&g_blame, g_repo, "b.txt", &opts));
	cl_assert_equal_i(4, git_blame_get_hunk_count(g_blame));
	check_blame_hunk_index(g_repo, g_blame, 0,  1, 4, 0, "da237394", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 1,  5, 1, 1, "b99f7ac0", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 2,  6, 5, 0, "63d671eb", "b.txt");
	check_blame_hunk_index(g_repo, g_blame, 3, 11, 5, 0, "bc7c5ac2", "b.txt");
}
