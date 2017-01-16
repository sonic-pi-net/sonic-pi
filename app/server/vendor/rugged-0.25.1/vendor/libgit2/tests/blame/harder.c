#include "clar_libgit2.h"

#include "blame.h"


/**
 * The test repo has a history that looks like this:
 *
 * * (A) bc7c5ac
 * |\
 * | * (B) aa06ecc
 * * | (C) 63d671e
 * |/  
 * * (D) da23739
 * * (E) b99f7ac
 *
 */

static git_repository *g_repo = NULL;

void test_blame_harder__initialize(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture("blametest.git")));
}

void test_blame_harder__cleanup(void)
{
	git_repository_free(g_repo);
	g_repo = NULL;
}



void test_blame_harder__m(void)
{
	/* TODO */
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	GIT_UNUSED(opts);

	opts.flags = GIT_BLAME_TRACK_COPIES_SAME_FILE;
}


void test_blame_harder__c(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	GIT_UNUSED(opts);

	/* Attribute the first hunk in b.txt to (E), since it was cut/pasted from
	 * a.txt in (D).
	 */
	opts.flags = GIT_BLAME_TRACK_COPIES_SAME_COMMIT_MOVES;
}

void test_blame_harder__cc(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	GIT_UNUSED(opts);

	/* Attribute the second hunk in b.txt to (E), since it was copy/pasted from
	 * a.txt in (C).
	 */
	opts.flags = GIT_BLAME_TRACK_COPIES_SAME_COMMIT_COPIES;
}

void test_blame_harder__ccc(void)
{
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	GIT_UNUSED(opts);
	
	/* Attribute the third hunk in b.txt to (E).  This hunk was deleted from
	 * a.txt in (D), but reintroduced in (B).
	 */
	opts.flags = GIT_BLAME_TRACK_COPIES_ANY_COMMIT_COPIES;
}
