#include "clar_libgit2.h"
#include "refspec.h"
#include "remote.h"

static void assert_refspec(unsigned int direction, const char *input, bool is_expected_to_be_valid)
{
	git_refspec refspec;
	int error;

	error = git_refspec__parse(&refspec, input, direction == GIT_DIRECTION_FETCH);
	git_refspec__free(&refspec);

	if (is_expected_to_be_valid)
		cl_assert_equal_i(0, error);
	else
		cl_assert_equal_i(GIT_ERROR, error);
}

void test_network_refspecs__parsing(void)
{
	// Ported from https://github.com/git/git/blob/abd2bde78bd994166900290434a2048e660dabed/t/t5511-refspec.sh

	assert_refspec(GIT_DIRECTION_PUSH, "", false);
	assert_refspec(GIT_DIRECTION_PUSH, ":", true);
	assert_refspec(GIT_DIRECTION_PUSH, "::", false);
	assert_refspec(GIT_DIRECTION_PUSH, "+:", true);

	assert_refspec(GIT_DIRECTION_FETCH, "", true);
	assert_refspec(GIT_DIRECTION_PUSH, ":", true);
	assert_refspec(GIT_DIRECTION_FETCH, "::", false);

	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/*:refs/remotes/frotz/*", true);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/*:refs/remotes/frotz", false);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads:refs/remotes/frotz/*", false);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/master:refs/remotes/frotz/xyzzy", true);

	/*
	 * These have invalid LHS, but we do not have a formal "valid sha-1
	 * expression syntax checker" so they are not checked with the current
	 * code.  They will be caught downstream anyway, but we may want to
	 * have tighter check later...
	 */
	//assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/master::refs/remotes/frotz/xyzzy", false);
	//assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/maste :refs/remotes/frotz/xyzzy", false);

	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/*:refs/remotes/frotz/*", true);
	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/*:refs/remotes/frotz", false);
	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads:refs/remotes/frotz/*", false);
	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/master:refs/remotes/frotz/xyzzy", true);
	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/master::refs/remotes/frotz/xyzzy", false);
	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/maste :refs/remotes/frotz/xyzzy", false);

	assert_refspec(GIT_DIRECTION_PUSH, "master~1:refs/remotes/frotz/backup", true);
	assert_refspec(GIT_DIRECTION_FETCH, "master~1:refs/remotes/frotz/backup", false);
	assert_refspec(GIT_DIRECTION_PUSH, "HEAD~4:refs/remotes/frotz/new", true);
	assert_refspec(GIT_DIRECTION_FETCH, "HEAD~4:refs/remotes/frotz/new", false);

	assert_refspec(GIT_DIRECTION_PUSH, "HEAD", true);
	assert_refspec(GIT_DIRECTION_FETCH, "HEAD", true);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/ nitfol", false);
	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/ nitfol", false);

	assert_refspec(GIT_DIRECTION_PUSH, "HEAD:", false);
	assert_refspec(GIT_DIRECTION_FETCH, "HEAD:", true);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/ nitfol:", false);
	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/ nitfol:", false);

	assert_refspec(GIT_DIRECTION_PUSH, ":refs/remotes/frotz/deleteme", true);
	assert_refspec(GIT_DIRECTION_FETCH, ":refs/remotes/frotz/HEAD-to-me", true);
	assert_refspec(GIT_DIRECTION_PUSH, ":refs/remotes/frotz/delete me", false);
	assert_refspec(GIT_DIRECTION_FETCH, ":refs/remotes/frotz/HEAD to me", false);

	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/*/for-linus:refs/remotes/mine/*-blah", false);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/*/for-linus:refs/remotes/mine/*-blah", false);

	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads*/for-linus:refs/remotes/mine/*", false);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads*/for-linus:refs/remotes/mine/*", false);

	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/*/*/for-linus:refs/remotes/mine/*", false);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/*/*/for-linus:refs/remotes/mine/*", false);

	assert_refspec(GIT_DIRECTION_FETCH, "refs/heads/*/for-linus:refs/remotes/mine/*", true);
	assert_refspec(GIT_DIRECTION_PUSH, "refs/heads/*/for-linus:refs/remotes/mine/*", true);

	assert_refspec(GIT_DIRECTION_FETCH, "master", true);
	assert_refspec(GIT_DIRECTION_PUSH, "master", true);

	assert_refspec(GIT_DIRECTION_FETCH, "refs/pull/*/head:refs/remotes/origin/pr/*", true);
}

static void assert_valid_transform(const char *refspec, const char *name, const char *result)
{
	git_refspec spec;
	git_buf buf = GIT_BUF_INIT;

	git_refspec__parse(&spec, refspec, true);
	cl_git_pass(git_refspec_transform(&buf, &spec, name));
	cl_assert_equal_s(result, buf.ptr);

	git_buf_free(&buf);
	git_refspec__free(&spec);
}

void test_network_refspecs__transform_mid_star(void)
{
	assert_valid_transform("refs/pull/*/head:refs/remotes/origin/pr/*", "refs/pull/23/head", "refs/remotes/origin/pr/23");
	assert_valid_transform("refs/heads/*:refs/remotes/origin/*", "refs/heads/master", "refs/remotes/origin/master");
	assert_valid_transform("refs/heads/*:refs/remotes/origin/*", "refs/heads/user/feature", "refs/remotes/origin/user/feature");
	assert_valid_transform("refs/heads/*:refs/heads/*", "refs/heads/master", "refs/heads/master");
	assert_valid_transform("refs/heads/*:refs/heads/*", "refs/heads/user/feature", "refs/heads/user/feature");
	assert_valid_transform("refs/*:refs/*", "refs/heads/master", "refs/heads/master");
}

static void assert_invalid_transform(const char *refspec, const char *name)
{
	git_refspec spec;
	git_buf buf = GIT_BUF_INIT;

	git_refspec__parse(&spec, refspec, true);
	cl_git_fail(git_refspec_transform(&buf, &spec, name));

	git_buf_free(&buf);
	git_refspec__free(&spec);
}

void test_network_refspecs__invalid(void)
{
	assert_invalid_transform("refs/heads/*:refs/remotes/origin/*", "master");
	assert_invalid_transform("refs/heads/*:refs/remotes/origin/*", "refs/headz/master");
}

static void assert_invalid_rtransform(const char *refspec, const char *name)
{
	git_refspec spec;
	git_buf buf = GIT_BUF_INIT;

	git_refspec__parse(&spec, refspec, true);
	cl_git_fail(git_refspec_rtransform(&buf, &spec, name));

	git_buf_free(&buf);
	git_refspec__free(&spec);
}

void test_network_refspecs__invalid_reverse(void)
{
	assert_invalid_rtransform("refs/heads/*:refs/remotes/origin/*", "master");
	assert_invalid_rtransform("refs/heads/*:refs/remotes/origin/*", "refs/remotes/o/master");
}
