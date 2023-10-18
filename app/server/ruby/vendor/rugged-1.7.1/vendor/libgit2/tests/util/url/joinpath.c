#include "clar_libgit2.h"
#include "net.h"

static git_net_url source, target;

void test_url_joinpath__initialize(void)
{
	memset(&source, 0, sizeof(source));
	memset(&target, 0, sizeof(target));
}

void test_url_joinpath__cleanup(void)
{
	git_net_url_dispose(&source);
	git_net_url_dispose(&target);
}

void test_url_joinpath__target_paths_and_queries(void)
{
	cl_git_pass(git_net_url_parse(&source, "http://example.com/a/b"));

	cl_git_pass(git_net_url_joinpath(&target, &source, "/c/d"));
	cl_assert_equal_s(target.path, "/a/b/c/d");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/c/d?foo"));
	cl_assert_equal_s(target.path, "/a/b/c/d");
	cl_assert_equal_s(target.query, "foo");
	git_net_url_dispose(&target);
}

void test_url_joinpath__source_query_removed(void)
{
	cl_git_pass(git_net_url_parse(&source, "http://example.com/a/b?query&one&two"));

	cl_git_pass(git_net_url_joinpath(&target, &source, "/c/d"));
	cl_assert_equal_s(target.path, "/a/b/c/d");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/c/d?foo"));
	cl_assert_equal_s(target.path, "/a/b/c/d");
	cl_assert_equal_s(target.query, "foo");
	git_net_url_dispose(&target);
}

void test_url_joinpath__source_lacks_path(void)
{
	cl_git_pass(git_net_url_parse(&source, "http://example.com"));

	cl_git_pass(git_net_url_joinpath(&target, &source, "/"));
	cl_assert_equal_s(target.path, "/");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, ""));
	cl_assert_equal_s(target.path, "/");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "asdf"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/asdf"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/foo/bar"));
	cl_assert_equal_s(target.path, "/foo/bar");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "asdf?hello"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/asdf?hello"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/foo/bar?hello"));
	cl_assert_equal_s(target.path, "/foo/bar");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);
}

void test_url_joinpath__source_is_slash(void)
{
	cl_git_pass(git_net_url_parse(&source, "http://example.com/"));

	cl_git_pass(git_net_url_joinpath(&target, &source, "/"));
	cl_assert_equal_s(target.path, "/");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, ""));
	cl_assert_equal_s(target.path, "/");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "asdf"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/asdf"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/foo/bar"));
	cl_assert_equal_s(target.path, "/foo/bar");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "asdf?hello"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/asdf?hello"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/foo/bar?hello"));
	cl_assert_equal_s(target.path, "/foo/bar");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);
}


void test_url_joinpath__source_has_query(void)
{
	cl_git_pass(git_net_url_parse(&source, "http://example.com?query"));

	cl_git_pass(git_net_url_joinpath(&target, &source, "/"));
	cl_assert_equal_s(target.path, "/");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, ""));
	cl_assert_equal_s(target.path, "/");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "asdf"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/asdf"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/foo/bar"));
	cl_assert_equal_s(target.path, "/foo/bar");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "asdf?hello"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/asdf?hello"));
	cl_assert_equal_s(target.path, "/asdf");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);

	cl_git_pass(git_net_url_joinpath(&target, &source, "/foo/bar?hello"));
	cl_assert_equal_s(target.path, "/foo/bar");
	cl_assert_equal_s(target.query, "hello");
	git_net_url_dispose(&target);
}


void test_url_joinpath__empty_query_ignored(void)
{
	cl_git_pass(git_net_url_parse(&source, "http://example.com/foo"));

	cl_git_pass(git_net_url_joinpath(&target, &source, "/bar/baz?"));
	cl_assert_equal_s(target.path, "/foo/bar/baz");
	cl_assert_equal_p(target.query, NULL);
	git_net_url_dispose(&target);
}
