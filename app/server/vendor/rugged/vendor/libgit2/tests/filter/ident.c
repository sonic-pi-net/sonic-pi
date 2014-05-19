#include "clar_libgit2.h"
#include "git2/sys/filter.h"

static git_repository *g_repo = NULL;

void test_filter_ident__initialize(void)
{
	g_repo = cl_git_sandbox_init("crlf");
}

void test_filter_ident__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void add_blob_and_filter(
	const char *data,
	git_filter_list *fl,
	const char *expected)
{
	git_oid id;
	git_blob *blob;
	git_buf out = { 0 };

	cl_git_mkfile("crlf/identtest", data);
	cl_git_pass(git_blob_create_fromworkdir(&id, g_repo, "identtest"));
	cl_git_pass(git_blob_lookup(&blob, g_repo, &id));

	cl_git_pass(git_filter_list_apply_to_blob(&out, fl, blob));

	cl_assert_equal_s(expected, out.ptr);

	git_blob_free(blob);
	git_buf_free(&out);
}

void test_filter_ident__to_worktree(void)
{
	git_filter_list *fl;
	git_filter *ident;

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_WORKTREE, 0));

	ident = git_filter_lookup(GIT_FILTER_IDENT);
	cl_assert(ident != NULL);

	cl_git_pass(git_filter_list_push(fl, ident, NULL));

	add_blob_and_filter(
		"Hello\n$Id$\nFun stuff\n", fl,
		"Hello\n$Id: b69e2387aafcaf73c4de5b9ab59abe27fdadee30$\nFun stuff\n");
	add_blob_and_filter(
		"Hello\n$Id: Junky$\nFun stuff\n", fl,
		"Hello\n$Id: 45cd107a7102911cb2a7df08404674327fa050b9$\nFun stuff\n");
	add_blob_and_filter(
		"$Id$\nAt the start\n", fl,
		"$Id: b13415c767abc196fb95bd17070e8c1113e32160$\nAt the start\n");
	add_blob_and_filter(
		"At the end\n$Id$", fl,
		"At the end\n$Id: 1344925c6bc65b34c5a7b50f86bf688e48e9a272$");
	add_blob_and_filter(
		"$Id$", fl,
		"$Id: b3f5ebfb5843bc43ceecff6d4f26bb37c615beb1$");
	add_blob_and_filter(
		"$Id: Some sort of junk goes here$", fl,
		"$Id: ab2dd3853c7c9a4bff55aca2bea077a73c32ac06$");

	add_blob_and_filter("$Id: ", fl, "$Id: ");
	add_blob_and_filter("$Id", fl, "$Id");
	add_blob_and_filter("$I", fl, "$I");
	add_blob_and_filter("Id$", fl, "Id$");

	git_filter_list_free(fl);
}

void test_filter_ident__to_odb(void)
{
	git_filter_list *fl;
	git_filter *ident;

	cl_git_pass(git_filter_list_new(
		&fl, g_repo, GIT_FILTER_TO_ODB, 0));

	ident = git_filter_lookup(GIT_FILTER_IDENT);
	cl_assert(ident != NULL);

	cl_git_pass(git_filter_list_push(fl, ident, NULL));

	add_blob_and_filter(
		"Hello\n$Id$\nFun stuff\n",
		fl, "Hello\n$Id$\nFun stuff\n");
	add_blob_and_filter(
		"Hello\n$Id: b69e2387aafcaf73c4de5b9ab59abe27fdadee30$\nFun stuff\n",
		fl, "Hello\n$Id$\nFun stuff\n");
	add_blob_and_filter(
		"Hello\n$Id: Any junk you may have left here$\nFun stuff\n",
		fl, "Hello\n$Id$\nFun stuff\n");
	add_blob_and_filter(
		"Hello\n$Id:$\nFun stuff\n",
		fl, "Hello\n$Id$\nFun stuff\n");
	add_blob_and_filter(
		"Hello\n$Id:x$\nFun stuff\n",
		fl, "Hello\n$Id$\nFun stuff\n");

	add_blob_and_filter(
		"$Id$\nAt the start\n", fl, "$Id$\nAt the start\n");
	add_blob_and_filter(
		"$Id: lots of random text that should be removed from here$\nAt the start\n", fl, "$Id$\nAt the start\n");
	add_blob_and_filter(
		"$Id: lots of random text that should not be removed without a terminator\nAt the start\n", fl, "$Id: lots of random text that should not be removed without a terminator\nAt the start\n");

	add_blob_and_filter(
		"At the end\n$Id$", fl, "At the end\n$Id$");
	add_blob_and_filter(
		"At the end\n$Id:$", fl, "At the end\n$Id$");
	add_blob_and_filter(
		"At the end\n$Id:asdfasdf$", fl, "At the end\n$Id$");
	add_blob_and_filter(
		"At the end\n$Id", fl, "At the end\n$Id");
	add_blob_and_filter(
		"At the end\n$IddI", fl, "At the end\n$IddI");

	add_blob_and_filter("$Id$", fl, "$Id$");
	add_blob_and_filter("$Id: any$", fl, "$Id$");
	add_blob_and_filter("$Id: any long stuff goes here you see$", fl, "$Id$");
	add_blob_and_filter("$Id: ", fl, "$Id: ");
	add_blob_and_filter("$Id", fl, "$Id");
	add_blob_and_filter("$I", fl, "$I");
	add_blob_and_filter("Id$", fl, "Id$");

	git_filter_list_free(fl);
}
