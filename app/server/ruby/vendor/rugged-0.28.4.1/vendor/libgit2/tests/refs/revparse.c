#include "clar_libgit2.h"

#include "git2/revparse.h"
#include "buffer.h"
#include "refs.h"
#include "path.h"

static git_repository *g_repo;
static git_object *g_obj;

/* Helpers */
static void test_object_and_ref_inrepo(
	const char *spec,
	const char *expected_oid,
	const char *expected_refname,
	git_repository *repo,
	bool assert_reference_retrieval)
{
	char objstr[64] = {0};
	git_object *obj = NULL;
	git_reference *ref = NULL;
	int error;

	error = git_revparse_ext(&obj, &ref, repo, spec);

	if (expected_oid != NULL) {
		cl_git_pass(error);
		git_oid_fmt(objstr, git_object_id(obj));
		cl_assert_equal_s(objstr, expected_oid);
	} else
		cl_git_fail(error);

	if (assert_reference_retrieval) {
		if (expected_refname == NULL)
			cl_assert(NULL == ref);
		else
			cl_assert_equal_s(expected_refname, git_reference_name(ref));
	}

	git_object_free(obj);
	git_reference_free(ref);
}

static void test_object_inrepo(const char *spec, const char *expected_oid, git_repository *repo)
{
	test_object_and_ref_inrepo(spec, expected_oid, NULL, repo, false);
}

static void test_id_inrepo(
	const char *spec,
	const char *expected_left,
	const char *expected_right,
	git_revparse_mode_t expected_flags,
	git_repository *repo)
{
	git_revspec revspec;
	int error = git_revparse(&revspec, repo, spec);

	if (expected_left) {
		char str[64] = {0};
		cl_assert_equal_i(0, error);
		git_oid_fmt(str, git_object_id(revspec.from));
		cl_assert_equal_s(str, expected_left);
		git_object_free(revspec.from);
	} else {
		cl_assert_equal_i(GIT_ENOTFOUND, error);
	}

	if (expected_right) {
		char str[64] = {0};
		git_oid_fmt(str, git_object_id(revspec.to));
		cl_assert_equal_s(str, expected_right);
		git_object_free(revspec.to);
	}

	if (expected_flags)
		cl_assert_equal_i(expected_flags, revspec.flags);
}

static void test_object(const char *spec, const char *expected_oid)
{
	test_object_inrepo(spec, expected_oid, g_repo);
}

static void test_object_and_ref(const char *spec, const char *expected_oid, const char *expected_refname)
{
	test_object_and_ref_inrepo(spec, expected_oid, expected_refname, g_repo, true);
}

static void test_rangelike(const char *rangelike,
						   const char *expected_left,
						   const char *expected_right,
						   git_revparse_mode_t expected_revparseflags)
{
	char objstr[64] = {0};
	git_revspec revspec;
	int error;

	error = git_revparse(&revspec, g_repo, rangelike);

	if (expected_left != NULL) {
		cl_assert_equal_i(0, error);
		cl_assert_equal_i(revspec.flags, expected_revparseflags);
		git_oid_fmt(objstr, git_object_id(revspec.from));
		cl_assert_equal_s(objstr, expected_left);
		git_oid_fmt(objstr, git_object_id(revspec.to));
		cl_assert_equal_s(objstr, expected_right);
	} else
		cl_assert(error != 0);

	git_object_free(revspec.from);
	git_object_free(revspec.to);
}


static void test_id(
	const char *spec,
	const char *expected_left,
	const char *expected_right,
	git_revparse_mode_t expected_flags)
{
	test_id_inrepo(spec, expected_left, expected_right, expected_flags, g_repo);
}

static void test_invalid_revspec(const char* invalid_spec)
{
	git_revspec revspec;

	cl_assert_equal_i(
		GIT_EINVALIDSPEC, git_revparse(&revspec, g_repo, invalid_spec));
}

void test_refs_revparse__initialize(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));
}

void test_refs_revparse__cleanup(void)
{
	git_repository_free(g_repo);
}

void test_refs_revparse__nonexistant_object(void)
{
	test_object("this-does-not-exist", NULL);
	test_object("this-does-not-exist^1", NULL);
	test_object("this-does-not-exist~2", NULL);
}

static void assert_invalid_single_spec(const char *invalid_spec)
{
	cl_assert_equal_i(
		GIT_EINVALIDSPEC, git_revparse_single(&g_obj, g_repo, invalid_spec));
}

void test_refs_revparse__invalid_reference_name(void)
{
	assert_invalid_single_spec("this doesn't make sense");
	assert_invalid_single_spec("Inv@{id");
	assert_invalid_single_spec("");
}

void test_refs_revparse__shas(void)
{
	test_object("c47800c7266a2be04c571c04d5a6614691ea99bd", "c47800c7266a2be04c571c04d5a6614691ea99bd");
	test_object("c47800c", "c47800c7266a2be04c571c04d5a6614691ea99bd");
}

void test_refs_revparse__head(void)
{
	test_object("HEAD", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("HEAD^0", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("HEAD~0", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("master", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
}

void test_refs_revparse__full_refs(void)
{
	test_object("refs/heads/master", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("refs/heads/test", "e90810b8df3e80c413d903f631643c716887138d");
	test_object("refs/tags/test", "b25fa35b38051e4ae45d4222e795f9df2e43f1d1");
}

void test_refs_revparse__partial_refs(void)
{
	test_object("point_to_blob", "1385f264afb75a56a5bec74243be9b367ba4ca08");
	test_object("packed-test", "4a202b346bb0fb0db7eff3cffeb3c70babbd2045");
	test_object("br2", "a4a7dce85cf63874e984719f4fdd239f5145052f");
}

void test_refs_revparse__describe_output(void)
{
	test_object("blah-7-gc47800c", "c47800c7266a2be04c571c04d5a6614691ea99bd");
	test_object("not-good", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
}

void test_refs_revparse__nth_parent(void)
{
	assert_invalid_single_spec("be3563a^-1");
	assert_invalid_single_spec("^");
	assert_invalid_single_spec("be3563a^{tree}^");
	assert_invalid_single_spec("point_to_blob^{blob}^");
	assert_invalid_single_spec("this doesn't make sense^1");

	test_object("be3563a^1", "9fd738e8f7967c078dceed8190330fc8648ee56a");
	test_object("be3563a^", "9fd738e8f7967c078dceed8190330fc8648ee56a");
	test_object("be3563a^2", "c47800c7266a2be04c571c04d5a6614691ea99bd");
	test_object("be3563a^1^1", "4a202b346bb0fb0db7eff3cffeb3c70babbd2045");
	test_object("be3563a^^", "4a202b346bb0fb0db7eff3cffeb3c70babbd2045");
	test_object("be3563a^2^1", "5b5b025afb0b4c913b4c338a42934a3863bf3644");
	test_object("be3563a^0", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("be3563a^{commit}^", "9fd738e8f7967c078dceed8190330fc8648ee56a");

	test_object("be3563a^42", NULL);
}

void test_refs_revparse__not_tag(void)
{
	test_object("point_to_blob^{}", "1385f264afb75a56a5bec74243be9b367ba4ca08");
	test_object("wrapped_tag^{}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("master^{}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("master^{tree}^{}", "944c0f6e4dfa41595e6eb3ceecdb14f50fe18162");
	test_object("e90810b^{}", "e90810b8df3e80c413d903f631643c716887138d");
	test_object("tags/e90810b^{}", "e90810b8df3e80c413d903f631643c716887138d");
	test_object("e908^{}", "e90810b8df3e80c413d903f631643c716887138d");
}

void test_refs_revparse__to_type(void)
{
	assert_invalid_single_spec("wrapped_tag^{trip}");
	test_object("point_to_blob^{commit}", NULL);
	cl_assert_equal_i(
		GIT_EPEEL, git_revparse_single(&g_obj, g_repo, "wrapped_tag^{blob}"));

	test_object("wrapped_tag^{commit}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("wrapped_tag^{tree}", "944c0f6e4dfa41595e6eb3ceecdb14f50fe18162");
	test_object("point_to_blob^{blob}", "1385f264afb75a56a5bec74243be9b367ba4ca08");
	test_object("master^{commit}^{commit}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
}

void test_refs_revparse__linear_history(void)
{
	assert_invalid_single_spec("~");
	test_object("foo~bar", NULL);

	assert_invalid_single_spec("master~bar");
	assert_invalid_single_spec("master~-1");
	assert_invalid_single_spec("master~0bar");
	assert_invalid_single_spec("this doesn't make sense~2");
	assert_invalid_single_spec("be3563a^{tree}~");
	assert_invalid_single_spec("point_to_blob^{blob}~");

	test_object("master~0", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("master~1", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("master~2", "9fd738e8f7967c078dceed8190330fc8648ee56a");
	test_object("master~1~1", "9fd738e8f7967c078dceed8190330fc8648ee56a");
	test_object("master~~", "9fd738e8f7967c078dceed8190330fc8648ee56a");
}

void test_refs_revparse__chaining(void)
{
	assert_invalid_single_spec("master@{0}@{0}");
	assert_invalid_single_spec("@{u}@{-1}");
	assert_invalid_single_spec("@{-1}@{-1}");
	assert_invalid_single_spec("@{-3}@{0}");

	test_object("master@{0}~1^1", "9fd738e8f7967c078dceed8190330fc8648ee56a");
	test_object("@{u}@{0}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("@{-1}@{0}", "a4a7dce85cf63874e984719f4fdd239f5145052f");
	test_object("@{-4}@{1}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("master~1^1", "9fd738e8f7967c078dceed8190330fc8648ee56a");
	test_object("master~1^2", "c47800c7266a2be04c571c04d5a6614691ea99bd");
	test_object("master^1^2~1", "5b5b025afb0b4c913b4c338a42934a3863bf3644");
	test_object("master^^2^", "5b5b025afb0b4c913b4c338a42934a3863bf3644");
	test_object("master^1^1^1^1^1", "8496071c1b46c854b31185ea97743be6a8774479");
	test_object("master^^1^2^1", NULL);
}

void test_refs_revparse__upstream(void)
{
	assert_invalid_single_spec("e90810b@{u}");
	assert_invalid_single_spec("refs/tags/e90810b@{u}");
	test_object("refs/heads/e90810b@{u}", NULL);

	test_object("master@{upstream}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("@{u}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("master@{u}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("heads/master@{u}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("refs/heads/master@{u}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
}

void test_refs_revparse__ordinal(void)
{
	assert_invalid_single_spec("master@{-2}");
	
	/* TODO: make the test below actually fail
	 * cl_git_fail(git_revparse_single(&g_obj, g_repo, "master@{1a}"));
	 */

	test_object("nope@{0}", NULL);
	test_object("master@{31415}", NULL);
	test_object("@{1000}", NULL);
	test_object("@{2}", NULL);

	test_object("@{0}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("@{1}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");

	test_object("master@{0}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("master@{1}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("heads/master@{1}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("refs/heads/master@{1}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
}

void test_refs_revparse__previous_head(void)
{
	assert_invalid_single_spec("@{-xyz}");
	assert_invalid_single_spec("@{-0}");
	assert_invalid_single_spec("@{-1b}");

	test_object("@{-42}", NULL);

	test_object("@{-2}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("@{-1}", "a4a7dce85cf63874e984719f4fdd239f5145052f");
}

static void create_fake_stash_reference_and_reflog(git_repository *repo)
{
	git_reference *master, *new_master;
	git_buf log_path = GIT_BUF_INIT;

	git_buf_joinpath(&log_path, git_repository_path(repo), "logs/refs/fakestash");

	cl_assert_equal_i(false, git_path_isfile(git_buf_cstr(&log_path)));

	cl_git_pass(git_reference_lookup(&master, repo, "refs/heads/master"));
	cl_git_pass(git_reference_rename(&new_master, master, "refs/fakestash", 0, NULL));
	git_reference_free(master);

	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&log_path)));

	git_buf_dispose(&log_path);
	git_reference_free(new_master);
}

void test_refs_revparse__reflog_of_a_ref_under_refs(void)
{
	git_repository *repo = cl_git_sandbox_init("testrepo.git");

	test_object_inrepo("refs/fakestash", NULL, repo);

	create_fake_stash_reference_and_reflog(repo);

	/*
	 * $ git reflog -1 refs/fakestash
	 * a65fedf refs/fakestash@{0}: commit: checking in
	 *
	 * $ git reflog -1 refs/fakestash@{0}
	 * a65fedf refs/fakestash@{0}: commit: checking in
	 *
	 * $ git reflog -1 fakestash
	 * a65fedf fakestash@{0}: commit: checking in
	 *
	 * $ git reflog -1 fakestash@{0}
	 * a65fedf fakestash@{0}: commit: checking in
	 */
	test_object_inrepo("refs/fakestash", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", repo);
	test_object_inrepo("refs/fakestash@{0}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", repo);
	test_object_inrepo("fakestash", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", repo);
	test_object_inrepo("fakestash@{0}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", repo);

	cl_git_sandbox_cleanup();
}

void test_refs_revparse__revwalk(void)
{
	test_object("master^{/not found in any commit}", NULL);
	test_object("master^{/merge}", NULL);
	assert_invalid_single_spec("master^{/((}");

	test_object("master^{/anoth}", "5b5b025afb0b4c913b4c338a42934a3863bf3644");
	test_object("master^{/Merge}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("br2^{/Merge}", "a4a7dce85cf63874e984719f4fdd239f5145052f");
	test_object("master^{/fo.rth}", "9fd738e8f7967c078dceed8190330fc8648ee56a");
}

void test_refs_revparse__date(void)
{
	/*
	 * $ git reflog HEAD --date=iso
	 * a65fedf HEAD@{2012-04-30 08:23:41 -0900}: checkout: moving from br2 to master
	 * a4a7dce HEAD@{2012-04-30 08:23:37 -0900}: commit: checking in
	 * c47800c HEAD@{2012-04-30 08:23:28 -0900}: checkout: moving from master to br2
	 * a65fedf HEAD@{2012-04-30 08:23:23 -0900}: commit:
	 * be3563a HEAD@{2012-04-30 10:22:43 -0700}: clone: from /Users/ben/src/libgit2/tes
	 *
	 * $ git reflog HEAD --date=raw
	 * a65fedf HEAD@{1335806621 -0900}: checkout: moving from br2 to master
	 * a4a7dce HEAD@{1335806617 -0900}: commit: checking in
	 * c47800c HEAD@{1335806608 -0900}: checkout: moving from master to br2
	 * a65fedf HEAD@{1335806603 -0900}: commit:
	 * be3563a HEAD@{1335806563 -0700}: clone: from /Users/ben/src/libgit2/tests/resour
	 */
	test_object("HEAD@{10 years ago}", NULL);

	test_object("HEAD@{1 second}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("HEAD@{1 second ago}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("HEAD@{2 days ago}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	/*
	 * $ git reflog master --date=iso
	 * a65fedf master@{2012-04-30 09:23:23 -0800}: commit: checking in
	 * be3563a master@{2012-04-30 09:22:43 -0800}: clone: from /Users/ben/src...
	 *
	 * $ git reflog master --date=raw
	 * a65fedf master@{1335806603 -0800}: commit: checking in
	 * be3563a master@{1335806563 -0800}: clone: from /Users/ben/src/libgit2/tests/reso
	 */


	/*
	 * $ git reflog -1 "master@{2012-04-30 17:22:42 +0000}"
	 * warning: Log for 'master' only goes back to Mon, 30 Apr 2012 09:22:43 -0800.
	 */
	test_object("master@{2012-04-30 17:22:42 +0000}", NULL);
	test_object("master@{2012-04-30 09:22:42 -0800}", NULL);

	/*
	 * $ git reflog -1 "master@{2012-04-30 17:22:43 +0000}"
	 * be3563a master@{Mon Apr 30 09:22:43 2012 -0800}: clone: from /Users/ben/src/libg
	 */
	test_object("master@{2012-04-30 17:22:43 +0000}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	test_object("master@{2012-04-30 09:22:43 -0800}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");

	/*
	 * $ git reflog -1 "master@{2012-4-30 09:23:27 -0800}"
	 * a65fedf master@{Mon Apr 30 09:23:23 2012 -0800}: commit: checking in
	 */
	test_object("master@{2012-4-30 09:23:27 -0800}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	/*
	 * $ git reflog -1 master@{2012-05-03}
	 * a65fedf master@{Mon Apr 30 09:23:23 2012 -0800}: commit: checking in
	 */
	test_object("master@{2012-05-03}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	/*
	 * $ git reflog -1 "master@{1335806603}"
	 * a65fedf
	 *
	 * $ git reflog -1 "master@{1335806602}"
	 * be3563a
	 */
	test_object("master@{1335806603}", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	test_object("master@{1335806602}", "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
}

void test_refs_revparse__colon(void)
{
	assert_invalid_single_spec(":/");
	assert_invalid_single_spec("point_to_blob:readme.txt");
	cl_git_fail(git_revparse_single(&g_obj, g_repo, ":2:README")); /* Not implemented  */

	test_object(":/not found in any commit", NULL);
	test_object("subtrees:ab/42.txt", NULL);
	test_object("subtrees:ab/4.txt/nope", NULL);
	test_object("subtrees:nope", NULL);
	test_object("test/master^1:branch_file.txt", NULL);

	/* From tags */
	test_object("test:readme.txt", "0266163a49e280c4f5ed1e08facd36a2bd716bcf");
	test_object("tags/test:readme.txt", "0266163a49e280c4f5ed1e08facd36a2bd716bcf");
	test_object("e90810b:readme.txt", "0266163a49e280c4f5ed1e08facd36a2bd716bcf");
	test_object("tags/e90810b:readme.txt", "0266163a49e280c4f5ed1e08facd36a2bd716bcf");

	/* From commits */
	test_object("a65f:branch_file.txt", "3697d64be941a53d4ae8f6a271e4e3fa56b022cc");

	/* From trees */
	test_object("a65f^{tree}:branch_file.txt", "3697d64be941a53d4ae8f6a271e4e3fa56b022cc");
	test_object("944c:branch_file.txt", "3697d64be941a53d4ae8f6a271e4e3fa56b022cc");

	/* Retrieving trees */
	test_object("master:", "944c0f6e4dfa41595e6eb3ceecdb14f50fe18162");
	test_object("subtrees:", "ae90f12eea699729ed24555e40b9fd669da12a12");
	test_object("subtrees:ab", "f1425cef211cc08caa31e7b545ffb232acb098c3");
	test_object("subtrees:ab/", "f1425cef211cc08caa31e7b545ffb232acb098c3");

	/* Retrieving blobs */
	test_object("subtrees:ab/4.txt", "d6c93164c249c8000205dd4ec5cbca1b516d487f");
	test_object("subtrees:ab/de/fgh/1.txt", "1f67fc4386b2d171e0d21be1c447e12660561f9b");
	test_object("master:README", "a8233120f6ad708f843d861ce2b7228ec4e3dec6");
	test_object("master:new.txt", "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd");
	test_object(":/Merge", "a4a7dce85cf63874e984719f4fdd239f5145052f");
	test_object(":/one", "c47800c7266a2be04c571c04d5a6614691ea99bd");
	test_object(":/packed commit t", "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9");
	test_object("test/master^2:branch_file.txt", "45b983be36b73c0788dc9cbcb76cbb80fc7bb057");
	test_object("test/master@{1}:branch_file.txt", "3697d64be941a53d4ae8f6a271e4e3fa56b022cc");
}

void test_refs_revparse__disambiguation(void)
{
	/*
	 * $ git show e90810b
	 * tag e90810b
	 * Tagger: Vicent Marti <tanoku@gmail.com>
	 * Date:   Thu Aug 12 03:59:17 2010 +0200
	 *
	 * This is a very simple tag.
	 *
	 * commit e90810b8df3e80c413d903f631643c716887138d
	 * Author: Vicent Marti <tanoku@gmail.com>
	 * Date:   Thu Aug 5 18:42:20 2010 +0200
	 *
	 *     Test commit 2
	 *
	 * diff --git a/readme.txt b/readme.txt
	 * index 6336846..0266163 100644
	 * --- a/readme.txt
	 * +++ b/readme.txt
	 * @@ -1 +1,2 @@
	 *  Testing a readme.txt
	 * +Now we add a single line here
	 *
	 * $ git show-ref e90810b
	 * 7b4384978d2493e851f9cca7858815fac9b10980 refs/tags/e90810b
	 *
	 */
	test_object("e90810b", "7b4384978d2493e851f9cca7858815fac9b10980");

	/*
	 * $ git show e90810
	 * commit e90810b8df3e80c413d903f631643c716887138d
	 * Author: Vicent Marti <tanoku@gmail.com>
	 * Date:   Thu Aug 5 18:42:20 2010 +0200
	 *
	 *     Test commit 2
	 *
	 * diff --git a/readme.txt b/readme.txt
	 * index 6336846..0266163 100644
	 * --- a/readme.txt
	 * +++ b/readme.txt
	 * @@ -1 +1,2 @@
	 *  Testing a readme.txt
	 * +Now we add a single line here
	 */
	test_object("e90810", "e90810b8df3e80c413d903f631643c716887138d");
}

void test_refs_revparse__a_too_short_objectid_returns_EAMBIGUOUS(void)
{
	cl_assert_equal_i(
		GIT_EAMBIGUOUS, git_revparse_single(&g_obj, g_repo, "e90"));
}

/*
 * $ echo "aabqhq" | git hash-object -t blob --stdin
 * dea509d0b3cb8ee0650f6ca210bc83f4678851ba
 * 
 * $ echo "aaazvc" | git hash-object -t blob --stdin
 * dea509d097ce692e167dfc6a48a7a280cc5e877e
 */
void test_refs_revparse__a_not_precise_enough_objectid_returns_EAMBIGUOUS(void)
{
	git_repository *repo;
	git_index *index;
	git_object *obj;

	repo = cl_git_sandbox_init("testrepo");

	cl_git_mkfile("testrepo/one.txt", "aabqhq\n");
	cl_git_mkfile("testrepo/two.txt", "aaazvc\n");
	
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, "one.txt"));
	cl_git_pass(git_index_add_bypath(index, "two.txt"));
	
	cl_git_fail_with(git_revparse_single(&obj, repo, "dea509d0"), GIT_EAMBIGUOUS);

	cl_git_pass(git_revparse_single(&obj, repo, "dea509d09"));

	git_object_free(obj);
	git_index_free(index);
	cl_git_sandbox_cleanup();
}

void test_refs_revparse__issue_994(void)
{
	git_repository *repo;
	git_reference *head, *with_at;
	git_object *target;
	
	repo = cl_git_sandbox_init("testrepo.git");

	cl_assert_equal_i(GIT_ENOTFOUND,
		git_revparse_single(&target, repo, "origin/bim_with_3d@11296"));

	cl_assert_equal_i(GIT_ENOTFOUND,
		git_revparse_single(&target, repo, "refs/remotes/origin/bim_with_3d@11296"));


	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_create(
		&with_at,
		repo,
		"refs/remotes/origin/bim_with_3d@11296",
		git_reference_target(head),
		0,
		NULL));

	cl_git_pass(git_revparse_single(&target, repo, "origin/bim_with_3d@11296"));
	git_object_free(target);

	cl_git_pass(git_revparse_single(&target, repo, "refs/remotes/origin/bim_with_3d@11296"));
	git_object_free(target);

	git_reference_free(with_at);
	git_reference_free(head);
	cl_git_sandbox_cleanup();
}

/**
 * $ git rev-parse blah-7-gc47800c
 * c47800c7266a2be04c571c04d5a6614691ea99bd
 *
 * $ git rev-parse HEAD~3
 * 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 *
 * $ git branch blah-7-gc47800c HEAD~3
 *
 * $ git rev-parse blah-7-gc47800c
 * 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 */
void test_refs_revparse__try_to_retrieve_branch_before_described_tag(void)
{
	git_repository *repo;
	git_reference *branch;
	git_object *target;
	char sha[GIT_OID_HEXSZ + 1];

	repo = cl_git_sandbox_init("testrepo.git");

	test_object_inrepo("blah-7-gc47800c", "c47800c7266a2be04c571c04d5a6614691ea99bd", repo);

	cl_git_pass(git_revparse_single(&target, repo, "HEAD~3"));
	cl_git_pass(git_branch_create(&branch, repo, "blah-7-gc47800c", (git_commit *)target, 0));

	git_oid_tostr(sha, GIT_OID_HEXSZ + 1, git_object_id(target));

	test_object_inrepo("blah-7-gc47800c", sha, repo);

	git_reference_free(branch);
	git_object_free(target);
	cl_git_sandbox_cleanup();
}

/**
 * $ git rev-parse a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 * a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 *
 * $ git rev-parse HEAD~3
 * 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 *
 * $ git branch a65fedf39aefe402d3bb6e24df4d4f5fe4547750 HEAD~3
 *
 * $ git rev-parse a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 * a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 *
 * $ git rev-parse heads/a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 * 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 */
void test_refs_revparse__try_to_retrieve_sha_before_branch(void)
{
	git_repository *repo;
	git_reference *branch;
	git_object *target;
	char sha[GIT_OID_HEXSZ + 1];

	repo = cl_git_sandbox_init("testrepo.git");

	test_object_inrepo("a65fedf39aefe402d3bb6e24df4d4f5fe4547750", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", repo);

	cl_git_pass(git_revparse_single(&target, repo, "HEAD~3"));
	cl_git_pass(git_branch_create(&branch, repo, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", (git_commit *)target, 0));

	git_oid_tostr(sha, GIT_OID_HEXSZ + 1, git_object_id(target));

	test_object_inrepo("a65fedf39aefe402d3bb6e24df4d4f5fe4547750", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", repo);
	test_object_inrepo("heads/a65fedf39aefe402d3bb6e24df4d4f5fe4547750", sha, repo);

	git_reference_free(branch);
	git_object_free(target);
	cl_git_sandbox_cleanup();
}

/**
 * $ git rev-parse c47800
 * c47800c7266a2be04c571c04d5a6614691ea99bd
 *
 * $ git rev-parse HEAD~3
 * 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 *
 * $ git branch c47800 HEAD~3
 *
 * $ git rev-parse c47800
 * 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 */
void test_refs_revparse__try_to_retrieve_branch_before_abbrev_sha(void)
{
	git_repository *repo;
	git_reference *branch;
	git_object *target;
	char sha[GIT_OID_HEXSZ + 1];

	repo = cl_git_sandbox_init("testrepo.git");

	test_object_inrepo("c47800", "c47800c7266a2be04c571c04d5a6614691ea99bd", repo);

	cl_git_pass(git_revparse_single(&target, repo, "HEAD~3"));
	cl_git_pass(git_branch_create(&branch, repo, "c47800", (git_commit *)target, 0));

	git_oid_tostr(sha, GIT_OID_HEXSZ + 1, git_object_id(target));

	test_object_inrepo("c47800", sha, repo);

	git_reference_free(branch);
	git_object_free(target);
	cl_git_sandbox_cleanup();
}


void test_refs_revparse__range(void)
{
	assert_invalid_single_spec("be3563a^1..be3563a");

	test_rangelike("be3563a^1..be3563a",
	               "9fd738e8f7967c078dceed8190330fc8648ee56a",
	               "be3563ae3f795b2b4353bcce3a527ad0a4f7f644",
	               GIT_REVPARSE_RANGE);

	test_rangelike("be3563a^1...be3563a",
	               "9fd738e8f7967c078dceed8190330fc8648ee56a",
	               "be3563ae3f795b2b4353bcce3a527ad0a4f7f644",
	               GIT_REVPARSE_RANGE | GIT_REVPARSE_MERGE_BASE);

	test_rangelike("be3563a^1.be3563a", NULL, NULL, 0);
}

void test_refs_revparse__parses_range_operator(void)
{
	test_id("HEAD", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", NULL, GIT_REVPARSE_SINGLE);
	test_id("HEAD~3..HEAD",
		"4a202b346bb0fb0db7eff3cffeb3c70babbd2045",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		GIT_REVPARSE_RANGE);

	test_id("HEAD~3...HEAD",
		"4a202b346bb0fb0db7eff3cffeb3c70babbd2045",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		GIT_REVPARSE_RANGE | GIT_REVPARSE_MERGE_BASE);

	test_id("HEAD~3..",
		"4a202b346bb0fb0db7eff3cffeb3c70babbd2045",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		GIT_REVPARSE_RANGE);

	test_id("HEAD~3...",
		"4a202b346bb0fb0db7eff3cffeb3c70babbd2045",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		GIT_REVPARSE_RANGE | GIT_REVPARSE_MERGE_BASE);

	test_id("..HEAD~3",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"4a202b346bb0fb0db7eff3cffeb3c70babbd2045",
		GIT_REVPARSE_RANGE);

	test_id("...HEAD~3",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"4a202b346bb0fb0db7eff3cffeb3c70babbd2045",
		GIT_REVPARSE_RANGE | GIT_REVPARSE_MERGE_BASE);

	test_id("...",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		GIT_REVPARSE_RANGE | GIT_REVPARSE_MERGE_BASE);

	test_invalid_revspec("..");
}

void test_refs_revparse__ext_retrieves_both_the_reference_and_its_target(void)
{
	test_object_and_ref(
		"master@{upstream}",
		"be3563ae3f795b2b4353bcce3a527ad0a4f7f644",
		"refs/remotes/test/master");

	test_object_and_ref(
		"@{-1}",
		"a4a7dce85cf63874e984719f4fdd239f5145052f",
		"refs/heads/br2");
}

void test_refs_revparse__ext_can_expand_short_reference_names(void)
{
	test_object_and_ref(
		"master",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"refs/heads/master");

	test_object_and_ref(
		"HEAD",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"refs/heads/master");

    test_object_and_ref(
		"tags/test",
		"b25fa35b38051e4ae45d4222e795f9df2e43f1d1",
        "refs/tags/test");
}

void test_refs_revparse__ext_returns_NULL_reference_when_expression_points_at_a_revision(void)
{
    test_object_and_ref(
        "HEAD~3",
        "4a202b346bb0fb0db7eff3cffeb3c70babbd2045",
        NULL);

    test_object_and_ref(
        "HEAD~0",
        "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
        NULL);

    test_object_and_ref(
        "HEAD^0",
        "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
        NULL);

    test_object_and_ref(
		"@{-1}@{0}",
		"a4a7dce85cf63874e984719f4fdd239f5145052f",
		NULL);
}

void test_refs_revparse__ext_returns_NULL_reference_when_expression_points_at_a_tree_content(void)
{
    test_object_and_ref(
		"tags/test:readme.txt",
		"0266163a49e280c4f5ed1e08facd36a2bd716bcf",
        NULL);
}

void test_refs_revparse__uneven_sizes(void)
{
	test_object("a65fedf39aefe402d3bb6e24df4d4f5fe454775",
				"a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	test_object("a65fedf39aefe402d3bb6e24df4d4f5fe45477",
				"a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	test_object("a65fedf39aefe402d3bb6e24df4d4f5fe4547",
				"a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	test_object("a65fedf39aefe402d3bb6e24df4d",
				"a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
}
