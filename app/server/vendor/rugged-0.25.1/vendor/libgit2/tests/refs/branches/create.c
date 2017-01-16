#include "clar_libgit2.h"
#include "refs.h"
#include "path.h"

static git_repository *repo;
static git_commit *target;
static git_reference *branch;

void test_refs_branches_create__initialize(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
	branch = NULL;
	target = NULL;
}

void test_refs_branches_create__cleanup(void)
{
	git_reference_free(branch);
	branch = NULL;

	git_commit_free(target);
	target = NULL;

	cl_git_sandbox_cleanup();
	repo = NULL;
}

static void retrieve_target_from_oid(git_commit **out, git_repository *repo, const char *sha)
{
	git_object *obj;

	cl_git_pass(git_revparse_single(&obj, repo, sha));
	cl_git_pass(git_commit_lookup(out, repo, git_object_id(obj)));
	git_object_free(obj);
}

static void retrieve_known_commit(git_commit **commit, git_repository *repo)
{
	retrieve_target_from_oid(commit, repo, "e90810b8df3");
}

#define NEW_BRANCH_NAME "new-branch-on-the-block"

void test_refs_branches_create__can_create_a_local_branch(void)
{
	retrieve_known_commit(&target, repo);

	cl_git_pass(git_branch_create(&branch, repo, NEW_BRANCH_NAME, target, 0));
	cl_git_pass(git_oid_cmp(git_reference_target(branch), git_commit_id(target)));
}

void test_refs_branches_create__can_not_create_a_branch_if_its_name_collide_with_an_existing_one(void)
{
	retrieve_known_commit(&target, repo);

	cl_assert_equal_i(GIT_EEXISTS, git_branch_create(&branch, repo, "br2", target, 0));
}

void test_refs_branches_create__can_force_create_over_an_existing_branch(void)
{
	retrieve_known_commit(&target, repo);

	cl_git_pass(git_branch_create(&branch, repo, "br2", target, 1));
	cl_git_pass(git_oid_cmp(git_reference_target(branch), git_commit_id(target)));
	cl_assert_equal_s("refs/heads/br2", git_reference_name(branch));
}

void test_refs_branches_create__cannot_force_create_over_current_branch_in_nonbare_repo(void)
{
	const git_oid *oid;
	git_reference *branch2;

	/* Default repo for these tests is a bare repo, but this test requires a non-bare one */
	cl_git_sandbox_cleanup();
	repo = cl_git_sandbox_init("testrepo");
	retrieve_known_commit(&target, repo);

	cl_git_pass(git_branch_lookup(&branch2, repo, "master", GIT_BRANCH_LOCAL));
	cl_assert_equal_s("refs/heads/master", git_reference_name(branch2));
	cl_assert_equal_i(true, git_branch_is_head(branch2));
	oid = git_reference_target(branch2);

	cl_git_fail_with(-1, git_branch_create(&branch, repo, "master", target, 1));
	branch = NULL;
	cl_git_pass(git_branch_lookup(&branch, repo, "master", GIT_BRANCH_LOCAL));
	cl_assert_equal_s("refs/heads/master", git_reference_name(branch));
	cl_git_pass(git_oid_cmp(git_reference_target(branch), oid));
	git_reference_free(branch2);
}

void test_refs_branches_create__can_force_create_over_current_branch_in_bare_repo(void)
{
	const git_oid *oid;
	git_reference *branch2;
	retrieve_known_commit(&target, repo);

	cl_git_pass(git_branch_lookup(&branch2, repo, "master", GIT_BRANCH_LOCAL));
	cl_assert_equal_s("refs/heads/master", git_reference_name(branch2));
	cl_assert_equal_i(true, git_branch_is_head(branch2));
	oid = git_commit_id(target);

	cl_git_pass(git_branch_create(&branch, repo, "master", target, 1));
	git_reference_free(branch);
	branch = NULL;
	cl_git_pass(git_branch_lookup(&branch, repo, "master", GIT_BRANCH_LOCAL));
	cl_assert_equal_s("refs/heads/master", git_reference_name(branch));
	cl_git_pass(git_oid_cmp(git_reference_target(branch), oid));
	git_reference_free(branch2);
}

void test_refs_branches_create__creating_a_branch_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	retrieve_known_commit(&target, repo);

	cl_assert_equal_i(GIT_EINVALIDSPEC,
		git_branch_create(&branch, repo, "inv@{id", target, 0));
}

void test_refs_branches_create__default_reflog_message(void)
{
	git_reflog *log;
	git_buf buf = GIT_BUF_INIT;
	const git_reflog_entry *entry;
	git_annotated_commit *annotated;
	git_signature *sig;
	git_config *cfg;

	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_string(cfg, "user.name", "Foo Bar"));
	cl_git_pass(git_config_set_string(cfg, "user.email", "foo@example.com"));
	git_config_free(cfg);

	cl_git_pass(git_signature_default(&sig, repo));

	retrieve_known_commit(&target, repo);
	cl_git_pass(git_branch_create(&branch, repo, NEW_BRANCH_NAME, target, false));
	cl_git_pass(git_reflog_read(&log, repo, "refs/heads/" NEW_BRANCH_NAME));

	entry = git_reflog_entry_byindex(log, 0);
	cl_git_pass(git_buf_printf(&buf, "branch: Created from %s", git_oid_tostr_s(git_commit_id(target))));
	cl_assert_equal_s(git_buf_cstr(&buf), git_reflog_entry_message(entry));
	cl_assert_equal_s(sig->email, git_reflog_entry_committer(entry)->email);

	cl_git_pass(git_reference_remove(repo, "refs/heads/" NEW_BRANCH_NAME));
	git_reference_free(branch);
	git_reflog_free(log);
	git_buf_clear(&buf);

	cl_git_pass(git_annotated_commit_from_revspec(&annotated, repo, "e90810b8df3"));
	cl_git_pass(git_branch_create_from_annotated(&branch, repo, NEW_BRANCH_NAME, annotated, true));
	cl_git_pass(git_reflog_read(&log, repo, "refs/heads/" NEW_BRANCH_NAME));

	entry = git_reflog_entry_byindex(log, 0);
	cl_git_pass(git_buf_printf(&buf, "branch: Created from e90810b8df3"));
	cl_assert_equal_s(git_buf_cstr(&buf), git_reflog_entry_message(entry));
	cl_assert_equal_s(sig->email, git_reflog_entry_committer(entry)->email);

	git_annotated_commit_free(annotated);
	git_buf_free(&buf);
	git_reflog_free(log);
	git_signature_free(sig);
}

static void assert_branch_matches_name(
	const char *expected, const char *lookup_as)
{
	git_reference *ref;
	git_buf b = GIT_BUF_INIT;

	cl_git_pass(git_branch_lookup(&ref, repo, lookup_as, GIT_BRANCH_LOCAL));

	cl_git_pass(git_buf_sets(&b, "refs/heads/"));
	cl_git_pass(git_buf_puts(&b, expected));
	cl_assert_equal_s(b.ptr, git_reference_name(ref));

	cl_git_pass(
		git_oid_cmp(git_reference_target(ref), git_commit_id(target)));

	git_reference_free(ref);
	git_buf_free(&b);
}

void test_refs_branches_create__can_create_branch_with_unicode(void)
{
	const char *nfc = "\xC3\x85\x73\x74\x72\xC3\xB6\x6D";
	const char *nfd = "\x41\xCC\x8A\x73\x74\x72\x6F\xCC\x88\x6D";
	const char *emoji = "\xF0\x9F\x8D\xB7";
	const char *names[] = { nfc, nfd, emoji };
	const char *alt[] = { nfd, nfc, NULL };
	const char *expected[] = { nfc, nfd, emoji };
	unsigned int i;
	bool fs_decompose_unicode =
		git_path_does_fs_decompose_unicode(git_repository_path(repo));

	retrieve_known_commit(&target, repo);

	if (cl_repo_get_bool(repo, "core.precomposeunicode"))
		expected[1] = nfc;
	/* test decomp. because not all Mac filesystems decompose unicode */
	else if (fs_decompose_unicode)
		expected[0] = nfd;

	for (i = 0; i < ARRAY_SIZE(names); ++i) {
		const char *name;
		cl_git_pass(git_branch_create(
			&branch, repo, names[i], target, 0));
		cl_git_pass(git_oid_cmp(
			git_reference_target(branch), git_commit_id(target)));

		cl_git_pass(git_branch_name(&name, branch));
		cl_assert_equal_s(expected[i], name);
		assert_branch_matches_name(expected[i], names[i]);
		if (fs_decompose_unicode && alt[i])
			assert_branch_matches_name(expected[i], alt[i]);

		cl_git_pass(git_branch_delete(branch));
		git_reference_free(branch);
		branch = NULL;
	}
}

/**
 * Verify that we can create a branch with a name that matches the
 * namespace of a previously delete branch.
 *
 * git branch level_one/level_two
 * git branch -D level_one/level_two
 * git branch level_one
 *
 * We expect the delete to have deleted the files:
 *     ".git/refs/heads/level_one/level_two"
 *     ".git/logs/refs/heads/level_one/level_two"
 * It may or may not have deleted the (now empty)
 * containing directories.  To match git.git behavior,
 * the second create needs to implicilty delete the
 * directories and create the new files.
 *     "refs/heads/level_one"
 *     "logs/refs/heads/level_one"
 *
 * We should not fail to create the branch or its
 * reflog because of an obsolete namespace container
 * directory.
 */
void test_refs_branches_create__name_vs_namespace(void)
{
	const char * name;
	struct item {
		const char *first;
		const char *second;
	};
	static const struct item item[] = {
		{ "level_one/level_two", "level_one" },
		{ "a/b/c/d/e",           "a/b/c/d" },
		{ "ss/tt/uu/vv/ww",      "ss" },
		/* And one test case that is deeper. */
		{ "xx1/xx2/xx3/xx4",     "xx1/xx2/xx3/xx4/xx5/xx6" },
		{ NULL, NULL },
	};
	const struct item *p;

	retrieve_known_commit(&target, repo);

	for (p=item; p->first; p++) {
		cl_git_pass(git_branch_create(&branch, repo, p->first, target, 0));
		cl_git_pass(git_oid_cmp(git_reference_target(branch), git_commit_id(target)));
		cl_git_pass(git_branch_name(&name, branch));
		cl_assert_equal_s(name, p->first);

		cl_git_pass(git_branch_delete(branch));
		git_reference_free(branch);
		branch = NULL;

		cl_git_pass(git_branch_create(&branch, repo, p->second, target, 0));
		git_reference_free(branch);
		branch = NULL;
	}
}

/**
 * We still need to fail if part of the namespace is
 * still in use.
 */
void test_refs_branches_create__name_vs_namespace_fail(void)
{
	const char * name;
	struct item {
		const char *first;
		const char *first_alternate;
		const char *second;
	};
	static const struct item item[] = {
		{ "level_one/level_two", "level_one/alternate", "level_one" },
		{ "a/b/c/d/e",           "a/b/c/d/alternate",   "a/b/c/d" },
		{ "ss/tt/uu/vv/ww",      "ss/alternate",        "ss" },
		{ NULL, NULL, NULL },
	};
	const struct item *p;

	retrieve_known_commit(&target, repo);

	for (p=item; p->first; p++) {
		cl_git_pass(git_branch_create(&branch, repo, p->first, target, 0));
		cl_git_pass(git_oid_cmp(git_reference_target(branch), git_commit_id(target)));
		cl_git_pass(git_branch_name(&name, branch));
		cl_assert_equal_s(name, p->first);

		cl_git_pass(git_branch_delete(branch));
		git_reference_free(branch);
		branch = NULL;

		cl_git_pass(git_branch_create(&branch, repo, p->first_alternate, target, 0));
		cl_git_pass(git_oid_cmp(git_reference_target(branch), git_commit_id(target)));
		cl_git_pass(git_branch_name(&name, branch));
		cl_assert_equal_s(name, p->first_alternate);

		/* we do not delete the alternate. */
		git_reference_free(branch);
		branch = NULL;

		cl_git_fail(git_branch_create(&branch, repo, p->second, target, 0));
		git_reference_free(branch);
		branch = NULL;
	}
}
