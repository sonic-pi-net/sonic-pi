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

	cl_git_pass(git_branch_create(&branch, repo, NEW_BRANCH_NAME, target, 0, NULL, NULL));
	cl_git_pass(git_oid_cmp(git_reference_target(branch), git_commit_id(target)));
}

void test_refs_branches_create__can_not_create_a_branch_if_its_name_collide_with_an_existing_one(void)
{
	retrieve_known_commit(&target, repo);

	cl_assert_equal_i(GIT_EEXISTS, git_branch_create(&branch, repo, "br2", target, 0, NULL, NULL));
}

void test_refs_branches_create__can_force_create_over_an_existing_branch(void)
{
	retrieve_known_commit(&target, repo);

	cl_git_pass(git_branch_create(&branch, repo, "br2", target, 1, NULL, NULL));
	cl_git_pass(git_oid_cmp(git_reference_target(branch), git_commit_id(target)));
	cl_assert_equal_s("refs/heads/br2", git_reference_name(branch));
}

void test_refs_branches_create__cannot_force_create_over_current_branch(void)
{
	const git_oid *oid;
	git_reference *branch2;
	retrieve_known_commit(&target, repo);

	cl_git_pass(git_branch_lookup(&branch2, repo, "master", GIT_BRANCH_LOCAL));
	cl_assert_equal_s("refs/heads/master", git_reference_name(branch2));
	cl_assert_equal_i(true, git_branch_is_head(branch2));
	oid = git_reference_target(branch2);

	cl_git_fail_with(-1, git_branch_create(&branch, repo, "master", target, 1, NULL, NULL));
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
		git_branch_create(&branch, repo, "inv@{id", target, 0, NULL, NULL));
}

void test_refs_branches_create__creation_creates_new_reflog(void)
{
	git_reflog *log;
	const git_reflog_entry *entry;
	git_signature *sig;

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	retrieve_known_commit(&target, repo);
	cl_git_pass(git_branch_create(&branch, repo, NEW_BRANCH_NAME, target, false, sig, "create!"));
	cl_git_pass(git_reflog_read(&log, repo, "refs/heads/" NEW_BRANCH_NAME));

	cl_assert_equal_i(1, git_reflog_entrycount(log));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("create!", git_reflog_entry_message(entry));
	cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);

	git_reflog_free(log);
	git_signature_free(sig);
}

void test_refs_branches_create__default_reflog_message(void)
{
	git_reflog *log;
	const git_reflog_entry *entry;
	git_signature *sig;
	git_config *cfg;

	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_string(cfg, "user.name", "Foo Bar"));
	cl_git_pass(git_config_set_string(cfg, "user.email", "foo@example.com"));
	git_config_free(cfg);

	cl_git_pass(git_signature_default(&sig, repo));

	retrieve_known_commit(&target, repo);
	cl_git_pass(git_branch_create(&branch, repo, NEW_BRANCH_NAME, target, false, NULL, NULL));
	cl_git_pass(git_reflog_read(&log, repo, "refs/heads/" NEW_BRANCH_NAME));

	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("Branch: created", git_reflog_entry_message(entry));
	cl_assert_equal_s(sig->email, git_reflog_entry_committer(entry)->email);

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
		cl_git_pass(git_branch_create(
			&branch, repo, names[i], target, 0, NULL, NULL));
		cl_git_pass(git_oid_cmp(
			git_reference_target(branch), git_commit_id(target)));

		assert_branch_matches_name(expected[i], names[i]);
		if (fs_decompose_unicode && alt[i])
			assert_branch_matches_name(expected[i], alt[i]);

		cl_git_pass(git_branch_delete(branch));
		git_reference_free(branch);
		branch = NULL;
	}
}
