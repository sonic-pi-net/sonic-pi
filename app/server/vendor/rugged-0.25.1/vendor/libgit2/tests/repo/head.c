#include "clar_libgit2.h"
#include "refs.h"
#include "repo_helpers.h"
#include "posix.h"
#include "git2/annotated_commit.h"

static const char *g_email = "foo@example.com";
static git_repository *repo;

void test_repo_head__initialize(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_repository_set_ident(repo, "Foo Bar", g_email));
}

void test_repo_head__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void check_last_reflog_entry(const char *email, const char *message)
{
	git_reflog *log;
	const git_reflog_entry *entry;

	cl_git_pass(git_reflog_read(&log, repo, GIT_HEAD_FILE));
	cl_assert(git_reflog_entrycount(log) > 0);
	entry = git_reflog_entry_byindex(log, 0);
	if (email)
		cl_assert_equal_s(email, git_reflog_entry_committer(entry)->email);
	if (message)
		cl_assert_equal_s(message, git_reflog_entry_message(entry));
	git_reflog_free(log);
}

void test_repo_head__head_detached(void)
{
	git_reference *ref;

	cl_assert_equal_i(false, git_repository_head_detached(repo));

	cl_git_pass(git_repository_detach_head(repo));
	check_last_reflog_entry(g_email, "checkout: moving from master to a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	cl_assert_equal_i(true, git_repository_head_detached(repo));

	/* take the repo back to it's original state */
	cl_git_pass(git_reference_symbolic_create(&ref, repo, "HEAD", "refs/heads/master",
				true, "REATTACH"));
	git_reference_free(ref);

	check_last_reflog_entry(g_email, "REATTACH");
	cl_assert_equal_i(false, git_repository_head_detached(repo));
}

void test_repo_head__unborn_head(void)
{
	git_reference *ref;

	cl_git_pass(git_repository_head_detached(repo));

	make_head_unborn(repo, NON_EXISTING_HEAD);

	cl_assert(git_repository_head_unborn(repo) == 1);


	/* take the repo back to it's original state */
	cl_git_pass(git_reference_symbolic_create(&ref, repo, "HEAD", "refs/heads/master", 1, NULL));
	cl_assert(git_repository_head_unborn(repo) == 0);

	git_reference_free(ref);
}

void test_repo_head__set_head_Attaches_HEAD_to_un_unborn_branch_when_the_branch_doesnt_exist(void)
{
	git_reference *head;

	cl_git_pass(git_repository_set_head(repo, "refs/heads/doesnt/exist/yet"));

	cl_assert_equal_i(false, git_repository_head_detached(repo));

	cl_assert_equal_i(GIT_EUNBORNBRANCH, git_repository_head(&head, repo));
}

void test_repo_head__set_head_Returns_ENOTFOUND_when_the_reference_doesnt_exist(void)
{
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_set_head(repo, "refs/tags/doesnt/exist/yet"));
}

void test_repo_head__set_head_Fails_when_the_reference_points_to_a_non_commitish(void)
{
	cl_git_fail(git_repository_set_head(repo, "refs/tags/point_to_blob"));
}

void test_repo_head__set_head_Attaches_HEAD_when_the_reference_points_to_a_branch(void)
{
	git_reference *head;

	cl_git_pass(git_repository_set_head(repo, "refs/heads/br2"));

	cl_assert_equal_i(false, git_repository_head_detached(repo));

	cl_git_pass(git_repository_head(&head, repo));
	cl_assert_equal_s("refs/heads/br2", git_reference_name(head));

	git_reference_free(head);
}

static void assert_head_is_correctly_detached(void)
{
	git_reference *head;
	git_object *commit;

	cl_assert_equal_i(true, git_repository_head_detached(repo));

	cl_git_pass(git_repository_head(&head, repo));

	cl_git_pass(git_object_lookup(&commit, repo, git_reference_target(head), GIT_OBJ_COMMIT));

	git_object_free(commit);
	git_reference_free(head);
}

void test_repo_head__set_head_Detaches_HEAD_when_the_reference_doesnt_point_to_a_branch(void)
{
	cl_git_pass(git_repository_set_head(repo, "refs/tags/test"));

	cl_assert_equal_i(true, git_repository_head_detached(repo));

	assert_head_is_correctly_detached();
}

void test_repo_head__set_head_detached_Return_ENOTFOUND_when_the_object_doesnt_exist(void)
{
	git_oid oid;

	cl_git_pass(git_oid_fromstr(&oid, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"));

	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_set_head_detached(repo, &oid));
}

void test_repo_head__set_head_detached_Fails_when_the_object_isnt_a_commitish(void)
{
	git_object *blob;

	cl_git_pass(git_revparse_single(&blob, repo, "point_to_blob"));

	cl_git_fail(git_repository_set_head_detached(repo, git_object_id(blob)));

	git_object_free(blob);
}

void test_repo_head__set_head_detached_Detaches_HEAD_and_make_it_point_to_the_peeled_commit(void)
{
	git_object *tag;

	cl_git_pass(git_revparse_single(&tag, repo, "tags/test"));
	cl_assert_equal_i(GIT_OBJ_TAG, git_object_type(tag));

	cl_git_pass(git_repository_set_head_detached(repo, git_object_id(tag)));

	assert_head_is_correctly_detached();

	git_object_free(tag);
}

void test_repo_head__detach_head_Detaches_HEAD_and_make_it_point_to_the_peeled_commit(void)
{
	cl_assert_equal_i(false, git_repository_head_detached(repo));

	cl_git_pass(git_repository_detach_head(repo));

	assert_head_is_correctly_detached();
}

void test_repo_head__detach_head_Fails_if_HEAD_and_point_to_a_non_commitish(void)
{
	git_reference *head;

	cl_git_pass(git_reference_symbolic_create(&head, repo, GIT_HEAD_FILE, "refs/tags/point_to_blob", 1, NULL));

	cl_git_fail(git_repository_detach_head(repo));

	git_reference_free(head);
}

void test_repo_head__detaching_an_unborn_branch_returns_GIT_EUNBORNBRANCH(void)
{
	make_head_unborn(repo, NON_EXISTING_HEAD);

	cl_assert_equal_i(GIT_EUNBORNBRANCH, git_repository_detach_head(repo));
}

void test_repo_head__retrieving_an_unborn_branch_returns_GIT_EUNBORNBRANCH(void)
{
	git_reference *head;

	make_head_unborn(repo, NON_EXISTING_HEAD);

	cl_assert_equal_i(GIT_EUNBORNBRANCH, git_repository_head(&head, repo));
}

void test_repo_head__retrieving_a_missing_head_returns_GIT_ENOTFOUND(void)
{
	git_reference *head;

	delete_head(repo);

	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_head(&head, repo));
}

void test_repo_head__can_tell_if_an_unborn_head_is_detached(void)
{
	make_head_unborn(repo, NON_EXISTING_HEAD);

	cl_assert_equal_i(false, git_repository_head_detached(repo));
}

static void test_reflog(git_repository *repo, size_t idx,
		const char *old_spec, const char *new_spec,
		const char *email, const char *message)
{
	git_reflog *log;
	const git_reflog_entry *entry;

	cl_git_pass(git_reflog_read(&log, repo, "HEAD"));
	entry = git_reflog_entry_byindex(log, idx);

	if (old_spec) {
		git_object *obj;
		cl_git_pass(git_revparse_single(&obj, repo, old_spec));
		cl_assert_equal_oid(git_object_id(obj), git_reflog_entry_id_old(entry));
		git_object_free(obj);
	}
	if (new_spec) {
		git_object *obj;
		cl_git_pass(git_revparse_single(&obj, repo, new_spec));
		cl_assert_equal_oid(git_object_id(obj), git_reflog_entry_id_new(entry));
		git_object_free(obj);
	}

	if (email) {
		cl_assert_equal_s(email, git_reflog_entry_committer(entry)->email);
	}
	if (message) {
		cl_assert_equal_s(message, git_reflog_entry_message(entry));
	}

	git_reflog_free(log);
}

void test_repo_head__setting_head_updates_reflog(void)
{
	git_object *tag;
	git_signature *sig;
	git_annotated_commit *annotated;

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	cl_git_pass(git_repository_set_head(repo, "refs/heads/haacked"));
	cl_git_pass(git_repository_set_head(repo, "refs/heads/unborn"));
	cl_git_pass(git_revparse_single(&tag, repo, "tags/test"));
	cl_git_pass(git_repository_set_head_detached(repo, git_object_id(tag)));
	cl_git_pass(git_repository_set_head(repo, "refs/heads/haacked"));

	test_reflog(repo, 2, NULL, "refs/heads/haacked", "foo@example.com", "checkout: moving from master to haacked");
	test_reflog(repo, 1, NULL, "tags/test^{commit}", "foo@example.com", "checkout: moving from unborn to e90810b8df3e80c413d903f631643c716887138d");
	test_reflog(repo, 0, "tags/test^{commit}", "refs/heads/haacked", "foo@example.com", "checkout: moving from e90810b8df3e80c413d903f631643c716887138d to haacked");

	cl_git_pass(git_annotated_commit_from_revspec(&annotated, repo, "haacked~0"));
	cl_git_pass(git_repository_set_head_detached_from_annotated(repo, annotated));

	test_reflog(repo, 0, NULL, "refs/heads/haacked", "foo@example.com", "checkout: moving from haacked to haacked~0");

	git_annotated_commit_free(annotated);
	git_object_free(tag);
	git_signature_free(sig);
}

static void assert_head_reflog(git_repository *repo, size_t idx,
			       const char *old_id, const char *new_id, const char *message)
{
	git_reflog *log;
	const git_reflog_entry *entry;
	char id_str[GIT_OID_HEXSZ + 1] = {0};

	cl_git_pass(git_reflog_read(&log, repo, GIT_HEAD_FILE));
	entry = git_reflog_entry_byindex(log, idx);

	git_oid_fmt(id_str, git_reflog_entry_id_old(entry));
	cl_assert_equal_s(old_id, id_str);

	git_oid_fmt(id_str, git_reflog_entry_id_new(entry));
	cl_assert_equal_s(new_id, id_str);

	cl_assert_equal_s(message, git_reflog_entry_message(entry));

	git_reflog_free(log);
}

void test_repo_head__detaching_writes_reflog(void)
{
	git_signature *sig;
	git_oid id;
	const char *msg;

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	msg = "checkout: moving from master to e90810b8df3e80c413d903f631643c716887138d";
	git_oid_fromstr(&id, "e90810b8df3e80c413d903f631643c716887138d");
	cl_git_pass(git_repository_set_head_detached(repo, &id));
	assert_head_reflog(repo, 0, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
			   "e90810b8df3e80c413d903f631643c716887138d", msg);

	msg = "checkout: moving from e90810b8df3e80c413d903f631643c716887138d to haacked";
	cl_git_pass(git_repository_set_head(repo, "refs/heads/haacked"));
	assert_head_reflog(repo, 0, "e90810b8df3e80c413d903f631643c716887138d",
			   "258f0e2a959a364e40ed6603d5d44fbb24765b10", msg);

	git_signature_free(sig);
}

void test_repo_head__orphan_branch_does_not_count(void)
{
	git_oid id;
	const char *msg;

	/* Have something known */
	msg = "checkout: moving from master to e90810b8df3e80c413d903f631643c716887138d";
	git_oid_fromstr(&id, "e90810b8df3e80c413d903f631643c716887138d");
	cl_git_pass(git_repository_set_head_detached(repo, &id));
	assert_head_reflog(repo, 0, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
			   "e90810b8df3e80c413d903f631643c716887138d", msg);

	/* Switching to an orphan branch does not write tot he reflog */
	cl_git_pass(git_repository_set_head(repo, "refs/heads/orphan"));
	assert_head_reflog(repo, 0, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
			   "e90810b8df3e80c413d903f631643c716887138d", msg);

	/* And coming back, we set the source to zero */
	msg = "checkout: moving from orphan to haacked";
	cl_git_pass(git_repository_set_head(repo, "refs/heads/haacked"));
	assert_head_reflog(repo, 0, "0000000000000000000000000000000000000000",
			   "258f0e2a959a364e40ed6603d5d44fbb24765b10", msg);
}

void test_repo_head__set_to_current_target(void)
{
	git_reflog *log;
	size_t nentries, nentries_after;

	cl_git_pass(git_reflog_read(&log, repo, GIT_HEAD_FILE));
	nentries = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_git_pass(git_repository_set_head(repo, "refs/heads/haacked"));
	cl_git_pass(git_repository_set_head(repo, "refs/heads/haacked"));

	cl_git_pass(git_reflog_read(&log, repo, GIT_HEAD_FILE));
	nentries_after = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_assert_equal_i(nentries + 1, nentries_after);
}

void test_repo_head__branch_birth(void)
{
	git_signature *sig;
	git_oid id;
	git_tree *tree;
	git_reference *ref;
	const char *msg;
	git_reflog *log;
	size_t nentries, nentries_after;

	cl_git_pass(git_reflog_read(&log, repo, GIT_HEAD_FILE));
	nentries = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	cl_git_pass(git_repository_head(&ref, repo));
	cl_git_pass(git_reference_peel((git_object **) &tree, ref, GIT_OBJ_TREE));
	git_reference_free(ref);

	cl_git_pass(git_repository_set_head(repo, "refs/heads/orphan"));

	cl_git_pass(git_reflog_read(&log, repo, GIT_HEAD_FILE));
	nentries_after = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_assert_equal_i(nentries, nentries_after);

	msg = "message 2";
	cl_git_pass(git_commit_create(&id, repo, "HEAD", sig, sig, NULL, msg, tree, 0, NULL));

	git_tree_free(tree);

	cl_git_pass(git_reflog_read(&log, repo, "refs/heads/orphan"));
	cl_assert_equal_i(1, git_reflog_entrycount(log));
	git_reflog_free(log);

	cl_git_pass(git_reflog_read(&log, repo, GIT_HEAD_FILE));
	nentries_after = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_assert_equal_i(nentries + 1, nentries_after);

	git_signature_free(sig);

}

static size_t entrycount(git_repository *repo, const char *name)
{
	git_reflog *log;
	size_t ret;

	cl_git_pass(git_reflog_read(&log, repo, name));
	ret = git_reflog_entrycount(log);
	git_reflog_free(log);

	return ret;
}

void test_repo_head__symref_chain(void)
{
	git_signature *sig;
	git_oid id;
	git_tree *tree;
	git_reference *ref;
	const char *msg;
	size_t nentries, nentries_master;

	nentries = entrycount(repo, GIT_HEAD_FILE);

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	cl_git_pass(git_repository_head(&ref, repo));
	cl_git_pass(git_reference_peel((git_object **) &tree, ref, GIT_OBJ_TREE));
	git_reference_free(ref);

	nentries_master = entrycount(repo, "refs/heads/master");

	msg = "message 1";
	cl_git_pass(git_reference_symbolic_create(&ref, repo, "refs/heads/master", "refs/heads/foo", 1, msg));
	git_reference_free(ref);

	cl_assert_equal_i(0, entrycount(repo, "refs/heads/foo"));
	cl_assert_equal_i(nentries, entrycount(repo, GIT_HEAD_FILE));
	cl_assert_equal_i(nentries_master, entrycount(repo, "refs/heads/master"));

	msg = "message 2";
	cl_git_pass(git_commit_create(&id, repo, "HEAD", sig, sig, NULL, msg, tree, 0, NULL));
	git_tree_free(tree);

	cl_assert_equal_i(1, entrycount(repo, "refs/heads/foo"));
	cl_assert_equal_i(nentries +1, entrycount(repo, GIT_HEAD_FILE));
	cl_assert_equal_i(nentries_master, entrycount(repo, "refs/heads/master"));

	git_signature_free(sig);

}
