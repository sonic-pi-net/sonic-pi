#include "clar_libgit2.h"

#include "futils.h"
#include "git2/reflog.h"
#include "reflog.h"
#include "refs.h"
#include "reflog_helpers.h"

static const char *g_email = "foo@example.com";
static git_repository *g_repo;

/* Fixture setup and teardown */
void test_refs_reflog_messages__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_repository_set_ident(g_repo, "Foo Bar", g_email));
}

void test_refs_reflog_messages__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_refs_reflog_messages__setting_head_updates_reflog(void)
{
	git_object *tag;
	git_annotated_commit *annotated;

	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/haacked")); /* 4 */
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/unborn"));
	cl_git_pass(git_revparse_single(&tag, g_repo, "tags/test"));
	cl_git_pass(git_repository_set_head_detached(g_repo, git_object_id(tag))); /* 3 */
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/haacked"));        /* 2 */
	cl_git_pass(git_repository_set_head(g_repo, "refs/tags/test"));            /* 1 */
	cl_git_pass(git_repository_set_head(g_repo, "refs/remotes/test/master"));  /* 0 */

	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 4,
		NULL, "refs/heads/haacked",
		"foo@example.com",
		"checkout: moving from master to haacked");
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 3,
		NULL, "tags/test^{commit}",
		"foo@example.com",
		"checkout: moving from unborn to e90810b8df3e80c413d903f631643c716887138d");
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 2,
		"tags/test^{commit}", "refs/heads/haacked",
		"foo@example.com",
		"checkout: moving from e90810b8df3e80c413d903f631643c716887138d to haacked");
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 1,
		"refs/heads/haacked", "tags/test^{commit}",
		"foo@example.com",
		"checkout: moving from haacked to test");
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		"tags/test^{commit}", "refs/remotes/test/master",
		"foo@example.com",
		"checkout: moving from e90810b8df3e80c413d903f631643c716887138d to test/master");

	cl_git_pass(git_annotated_commit_from_revspec(&annotated, g_repo, "haacked~0"));
	cl_git_pass(git_repository_set_head_detached_from_annotated(g_repo, annotated));

	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		NULL, "refs/heads/haacked",
		"foo@example.com",
		"checkout: moving from be3563ae3f795b2b4353bcce3a527ad0a4f7f644 to haacked~0");

	git_annotated_commit_free(annotated);
	git_object_free(tag);
}

void test_refs_reflog_messages__setting_head_to_same_target_ignores_reflog(void)
{
	size_t nentries, nentries_after;

	nentries = reflog_entrycount(g_repo, GIT_HEAD_FILE);

	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/haacked"));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/haacked"));

	nentries_after = reflog_entrycount(g_repo, GIT_HEAD_FILE);

	cl_assert_equal_i(nentries + 1, nentries_after);
}

void test_refs_reflog_messages__detaching_writes_reflog(void)
{
	git_oid id;
	const char *msg;

	msg = "checkout: moving from master to e90810b8df3e80c413d903f631643c716887138d";
	git_oid__fromstr(&id, "e90810b8df3e80c413d903f631643c716887138d", GIT_OID_SHA1);
	cl_git_pass(git_repository_set_head_detached(g_repo, &id));
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"e90810b8df3e80c413d903f631643c716887138d",
		NULL, msg);

	msg = "checkout: moving from e90810b8df3e80c413d903f631643c716887138d to haacked";
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/haacked"));
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		"e90810b8df3e80c413d903f631643c716887138d",
		"258f0e2a959a364e40ed6603d5d44fbb24765b10",
		NULL, msg);
}

void test_refs_reflog_messages__orphan_branch_does_not_count(void)
{
	git_oid id;
	const char *msg;

	/* Have something known */
	msg = "checkout: moving from master to e90810b8df3e80c413d903f631643c716887138d";
	git_oid__fromstr(&id, "e90810b8df3e80c413d903f631643c716887138d", GIT_OID_SHA1);
	cl_git_pass(git_repository_set_head_detached(g_repo, &id));
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"e90810b8df3e80c413d903f631643c716887138d",
		NULL, msg);

	/* Switching to an orphan branch does not write to the reflog */
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/orphan"));
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"e90810b8df3e80c413d903f631643c716887138d",
		NULL, msg);

	/* And coming back, we set the source to zero */
	msg = "checkout: moving from orphan to haacked";
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/haacked"));
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		"0000000000000000000000000000000000000000",
		"258f0e2a959a364e40ed6603d5d44fbb24765b10",
		NULL, msg);
}

void test_refs_reflog_messages__branch_birth(void)
{
	git_signature *sig;
	git_oid id;
	git_tree *tree;
	git_reference *ref;
	const char *msg;
	size_t nentries, nentries_after;

	nentries = reflog_entrycount(g_repo, GIT_HEAD_FILE);

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	cl_git_pass(git_repository_head(&ref, g_repo));
	cl_git_pass(git_reference_peel((git_object **) &tree, ref, GIT_OBJECT_TREE));

	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/orphan"));

	nentries_after = reflog_entrycount(g_repo, GIT_HEAD_FILE);

	cl_assert_equal_i(nentries, nentries_after);

	msg = "message 2";
	cl_git_pass(git_commit_create(&id, g_repo, "HEAD", sig, sig, NULL, msg, tree, 0, NULL));

	cl_assert_equal_i(1, reflog_entrycount(g_repo, "refs/heads/orphan"));

	nentries_after = reflog_entrycount(g_repo, GIT_HEAD_FILE);

	cl_assert_equal_i(nentries + 1, nentries_after);

	git_signature_free(sig);
	git_tree_free(tree);
	git_reference_free(ref);
}

void test_refs_reflog_messages__commit_on_symbolic_ref_updates_head_reflog(void)
{
	git_signature *sig;
	git_oid id;
	git_tree *tree;
	git_reference *ref1, *ref2;
	const char *msg;
	size_t nentries_head, nentries_master;

	nentries_head = reflog_entrycount(g_repo, GIT_HEAD_FILE);

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	cl_git_pass(git_repository_head(&ref1, g_repo));
	cl_git_pass(git_reference_peel((git_object **) &tree, ref1, GIT_OBJECT_TREE));

	nentries_master = reflog_entrycount(g_repo, "refs/heads/master");

	msg = "message 1";
	cl_git_pass(git_reference_symbolic_create(&ref2, g_repo, "refs/heads/master", "refs/heads/foo", 1, msg));

	cl_assert_equal_i(0, reflog_entrycount(g_repo, "refs/heads/foo"));
	cl_assert_equal_i(nentries_head, reflog_entrycount(g_repo, GIT_HEAD_FILE));
	cl_assert_equal_i(nentries_master, reflog_entrycount(g_repo, "refs/heads/master"));

	msg = "message 2";
	cl_git_pass(git_commit_create(&id, g_repo, "HEAD", sig, sig, NULL, msg, tree, 0, NULL));

	cl_assert_equal_i(1, reflog_entrycount(g_repo, "refs/heads/foo"));
	cl_assert_equal_i(nentries_head + 1, reflog_entrycount(g_repo, GIT_HEAD_FILE));
	cl_assert_equal_i(nentries_master, reflog_entrycount(g_repo, "refs/heads/master"));

	git_signature_free(sig);
	git_reference_free(ref1);
	git_reference_free(ref2);
	git_tree_free(tree);
}

void test_refs_reflog_messages__show_merge_for_merge_commits(void)
{
	git_oid b1_oid;
	git_oid b2_oid;
	git_oid merge_commit_oid;
	git_commit *b1_commit;
	git_commit *b2_commit;
	git_signature *s;
	git_commit *parent_commits[2];
	git_tree *tree;

	cl_git_pass(git_signature_now(&s, "alice", "alice@example.com"));

	cl_git_pass(git_reference_name_to_id(&b1_oid, g_repo, "HEAD"));
	cl_git_pass(git_reference_name_to_id(&b2_oid, g_repo, "refs/heads/test"));

	cl_git_pass(git_commit_lookup(&b1_commit, g_repo, &b1_oid));
	cl_git_pass(git_commit_lookup(&b2_commit, g_repo, &b2_oid));

	parent_commits[0] = b1_commit;
	parent_commits[1] = b2_commit;

	cl_git_pass(git_commit_tree(&tree, b1_commit));

	cl_git_pass(git_commit_create(&merge_commit_oid,
		g_repo, "HEAD", s, s, NULL,
		"Merge commit", tree,
		2, (const struct git_commit **) parent_commits));

	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		NULL,
		git_oid_tostr_s(&merge_commit_oid),
		NULL, "commit (merge): Merge commit");

	git_tree_free(tree);
	git_commit_free(b1_commit);
	git_commit_free(b2_commit);
	git_signature_free(s);
}

void test_refs_reflog_messages__creating_a_direct_reference(void)
{
	git_reference *reference;
	git_oid id;
	git_reflog *reflog;
	const git_reflog_entry *entry;

	const char *name = "refs/heads/new-head";
	const char *message = "You've been logged, mate!";

	cl_git_pass(git_reference_name_to_id(&id, g_repo, "HEAD"));

	cl_git_pass(git_reference_create(&reference, g_repo, name, &id, 0, message));

	cl_git_pass(git_reflog_read(&reflog, g_repo, name));
	cl_assert_equal_sz(1, git_reflog_entrycount(reflog));

	entry = git_reflog_entry_byindex(reflog, 0);
	cl_assert(git_oid_streq(&entry->oid_old, GIT_OID_SHA1_HEXZERO) == 0);
	cl_assert_equal_oid(&id, &entry->oid_cur);
	cl_assert_equal_s(message, entry->msg);

	git_reflog_free(reflog);
	git_reference_free(reference);
}

void test_refs_reflog_messages__newline_gets_replaced(void)
{
	const git_reflog_entry *entry;
	git_signature *signature;
	git_reflog *reflog;
	git_oid oid;

	cl_git_pass(git_signature_now(&signature, "me", "foo@example.com"));
	cl_git_pass(git_oid__fromstr(&oid, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", GIT_OID_SHA1));

	cl_git_pass(git_reflog_read(&reflog, g_repo, "HEAD"));
	cl_assert_equal_sz(7, git_reflog_entrycount(reflog));
	cl_git_pass(git_reflog_append(reflog, &oid, signature, "inner\nnewline"));
	cl_assert_equal_sz(8, git_reflog_entrycount(reflog));

	cl_assert(entry = git_reflog_entry_byindex(reflog, 0));
	cl_assert_equal_s(git_reflog_entry_message(entry), "inner newline");

	git_signature_free(signature);
	git_reflog_free(reflog);
}

void test_refs_reflog_messages__renaming_ref(void)
{
	git_reference *ref, *new_ref;

	cl_git_pass(git_reference_lookup(&ref, g_repo, "refs/heads/master"));
	cl_git_pass(git_reference_rename(&new_ref, ref, "refs/heads/renamed", false,
									 "message"));

	cl_reflog_check_entry(g_repo, git_reference_name(new_ref), 0,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"foo@example.com", "message");

	git_reference_free(ref);
	git_reference_free(new_ref);
}

void test_refs_reflog_messages__updating_a_direct_reference(void)
{
	git_reference *ref, *ref_out, *target_ref;
	git_oid target_id;
	const char *message = "You've been logged, mate!";

	git_reference_name_to_id(&target_id, g_repo, "refs/heads/haacked");
	cl_git_pass(git_reference_lookup(&target_ref, g_repo, "refs/heads/haacked"));

	cl_git_pass(git_reference_lookup(&ref, g_repo, "refs/heads/master"));

	cl_git_pass(git_reference_set_target(&ref_out, ref, &target_id, message));

	cl_reflog_check_entry(g_repo, "refs/heads/master", 0,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"258f0e2a959a364e40ed6603d5d44fbb24765b10",
		NULL, message);

	git_reference_free(target_ref);
	git_reference_free(ref);
	git_reference_free(ref_out);
}

#define NEW_BRANCH_NAME "new-branch-on-the-block"

void test_refs_reflog_messages__creating_branches_default_messages(void)
{
	git_str buf = GIT_STR_INIT;
	git_annotated_commit *annotated;
	git_object *obj;
	git_commit *target;
	git_reference *branch1, *branch2;

	cl_git_pass(git_revparse_single(&obj, g_repo, "e90810b8df3"));
	cl_git_pass(git_commit_lookup(&target, g_repo, git_object_id(obj)));
	git_object_free(obj);

	cl_git_pass(git_branch_create(&branch1, g_repo, NEW_BRANCH_NAME, target, false));

	cl_git_pass(git_str_printf(&buf, "branch: Created from %s", git_oid_tostr_s(git_commit_id(target))));
	cl_reflog_check_entry(g_repo, "refs/heads/" NEW_BRANCH_NAME, 0,
		GIT_OID_SHA1_HEXZERO,
		git_oid_tostr_s(git_commit_id(target)),
		g_email, git_str_cstr(&buf));

	cl_git_pass(git_reference_remove(g_repo, "refs/heads/" NEW_BRANCH_NAME));

	cl_git_pass(git_annotated_commit_from_revspec(&annotated, g_repo, "e90810b8df3"));
	cl_git_pass(git_branch_create_from_annotated(&branch2, g_repo, NEW_BRANCH_NAME, annotated, true));

	cl_reflog_check_entry(g_repo, "refs/heads/" NEW_BRANCH_NAME, 0,
		GIT_OID_SHA1_HEXZERO,
		git_oid_tostr_s(git_commit_id(target)),
		g_email, "branch: Created from e90810b8df3");

	git_annotated_commit_free(annotated);
	git_str_dispose(&buf);
	git_commit_free(target);
	git_reference_free(branch1);
	git_reference_free(branch2);
}

void test_refs_reflog_messages__moving_branch_default_message(void)
{
	git_reference *branch;
	git_reference *new_branch;
	git_oid id;

	cl_git_pass(git_reference_lookup(&branch, g_repo, "refs/heads/master"));
	git_oid_cpy(&id, git_reference_target(branch));
	cl_git_pass(git_branch_move(&new_branch, branch, "master2", 0));

	cl_reflog_check_entry(g_repo, git_reference_name(new_branch), 0,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		g_email,
		"branch: renamed refs/heads/master to refs/heads/master2");

	git_reference_free(branch);
	git_reference_free(new_branch);
}

void test_refs_reflog_messages__detaching_head_default_message(void)
{
	git_reference *ref;

	cl_assert_equal_i(false, git_repository_head_detached(g_repo));

	cl_git_pass(git_repository_detach_head(g_repo));
	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		NULL, "checkout: moving from master to a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	cl_assert_equal_i(true, git_repository_head_detached(g_repo));

	/* take the repo back to its original state */
	cl_git_pass(git_reference_symbolic_create(&ref, g_repo, "HEAD", "refs/heads/master",
											  true, "REATTACH"));

	cl_reflog_check_entry(g_repo, GIT_HEAD_FILE, 0,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
		NULL, "REATTACH");

	cl_assert_equal_i(false, git_repository_head_detached(g_repo));

	git_reference_free(ref);
}
