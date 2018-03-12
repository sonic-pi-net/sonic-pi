#include "clar_libgit2.h"

#include "fileops.h"
#include "git2/reflog.h"
#include "reflog.h"

static const char *merge_reflog_message = "commit (merge): Merge commit";
static const char *new_ref = "refs/heads/test-reflog";
static const char *current_master_tip = "a65fedf39aefe402d3bb6e24df4d4f5fe4547750";
#define commit_msg "commit: bla bla"

static git_repository *g_repo;


// helpers
static void assert_signature(const git_signature *expected, const git_signature *actual)
{
	cl_assert(actual);
	cl_assert_equal_s(expected->name, actual->name);
	cl_assert_equal_s(expected->email, actual->email);
	cl_assert(expected->when.offset == actual->when.offset);
	cl_assert(expected->when.time == actual->when.time);
}


// Fixture setup and teardown
void test_refs_reflog_reflog__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo.git");
}

void test_refs_reflog_reflog__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

static void assert_appends(const git_signature *committer, const git_oid *oid)
{
	git_repository *repo2;
	git_reference *lookedup_ref;
	git_reflog *reflog;
	const git_reflog_entry *entry;

	/* Reopen a new instance of the repository */
	cl_git_pass(git_repository_open(&repo2, "testrepo.git"));

	/* Lookup the previously created branch */
	cl_git_pass(git_reference_lookup(&lookedup_ref, repo2, new_ref));

	/* Read and parse the reflog for this branch */
	cl_git_pass(git_reflog_read(&reflog, repo2, new_ref));
	cl_assert_equal_i(3, (int)git_reflog_entrycount(reflog));

	/* The first one was the creation of the branch */
	entry = git_reflog_entry_byindex(reflog, 2);
	cl_assert(git_oid_streq(&entry->oid_old, GIT_OID_HEX_ZERO) == 0);

	entry = git_reflog_entry_byindex(reflog, 1);
	assert_signature(committer, entry->committer);
	cl_assert(git_oid_cmp(oid, &entry->oid_old) == 0);
	cl_assert(git_oid_cmp(oid, &entry->oid_cur) == 0);
	cl_assert(entry->msg == NULL);

	entry = git_reflog_entry_byindex(reflog, 0);
	assert_signature(committer, entry->committer);
	cl_assert(git_oid_cmp(oid, &entry->oid_cur) == 0);
	cl_assert_equal_s(commit_msg, entry->msg);

	git_reflog_free(reflog);
	git_repository_free(repo2);

	git_reference_free(lookedup_ref);
}

void test_refs_reflog_reflog__append_then_read(void)
{
	/* write a reflog for a given reference and ensure it can be read back */
	git_reference *ref;
	git_oid oid;
	git_signature *committer;
	git_reflog *reflog;

	/* Create a new branch pointing at the HEAD */
	git_oid_fromstr(&oid, current_master_tip);
	cl_git_pass(git_reference_create(&ref, g_repo, new_ref, &oid, 0, NULL));
	git_reference_free(ref);

	cl_git_pass(git_signature_now(&committer, "foo", "foo@bar"));

	cl_git_pass(git_reflog_read(&reflog, g_repo, new_ref));

	cl_git_fail(git_reflog_append(reflog, &oid, committer, "no inner\nnewline"));
	cl_git_pass(git_reflog_append(reflog, &oid, committer, NULL));
	cl_git_pass(git_reflog_append(reflog, &oid, committer, commit_msg "\n"));
	cl_git_pass(git_reflog_write(reflog));
	git_reflog_free(reflog);

	assert_appends(committer, &oid);

	git_signature_free(committer);
}

void test_refs_reflog_reflog__renaming_the_reference_moves_the_reflog(void)
{
	git_reference *master, *new_master;
	git_buf master_log_path = GIT_BUF_INIT, moved_log_path = GIT_BUF_INIT;

	git_buf_joinpath(&master_log_path, git_repository_path(g_repo), GIT_REFLOG_DIR);
	git_buf_puts(&moved_log_path, git_buf_cstr(&master_log_path));
	git_buf_joinpath(&master_log_path, git_buf_cstr(&master_log_path), "refs/heads/master");
	git_buf_joinpath(&moved_log_path, git_buf_cstr(&moved_log_path), "refs/moved");

	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&master_log_path)));
	cl_assert_equal_i(false, git_path_isfile(git_buf_cstr(&moved_log_path)));

	cl_git_pass(git_reference_lookup(&master, g_repo, "refs/heads/master"));
	cl_git_pass(git_reference_rename(&new_master, master, "refs/moved", 0, NULL));
	git_reference_free(master);

	cl_assert_equal_i(false, git_path_isfile(git_buf_cstr(&master_log_path)));
	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&moved_log_path)));

	git_reference_free(new_master);
	git_buf_free(&moved_log_path);
	git_buf_free(&master_log_path);
}

void test_refs_reflog_reflog__deleting_the_reference_deletes_the_reflog(void)
{
	git_reference *master;
	git_buf master_log_path = GIT_BUF_INIT;

	git_buf_joinpath(&master_log_path, git_repository_path(g_repo), GIT_REFLOG_DIR);
	git_buf_joinpath(&master_log_path, git_buf_cstr(&master_log_path), "refs/heads/master");

	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&master_log_path)));

	cl_git_pass(git_reference_lookup(&master, g_repo, "refs/heads/master"));
	cl_git_pass(git_reference_delete(master));
	git_reference_free(master);

	cl_assert_equal_i(false, git_path_isfile(git_buf_cstr(&master_log_path)));
	git_buf_free(&master_log_path);
}

void test_refs_reflog_reflog__removes_empty_reflog_dir(void)
{
	git_reference *ref;
	git_buf log_path = GIT_BUF_INIT;
	git_oid id;

	/* Create a new branch pointing at the HEAD */
	git_oid_fromstr(&id, current_master_tip);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/new-dir/new-head", &id, 0, NULL));

	git_buf_joinpath(&log_path, git_repository_path(g_repo), GIT_REFLOG_DIR);
	git_buf_joinpath(&log_path, git_buf_cstr(&log_path), "refs/heads/new-dir/new-head");

	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&log_path)));

	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);

	/* new ref creation should succeed since new-dir is empty */
	git_oid_fromstr(&id, current_master_tip);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/new-dir", &id, 0, NULL));
	git_reference_free(ref);

	git_buf_free(&log_path);
}

void test_refs_reflog_reflog__fails_gracefully_on_nonempty_reflog_dir(void)
{
	git_reference *ref;
	git_buf log_path = GIT_BUF_INIT;
	git_oid id;

	/* Create a new branch pointing at the HEAD */
	git_oid_fromstr(&id, current_master_tip);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/new-dir/new-head", &id, 0, NULL));
	git_reference_free(ref);

	git_buf_joinpath(&log_path, git_repository_path(g_repo), GIT_REFLOG_DIR);
	git_buf_joinpath(&log_path, git_buf_cstr(&log_path), "refs/heads/new-dir/new-head");

	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&log_path)));

	/* delete the ref manually, leave the reflog */
	cl_must_pass(p_unlink("testrepo.git/refs/heads/new-dir/new-head"));

	/* new ref creation should fail since new-dir contains reflogs still */
	git_oid_fromstr(&id, current_master_tip);
	cl_git_fail_with(GIT_EDIRECTORY, git_reference_create(&ref, g_repo, "refs/heads/new-dir", &id, 0, NULL));
	git_reference_free(ref);

	git_buf_free(&log_path);
}

static void assert_has_reflog(bool expected_result, const char *name)
{
	cl_assert_equal_i(expected_result, git_reference_has_log(g_repo, name));
}

void test_refs_reflog_reflog__reference_has_reflog(void)
{
	assert_has_reflog(true, "HEAD");
	assert_has_reflog(true, "refs/heads/master");
	assert_has_reflog(false, "refs/heads/subtrees");
}

void test_refs_reflog_reflog__reading_the_reflog_from_a_reference_with_no_log_returns_an_empty_one(void)
{
	git_reflog *reflog;
	const char *refname = "refs/heads/subtrees";
	git_buf subtrees_log_path = GIT_BUF_INIT;

	git_buf_join_n(&subtrees_log_path, '/', 3, git_repository_path(g_repo), GIT_REFLOG_DIR, refname);
	cl_assert_equal_i(false, git_path_isfile(git_buf_cstr(&subtrees_log_path)));

	cl_git_pass(git_reflog_read(&reflog, g_repo, refname));

	cl_assert_equal_i(0, (int)git_reflog_entrycount(reflog));

	git_reflog_free(reflog);
	git_buf_free(&subtrees_log_path);
}

void test_refs_reflog_reflog__reading_a_reflog_with_invalid_format_returns_error(void)
{
	git_reflog *reflog;
	const git_error *error;
	const char *refname = "refs/heads/newline";
	const char *refmessage =
		"Reflog*message with a newline and enough content after it to pass the GIT_REFLOG_SIZE_MIN check inside reflog_parse.";
	git_reference *ref;
	git_oid id;
	git_buf logpath = GIT_BUF_INIT, logcontents = GIT_BUF_INIT;
	char *star;

	git_oid_fromstr(&id, current_master_tip);

	/* create a new branch */
	cl_git_pass(git_reference_create(&ref, g_repo, refname, &id, 1, refmessage));

	/* corrupt the branch reflog by introducing a newline inside the reflog message (we replace '*' with '\n') */
	git_buf_join_n(&logpath, '/', 3, git_repository_path(g_repo), GIT_REFLOG_DIR, refname);
	cl_git_pass(git_futils_readbuffer(&logcontents, git_buf_cstr(&logpath)));
	cl_assert((star = strchr(git_buf_cstr(&logcontents), '*')) != NULL);
	*star = '\n';
	cl_git_rewritefile(git_buf_cstr(&logpath), git_buf_cstr(&logcontents));

	/* confirm that the file was rewritten successfully and now contains a '\n' in the expected location */
	cl_git_pass(git_futils_readbuffer(&logcontents, git_buf_cstr(&logpath)));
	cl_assert(strstr(git_buf_cstr(&logcontents), "Reflog\nmessage") != NULL);

	/* clear the error state so we can capture the error generated by git_reflog_read */
	giterr_clear();

	cl_git_fail(git_reflog_read(&reflog, g_repo, refname));

	error = giterr_last();

	cl_assert(error != NULL);
	cl_assert_equal_s("unable to parse OID - contains invalid characters", error->message);

	git_reference_free(ref);
	git_buf_free(&logpath);
	git_buf_free(&logcontents);
}

void test_refs_reflog_reflog__cannot_write_a_moved_reflog(void)
{
	git_reference *master, *new_master;
	git_buf master_log_path = GIT_BUF_INIT, moved_log_path = GIT_BUF_INIT;
	git_reflog *reflog;

	cl_git_pass(git_reference_lookup(&master, g_repo, "refs/heads/master"));
	cl_git_pass(git_reflog_read(&reflog, g_repo, "refs/heads/master"));

	cl_git_pass(git_reflog_write(reflog));

	cl_git_pass(git_reference_rename(&new_master, master, "refs/moved", 0, NULL));
	git_reference_free(master);

	cl_git_fail(git_reflog_write(reflog));

	git_reflog_free(reflog);
	git_reference_free(new_master);
	git_buf_free(&moved_log_path);
	git_buf_free(&master_log_path);
}

void test_refs_reflog_reflog__renaming_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	cl_assert_equal_i(GIT_EINVALIDSPEC,
			  git_reflog_rename(g_repo, "refs/heads/master", "refs/heads/Inv@{id"));
}

void test_refs_reflog_reflog__write_only_std_locations(void)
{
	git_reference *ref;
	git_oid id;

	git_oid_fromstr(&id, current_master_tip);

	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/foo", &id, 1, NULL));
	git_reference_free(ref);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/tags/foo", &id, 1, NULL));
	git_reference_free(ref);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/notes/foo", &id, 1, NULL));
	git_reference_free(ref);

	assert_has_reflog(true, "refs/heads/foo");
	assert_has_reflog(false, "refs/tags/foo");
	assert_has_reflog(true, "refs/notes/foo");

}

void test_refs_reflog_reflog__write_when_explicitly_active(void)
{
	git_reference *ref;
	git_oid id;

	git_oid_fromstr(&id, current_master_tip);
	git_reference_ensure_log(g_repo, "refs/tags/foo");

	cl_git_pass(git_reference_create(&ref, g_repo, "refs/tags/foo", &id, 1, NULL));
	git_reference_free(ref);
	assert_has_reflog(true, "refs/tags/foo");
}

void test_refs_reflog_reflog__append_to_HEAD_when_changing_current_branch(void)
{
	size_t nlogs, nlogs_after;
	git_reference *ref;
	git_reflog *log;
	git_oid id;

	cl_git_pass(git_reflog_read(&log, g_repo, "HEAD"));
	nlogs = git_reflog_entrycount(log);
	git_reflog_free(log);

	/* Move it back */
	git_oid_fromstr(&id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/master", &id, 1, NULL));
	git_reference_free(ref);

	cl_git_pass(git_reflog_read(&log, g_repo, "HEAD"));
	nlogs_after = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_assert_equal_i(nlogs_after, nlogs + 1);
}

void test_refs_reflog_reflog__do_not_append_when_no_update(void)
{
	size_t nlogs, nlogs_after;
	git_reference *ref, *ref2;
	git_reflog *log;

	cl_git_pass(git_reflog_read(&log, g_repo, "HEAD"));
	nlogs = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_git_pass(git_reference_lookup(&ref, g_repo, "refs/heads/master"));
	cl_git_pass(git_reference_create(&ref2, g_repo, "refs/heads/master",
					 git_reference_target(ref), 1, NULL));

	git_reference_free(ref);
	git_reference_free(ref2);

	cl_git_pass(git_reflog_read(&log, g_repo, "HEAD"));
	nlogs_after = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_assert_equal_i(nlogs_after, nlogs);
}

static void assert_no_reflog_update(void)
{
	size_t nlogs, nlogs_after;
	size_t nlogs_master, nlogs_master_after;
	git_reference *ref;
	git_reflog *log;
	git_oid id;

	cl_git_pass(git_reflog_read(&log, g_repo, "HEAD"));
	nlogs = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_git_pass(git_reflog_read(&log, g_repo, "refs/heads/master"));
	nlogs_master = git_reflog_entrycount(log);
	git_reflog_free(log);

	/* Move it back */
	git_oid_fromstr(&id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/master", &id, 1, NULL));
	git_reference_free(ref);

	cl_git_pass(git_reflog_read(&log, g_repo, "HEAD"));
	nlogs_after = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_assert_equal_i(nlogs_after, nlogs);

	cl_git_pass(git_reflog_read(&log, g_repo, "refs/heads/master"));
	nlogs_master_after = git_reflog_entrycount(log);
	git_reflog_free(log);

	cl_assert_equal_i(nlogs_after, nlogs);
	cl_assert_equal_i(nlogs_master_after, nlogs_master);

}

void test_refs_reflog_reflog__logallrefupdates_bare_set_false(void)
{
	git_config *config;

	cl_git_pass(git_repository_config(&config, g_repo));
	cl_git_pass(git_config_set_bool(config, "core.logallrefupdates", false));
	git_config_free(config);

	assert_no_reflog_update();
}

void test_refs_reflog_reflog__logallrefupdates_bare_unset(void)
{
	git_config *config;

	cl_git_pass(git_repository_config(&config, g_repo));
	cl_git_pass(git_config_delete_entry(config, "core.logallrefupdates"));
	git_config_free(config);

	assert_no_reflog_update();
}

void test_refs_reflog_reflog__logallrefupdates_nonbare_set_false(void)
{
	git_config *config;

	cl_git_sandbox_cleanup();
	g_repo = cl_git_sandbox_init("testrepo");


	cl_git_pass(git_repository_config(&config, g_repo));
	cl_git_pass(git_config_set_bool(config, "core.logallrefupdates", false));
	git_config_free(config);

	assert_no_reflog_update();
}

void test_refs_reflog_reflog__show_merge_for_merge_commits(void)
{
	git_oid b1_oid;
	git_oid b2_oid;
	git_oid merge_commit_oid;
	git_commit *b1_commit;
	git_commit *b2_commit;
	git_signature *s;
	git_commit *parent_commits[2];
	git_tree *tree;
	git_reflog *log;
	const git_reflog_entry *entry;

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

	cl_git_pass(git_reflog_read(&log, g_repo, "HEAD"));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s(merge_reflog_message, git_reflog_entry_message(entry));

	git_reflog_free(log);
	git_tree_free(tree);
	git_commit_free(b1_commit);
	git_commit_free(b2_commit);
	git_signature_free(s);
}
