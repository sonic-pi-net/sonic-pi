#include "clar_libgit2.h"

#include "futils.h"
#include "git2/reflog.h"
#include "reflog.h"

static const char *new_ref = "refs/heads/test-reflog";
static const char *current_master_tip = "a65fedf39aefe402d3bb6e24df4d4f5fe4547750";
#define commit_msg "commit: bla bla"

static git_repository *g_repo;


/* helpers */
static void assert_signature(const git_signature *expected, const git_signature *actual)
{
	cl_assert(actual);
	cl_assert_equal_s(expected->name, actual->name);
	cl_assert_equal_s(expected->email, actual->email);
	cl_assert(expected->when.offset == actual->when.offset);
	cl_assert(expected->when.time == actual->when.time);
}


/* Fixture setup and teardown */
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
	cl_assert(git_oid_streq(&entry->oid_old, GIT_OID_SHA1_HEXZERO) == 0);

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
	git_oid__fromstr(&oid, current_master_tip, GIT_OID_SHA1);
	cl_git_pass(git_reference_create(&ref, g_repo, new_ref, &oid, 0, NULL));
	git_reference_free(ref);

	cl_git_pass(git_signature_now(&committer, "foo", "foo@bar"));

	cl_git_pass(git_reflog_read(&reflog, g_repo, new_ref));
	cl_git_pass(git_reflog_append(reflog, &oid, committer, NULL));
	cl_git_pass(git_reflog_append(reflog, &oid, committer, commit_msg "\n"));
	cl_git_pass(git_reflog_write(reflog));

	assert_appends(committer, &oid);

	git_reflog_free(reflog);
	git_signature_free(committer);
}

void test_refs_reflog_reflog__renaming_the_reference_moves_the_reflog(void)
{
	git_reference *master, *new_master;
	git_str master_log_path = GIT_STR_INIT, moved_log_path = GIT_STR_INIT;

	git_str_joinpath(&master_log_path, git_repository_path(g_repo), GIT_REFLOG_DIR);
	git_str_puts(&moved_log_path, git_str_cstr(&master_log_path));
	git_str_joinpath(&master_log_path, git_str_cstr(&master_log_path), "refs/heads/master");
	git_str_joinpath(&moved_log_path, git_str_cstr(&moved_log_path), "refs/moved");

	cl_assert_equal_i(true, git_fs_path_isfile(git_str_cstr(&master_log_path)));
	cl_assert_equal_i(false, git_fs_path_isfile(git_str_cstr(&moved_log_path)));

	cl_git_pass(git_reference_lookup(&master, g_repo, "refs/heads/master"));
	cl_git_pass(git_reference_rename(&new_master, master, "refs/moved", 0, NULL));
	git_reference_free(master);

	cl_assert_equal_i(false, git_fs_path_isfile(git_str_cstr(&master_log_path)));
	cl_assert_equal_i(true, git_fs_path_isfile(git_str_cstr(&moved_log_path)));

	git_reference_free(new_master);
	git_str_dispose(&moved_log_path);
	git_str_dispose(&master_log_path);
}

void test_refs_reflog_reflog__deleting_the_reference_deletes_the_reflog(void)
{
	git_reference *master;
	git_str master_log_path = GIT_STR_INIT;

	git_str_joinpath(&master_log_path, git_repository_path(g_repo), GIT_REFLOG_DIR);
	git_str_joinpath(&master_log_path, git_str_cstr(&master_log_path), "refs/heads/master");

	cl_assert_equal_i(true, git_fs_path_isfile(git_str_cstr(&master_log_path)));

	cl_git_pass(git_reference_lookup(&master, g_repo, "refs/heads/master"));
	cl_git_pass(git_reference_delete(master));
	git_reference_free(master);

	cl_assert_equal_i(false, git_fs_path_isfile(git_str_cstr(&master_log_path)));
	git_str_dispose(&master_log_path);
}

void test_refs_reflog_reflog__removes_empty_reflog_dir(void)
{
	git_reference *ref;
	git_str log_path = GIT_STR_INIT;
	git_oid id;

	/* Create a new branch pointing at the HEAD */
	git_oid__fromstr(&id, current_master_tip, GIT_OID_SHA1);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/new-dir/new-head", &id, 0, NULL));

	git_str_joinpath(&log_path, git_repository_path(g_repo), GIT_REFLOG_DIR);
	git_str_joinpath(&log_path, git_str_cstr(&log_path), "refs/heads/new-dir/new-head");

	cl_assert_equal_i(true, git_fs_path_isfile(git_str_cstr(&log_path)));

	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);

	/* new ref creation should succeed since new-dir is empty */
	git_oid__fromstr(&id, current_master_tip, GIT_OID_SHA1);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/new-dir", &id, 0, NULL));
	git_reference_free(ref);

	git_str_dispose(&log_path);
}

void test_refs_reflog_reflog__fails_gracefully_on_nonempty_reflog_dir(void)
{
	git_reference *ref;
	git_str log_path = GIT_STR_INIT;
	git_oid id;

	/* Create a new branch pointing at the HEAD */
	git_oid__fromstr(&id, current_master_tip, GIT_OID_SHA1);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/new-dir/new-head", &id, 0, NULL));
	git_reference_free(ref);

	git_str_joinpath(&log_path, git_repository_path(g_repo), GIT_REFLOG_DIR);
	git_str_joinpath(&log_path, git_str_cstr(&log_path), "refs/heads/new-dir/new-head");

	cl_assert_equal_i(true, git_fs_path_isfile(git_str_cstr(&log_path)));

	/* delete the ref manually, leave the reflog */
	cl_must_pass(p_unlink("testrepo.git/refs/heads/new-dir/new-head"));

	/* new ref creation should fail since new-dir contains reflogs still */
	git_oid__fromstr(&id, current_master_tip, GIT_OID_SHA1);
	cl_git_fail_with(GIT_EDIRECTORY, git_reference_create(&ref, g_repo, "refs/heads/new-dir", &id, 0, NULL));
	git_reference_free(ref);

	git_str_dispose(&log_path);
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
	git_str subtrees_log_path = GIT_STR_INIT;

	git_str_join_n(&subtrees_log_path, '/', 3, git_repository_path(g_repo), GIT_REFLOG_DIR, refname);
	cl_assert_equal_i(false, git_fs_path_isfile(git_str_cstr(&subtrees_log_path)));

	cl_git_pass(git_reflog_read(&reflog, g_repo, refname));

	cl_assert_equal_i(0, (int)git_reflog_entrycount(reflog));

	git_reflog_free(reflog);
	git_str_dispose(&subtrees_log_path);
}

void test_refs_reflog_reflog__reading_a_reflog_with_invalid_format_succeeds(void)
{
	git_reflog *reflog;
	const char *refname = "refs/heads/newline";
	const char *refmessage =
		"Reflog*message with a newline and enough content after it to pass the GIT_REFLOG_SIZE_MIN check inside reflog_parse.";
	const git_reflog_entry *entry;
	git_reference *ref;
	git_oid id;
	git_str logpath = GIT_STR_INIT, logcontents = GIT_STR_INIT;
	char *star;

	/* Create a new branch. */
	cl_git_pass(git_oid__fromstr(&id, current_master_tip, GIT_OID_SHA1));
	cl_git_pass(git_reference_create(&ref, g_repo, refname, &id, 1, refmessage));

	/*
	 * Corrupt the branch reflog by introducing a newline inside the reflog message.
	 * We do this by replacing '*' with '\n'
	 */
	cl_git_pass(git_str_join_n(&logpath, '/', 3, git_repository_path(g_repo), GIT_REFLOG_DIR, refname));
	cl_git_pass(git_futils_readbuffer(&logcontents, git_str_cstr(&logpath)));
	cl_assert((star = strchr(git_str_cstr(&logcontents), '*')) != NULL);
	*star = '\n';
	cl_git_rewritefile(git_str_cstr(&logpath), git_str_cstr(&logcontents));

	/*
	 * Confirm that the file was rewritten successfully
	 * and now contains a '\n' in the expected location
	 */
	cl_git_pass(git_futils_readbuffer(&logcontents, git_str_cstr(&logpath)));
	cl_assert(strstr(git_str_cstr(&logcontents), "Reflog\nmessage") != NULL);

	cl_git_pass(git_reflog_read(&reflog, g_repo, refname));
	cl_assert(entry = git_reflog_entry_byindex(reflog, 0));
	cl_assert_equal_s(git_reflog_entry_message(entry), "Reflog");

	git_reference_free(ref);
	git_reflog_free(reflog);
	git_str_dispose(&logpath);
	git_str_dispose(&logcontents);
}

void test_refs_reflog_reflog__cannot_write_a_moved_reflog(void)
{
	git_reference *master, *new_master;
	git_str master_log_path = GIT_STR_INIT, moved_log_path = GIT_STR_INIT;
	git_reflog *reflog;

	cl_git_pass(git_reference_lookup(&master, g_repo, "refs/heads/master"));
	cl_git_pass(git_reflog_read(&reflog, g_repo, "refs/heads/master"));

	cl_git_pass(git_reflog_write(reflog));

	cl_git_pass(git_reference_rename(&new_master, master, "refs/moved", 0, NULL));
	git_reference_free(master);

	cl_git_fail(git_reflog_write(reflog));

	git_reflog_free(reflog);
	git_reference_free(new_master);
	git_str_dispose(&moved_log_path);
	git_str_dispose(&master_log_path);
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

	git_oid__fromstr(&id, current_master_tip, GIT_OID_SHA1);

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

	git_oid__fromstr(&id, current_master_tip, GIT_OID_SHA1);
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
	git_oid__fromstr(&id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644", GIT_OID_SHA1);
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
	git_oid__fromstr(&id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644", GIT_OID_SHA1);
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

void test_refs_reflog_reflog__logallrefupdates_bare_set_always(void)
{
	git_config *config;
	git_reference *ref;
	git_reflog *log;
	git_oid id;

	cl_git_pass(git_repository_config(&config, g_repo));
	cl_git_pass(git_config_set_string(config, "core.logallrefupdates", "always"));
	git_config_free(config);

	git_oid__fromstr(&id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644", GIT_OID_SHA1);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/bork", &id, 1, "message"));

	cl_git_pass(git_reflog_read(&log, g_repo, "refs/bork"));
	cl_assert_equal_i(1, git_reflog_entrycount(log));
	cl_assert_equal_s("message", git_reflog_entry_byindex(log, 0)->msg);

	git_reflog_free(log);
	git_reference_free(ref);
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
