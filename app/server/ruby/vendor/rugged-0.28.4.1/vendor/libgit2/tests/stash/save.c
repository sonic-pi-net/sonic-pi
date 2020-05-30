#include "clar_libgit2.h"
#include "futils.h"
#include "stash_helpers.h"

static git_repository *repo;
static git_signature *signature;
static git_oid stash_tip_oid;

/*
 * Friendly reminder, in order to ease the reading of the following tests:
 *
 * "stash"		points to the worktree commit
 * "stash^1"	points to the base commit (HEAD when the stash was created)
 * "stash^2"	points to the index commit
 * "stash^3"	points to the untracked commit
 */

void test_stash_save__initialize(void)
{
	cl_git_pass(git_repository_init(&repo, "stash", 0));
	cl_git_pass(git_signature_new(&signature, "nulltoken", "emeric.fermas@gmail.com", 1323847743, 60)); /* Wed Dec 14 08:29:03 2011 +0100 */

	setup_stash(repo, signature);
}

void test_stash_save__cleanup(void)
{
	git_signature_free(signature);
	signature = NULL;

	git_repository_free(repo);
	repo = NULL;

	cl_git_pass(git_futils_rmdir_r("stash", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_fixture_cleanup("sorry-it-is-a-non-bare-only-party");
}

static void assert_object_oid(const char* revision, const char* expected_oid, git_object_t type)
{
	int result;
	git_object *obj;

	result = git_revparse_single(&obj, repo, revision);

	if (!expected_oid) {
		cl_assert_equal_i(GIT_ENOTFOUND, result);
		return;
	} else
		cl_assert_equal_i(0, result);

	cl_git_pass(git_oid_streq(git_object_id(obj), expected_oid));
	cl_assert_equal_i(type, git_object_type(obj));
	git_object_free(obj);
}

static void assert_blob_oid(const char* revision, const char* expected_oid)
{
	assert_object_oid(revision, expected_oid, GIT_OBJECT_BLOB);
}

void test_stash_save__does_not_keep_index_by_default(void)
{
/*
$ git stash

$ git show refs/stash:what
see you later

$ git show refs/stash:how
not so small and

$ git show refs/stash:who
funky world

$ git show refs/stash:when
fatal: Path 'when' exists on disk, but not in 'stash'.

$ git show refs/stash^2:what
goodbye

$ git show refs/stash^2:how
not so small and

$ git show refs/stash^2:who
world

$ git show refs/stash^2:when
fatal: Path 'when' exists on disk, but not in 'stash^2'.

$ git status --short
?? when

*/
	unsigned int status;

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));
	cl_git_pass(git_status_file(&status, repo, "when"));

	assert_blob_oid("refs/stash:what", "bc99dc98b3eba0e9157e94769cd4d49cb49de449");	/* see you later */
	assert_blob_oid("refs/stash:how", "e6d64adb2c7f3eb8feb493b556cc8070dca379a3");	/* not so small and */
	assert_blob_oid("refs/stash:who", "a0400d4954659306a976567af43125a0b1aa8595");	/* funky world */
	assert_blob_oid("refs/stash:when", NULL);
	assert_blob_oid("refs/stash:why", "88c2533e21f098b89c91a431d8075cbdbe422a51"); /* would anybody use stash? */
	assert_blob_oid("refs/stash:where", "e3d6434ec12eb76af8dfa843a64ba6ab91014a0b"); /* .... */
	assert_blob_oid("refs/stash:.gitignore", "ac4d88de61733173d9959e4b77c69b9f17a00980");
	assert_blob_oid("refs/stash:just.ignore", NULL);

	assert_blob_oid("refs/stash^2:what", "dd7e1c6f0fefe118f0b63d9f10908c460aa317a6");	/* goodbye */
	assert_blob_oid("refs/stash^2:how", "e6d64adb2c7f3eb8feb493b556cc8070dca379a3");	/* not so small and */
	assert_blob_oid("refs/stash^2:who", "cc628ccd10742baea8241c5924df992b5c019f71");	/* world */
	assert_blob_oid("refs/stash^2:when", NULL);
	assert_blob_oid("refs/stash^2:why", "88c2533e21f098b89c91a431d8075cbdbe422a51"); /* would anybody use stash? */
	assert_blob_oid("refs/stash^2:where", "e08f7fbb9a42a0c5367cf8b349f1f08c3d56bd72"); /* ???? */
	assert_blob_oid("refs/stash^2:.gitignore", "ac4d88de61733173d9959e4b77c69b9f17a00980");
	assert_blob_oid("refs/stash^2:just.ignore", NULL);

	assert_blob_oid("refs/stash^3", NULL);

	cl_assert_equal_i(GIT_STATUS_WT_NEW, status);
}

void test_stash_save__can_keep_index(void)
{
	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_KEEP_INDEX));

	assert_status(repo, "what", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "just.ignore", GIT_STATUS_IGNORED);
}

static void assert_commit_message_contains(const char *revision, const char *fragment)
{
	git_commit *commit;

	cl_git_pass(git_revparse_single((git_object**)&commit, repo, revision));

	cl_assert(strstr(git_commit_message(commit), fragment) != NULL);

	git_commit_free(commit);
}

void test_stash_save__can_include_untracked_files(void)
{
	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));

	assert_commit_message_contains("refs/stash^3", "untracked files on master: ");

	assert_blob_oid("refs/stash^3:what", NULL);
	assert_blob_oid("refs/stash^3:how", NULL);
	assert_blob_oid("refs/stash^3:who", NULL);
	assert_blob_oid("refs/stash^3:when", "b6ed15e81e2593d7bb6265eb4a991d29dc3e628b");
	assert_blob_oid("refs/stash^3:just.ignore", NULL);
}

void test_stash_save__untracked_skips_ignored(void)
{
	cl_git_append2file("stash/.gitignore", "bundle/vendor/\n");
	cl_must_pass(p_mkdir("stash/bundle", 0777));
	cl_must_pass(p_mkdir("stash/bundle/vendor", 0777));
	cl_git_mkfile("stash/bundle/vendor/blah", "contents\n");

	cl_assert(git_path_exists("stash/when")); /* untracked */
	cl_assert(git_path_exists("stash/just.ignore")); /* ignored */
	cl_assert(git_path_exists("stash/bundle/vendor/blah")); /* ignored */

	cl_git_pass(git_stash_save(
		&stash_tip_oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));

	cl_assert(!git_path_exists("stash/when"));
	cl_assert(git_path_exists("stash/bundle/vendor/blah"));
	cl_assert(git_path_exists("stash/just.ignore"));
}

void test_stash_save__can_include_untracked_and_ignored_files(void)
{
	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED | GIT_STASH_INCLUDE_IGNORED));

	assert_commit_message_contains("refs/stash^3", "untracked files on master: ");

	assert_blob_oid("refs/stash^3:what", NULL);
	assert_blob_oid("refs/stash^3:how", NULL);
	assert_blob_oid("refs/stash^3:who", NULL);
	assert_blob_oid("refs/stash^3:when", "b6ed15e81e2593d7bb6265eb4a991d29dc3e628b");
	assert_blob_oid("refs/stash^3:just.ignore", "78925fb1236b98b37a35e9723033e627f97aa88b");

	cl_assert(!git_path_exists("stash/just.ignore"));
}

/*
 * Note: this test was flaky prior to fixing #4101 -- run it several
 * times to get a failure.  The issues is that whether the fast
 * (stat-only) codepath is used inside stash's diff operation depends
 * on whether files are "racily clean", and there doesn't seem to be
 * an easy way to force the exact required state.
 */
void test_stash_save__untracked_regression(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	const char *paths[] = {"what", "where", "how", "why"};
	git_reference *head;
	git_commit *head_commit;
	git_buf untracked_dir;

	const char* workdir = git_repository_workdir(repo);

	git_buf_init(&untracked_dir, 0);
	git_buf_printf(&untracked_dir, "%sz", workdir);

	cl_assert(!p_mkdir(untracked_dir.ptr, 0777));

	cl_git_pass(git_repository_head(&head, repo));

	cl_git_pass(git_reference_peel((git_object **)&head_commit, head, GIT_OBJECT_COMMIT));

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	opts.paths.strings = (char **)paths;
	opts.paths.count = 4;

	cl_git_pass(git_checkout_tree(repo, (git_object*)head_commit, &opts));

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	assert_commit_message_contains("refs/stash", "WIP on master");

	git_reference_free(head);
	git_commit_free(head_commit);
	git_buf_dispose(&untracked_dir);
}

#define MESSAGE "Look Ma! I'm on TV!"
void test_stash_save__can_accept_a_message(void)
{
	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, MESSAGE, GIT_STASH_DEFAULT));

	assert_commit_message_contains("refs/stash^2", "index on master: ");
	assert_commit_message_contains("refs/stash", "On master: " MESSAGE);
}

void test_stash_save__cannot_stash_against_an_unborn_branch(void)
{
	git_reference *head;

	cl_git_pass(git_reference_symbolic_create(&head, repo, "HEAD", "refs/heads/unborn", 1, NULL));

	cl_assert_equal_i(GIT_EUNBORNBRANCH,
		git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	git_reference_free(head);
}

void test_stash_save__cannot_stash_against_a_bare_repository(void)
{
	git_repository *local;

	cl_git_pass(git_repository_init(&local, "sorry-it-is-a-non-bare-only-party", 1));

	cl_assert_equal_i(GIT_EBAREREPO,
		git_stash_save(&stash_tip_oid, local, signature, NULL, GIT_STASH_DEFAULT));

	git_repository_free(local);
}

void test_stash_save__can_stash_against_a_detached_head(void)
{
	git_repository_detach_head(repo);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	assert_commit_message_contains("refs/stash^2", "index on (no branch): ");
	assert_commit_message_contains("refs/stash", "WIP on (no branch): ");
}

void test_stash_save__stashing_updates_the_reflog(void)
{
	assert_object_oid("refs/stash@{0}", NULL, GIT_OBJECT_COMMIT);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	assert_object_oid("refs/stash@{0}", git_oid_tostr_s(&stash_tip_oid), GIT_OBJECT_COMMIT);
	assert_object_oid("refs/stash@{1}", NULL, GIT_OBJECT_COMMIT);
}

void test_stash_save__multiline_message(void)
{
	const char *msg = "This\n\nis a multiline message\n";
	const git_reflog_entry *entry;
	git_reflog *reflog;

	assert_object_oid("refs/stash@{0}", NULL, GIT_OBJECT_COMMIT);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, msg, GIT_STASH_DEFAULT));

	cl_git_pass(git_reflog_read(&reflog, repo, "refs/stash"));
	cl_assert(entry = git_reflog_entry_byindex(reflog, 0));
	cl_assert_equal_s(git_reflog_entry_message(entry), "On master: This  is a multiline message");

	assert_object_oid("refs/stash@{0}", git_oid_tostr_s(&stash_tip_oid), GIT_OBJECT_COMMIT);
	assert_commit_message_contains("refs/stash@{0}", msg);

	git_reflog_free(reflog);
}

void test_stash_save__cannot_stash_when_there_are_no_local_change(void)
{
	git_index *index;
	git_oid stash_tip_oid;

	cl_git_pass(git_repository_index(&index, repo));

	/*
	 * 'what', 'where' and 'who' are being committed.
	 * 'when' remains untracked.
	 */
	cl_git_pass(git_index_add_bypath(index, "what"));
	cl_git_pass(git_index_add_bypath(index, "where"));
	cl_git_pass(git_index_add_bypath(index, "who"));

	cl_repo_commit_from_index(NULL, repo, signature, 0, "Initial commit");
	git_index_free(index);

	cl_assert_equal_i(GIT_ENOTFOUND,
		git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	p_unlink("stash/when");
	cl_assert_equal_i(GIT_ENOTFOUND,
		git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));
}

void test_stash_save__can_stage_normal_then_stage_untracked(void)
{
	/*
	 * $ git ls-tree stash@{1}^0
	 * 100644 blob ac4d88de61733173d9959e4b77c69b9f17a00980    .gitignore
	 * 100644 blob e6d64adb2c7f3eb8feb493b556cc8070dca379a3    how
	 * 100644 blob bc99dc98b3eba0e9157e94769cd4d49cb49de449    what
	 * 100644 blob a0400d4954659306a976567af43125a0b1aa8595    who
	 *
	 * $ git ls-tree stash@{1}^1
	 * 100644 blob ac4d88de61733173d9959e4b77c69b9f17a00980    .gitignore
	 * 100644 blob ac790413e2d7a26c3767e78c57bb28716686eebc    how
	 * 100644 blob ce013625030ba8dba906f756967f9e9ca394464a    what
	 * 100644 blob cc628ccd10742baea8241c5924df992b5c019f71    who
	 *
	 * $ git ls-tree stash@{1}^2
	 * 100644 blob ac4d88de61733173d9959e4b77c69b9f17a00980    .gitignore
	 * 100644 blob e6d64adb2c7f3eb8feb493b556cc8070dca379a3    how
	 * 100644 blob dd7e1c6f0fefe118f0b63d9f10908c460aa317a6    what
	 * 100644 blob cc628ccd10742baea8241c5924df992b5c019f71    who
	 *
	 * $ git ls-tree stash@{1}^3
	 * fatal: Not a valid object name stash@{1}^3
	 *
	 * $ git ls-tree stash@{0}^0
	 * 100644 blob ac4d88de61733173d9959e4b77c69b9f17a00980    .gitignore
	 * 100644 blob ac790413e2d7a26c3767e78c57bb28716686eebc    how
	 * 100644 blob ce013625030ba8dba906f756967f9e9ca394464a    what
	 * 100644 blob cc628ccd10742baea8241c5924df992b5c019f71    who
	 *
	 * $ git ls-tree stash@{0}^1
	 * 100644 blob ac4d88de61733173d9959e4b77c69b9f17a00980    .gitignore
	 * 100644 blob ac790413e2d7a26c3767e78c57bb28716686eebc    how
	 * 100644 blob ce013625030ba8dba906f756967f9e9ca394464a    what
	 * 100644 blob cc628ccd10742baea8241c5924df992b5c019f71    who
	 *
	 * $ git ls-tree stash@{0}^2
	 * 100644 blob ac4d88de61733173d9959e4b77c69b9f17a00980    .gitignore
	 * 100644 blob ac790413e2d7a26c3767e78c57bb28716686eebc    how
	 * 100644 blob ce013625030ba8dba906f756967f9e9ca394464a    what
	 * 100644 blob cc628ccd10742baea8241c5924df992b5c019f71    who
	 *
	 * $ git ls-tree stash@{0}^3
	 * 100644 blob b6ed15e81e2593d7bb6265eb4a991d29dc3e628b    when
	*/

	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "who", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "just.ignore", GIT_STATUS_IGNORED);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));
	assert_status(repo, "what", GIT_STATUS_CURRENT);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "just.ignore", GIT_STATUS_IGNORED);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));
	assert_status(repo, "what", GIT_STATUS_CURRENT);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_ENOTFOUND);
	assert_status(repo, "just.ignore", GIT_STATUS_IGNORED);


	assert_blob_oid("stash@{1}^0:what", "bc99dc98b3eba0e9157e94769cd4d49cb49de449");	/* see you later */
	assert_blob_oid("stash@{1}^0:how", "e6d64adb2c7f3eb8feb493b556cc8070dca379a3");		/* not so small and */
	assert_blob_oid("stash@{1}^0:who", "a0400d4954659306a976567af43125a0b1aa8595");		/* funky world */
	assert_blob_oid("stash@{1}^0:when", NULL);

	assert_blob_oid("stash@{1}^2:what", "dd7e1c6f0fefe118f0b63d9f10908c460aa317a6");	/* goodbye */
	assert_blob_oid("stash@{1}^2:how", "e6d64adb2c7f3eb8feb493b556cc8070dca379a3");		/* not so small and */
	assert_blob_oid("stash@{1}^2:who", "cc628ccd10742baea8241c5924df992b5c019f71");		/* world */
	assert_blob_oid("stash@{1}^2:when", NULL);

	assert_object_oid("stash@{1}^3", NULL, GIT_OBJECT_COMMIT);

	assert_blob_oid("stash@{0}^0:what", "ce013625030ba8dba906f756967f9e9ca394464a");	/* hello */
	assert_blob_oid("stash@{0}^0:how", "ac790413e2d7a26c3767e78c57bb28716686eebc");		/* small */
	assert_blob_oid("stash@{0}^0:who", "cc628ccd10742baea8241c5924df992b5c019f71");		/* world */
	assert_blob_oid("stash@{0}^0:when", NULL);

	assert_blob_oid("stash@{0}^2:what", "ce013625030ba8dba906f756967f9e9ca394464a");	/* hello */
	assert_blob_oid("stash@{0}^2:how", "ac790413e2d7a26c3767e78c57bb28716686eebc");		/* small */
	assert_blob_oid("stash@{0}^2:who", "cc628ccd10742baea8241c5924df992b5c019f71");		/* world */
	assert_blob_oid("stash@{0}^2:when", NULL);

	assert_blob_oid("stash@{0}^3:when", "b6ed15e81e2593d7bb6265eb4a991d29dc3e628b");	/* now */
}

#define EMPTY_TREE "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

void test_stash_save__including_untracked_without_any_untracked_file_creates_an_empty_tree(void)
{
	cl_must_pass(p_unlink("stash/when"));

	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "who", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "when", GIT_ENOTFOUND);
	assert_status(repo, "just.ignore", GIT_STATUS_IGNORED);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));

	assert_object_oid("stash^3^{tree}", EMPTY_TREE, GIT_OBJECT_TREE);
}

void test_stash_save__ignored_directory(void)
{
	cl_git_pass(p_mkdir("stash/ignored_directory", 0777));
	cl_git_pass(p_mkdir("stash/ignored_directory/sub", 0777));
	cl_git_mkfile("stash/ignored_directory/sub/some_file", "stuff");

	assert_status(repo, "ignored_directory/sub/some_file", GIT_STATUS_WT_NEW);
	cl_git_pass(git_ignore_add_rule(repo, "ignored_directory/"));
	assert_status(repo, "ignored_directory/sub/some_file", GIT_STATUS_IGNORED);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED | GIT_STASH_INCLUDE_IGNORED));

	cl_assert(!git_path_exists("stash/ignored_directory/sub/some_file"));
	cl_assert(!git_path_exists("stash/ignored_directory/sub"));
	cl_assert(!git_path_exists("stash/ignored_directory"));
}

void test_stash_save__skip_submodules(void)
{
	git_repository *untracked_repo;
	cl_git_pass(git_repository_init(&untracked_repo, "stash/untracked_repo", false));
	cl_git_mkfile("stash/untracked_repo/content", "stuff");
	git_repository_free(untracked_repo);

	assert_status(repo, "untracked_repo/", GIT_STATUS_WT_NEW);

	cl_git_pass(git_stash_save(
		&stash_tip_oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));

	assert_status(repo, "untracked_repo/", GIT_STATUS_WT_NEW);
}

void test_stash_save__deleted_in_index_modified_in_workdir(void)
{
	git_index *index;

	git_repository_index(&index, repo);

	cl_git_pass(git_index_remove_bypath(index, "who"));
	cl_git_pass(git_index_write(index));

	assert_status(repo, "who", GIT_STATUS_WT_NEW | GIT_STATUS_INDEX_DELETED);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	assert_blob_oid("stash@{0}^0:who", "a0400d4954659306a976567af43125a0b1aa8595");
	assert_blob_oid("stash@{0}^2:who", NULL);

	git_index_free(index);
}
