#include "clar_libgit2.h"
#include "fileops.h"
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

static void assert_object_oid(const char* revision, const char* expected_oid, git_otype type)
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
	assert_object_oid(revision, expected_oid, GIT_OBJ_BLOB);
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
	assert_blob_oid("refs/stash:just.ignore", NULL);

	assert_blob_oid("refs/stash^2:what", "dd7e1c6f0fefe118f0b63d9f10908c460aa317a6");	/* goodbye */
	assert_blob_oid("refs/stash^2:how", "e6d64adb2c7f3eb8feb493b556cc8070dca379a3");	/* not so small and */
	assert_blob_oid("refs/stash^2:who", "cc628ccd10742baea8241c5924df992b5c019f71");	/* world */
	assert_blob_oid("refs/stash^2:when", NULL);
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

	cl_git_pass(git_reference_symbolic_create(&head, repo, "HEAD", "refs/heads/unborn", 1, NULL, NULL));

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
	git_repository_detach_head(repo, NULL, NULL);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	assert_commit_message_contains("refs/stash^2", "index on (no branch): ");
	assert_commit_message_contains("refs/stash", "WIP on (no branch): ");
}

void test_stash_save__stashing_updates_the_reflog(void)
{
	char *sha;

	assert_object_oid("refs/stash@{0}", NULL, GIT_OBJ_COMMIT);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	sha = git_oid_allocfmt(&stash_tip_oid);

	assert_object_oid("refs/stash@{0}", sha, GIT_OBJ_COMMIT);
	assert_object_oid("refs/stash@{1}", NULL, GIT_OBJ_COMMIT);

	git__free(sha);
}

void test_stash_save__cannot_stash_when_there_are_no_local_change(void)
{
	git_index *index;
	git_oid stash_tip_oid;

	cl_git_pass(git_repository_index(&index, repo));

	/*
	 * 'what' and 'who' are being committed.
	 * 'when' remain untracked.
	 */
	cl_git_pass(git_index_add_bypath(index, "what"));
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

	assert_object_oid("stash@{1}^3", NULL, GIT_OBJ_COMMIT);

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

	assert_object_oid("stash^3^{tree}", EMPTY_TREE, GIT_OBJ_TREE);
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
