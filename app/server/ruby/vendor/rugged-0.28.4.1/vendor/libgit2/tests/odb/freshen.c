#include "clar_libgit2.h"
#include "odb.h"
#include "posix.h"

static git_repository *repo;
static git_odb *odb;

void test_odb_freshen__initialize(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_repository_odb(&odb, repo));
}

void test_odb_freshen__cleanup(void)
{
	git_odb_free(odb);
	cl_git_sandbox_cleanup();
}

static void set_time_wayback(struct stat *out, const char *fn)
{
	git_buf fullpath = GIT_BUF_INIT;
	struct p_timeval old[2];

	old[0].tv_sec = 1234567890;
	old[0].tv_usec = 0;
	old[1].tv_sec = 1234567890;
	old[1].tv_usec = 0;

	git_buf_joinpath(&fullpath, "testrepo.git/objects", fn);

	cl_must_pass(p_utimes(git_buf_cstr(&fullpath), old));
	cl_must_pass(p_lstat(git_buf_cstr(&fullpath), out));
	git_buf_dispose(&fullpath);
}

#define LOOSE_STR     "my new file\n"
#define LOOSE_BLOB_ID "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"
#define LOOSE_BLOB_FN "a7/1586c1dfe8a71c6cbf6c129f404c5642ff31bd"

void test_odb_freshen__loose_blob(void)
{
	git_oid expected_id, id;
	struct stat before, after;

	cl_git_pass(git_oid_fromstr(&expected_id, LOOSE_BLOB_ID));
	set_time_wayback(&before, LOOSE_BLOB_FN);

	/* make sure we freshen a blob */
	cl_git_pass(git_blob_create_from_buffer(&id, repo, LOOSE_STR, CONST_STRLEN(LOOSE_STR)));
	cl_assert_equal_oid(&expected_id, &id);
	cl_must_pass(p_lstat("testrepo.git/objects/" LOOSE_BLOB_FN, &after));

	cl_assert(before.st_atime < after.st_atime);
	cl_assert(before.st_mtime < after.st_mtime);
}

#define UNIQUE_STR     "doesnt exist in the odb yet\n"
#define UNIQUE_BLOB_ID "78a87d0b8878c5953b9a63015ff4e22a3d898826"
#define UNIQUE_BLOB_FN "78/a87d0b8878c5953b9a63015ff4e22a3d898826"

void test_odb_freshen__readonly_object(void)
{
	git_oid expected_id, id;
	struct stat before, after;

	cl_git_pass(git_oid_fromstr(&expected_id, UNIQUE_BLOB_ID));

	cl_git_pass(git_blob_create_from_buffer(&id, repo, UNIQUE_STR, CONST_STRLEN(UNIQUE_STR)));
	cl_assert_equal_oid(&expected_id, &id);

	set_time_wayback(&before, UNIQUE_BLOB_FN);
	cl_assert((before.st_mode & S_IWUSR) == 0);

	cl_git_pass(git_blob_create_from_buffer(&id, repo, UNIQUE_STR, CONST_STRLEN(UNIQUE_STR)));
	cl_assert_equal_oid(&expected_id, &id);
	cl_must_pass(p_lstat("testrepo.git/objects/" UNIQUE_BLOB_FN, &after));

	cl_assert(before.st_atime < after.st_atime);
	cl_assert(before.st_mtime < after.st_mtime);
}

#define LOOSE_TREE_ID "944c0f6e4dfa41595e6eb3ceecdb14f50fe18162"
#define LOOSE_TREE_FN "94/4c0f6e4dfa41595e6eb3ceecdb14f50fe18162"

void test_odb_freshen__loose_tree(void)
{
	git_oid expected_id, id;
	git_tree *tree;
	struct stat before, after;

	cl_git_pass(git_oid_fromstr(&expected_id, LOOSE_TREE_ID));
	set_time_wayback(&before, LOOSE_TREE_FN);

	cl_git_pass(git_tree_lookup(&tree, repo, &expected_id));
	cl_git_pass(git_tree_create_updated(&id, repo, tree, 0, NULL));

	/* make sure we freshen a tree */
	cl_assert_equal_oid(&expected_id, &id);
	cl_must_pass(p_lstat("testrepo.git/objects/" LOOSE_TREE_FN, &after));

	cl_assert(before.st_atime < after.st_atime);
	cl_assert(before.st_mtime < after.st_mtime);

	git_tree_free(tree);
}

void test_odb_freshen__tree_during_commit(void)
{
	git_oid tree_id, parent_id, commit_id;
	git_tree *tree;
	git_commit *parent;
	git_signature *signature;
	struct stat before, after;

	cl_git_pass(git_oid_fromstr(&tree_id, LOOSE_TREE_ID));
	cl_git_pass(git_tree_lookup(&tree, repo, &tree_id));
	set_time_wayback(&before, LOOSE_TREE_FN);

	cl_git_pass(git_oid_fromstr(&parent_id, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750"));
	cl_git_pass(git_commit_lookup(&parent, repo, &parent_id));

	cl_git_pass(git_signature_new(&signature,
		"Refresher", "refresher@example.com", 1488547083, 0));

	cl_git_pass(git_commit_create(&commit_id, repo, NULL,
		signature, signature, NULL, "New commit pointing to old tree",
		tree, 1, (const git_commit **)&parent));

	/* make sure we freshen the tree the commit points to */
	cl_must_pass(p_lstat("testrepo.git/objects/" LOOSE_TREE_FN, &after));
	cl_assert(before.st_atime < after.st_atime);
	cl_assert(before.st_mtime < after.st_mtime);

	git_signature_free(signature);
	git_commit_free(parent);
	git_tree_free(tree);
}

#define PACKED_STR "Testing a readme.txt\n"
#define PACKED_ID  "6336846bd5c88d32f93ae57d846683e61ab5c530"
#define PACKED_FN  "pack-d85f5d483273108c9d8dd0e4728ccf0b2982423a.pack"

void test_odb_freshen__packed_object(void)
{
	git_oid expected_id, id;
	struct stat before, after;
	struct p_timeval old_times[2];

	cl_git_pass(git_oid_fromstr(&expected_id, PACKED_ID));

	old_times[0].tv_sec = 1234567890;
	old_times[0].tv_usec = 0;
	old_times[1].tv_sec = 1234567890;
	old_times[1].tv_usec = 0;

	/* set time to way back */
	cl_must_pass(p_utimes("testrepo.git/objects/pack/" PACKED_FN, old_times));
	cl_must_pass(p_lstat("testrepo.git/objects/pack/" PACKED_FN, &before));

	/* ensure that packfile is freshened */
	cl_git_pass(git_odb_write(&id, odb, PACKED_STR,
		CONST_STRLEN(PACKED_STR), GIT_OBJECT_BLOB));
	cl_assert_equal_oid(&expected_id, &id);
	cl_must_pass(p_lstat("testrepo.git/objects/pack/" PACKED_FN, &after));

	cl_assert(before.st_atime < after.st_atime);
	cl_assert(before.st_mtime < after.st_mtime);

	memcpy(&before, &after, sizeof(struct stat));

	/* ensure that the pack file is not freshened again immediately */
	cl_git_pass(git_odb_write(&id, odb, PACKED_STR,
		CONST_STRLEN(PACKED_STR), GIT_OBJECT_BLOB));
	cl_assert_equal_oid(&expected_id, &id);
	cl_must_pass(p_lstat("testrepo.git/objects/pack/" PACKED_FN, &after));

	cl_assert(before.st_atime == after.st_atime);
	cl_assert(before.st_atime_nsec == after.st_atime_nsec);
	cl_assert(before.st_mtime == after.st_mtime);
	cl_assert(before.st_mtime_nsec == after.st_mtime_nsec);
}

