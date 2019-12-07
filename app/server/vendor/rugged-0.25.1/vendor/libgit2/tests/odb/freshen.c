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

#define LOOSE_STR "hey\n"
#define LOOSE_ID  "1385f264afb75a56a5bec74243be9b367ba4ca08"
#define LOOSE_FN  "13/85f264afb75a56a5bec74243be9b367ba4ca08"

void test_odb_freshen__loose_object(void)
{
	git_oid expected_id, id;
	struct stat before, after;
	struct p_timeval old_times[2];

	cl_git_pass(git_oid_fromstr(&expected_id, LOOSE_ID));

	old_times[0].tv_sec = 1234567890;
	old_times[0].tv_usec = 0;
	old_times[1].tv_sec = 1234567890;
	old_times[1].tv_usec = 0;

	/* set time to way back */
	cl_must_pass(p_utimes("testrepo.git/objects/" LOOSE_FN, old_times));
	cl_must_pass(p_lstat("testrepo.git/objects/" LOOSE_FN, &before));

	cl_git_pass(git_odb_write(&id, odb, LOOSE_STR, CONST_STRLEN(LOOSE_STR),
		GIT_OBJ_BLOB));
	cl_assert_equal_oid(&expected_id, &id);
	cl_must_pass(p_lstat("testrepo.git/objects/" LOOSE_FN, &after));

	cl_assert(before.st_atime < after.st_atime);
	cl_assert(before.st_mtime < after.st_mtime);
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
		CONST_STRLEN(PACKED_STR), GIT_OBJ_BLOB));
	cl_assert_equal_oid(&expected_id, &id);
	cl_must_pass(p_lstat("testrepo.git/objects/pack/" PACKED_FN, &after));

	cl_assert(before.st_atime < after.st_atime);
	cl_assert(before.st_mtime < after.st_mtime);

	memcpy(&before, &after, sizeof(struct stat));

	/* ensure that the pack file is not freshened again immediately */
	cl_git_pass(git_odb_write(&id, odb, PACKED_STR,
		CONST_STRLEN(PACKED_STR), GIT_OBJ_BLOB));
	cl_assert_equal_oid(&expected_id, &id);
	cl_must_pass(p_lstat("testrepo.git/objects/pack/" PACKED_FN, &after));

	cl_assert(before.st_atime == after.st_atime);
	cl_assert(before.st_atime_nsec == after.st_atime_nsec);
	cl_assert(before.st_mtime == after.st_mtime);
	cl_assert(before.st_mtime_nsec == after.st_mtime_nsec);
}

