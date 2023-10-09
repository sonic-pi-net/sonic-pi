#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "futils.h"

static git_repository *repo;

#define WORKDIR "empty_standard_repo"
#define BARE_REPO "testrepo.git"
#define ELSEWHERE "elsewhere"

typedef int (*blob_creator_fn)(
	git_oid *,
	git_repository *,
	const char *);

void test_object_blob_write__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void assert_blob_creation(const char *path_to_file, const char *blob_from_path, blob_creator_fn creator)
{
	git_oid oid;
	cl_git_mkfile(path_to_file, "1..2...3... Can you hear me?\n");

	cl_must_pass(creator(&oid, repo, blob_from_path));
	cl_assert(git_oid_streq(&oid, "da5e4f20c91c81b44a7e298f3d3fb3fe2f178e32") == 0);
}

void test_object_blob_write__can_create_a_blob_in_a_standard_repo_from_a_file_located_in_the_working_directory(void)
{
	repo = cl_git_sandbox_init(WORKDIR);

	assert_blob_creation(WORKDIR "/test.txt", "test.txt", &git_blob_create_from_workdir);
}

void test_object_blob_write__can_create_a_blob_in_a_standard_repo_from_a_absolute_filepath_pointing_outside_of_the_working_directory(void)
{
	git_str full_path = GIT_STR_INIT;

	repo = cl_git_sandbox_init(WORKDIR);

	cl_must_pass(p_mkdir(ELSEWHERE, 0777));
	cl_must_pass(git_fs_path_prettify_dir(&full_path, ELSEWHERE, NULL));
	cl_must_pass(git_str_puts(&full_path, "test.txt"));

	assert_blob_creation(ELSEWHERE "/test.txt", git_str_cstr(&full_path), &git_blob_create_from_disk);

	git_str_dispose(&full_path);
	cl_must_pass(git_futils_rmdir_r(ELSEWHERE, NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_object_blob_write__can_create_a_blob_in_a_bare_repo_from_a_absolute_filepath(void)
{
	git_str full_path = GIT_STR_INIT;

	repo = cl_git_sandbox_init(BARE_REPO);

	cl_must_pass(p_mkdir(ELSEWHERE, 0777));
	cl_must_pass(git_fs_path_prettify_dir(&full_path, ELSEWHERE, NULL));
	cl_must_pass(git_str_puts(&full_path, "test.txt"));

	assert_blob_creation(ELSEWHERE "/test.txt", git_str_cstr(&full_path), &git_blob_create_from_disk);

	git_str_dispose(&full_path);
	cl_must_pass(git_futils_rmdir_r(ELSEWHERE, NULL, GIT_RMDIR_REMOVE_FILES));
}
