#include "clar_libgit2.h"
#include "git2/sys/odb_backend.h"

typedef struct {
	git_odb_backend base;
	size_t position;
} fake_backend;

static git_odb_backend *new_backend(size_t position)
{
	fake_backend *b;

	b = git__calloc(1, sizeof(fake_backend));
	if (b == NULL)
		return NULL;

	b->base.version = GIT_ODB_BACKEND_VERSION;
	b->position = position;
	return (git_odb_backend *)b;
}

static void check_backend_sorting(git_odb *odb)
{
	size_t i, max_i = git_odb_num_backends(odb);
	fake_backend *internal;

	for (i = 0; i < max_i; ++i) {
		cl_git_pass(git_odb_get_backend((git_odb_backend **)&internal, odb, i));
		cl_assert(internal != NULL);
		cl_assert_equal_sz(i, internal->position);
	}
}

static git_odb *_odb;

void test_odb_sorting__initialize(void)
{
	cl_git_pass(git_odb_new(&_odb));
}

void test_odb_sorting__cleanup(void)
{
	git_odb_free(_odb);
	_odb = NULL;
}

void test_odb_sorting__basic_backends_sorting(void)
{
	cl_git_pass(git_odb_add_backend(_odb, new_backend(0), 5));
	cl_git_pass(git_odb_add_backend(_odb, new_backend(2), 3));
	cl_git_pass(git_odb_add_backend(_odb, new_backend(1), 4));
	cl_git_pass(git_odb_add_backend(_odb, new_backend(3), 1));

	check_backend_sorting(_odb);
}

void test_odb_sorting__alternate_backends_sorting(void)
{
	cl_git_pass(git_odb_add_backend(_odb, new_backend(0), 5));
	cl_git_pass(git_odb_add_backend(_odb, new_backend(2), 3));
	cl_git_pass(git_odb_add_backend(_odb, new_backend(1), 4));
	cl_git_pass(git_odb_add_backend(_odb, new_backend(3), 1));
	cl_git_pass(git_odb_add_alternate(_odb, new_backend(4), 5));
	cl_git_pass(git_odb_add_alternate(_odb, new_backend(6), 3));
	cl_git_pass(git_odb_add_alternate(_odb, new_backend(5), 4));
	cl_git_pass(git_odb_add_alternate(_odb, new_backend(7), 1));

	check_backend_sorting(_odb);
}
