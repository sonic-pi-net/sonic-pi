#include "clar_libgit2.h"
#include "index.h"

static git_repository *g_repo = NULL;

void test_index_version__can_write_v4(void)
{
	git_index *index;
	const git_index_entry *entry;

	g_repo = cl_git_sandbox_init("filemodes");
	cl_git_pass(git_repository_index(&index, g_repo));

	cl_assert(index->on_disk);
	cl_assert(git_index_version(index) == 2);

	cl_assert(git_index_entrycount(index) == 6);

	cl_git_pass(git_index_set_version(index, 4));

	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_assert(git_index_version(index) == 4);

	entry = git_index_get_bypath(index, "exec_off", 0);
	cl_assert(entry);
	entry = git_index_get_bypath(index, "exec_off2on_staged", 0);
	cl_assert(entry);
	entry = git_index_get_bypath(index, "exec_on", 0);
	cl_assert(entry);

	git_index_free(index);
}

void test_index_version__cleanup(void)
{
        cl_git_sandbox_cleanup();
        g_repo = NULL;
}
