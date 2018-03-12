#include "clar_libgit2.h"

#include "repository.h"
#include "git2/reflog.h"
#include "reflog.h"

static git_repository *g_repo;



void test_refs_list__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_list__cleanup(void)
{
   cl_git_sandbox_cleanup();
}



void test_refs_list__all(void)
{
   // try to list all the references in our test repo
	git_strarray ref_list;

	cl_git_pass(git_reference_list(&ref_list, g_repo));

	/*{
		unsigned short i;
		for (i = 0; i < ref_list.count; ++i)
			printf("# %s\n", ref_list.strings[i]);
	}*/

	/* We have exactly 12 refs in total if we include the packed ones:
	 * there is a reference that exists both in the packfile and as
	 * loose, but we only list it once */
	cl_assert_equal_i((int)ref_list.count, 17);

	git_strarray_free(&ref_list);
}

void test_refs_list__do_not_retrieve_references_which_name_end_with_a_lock_extension(void)
{
	git_strarray ref_list;

	/* Create a fake locked reference */
	cl_git_mkfile(
		"./testrepo/.git/refs/heads/hanwen.lock",
		"144344043ba4d4a405da03de3844aa829ae8be0e\n");

	cl_git_pass(git_reference_list(&ref_list, g_repo));
	cl_assert_equal_i((int)ref_list.count, 17);

	git_strarray_free(&ref_list);
}
