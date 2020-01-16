#include "clar_libgit2.h"
#include <git2.h>
#include "strmap.h"
#include "mwindow.h"
#include "pack.h"

extern git_strmap *git__pack_cache;

void test_pack_sharing__open_two_repos(void)
{
	git_repository *repo1, *repo2;
	git_object *obj1, *obj2;
	git_oid id;
	size_t pos;
	void *data;
	int error;

	cl_git_pass(git_repository_open(&repo1, cl_fixture("testrepo.git")));
	cl_git_pass(git_repository_open(&repo2, cl_fixture("testrepo.git")));

	git_oid_fromstr(&id, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	cl_git_pass(git_object_lookup(&obj1, repo1, &id, GIT_OBJECT_ANY));
	cl_git_pass(git_object_lookup(&obj2, repo2, &id, GIT_OBJECT_ANY));

	pos = 0;
	while ((error = git_strmap_iterate(&data, git__pack_cache, &pos, NULL)) == 0) {
		struct git_pack_file *pack = (struct git_pack_file *) data;

		cl_assert_equal_i(2, pack->refcount.val);
	}

	cl_assert_equal_i(3, git_strmap_size(git__pack_cache));

	git_object_free(obj1);
	git_object_free(obj2);
	git_repository_free(repo1);
	git_repository_free(repo2);

	/* we don't want to keep the packs open after the repos go away */
	cl_assert_equal_i(0, git_strmap_size(git__pack_cache));
}
