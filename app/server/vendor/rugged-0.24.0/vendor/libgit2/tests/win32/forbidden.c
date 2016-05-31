#include "clar_libgit2.h"

#include "repository.h"
#include "buffer.h"
#include "submodule.h"

static const char *repo_name = "win32-forbidden";
static git_repository *repo;

void test_win32_forbidden__initialize(void)
{
	repo = cl_git_sandbox_init(repo_name);
}

void test_win32_forbidden__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_win32_forbidden__can_open_index(void)
{
	git_index *index;
	cl_git_pass(git_repository_index(&index, repo));
	cl_assert_equal_i(7, git_index_entrycount(index));

	/* ensure we can even write the unmodified index */
	cl_git_pass(git_index_write(index));

	git_index_free(index);
}

void test_win32_forbidden__can_add_forbidden_filename_with_entry(void)
{
	git_index *index;
	git_index_entry entry = {{0}};

	cl_git_pass(git_repository_index(&index, repo));

	entry.path = "aux";
	entry.mode = GIT_FILEMODE_BLOB;
	git_oid_fromstr(&entry.id, "da623abd956bb2fd8052c708c7ed43f05d192d37");

	cl_git_pass(git_index_add(index, &entry));

	git_index_free(index);
}

void test_win32_forbidden__cannot_add_dot_git_even_with_entry(void)
{
	git_index *index;
	git_index_entry entry = {{0}};

	cl_git_pass(git_repository_index(&index, repo));

	entry.path = "foo/.git";
	entry.mode = GIT_FILEMODE_BLOB;
	git_oid_fromstr(&entry.id, "da623abd956bb2fd8052c708c7ed43f05d192d37");

	cl_git_fail(git_index_add(index, &entry));

	git_index_free(index);
}

void test_win32_forbidden__cannot_add_forbidden_filename_from_filesystem(void)
{
	git_index *index;

	/* since our function calls are very low-level, we can create `aux.`,
	 * but we should not be able to add it to the index
	 */
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_write2file("win32-forbidden/aux.", "foo\n", 4, O_RDWR | O_CREAT, 0666);

#ifdef GIT_WIN32
	cl_git_fail(git_index_add_bypath(index, "aux."));
#else
	cl_git_pass(git_index_add_bypath(index, "aux."));
#endif

	cl_must_pass(p_unlink("win32-forbidden/aux."));
	git_index_free(index);
}

static int dummy_submodule_cb(
	git_submodule *sm, const char *name, void *payload)
{
	GIT_UNUSED(sm);
	GIT_UNUSED(name);
	GIT_UNUSED(payload);
	return 0;
}

void test_win32_forbidden__can_diff_tree_to_index(void)
{
	git_diff *diff;
	git_tree *tree;

	cl_git_pass(git_repository_head_tree(&tree, repo));
	cl_git_pass(git_diff_tree_to_index(&diff, repo, tree, NULL, NULL));
	cl_assert_equal_i(0, git_diff_num_deltas(diff));
	git_diff_free(diff);
	git_tree_free(tree);
}

void test_win32_forbidden__can_diff_tree_to_tree(void)
{
	git_diff *diff;
	git_tree *tree;

	cl_git_pass(git_repository_head_tree(&tree, repo));
	cl_git_pass(git_diff_tree_to_tree(&diff, repo, tree, tree, NULL));
	cl_assert_equal_i(0, git_diff_num_deltas(diff));
	git_diff_free(diff);
	git_tree_free(tree);
}

void test_win32_forbidden__can_diff_index_to_workdir(void)
{
	git_index *index;
	git_diff *diff;
	const git_diff_delta *delta;
	git_tree *tree;
	size_t i;

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_repository_head_tree(&tree, repo));
	cl_git_pass(git_diff_index_to_workdir(&diff, repo, index, NULL));

	for (i = 0; i < git_diff_num_deltas(diff); i++) {
		delta = git_diff_get_delta(diff, i);
		cl_assert_equal_i(GIT_DELTA_DELETED, delta->status);
	}

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);
}

void test_win32_forbidden__checking_out_forbidden_index_fails(void)
{
#ifdef GIT_WIN32
	git_index *index;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_diff *diff;
	const git_diff_delta *delta;
	git_tree *tree;
	size_t num_deltas, i;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_fail(git_checkout_index(repo, index, &opts));

	cl_git_pass(git_repository_head_tree(&tree, repo));
	cl_git_pass(git_diff_index_to_workdir(&diff, repo, index, NULL));

	num_deltas = git_diff_num_deltas(diff);

	cl_assert(num_deltas > 0);

	for (i = 0; i < num_deltas; i++) {
		delta = git_diff_get_delta(diff, i);
		cl_assert_equal_i(GIT_DELTA_DELETED, delta->status);
	}

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);
#endif
}

void test_win32_forbidden__can_query_submodules(void)
{
	cl_git_pass(git_submodule_foreach(repo, dummy_submodule_cb, NULL));
}

void test_win32_forbidden__can_blame_file(void)
{
	git_blame *blame;

	cl_git_pass(git_blame_file(&blame, repo, "aux", NULL));
	git_blame_free(blame);
}
