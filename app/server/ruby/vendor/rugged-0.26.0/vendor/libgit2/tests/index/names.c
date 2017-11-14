#include "clar_libgit2.h"
#include "index.h"
#include "git2/sys/index.h"
#include "git2/repository.h"
#include "../reset/reset_helpers.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH "mergedrepo"
#define TEST_INDEX_PATH TEST_REPO_PATH "/.git/index"

// Fixture setup and teardown
void test_index_names__initialize(void)
{
	repo = cl_git_sandbox_init("mergedrepo");
	git_repository_index(&repo_index, repo);
}

void test_index_names__cleanup(void)
{
	git_index_free(repo_index);
	repo_index = NULL;

	cl_git_sandbox_cleanup();
}

void test_index_names__add(void)
{
	const git_index_name_entry *conflict_name;

	cl_git_pass(git_index_name_add(repo_index, "ancestor", "ours", "theirs"));
	cl_git_pass(git_index_name_add(repo_index, "ancestor2", "ours2", NULL));
	cl_git_pass(git_index_name_add(repo_index, "ancestor3", NULL, "theirs3"));

	cl_assert(git_index_name_entrycount(repo_index) == 3);

	conflict_name = git_index_name_get_byindex(repo_index, 0);
	cl_assert(strcmp(conflict_name->ancestor, "ancestor") == 0);
	cl_assert(strcmp(conflict_name->ours, "ours") == 0);
	cl_assert(strcmp(conflict_name->theirs, "theirs") == 0);

	conflict_name = git_index_name_get_byindex(repo_index, 1);
	cl_assert(strcmp(conflict_name->ancestor, "ancestor2") == 0);
	cl_assert(strcmp(conflict_name->ours, "ours2") == 0);
	cl_assert(conflict_name->theirs == NULL);

	conflict_name = git_index_name_get_byindex(repo_index, 2);
	cl_assert(strcmp(conflict_name->ancestor, "ancestor3") == 0);
	cl_assert(conflict_name->ours == NULL);
	cl_assert(strcmp(conflict_name->theirs, "theirs3") == 0);
}

void test_index_names__roundtrip(void)
{
	const git_index_name_entry *conflict_name;

	cl_git_pass(git_index_name_add(repo_index, "ancestor", "ours", "theirs"));
	cl_git_pass(git_index_name_add(repo_index, "ancestor2", "ours2", NULL));
	cl_git_pass(git_index_name_add(repo_index, "ancestor3", NULL, "theirs3"));

	cl_git_pass(git_index_write(repo_index));
	git_index_clear(repo_index);
	cl_assert(git_index_name_entrycount(repo_index) == 0);

	cl_git_pass(git_index_read(repo_index, true));
	cl_assert(git_index_name_entrycount(repo_index) == 3);

	conflict_name = git_index_name_get_byindex(repo_index, 0);
	cl_assert(strcmp(conflict_name->ancestor, "ancestor") == 0);
	cl_assert(strcmp(conflict_name->ours, "ours") == 0);
	cl_assert(strcmp(conflict_name->theirs, "theirs") == 0);

	conflict_name = git_index_name_get_byindex(repo_index, 1);
	cl_assert(strcmp(conflict_name->ancestor, "ancestor2") == 0);
	cl_assert(strcmp(conflict_name->ours, "ours2") == 0);
	cl_assert(conflict_name->theirs == NULL);

	conflict_name = git_index_name_get_byindex(repo_index, 2);
	cl_assert(strcmp(conflict_name->ancestor, "ancestor3") == 0);
	cl_assert(conflict_name->ours == NULL);
	cl_assert(strcmp(conflict_name->theirs, "theirs3") == 0);
}

void test_index_names__cleaned_on_reset_hard(void)
{
	git_object *target;

	cl_git_pass(git_revparse_single(&target, repo, "3a34580"));

	test_index_names__add();
	cl_git_pass(git_reset(repo, target, GIT_RESET_HARD, NULL));
	cl_assert(git_index_name_entrycount(repo_index) == 0);

	git_object_free(target);
}

void test_index_names__cleaned_on_reset_mixed(void)
{
	git_object *target;

	cl_git_pass(git_revparse_single(&target, repo, "3a34580"));

	test_index_names__add();
	cl_git_pass(git_reset(repo, target, GIT_RESET_MIXED, NULL));
	cl_assert(git_index_name_entrycount(repo_index) == 0);

	git_object_free(target);
}

void test_index_names__cleaned_on_checkout_tree(void)
{
	git_oid oid;
	git_object *obj;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_UPDATE_ONLY;

	test_index_names__add();
	git_reference_name_to_id(&oid, repo, "refs/heads/master");
	git_object_lookup(&obj, repo, &oid, GIT_OBJ_ANY);
	git_checkout_tree(repo, obj, &opts);
	cl_assert_equal_sz(0, git_index_name_entrycount(repo_index));

	git_object_free(obj);
}

void test_index_names__cleaned_on_checkout_head(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_UPDATE_ONLY;

	test_index_names__add();
	git_checkout_head(repo, &opts);
	cl_assert_equal_sz(0, git_index_name_entrycount(repo_index));
}

void test_index_names__retained_on_checkout_index(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_UPDATE_ONLY;

	test_index_names__add();
	git_checkout_index(repo, repo_index, &opts);
	cl_assert(git_index_name_entrycount(repo_index) > 0);
}
