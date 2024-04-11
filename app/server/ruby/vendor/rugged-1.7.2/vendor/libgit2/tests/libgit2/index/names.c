#include "clar_libgit2.h"
#include "index.h"
#include "git2/sys/index.h"
#include "git2/repository.h"
#include "../reset/reset_helpers.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH "mergedrepo"
#define TEST_INDEX_PATH TEST_REPO_PATH "/.git/index"

/* Fixture setup and teardown */
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

static void index_add_conflicts(void)
{
	git_index_entry entry = {{0}};
	const char *paths[][3] = {
		{ "ancestor", "ours", "theirs" },
		{ "ancestor2", "ours2", "theirs2" },
		{ "ancestor3", "ours3", "theirs3" } };
	const char **conflict;
	size_t i;

	for (i = 0; i < ARRAY_SIZE(paths); i++) {
		conflict = paths[i];

		/* ancestor */
		entry.path = conflict[0];
		entry.mode = GIT_FILEMODE_BLOB;
		GIT_INDEX_ENTRY_STAGE_SET(&entry, GIT_INDEX_STAGE_ANCESTOR);
		git_oid__fromstr(&entry.id, "1f85ca51b8e0aac893a621b61a9c2661d6aa6d81", GIT_OID_SHA1);
		cl_git_pass(git_index_add(repo_index, &entry));

		/* ours */
		entry.path = conflict[1];
		entry.mode = GIT_FILEMODE_BLOB;
		GIT_INDEX_ENTRY_STAGE_SET(&entry, GIT_INDEX_STAGE_OURS);
		git_oid__fromstr(&entry.id, "1f85ca51b8e0aac893a621b61a9c2661d6aa6d81", GIT_OID_SHA1);
		cl_git_pass(git_index_add(repo_index, &entry));

		/* theirs */
		entry.path = conflict[2];
		entry.mode = GIT_FILEMODE_BLOB;
		GIT_INDEX_ENTRY_STAGE_SET(&entry, GIT_INDEX_STAGE_THEIRS);
		git_oid__fromstr(&entry.id, "1f85ca51b8e0aac893a621b61a9c2661d6aa6d81", GIT_OID_SHA1);
		cl_git_pass(git_index_add(repo_index, &entry));
	}
}

void test_index_names__add(void)
{
	const git_index_name_entry *conflict_name;

	index_add_conflicts();
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

	cl_git_pass(git_index_write(repo_index));
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

	opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_UPDATE_ONLY;

	test_index_names__add();
	cl_git_pass(git_reference_name_to_id(&oid, repo, "refs/heads/master"));
	cl_git_pass(git_object_lookup(&obj, repo, &oid, GIT_OBJECT_ANY));
	cl_git_pass(git_checkout_tree(repo, obj, &opts));
	cl_assert_equal_sz(0, git_index_name_entrycount(repo_index));

	git_object_free(obj);
}

void test_index_names__cleaned_on_checkout_head(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_UPDATE_ONLY;

	test_index_names__add();
	cl_git_pass(git_checkout_head(repo, &opts));
	cl_assert_equal_sz(0, git_index_name_entrycount(repo_index));
}

void test_index_names__retained_on_checkout_index(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_UPDATE_ONLY;

	test_index_names__add();
	cl_git_pass(git_checkout_index(repo, repo_index, &opts));
	cl_assert(git_index_name_entrycount(repo_index) > 0);
}
