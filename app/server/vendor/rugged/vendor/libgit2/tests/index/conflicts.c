#include "clar_libgit2.h"
#include "index.h"
#include "git2/repository.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH "mergedrepo"
#define TEST_INDEX_PATH TEST_REPO_PATH "/.git/index"

#define CONFLICTS_ONE_ANCESTOR_OID "1f85ca51b8e0aac893a621b61a9c2661d6aa6d81"
#define CONFLICTS_ONE_OUR_OID "6aea5f295304c36144ad6e9247a291b7f8112399"
#define CONFLICTS_ONE_THEIR_OID "516bd85f78061e09ccc714561d7b504672cb52da"

#define CONFLICTS_TWO_ANCESTOR_OID "84af62840be1b1c47b778a8a249f3ff45155038c"
#define CONFLICTS_TWO_OUR_OID "8b3f43d2402825c200f835ca1762413e386fd0b2"
#define CONFLICTS_TWO_THEIR_OID "220bd62631c8cf7a83ef39c6b94595f00517211e"

#define TEST_ANCESTOR_OID "f00ff00ff00ff00ff00ff00ff00ff00ff00ff00f"
#define TEST_OUR_OID "b44bb44bb44bb44bb44bb44bb44bb44bb44bb44b"
#define TEST_THEIR_OID "0123456789abcdef0123456789abcdef01234567"

// Fixture setup and teardown
void test_index_conflicts__initialize(void)
{
	repo = cl_git_sandbox_init("mergedrepo");
	git_repository_index(&repo_index, repo);
}

void test_index_conflicts__cleanup(void)
{
	git_index_free(repo_index);
	repo_index = NULL;

	cl_git_sandbox_cleanup();
}

void test_index_conflicts__add(void)
{
	git_index_entry ancestor_entry, our_entry, their_entry;

	cl_assert(git_index_entrycount(repo_index) == 8);

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	ancestor_entry.path = "test-one.txt";
	ancestor_entry.flags |= (1 << GIT_IDXENTRY_STAGESHIFT);
	git_oid_fromstr(&ancestor_entry.id, TEST_ANCESTOR_OID);

	our_entry.path = "test-one.txt";
	ancestor_entry.flags |= (2 << GIT_IDXENTRY_STAGESHIFT);
	git_oid_fromstr(&our_entry.id, TEST_OUR_OID);

	their_entry.path = "test-one.txt";
	ancestor_entry.flags |= (3 << GIT_IDXENTRY_STAGESHIFT);
	git_oid_fromstr(&their_entry.id, TEST_THEIR_OID);

	cl_git_pass(git_index_conflict_add(repo_index, &ancestor_entry, &our_entry, &their_entry));

	cl_assert(git_index_entrycount(repo_index) == 11);
}

void test_index_conflicts__add_fixes_incorrect_stage(void)
{
	git_index_entry ancestor_entry, our_entry, their_entry;
	const git_index_entry *conflict_entry[3];

	cl_assert(git_index_entrycount(repo_index) == 8);

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	ancestor_entry.path = "test-one.txt";
	ancestor_entry.flags |= (3 << GIT_IDXENTRY_STAGESHIFT);
	git_oid_fromstr(&ancestor_entry.id, TEST_ANCESTOR_OID);

	our_entry.path = "test-one.txt";
	ancestor_entry.flags |= (1 << GIT_IDXENTRY_STAGESHIFT);
	git_oid_fromstr(&our_entry.id, TEST_OUR_OID);

	their_entry.path = "test-one.txt";
	ancestor_entry.flags |= (2 << GIT_IDXENTRY_STAGESHIFT);
	git_oid_fromstr(&their_entry.id, TEST_THEIR_OID);

	cl_git_pass(git_index_conflict_add(repo_index, &ancestor_entry, &our_entry, &their_entry));

	cl_assert(git_index_entrycount(repo_index) == 11);

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1], &conflict_entry[2], repo_index, "test-one.txt"));

	cl_assert(git_index_entry_stage(conflict_entry[0]) == 1);
	cl_assert(git_index_entry_stage(conflict_entry[1]) == 2);
	cl_assert(git_index_entry_stage(conflict_entry[2]) == 3);
}

void test_index_conflicts__get(void)
{
	const git_index_entry *conflict_entry[3];
	git_oid oid;

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], repo_index, "conflicts-one.txt"));

	cl_assert_equal_s("conflicts-one.txt", conflict_entry[0]->path);

	git_oid_fromstr(&oid, CONFLICTS_ONE_ANCESTOR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);

	git_oid_fromstr(&oid, CONFLICTS_ONE_OUR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);

	git_oid_fromstr(&oid, CONFLICTS_ONE_THEIR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], repo_index, "conflicts-two.txt"));

	cl_assert_equal_s("conflicts-two.txt", conflict_entry[0]->path);

	git_oid_fromstr(&oid, CONFLICTS_TWO_ANCESTOR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);

	git_oid_fromstr(&oid, CONFLICTS_TWO_OUR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);

	git_oid_fromstr(&oid, CONFLICTS_TWO_THEIR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);
}

void test_index_conflicts__iterate(void)
{
	git_index_conflict_iterator *iterator;
	const git_index_entry *conflict_entry[3];
	git_oid oid;

	cl_git_pass(git_index_conflict_iterator_new(&iterator, repo_index));

	cl_git_pass(git_index_conflict_next(&conflict_entry[0], &conflict_entry[1], &conflict_entry[2], iterator));

	git_oid_fromstr(&oid, CONFLICTS_ONE_ANCESTOR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-one.txt") == 0);

	git_oid_fromstr(&oid, CONFLICTS_ONE_OUR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-one.txt") == 0);

	git_oid_fromstr(&oid, CONFLICTS_ONE_THEIR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-one.txt") == 0);

	cl_git_pass(git_index_conflict_next(&conflict_entry[0], &conflict_entry[1], &conflict_entry[2], iterator));

	git_oid_fromstr(&oid, CONFLICTS_TWO_ANCESTOR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-two.txt") == 0);

	git_oid_fromstr(&oid, CONFLICTS_TWO_OUR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-two.txt") == 0);

	git_oid_fromstr(&oid, CONFLICTS_TWO_THEIR_OID);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-two.txt") == 0);

	cl_assert(git_index_conflict_next(&conflict_entry[0], &conflict_entry[1], &conflict_entry[2], iterator) == GIT_ITEROVER);

	cl_assert(conflict_entry[0] == NULL);
	cl_assert(conflict_entry[2] == NULL);
	cl_assert(conflict_entry[2] == NULL);

	git_index_conflict_iterator_free(iterator);
}

void test_index_conflicts__remove(void)
{
	const git_index_entry *entry;
	size_t i;

	cl_assert(git_index_entrycount(repo_index) == 8);

	cl_git_pass(git_index_conflict_remove(repo_index, "conflicts-one.txt"));
	cl_assert(git_index_entrycount(repo_index) == 5);

	for (i = 0; i < git_index_entrycount(repo_index); i++) {
		cl_assert(entry = git_index_get_byindex(repo_index, i));
		cl_assert(strcmp(entry->path, "conflicts-one.txt") != 0);
	}

	cl_git_pass(git_index_conflict_remove(repo_index, "conflicts-two.txt"));
	cl_assert(git_index_entrycount(repo_index) == 2);

	for (i = 0; i < git_index_entrycount(repo_index); i++) {
		cl_assert(entry = git_index_get_byindex(repo_index, i));
		cl_assert(strcmp(entry->path, "conflicts-two.txt") != 0);
	}
}

void test_index_conflicts__moved_to_reuc_on_add(void)
{
	const git_index_entry *entry;
	size_t i;

	cl_assert(git_index_entrycount(repo_index) == 8);

	cl_git_mkfile("./mergedrepo/conflicts-one.txt", "new-file\n");

	cl_git_pass(git_index_add_bypath(repo_index, "conflicts-one.txt"));

	cl_assert(git_index_entrycount(repo_index) == 6);

	for (i = 0; i < git_index_entrycount(repo_index); i++) {
		cl_assert(entry = git_index_get_byindex(repo_index, i));

		if (strcmp(entry->path, "conflicts-one.txt") == 0)
			cl_assert(git_index_entry_stage(entry) == 0);
	}
}

void test_index_conflicts__moved_to_reuc_on_remove(void)
{
	const git_index_entry *entry;
	size_t i;

	cl_assert(git_index_entrycount(repo_index) == 8);

	cl_git_pass(p_unlink("./mergedrepo/conflicts-one.txt"));

	cl_git_pass(git_index_remove_bypath(repo_index, "conflicts-one.txt"));

	cl_assert(git_index_entrycount(repo_index) == 5);

	for (i = 0; i < git_index_entrycount(repo_index); i++) {
		cl_assert(entry = git_index_get_byindex(repo_index, i));
		cl_assert(strcmp(entry->path, "conflicts-one.txt") != 0);
	}
}

void test_index_conflicts__remove_all_conflicts(void)
{
	size_t i;
	const git_index_entry *entry;

	cl_assert(git_index_entrycount(repo_index) == 8);

	cl_assert_equal_i(true, git_index_has_conflicts(repo_index));

	git_index_conflict_cleanup(repo_index);

	cl_assert_equal_i(false, git_index_has_conflicts(repo_index));

	cl_assert(git_index_entrycount(repo_index) == 2);

	for (i = 0; i < git_index_entrycount(repo_index); i++) {
		cl_assert(entry = git_index_get_byindex(repo_index, i));
		cl_assert(git_index_entry_stage(entry) == 0);
	}
}

void test_index_conflicts__partial(void)
{
	git_index_entry ancestor_entry, our_entry, their_entry;
	const git_index_entry *conflict_entry[3];

	cl_assert(git_index_entrycount(repo_index) == 8);

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	ancestor_entry.path = "test-one.txt";
	ancestor_entry.flags |= (1 << GIT_IDXENTRY_STAGESHIFT);
	git_oid_fromstr(&ancestor_entry.id, TEST_ANCESTOR_OID);

	cl_git_pass(git_index_conflict_add(repo_index, &ancestor_entry, NULL, NULL));
	cl_assert(git_index_entrycount(repo_index) == 9);

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], repo_index, "test-one.txt"));

	cl_assert_equal_oid(&ancestor_entry.id, &conflict_entry[0]->id);
	cl_assert(conflict_entry[1] == NULL);
	cl_assert(conflict_entry[2] == NULL);
}
