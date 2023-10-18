#include "clar_libgit2.h"
#include "index.h"
#include "git2/repository.h"
#include "conflicts.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH "mergedrepo"
#define TEST_INDEX_PATH TEST_REPO_PATH "/.git/index"

/* Fixture setup and teardown */
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
	ancestor_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, 1);
	git_oid__fromstr(&ancestor_entry.id, CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);

	our_entry.path = "test-one.txt";
	our_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&our_entry, 2);
	git_oid__fromstr(&our_entry.id, CONFLICTS_ONE_OUR_OID, GIT_OID_SHA1);

	their_entry.path = "test-one.txt";
	their_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, 2);
	git_oid__fromstr(&their_entry.id, CONFLICTS_ONE_THEIR_OID, GIT_OID_SHA1);

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
	ancestor_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, 3);
	git_oid__fromstr(&ancestor_entry.id, CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);

	our_entry.path = "test-one.txt";
	our_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&our_entry, 1);
	git_oid__fromstr(&our_entry.id, CONFLICTS_ONE_OUR_OID, GIT_OID_SHA1);

	their_entry.path = "test-one.txt";
	their_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&their_entry, 2);
	git_oid__fromstr(&their_entry.id, CONFLICTS_ONE_THEIR_OID, GIT_OID_SHA1);

	cl_git_pass(git_index_conflict_add(repo_index, &ancestor_entry, &our_entry, &their_entry));

	cl_assert(git_index_entrycount(repo_index) == 11);

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1], &conflict_entry[2], repo_index, "test-one.txt"));

	cl_assert(git_index_entry_stage(conflict_entry[0]) == 1);
	cl_assert(git_index_entry_stage(conflict_entry[1]) == 2);
	cl_assert(git_index_entry_stage(conflict_entry[2]) == 3);
}

void test_index_conflicts__add_detects_invalid_filemode(void)
{
	git_index_entry ancestor_entry, our_entry, their_entry;
	git_index_entry *conflict_entry[3];
	int i;

	cl_assert(git_index_entrycount(repo_index) == 8);

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	conflict_entry[0] = &ancestor_entry;
	conflict_entry[1] = &our_entry;
	conflict_entry[2] = &their_entry;

	for (i = 0; i < 3; i++) {
		ancestor_entry.path = "test-one.txt";
		ancestor_entry.mode = 0100644;
		GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, 3);
		git_oid__fromstr(&ancestor_entry.id, CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);

		our_entry.path = "test-one.txt";
		our_entry.mode = 0100644;
		GIT_INDEX_ENTRY_STAGE_SET(&our_entry, 1);
		git_oid__fromstr(&our_entry.id, CONFLICTS_ONE_OUR_OID, GIT_OID_SHA1);

		their_entry.path = "test-one.txt";
		their_entry.mode = 0100644;
		GIT_INDEX_ENTRY_STAGE_SET(&their_entry, 2);
		git_oid__fromstr(&their_entry.id, CONFLICTS_ONE_THEIR_OID, GIT_OID_SHA1);

		/* Corrupt the conflict entry's mode */
		conflict_entry[i]->mode = 027431745;

		cl_git_fail(git_index_conflict_add(repo_index, &ancestor_entry, &our_entry, &their_entry));
	}

	cl_assert(git_index_entrycount(repo_index) == 8);
}


void test_index_conflicts__add_removes_stage_zero(void)
{
	git_index_entry ancestor_entry, our_entry, their_entry;
	const git_index_entry *conflict_entry[3];

	cl_assert(git_index_entrycount(repo_index) == 8);

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	cl_git_mkfile("./mergedrepo/test-one.txt", "new-file\n");
	cl_git_pass(git_index_add_bypath(repo_index, "test-one.txt"));
	cl_assert(git_index_entrycount(repo_index) == 9);

	ancestor_entry.path = "test-one.txt";
	ancestor_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, 3);
	git_oid__fromstr(&ancestor_entry.id, CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);

	our_entry.path = "test-one.txt";
	our_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&our_entry, 1);
	git_oid__fromstr(&our_entry.id, CONFLICTS_ONE_OUR_OID, GIT_OID_SHA1);

	their_entry.path = "test-one.txt";
	their_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&their_entry, 2);
	git_oid__fromstr(&their_entry.id, CONFLICTS_ONE_THEIR_OID, GIT_OID_SHA1);

	cl_git_pass(git_index_conflict_add(repo_index, &ancestor_entry, &our_entry, &their_entry));

	cl_assert(git_index_entrycount(repo_index) == 11);

	cl_assert_equal_p(NULL, git_index_get_bypath(repo_index, "test-one.txt", 0));

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1], &conflict_entry[2], repo_index, "test-one.txt"));

	cl_assert_equal_oid(&ancestor_entry.id, &conflict_entry[0]->id);
	cl_assert_equal_i(1, git_index_entry_stage(conflict_entry[0]));
	cl_assert_equal_oid(&our_entry.id, &conflict_entry[1]->id);
	cl_assert_equal_i(2, git_index_entry_stage(conflict_entry[1]));
	cl_assert_equal_oid(&their_entry.id, &conflict_entry[2]->id);
	cl_assert_equal_i(3, git_index_entry_stage(conflict_entry[2]));
}

void test_index_conflicts__get(void)
{
	const git_index_entry *conflict_entry[3];
	git_oid oid;

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], repo_index, "conflicts-one.txt"));

	cl_assert_equal_s("conflicts-one.txt", conflict_entry[0]->path);

	git_oid__fromstr(&oid, CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);

	git_oid__fromstr(&oid, CONFLICTS_ONE_OUR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);

	git_oid__fromstr(&oid, CONFLICTS_ONE_THEIR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], repo_index, "conflicts-two.txt"));

	cl_assert_equal_s("conflicts-two.txt", conflict_entry[0]->path);

	git_oid__fromstr(&oid, CONFLICTS_TWO_ANCESTOR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);

	git_oid__fromstr(&oid, CONFLICTS_TWO_OUR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);

	git_oid__fromstr(&oid, CONFLICTS_TWO_THEIR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);
}

void test_index_conflicts__iterate(void)
{
	git_index_conflict_iterator *iterator;
	const git_index_entry *conflict_entry[3];
	git_oid oid;

	cl_git_pass(git_index_conflict_iterator_new(&iterator, repo_index));

	cl_git_pass(git_index_conflict_next(&conflict_entry[0], &conflict_entry[1], &conflict_entry[2], iterator));

	git_oid__fromstr(&oid, CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-one.txt") == 0);

	git_oid__fromstr(&oid, CONFLICTS_ONE_OUR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-one.txt") == 0);

	git_oid__fromstr(&oid, CONFLICTS_ONE_THEIR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-one.txt") == 0);

	cl_git_pass(git_index_conflict_next(&conflict_entry[0], &conflict_entry[1], &conflict_entry[2], iterator));

	git_oid__fromstr(&oid, CONFLICTS_TWO_ANCESTOR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-two.txt") == 0);

	git_oid__fromstr(&oid, CONFLICTS_TWO_OUR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);
	cl_assert(git__strcmp(conflict_entry[0]->path, "conflicts-two.txt") == 0);

	git_oid__fromstr(&oid, CONFLICTS_TWO_THEIR_OID, GIT_OID_SHA1);
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
			cl_assert(!git_index_entry_is_conflict(entry));
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
		cl_assert(!git_index_entry_is_conflict(entry));
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
	ancestor_entry.mode = 0100644;
	GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, 1);
	git_oid__fromstr(&ancestor_entry.id, CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);

	cl_git_pass(git_index_conflict_add(repo_index, &ancestor_entry, NULL, NULL));
	cl_assert(git_index_entrycount(repo_index) == 9);

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], repo_index, "test-one.txt"));

	cl_assert_equal_oid(&ancestor_entry.id, &conflict_entry[0]->id);
	cl_assert(conflict_entry[1] == NULL);
	cl_assert(conflict_entry[2] == NULL);
}

void test_index_conflicts__case_matters(void)
{
	const git_index_entry *conflict_entry[3];
	git_oid oid;
	const char *upper_case = "DIFFERS-IN-CASE.TXT";
	const char *mixed_case = "Differs-In-Case.txt";
	const char *correct_case;
	bool ignorecase = cl_repo_get_bool(repo, "core.ignorecase");

	git_index_entry ancestor_entry, our_entry, their_entry;

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	ancestor_entry.path = upper_case;
	GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, GIT_INDEX_STAGE_ANCESTOR);
	git_oid__fromstr(&ancestor_entry.id, CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);
	ancestor_entry.mode = GIT_FILEMODE_BLOB;

	our_entry.path = upper_case;
	GIT_INDEX_ENTRY_STAGE_SET(&our_entry, GIT_INDEX_STAGE_OURS);
	git_oid__fromstr(&our_entry.id, CONFLICTS_ONE_OUR_OID, GIT_OID_SHA1);
	our_entry.mode = GIT_FILEMODE_BLOB;

	their_entry.path = upper_case;
	GIT_INDEX_ENTRY_STAGE_SET(&their_entry, GIT_INDEX_STAGE_THEIRS);
	git_oid__fromstr(&their_entry.id, CONFLICTS_ONE_THEIR_OID, GIT_OID_SHA1);
	their_entry.mode = GIT_FILEMODE_BLOB;

	cl_git_pass(git_index_conflict_add(repo_index,
		&ancestor_entry, &our_entry, &their_entry));

	ancestor_entry.path = mixed_case;
	GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, GIT_INDEX_STAGE_ANCESTOR);
	git_oid__fromstr(&ancestor_entry.id, CONFLICTS_TWO_ANCESTOR_OID, GIT_OID_SHA1);
	ancestor_entry.mode = GIT_FILEMODE_BLOB;

	our_entry.path = mixed_case;
	GIT_INDEX_ENTRY_STAGE_SET(&ancestor_entry, GIT_INDEX_STAGE_ANCESTOR);
	git_oid__fromstr(&our_entry.id, CONFLICTS_TWO_OUR_OID, GIT_OID_SHA1);
	ancestor_entry.mode = GIT_FILEMODE_BLOB;

	their_entry.path = mixed_case;
	GIT_INDEX_ENTRY_STAGE_SET(&their_entry, GIT_INDEX_STAGE_THEIRS);
	git_oid__fromstr(&their_entry.id, CONFLICTS_TWO_THEIR_OID, GIT_OID_SHA1);
	their_entry.mode = GIT_FILEMODE_BLOB;

	cl_git_pass(git_index_conflict_add(repo_index,
		&ancestor_entry, &our_entry, &their_entry));

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], repo_index, upper_case));

	/*
	 * We inserted with mixed case last, so on a case-insensitive
	 * fs we should get the mixed case.
	 */
	if (ignorecase)
		correct_case = mixed_case;
	else
		correct_case = upper_case;

	cl_assert_equal_s(correct_case, conflict_entry[0]->path);
	git_oid__fromstr(&oid, ignorecase ? CONFLICTS_TWO_ANCESTOR_OID : CONFLICTS_ONE_ANCESTOR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);

	cl_assert_equal_s(correct_case, conflict_entry[1]->path);
	git_oid__fromstr(&oid, ignorecase ? CONFLICTS_TWO_OUR_OID : CONFLICTS_ONE_OUR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);

	cl_assert_equal_s(correct_case, conflict_entry[2]->path);
	git_oid__fromstr(&oid, ignorecase ? CONFLICTS_TWO_THEIR_OID : CONFLICTS_ONE_THEIR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], repo_index, mixed_case));

	cl_assert_equal_s(mixed_case, conflict_entry[0]->path);
	git_oid__fromstr(&oid, CONFLICTS_TWO_ANCESTOR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[0]->id);

	cl_assert_equal_s(mixed_case, conflict_entry[1]->path);
	git_oid__fromstr(&oid, CONFLICTS_TWO_OUR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[1]->id);

	cl_assert_equal_s(mixed_case, conflict_entry[2]->path);
	git_oid__fromstr(&oid, CONFLICTS_TWO_THEIR_OID, GIT_OID_SHA1);
	cl_assert_equal_oid(&oid, &conflict_entry[2]->id);
}
