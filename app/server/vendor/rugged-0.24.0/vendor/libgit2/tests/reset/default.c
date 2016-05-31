#include "clar_libgit2.h"
#include "posix.h"
#include "reset_helpers.h"
#include "path.h"

static git_repository *_repo;
static git_object *_target;
static git_strarray _pathspecs;
static git_index *_index;

static void initialize(const char *repo_name)
{
	_repo = cl_git_sandbox_init(repo_name);
	cl_git_pass(git_repository_index(&_index, _repo));

	_target = NULL;

	_pathspecs.strings = NULL;
	_pathspecs.count = 0;
}

void test_reset_default__initialize(void)
{
}

void test_reset_default__cleanup(void)
{
	git_object_free(_target);
	_target = NULL;

	git_index_free(_index);
	_index = NULL;

	cl_git_sandbox_cleanup();
}

static void assert_content_in_index(
	git_strarray *pathspecs,
	bool should_exist,
	git_strarray *expected_shas)
{
	size_t i, pos;
	int error;

	for (i = 0; i < pathspecs->count; i++) {
		error = git_index_find(&pos, _index, pathspecs->strings[i]);

		if (should_exist) {
			const git_index_entry *entry;

			cl_assert(error != GIT_ENOTFOUND);

			entry = git_index_get_byindex(_index, pos);
			cl_assert(entry != NULL);

			if (!expected_shas)
				continue;

			cl_git_pass(git_oid_streq(&entry->id, expected_shas->strings[i]));
		} else
			cl_assert_equal_i(should_exist, error != GIT_ENOTFOUND);
	}
}

void test_reset_default__resetting_filepaths_against_a_null_target_removes_them_from_the_index(void)
{
	char *paths[] = { "staged_changes", "staged_new_file" };

	initialize("status");

	_pathspecs.strings = paths;
	_pathspecs.count = 2;

	assert_content_in_index(&_pathspecs, true, NULL);

	cl_git_pass(git_reset_default(_repo, NULL, &_pathspecs));

	assert_content_in_index(&_pathspecs, false, NULL);
}

/*
 * $ git ls-files --cached -s --abbrev=7 -- "staged*"
 * 100644 55d316c 0        staged_changes
 * 100644 a6be623 0        staged_changes_file_deleted
 * ...
 *
 * $ git reset 0017bd4 -- staged_changes staged_changes_file_deleted
 * Unstaged changes after reset:
 * ...
 *
 * $ git ls-files --cached -s --abbrev=7 -- "staged*"
 * 100644 32504b7 0        staged_changes
 * 100644 061d42a 0        staged_changes_file_deleted
 * ...
 */
void test_reset_default__resetting_filepaths_replaces_their_corresponding_index_entries(void)
{
	git_strarray before, after;

	char *paths[] = { "staged_changes", "staged_changes_file_deleted" };
	char *before_shas[] = { "55d316c9ba708999f1918e9677d01dfcae69c6b9",
		"a6be623522ce87a1d862128ac42672604f7b468b" };
	char *after_shas[] = { "32504b727382542f9f089e24fddac5e78533e96c",
		"061d42a44cacde5726057b67558821d95db96f19" };

	initialize("status");

	_pathspecs.strings = paths;
	_pathspecs.count = 2;
	before.strings = before_shas;
	before.count = 2;
	after.strings = after_shas;
	after.count = 2;

	cl_git_pass(git_revparse_single(&_target, _repo, "0017bd4"));
	assert_content_in_index(&_pathspecs, true, &before);

	cl_git_pass(git_reset_default(_repo, _target, &_pathspecs));

	assert_content_in_index(&_pathspecs, true, &after);
}

/*
 * $ git ls-files --cached -s --abbrev=7 -- conflicts-one.txt
 * 100644 1f85ca5 1        conflicts-one.txt
 * 100644 6aea5f2 2        conflicts-one.txt
 * 100644 516bd85 3        conflicts-one.txt
 *
 * $  git reset 9a05ccb -- conflicts-one.txt
 * Unstaged changes after reset:
 * ...
 *
 * $ git ls-files --cached -s --abbrev=7 -- conflicts-one.txt
 * 100644 1f85ca5 0        conflicts-one.txt
 *
 */
void test_reset_default__resetting_filepaths_clears_previous_conflicts(void)
{
	const git_index_entry *conflict_entry[3];
	git_strarray after;

	char *paths[] = { "conflicts-one.txt" };
	char *after_shas[] = { "1f85ca51b8e0aac893a621b61a9c2661d6aa6d81" };

	initialize("mergedrepo");

	_pathspecs.strings = paths;
	_pathspecs.count = 1;
	after.strings = after_shas;
	after.count = 1;

	cl_git_pass(git_index_conflict_get(&conflict_entry[0], &conflict_entry[1],
		&conflict_entry[2], _index, "conflicts-one.txt"));

	cl_git_pass(git_revparse_single(&_target, _repo, "9a05ccb"));
	cl_git_pass(git_reset_default(_repo, _target, &_pathspecs));

	assert_content_in_index(&_pathspecs, true, &after);

	cl_assert_equal_i(GIT_ENOTFOUND, git_index_conflict_get(&conflict_entry[0],
		&conflict_entry[1], &conflict_entry[2], _index, "conflicts-one.txt"));
}

/*
$  git reset HEAD -- "I_am_not_there.txt" "me_neither.txt"
Unstaged changes after reset:
...
*/
void test_reset_default__resetting_unknown_filepaths_does_not_fail(void)
{
	char *paths[] = { "I_am_not_there.txt", "me_neither.txt" };

	initialize("status");

	_pathspecs.strings = paths;
	_pathspecs.count = 2;

	assert_content_in_index(&_pathspecs, false, NULL);

	cl_git_pass(git_revparse_single(&_target, _repo, "HEAD"));
	cl_git_pass(git_reset_default(_repo, _target, &_pathspecs));

	assert_content_in_index(&_pathspecs, false, NULL);
}

void test_reset_default__staged_rename_reset_delete(void)
{
	git_index_entry entry;
	const git_index_entry *existing;
	char *paths[] = { "new.txt" };

	initialize("testrepo2");

	existing = git_index_get_bypath(_index, "new.txt", 0);
	cl_assert(existing);
	memcpy(&entry, existing, sizeof(entry));

	cl_git_pass(git_index_remove_bypath(_index, "new.txt"));

	entry.path = "renamed.txt";
	cl_git_pass(git_index_add(_index, &entry));

	_pathspecs.strings = paths;
	_pathspecs.count = 1;

	assert_content_in_index(&_pathspecs, false, NULL);

	cl_git_pass(git_revparse_single(&_target, _repo, "HEAD"));
	cl_git_pass(git_reset_default(_repo, _target, &_pathspecs));

	assert_content_in_index(&_pathspecs, true, NULL);
}
