#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "submodule_helpers.h"
#include "futils.h"
#include "iterator.h"

static git_repository *g_repo = NULL;

void test_submodule_status__initialize(void)
{
	g_repo = setup_fixture_submod2();
}

void test_submodule_status__cleanup(void)
{
}

void test_submodule_status__unchanged(void)
{
	unsigned int status = get_submodule_status(g_repo, "sm_unchanged");
	unsigned int expected =
		GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_IN_WD;

	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));
	cl_assert(expected == status);
}

static void rm_submodule(const char *name)
{
	git_str path = GIT_STR_INIT;
	cl_git_pass(git_str_joinpath(&path, git_repository_workdir(g_repo), name));
	cl_git_pass(git_futils_rmdir_r(path.ptr, NULL, GIT_RMDIR_REMOVE_FILES));
	git_str_dispose(&path);
}

static void add_submodule_to_index(const char *name)
{
	git_submodule *sm;
	cl_git_pass(git_submodule_lookup(&sm, g_repo, name));
	cl_git_pass(git_submodule_add_to_index(sm, true));
	git_submodule_free(sm);
}

static void rm_submodule_from_index(const char *name)
{
	git_index *index;
	size_t pos;

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_assert(!git_index_find(&pos, index, name));
	cl_git_pass(git_index_remove(index, name, 0));
	cl_git_pass(git_index_write(index));
	git_index_free(index);
}

/* 4 values of GIT_SUBMODULE_IGNORE to check */

void test_submodule_status__ignore_none(void)
{
	unsigned int status;

	rm_submodule("sm_unchanged");

	refute_submodule_exists(g_repo, "just_a_dir", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "not-submodule", GIT_EEXISTS);
	refute_submodule_exists(g_repo, "not", GIT_EEXISTS);

	status = get_submodule_status(g_repo, "sm_changed_index");
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_INDEX_MODIFIED) != 0);

	status = get_submodule_status(g_repo, "sm_changed_head");
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_MODIFIED) != 0);

	status = get_submodule_status(g_repo, "sm_changed_file");
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_WD_MODIFIED) != 0);

	status = get_submodule_status(g_repo, "sm_changed_untracked_file");
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_UNTRACKED) != 0);

	status = get_submodule_status(g_repo, "sm_missing_commits");
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_MODIFIED) != 0);

	status = get_submodule_status(g_repo, "sm_added_and_uncommited");
	cl_assert((status & GIT_SUBMODULE_STATUS_INDEX_ADDED) != 0);

	/* removed sm_unchanged for deleted workdir */
	status = get_submodule_status(g_repo, "sm_unchanged");
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_DELETED) != 0);

	/* now mkdir sm_unchanged to test uninitialized */
	cl_git_pass(git_futils_mkdir_relative("sm_unchanged", "submod2", 0755, 0, NULL));
	status = get_submodule_status(g_repo, "sm_unchanged");
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_UNINITIALIZED) != 0);

	/* update sm_changed_head in index */
	add_submodule_to_index("sm_changed_head");
	status = get_submodule_status(g_repo, "sm_changed_head");
	cl_assert((status & GIT_SUBMODULE_STATUS_INDEX_MODIFIED) != 0);

	/* remove sm_changed_head from index */
	rm_submodule_from_index("sm_changed_head");
	status = get_submodule_status(g_repo, "sm_changed_head");
	cl_assert((status & GIT_SUBMODULE_STATUS_INDEX_DELETED) != 0);
}

void test_submodule_status__ignore_untracked(void)
{
	unsigned int status;
	git_submodule_ignore_t ign = GIT_SUBMODULE_IGNORE_UNTRACKED;

	rm_submodule("sm_unchanged");

	refute_submodule_exists(g_repo, "just_a_dir", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "not-submodule", GIT_EEXISTS);
	refute_submodule_exists(g_repo, "not", GIT_EEXISTS);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_index", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_INDEX_MODIFIED) != 0);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_head", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_MODIFIED) != 0);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_file", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_WD_MODIFIED) != 0);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_untracked_file", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_missing_commits", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_MODIFIED) != 0);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_added_and_uncommited", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_INDEX_ADDED) != 0);

	/* removed sm_unchanged for deleted workdir */
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_unchanged", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_DELETED) != 0);

	/* now mkdir sm_unchanged to test uninitialized */
	cl_git_pass(git_futils_mkdir_relative("sm_unchanged", "submod2", 0755, 0, NULL));
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_unchanged", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_UNINITIALIZED) != 0);

	/* update sm_changed_head in index */
	add_submodule_to_index("sm_changed_head");
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_head", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_INDEX_MODIFIED) != 0);
}

void test_submodule_status__ignore_dirty(void)
{
	unsigned int status;
	git_submodule_ignore_t ign = GIT_SUBMODULE_IGNORE_DIRTY;

	rm_submodule("sm_unchanged");

	refute_submodule_exists(g_repo, "just_a_dir", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "not-submodule", GIT_EEXISTS);
	refute_submodule_exists(g_repo, "not", GIT_EEXISTS);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_index", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_head", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_MODIFIED) != 0);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_file", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_untracked_file", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_missing_commits", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_MODIFIED) != 0);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_added_and_uncommited", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_INDEX_ADDED) != 0);

	/* removed sm_unchanged for deleted workdir */
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_unchanged", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_DELETED) != 0);

	/* now mkdir sm_unchanged to test uninitialized */
	cl_git_pass(git_futils_mkdir_relative("sm_unchanged", "submod2", 0755, 0, NULL));
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_unchanged", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_WD_UNINITIALIZED) != 0);

	/* update sm_changed_head in index */
	add_submodule_to_index("sm_changed_head");
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_head", ign));
	cl_assert((status & GIT_SUBMODULE_STATUS_INDEX_MODIFIED) != 0);
}

void test_submodule_status__ignore_all(void)
{
	unsigned int status;
	git_submodule_ignore_t ign = GIT_SUBMODULE_IGNORE_ALL;

	rm_submodule("sm_unchanged");

	refute_submodule_exists(g_repo, "just_a_dir", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "not-submodule", GIT_EEXISTS);
	refute_submodule_exists(g_repo, "not", GIT_EEXISTS);

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_index", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_head", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_file", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_untracked_file", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_missing_commits", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	cl_git_pass(git_submodule_status(&status, g_repo,"sm_added_and_uncommited", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	/* removed sm_unchanged for deleted workdir */
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_unchanged", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	/* now mkdir sm_unchanged to test uninitialized */
	cl_git_pass(git_futils_mkdir_relative("sm_unchanged", "submod2", 0755, 0, NULL));
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_unchanged", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	/* update sm_changed_head in index */
	add_submodule_to_index("sm_changed_head");
	cl_git_pass(git_submodule_status(&status, g_repo,"sm_changed_head", ign));
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));
}

typedef struct {
	size_t counter;
	const char **paths;
	int *statuses;
} submodule_expectations;

static int confirm_submodule_status(
	const char *path, unsigned int status_flags, void *payload)
{
	submodule_expectations *exp = payload;

	while (exp->statuses[exp->counter] < 0)
		exp->counter++;

	cl_assert_equal_i(exp->statuses[exp->counter], (int)status_flags);
	cl_assert_equal_s(exp->paths[exp->counter++], path);

	GIT_UNUSED(status_flags);

	return 0;
}

void test_submodule_status__iterator(void)
{
	git_iterator *iter;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *entry;
	size_t i;
	static const char *expected[] = {
		".gitmodules",
		"just_a_dir/",
		"just_a_dir/contents",
		"just_a_file",
		"not-submodule/",
		"not-submodule/README.txt",
		"not/",
		"not/README.txt",
		"README.txt",
		"sm_added_and_uncommited",
		"sm_changed_file",
		"sm_changed_head",
		"sm_changed_index",
		"sm_changed_untracked_file",
		"sm_missing_commits",
		"sm_unchanged",
		NULL
	};
	static int expected_flags[] = {
		GIT_STATUS_INDEX_MODIFIED | GIT_STATUS_WT_MODIFIED, /* ".gitmodules" */
		-1,					    /* "just_a_dir/" will be skipped */
		GIT_STATUS_CURRENT,     /* "just_a_dir/contents" */
		GIT_STATUS_CURRENT,	    /* "just_a_file" */
		GIT_STATUS_WT_NEW,      /* "not-submodule/" untracked item */
		-1,                     /* "not-submodule/README.txt" */
		GIT_STATUS_WT_NEW,      /* "not/" untracked item */
		-1,                     /* "not/README.txt" */
		GIT_STATUS_CURRENT,     /* "README.txt */
		GIT_STATUS_INDEX_NEW,   /* "sm_added_and_uncommited" */
		GIT_STATUS_WT_MODIFIED, /* "sm_changed_file" */
		GIT_STATUS_WT_MODIFIED, /* "sm_changed_head" */
		GIT_STATUS_WT_MODIFIED, /* "sm_changed_index" */
		GIT_STATUS_WT_MODIFIED, /* "sm_changed_untracked_file" */
		GIT_STATUS_WT_MODIFIED, /* "sm_missing_commits" */
		GIT_STATUS_CURRENT,     /* "sm_unchanged" */
		0
	};
	submodule_expectations exp = { 0, expected, expected_flags };
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	git_index *index;

	iter_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_iterator_for_workdir(&iter, g_repo, index, NULL, &iter_opts));

	for (i = 0; !git_iterator_advance(&entry, iter); ++i)
		cl_assert_equal_s(expected[i], entry->path);

	git_iterator_free(iter);
	git_index_free(index);

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_INCLUDE_UNMODIFIED |
		GIT_STATUS_OPT_INCLUDE_IGNORED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS |
		GIT_STATUS_OPT_SORT_CASE_INSENSITIVELY;

	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, confirm_submodule_status, &exp));
}

void test_submodule_status__untracked_dirs_containing_ignored_files(void)
{
	unsigned int status, expected;

	cl_git_append2file(
		"submod2/.git/modules/sm_unchanged/info/exclude", "\n*.ignored\n");

	cl_git_pass(
		git_futils_mkdir_relative("sm_unchanged/directory", "submod2", 0755, 0, NULL));
	cl_git_mkfile(
		"submod2/sm_unchanged/directory/i_am.ignored",
		"ignore this file, please\n");

	status = get_submodule_status(g_repo, "sm_unchanged");
	cl_assert(GIT_SUBMODULE_STATUS_IS_UNMODIFIED(status));

	expected = GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_IN_WD;
	cl_assert(status == expected);
}
