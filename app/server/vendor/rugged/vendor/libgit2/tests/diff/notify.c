#include "clar_libgit2.h"
#include "diff_helpers.h"

static git_repository *g_repo = NULL;

void test_diff_notify__initialize(void)
{
}

void test_diff_notify__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static int assert_called_notifications(
	const git_diff *diff_so_far,
	const git_diff_delta *delta_to_add,
	const char *matched_pathspec,
	void *payload)
{
	bool found = false;
	notify_expected *exp = (notify_expected*)payload;
	notify_expected *e;

	GIT_UNUSED(diff_so_far);

	for (e = exp; e->path != NULL; e++) {
		if (strcmp(e->path, delta_to_add->new_file.path))
			continue;

		cl_assert_equal_s(e->matched_pathspec, matched_pathspec);

		found = true;
		break;
	}

	cl_assert(found);
	return 0;
}

static void test_notify(
	char **searched_pathspecs,
	int pathspecs_count,
	notify_expected *expected_matched_pathspecs,
	int expected_diffed_files_count)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	diff_expects exp;

	g_repo = cl_git_sandbox_init("status");

	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;
	opts.notify_cb = assert_called_notifications;
	opts.pathspec.strings = searched_pathspecs;
	opts.pathspec.count   = pathspecs_count;

	opts.notify_payload = expected_matched_pathspecs;
	memset(&exp, 0, sizeof(exp));

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, NULL, &opts));
	cl_git_pass(git_diff_foreach(diff, diff_file_cb, NULL, NULL, &exp));

	cl_assert_equal_i(expected_diffed_files_count, exp.files);

	git_diff_free(diff);
}

void test_diff_notify__notify_single_pathspec(void)
{
	char *searched_pathspecs[] = {
		"*_deleted",
	};
	notify_expected expected_matched_pathspecs[] = {
		{ "file_deleted", "*_deleted" },
		{ "staged_changes_file_deleted", "*_deleted" },
		{ NULL, NULL }
	};

	test_notify(searched_pathspecs, 1, expected_matched_pathspecs, 2);
}

void test_diff_notify__notify_multiple_pathspec(void)
{
	char *searched_pathspecs[] = {
		"staged_changes_cant_find_me",
		"subdir/modified_cant_find_me",
		"subdir/*",
		"staged*"
	};
	notify_expected expected_matched_pathspecs[] = {
		{ "staged_changes_file_deleted", "staged*" },
		{ "staged_changes_modified_file", "staged*" },
		{ "staged_delete_modified_file", "staged*" },
		{ "staged_new_file_deleted_file", "staged*" },
		{ "staged_new_file_modified_file", "staged*" },
		{ "subdir/deleted_file", "subdir/*" },
		{ "subdir/modified_file", "subdir/*" },
		{ "subdir/new_file", "subdir/*" },
		{ NULL, NULL }
	};

	test_notify(searched_pathspecs, 4, expected_matched_pathspecs, 8);
}

void test_diff_notify__notify_catchall_with_empty_pathspecs(void)
{
	char *searched_pathspecs[] = {
		"",
		""
	};
	notify_expected expected_matched_pathspecs[] = {
		{ "file_deleted", NULL },
		{ "ignored_file", NULL },
		{ "modified_file", NULL },
		{ "new_file", NULL },
		{ "\xe8\xbf\x99", NULL },
		{ "staged_changes_file_deleted", NULL },
		{ "staged_changes_modified_file", NULL },
		{ "staged_delete_modified_file", NULL },
		{ "staged_new_file_deleted_file", NULL },
		{ "staged_new_file_modified_file", NULL },
		{ "subdir/deleted_file", NULL },
		{ "subdir/modified_file", NULL },
		{ "subdir/new_file", NULL },
		{ NULL, NULL }
	};

	test_notify(searched_pathspecs, 1, expected_matched_pathspecs, 13);
}

void test_diff_notify__notify_catchall(void)
{
	char *searched_pathspecs[] = {
		"*",
	};
	notify_expected expected_matched_pathspecs[] = {
		{ "file_deleted", "*" },
		{ "ignored_file", "*" },
		{ "modified_file", "*" },
		{ "new_file", "*" },
		{ "\xe8\xbf\x99", "*" },
		{ "staged_changes_file_deleted", "*" },
		{ "staged_changes_modified_file", "*" },
		{ "staged_delete_modified_file", "*" },
		{ "staged_new_file_deleted_file", "*" },
		{ "staged_new_file_modified_file", "*" },
		{ "subdir/deleted_file", "*" },
		{ "subdir/modified_file", "*" },
		{ "subdir/new_file", "*" },
		{ NULL, NULL }
	};

	test_notify(searched_pathspecs, 1, expected_matched_pathspecs, 13);
}

static int abort_diff(
	const git_diff *diff_so_far,
	const git_diff_delta *delta_to_add,
	const char *matched_pathspec,
	void *payload)
{
	GIT_UNUSED(diff_so_far);
	GIT_UNUSED(delta_to_add);
	GIT_UNUSED(matched_pathspec);
	GIT_UNUSED(payload);

	return -42;
}

void test_diff_notify__notify_cb_can_abort_diff(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	char *pathspec = NULL;

	g_repo = cl_git_sandbox_init("status");

	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;
	opts.notify_cb = abort_diff;
	opts.pathspec.strings = &pathspec;
	opts.pathspec.count   = 1;

	pathspec = "file_deleted";
	cl_git_fail_with(
		git_diff_index_to_workdir(&diff, g_repo, NULL, &opts), -42);

	pathspec = "staged_changes_modified_file";
	cl_git_fail_with(
		git_diff_index_to_workdir(&diff, g_repo, NULL, &opts), -42);
}

static int filter_all(
	const git_diff *diff_so_far,
	const git_diff_delta *delta_to_add,
	const char *matched_pathspec,
	void *payload)
{
	GIT_UNUSED(diff_so_far);
	GIT_UNUSED(delta_to_add);
	GIT_UNUSED(matched_pathspec);
	GIT_UNUSED(payload);

	return 42;
}

void test_diff_notify__notify_cb_can_be_used_as_filtering_function(void)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	char *pathspec = NULL;
	diff_expects exp;

	g_repo = cl_git_sandbox_init("status");

	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;
	opts.notify_cb = filter_all;
	opts.pathspec.strings = &pathspec;
	opts.pathspec.count   = 1;

	pathspec = "*_deleted";
	memset(&exp, 0, sizeof(exp));

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, NULL, &opts));
	cl_git_pass(git_diff_foreach(diff, diff_file_cb, NULL, NULL, &exp));

	cl_assert_equal_i(0, exp.files);

	git_diff_free(diff);
}
