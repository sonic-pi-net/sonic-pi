#include "clar_libgit2.h"
#include <git2/sys/config.h>
#include <git2/sys/filter.h>
#include <git2/sys/odb_backend.h>
#include <git2/sys/refdb_backend.h>
#include <git2/sys/transport.h>

#define STRINGIFY(s) #s

/* Checks two conditions for the specified structure:
 *     1. That the initializers for the latest version produces the same
 *        in-memory representation.
 *     2. That the function-based initializer supports all versions from 1...n,
 *        where n is the latest version (often represented by GIT_*_VERSION).
 *
 * Parameters:
 *     structname: The name of the structure to test, e.g. git_blame_options.
 *     structver: The latest version of the specified structure.
 *     macroinit: The macro that initializes the latest version of the structure.
 *     funcinitname: The function that initializes the structure. Must have the
 *                   signature "int (structname* instance, int version)".
 */
#define CHECK_MACRO_FUNC_INIT_EQUAL(structname, structver, macroinit, funcinitname) \
do { \
	structname structname##_macro_latest = macroinit; \
	structname structname##_func_latest; \
	int structname##_curr_ver = structver - 1; \
	memset(&structname##_func_latest, 0, sizeof(structname##_func_latest)); \
	cl_git_pass(funcinitname(&structname##_func_latest, structver)); \
	options_cmp(&structname##_macro_latest, &structname##_func_latest, \
		sizeof(structname), STRINGIFY(structname)); \
	\
	while (structname##_curr_ver > 0) \
	{ \
		structname macro; \
		cl_git_pass(funcinitname(&macro, structname##_curr_ver)); \
		structname##_curr_ver--; \
	}\
} while(0)

static void options_cmp(void *one, void *two, size_t size, const char *name)
{
	size_t i;

	for (i = 0; i < size; i++) {
		if (((char *)one)[i] != ((char *)two)[i]) {
			char desc[1024];

			p_snprintf(desc, 1024, "Difference in %s at byte %" PRIuZ ": macro=%u / func=%u",
				name, i, ((char *)one)[i], ((char *)two)[i]);
			clar__fail(__FILE__, __LINE__,
				"Difference between macro and function options initializer",
				desc, 0);
			return;
		}
	}
}

void test_core_structinit__compare(void)
{
	/* These tests assume that they can memcmp() two structures that were
	 * initialized with the same static initializer.  Eg,
	 * git_blame_options = GIT_BLAME_OPTIONS_INIT;
	 *
	 * This assumption fails when there is padding between structure members,
	 * which is not guaranteed to be initialized to anything sane at all.
	 *
	 * Assume most compilers, in a debug build, will clear that memory for
	 * us or set it to sentinal markers.  Etc.
	 */
#if !defined(DEBUG) && !defined(_DEBUG)
	clar__skip();
#endif

	/* blame */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_blame_options, GIT_BLAME_OPTIONS_VERSION, \
		GIT_BLAME_OPTIONS_INIT, git_blame_init_options);

	/* checkout */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_checkout_options, GIT_CHECKOUT_OPTIONS_VERSION, \
		GIT_CHECKOUT_OPTIONS_INIT, git_checkout_init_options);

	/* clone */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_clone_options, GIT_CLONE_OPTIONS_VERSION, \
		GIT_CLONE_OPTIONS_INIT, git_clone_init_options);

	/* diff */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_diff_options, GIT_DIFF_OPTIONS_VERSION, \
		GIT_DIFF_OPTIONS_INIT, git_diff_init_options);

	/* diff_find */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_diff_find_options, GIT_DIFF_FIND_OPTIONS_VERSION, \
		GIT_DIFF_FIND_OPTIONS_INIT, git_diff_find_init_options);

	/* filter */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_filter, GIT_FILTER_VERSION, \
		GIT_FILTER_INIT, git_filter_init);

	/* merge_file_input */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_merge_file_input, GIT_MERGE_FILE_INPUT_VERSION, \
		GIT_MERGE_FILE_INPUT_INIT, git_merge_file_init_input);

	/* merge_file */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_merge_file_options, GIT_MERGE_FILE_OPTIONS_VERSION, \
		GIT_MERGE_FILE_OPTIONS_INIT, git_merge_file_init_options);

	/* merge_tree */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_merge_options, GIT_MERGE_OPTIONS_VERSION, \
		GIT_MERGE_OPTIONS_INIT, git_merge_init_options);

	/* push */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_push_options, GIT_PUSH_OPTIONS_VERSION, \
		GIT_PUSH_OPTIONS_INIT, git_push_init_options);

	/* remote */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_remote_callbacks, GIT_REMOTE_CALLBACKS_VERSION, \
		GIT_REMOTE_CALLBACKS_INIT, git_remote_init_callbacks);

	/* repository_init */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_repository_init_options, GIT_REPOSITORY_INIT_OPTIONS_VERSION, \
		GIT_REPOSITORY_INIT_OPTIONS_INIT, git_repository_init_init_options);

	/* revert */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_revert_options, GIT_REVERT_OPTIONS_VERSION, \
		GIT_REVERT_OPTIONS_INIT, git_revert_init_options);

	/* stash apply */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_stash_apply_options, GIT_STASH_APPLY_OPTIONS_VERSION, \
		GIT_STASH_APPLY_OPTIONS_INIT, git_stash_apply_init_options);

	/* status */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_status_options, GIT_STATUS_OPTIONS_VERSION, \
		GIT_STATUS_OPTIONS_INIT, git_status_init_options);

	/* transport */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_transport, GIT_TRANSPORT_VERSION, \
		GIT_TRANSPORT_INIT, git_transport_init);

	/* config_backend */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_config_backend, GIT_CONFIG_BACKEND_VERSION, \
		GIT_CONFIG_BACKEND_INIT, git_config_init_backend);

	/* odb_backend */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_odb_backend, GIT_ODB_BACKEND_VERSION, \
		GIT_ODB_BACKEND_INIT, git_odb_init_backend);

	/* refdb_backend */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_refdb_backend, GIT_REFDB_BACKEND_VERSION, \
		GIT_REFDB_BACKEND_INIT, git_refdb_init_backend);

	/* submodule update */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_submodule_update_options, GIT_SUBMODULE_UPDATE_OPTIONS_VERSION, \
		GIT_SUBMODULE_UPDATE_OPTIONS_INIT, git_submodule_update_init_options);

	/* submodule update */
	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_proxy_options, GIT_PROXY_OPTIONS_VERSION, \
		GIT_PROXY_OPTIONS_INIT, git_proxy_init_options);

	CHECK_MACRO_FUNC_INIT_EQUAL( \
		git_diff_patchid_options, GIT_DIFF_PATCHID_OPTIONS_VERSION, \
		GIT_DIFF_PATCHID_OPTIONS_INIT, git_diff_patchid_init_options);
}
