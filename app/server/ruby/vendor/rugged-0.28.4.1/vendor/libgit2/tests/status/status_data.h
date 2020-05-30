#include "status_helpers.h"

/* A utf-8 string with 83 characters, but 249 bytes. */
static const char *longname = "\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97";


/* entries for a plain copy of tests/resources/status */

static const char *entry_paths0[] = {
	"file_deleted",
	"ignored_file",
	"modified_file",
	"new_file",
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_file_deleted",
	"staged_delete_modified_file",
	"staged_new_file",
	"staged_new_file_deleted_file",
	"staged_new_file_modified_file",

	"subdir/deleted_file",
	"subdir/modified_file",
	"subdir/new_file",

	"\xe8\xbf\x99",
};

static const unsigned int entry_statuses0[] = {
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_IGNORED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_INDEX_MODIFIED | GIT_STATUS_WT_DELETED,
	GIT_STATUS_INDEX_MODIFIED | GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_INDEX_DELETED | GIT_STATUS_WT_NEW,
	GIT_STATUS_INDEX_NEW,
	GIT_STATUS_INDEX_NEW | GIT_STATUS_WT_DELETED,
	GIT_STATUS_INDEX_NEW | GIT_STATUS_WT_MODIFIED,

	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,

	GIT_STATUS_WT_NEW,
};

static const int entry_count0 = 16;

/* entries for a copy of tests/resources/status with all content
 * deleted from the working directory
 */

static const char *entry_paths2[] = {
	"current_file",
	"file_deleted",
	"modified_file",
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_file_deleted",
	"staged_delete_modified_file",
	"staged_new_file",
	"staged_new_file_deleted_file",
	"staged_new_file_modified_file",
	"subdir.txt",
	"subdir/current_file",
	"subdir/deleted_file",
	"subdir/modified_file",
};

static const unsigned int entry_statuses2[] = {
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
};

static const int entry_count2 = 15;

/* entries for a copy of tests/resources/status with some mods */

static const char *entry_paths3_icase[] = {
	".HEADER",
	"42-is-not-prime.sigh",
	"current_file",
	"current_file/",
	"file_deleted",
	"ignored_file",
	"modified_file",
	"new_file",
	"README.md",
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_file_deleted",
	"staged_delete_modified_file",
	"staged_new_file",
	"staged_new_file_deleted_file",
	"staged_new_file_modified_file",
	"subdir",
	"subdir/current_file",
	"subdir/deleted_file",
	"subdir/modified_file",
	"\xe8\xbf\x99",
};

static const unsigned int entry_statuses3_icase[] = {
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_IGNORED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_WT_NEW | GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_NEW,
};

static const char *entry_paths3[] = {
	".HEADER",
	"42-is-not-prime.sigh",
	"README.md",
	"current_file",
	"current_file/",
	"file_deleted",
	"ignored_file",
	"modified_file",
	"new_file",
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_file_deleted",
	"staged_delete_modified_file",
	"staged_new_file",
	"staged_new_file_deleted_file",
	"staged_new_file_modified_file",
	"subdir",
	"subdir/current_file",
	"subdir/deleted_file",
	"subdir/modified_file",
	"\xe8\xbf\x99",
};

static const unsigned int entry_statuses3[] = {
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_IGNORED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_WT_NEW | GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_NEW,
};

static const int entry_count3 = 22;


/* entries for a copy of tests/resources/status with some mods
 * and different options to the status call
 */

static const char *entry_paths4[] = {
	".new_file",
	"current_file",
	"current_file/current_file",
	"current_file/modified_file",
	"current_file/new_file",
	"file_deleted",
	"modified_file",
	"new_file",
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_file_deleted",
	"staged_delete_modified_file",
	"staged_new_file",
	"staged_new_file_deleted_file",
	"staged_new_file_modified_file",
	"subdir",
	"subdir/current_file",
	"subdir/deleted_file",
	"subdir/modified_file",
	"zzz_new_dir/new_file",
	"zzz_new_file",
	"\xe8\xbf\x99",
};

static const unsigned int entry_statuses4[] = {
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_WT_NEW | GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_DELETED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
};

static const int entry_count4 = 23;


/* entries for a copy of tests/resources/status with options
 * passed to the status call in order to only get the differences
 * between the HEAD and the index (changes to be committed)
 */

static const char *entry_paths5[] = {
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_file_deleted",
	"staged_delete_modified_file",
	"staged_new_file",
	"staged_new_file_deleted_file",
	"staged_new_file_modified_file",
};

static const unsigned int entry_statuses5[] = {
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_INDEX_NEW,
	GIT_STATUS_INDEX_NEW,
	GIT_STATUS_INDEX_NEW,
};

static const int entry_count5 = 8;


/* entries for a copy of tests/resources/status with options
 * passed to the status call in order to only get the differences
 * between the workdir and the index (changes not staged, untracked files)
 */

static const char *entry_paths6[] = {
	"file_deleted",
	"ignored_file",
	"modified_file",
	"new_file",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_modified_file",
	"staged_new_file_deleted_file",
	"staged_new_file_modified_file",
	"subdir/deleted_file",
	"subdir/modified_file",
	"subdir/new_file",
	"\xe8\xbf\x99",
};

static const unsigned int entry_statuses6[] = {
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_IGNORED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_WT_NEW,
};

static const int entry_count6 = 13;
