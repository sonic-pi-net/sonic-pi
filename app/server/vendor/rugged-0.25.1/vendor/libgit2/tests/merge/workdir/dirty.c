#include "clar_libgit2.h"
#include "git2/merge.h"
#include "buffer.h"
#include "merge.h"
#include "index.h"
#include "../merge_helpers.h"
#include "posix.h"

#define TEST_REPO_PATH "merge-resolve"
#define MERGE_BRANCH_OID "7cb63eed597130ba4abb87b3e544b85021905520"

#define AUTOMERGEABLE_MERGED_FILE \
	"this file is changed in master\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is changed in branch\n"

#define CHANGED_IN_BRANCH_FILE \
	"changed in branch\n"

static git_repository *repo;
static git_index *repo_index;

static char *unaffected[][4] = {
	{ "added-in-master.txt", NULL },
	{ "changed-in-master.txt", NULL },
	{ "unchanged.txt", NULL },
	{ "added-in-master.txt", "changed-in-master.txt", NULL },
	{ "added-in-master.txt", "unchanged.txt", NULL },
	{ "changed-in-master.txt", "unchanged.txt", NULL },
	{ "added-in-master.txt", "changed-in-master.txt", "unchanged.txt", NULL },
	{ "new_file.txt", NULL },
	{ "new_file.txt", "unchanged.txt", NULL },
	{ NULL },
};

static char *affected[][5] = {
	{ "automergeable.txt", NULL },
	{ "changed-in-branch.txt", NULL },
	{ "conflicting.txt", NULL },
	{ "removed-in-branch.txt", NULL },
	{ "automergeable.txt", "changed-in-branch.txt", NULL },
	{ "automergeable.txt", "conflicting.txt", NULL },
	{ "automergeable.txt", "removed-in-branch.txt", NULL },
	{ "changed-in-branch.txt", "conflicting.txt", NULL },
	{ "changed-in-branch.txt", "removed-in-branch.txt", NULL },
	{ "conflicting.txt", "removed-in-branch.txt", NULL },
	{ "automergeable.txt", "changed-in-branch.txt", "conflicting.txt", NULL },
	{ "automergeable.txt", "changed-in-branch.txt", "removed-in-branch.txt", NULL },
	{ "automergeable.txt", "conflicting.txt", "removed-in-branch.txt", NULL },
	{ "changed-in-branch.txt", "conflicting.txt", "removed-in-branch.txt", NULL },
	{ "automergeable.txt", "changed-in-branch.txt", "conflicting.txt", "removed-in-branch.txt", NULL },
	{ NULL },
};

static char *result_contents[4][6] = {
	{ "automergeable.txt", AUTOMERGEABLE_MERGED_FILE, NULL, NULL },
	{ "changed-in-branch.txt", CHANGED_IN_BRANCH_FILE, NULL, NULL },
	{ "automergeable.txt", AUTOMERGEABLE_MERGED_FILE, "changed-in-branch.txt", CHANGED_IN_BRANCH_FILE, NULL, NULL },
	{ NULL }
};

void test_merge_workdir_dirty__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
	git_repository_index(&repo_index, repo);
}

void test_merge_workdir_dirty__cleanup(void)
{
	git_index_free(repo_index);
	cl_git_sandbox_cleanup();
}

static void set_core_autocrlf_to(git_repository *repo, bool value)
{
	git_config *cfg;

	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_bool(cfg, "core.autocrlf", value));

	git_config_free(cfg);
}

static int merge_branch(void)
{
	git_oid their_oids[1];
	git_annotated_commit *their_head;
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	int error;

	cl_git_pass(git_oid_fromstr(&their_oids[0], MERGE_BRANCH_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_head, repo, &their_oids[0]));

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;
	error = git_merge(repo, (const git_annotated_commit **)&their_head, 1, &merge_opts, &checkout_opts);

	git_annotated_commit_free(their_head);

	return error;
}

static void write_files(char *files[])
{
	char *filename;
	git_buf path = GIT_BUF_INIT, content = GIT_BUF_INIT;
	size_t i;

	for (i = 0, filename = files[i]; filename; filename = files[++i]) {
		git_buf_clear(&path);
		git_buf_clear(&content);

		git_buf_printf(&path, "%s/%s", TEST_REPO_PATH, filename);
		git_buf_printf(&content, "This is a dirty file in the working directory!\n\n"
			"It will not be staged!  Its filename is %s.\n", filename);

		cl_git_mkfile(path.ptr, content.ptr);
	}

	git_buf_free(&path);
	git_buf_free(&content);
}

static void hack_index(char *files[])
{
	char *filename;
	struct stat statbuf;
	git_buf path = GIT_BUF_INIT;
	git_index_entry *entry;
	struct p_timeval times[2];
	time_t now;
	size_t i;

	/* Update the index to suggest that checkout placed these files on
	 * disk, keeping the object id but updating the cache, which will
	 * emulate a Git implementation's different filter.
	 *
	 * We set the file's timestamp to before now to pretend that
	 * it was an old checkout so we don't trigger the racy
	 * protections would would check the content.
	 */

	now = time(NULL);
	times[0].tv_sec  = now - 5;
	times[0].tv_usec = 0;
	times[1].tv_sec  = now - 5;
	times[1].tv_usec = 0;

	for (i = 0, filename = files[i]; filename; filename = files[++i]) {
		git_buf_clear(&path);

		cl_assert(entry = (git_index_entry *)
			git_index_get_bypath(repo_index, filename, 0));

		cl_git_pass(git_buf_printf(&path, "%s/%s", TEST_REPO_PATH, filename));
		cl_git_pass(p_utimes(path.ptr, times));
		cl_git_pass(p_stat(path.ptr, &statbuf));

		entry->ctime.seconds = (int32_t)statbuf.st_ctime;
		entry->mtime.seconds = (int32_t)statbuf.st_mtime;
#if defined(GIT_USE_NSEC)
		entry->ctime.nanoseconds = statbuf.st_ctime_nsec;
		entry->mtime.nanoseconds = statbuf.st_mtime_nsec;
#else
		entry->ctime.nanoseconds = 0;
		entry->mtime.nanoseconds = 0;
#endif
		entry->dev = statbuf.st_dev;
		entry->ino = statbuf.st_ino;
		entry->uid  = statbuf.st_uid;
		entry->gid  = statbuf.st_gid;
		entry->file_size = (uint32_t)statbuf.st_size;
	}

	git_buf_free(&path);
}

static void stage_random_files(char *files[])
{
	char *filename;
	size_t i;

	write_files(files);

	for (i = 0, filename = files[i]; filename; filename = files[++i])
		cl_git_pass(git_index_add_bypath(repo_index, filename));
}

static void stage_content(char *content[])
{
	git_reference *head;
	git_object *head_object;
	git_buf path = GIT_BUF_INIT;
	char *filename, *text;
	size_t i;

	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_peel(&head_object, head, GIT_OBJ_COMMIT));
	cl_git_pass(git_reset(repo, head_object, GIT_RESET_HARD, NULL));

	for (i = 0, filename = content[i], text = content[++i];
		filename && text;
		filename = content[++i], text = content[++i]) {

		git_buf_clear(&path);

		cl_git_pass(git_buf_printf(&path, "%s/%s", TEST_REPO_PATH, filename));

		cl_git_mkfile(path.ptr, text);
		cl_git_pass(git_index_add_bypath(repo_index, filename));
	}

	git_object_free(head_object);
	git_reference_free(head);
	git_buf_free(&path);
}

static int merge_dirty_files(char *dirty_files[])
{
	git_reference *head;
	git_object *head_object;
	int error;

	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_peel(&head_object, head, GIT_OBJ_COMMIT));
	cl_git_pass(git_reset(repo, head_object, GIT_RESET_HARD, NULL));

	write_files(dirty_files);

	error = merge_branch();

	git_object_free(head_object);
	git_reference_free(head);

	return error;
}

static int merge_differently_filtered_files(char *files[])
{
	git_reference *head;
	git_object *head_object;
	int error;

	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_peel(&head_object, head, GIT_OBJ_COMMIT));
	cl_git_pass(git_reset(repo, head_object, GIT_RESET_HARD, NULL));

	/* Emulate checkout with a broken or misconfigured filter:  modify some
	 * files on-disk and then update the index with the updated file size
	 * and time, as if some filter applied them.  These files should not be
	 * treated as dirty since we created them.
	 *
	 * (Make sure to update the index stamp to defeat racy-git protections
	 * trying to sanity check the files in the index; those would rehash the
	 * files, showing them as dirty, the exact mechanism we're trying to avoid.)
	 */

	write_files(files);
	hack_index(files);

	cl_git_pass(git_index_write(repo_index));

	error = merge_branch();

	git_object_free(head_object);
	git_reference_free(head);

	return error;
}

static int merge_staged_files(char *staged_files[])
{	
	stage_random_files(staged_files);
	return merge_branch();
}

void test_merge_workdir_dirty__unaffected_dirty_files_allowed(void)
{
	char **files;
	size_t i;

	for (i = 0, files = unaffected[i]; files[0]; files = unaffected[++i])
		cl_git_pass(merge_dirty_files(files));
}

void test_merge_workdir_dirty__unstaged_deletes_maintained(void)
{
	git_reference *head;
	git_object *head_object;

	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_peel(&head_object, head, GIT_OBJ_COMMIT));
	cl_git_pass(git_reset(repo, head_object, GIT_RESET_HARD, NULL));

	cl_git_pass(p_unlink("merge-resolve/unchanged.txt"));

	cl_git_pass(merge_branch());

	git_object_free(head_object);
	git_reference_free(head);
}

void test_merge_workdir_dirty__affected_dirty_files_disallowed(void)
{
	char **files;
	size_t i;

	for (i = 0, files = affected[i]; files[0]; files = affected[++i])
		cl_git_fail(merge_dirty_files(files));
}

void test_merge_workdir_dirty__staged_files_in_index_disallowed(void)
{
	char **files;
	size_t i;

	for (i = 0, files = unaffected[i]; files[0]; files = unaffected[++i])
		cl_git_fail(merge_staged_files(files));

	for (i = 0, files = affected[i]; files[0]; files = affected[++i])
		cl_git_fail(merge_staged_files(files));
}

void test_merge_workdir_dirty__identical_staged_files_allowed(void)
{
	char **content;
	size_t i;

	set_core_autocrlf_to(repo, false);
	
	for (i = 0, content = result_contents[i]; content[0]; content = result_contents[++i]) {
		stage_content(content);

		git_index_write(repo_index);
		cl_git_pass(merge_branch());
	}
}

void test_merge_workdir_dirty__honors_cache(void)
{
	char **files;
	size_t i;

	for (i = 0, files = affected[i]; files[0]; files = affected[++i])
		cl_git_pass(merge_differently_filtered_files(files));
}
