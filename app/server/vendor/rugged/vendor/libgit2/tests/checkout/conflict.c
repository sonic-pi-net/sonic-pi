#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/sys/index.h"
#include "fileops.h"

static git_repository *g_repo;
static git_index *g_index;

#define TEST_REPO_PATH "merge-resolve"

#define CONFLICTING_ANCESTOR_OID   "d427e0b2e138501a3d15cc376077a3631e15bd46"
#define CONFLICTING_OURS_OID       "4e886e602529caa9ab11d71f86634bd1b6e0de10"
#define CONFLICTING_THEIRS_OID     "2bd0a343aeef7a2cf0d158478966a6e587ff3863"

#define AUTOMERGEABLE_ANCESTOR_OID "6212c31dab5e482247d7977e4f0dd3601decf13b"
#define AUTOMERGEABLE_OURS_OID     "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf"
#define AUTOMERGEABLE_THEIRS_OID   "058541fc37114bfc1dddf6bd6bffc7fae5c2e6fe"

#define LINK_ANCESTOR_OID          "1a010b1c0f081b2e8901d55307a15c29ff30af0e"
#define LINK_OURS_OID              "72ea499e108df5ff0a4a913e7655bbeeb1fb69f2"
#define LINK_THEIRS_OID            "8bfb012a6d809e499bd8d3e194a3929bc8995b93"

#define LINK_ANCESTOR_TARGET       "file"
#define LINK_OURS_TARGET           "other-file"
#define LINK_THEIRS_TARGET         "still-another-file"

#define CONFLICTING_OURS_FILE \
	"this file is changed in master and branch\n"
#define CONFLICTING_THEIRS_FILE \
	"this file is changed in branch and master\n"
#define CONFLICTING_DIFF3_FILE \
	"<<<<<<< ours\n" \
	"this file is changed in master and branch\n" \
	"=======\n" \
	"this file is changed in branch and master\n" \
	">>>>>>> theirs\n"

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

struct checkout_index_entry {
	uint16_t mode;
	char oid_str[GIT_OID_HEXSZ+1];
	int stage;
	char path[128];
};

struct checkout_name_entry {
	char ancestor[64];
	char ours[64];
	char theirs[64];
};

void test_checkout_conflict__initialize(void)
{
	git_config *cfg;

	g_repo = cl_git_sandbox_init(TEST_REPO_PATH);
	git_repository_index(&g_index, g_repo);

	cl_git_rewritefile(
		TEST_REPO_PATH "/.gitattributes",
		"* text eol=lf\n");

	/* Ensure that the user's merge.conflictstyle doesn't interfere */
	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_set_string(cfg, "merge.conflictstyle", "merge"));
	git_config_free(cfg);
}

void test_checkout_conflict__cleanup(void)
{
	git_index_free(g_index);
	cl_git_sandbox_cleanup();
}

static void create_index(struct checkout_index_entry *entries, size_t entries_len)
{
	git_buf path = GIT_BUF_INIT;
	size_t i;

	for (i = 0; i < entries_len; i++) {
		git_buf_joinpath(&path, TEST_REPO_PATH, entries[i].path);

		if (entries[i].stage == 3 && (i == 0 || strcmp(entries[i-1].path, entries[i].path) != 0 || entries[i-1].stage != 2))
			p_unlink(git_buf_cstr(&path));

		git_index_remove_bypath(g_index, entries[i].path);
	}

	for (i = 0; i < entries_len; i++) {
		git_index_entry entry;

		memset(&entry, 0x0, sizeof(git_index_entry));

		entry.mode = entries[i].mode;
		entry.flags = entries[i].stage << GIT_IDXENTRY_STAGESHIFT;
		git_oid_fromstr(&entry.id, entries[i].oid_str);
		entry.path = entries[i].path;

		cl_git_pass(git_index_add(g_index, &entry));
	}

	git_buf_free(&path);
}

static void create_index_names(struct checkout_name_entry *entries, size_t entries_len)
{
	size_t i;

	for (i = 0; i < entries_len; i++) {
		cl_git_pass(git_index_name_add(g_index,
			strlen(entries[i].ancestor) == 0 ? NULL : entries[i].ancestor,
			strlen(entries[i].ours) == 0 ? NULL : entries[i].ours,
			strlen(entries[i].theirs) == 0 ? NULL : entries[i].theirs));
	}
}

static void create_conflicting_index(void)
{
	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "conflicting.txt" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "conflicting.txt" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "conflicting.txt" },
	};

	create_index(checkout_index_entries, 3);
	git_index_write(g_index);
}

static void ensure_workdir_contents(const char *path, const char *contents)
{
	git_buf fullpath = GIT_BUF_INIT, data_buf = GIT_BUF_INIT;

	cl_git_pass(
		git_buf_joinpath(&fullpath, git_repository_workdir(g_repo), path));

	cl_git_pass(git_futils_readbuffer(&data_buf, git_buf_cstr(&fullpath)));
	cl_assert(strcmp(git_buf_cstr(&data_buf), contents) == 0);

	git_buf_free(&fullpath);
	git_buf_free(&data_buf);
}

static void ensure_workdir_oid(const char *path, const char *oid_str)
{
	git_oid expected, actual;

	cl_git_pass(git_oid_fromstr(&expected, oid_str));
	cl_git_pass(git_repository_hashfile(&actual, g_repo, path, GIT_OBJ_BLOB, NULL));
	cl_assert_equal_oid(&expected, &actual);
}

static void ensure_workdir_mode(const char *path, int mode)
{
#ifndef GIT_WIN32
	git_buf fullpath = GIT_BUF_INIT;
	struct stat st;

	cl_git_pass(
		git_buf_joinpath(&fullpath, git_repository_workdir(g_repo), path));

	cl_git_pass(p_stat(git_buf_cstr(&fullpath), &st));
	cl_assert_equal_i((mode & S_IRWXU), (st.st_mode & S_IRWXU));

	git_buf_free(&fullpath);
#endif
}

static void ensure_workdir(const char *path, int mode, const char *oid_str)
{
	ensure_workdir_mode(path, mode);
	ensure_workdir_oid(path, oid_str);
}

static void ensure_workdir_link(const char *path, const char *target)
{
#ifdef GIT_WIN32
	ensure_workdir_contents(path, target);
#else
	git_buf fullpath = GIT_BUF_INIT;
	char actual[1024];
	struct stat st;
	int len;

	cl_git_pass(
		git_buf_joinpath(&fullpath, git_repository_workdir(g_repo), path));

	cl_git_pass(p_lstat(git_buf_cstr(&fullpath), &st));
	cl_assert(S_ISLNK(st.st_mode));

	cl_assert((len = p_readlink(git_buf_cstr(&fullpath), actual, 1024)) > 0);
	actual[len] = '\0';
	cl_assert(strcmp(actual, target) == 0);

	git_buf_free(&fullpath);
#endif
}

void test_checkout_conflict__ignored(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy |= GIT_CHECKOUT_SKIP_UNMERGED;

	create_conflicting_index();
	cl_git_pass(p_unlink(TEST_REPO_PATH "/conflicting.txt"));

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	cl_assert(!git_path_exists(TEST_REPO_PATH "/conflicting.txt"));
}

void test_checkout_conflict__ours(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy |= GIT_CHECKOUT_USE_OURS;

	create_conflicting_index();

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir_contents("conflicting.txt", CONFLICTING_OURS_FILE);
}

void test_checkout_conflict__theirs(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy |= GIT_CHECKOUT_USE_THEIRS;

	create_conflicting_index();

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir_contents("conflicting.txt", CONFLICTING_THEIRS_FILE);

}

void test_checkout_conflict__diff3(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	create_conflicting_index();

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir_contents("conflicting.txt", CONFLICTING_DIFF3_FILE);
}

void test_checkout_conflict__automerge(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, AUTOMERGEABLE_ANCESTOR_OID, 1, "automergeable.txt" },
		{ 0100644, AUTOMERGEABLE_OURS_OID, 2, "automergeable.txt" },
		{ 0100644, AUTOMERGEABLE_THEIRS_OID, 3, "automergeable.txt" },
	};

	create_index(checkout_index_entries, 3);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir_contents("automergeable.txt", AUTOMERGEABLE_MERGED_FILE);
}

void test_checkout_conflict__directory_file(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "df-1" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "df-1" },
		{ 0100644, CONFLICTING_THEIRS_OID, 0, "df-1/file" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "df-2" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "df-2" },
		{ 0100644, CONFLICTING_OURS_OID, 0, "df-2/file" },

		{ 0100644, CONFLICTING_THEIRS_OID, 3, "df-3" },
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "df-3/file" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "df-3/file" },

		{ 0100644, CONFLICTING_OURS_OID, 2, "df-4" },
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "df-4/file" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "df-4/file" },
	};

	opts.checkout_strategy |= GIT_CHECKOUT_SAFE;

	create_index(checkout_index_entries, 12);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir_oid("df-1/file", CONFLICTING_THEIRS_OID);
	ensure_workdir_oid("df-1~ours", CONFLICTING_OURS_OID);
	ensure_workdir_oid("df-2/file", CONFLICTING_OURS_OID);
	ensure_workdir_oid("df-2~theirs", CONFLICTING_THEIRS_OID);
	ensure_workdir_oid("df-3/file", CONFLICTING_OURS_OID);
	ensure_workdir_oid("df-3~theirs", CONFLICTING_THEIRS_OID);
	ensure_workdir_oid("df-4~ours", CONFLICTING_OURS_OID);
	ensure_workdir_oid("df-4/file", CONFLICTING_THEIRS_OID);
}

void test_checkout_conflict__directory_file_with_custom_labels(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "df-1" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "df-1" },
		{ 0100644, CONFLICTING_THEIRS_OID, 0, "df-1/file" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "df-2" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "df-2" },
		{ 0100644, CONFLICTING_OURS_OID, 0, "df-2/file" },

		{ 0100644, CONFLICTING_THEIRS_OID, 3, "df-3" },
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "df-3/file" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "df-3/file" },

		{ 0100644, CONFLICTING_OURS_OID, 2, "df-4" },
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "df-4/file" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "df-4/file" },
	};

	opts.checkout_strategy |= GIT_CHECKOUT_SAFE;
	opts.our_label = "HEAD";
	opts.their_label = "branch";

	create_index(checkout_index_entries, 12);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir_oid("df-1/file", CONFLICTING_THEIRS_OID);
	ensure_workdir_oid("df-1~HEAD", CONFLICTING_OURS_OID);
	ensure_workdir_oid("df-2/file", CONFLICTING_OURS_OID);
	ensure_workdir_oid("df-2~branch", CONFLICTING_THEIRS_OID);
	ensure_workdir_oid("df-3/file", CONFLICTING_OURS_OID);
	ensure_workdir_oid("df-3~branch", CONFLICTING_THEIRS_OID);
	ensure_workdir_oid("df-4~HEAD", CONFLICTING_OURS_OID);
	ensure_workdir_oid("df-4/file", CONFLICTING_THEIRS_OID);
}

void test_checkout_conflict__link_file(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "link-1" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "link-1" },
		{ 0120000, LINK_THEIRS_OID, 3, "link-1" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "link-2" },
		{ 0120000, LINK_OURS_OID, 2, "link-2" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "link-2" },

		{ 0120000, LINK_ANCESTOR_OID, 1, "link-3" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "link-3" },
		{ 0120000, LINK_THEIRS_OID, 3, "link-3" },

		{ 0120000, LINK_ANCESTOR_OID, 1, "link-4" },
		{ 0120000, LINK_OURS_OID, 2, "link-4" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "link-4" },
	};

	opts.checkout_strategy |= GIT_CHECKOUT_SAFE;

	create_index(checkout_index_entries, 12);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	/* Typechange conflicts always keep the file in the workdir */
	ensure_workdir_oid("link-1", CONFLICTING_OURS_OID);
	ensure_workdir_oid("link-2", CONFLICTING_THEIRS_OID);
	ensure_workdir_oid("link-3", CONFLICTING_OURS_OID);
	ensure_workdir_oid("link-4", CONFLICTING_THEIRS_OID);
}

void test_checkout_conflict__links(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0120000, LINK_ANCESTOR_OID, 1, "link-1" },
		{ 0120000, LINK_OURS_OID, 2, "link-1" },
		{ 0120000, LINK_THEIRS_OID, 3, "link-1" },

		{ 0120000, LINK_OURS_OID, 2, "link-2" },
		{ 0120000, LINK_THEIRS_OID, 3, "link-2" },
	};

	opts.checkout_strategy |= GIT_CHECKOUT_SAFE;

	create_index(checkout_index_entries, 5);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	/* Conflicts with links always keep the ours side (even with -Xtheirs) */
	ensure_workdir_link("link-1", LINK_OURS_TARGET);
	ensure_workdir_link("link-2", LINK_OURS_TARGET);
}

void test_checkout_conflict__add_add(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, CONFLICTING_OURS_OID, 2, "conflicting.txt" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "conflicting.txt" },
	};

	opts.checkout_strategy |= GIT_CHECKOUT_SAFE;

	create_index(checkout_index_entries, 2);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	/* Add/add writes diff3 files */
	ensure_workdir_contents("conflicting.txt", CONFLICTING_DIFF3_FILE);
}

void test_checkout_conflict__mode_change(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "executable-1" },
		{ 0100755, CONFLICTING_ANCESTOR_OID, 2, "executable-1" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "executable-1" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "executable-2" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "executable-2" },
		{ 0100755, CONFLICTING_ANCESTOR_OID, 3, "executable-2" },

		{ 0100755, CONFLICTING_ANCESTOR_OID, 1, "executable-3" },
		{ 0100644, CONFLICTING_ANCESTOR_OID, 2, "executable-3" },
		{ 0100755, CONFLICTING_THEIRS_OID, 3, "executable-3" },

		{ 0100755, CONFLICTING_ANCESTOR_OID, 1, "executable-4" },
		{ 0100755, CONFLICTING_OURS_OID, 2, "executable-4" },
		{ 0100644, CONFLICTING_ANCESTOR_OID, 3, "executable-4" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "executable-5" },
		{ 0100755, CONFLICTING_OURS_OID, 2, "executable-5" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "executable-5" },

		{ 0100755, CONFLICTING_ANCESTOR_OID, 1, "executable-6" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "executable-6" },
		{ 0100755, CONFLICTING_THEIRS_OID, 3, "executable-6" },
	};

	opts.checkout_strategy |= GIT_CHECKOUT_SAFE;

	create_index(checkout_index_entries, 18);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	/* Keep the modified mode */
	ensure_workdir_oid("executable-1", CONFLICTING_THEIRS_OID);
	ensure_workdir_mode("executable-1", 0100755);

	ensure_workdir_oid("executable-2", CONFLICTING_OURS_OID);
	ensure_workdir_mode("executable-2", 0100755);

	ensure_workdir_oid("executable-3", CONFLICTING_THEIRS_OID);
	ensure_workdir_mode("executable-3", 0100644);

	ensure_workdir_oid("executable-4", CONFLICTING_OURS_OID);
	ensure_workdir_mode("executable-4", 0100644);

	ensure_workdir_contents("executable-5", CONFLICTING_DIFF3_FILE);
	ensure_workdir_mode("executable-5", 0100755);

	ensure_workdir_contents("executable-6", CONFLICTING_DIFF3_FILE);
	ensure_workdir_mode("executable-6", 0100644);
}

void test_checkout_conflict__renames(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e", 0, "0a-no-change.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-duplicated-in-ours.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 1, "0b-rewritten-in-ours.txt" },
		{ 0100644, "e376fbdd06ebf021c92724da9f26f44212734e3e", 2, "0b-rewritten-in-ours.txt" },
		{ 0100644, "b2d399ae15224e1d58066e3c8df70ce37de7a656", 3, "0b-rewritten-in-ours.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-duplicated-in-theirs.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 1, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "efc9121fdedaf08ba180b53ebfbcf71bd488ed09", 2, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "712ebba6669ea847d9829e4f1059d6c830c8b531", 3, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "0d872f8e871a30208305978ecbf9e66d864f1638", 0, "1a-newname-in-ours-edited-in-theirs.txt" },
		{ 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-newname-in-ours.txt" },
		{ 0100644, "ed9523e62e453e50dd9be1606af19399b96e397a", 0, "1b-newname-in-theirs-edited-in-ours.txt" },
		{ 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-newname-in-theirs.txt" },
		{ 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 2, "3a-newname-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 1, "3a-renamed-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 3, "3b-newname-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 1, "3b-renamed-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 2, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "8b5b53cb2aa9ceb1139f5312fcfa3cc3c5a47c9a", 3, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 1, "4a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9", 2, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 3, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 1, "4b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 2, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "98ba4205fcf31f5dd93c916d35fe3f3b3d0e6714", 3, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 1, "5a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 3, "5a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced", 2, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 3, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 1, "5b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 2, "5b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 2, "6-both-renamed-1-to-2-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 3, "6-both-renamed-1-to-2-theirs.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 1, "6-both-renamed-1-to-2.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 1, "7-both-renamed-side-1.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 3, "7-both-renamed-side-1.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 1, "7-both-renamed-side-2.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 2, "7-both-renamed-side-2.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 2, "7-both-renamed.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 3, "7-both-renamed.txt" }
	};

	struct checkout_name_entry checkout_name_entries[] = {
		{
			"3a-renamed-in-ours-deleted-in-theirs.txt",
			"3a-newname-in-ours-deleted-in-theirs.txt",
			""
		},

		{
			"3b-renamed-in-theirs-deleted-in-ours.txt",
			"",
			"3b-newname-in-theirs-deleted-in-ours.txt"
		},

		{
			"4a-renamed-in-ours-added-in-theirs.txt",
			"4a-newname-in-ours-added-in-theirs.txt",
			""
		},

		{
			"4b-renamed-in-theirs-added-in-ours.txt",
			"",
			"4b-newname-in-theirs-added-in-ours.txt"
		},

		{
			"5a-renamed-in-ours-added-in-theirs.txt",
			"5a-newname-in-ours-added-in-theirs.txt",
			"5a-renamed-in-ours-added-in-theirs.txt"
		},

		{
			"5b-renamed-in-theirs-added-in-ours.txt",
			"5b-renamed-in-theirs-added-in-ours.txt",
			"5b-newname-in-theirs-added-in-ours.txt"
		},

		{
			"6-both-renamed-1-to-2.txt",
			"6-both-renamed-1-to-2-ours.txt",
			"6-both-renamed-1-to-2-theirs.txt"
		},

		{
			"7-both-renamed-side-1.txt",
			"7-both-renamed.txt",
			"7-both-renamed-side-1.txt"
		},

		{
			"7-both-renamed-side-2.txt",
			"7-both-renamed-side-2.txt",
			"7-both-renamed.txt"
		}
	};

	opts.checkout_strategy |= GIT_CHECKOUT_SAFE;

	create_index(checkout_index_entries, 41);
	create_index_names(checkout_name_entries, 9);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir("0a-no-change.txt",
		0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e");

	ensure_workdir("0b-duplicated-in-ours.txt",
		0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6");

	ensure_workdir("0b-rewritten-in-ours.txt",
		0100644, "4c7e515d6d52d820496858f2f059ece69e99e2e3");

	ensure_workdir("0c-duplicated-in-theirs.txt",
		0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31");

	ensure_workdir("0c-rewritten-in-theirs.txt",
		0100644, "4648d658682d1155c2a3db5b0c53305e26884ea5");

	ensure_workdir("1a-newname-in-ours-edited-in-theirs.txt",
		0100644, "0d872f8e871a30208305978ecbf9e66d864f1638");

	ensure_workdir("1a-newname-in-ours.txt",
		0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb");

	ensure_workdir("1b-newname-in-theirs-edited-in-ours.txt",
		0100644, "ed9523e62e453e50dd9be1606af19399b96e397a");

	ensure_workdir("1b-newname-in-theirs.txt",
		0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136");

	ensure_workdir("2-newname-in-both.txt",
		0100644, "178940b450f238a56c0d75b7955cb57b38191982");

	ensure_workdir("3a-newname-in-ours-deleted-in-theirs.txt",
		0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9");

	ensure_workdir("3b-newname-in-theirs-deleted-in-ours.txt",
		0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495");

	ensure_workdir("4a-newname-in-ours-added-in-theirs.txt~ours",
		0100644, "227792b52aaa0b238bea00ec7e509b02623f168c");

	ensure_workdir("4a-newname-in-ours-added-in-theirs.txt~theirs",
		0100644, "8b5b53cb2aa9ceb1139f5312fcfa3cc3c5a47c9a");

	ensure_workdir("4b-newname-in-theirs-added-in-ours.txt~ours",
		0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9");

	ensure_workdir("4b-newname-in-theirs-added-in-ours.txt~theirs",
		0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db");

	ensure_workdir("5a-newname-in-ours-added-in-theirs.txt~ours",
		0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436");

	ensure_workdir("5a-newname-in-ours-added-in-theirs.txt~theirs",
		0100644, "98ba4205fcf31f5dd93c916d35fe3f3b3d0e6714");

	ensure_workdir("5b-newname-in-theirs-added-in-ours.txt~ours",
		0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced");

	ensure_workdir("5b-newname-in-theirs-added-in-ours.txt~theirs",
		0100644, "63247125386de9ec90a27ad36169307bf8a11a38");

	ensure_workdir("6-both-renamed-1-to-2-ours.txt",
		0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450");

	ensure_workdir("6-both-renamed-1-to-2-theirs.txt",
		0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450");

	ensure_workdir("7-both-renamed.txt~ours",
		0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11");

	ensure_workdir("7-both-renamed.txt~theirs",
		0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07");
}

void test_checkout_conflict__rename_keep_ours(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	
	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e", 0, "0a-no-change.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-duplicated-in-ours.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 1, "0b-rewritten-in-ours.txt" },
		{ 0100644, "e376fbdd06ebf021c92724da9f26f44212734e3e", 2, "0b-rewritten-in-ours.txt" },
		{ 0100644, "b2d399ae15224e1d58066e3c8df70ce37de7a656", 3, "0b-rewritten-in-ours.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-duplicated-in-theirs.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 1, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "efc9121fdedaf08ba180b53ebfbcf71bd488ed09", 2, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "712ebba6669ea847d9829e4f1059d6c830c8b531", 3, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "0d872f8e871a30208305978ecbf9e66d864f1638", 0, "1a-newname-in-ours-edited-in-theirs.txt" },
		{ 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-newname-in-ours.txt" },
		{ 0100644, "ed9523e62e453e50dd9be1606af19399b96e397a", 0, "1b-newname-in-theirs-edited-in-ours.txt" },
		{ 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-newname-in-theirs.txt" },
		{ 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 2, "3a-newname-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 1, "3a-renamed-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 3, "3b-newname-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 1, "3b-renamed-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 2, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "8b5b53cb2aa9ceb1139f5312fcfa3cc3c5a47c9a", 3, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 1, "4a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9", 2, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 3, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 1, "4b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 2, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "98ba4205fcf31f5dd93c916d35fe3f3b3d0e6714", 3, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 1, "5a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 3, "5a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced", 2, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 3, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 1, "5b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 2, "5b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 2, "6-both-renamed-1-to-2-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 3, "6-both-renamed-1-to-2-theirs.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 1, "6-both-renamed-1-to-2.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 1, "7-both-renamed-side-1.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 3, "7-both-renamed-side-1.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 1, "7-both-renamed-side-2.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 2, "7-both-renamed-side-2.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 2, "7-both-renamed.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 3, "7-both-renamed.txt" }
	};
	
	struct checkout_name_entry checkout_name_entries[] = {
		{
			"3a-renamed-in-ours-deleted-in-theirs.txt",
			"3a-newname-in-ours-deleted-in-theirs.txt",
			""
		},
		
		{
			"3b-renamed-in-theirs-deleted-in-ours.txt",
			"",
			"3b-newname-in-theirs-deleted-in-ours.txt"
		},
		
		{
			"4a-renamed-in-ours-added-in-theirs.txt",
			"4a-newname-in-ours-added-in-theirs.txt",
			""
		},
		
		{
			"4b-renamed-in-theirs-added-in-ours.txt",
			"",
			"4b-newname-in-theirs-added-in-ours.txt"
		},
		
		{
			"5a-renamed-in-ours-added-in-theirs.txt",
			"5a-newname-in-ours-added-in-theirs.txt",
			"5a-renamed-in-ours-added-in-theirs.txt"
		},
		
		{
			"5b-renamed-in-theirs-added-in-ours.txt",
			"5b-renamed-in-theirs-added-in-ours.txt",
			"5b-newname-in-theirs-added-in-ours.txt"
		},
		
		{
			"6-both-renamed-1-to-2.txt",
			"6-both-renamed-1-to-2-ours.txt",
			"6-both-renamed-1-to-2-theirs.txt"
		},
		
		{
			"7-both-renamed-side-1.txt",
			"7-both-renamed.txt",
			"7-both-renamed-side-1.txt"
		},
		
		{
			"7-both-renamed-side-2.txt",
			"7-both-renamed-side-2.txt",
			"7-both-renamed.txt"
		}
	};
	
	opts.checkout_strategy |= GIT_CHECKOUT_SAFE | GIT_CHECKOUT_USE_OURS;
	
	create_index(checkout_index_entries, 41);
	create_index_names(checkout_name_entries, 9);
	git_index_write(g_index);
	
	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));
		
	ensure_workdir("0a-no-change.txt",
				   0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e");
	
	ensure_workdir("0b-duplicated-in-ours.txt",
				   0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6");
	
	ensure_workdir("0b-rewritten-in-ours.txt",
				   0100644, "e376fbdd06ebf021c92724da9f26f44212734e3e");
	
	ensure_workdir("0c-duplicated-in-theirs.txt",
				   0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31");
	
	ensure_workdir("0c-rewritten-in-theirs.txt",
				   0100644, "efc9121fdedaf08ba180b53ebfbcf71bd488ed09");
	
	ensure_workdir("1a-newname-in-ours-edited-in-theirs.txt",
				   0100644, "0d872f8e871a30208305978ecbf9e66d864f1638");
	
	ensure_workdir("1a-newname-in-ours.txt",
				   0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb");
	
	ensure_workdir("1b-newname-in-theirs-edited-in-ours.txt",
				   0100644, "ed9523e62e453e50dd9be1606af19399b96e397a");
	
	ensure_workdir("1b-newname-in-theirs.txt",
				   0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136");
	
	ensure_workdir("2-newname-in-both.txt",
				   0100644, "178940b450f238a56c0d75b7955cb57b38191982");

	ensure_workdir("3a-newname-in-ours-deleted-in-theirs.txt",
				   0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9");
	
	ensure_workdir("3b-newname-in-theirs-deleted-in-ours.txt",
				   0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495");
	
	ensure_workdir("4a-newname-in-ours-added-in-theirs.txt",
				   0100644, "227792b52aaa0b238bea00ec7e509b02623f168c");
	
	ensure_workdir("4b-newname-in-theirs-added-in-ours.txt",
				   0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9");
	
	ensure_workdir("5a-newname-in-ours-added-in-theirs.txt",
				   0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436");
		
	ensure_workdir("5b-newname-in-theirs-added-in-ours.txt",
				   0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced");

	ensure_workdir("6-both-renamed-1-to-2-ours.txt",
				   0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450");
	
	ensure_workdir("7-both-renamed.txt",
				   0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11");
}

void test_checkout_conflict__name_mangled_file_exists_in_workdir(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 1, "test-one-side-one.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 3, "test-one-side-one.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 1, "test-one-side-two.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 2, "test-one-side-two.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 2, "test-one.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 3, "test-one.txt" },

		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 1, "test-two-side-one.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 3, "test-two-side-one.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 1, "test-two-side-two.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 2, "test-two-side-two.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 2, "test-two.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 3, "test-two.txt" },

		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 1, "test-three-side-one.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 3, "test-three-side-one.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 1, "test-three-side-two.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 2, "test-three-side-two.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 2, "test-three.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 3, "test-three.txt" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "directory_file-one" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "directory_file-one" },
		{ 0100644, CONFLICTING_THEIRS_OID, 0, "directory_file-one/file" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "directory_file-two" },
		{ 0100644, CONFLICTING_OURS_OID, 0, "directory_file-two/file" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "directory_file-two" },
	};

	struct checkout_name_entry checkout_name_entries[] = {
		{
			"test-one-side-one.txt",
			"test-one.txt",
			"test-one-side-one.txt"
		},
		{
			"test-one-side-two.txt",
			"test-one-side-two.txt",
			"test-one.txt"
		},

		{
			"test-two-side-one.txt",
			"test-two.txt",
			"test-two-side-one.txt"
		},
		{
			"test-two-side-two.txt",
			"test-two-side-two.txt",
			"test-two.txt"
		},

		{
			"test-three-side-one.txt",
			"test-three.txt",
			"test-three-side-one.txt"
		},
		{
			"test-three-side-two.txt",
			"test-three-side-two.txt",
			"test-three.txt"
		}
	};

	opts.checkout_strategy |= GIT_CHECKOUT_SAFE;

	create_index(checkout_index_entries, 24);
	create_index_names(checkout_name_entries, 6);
	git_index_write(g_index);

	/* Add some files on disk that conflict with the names that would be chosen
	 * for the files written for each side. */

	cl_git_rewritefile("merge-resolve/test-one.txt~ours",
		"Expect index contents to be written to ~ours_0");
	cl_git_rewritefile("merge-resolve/test-one.txt~theirs",
		"Expect index contents to be written to ~theirs_0");

	cl_git_rewritefile("merge-resolve/test-two.txt~ours",
		"Expect index contents to be written to ~ours_3");
	cl_git_rewritefile("merge-resolve/test-two.txt~theirs",
		"Expect index contents to be written to ~theirs_3");
	cl_git_rewritefile("merge-resolve/test-two.txt~ours_0",
		"Expect index contents to be written to ~ours_3");
	cl_git_rewritefile("merge-resolve/test-two.txt~theirs_0",
		"Expect index contents to be written to ~theirs_3");
	cl_git_rewritefile("merge-resolve/test-two.txt~ours_1",
		"Expect index contents to be written to ~ours_3");
	cl_git_rewritefile("merge-resolve/test-two.txt~theirs_1",
		"Expect index contents to be written to ~theirs_3");
	cl_git_rewritefile("merge-resolve/test-two.txt~ours_2",
		"Expect index contents to be written to ~ours_3");
	cl_git_rewritefile("merge-resolve/test-two.txt~theirs_2",
		"Expect index contents to be written to ~theirs_3");

	cl_git_rewritefile("merge-resolve/test-three.txt~Ours",
		"Expect case insensitive filesystems to create ~ours_0");
	cl_git_rewritefile("merge-resolve/test-three.txt~THEIRS",
		"Expect case insensitive filesystems to create ~theirs_0");

	cl_git_rewritefile("merge-resolve/directory_file-one~ours",
		"Index contents written to ~ours_0 in this D/F conflict");
	cl_git_rewritefile("merge-resolve/directory_file-two~theirs",
		"Index contents written to ~theirs_0 in this D/F conflict");

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir("test-one.txt~ours_0",
		0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11");
	ensure_workdir("test-one.txt~theirs_0",
		0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07");

	ensure_workdir("test-two.txt~ours_3",
		0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11");
	ensure_workdir("test-two.txt~theirs_3",
		0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07");

	/* Name is mangled on case insensitive only */
#if defined(GIT_WIN32) || defined(__APPLE__)
	ensure_workdir("test-three.txt~ours_0",
		0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11");
	ensure_workdir("test-three.txt~theirs_0",
		0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07");
#else
	ensure_workdir("test-three.txt~ours",
		0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11");
	ensure_workdir("test-three.txt~theirs",
		0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07");
#endif

	ensure_workdir("directory_file-one~ours_0", 0100644, CONFLICTING_OURS_OID);
	ensure_workdir("directory_file-two~theirs_0", 0100644, CONFLICTING_THEIRS_OID);
}

void test_checkout_conflict__update_only(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, AUTOMERGEABLE_ANCESTOR_OID, 1, "automergeable.txt" },
		{ 0100644, AUTOMERGEABLE_OURS_OID, 2, "automergeable.txt" },
		{ 0100644, AUTOMERGEABLE_THEIRS_OID, 3, "automergeable.txt" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "modify-delete" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "modify-delete" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "directory_file-one" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "directory_file-one" },
		{ 0100644, CONFLICTING_THEIRS_OID, 0, "directory_file-one/file" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "directory_file-two" },
		{ 0100644, CONFLICTING_OURS_OID, 0, "directory_file-two/file" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "directory_file-two" },
	};

	opts.checkout_strategy |= GIT_CHECKOUT_UPDATE_ONLY;

	create_index(checkout_index_entries, 3);
	git_index_write(g_index);

	cl_git_pass(p_mkdir("merge-resolve/directory_file-two", 0777));
	cl_git_rewritefile("merge-resolve/directory_file-two/file", CONFLICTING_OURS_FILE);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir_contents("automergeable.txt", AUTOMERGEABLE_MERGED_FILE);
	ensure_workdir("directory_file-two/file", 0100644, CONFLICTING_OURS_OID);

	cl_assert(!git_path_exists("merge-resolve/modify-delete"));
	cl_assert(!git_path_exists("merge-resolve/test-one.txt"));
	cl_assert(!git_path_exists("merge-resolve/test-one-side-one.txt"));
	cl_assert(!git_path_exists("merge-resolve/test-one-side-two.txt"));
	cl_assert(!git_path_exists("merge-resolve/test-one.txt~ours"));
	cl_assert(!git_path_exists("merge-resolve/test-one.txt~theirs"));
	cl_assert(!git_path_exists("merge-resolve/directory_file-one/file"));
	cl_assert(!git_path_exists("merge-resolve/directory_file-one~ours"));
	cl_assert(!git_path_exists("merge-resolve/directory_file-two~theirs"));
}

void test_checkout_conflict__path_filters(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	char *paths[] = { "conflicting-1.txt", "conflicting-3.txt" };
	git_strarray patharray = {0};

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "conflicting-1.txt" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "conflicting-1.txt" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "conflicting-1.txt" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "conflicting-2.txt" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "conflicting-2.txt" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "conflicting-2.txt" },

		{ 0100644, AUTOMERGEABLE_ANCESTOR_OID, 1, "conflicting-3.txt" },
		{ 0100644, AUTOMERGEABLE_OURS_OID, 2, "conflicting-3.txt" },
		{ 0100644, AUTOMERGEABLE_THEIRS_OID, 3, "conflicting-3.txt" },

		{ 0100644, AUTOMERGEABLE_ANCESTOR_OID, 1, "conflicting-4.txt" },
		{ 0100644, AUTOMERGEABLE_OURS_OID, 2, "conflicting-4.txt" },
		{ 0100644, AUTOMERGEABLE_THEIRS_OID, 3, "conflicting-4.txt" },
	};

	patharray.count = 2;
	patharray.strings = paths;

	opts.paths = patharray;

	create_index(checkout_index_entries, 12);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	ensure_workdir_contents("conflicting-1.txt", CONFLICTING_DIFF3_FILE);
	cl_assert(!git_path_exists("merge-resolve/conflicting-2.txt"));
	ensure_workdir_contents("conflicting-3.txt", AUTOMERGEABLE_MERGED_FILE);
	cl_assert(!git_path_exists("merge-resolve/conflicting-4.txt"));
}

static void collect_progress(
	const char *path,
	size_t completed_steps,
	size_t total_steps,
	void *payload)
{
	git_vector *paths = payload;

	GIT_UNUSED(completed_steps);
	GIT_UNUSED(total_steps);

	if (path == NULL)
		return;

	git_vector_insert(paths, strdup(path));
}

void test_checkout_conflict__report_progress(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_vector paths = GIT_VECTOR_INIT;
	char *path;
	size_t i;

	struct checkout_index_entry checkout_index_entries[] = {
		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "conflicting-1.txt" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "conflicting-1.txt" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "conflicting-1.txt" },

		{ 0100644, CONFLICTING_ANCESTOR_OID, 1, "conflicting-2.txt" },
		{ 0100644, CONFLICTING_OURS_OID, 2, "conflicting-2.txt" },
		{ 0100644, CONFLICTING_THEIRS_OID, 3, "conflicting-2.txt" },

		{ 0100644, AUTOMERGEABLE_ANCESTOR_OID, 1, "conflicting-3.txt" },
		{ 0100644, AUTOMERGEABLE_OURS_OID, 2, "conflicting-3.txt" },
		{ 0100644, AUTOMERGEABLE_THEIRS_OID, 3, "conflicting-3.txt" },

		{ 0100644, AUTOMERGEABLE_ANCESTOR_OID, 1, "conflicting-4.txt" },
		{ 0100644, AUTOMERGEABLE_OURS_OID, 2, "conflicting-4.txt" },
		{ 0100644, AUTOMERGEABLE_THEIRS_OID, 3, "conflicting-4.txt" },
	};

	opts.progress_cb = collect_progress;
	opts.progress_payload = &paths;


	create_index(checkout_index_entries, 12);
	git_index_write(g_index);

	cl_git_pass(git_checkout_index(g_repo, g_index, &opts));

	cl_assert_equal_i(4, git_vector_length(&paths));
	cl_assert_equal_s("conflicting-1.txt", git_vector_get(&paths, 0));
	cl_assert_equal_s("conflicting-2.txt", git_vector_get(&paths, 1));
	cl_assert_equal_s("conflicting-3.txt", git_vector_get(&paths, 2));
	cl_assert_equal_s("conflicting-4.txt", git_vector_get(&paths, 3));

	git_vector_foreach(&paths, i, path)
		git__free(path);

	git_vector_free(&paths);
}
