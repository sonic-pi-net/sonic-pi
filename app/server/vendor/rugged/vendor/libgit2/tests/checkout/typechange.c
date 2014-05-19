#include "clar_libgit2.h"
#include "git2/checkout.h"
#include "path.h"
#include "posix.h"
#include "fileops.h"

static git_repository *g_repo = NULL;

static const char *g_typechange_oids[] = {
	"79b9f23e85f55ea36a472a902e875bc1121a94cb",
	"9bdb75b73836a99e3dbeea640a81de81031fdc29",
	"0e7ed140b514b8cae23254cb8656fe1674403aff",
	"9d0235c7a7edc0889a18f97a42ee6db9fe688447",
	"9b19edf33a03a0c59cdfc113bfa5c06179bf9b1a",
	"1b63caae4a5ca96f78e8dfefc376c6a39a142475",
	"6eae26c90e8ccc4d16208972119c40635489c6f0",
	NULL
};

static bool g_typechange_empty[] = {
	true, false, false, false, false, false, true, true
};

void test_checkout_typechange__initialize(void)
{
	g_repo = cl_git_sandbox_init("typechanges");

	cl_fixture_sandbox("submod2_target");
	p_rename("submod2_target/.gitted", "submod2_target/.git");
}

void test_checkout_typechange__cleanup(void)
{
	cl_git_sandbox_cleanup();
	cl_fixture_cleanup("submod2_target");
}

static void assert_file_exists(const char *path)
{
	cl_assert_(git_path_isfile(path), path);
}

static void assert_dir_exists(const char *path)
{
	cl_assert_(git_path_isdir(path), path);
}

static void assert_workdir_matches_tree(
	git_repository *repo, const git_oid *id, const char *root, bool recurse)
{
	git_object *obj;
	git_tree *tree;
	size_t i, max_i;
	git_buf path = GIT_BUF_INIT;

	if (!root)
		root = git_repository_workdir(repo);
	cl_assert(root);

	cl_git_pass(git_object_lookup(&obj, repo, id, GIT_OBJ_ANY));
	cl_git_pass(git_object_peel((git_object **)&tree, obj, GIT_OBJ_TREE));
	git_object_free(obj);

	max_i = git_tree_entrycount(tree);

	for (i = 0; i < max_i; ++i) {
		const git_tree_entry *te = git_tree_entry_byindex(tree, i);
		cl_assert(te);

		cl_git_pass(git_buf_joinpath(&path, root, git_tree_entry_name(te)));

		switch (git_tree_entry_type(te)) {
		case GIT_OBJ_COMMIT:
			assert_dir_exists(path.ptr);
			break;
		case GIT_OBJ_TREE:
			assert_dir_exists(path.ptr);
			if (recurse)
				assert_workdir_matches_tree(
					repo, git_tree_entry_id(te), path.ptr, true);
			break;
		case GIT_OBJ_BLOB:
			switch (git_tree_entry_filemode(te)) {
			case GIT_FILEMODE_BLOB:
			case GIT_FILEMODE_BLOB_EXECUTABLE:
				assert_file_exists(path.ptr);
				/* because of cross-platform, don't confirm exec bit yet */
				break;
			case GIT_FILEMODE_LINK:
				cl_assert_(git_path_exists(path.ptr), path.ptr);
				/* because of cross-platform, don't confirm link yet */
				break;
			default:
				cl_assert(false); /* really?! */
			}
			break;
		default:
			cl_assert(false); /* really?!! */
		}
	}

	git_tree_free(tree);
	git_buf_free(&path);
}

void test_checkout_typechange__checkout_typechanges_safe(void)
{
	int i;
	git_object *obj;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	for (i = 0; g_typechange_oids[i] != NULL; ++i) {
		cl_git_pass(git_revparse_single(&obj, g_repo, g_typechange_oids[i]));

		opts.checkout_strategy = GIT_CHECKOUT_FORCE;

		/* There are bugs in some submodule->tree changes that prevent
		 * SAFE from passing here, even though the following should work:
		 */
		/* !i ? GIT_CHECKOUT_FORCE : GIT_CHECKOUT_SAFE; */

		cl_git_pass(git_checkout_tree(g_repo, obj, &opts));

		cl_git_pass(
			git_repository_set_head_detached(g_repo, git_object_id(obj), NULL, NULL));

		assert_workdir_matches_tree(g_repo, git_object_id(obj), NULL, true);

		git_object_free(obj);

		if (!g_typechange_empty[i]) {
			cl_assert(git_path_isdir("typechanges"));
			cl_assert(git_path_exists("typechanges/a"));
			cl_assert(git_path_exists("typechanges/b"));
			cl_assert(git_path_exists("typechanges/c"));
			cl_assert(git_path_exists("typechanges/d"));
			cl_assert(git_path_exists("typechanges/e"));
		} else {
			cl_assert(git_path_isdir("typechanges"));
			cl_assert(!git_path_exists("typechanges/a"));
			cl_assert(!git_path_exists("typechanges/b"));
			cl_assert(!git_path_exists("typechanges/c"));
			cl_assert(!git_path_exists("typechanges/d"));
			cl_assert(!git_path_exists("typechanges/e"));
		}
	}
}

typedef struct {
	int conflicts;
	int dirty;
	int updates;
	int untracked;
	int ignored;
} notify_counts;

static int notify_counter(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *baseline,
	const git_diff_file *target,
	const git_diff_file *workdir,
	void *payload)
{
	notify_counts *cts = payload;

	GIT_UNUSED(path);
	GIT_UNUSED(baseline);
	GIT_UNUSED(target);
	GIT_UNUSED(workdir);

	switch (why) {
	case GIT_CHECKOUT_NOTIFY_CONFLICT:  cts->conflicts++; break;
	case GIT_CHECKOUT_NOTIFY_DIRTY:     cts->dirty++;     break;
	case GIT_CHECKOUT_NOTIFY_UPDATED:   cts->updates++;   break;
	case GIT_CHECKOUT_NOTIFY_UNTRACKED: cts->untracked++; break;
	case GIT_CHECKOUT_NOTIFY_IGNORED:   cts->ignored++;   break;
	default: break;
	}

	return 0;
}

static void force_create_file(const char *file)
{
	int error = git_futils_rmdir_r(file, NULL,
		GIT_RMDIR_REMOVE_FILES | GIT_RMDIR_REMOVE_BLOCKERS);
	cl_assert(!error || error == GIT_ENOTFOUND);
	cl_git_pass(git_futils_mkpath2file(file, 0777));
	cl_git_rewritefile(file, "yowza!!");
}

void test_checkout_typechange__checkout_with_conflicts(void)
{
	int i;
	git_object *obj;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	notify_counts cts = {0};

	opts.notify_flags =
		GIT_CHECKOUT_NOTIFY_CONFLICT | GIT_CHECKOUT_NOTIFY_UNTRACKED;
	opts.notify_cb = notify_counter;
	opts.notify_payload = &cts;

	for (i = 0; g_typechange_oids[i] != NULL; ++i) {
		cl_git_pass(git_revparse_single(&obj, g_repo, g_typechange_oids[i]));

		force_create_file("typechanges/a/blocker");
		force_create_file("typechanges/b");
		force_create_file("typechanges/c/sub/sub/file");
		git_futils_rmdir_r("typechanges/d", NULL, GIT_RMDIR_REMOVE_FILES);
		p_mkdir("typechanges/d", 0777); /* intentionally empty dir */
		force_create_file("typechanges/untracked");

		opts.checkout_strategy = GIT_CHECKOUT_SAFE_CREATE;
		memset(&cts, 0, sizeof(cts));

		cl_git_fail(git_checkout_tree(g_repo, obj, &opts));
		cl_assert(cts.conflicts > 0);
		cl_assert(cts.untracked > 0);

		opts.checkout_strategy =
			GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;
		memset(&cts, 0, sizeof(cts));

		cl_assert(git_path_exists("typechanges/untracked"));

		cl_git_pass(git_checkout_tree(g_repo, obj, &opts));
		cl_assert_equal_i(0, cts.conflicts);

		cl_assert(!git_path_exists("typechanges/untracked"));

		cl_git_pass(
			git_repository_set_head_detached(g_repo, git_object_id(obj), NULL, NULL));

		assert_workdir_matches_tree(g_repo, git_object_id(obj), NULL, true);

		git_object_free(obj);
	}
}
