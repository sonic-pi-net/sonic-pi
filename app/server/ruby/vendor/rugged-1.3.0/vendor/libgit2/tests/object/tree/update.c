#include "clar_libgit2.h"
#include "tree.h"

static git_repository *g_repo;

void test_object_tree_update__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo2");
}

void test_object_tree_update__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_object_tree_update__remove_blob(void)
{
	git_oid tree_index_id, tree_updater_id, base_id;
	git_tree *base_tree;
	git_index *idx;
	const char *path = "README";

	git_tree_update updates[] = {
		{ GIT_TREE_UPDATE_REMOVE, {{0}}, GIT_FILEMODE_BLOB /* ignored */, path},
	};

	cl_git_pass(git_oid_fromstr(&base_id, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b"));
	cl_git_pass(git_tree_lookup(&base_tree, g_repo, &base_id));

	/* Create it with an index */
	cl_git_pass(git_index_new(&idx));
	cl_git_pass(git_index_read_tree(idx, base_tree));
	cl_git_pass(git_index_remove(idx, path, 0));
	cl_git_pass(git_index_write_tree_to(&tree_index_id, idx, g_repo));
	git_index_free(idx);

	/* Perform the same operation via the tree updater */
	cl_git_pass(git_tree_create_updated(&tree_updater_id, g_repo, base_tree, 1, updates));

	cl_assert_equal_oid(&tree_index_id, &tree_updater_id);

	git_tree_free(base_tree);
}

void test_object_tree_update__remove_blob_deeper(void)
{
	git_oid tree_index_id, tree_updater_id, base_id;
	git_tree *base_tree;
	git_index *idx;
	const char *path = "subdir/README";

	git_tree_update updates[] = {
		{ GIT_TREE_UPDATE_REMOVE, {{0}}, GIT_FILEMODE_BLOB /* ignored */, path},
	};

	cl_git_pass(git_oid_fromstr(&base_id, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b"));
	cl_git_pass(git_tree_lookup(&base_tree, g_repo, &base_id));

	/* Create it with an index */
	cl_git_pass(git_index_new(&idx));
	cl_git_pass(git_index_read_tree(idx, base_tree));
	cl_git_pass(git_index_remove(idx, path, 0));
	cl_git_pass(git_index_write_tree_to(&tree_index_id, idx, g_repo));
	git_index_free(idx);

	/* Perform the same operation via the tree updater */
	cl_git_pass(git_tree_create_updated(&tree_updater_id, g_repo, base_tree, 1, updates));

	cl_assert_equal_oid(&tree_index_id, &tree_updater_id);

	git_tree_free(base_tree);
}

void test_object_tree_update__remove_all_entries(void)
{
	git_oid tree_index_id, tree_updater_id, base_id;
	git_tree *base_tree;
	git_index *idx;
	const char *path1 = "subdir/subdir2/README";
	const char *path2 = "subdir/subdir2/new.txt";

	git_tree_update updates[] = {
		{ GIT_TREE_UPDATE_REMOVE, {{0}}, GIT_FILEMODE_BLOB /* ignored */, path1},
		{ GIT_TREE_UPDATE_REMOVE, {{0}}, GIT_FILEMODE_BLOB /* ignored */, path2},
	};

	cl_git_pass(git_oid_fromstr(&base_id, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b"));
	cl_git_pass(git_tree_lookup(&base_tree, g_repo, &base_id));

	/* Create it with an index */
	cl_git_pass(git_index_new(&idx));
	cl_git_pass(git_index_read_tree(idx, base_tree));
	cl_git_pass(git_index_remove(idx, path1, 0));
	cl_git_pass(git_index_remove(idx, path2, 0));
	cl_git_pass(git_index_write_tree_to(&tree_index_id, idx, g_repo));
	git_index_free(idx);

	/* Perform the same operation via the tree updater */
	cl_git_pass(git_tree_create_updated(&tree_updater_id, g_repo, base_tree, 2, updates));

	cl_assert_equal_oid(&tree_index_id, &tree_updater_id);

	git_tree_free(base_tree);
}

void test_object_tree_update__replace_blob(void)
{
	git_oid tree_index_id, tree_updater_id, base_id;
	git_tree *base_tree;
	git_index *idx;
	const char *path = "README";
	git_index_entry entry = { {0} };

	git_tree_update updates[] = {
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, path},
	};

	cl_git_pass(git_oid_fromstr(&base_id, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b"));
	cl_git_pass(git_tree_lookup(&base_tree, g_repo, &base_id));

	/* Create it with an index */
	cl_git_pass(git_index_new(&idx));
	cl_git_pass(git_index_read_tree(idx, base_tree));

	entry.path = path;
	cl_git_pass(git_oid_fromstr(&entry.id, "fa49b077972391ad58037050f2a75f74e3671e92"));
	entry.mode = GIT_FILEMODE_BLOB;
	cl_git_pass(git_index_add(idx, &entry));

	cl_git_pass(git_index_write_tree_to(&tree_index_id, idx, g_repo));
	git_index_free(idx);

	/* Perform the same operation via the tree updater */
	cl_git_pass(git_oid_fromstr(&updates[0].id, "fa49b077972391ad58037050f2a75f74e3671e92"));
	cl_git_pass(git_tree_create_updated(&tree_updater_id, g_repo, base_tree, 1, updates));

	cl_assert_equal_oid(&tree_index_id, &tree_updater_id);

	git_tree_free(base_tree);
}

void test_object_tree_update__add_blobs(void)
{
	git_oid tree_index_id, tree_updater_id, base_id;
	git_tree *base_tree;
	git_index *idx;
	git_index_entry entry = { {0} };
	int i;
	const char *paths[] = {
		"some/deep/path",
		"some/other/path",
		"a/path/elsewhere",
	};

	git_tree_update updates[] = {
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, paths[0]},
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, paths[1]},
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, paths[2]},
	};

	cl_git_pass(git_oid_fromstr(&base_id, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b"));

	entry.mode = GIT_FILEMODE_BLOB;
	cl_git_pass(git_oid_fromstr(&entry.id, "fa49b077972391ad58037050f2a75f74e3671e92"));

	for (i = 0; i < 3; i++) {
		cl_git_pass(git_oid_fromstr(&updates[i].id, "fa49b077972391ad58037050f2a75f74e3671e92"));
	}

	for (i = 0; i < 2; i++) {
		int j;

		/* Create it with an index */
		cl_git_pass(git_index_new(&idx));

		base_tree = NULL;
		if (i == 1) {
			cl_git_pass(git_tree_lookup(&base_tree, g_repo, &base_id));
			cl_git_pass(git_index_read_tree(idx, base_tree));
		}

		for (j = 0; j < 3; j++) {
			entry.path = paths[j];
			cl_git_pass(git_index_add(idx, &entry));
		}

		cl_git_pass(git_index_write_tree_to(&tree_index_id, idx, g_repo));
		git_index_free(idx);

		/* Perform the same operations via the tree updater */
		cl_git_pass(git_tree_create_updated(&tree_updater_id, g_repo, base_tree, 3, updates));

		cl_assert_equal_oid(&tree_index_id, &tree_updater_id);
	}

	git_tree_free(base_tree);
}

void test_object_tree_update__add_blobs_unsorted(void)
{
	git_oid tree_index_id, tree_updater_id, base_id;
	git_tree *base_tree;
	git_index *idx;
	git_index_entry entry = { {0} };
	int i;
	const char *paths[] = {
		"some/deep/path",
		"a/path/elsewhere",
		"some/other/path",
	};

	git_tree_update updates[] = {
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, paths[0]},
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, paths[1]},
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, paths[2]},
	};

	cl_git_pass(git_oid_fromstr(&base_id, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b"));

	entry.mode = GIT_FILEMODE_BLOB;
	cl_git_pass(git_oid_fromstr(&entry.id, "fa49b077972391ad58037050f2a75f74e3671e92"));

	for (i = 0; i < 3; i++) {
		cl_git_pass(git_oid_fromstr(&updates[i].id, "fa49b077972391ad58037050f2a75f74e3671e92"));
	}

	for (i = 0; i < 2; i++) {
		int j;

		/* Create it with an index */
		cl_git_pass(git_index_new(&idx));

		base_tree = NULL;
		if (i == 1) {
			cl_git_pass(git_tree_lookup(&base_tree, g_repo, &base_id));
			cl_git_pass(git_index_read_tree(idx, base_tree));
		}

		for (j = 0; j < 3; j++) {
			entry.path = paths[j];
			cl_git_pass(git_index_add(idx, &entry));
		}

		cl_git_pass(git_index_write_tree_to(&tree_index_id, idx, g_repo));
		git_index_free(idx);

		/* Perform the same operations via the tree updater */
		cl_git_pass(git_tree_create_updated(&tree_updater_id, g_repo, base_tree, 3, updates));

		cl_assert_equal_oid(&tree_index_id, &tree_updater_id);
	}

	git_tree_free(base_tree);
}

void test_object_tree_update__add_conflict(void)
{
	int i;
	git_oid tree_updater_id;
	git_tree_update updates[] = {
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, "a/dir/blob"},
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, "a/dir"},
	};

	for (i = 0; i < 2; i++) {
		cl_git_pass(git_oid_fromstr(&updates[i].id, "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"));
	}

	cl_git_fail(git_tree_create_updated(&tree_updater_id, g_repo, NULL, 2, updates));
}

void test_object_tree_update__add_conflict2(void)
{
	int i;
	git_oid tree_updater_id;
	git_tree_update updates[] = {
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_BLOB, "a/dir/blob"},
		{ GIT_TREE_UPDATE_UPSERT, {{0}}, GIT_FILEMODE_TREE, "a/dir/blob"},
	};

	for (i = 0; i < 2; i++) {
		cl_git_pass(git_oid_fromstr(&updates[i].id, "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"));
	}

	cl_git_fail(git_tree_create_updated(&tree_updater_id, g_repo, NULL, 2, updates));
}

void test_object_tree_update__remove_invalid_submodule(void)
{
	git_tree *baseline;
	git_oid updated_tree_id, baseline_id;
	git_tree_update updates[] = {
		{GIT_TREE_UPDATE_REMOVE, {{0}}, GIT_FILEMODE_BLOB, "submodule"},
	};

	/* This tree contains a submodule with an all-zero commit for a submodule named 'submodule' */
	cl_git_pass(git_oid_fromstr(&baseline_id, "396c7f1adb7925f51ba13a75f48252f44c5a14a2"));
	cl_git_pass(git_tree_lookup(&baseline, g_repo, &baseline_id));
	cl_git_pass(git_tree_create_updated(&updated_tree_id, g_repo, baseline, 1, updates));

	git_tree_free(baseline);
}
