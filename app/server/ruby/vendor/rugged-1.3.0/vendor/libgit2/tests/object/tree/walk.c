#include "clar_libgit2.h"
#include "tree.h"

static const char *tree_oid = "1810dff58d8a660512d4832e740f692884338ccd";
static git_repository *g_repo;

void test_object_tree_walk__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_object_tree_walk__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

static int treewalk_count_cb(
	const char *root, const git_tree_entry *entry, void *payload)
{
	int *count = payload;

	GIT_UNUSED(root);
	GIT_UNUSED(entry);

	(*count) += 1;

	return 0;
}

void test_object_tree_walk__0(void)
{
	git_oid id;
	git_tree *tree;
	int ct;

	git_oid_fromstr(&id, tree_oid);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));

	ct = 0;
	cl_git_pass(git_tree_walk(tree, GIT_TREEWALK_PRE, treewalk_count_cb, &ct));
	cl_assert_equal_i(3, ct);

	ct = 0;
	cl_git_pass(git_tree_walk(tree, GIT_TREEWALK_POST, treewalk_count_cb, &ct));
	cl_assert_equal_i(3, ct);

	git_tree_free(tree);
}


static int treewalk_stop_cb(
	const char *root, const git_tree_entry *entry, void *payload)
{
	int *count = payload;

	GIT_UNUSED(root);
	GIT_UNUSED(entry);

	(*count) += 1;

	return (*count == 2) ? -123 : 0;
}

static int treewalk_stop_immediately_cb(
	const char *root, const git_tree_entry *entry, void *payload)
{
	GIT_UNUSED(root);
	GIT_UNUSED(entry);
	GIT_UNUSED(payload);
	return -100;
}

void test_object_tree_walk__1(void)
{
	git_oid id;
	git_tree *tree;
	int ct;

	git_oid_fromstr(&id, tree_oid);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));

	ct = 0;
	cl_assert_equal_i(
		-123, git_tree_walk(tree, GIT_TREEWALK_PRE, treewalk_stop_cb, &ct));
	cl_assert_equal_i(2, ct);

	ct = 0;
	cl_assert_equal_i(
		-123, git_tree_walk(tree, GIT_TREEWALK_POST, treewalk_stop_cb, &ct));
	cl_assert_equal_i(2, ct);

	cl_assert_equal_i(
		-100, git_tree_walk(
			tree, GIT_TREEWALK_PRE, treewalk_stop_immediately_cb, NULL));

	cl_assert_equal_i(
		-100, git_tree_walk(
			tree, GIT_TREEWALK_POST, treewalk_stop_immediately_cb, NULL));

	git_tree_free(tree);
}


struct treewalk_skip_data {
	int files;
	int dirs;
	const char *skip;
	const char *stop;
};

static int treewalk_skip_de_cb(
	const char *root, const git_tree_entry *entry, void *payload)
{
	struct treewalk_skip_data *data = payload;
	const char *name = git_tree_entry_name(entry);

	GIT_UNUSED(root);

	if (git_tree_entry_type(entry) == GIT_OBJECT_TREE)
		data->dirs++;
	else
		data->files++;

	if (data->skip && !strcmp(name, data->skip))
		return 1;
	else if (data->stop && !strcmp(name, data->stop))
		return -1;
	else
		return 0;
}

void test_object_tree_walk__2(void)
{
	git_oid id;
	git_tree *tree;
	struct treewalk_skip_data data;

	/* look up a deep tree */
	git_oid_fromstr(&id, "ae90f12eea699729ed24555e40b9fd669da12a12");
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));

	memset(&data, 0, sizeof(data));
	data.skip = "de";

	cl_assert_equal_i(0, git_tree_walk(
		tree, GIT_TREEWALK_PRE, treewalk_skip_de_cb, &data));
	cl_assert_equal_i(5, data.files);
	cl_assert_equal_i(3, data.dirs);

	memset(&data, 0, sizeof(data));
	data.stop = "3.txt";

	cl_assert_equal_i(-1, git_tree_walk(
		tree, GIT_TREEWALK_PRE, treewalk_skip_de_cb, &data));
	cl_assert_equal_i(3, data.files);
	cl_assert_equal_i(2, data.dirs);

	memset(&data, 0, sizeof(data));
	data.skip = "new.txt";

	cl_assert_equal_i(0, git_tree_walk(
		tree, GIT_TREEWALK_PRE, treewalk_skip_de_cb, &data));
	cl_assert_equal_i(7, data.files);
	cl_assert_equal_i(4, data.dirs);

	memset(&data, 0, sizeof(data));
	data.stop = "new.txt";

	cl_assert_equal_i(-1, git_tree_walk(
		tree, GIT_TREEWALK_PRE, treewalk_skip_de_cb, &data));
	cl_assert_equal_i(7, data.files);
	cl_assert_equal_i(4, data.dirs);

	git_tree_free(tree);
}
