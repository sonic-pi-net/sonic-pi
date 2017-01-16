#include "clar_libgit2.h"
#include "tree.h"

static git_repository *_repo;

void test_object_tree_duplicateentries__initialize(void) {
   _repo = cl_git_sandbox_init("testrepo");
}

void test_object_tree_duplicateentries__cleanup(void) {
   cl_git_sandbox_cleanup();
}

/*
 * $ git show --format=raw refs/heads/dir
 * commit 144344043ba4d4a405da03de3844aa829ae8be0e
 * tree d52a8fe84ceedf260afe4f0287bbfca04a117e83
 * parent cf80f8de9f1185bf3a05f993f6121880dd0cfbc9
 * author Ben Straub <bstraub@github.com> 1343755506 -0700
 * committer Ben Straub <bstraub@github.com> 1343755506 -0700
 * 
 *     Change a file mode
 * 
 * diff --git a/a/b.txt b/a/b.txt
 * old mode 100644
 * new mode 100755
 *
 * $ git ls-tree d52a8fe84ceedf260afe4f0287bbfca04a117e83
 * 100644 blob a8233120f6ad708f843d861ce2b7228ec4e3dec6    README
 * 040000 tree 4e0883eeeeebc1fb1735161cea82f7cb5fab7e63    a
 * 100644 blob 45b983be36b73c0788dc9cbcb76cbb80fc7bb057    branch_file.txt
 * 100644 blob a71586c1dfe8a71c6cbf6c129f404c5642ff31bd    new.txt
 */

static void tree_checker(
	git_oid *tid,
	const char *expected_sha,
	git_filemode_t expected_filemode)
{
	git_tree *tree;
	const git_tree_entry *entry;
	git_oid oid;

	cl_git_pass(git_tree_lookup(&tree, _repo, tid));
	cl_assert_equal_i(1, (int)git_tree_entrycount(tree));
	entry = git_tree_entry_byindex(tree, 0);

	cl_git_pass(git_oid_fromstr(&oid, expected_sha));

	cl_assert_equal_i(0, git_oid_cmp(&oid, git_tree_entry_id(entry)));
	cl_assert_equal_i(expected_filemode, git_tree_entry_filemode(entry));

	git_tree_free(tree);
}

static void tree_creator(git_oid *out, void (*fn)(git_treebuilder *))
{
	git_treebuilder *builder;

	cl_git_pass(git_treebuilder_new(&builder, _repo, NULL));

	fn(builder);

	cl_git_pass(git_treebuilder_write(out, builder));
	git_treebuilder_free(builder);
}

static void two_blobs(git_treebuilder *bld)
{
	git_oid oid;
	const git_tree_entry *entry;
	
	cl_git_pass(git_oid_fromstr(&oid,
		"a8233120f6ad708f843d861ce2b7228ec4e3dec6"));	/* blob oid (README) */

	cl_git_pass(git_treebuilder_insert(
		&entry,	bld, "duplicate", &oid,
		GIT_FILEMODE_BLOB));

	cl_git_pass(git_oid_fromstr(&oid,
		"a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"));	/* blob oid (new.txt) */

	cl_git_pass(git_treebuilder_insert(
		&entry,	bld, "duplicate", &oid,
		GIT_FILEMODE_BLOB));
}

static void one_blob_and_one_tree(git_treebuilder *bld)
{
	git_oid oid;
	const git_tree_entry *entry;
	
	cl_git_pass(git_oid_fromstr(&oid,
		"a8233120f6ad708f843d861ce2b7228ec4e3dec6"));	/* blob oid (README) */

	cl_git_pass(git_treebuilder_insert(
		&entry,	bld, "duplicate", &oid,
		GIT_FILEMODE_BLOB));

	cl_git_pass(git_oid_fromstr(&oid,
		"4e0883eeeeebc1fb1735161cea82f7cb5fab7e63"));	/* tree oid (a) */

	cl_git_pass(git_treebuilder_insert(
		&entry,	bld, "duplicate", &oid,
		GIT_FILEMODE_TREE));
}

void test_object_tree_duplicateentries__cannot_create_a_duplicate_entry_through_the_treebuilder(void)
{
	git_oid tid;

	tree_creator(&tid, two_blobs);
	tree_checker(&tid, "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd", GIT_FILEMODE_BLOB);
	
	tree_creator(&tid, one_blob_and_one_tree);
	tree_checker(&tid, "4e0883eeeeebc1fb1735161cea82f7cb5fab7e63", GIT_FILEMODE_TREE);
}

static void add_fake_conflicts(git_index *index)
{
	git_index_entry ancestor_entry, our_entry, their_entry;

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	ancestor_entry.path = "duplicate";
	ancestor_entry.mode = GIT_FILEMODE_BLOB;
	GIT_IDXENTRY_STAGE_SET(&ancestor_entry, 1);
	git_oid_fromstr(&ancestor_entry.id, "a8233120f6ad708f843d861ce2b7228ec4e3dec6");

	our_entry.path = "duplicate";
	our_entry.mode = GIT_FILEMODE_BLOB;
	GIT_IDXENTRY_STAGE_SET(&our_entry, 2);
	git_oid_fromstr(&our_entry.id, "45b983be36b73c0788dc9cbcb76cbb80fc7bb057");

	their_entry.path = "duplicate";
	their_entry.mode = GIT_FILEMODE_BLOB;
	GIT_IDXENTRY_STAGE_SET(&their_entry, 3);
	git_oid_fromstr(&their_entry.id, "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd");

	cl_git_pass(git_index_conflict_add(index, &ancestor_entry, &our_entry, &their_entry));
}

void test_object_tree_duplicateentries__cannot_create_a_duplicate_entry_building_a_tree_from_a_index_with_conflicts(void)
{
	git_index *index;
	git_oid tid;

	cl_git_pass(git_repository_index(&index, _repo));

	add_fake_conflicts(index); 

	cl_assert_equal_i(GIT_EUNMERGED, git_index_write_tree(&tid, index));

	git_index_free(index);
}
