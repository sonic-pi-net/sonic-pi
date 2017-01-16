#include "clar_libgit2.h"
#include "posix.h"
#include "index.h"
#include "conflicts.h"

static git_repository *_repo;
static git_index *_index;

void test_index_read_index__initialize(void)
{
	git_object *head;
	git_reference *head_ref;

	_repo = cl_git_sandbox_init("testrepo");
	cl_git_pass(git_revparse_ext(&head, &head_ref, _repo, "HEAD"));
	cl_git_pass(git_reset(_repo, head, GIT_RESET_HARD, NULL));
	cl_git_pass(git_repository_index(&_index, _repo));

	git_reference_free(head_ref);
	git_object_free(head);
}

void test_index_read_index__cleanup(void)
{
	git_index_free(_index);
	cl_git_sandbox_cleanup();
}

void test_index_read_index__maintains_stat_cache(void)
{
	git_index *new_index;
	git_oid index_id;
	git_index_entry new_entry;
	const git_index_entry *e;
	git_tree *tree;
	size_t i;

	cl_assert_equal_i(4, git_index_entrycount(_index));

	/* write-tree */
	cl_git_pass(git_index_write_tree(&index_id, _index));

	/* read-tree, then read index */
	git_tree_lookup(&tree, _repo, &index_id);
	cl_git_pass(git_index_new(&new_index));
	cl_git_pass(git_index_read_tree(new_index, tree));
	git_tree_free(tree);

	/* add a new entry that will not have stat data */
	memset(&new_entry, 0, sizeof(git_index_entry));
	new_entry.path = "Hello";
	git_oid_fromstr(&new_entry.id, "0123456789012345678901234567890123456789");
	new_entry.file_size = 1234;
	new_entry.mode = 0100644;
	cl_git_pass(git_index_add(new_index, &new_entry));
	cl_assert_equal_i(5, git_index_entrycount(new_index));

	cl_git_pass(git_index_read_index(_index, new_index));
	git_index_free(new_index);

	cl_assert_equal_i(5, git_index_entrycount(_index));

	for (i = 0; i < git_index_entrycount(_index); i++) {
		e = git_index_get_byindex(_index, i);

		if (strcmp(e->path, "Hello") == 0) {
			cl_assert_equal_i(0, e->ctime.seconds);
			cl_assert_equal_i(0, e->mtime.seconds);
		} else {
			cl_assert(0 != e->ctime.seconds);
			cl_assert(0 != e->mtime.seconds);
		}
	}
}

static bool roundtrip_with_read_index(const char *tree_idstr)
{
	git_oid tree_id, new_tree_id;
	git_tree *tree;
	git_index *tree_index;

	cl_git_pass(git_oid_fromstr(&tree_id, tree_idstr));
	cl_git_pass(git_tree_lookup(&tree, _repo, &tree_id));
	cl_git_pass(git_index_new(&tree_index));
	cl_git_pass(git_index_read_tree(tree_index, tree));
	cl_git_pass(git_index_read_index(_index, tree_index));
	cl_git_pass(git_index_write_tree(&new_tree_id, _index));

	git_tree_free(tree);
	git_index_free(tree_index);

	return git_oid_equal(&tree_id, &new_tree_id);
}

void test_index_read_index__produces_treesame_indexes(void)
{
	roundtrip_with_read_index("53fc32d17276939fc79ed05badaef2db09990016");
	roundtrip_with_read_index("944c0f6e4dfa41595e6eb3ceecdb14f50fe18162");
	roundtrip_with_read_index("1810dff58d8a660512d4832e740f692884338ccd");
	roundtrip_with_read_index("d52a8fe84ceedf260afe4f0287bbfca04a117e83");
	roundtrip_with_read_index("c36d8ea75da8cb510fcb0c408c1d7e53f9a99dbe");
	roundtrip_with_read_index("7b2417a23b63e1fdde88c80e14b33247c6e5785a");
	roundtrip_with_read_index("f82a8eb4cb20e88d1030fd10d89286215a715396");
	roundtrip_with_read_index("fd093bff70906175335656e6ce6ae05783708765");
	roundtrip_with_read_index("ae90f12eea699729ed24555e40b9fd669da12a12");
}

void test_index_read_index__read_and_writes(void)
{
	git_oid tree_id, new_tree_id;
	git_tree *tree;
	git_index *tree_index, *new_index;

	cl_git_pass(git_oid_fromstr(&tree_id, "ae90f12eea699729ed24555e40b9fd669da12a12"));
	cl_git_pass(git_tree_lookup(&tree, _repo, &tree_id));
	cl_git_pass(git_index_new(&tree_index));
	cl_git_pass(git_index_read_tree(tree_index, tree));
	cl_git_pass(git_index_read_index(_index, tree_index));
	cl_git_pass(git_index_write(_index));

	cl_git_pass(git_index_open(&new_index, git_index_path(_index)));
	cl_git_pass(git_index_write_tree_to(&new_tree_id, new_index, _repo));

	cl_assert_equal_oid(&tree_id, &new_tree_id);

	git_tree_free(tree);
	git_index_free(tree_index);
	git_index_free(new_index);
}

static void add_conflicts(git_index *index, const char *filename)
{
	git_index_entry ancestor_entry, our_entry, their_entry;
	static int conflict_idx = 0;
	char *ancestor_ids[] =
		{ CONFLICTS_ONE_ANCESTOR_OID, CONFLICTS_TWO_ANCESTOR_OID };
	char *our_ids[] =
		{ CONFLICTS_ONE_OUR_OID, CONFLICTS_TWO_OUR_OID };
	char *their_ids[] =
		{ CONFLICTS_ONE_THEIR_OID, CONFLICTS_TWO_THEIR_OID };

	conflict_idx = (conflict_idx + 1) % 2;

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	ancestor_entry.path = filename;
	ancestor_entry.mode = 0100644;
	GIT_IDXENTRY_STAGE_SET(&ancestor_entry, 1);
	git_oid_fromstr(&ancestor_entry.id, ancestor_ids[conflict_idx]);

	our_entry.path = filename;
	our_entry.mode = 0100644;
	GIT_IDXENTRY_STAGE_SET(&our_entry, 2);
	git_oid_fromstr(&our_entry.id, our_ids[conflict_idx]);

	their_entry.path = filename;
	their_entry.mode = 0100644;
	GIT_IDXENTRY_STAGE_SET(&ancestor_entry, 2);
	git_oid_fromstr(&their_entry.id, their_ids[conflict_idx]);

	cl_git_pass(git_index_conflict_add(index, &ancestor_entry,
		&our_entry, &their_entry));
}

void test_index_read_index__handles_conflicts(void)
{
	git_oid tree_id;
	git_tree *tree;
	git_index *index, *new_index;
	git_index_conflict_iterator *conflict_iterator;
	const git_index_entry *ancestor, *ours, *theirs;

	cl_git_pass(git_oid_fromstr(&tree_id, "ae90f12eea699729ed24555e40b9fd669da12a12"));
	cl_git_pass(git_tree_lookup(&tree, _repo, &tree_id));
	cl_git_pass(git_index_new(&index));
	cl_git_pass(git_index_new(&new_index));
	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_read_tree(new_index, tree));

	/* put some conflicts in only the old side, these should be removed */
	add_conflicts(index, "orig_side-1.txt");
	add_conflicts(index, "orig_side-2.txt");

	/* put some conflicts in both indexes, these should be unchanged */
	add_conflicts(index, "both_sides-1.txt");
	add_conflicts(new_index,  "both_sides-1.txt");
	add_conflicts(index, "both_sides-2.txt");
	add_conflicts(new_index,  "both_sides-2.txt");

	/* put some conflicts in the new index, these should be added */
	add_conflicts(new_index, "new_side-1.txt");
	add_conflicts(new_index, "new_side-2.txt");

	cl_git_pass(git_index_read_index(index, new_index));
	cl_git_pass(git_index_conflict_iterator_new(&conflict_iterator, index));

	cl_git_pass(git_index_conflict_next(
		&ancestor, &ours, &theirs, conflict_iterator));
	cl_assert_equal_s("both_sides-1.txt", ancestor->path);
	cl_assert_equal_s("both_sides-1.txt", ours->path);
	cl_assert_equal_s("both_sides-1.txt", theirs->path);

	cl_git_pass(git_index_conflict_next(
		&ancestor, &ours, &theirs, conflict_iterator));
	cl_assert_equal_s("both_sides-2.txt", ancestor->path);
	cl_assert_equal_s("both_sides-2.txt", ours->path);
	cl_assert_equal_s("both_sides-2.txt", theirs->path);

	cl_git_pass(git_index_conflict_next(
		&ancestor, &ours, &theirs, conflict_iterator));
	cl_assert_equal_s("new_side-1.txt", ancestor->path);
	cl_assert_equal_s("new_side-1.txt", ours->path);
	cl_assert_equal_s("new_side-1.txt", theirs->path);

	cl_git_pass(git_index_conflict_next(
		&ancestor, &ours, &theirs, conflict_iterator));
	cl_assert_equal_s("new_side-2.txt", ancestor->path);
	cl_assert_equal_s("new_side-2.txt", ours->path);
	cl_assert_equal_s("new_side-2.txt", theirs->path);


	cl_git_fail_with(GIT_ITEROVER, git_index_conflict_next(
		&ancestor, &ours, &theirs, conflict_iterator));

	git_index_conflict_iterator_free(conflict_iterator);

	git_tree_free(tree);
	git_index_free(new_index);
	git_index_free(index);
}
