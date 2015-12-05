#include "clar_libgit2.h"
#include "posix.h"
#include "index.h"

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
