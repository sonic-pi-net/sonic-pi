#include "clar_libgit2.h"
#include "posix.h"

void test_index_rename__single_file(void)
{
	git_repository *repo;
	git_index *index;
	size_t position;
	git_oid expected;
	const git_index_entry *entry;

	p_mkdir("rename", 0700);

	cl_git_pass(git_repository_init(&repo, "./rename", 0));
	cl_git_pass(git_repository_index(&index, repo));

	cl_assert(git_index_entrycount(index) == 0);

	cl_git_mkfile("./rename/lame.name.txt", "new_file\n");

	/* This should add a new blob to the object database in 'd4/fa8600b4f37d7516bef4816ae2c64dbf029e3a' */
	cl_git_pass(git_index_add_bypath(index, "lame.name.txt"));
	cl_assert(git_index_entrycount(index) == 1);

	cl_git_pass(git_oid_fromstr(&expected, "d4fa8600b4f37d7516bef4816ae2c64dbf029e3a"));

	cl_assert(!git_index_find(&position, index, "lame.name.txt"));

	entry = git_index_get_byindex(index, position);
	cl_assert_equal_oid(&expected, &entry->id);

	/* This removes the entry from the index, but not from the object database */
	cl_git_pass(git_index_remove(index, "lame.name.txt", 0));
	cl_assert(git_index_entrycount(index) == 0);

	p_rename("./rename/lame.name.txt", "./rename/fancy.name.txt");

	cl_git_pass(git_index_add_bypath(index, "fancy.name.txt"));
	cl_assert(git_index_entrycount(index) == 1);

	cl_assert(!git_index_find(&position, index, "fancy.name.txt"));

	entry = git_index_get_byindex(index, position);
	cl_assert_equal_oid(&expected, &entry->id);

	git_index_free(index);
	git_repository_free(repo);

	cl_fixture_cleanup("rename");
}

void test_index_rename__casechanging(void)
{
	git_repository *repo;
	git_index *index;
	const git_index_entry *entry;
	git_index_entry new = {{0}};

	p_mkdir("rename", 0700);

	cl_git_pass(git_repository_init(&repo, "./rename", 0));
	cl_git_pass(git_repository_index(&index, repo));

	cl_git_mkfile("./rename/lame.name.txt", "new_file\n");

	cl_git_pass(git_index_add_bypath(index, "lame.name.txt"));
	cl_assert_equal_i(1, git_index_entrycount(index));
	cl_assert((entry = git_index_get_bypath(index, "lame.name.txt", 0)));

	memcpy(&new, entry, sizeof(git_index_entry));
	new.path = "LAME.name.TXT";

	cl_git_pass(git_index_add(index, &new));
	cl_assert((entry = git_index_get_bypath(index, "LAME.name.TXT", 0)));

	if (cl_repo_get_bool(repo, "core.ignorecase"))
		cl_assert_equal_i(1, git_index_entrycount(index));
	else
		cl_assert_equal_i(2, git_index_entrycount(index));

	git_index_free(index);
	git_repository_free(repo);

	cl_fixture_cleanup("rename");
}

