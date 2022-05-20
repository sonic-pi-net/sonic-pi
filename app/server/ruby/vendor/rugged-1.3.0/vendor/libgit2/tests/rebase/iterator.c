#include "clar_libgit2.h"
#include "git2/rebase.h"
#include "posix.h"

#include <fcntl.h>

static git_repository *repo;
static git_index *_index;
static git_signature *signature;

/* Fixture setup and teardown */
void test_rebase_iterator__initialize(void)
{
	repo = cl_git_sandbox_init("rebase");
	cl_git_pass(git_repository_index(&_index, repo));
	cl_git_pass(git_signature_new(&signature, "Rebaser",
		"rebaser@rebaser.rb", 1405694510, 0));
}

void test_rebase_iterator__cleanup(void)
{
	git_signature_free(signature);
	git_index_free(_index);
	cl_git_sandbox_cleanup();
}

static void test_operations(git_rebase *rebase, size_t expected_current)
{
	size_t i, expected_count = 5;
	git_oid expected_oid[5];
	git_rebase_operation *operation;

	git_oid_fromstr(&expected_oid[0], "da9c51a23d02d931a486f45ad18cda05cf5d2b94");
	git_oid_fromstr(&expected_oid[1], "8d1f13f93c4995760ac07d129246ac1ff64c0be9");
	git_oid_fromstr(&expected_oid[2], "3069cc907e6294623e5917ef6de663928c1febfb");
	git_oid_fromstr(&expected_oid[3], "588e5d2f04d49707fe4aab865e1deacaf7ef6787");
	git_oid_fromstr(&expected_oid[4], "b146bd7608eac53d9bf9e1a6963543588b555c64");

	cl_assert_equal_i(expected_count, git_rebase_operation_entrycount(rebase));
	cl_assert_equal_i(expected_current, git_rebase_operation_current(rebase));

	for (i = 0; i < expected_count; i++) {
		operation = git_rebase_operation_byindex(rebase, i);
		cl_assert_equal_i(GIT_REBASE_OPERATION_PICK, operation->type);
		cl_assert_equal_oid(&expected_oid[i], &operation->id);
		cl_assert_equal_p(NULL, operation->exec);
	}
}

void test_iterator(bool inmemory)
{
	git_rebase *rebase;
	git_rebase_options opts = GIT_REBASE_OPTIONS_INIT;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_oid commit_id, expected_id;
	int error;

	opts.inmemory = inmemory;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, &opts));
	test_operations(rebase, GIT_REBASE_NO_OPERATION);

	if (!inmemory) {
		git_rebase_free(rebase);
		cl_git_pass(git_rebase_open(&rebase, repo, NULL));
	}

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));
	test_operations(rebase, 0);

	git_oid_fromstr(&expected_id, "776e4c48922799f903f03f5f6e51da8b01e4cce0");
	cl_assert_equal_oid(&expected_id, &commit_id);

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));
	test_operations(rebase, 1);

	git_oid_fromstr(&expected_id, "ba1f9b4fd5cf8151f7818be2111cc0869f1eb95a");
	cl_assert_equal_oid(&expected_id, &commit_id);

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));
	test_operations(rebase, 2);

	git_oid_fromstr(&expected_id, "948b12fe18b84f756223a61bece4c307787cd5d4");
	cl_assert_equal_oid(&expected_id, &commit_id);

	if (!inmemory) {
		git_rebase_free(rebase);
		cl_git_pass(git_rebase_open(&rebase, repo, NULL));
	}

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));
	test_operations(rebase, 3);

	git_oid_fromstr(&expected_id, "d9d5d59d72c9968687f9462578d79878cd80e781");
	cl_assert_equal_oid(&expected_id, &commit_id);

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));
	test_operations(rebase, 4);

	git_oid_fromstr(&expected_id, "9cf383c0a125d89e742c5dec58ed277dd07588b3");
	cl_assert_equal_oid(&expected_id, &commit_id);

	cl_git_fail(error = git_rebase_next(&rebase_operation, rebase));
	cl_assert_equal_i(GIT_ITEROVER, error);
	test_operations(rebase, 4);

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_iterator__iterates(void)
{
	test_iterator(false);
}

void test_rebase_iterator__iterates_inmemory(void)
{
	test_iterator(true);
}
