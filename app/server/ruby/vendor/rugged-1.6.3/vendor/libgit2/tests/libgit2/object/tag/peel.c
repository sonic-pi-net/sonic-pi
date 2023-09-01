#include "clar_libgit2.h"
#include "tag.h"

static git_repository *repo;
static git_tag *tag;
static git_object *target;

void test_object_tag_peel__initialize(void)
{
	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&repo, "testrepo.git"));
}

void test_object_tag_peel__cleanup(void)
{
	git_tag_free(tag);
	tag = NULL;

	git_object_free(target);
	target = NULL;

	git_repository_free(repo);
	repo = NULL;

	cl_fixture_cleanup("testrepo.git");
}

static void retrieve_tag_from_oid(git_tag **tag_out, git_repository *repo, const char *sha)
{
	git_oid oid;

	cl_git_pass(git_oid__fromstr(&oid, sha, GIT_OID_SHA1));
	cl_git_pass(git_tag_lookup(tag_out, repo, &oid));
}

void test_object_tag_peel__can_peel_to_a_commit(void)
{
	retrieve_tag_from_oid(&tag, repo, "7b4384978d2493e851f9cca7858815fac9b10980");

	cl_git_pass(git_tag_peel(&target, tag));
	cl_assert(git_object_type(target) == GIT_OBJECT_COMMIT);
	cl_git_pass(git_oid_streq(git_object_id(target), "e90810b8df3e80c413d903f631643c716887138d"));
}

void test_object_tag_peel__can_peel_several_nested_tags_to_a_commit(void)
{
	retrieve_tag_from_oid(&tag, repo, "b25fa35b38051e4ae45d4222e795f9df2e43f1d1");

	cl_git_pass(git_tag_peel(&target, tag));
	cl_assert(git_object_type(target) == GIT_OBJECT_COMMIT);
	cl_git_pass(git_oid_streq(git_object_id(target), "e90810b8df3e80c413d903f631643c716887138d"));
}

void test_object_tag_peel__can_peel_to_a_non_commit(void)
{
	retrieve_tag_from_oid(&tag, repo, "521d87c1ec3aef9824daf6d96cc0ae3710766d91");

	cl_git_pass(git_tag_peel(&target, tag));
	cl_assert(git_object_type(target) == GIT_OBJECT_BLOB);
	cl_git_pass(git_oid_streq(git_object_id(target), "1385f264afb75a56a5bec74243be9b367ba4ca08"));
}
