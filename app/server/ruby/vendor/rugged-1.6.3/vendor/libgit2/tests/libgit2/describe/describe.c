#include "clar_libgit2.h"
#include "describe_helpers.h"

void test_describe_describe__can_describe_against_a_bare_repo(void)
{
	git_repository *repo;
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_describe_format_options fmt_opts = GIT_DESCRIBE_FORMAT_OPTIONS_INIT;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	assert_describe("hard_tag", "HEAD", repo, &opts, &fmt_opts);

	opts.show_commit_oid_as_fallback = 1;

	assert_describe("be3563a*", "HEAD^", repo, &opts, &fmt_opts);

	git_repository_free(repo);
}

static int delete_cb(git_reference *ref, void *payload)
{
	GIT_UNUSED(payload);

	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);

	return 0;
}

void test_describe_describe__describe_a_repo_with_no_refs(void)
{
	git_repository *repo;
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_str buf = GIT_STR_INIT;
	git_object *object;
	git_describe_result *result = NULL;

	repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_revparse_single(&object, repo, "HEAD"));

	cl_git_pass(git_reference_foreach(repo, delete_cb, NULL));

	/* Impossible to describe without falling back to OIDs */
	cl_git_fail(git_describe_commit(&result, object, &opts));

	/* Try again with OID fallbacks */
	opts.show_commit_oid_as_fallback = 1;
	cl_git_pass(git_describe_commit(&result, object, &opts));

	git_describe_result_free(result);
	git_object_free(object);
	git_str_dispose(&buf);
	cl_git_sandbox_cleanup();
}
