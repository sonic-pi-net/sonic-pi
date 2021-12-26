#include "describe_helpers.h"

#include "wildmatch.h"

void assert_describe(
	const char *expected_output,
	const char *revparse_spec,
	git_repository *repo,
	git_describe_options *opts,
	git_describe_format_options *fmt_opts)
{
	git_object *object;
	git_buf label = GIT_BUF_INIT;
	git_describe_result *result;

	cl_git_pass(git_revparse_single(&object, repo, revparse_spec));

	cl_git_pass(git_describe_commit(&result, object, opts));
	cl_git_pass(git_describe_format(&label, result, fmt_opts));

	cl_must_pass(wildmatch(expected_output, git_buf_cstr(&label), 0));

	git_describe_result_free(result);
	git_object_free(object);
	git_buf_dispose(&label);
}

void assert_describe_workdir(
	const char *expected_output,
	git_repository *repo,
	git_describe_options *opts,
	git_describe_format_options *fmt_opts)
{
	git_buf label = GIT_BUF_INIT;
	git_describe_result *result;

	cl_git_pass(git_describe_workdir(&result, repo, opts));
	cl_git_pass(git_describe_format(&label, result, fmt_opts));

	cl_must_pass(wildmatch(expected_output, git_buf_cstr(&label), 0));

	git_describe_result_free(result);
	git_buf_dispose(&label);
}
