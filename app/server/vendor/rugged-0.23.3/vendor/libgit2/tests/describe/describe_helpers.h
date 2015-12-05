#include "clar_libgit2.h"
#include "buffer.h"

extern void assert_describe(
	const char *expected_output,
	const char *revparse_spec,
	git_repository *repo,
	git_describe_options *opts,
	git_describe_format_options *fmt_opts);

extern void assert_describe_workdir(
	const char *expected_output,
	git_repository *repo,
	git_describe_options *opts,
	git_describe_format_options *fmt_opts);
