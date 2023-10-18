#include "clar_libgit2.h"
#include "reset_helpers.h"

void reflog_check(git_repository *repo, const char *refname,
		size_t exp_count, const char *exp_email, const char *exp_msg)
{
	git_reflog *log;
	const git_reflog_entry *entry;

	GIT_UNUSED(exp_email);

	cl_git_pass(git_reflog_read(&log, repo, refname));
	cl_assert_equal_i(exp_count, git_reflog_entrycount(log));
	entry = git_reflog_entry_byindex(log, 0);

	if (exp_msg)
		cl_assert_equal_s(exp_msg, git_reflog_entry_message(entry));

	git_reflog_free(log);
}
