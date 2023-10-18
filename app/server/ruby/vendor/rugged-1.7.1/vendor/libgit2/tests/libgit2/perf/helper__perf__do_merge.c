#include "clar_libgit2.h"
#include "helper__perf__do_merge.h"
#include "helper__perf__timer.h"

static git_repository * g_repo;

void perf__do_merge(const char *fixture,
					const char *test_name,
					const char *id_a,
					const char *id_b)
{
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_clone_options clone_opts = GIT_CLONE_OPTIONS_INIT;
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;
	git_oid oid_a;
	git_oid oid_b;
	git_reference *ref_branch_a = NULL;
	git_reference *ref_branch_b = NULL;
	git_commit *commit_a = NULL;
	git_commit *commit_b = NULL;
	git_annotated_commit *annotated_commits[1] = { NULL };
	perf_timer t_total = PERF_TIMER_INIT;
	perf_timer t_clone = PERF_TIMER_INIT;
	perf_timer t_checkout = PERF_TIMER_INIT;
	perf_timer t_merge = PERF_TIMER_INIT;

	perf__timer__start(&t_total);

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;
	clone_opts.checkout_opts = checkout_opts;

	perf__timer__start(&t_clone);
	cl_git_pass(git_clone(&g_repo, fixture, test_name, &clone_opts));
	perf__timer__stop(&t_clone);
	
	git_oid__fromstr(&oid_a, id_a, GIT_OID_SHA1);
	cl_git_pass(git_commit_lookup(&commit_a, g_repo, &oid_a));
	cl_git_pass(git_branch_create(&ref_branch_a, g_repo,
								  "A", commit_a,
								  0));

	perf__timer__start(&t_checkout);
	cl_git_pass(git_checkout_tree(g_repo, (git_object*)commit_a, &checkout_opts));
	perf__timer__stop(&t_checkout);

	cl_git_pass(git_repository_set_head(g_repo, git_reference_name(ref_branch_a)));

	git_oid__fromstr(&oid_b, id_b, GIT_OID_SHA1);
	cl_git_pass(git_commit_lookup(&commit_b, g_repo, &oid_b));
	cl_git_pass(git_branch_create(&ref_branch_b, g_repo,
								  "B", commit_b,
								  0));

	cl_git_pass(git_annotated_commit_lookup(&annotated_commits[0], g_repo, &oid_b));

	perf__timer__start(&t_merge);
	cl_git_pass(git_merge(g_repo,
						  (const git_annotated_commit **)annotated_commits, 1,
						  &merge_opts, &checkout_opts));
	perf__timer__stop(&t_merge);

	git_reference_free(ref_branch_a);
	git_reference_free(ref_branch_b);
	git_commit_free(commit_a);
	git_commit_free(commit_b);
	git_annotated_commit_free(annotated_commits[0]);
	git_repository_free(g_repo);

	perf__timer__stop(&t_total);

	perf__timer__report(&t_clone, "%s: clone", test_name);
	perf__timer__report(&t_checkout, "%s: checkout", test_name);
	perf__timer__report(&t_merge, "%s: merge", test_name);
	perf__timer__report(&t_total, "%s: total", test_name);
}
