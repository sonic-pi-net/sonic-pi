#include "clar_libgit2.h"
#include "buffer.h"
#include "path.h"
#include "util.h"
#include "posix.h"
#include "submodule_helpers.h"
#include "git2/sys/repository.h"

/* rewrite gitmodules -> .gitmodules
 * rewrite the empty or relative urls inside each module
 * rename the .gitted directory inside any submodule to .git
 */
void rewrite_gitmodules(const char *workdir)
{
	git_buf in_f = GIT_BUF_INIT, out_f = GIT_BUF_INIT, path = GIT_BUF_INIT;
	FILE *in, *out;
	char line[256];

	cl_git_pass(git_buf_joinpath(&in_f, workdir, "gitmodules"));
	cl_git_pass(git_buf_joinpath(&out_f, workdir, ".gitmodules"));

	cl_assert((in  = fopen(in_f.ptr, "rb")) != NULL);
	cl_assert((out = fopen(out_f.ptr, "wb")) != NULL);

	while (fgets(line, sizeof(line), in) != NULL) {
		char *scan = line;

		while (*scan == ' ' || *scan == '\t') scan++;

		/* rename .gitted -> .git in submodule directories */
		if (git__prefixcmp(scan, "path =") == 0) {
			scan += strlen("path =");
			while (*scan == ' ') scan++;

			git_buf_joinpath(&path, workdir, scan);
			git_buf_rtrim(&path);
			git_buf_joinpath(&path, path.ptr, ".gitted");

			if (!git_buf_oom(&path) && p_access(path.ptr, F_OK) == 0) {
				git_buf_joinpath(&out_f, workdir, scan);
				git_buf_rtrim(&out_f);
				git_buf_joinpath(&out_f, out_f.ptr, ".git");

				if (!git_buf_oom(&out_f))
					p_rename(path.ptr, out_f.ptr);
			}
		}

		/* copy non-"url =" lines verbatim */
		if (git__prefixcmp(scan, "url =") != 0) {
			fputs(line, out);
			continue;
		}

		/* convert relative URLs in "url =" lines */
		scan += strlen("url =");
		while (*scan == ' ') scan++;

		if (*scan == '.') {
			git_buf_joinpath(&path, workdir, scan);
			git_buf_rtrim(&path);
		} else if (!*scan || *scan == '\n') {
			git_buf_joinpath(&path, workdir, "../testrepo.git");
		} else {
			fputs(line, out);
			continue;
		}

		git_path_prettify(&path, path.ptr, NULL);
		git_buf_putc(&path, '\n');
		cl_assert(!git_buf_oom(&path));

		fwrite(line, scan - line, sizeof(char), out);
		fputs(path.ptr, out);
	}

	fclose(in);
	fclose(out);

	cl_must_pass(p_unlink(in_f.ptr));

	git_buf_free(&in_f);
	git_buf_free(&out_f);
	git_buf_free(&path);
}

static void cleanup_fixture_submodules(void *payload)
{
	cl_git_sandbox_cleanup(); /* either "submodules" or "submod2" */

	if (payload)
		cl_fixture_cleanup(payload);
}

git_repository *setup_fixture_submodules(void)
{
	git_repository *repo = cl_git_sandbox_init("submodules");

	cl_fixture_sandbox("testrepo.git");

	rewrite_gitmodules(git_repository_workdir(repo));
	p_rename("submodules/testrepo/.gitted", "submodules/testrepo/.git");

	cl_set_cleanup(cleanup_fixture_submodules, "testrepo.git");

	cl_git_pass(git_repository_reinit_filesystem(repo, 1));

	return repo;
}

git_repository *setup_fixture_submod2(void)
{
	git_repository *repo = cl_git_sandbox_init("submod2");

	cl_fixture_sandbox("submod2_target");
	p_rename("submod2_target/.gitted", "submod2_target/.git");

	rewrite_gitmodules(git_repository_workdir(repo));
	p_rename("submod2/not-submodule/.gitted", "submod2/not-submodule/.git");
	p_rename("submod2/not/.gitted", "submod2/not/.git");

	cl_set_cleanup(cleanup_fixture_submodules, "submod2_target");

	cl_git_pass(git_repository_reinit_filesystem(repo, 1));

	return repo;
}

void assert__submodule_exists(
	git_repository *repo, const char *name,
	const char *msg, const char *file, int line)
{
	git_submodule *sm;
	int error = git_submodule_lookup(&sm, repo, name);
	if (error)
		cl_git_report_failure(error, file, line, msg);
	cl_assert_at_line(sm != NULL, file, line);
	git_submodule_free(sm);
}

void refute__submodule_exists(
	git_repository *repo, const char *name, int expected_error,
	const char *msg, const char *file, int line)
{
	git_submodule *sm;
	clar__assert_equal(
		file, line, msg, 1, "%i",
		expected_error, (int)(git_submodule_lookup(&sm, repo, name)));
}

unsigned int get_submodule_status(git_repository *repo, const char *name)
{
	git_submodule *sm = NULL;
	unsigned int status = 0;

	cl_git_pass(git_submodule_lookup(&sm, repo, name));
	cl_assert(sm);
	cl_git_pass(git_submodule_status(&status, sm));
	git_submodule_free(sm);

	return status;
}

static int print_submodules(git_submodule *sm, const char *name, void *p)
{
	unsigned int loc = 0;
	GIT_UNUSED(p);
	git_submodule_location(&loc, sm);
	fprintf(stderr, "# submodule %s (at %s) flags %x\n",
		name, git_submodule_path(sm), loc);
	return 0;
}

void dump_submodules(git_repository *repo)
{
	git_submodule_foreach(repo, print_submodules, NULL);
}

