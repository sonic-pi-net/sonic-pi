#include "clar_libgit2.h"

#include "git2/clone.h"
#include "git2/transport.h"
#include "git2/sys/transport.h"
#include "futils.h"

static int custom_transport(
	git_transport **out,
	git_remote *owner,
	void *payload)
{
	*((int*)payload) = 1;

	return git_transport_local(out, owner, payload);
}

static int custom_transport_remote_create(
	git_remote **out,
	git_repository *repo,
	const char *name,
	const char *url,
	void *payload)
{
	int error;

	GIT_UNUSED(payload);

	if ((error = git_remote_create(out, repo, name, url)) < 0)
		return error;

	return 0;
}

void test_clone_transport__custom_transport(void)
{
	git_repository *repo;
	git_clone_options clone_opts = GIT_CLONE_OPTIONS_INIT;
	int custom_transport_used = 0;

	clone_opts.remote_cb = custom_transport_remote_create;
	clone_opts.fetch_opts.callbacks.transport = custom_transport;
	clone_opts.fetch_opts.callbacks.payload = &custom_transport_used;

	cl_git_pass(git_clone(&repo, cl_fixture("testrepo.git"), "./custom_transport.git", &clone_opts));
	git_repository_free(repo);

	cl_git_pass(git_futils_rmdir_r("./custom_transport.git", NULL, GIT_RMDIR_REMOVE_FILES));

	cl_assert(custom_transport_used == 1);
}
