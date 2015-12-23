#include "clar_libgit2.h"
#include "git2/sys/transport.h"

static git_transport _transport = GIT_TRANSPORT_INIT;

static int dummy_transport(git_transport **transport, git_remote *owner, void *param)
{
	*transport = &_transport;
	GIT_UNUSED(owner);
	GIT_UNUSED(param);
	return 0;
}

void test_transport_register__custom_transport(void)
{
	git_transport *transport;

	cl_git_pass(git_transport_register("something", dummy_transport, NULL));

	cl_git_pass(git_transport_new(&transport, NULL, "something://somepath"));

	cl_assert(transport == &_transport);

	cl_git_pass(git_transport_unregister("something"));
}

void test_transport_register__custom_transport_error_doubleregister(void)
{
	cl_git_pass(git_transport_register("something", dummy_transport, NULL));

	cl_git_fail_with(git_transport_register("something", dummy_transport, NULL), GIT_EEXISTS);

	cl_git_pass(git_transport_unregister("something"));
}

void test_transport_register__custom_transport_error_remove_non_existing(void)
{
	cl_git_fail_with(git_transport_unregister("something"), GIT_ENOTFOUND);
}

void test_transport_register__custom_transport_ssh(void)
{
	git_transport *transport;

#ifndef GIT_SSH
	cl_git_fail_with(git_transport_new(&transport, NULL, "ssh://somehost:somepath"), -1);
	cl_git_fail_with(git_transport_new(&transport, NULL, "git@somehost:somepath"), -1);
#else
	cl_git_pass(git_transport_new(&transport, NULL, "git@somehost:somepath"));
	transport->free(transport);
#endif

	cl_git_pass(git_transport_register("ssh", dummy_transport, NULL));

	cl_git_pass(git_transport_new(&transport, NULL, "git@somehost:somepath"));

	cl_assert(transport == &_transport);

	cl_git_pass(git_transport_unregister("ssh"));

#ifndef GIT_SSH
	cl_git_fail_with(git_transport_new(&transport, NULL, "ssh://somehost:somepath"), -1);
	cl_git_fail_with(git_transport_new(&transport, NULL, "git@somehost:somepath"), -1);
#else
	cl_git_pass(git_transport_new(&transport, NULL, "git@somehost:somepath"));
	transport->free(transport);
#endif
}
