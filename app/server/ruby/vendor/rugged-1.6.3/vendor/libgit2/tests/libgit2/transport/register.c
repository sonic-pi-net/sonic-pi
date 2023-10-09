#include "clar_libgit2.h"
#include "git2/sys/remote.h"
#include "git2/sys/transport.h"

static const char *proxy_url = "https://proxy";
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
	const char *urls[] = {
		"ssh://somehost:somepath",
		"ssh+git://somehost:somepath",
		"git+ssh://somehost:somepath",
		"git@somehost:somepath",
		"ssh://somehost:somepath%20with%20%spaces",
		"ssh://somehost:somepath with spaces"
	};
	git_transport *transport;
	unsigned i;

	for (i = 0; i < ARRAY_SIZE(urls); i++) {
#ifndef GIT_SSH
		cl_git_fail_with(git_transport_new(&transport, NULL, urls[i]), -1);
#else
		cl_git_pass(git_transport_new(&transport, NULL, urls[i]));
		transport->free(transport);
#endif
	}

	cl_git_pass(git_transport_register("ssh", dummy_transport, NULL));

	cl_git_pass(git_transport_new(&transport, NULL, "git@somehost:somepath"));

	cl_assert(transport == &_transport);

	cl_git_pass(git_transport_unregister("ssh"));

	for (i = 0; i < ARRAY_SIZE(urls); i++) {
#ifndef GIT_SSH
		cl_git_fail_with(git_transport_new(&transport, NULL, urls[i]), -1);
#else
		cl_git_pass(git_transport_new(&transport, NULL, urls[i]));
		transport->free(transport);
#endif
	}
}

static int custom_subtransport_stream__read(
		git_smart_subtransport_stream *stream,
		char *buffer,
		size_t buf_size,
		size_t *bytes_read)
{
	GIT_UNUSED(stream);
	GIT_UNUSED(buffer);
	GIT_UNUSED(buf_size);

	*bytes_read = 0;

	git_error_set_str(42, "unimplemented");
	return GIT_EUSER;
}

static int custom_subtransport_stream__write(
		git_smart_subtransport_stream *stream,
		const char *buffer,
		size_t len)
{
	GIT_UNUSED(stream);
	GIT_UNUSED(buffer);
	GIT_UNUSED(len);

	git_error_set_str(42, "unimplemented");
	return GIT_EUSER;
}

static void custom_subtransport_stream__free(
		git_smart_subtransport_stream *stream)
{
	git__free(stream);
}

struct custom_subtransport {
	git_smart_subtransport subtransport;
	git_transport *owner;
	int *called;
};

static int custom_subtransport__action(
		git_smart_subtransport_stream **out,
		git_smart_subtransport *transport,
		const char *url,
		git_smart_service_t action)
{
	struct custom_subtransport *t = (struct custom_subtransport *)transport;
	git_remote_connect_options opts = GIT_REMOTE_CONNECT_OPTIONS_INIT;
	int ret;

	GIT_UNUSED(url);
	GIT_UNUSED(action);

	ret = git_transport_remote_connect_options(&opts, t->owner);

	/* increase the counter once if this function was called at all and once more if the URL matches. */
	(*t->called)++;
	if (strcmp(proxy_url, opts.proxy_opts.url) == 0)
		(*t->called)++;

	git_remote_connect_options_dispose(&opts);

	*out = git__calloc(1, sizeof(git_smart_subtransport_stream));
	(*out)->subtransport = transport;
	(*out)->read = custom_subtransport_stream__read;
	(*out)->write = custom_subtransport_stream__write;
	(*out)->free = custom_subtransport_stream__free;

	return ret;
}

static int custom_subtransport__close(git_smart_subtransport *transport)
{
	GIT_UNUSED(transport);

	return 0;
}

static void custom_subtransport__free(git_smart_subtransport *transport)
{
	GIT_UNUSED(transport);

	git__free(transport);
}

static int custom_transport_callback(git_smart_subtransport **out, git_transport *owner, void *param)
{
	struct custom_subtransport *subtransport = git__calloc(1, sizeof(struct custom_subtransport));
	subtransport->called = (int *)param;
	subtransport->owner = owner;
	subtransport->subtransport.action = custom_subtransport__action;
	subtransport->subtransport.close = custom_subtransport__close;
	subtransport->subtransport.free = custom_subtransport__free;

	*out = &subtransport->subtransport;

	return 0;
}


static int custom_transport(git_transport **out, git_remote *owner, void *param)
{
	struct git_smart_subtransport_definition definition;
	definition.callback = custom_transport_callback;
	definition.rpc = false;
	definition.param = param;
	return git_transport_smart(out, owner, &definition);
}

void test_transport_register__custom_transport_callbacks(void)
{
	git_transport *transport;
	int called = 0;
	const char *url = "custom://somepath";
	git_remote_connect_options opts = GIT_REMOTE_CONNECT_OPTIONS_INIT;
	git_repository *repo;
	git_remote *remote;

	cl_git_pass(git_repository_init(&repo, "./transport", 0));
	cl_git_pass(git_remote_create(&remote, repo, "test",
		cl_fixture("testrepo.git")));

	cl_git_pass(git_transport_register("custom", custom_transport, &called));

	cl_git_pass(git_transport_new(&transport, remote, url));

	opts.follow_redirects = GIT_REMOTE_REDIRECT_NONE;
	opts.proxy_opts.url = proxy_url;

	/* This is expected to fail, since the subtransport_stream is not implemented */
	transport->connect(transport, url, GIT_SERVICE_UPLOADPACK_LS, &opts);
	/* the counter is increased twice if everything goes as planned */
	cl_assert_equal_i(2, called);
	cl_git_pass(transport->close(transport));
	transport->free(transport);

	cl_git_pass(git_transport_unregister("custom"));
	git_remote_free(remote);
	git_repository_free(repo);
	cl_fixture_cleanup("testrepo.git");
}
