#include "clar_libgit2.h"
#include "git2/sys/stream.h"
#include "streams/tls.h"
#include "streams/socket.h"
#include "stream.h"

static git_stream test_stream;
static int ctor_called;

void test_stream_registration__cleanup(void)
{
	cl_git_pass(git_stream_register(GIT_STREAM_TLS | GIT_STREAM_STANDARD, NULL));
}

static int test_stream_init(git_stream **out, const char *host, const char *port)
{
	GIT_UNUSED(host);
	GIT_UNUSED(port);

	ctor_called = 1;
	*out = &test_stream;

	return 0;
}

static int test_stream_wrap(git_stream **out, git_stream *in, const char *host)
{
	GIT_UNUSED(in);
	GIT_UNUSED(host);

	ctor_called = 1;
	*out = &test_stream;

	return 0;
}

void test_stream_registration__insecure(void)
{
	git_stream *stream;
	git_stream_registration registration = {0};

	registration.version = 1;
	registration.init = test_stream_init;
	registration.wrap = test_stream_wrap;

	ctor_called = 0;
	cl_git_pass(git_stream_register(GIT_STREAM_STANDARD, &registration));
	cl_git_pass(git_socket_stream_new(&stream, "localhost", "80"));
	cl_assert_equal_i(1, ctor_called);
	cl_assert_equal_p(&test_stream, stream);

	ctor_called = 0;
	stream = NULL;
	cl_git_pass(git_stream_register(GIT_STREAM_STANDARD, NULL));
	cl_git_pass(git_socket_stream_new(&stream, "localhost", "80"));

	cl_assert_equal_i(0, ctor_called);
	cl_assert(&test_stream != stream);

	git_stream_free(stream);
}

void test_stream_registration__tls(void)
{
	git_stream *stream;
	git_stream_registration registration = {0};
	int error;

	registration.version = 1;
	registration.init = test_stream_init;
	registration.wrap = test_stream_wrap;

	ctor_called = 0;
	cl_git_pass(git_stream_register(GIT_STREAM_TLS, &registration));
	cl_git_pass(git_tls_stream_new(&stream, "localhost", "443"));
	cl_assert_equal_i(1, ctor_called);
	cl_assert_equal_p(&test_stream, stream);

	ctor_called = 0;
	stream = NULL;
	cl_git_pass(git_stream_register(GIT_STREAM_TLS, NULL));
	error = git_tls_stream_new(&stream, "localhost", "443");

	/* We don't have TLS support enabled, or we're on Windows,
	 * which has no arbitrary TLS stream support.
	 */
#if defined(GIT_WIN32) || !defined(GIT_HTTPS)
	cl_git_fail_with(-1, error);
#else
	cl_git_pass(error);
#endif

	cl_assert_equal_i(0, ctor_called);
	cl_assert(&test_stream != stream);

	git_stream_free(stream);
}

void test_stream_registration__both(void)
{
	git_stream *stream;
	git_stream_registration registration = {0};

	registration.version = 1;
	registration.init = test_stream_init;
	registration.wrap = test_stream_wrap;

	cl_git_pass(git_stream_register(GIT_STREAM_STANDARD | GIT_STREAM_TLS, &registration));

	ctor_called = 0;
	cl_git_pass(git_tls_stream_new(&stream, "localhost", "443"));
	cl_assert_equal_i(1, ctor_called);
	cl_assert_equal_p(&test_stream, stream);

	ctor_called = 0;
	cl_git_pass(git_socket_stream_new(&stream, "localhost", "80"));
	cl_assert_equal_i(1, ctor_called);
	cl_assert_equal_p(&test_stream, stream);
}
