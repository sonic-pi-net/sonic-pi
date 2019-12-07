#include "clar_libgit2.h"
#include "git2/sys/stream.h"
#include "tls_stream.h"
#include "stream.h"

static git_stream test_stream;
static int ctor_called;

static int test_ctor(git_stream **out, const char *host, const char *port)
{
	GIT_UNUSED(host);
	GIT_UNUSED(port);

	ctor_called = 1;
	*out = &test_stream;

	return 0;
}

void test_core_stream__register_tls(void)
{
	git_stream *stream;
	int error;

	ctor_called = 0;
	cl_git_pass(git_stream_register_tls(test_ctor));
	cl_git_pass(git_tls_stream_new(&stream, "localhost", "443"));
	cl_assert_equal_i(1, ctor_called);
	cl_assert_equal_p(&test_stream, stream);

	ctor_called = 0;
	stream = NULL;
	cl_git_pass(git_stream_register_tls(NULL));
	error = git_tls_stream_new(&stream, "localhost", "443");

	/* We don't have arbitrary TLS stream support on Windows
	 * or when openssl support is disabled (except on OSX
	 * with Security framework).
	 */
#if defined(GIT_WIN32) || \
	(!defined(GIT_SECURE_TRANSPORT) && !defined(GIT_OPENSSL))
	cl_git_fail_with(-1, error);
#else
	cl_git_pass(error);
#endif

	cl_assert_equal_i(0, ctor_called);
	cl_assert(&test_stream != stream);

	git_stream_free(stream);
}
