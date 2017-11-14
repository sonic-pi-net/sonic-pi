#include "clar_libgit2.h"
#include "clar_libgit2_trace.h"
#include "trace.h"

static int written = 0;

static void trace_callback(git_trace_level_t level, const char *message)
{
	GIT_UNUSED(level);

	cl_assert(strcmp(message, "Hello world!") == 0);

	written = 1;
}

void test_trace_trace__initialize(void)
{
	/* If global tracing is enabled, disable for the duration of this test. */
	cl_global_trace_disable();

	git_trace_set(GIT_TRACE_INFO, trace_callback);
	written = 0;
}

void test_trace_trace__cleanup(void)
{
	git_trace_set(GIT_TRACE_NONE, NULL);

	/* If global tracing was enabled, restart it. */
	cl_global_trace_register();
}

void test_trace_trace__sets(void)
{
#ifdef GIT_TRACE
	cl_assert(git_trace_level() == GIT_TRACE_INFO);
#else
	cl_skip();
#endif
}

void test_trace_trace__can_reset(void)
{
#ifdef GIT_TRACE
	cl_assert(git_trace_level() == GIT_TRACE_INFO);
	cl_git_pass(git_trace_set(GIT_TRACE_ERROR, trace_callback));

	cl_assert(written == 0);
	git_trace(GIT_TRACE_INFO, "Hello %s!", "world");
	cl_assert(written == 0);

	git_trace(GIT_TRACE_ERROR, "Hello %s!", "world");
	cl_assert(written == 1);
#else
	cl_skip();
#endif
}

void test_trace_trace__can_unset(void)
{
#ifdef GIT_TRACE
	cl_assert(git_trace_level() == GIT_TRACE_INFO);
	cl_git_pass(git_trace_set(GIT_TRACE_NONE, NULL));

	cl_assert(git_trace_level() == GIT_TRACE_NONE);

	cl_assert(written == 0);
	git_trace(GIT_TRACE_FATAL, "Hello %s!", "world");
	cl_assert(written == 0);
#else
	cl_skip();
#endif
}

void test_trace_trace__skips_higher_level(void)
{
#ifdef GIT_TRACE
	cl_assert(written == 0);
	git_trace(GIT_TRACE_DEBUG, "Hello %s!", "world");
	cl_assert(written == 0);
#else
	cl_skip();
#endif
}

void test_trace_trace__writes(void)
{
#ifdef GIT_TRACE
	cl_assert(written == 0);
	git_trace(GIT_TRACE_INFO, "Hello %s!", "world");
	cl_assert(written == 1);
#else
	cl_skip();
#endif
}

void test_trace_trace__writes_lower_level(void)
{
#ifdef GIT_TRACE
	cl_assert(written == 0);
	git_trace(GIT_TRACE_ERROR, "Hello %s!", "world");
	cl_assert(written == 1);
#else
	cl_skip();
#endif
}
