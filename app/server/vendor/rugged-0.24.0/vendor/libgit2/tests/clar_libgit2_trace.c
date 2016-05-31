#include "clar_libgit2.h"
#include "clar_libgit2_trace.h"
#include "clar_libgit2_timer.h"
#include "trace.h"


struct method {
	const char *name;
	void (*git_trace_cb)(git_trace_level_t level, const char *msg);
	void (*close)(void);
};


#if defined(GIT_TRACE)
static void _git_trace_cb__printf(git_trace_level_t level, const char *msg)
{
	/* TODO Use level to print a per-message prefix. */
	GIT_UNUSED(level);

	printf("%s\n", msg);
}

#if defined(GIT_WIN32)
static void _git_trace_cb__debug(git_trace_level_t level, const char *msg)
{
	/* TODO Use level to print a per-message prefix. */
	GIT_UNUSED(level);

	OutputDebugString(msg);
	OutputDebugString("\n");

	printf("%s\n", msg);
}
#else
#define _git_trace_cb__debug _git_trace_cb__printf
#endif


static void _trace_printf_close(void)
{
	fflush(stdout);
}

#define _trace_debug_close _trace_printf_close


static struct method s_methods[] = {
	{ "printf", _git_trace_cb__printf, _trace_printf_close },
	{ "debug",  _git_trace_cb__debug,  _trace_debug_close  },
	/* TODO add file method */
	{0},
};


static int s_trace_loaded = 0;
static int s_trace_level = GIT_TRACE_NONE;
static struct method *s_trace_method = NULL;


static int set_method(const char *name)
{
	int k;

	if (!name || !*name)
		name = "printf";

	for (k=0; (s_methods[k].name); k++) {
		if (strcmp(name, s_methods[k].name) == 0) {
			s_trace_method = &s_methods[k];
			return 0;
		}
	}
	fprintf(stderr, "Unknown CLAR_TRACE_METHOD: '%s'\n", name);
	return -1;
}


/**
 * Lookup CLAR_TRACE_LEVEL and CLAR_TRACE_METHOD from
 * the environment and set the above s_trace_* fields.
 *
 * If CLAR_TRACE_LEVEL is not set, we disable tracing.
 *
 * TODO If set, we assume GIT_TRACE_TRACE level, which
 * logs everything. Later, we may want to parse the
 * value of the environment variable and set a specific
 * level.
 *
 * We assume the "printf" method.  This can be changed
 * with the CLAR_TRACE_METHOD environment variable.
 * Currently, this is only needed on Windows for a "debug"
 * version which also writes to the debug output window
 * in Visual Studio.
 *
 * TODO add a "file" method that would open and write
 * to a well-known file. This would help keep trace
 * output and clar output separate.
 *
 */
static void _load_trace_params(void)
{
	char *sz_level;
	char *sz_method;

	s_trace_loaded = 1;

	sz_level = cl_getenv("CLAR_TRACE_LEVEL");
	if (!sz_level || !*sz_level) {
		s_trace_level = GIT_TRACE_NONE;
		s_trace_method = NULL;
		return;
	}

	/* TODO Parse sz_level and set s_trace_level. */
	s_trace_level = GIT_TRACE_TRACE;

	sz_method = cl_getenv("CLAR_TRACE_METHOD");
	if (set_method(sz_method) < 0)
		set_method(NULL);
}

#define HR "================================================================"

/**
 * Timer to report the take spend in a test's run() method.
 */
static cl_perf_timer s_timer_run = CL_PERF_TIMER_INIT;

/**
 * Timer to report total time in a test (init, run, cleanup).
 */
static cl_perf_timer s_timer_test = CL_PERF_TIMER_INIT;

void _cl_trace_cb__event_handler(
	cl_trace_event ev,
	const char *suite_name,
	const char *test_name,
	void *payload)
{
	GIT_UNUSED(payload);

	switch (ev) {
	case CL_TRACE__SUITE_BEGIN:
		git_trace(GIT_TRACE_TRACE, "\n\n%s\n%s: Begin Suite", HR, suite_name);
#if 0 && defined(GIT_MSVC_CRTDBG)
		git_win32__crtdbg_stacktrace__dump(
			GIT_WIN32__CRTDBG_STACKTRACE__SET_MARK,
			suite_name);
#endif
		break;

	case CL_TRACE__SUITE_END:
#if 0 && defined(GIT_MSVC_CRTDBG)
		/* As an example of checkpointing, dump leaks within this suite.
		 * This may generate false positives for things like the global
		 * TLS error state and maybe the odb cache since they aren't
		 * freed until the global shutdown and outside the scope of this
		 * set of tests.
		 *
		 * This may under-report if the test itself uses a checkpoint.
		 * See tests/trace/windows/stacktrace.c
		 */
		git_win32__crtdbg_stacktrace__dump(
			GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
			suite_name);
#endif
		git_trace(GIT_TRACE_TRACE, "\n\n%s: End Suite\n%s", suite_name, HR);
		break;

	case CL_TRACE__TEST__BEGIN:
		git_trace(GIT_TRACE_TRACE, "\n%s::%s: Begin Test", suite_name, test_name);
		cl_perf_timer__init(&s_timer_test);
		cl_perf_timer__start(&s_timer_test);
		break;

	case CL_TRACE__TEST__END:
		cl_perf_timer__stop(&s_timer_test);
		git_trace(GIT_TRACE_TRACE, "%s::%s: End Test (%.3f %.3f)", suite_name, test_name,
				  cl_perf_timer__last(&s_timer_run),
				  cl_perf_timer__last(&s_timer_test));
		break;

	case CL_TRACE__TEST__RUN_BEGIN:
		git_trace(GIT_TRACE_TRACE, "%s::%s: Begin Run", suite_name, test_name);
		cl_perf_timer__init(&s_timer_run);
		cl_perf_timer__start(&s_timer_run);
		break;

	case CL_TRACE__TEST__RUN_END:
		cl_perf_timer__stop(&s_timer_run);
		git_trace(GIT_TRACE_TRACE, "%s::%s: End Run", suite_name, test_name);
		break;

	case CL_TRACE__TEST__LONGJMP:
		cl_perf_timer__stop(&s_timer_run);
		git_trace(GIT_TRACE_TRACE, "%s::%s: Aborted", suite_name, test_name);
		break;

	default:
		break;
	}
}

#endif /*GIT_TRACE*/

/**
 * Setup/Enable git_trace() based upon settings user's environment.
 *
 */
void cl_global_trace_register(void)
{
#if defined(GIT_TRACE)
	if (!s_trace_loaded)
		_load_trace_params();

	if (s_trace_level == GIT_TRACE_NONE)
		return;
	if (s_trace_method == NULL)
		return;
	if (s_trace_method->git_trace_cb == NULL)
		return;

	git_trace_set(s_trace_level, s_trace_method->git_trace_cb);
	cl_trace_register(_cl_trace_cb__event_handler, NULL);
#endif
}

/**
 * If we turned on git_trace() earlier, turn it off.
 *
 * This is intended to let us close/flush any buffered
 * IO if necessary.
 *
 */
void cl_global_trace_disable(void)
{
#if defined(GIT_TRACE)
	cl_trace_register(NULL, NULL);
	git_trace_set(GIT_TRACE_NONE, NULL);
	if (s_trace_method && s_trace_method->close)
		s_trace_method->close();

	/* Leave s_trace_ vars set so they can restart tracing
	 * since we only want to hit the environment variables
	 * once.
	 */
#endif
}
