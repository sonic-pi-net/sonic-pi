#include "clar_libgit2.h"
#include "win32/w32_leakcheck.h"

#if defined(GIT_WIN32_LEAKCHECK)
static void a(void)
{
	char buf[10000];

	cl_assert(git_win32_leakcheck_stack(buf, sizeof(buf), 0, NULL, NULL) == 0);

#if 0
	fprintf(stderr, "Stacktrace from [%s:%d]:\n%s\n", __FILE__, __LINE__, buf);
#endif
}

static void b(void)
{
	a();
}

static void c(void)
{
	b();
}
#endif

void test_trace_windows_stacktrace__basic(void)
{
#if defined(GIT_WIN32_LEAKCHECK)
	c();
#endif
}


void test_trace_windows_stacktrace__leaks(void)
{
#if defined(GIT_WIN32_LEAKCHECK)
	void * p1;
	void * p2;
	void * p3;
	void * p4;
	int before, after;
	int leaks;
	int error;

	/* remember outstanding leaks due to set setup
	 * and set mark/checkpoint.
	 */
	before = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_TOTAL |
		GIT_WIN32_LEAKCHECK_STACKTRACE_SET_MARK,
		NULL);

	p1 = git__malloc(5);
	leaks = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK,
		"p1");
	cl_assert_equal_i(1, leaks);

	p2 = git__malloc(5);
	leaks = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK,
		"p1,p2");
	cl_assert_equal_i(2, leaks);

	p3 = git__malloc(5);
	leaks = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK,
		"p1,p2,p3");
	cl_assert_equal_i(3, leaks);

	git__free(p2);
	leaks = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK,
		"p1,p3");
	cl_assert_equal_i(2, leaks);

	/* move the mark. only new leaks should appear afterwards */
	error = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_SET_MARK,
		NULL);
	/* cannot use cl_git_pass() since that may allocate memory. */
	cl_assert_equal_i(0, error);

	leaks = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK,
		"not_p1,not_p3");
	cl_assert_equal_i(0, leaks);

	p4 = git__malloc(5);
	leaks = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK,
		"p4,not_p1,not_p3");
	cl_assert_equal_i(1, leaks);

	git__free(p1);
	git__free(p3);
	leaks = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK,
		"p4");
	cl_assert_equal_i(1, leaks);

	git__free(p4);
	leaks = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK,
		"end");
	cl_assert_equal_i(0, leaks);

	/* confirm current absolute leaks count matches beginning value. */
	after = git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET |
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_TOTAL,
		"total");
	cl_assert_equal_i(before, after);
#endif
}

#if defined(GIT_WIN32_LEAKCHECK)
static void aux_cb_alloc__1(unsigned int *aux_id)
{
	static unsigned int aux_counter = 0;

	*aux_id = aux_counter++;
}

static void aux_cb_lookup__1(unsigned int aux_id, char *aux_msg, size_t aux_msg_len)
{
	p_snprintf(aux_msg, aux_msg_len, "\tQQ%08x\n", aux_id);
}

#endif

void test_trace_windows_stacktrace__aux1(void)
{
#if defined(GIT_WIN32_LEAKCHECK)
	git_win32_leakcheck_stack_set_aux_cb(aux_cb_alloc__1, aux_cb_lookup__1);
	c();
	c();
	c();
	c();
	git_win32_leakcheck_stack_set_aux_cb(NULL, NULL);
#endif
}
