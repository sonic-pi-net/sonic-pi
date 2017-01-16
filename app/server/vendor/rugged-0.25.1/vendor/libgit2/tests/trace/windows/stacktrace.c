#include "clar_libgit2.h"
#include "win32/w32_stack.h"

#if defined(GIT_MSVC_CRTDBG)
static void a(void)
{
	char buf[10000];

	cl_assert(git_win32__stack(buf, sizeof(buf), 0, NULL, NULL) == 0);

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
#if defined(GIT_MSVC_CRTDBG)
	c();
#endif
}


void test_trace_windows_stacktrace__leaks(void)
{
#if defined(GIT_MSVC_CRTDBG)
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
	before = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_TOTAL |
		GIT_WIN32__CRTDBG_STACKTRACE__SET_MARK,
		NULL);

	p1 = git__malloc(5);
	leaks = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
		"p1");
	cl_assert((leaks == 1));

	p2 = git__malloc(5);
	leaks = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
		"p1,p2");
	cl_assert((leaks == 2));

	p3 = git__malloc(5);
	leaks = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
		"p1,p2,p3");
	cl_assert((leaks == 3));

	git__free(p2);
	leaks = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
		"p1,p3");
	cl_assert((leaks == 2));

	/* move the mark. only new leaks should appear afterwards */
	error = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__SET_MARK,
		NULL);
	cl_assert((error == 0));

	leaks = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
		"not_p1,not_p3");
	cl_assert((leaks == 0));

	p4 = git__malloc(5);
	leaks = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
		"p4,not_p1,not_p3");
	cl_assert((leaks == 1));

	git__free(p1);
	git__free(p3);
	leaks = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
		"p4");
	cl_assert((leaks == 1));

	git__free(p4);
	leaks = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK,
		"end");
	cl_assert((leaks == 0));

	/* confirm current absolute leaks count matches beginning value. */
	after = git_win32__crtdbg_stacktrace__dump(
		GIT_WIN32__CRTDBG_STACKTRACE__QUIET |
		GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_TOTAL,
		"total");
	cl_assert((before == after));
#endif
}

#if defined(GIT_MSVC_CRTDBG)
static void aux_cb_alloc__1(unsigned int *aux_id)
{
	static unsigned int aux_counter = 0;

	*aux_id = aux_counter++;
}

static void aux_cb_lookup__1(unsigned int aux_id, char *aux_msg, unsigned int aux_msg_len)
{
	p_snprintf(aux_msg, aux_msg_len, "\tQQ%08x\n", aux_id);
}

#endif

void test_trace_windows_stacktrace__aux1(void)
{
#if defined(GIT_MSVC_CRTDBG)
	git_win32__stack__set_aux_cb(aux_cb_alloc__1, aux_cb_lookup__1);
	c();
	c();
	c();
	c();
	git_win32__stack__set_aux_cb(NULL, NULL);
#endif
}
