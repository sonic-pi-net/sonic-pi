#include "clar_libgit2.h"
#include "clar_libgit2_trace.h"

#ifdef GIT_WIN32_LEAKCHECK
# include "win32/w32_leakcheck.h"
#endif

#ifdef _WIN32
int __cdecl main(int argc, char *argv[])
#else
int main(int argc, char *argv[])
#endif
{
	int res;
	char *at_exit_cmd;

	clar_test_init(argc, argv);

	res = git_libgit2_init();
	if (res < 0) {
		const git_error *err = git_error_last();
		const char *msg = err ? err->message : "unknown failure";
		fprintf(stderr, "failed to init libgit2: %s\n", msg);
		return res;
	}

	cl_global_trace_register();
	cl_sandbox_set_search_path_defaults();

	/* Run the test suite */
	res = clar_test_run();

	clar_test_shutdown();

	cl_global_trace_disable();
	git_libgit2_shutdown();

#ifdef GIT_WIN32_LEAKCHECK
	if (git_win32_leakcheck_has_leaks())
		res = res || 1;
#endif

	at_exit_cmd = getenv("CLAR_AT_EXIT");
	if (at_exit_cmd != NULL) {
		int at_exit = system(at_exit_cmd);
		return res || at_exit;
	}

	return res;
}
