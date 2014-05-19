#include "clar_libgit2.h"

#ifdef _WIN32
int __cdecl main(int argc, char *argv[])
#else
int main(int argc, char *argv[])
#endif
{
	int res;

	clar_test_init(argc, argv);

	git_threads_init();
	cl_sandbox_set_search_path_defaults();

	/* Run the test suite */
	res = clar_test_run();

	clar_test_shutdown();

	giterr_clear();
	git_threads_shutdown();

	return res;
}
