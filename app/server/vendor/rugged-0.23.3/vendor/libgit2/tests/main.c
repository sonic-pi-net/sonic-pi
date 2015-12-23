
#if defined(GIT_MSVC_CRTDBG)
/* Enable MSVC CRTDBG memory leak reporting.  See src/util.h for details. */
#include <stdlib.h>
#include <crtdbg.h>
#endif

#include "clar_libgit2.h"
#include "clar_libgit2_trace.h"

#ifdef _WIN32
int __cdecl main(int argc, char *argv[])
#else
int main(int argc, char *argv[])
#endif
{
	int res;

#if defined(GIT_MSVC_CRTDBG)
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);

	_CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_DEBUG | _CRTDBG_MODE_FILE);
	_CrtSetReportMode(_CRT_ERROR,  _CRTDBG_MODE_DEBUG | _CRTDBG_MODE_FILE);
	_CrtSetReportMode(_CRT_WARN,   _CRTDBG_MODE_DEBUG | _CRTDBG_MODE_FILE);

	_CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);
	_CrtSetReportFile(_CRT_ERROR,  _CRTDBG_FILE_STDERR);
	_CrtSetReportFile(_CRT_WARN,   _CRTDBG_FILE_STDERR);
#endif

	clar_test_init(argc, argv);

	git_libgit2_init();
	cl_global_trace_register();
	cl_sandbox_set_search_path_defaults();

	/* Run the test suite */
	res = clar_test_run();

	clar_test_shutdown();

	cl_global_trace_disable();
	git_libgit2_shutdown();

	return res;
}
