#include "clar_libgit2.h"
#include "helper__perf__do_merge.h"

/* This test requires a large repo with many files.
 * It doesn't care about the contents, just the size.
 * 
 * For now, we use the LibGit2 repo containing the
 * source tree because it is already here.
 *
 * `find . | wc -l` reports 5128.
 * 
 */
#define SRC_REPO (cl_fixture("../.."))

/* We need 2 arbitrary commits within that repo
 * that have a large number of changed files.
 * Again, we don't care about the actual contents,
 * just the size.
 *
 * For now, we use these public branches:
 * maint/v0.21 d853fb9f24e0fe63b3dce9fbc04fd9cfe17a030b Always checkout with case sensitive iterator
 * maint/v0.22 1ce9ea3ba9b4fa666602d52a5281d41a482cc58b checkout tests: cleanup realpath impl on Win32
 *
 */
#define ID_BRANCH_A "d853fb9f24e0fe63b3dce9fbc04fd9cfe17a030b"
#define ID_BRANCH_B "1ce9ea3ba9b4fa666602d52a5281d41a482cc58b"

void test_perf_merge__m1(void)
{
	perf__do_merge(SRC_REPO, "m1", ID_BRANCH_A, ID_BRANCH_B);
}
