#include "clar_libgit2.h"

#include "util.h"

void test_date_date__overflow(void)
{
#ifdef __LP64__
   git_time_t d2038, d2039;

   /* This is expected to fail on a 32-bit machine. */
   cl_git_pass(git__date_parse(&d2038, "2038-1-1"));
   cl_git_pass(git__date_parse(&d2039, "2039-1-1"));
   cl_assert(d2038 < d2039);
#endif
}
