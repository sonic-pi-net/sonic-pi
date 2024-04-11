#include "clar_libgit2.h"

#include "date.h"

void test_date_date__overflow(void)
{
#ifdef __LP64__
   git_time_t d2038, d2039;

   /* This is expected to fail on a 32-bit machine. */
   cl_git_pass(git_date_parse(&d2038, "2038-1-1"));
   cl_git_pass(git_date_parse(&d2039, "2039-1-1"));
   cl_assert(d2038 < d2039);
#endif
}

void test_date_date__invalid_date(void)
{
   git_time_t d;
   cl_git_fail(git_date_parse(&d, ""));
   cl_git_fail(git_date_parse(&d, "NEITHER_INTEGER_NOR_DATETIME"));
}
