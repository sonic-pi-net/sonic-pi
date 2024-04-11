#ifndef __CLAR_LIBGIT2_ALLOC__
#define __CLAR_LIBGIT2_ALLOC__

#include "clar.h"
#include "common.h"
#include "git2/sys/alloc.h"

void cl_alloc_limit(size_t bytes);
void cl_alloc_reset(void);

#endif
