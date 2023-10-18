#include "clar_libgit2.h"
#include "pool.h"
#include "git2/oid.h"

static char to_hex[] = "0123456789abcdef";

void test_core_pool__oid(void)
{
	git_pool p;
	char oid_hex[GIT_OID_SHA1_HEXSIZE];
	git_oid *oid;
	int i, j;

	memset(oid_hex, '0', sizeof(oid_hex));

	git_pool_init(&p, sizeof(git_oid));
	p.page_size = 4000;

	for (i = 1000; i < 10000; i++) {
		oid = git_pool_malloc(&p, 1);
		cl_assert(oid != NULL);

		for (j = 0; j < 8; j++)
			oid_hex[j] = to_hex[(i >> (4 * j)) & 0x0f];
		cl_git_pass(git_oid__fromstr(oid, oid_hex, GIT_OID_SHA1));
	}

#ifndef GIT_DEBUG_POOL
	/* with fixed page size, allocation must end up with these values */

# ifdef GIT_EXPERIMENTAL_SHA256
	cl_assert_equal_i(sizeof(void *) == 8 ? 90 : 82, git_pool__open_pages(&p));
# else
	cl_assert_equal_i(sizeof(void *) == 8 ? 55 : 45, git_pool__open_pages(&p));
# endif
#endif
	git_pool_clear(&p);
}
