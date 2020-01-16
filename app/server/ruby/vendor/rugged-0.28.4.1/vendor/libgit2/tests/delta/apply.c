#include "clar_libgit2.h"

#include "delta.h"

void test_delta_apply__read_at_off(void)
{
	unsigned char base[16] = { 0 }, delta[] = { 0x10, 0x10, 0xff, 0xff, 0xff, 0xff, 0xff, 0x10, 0x00, 0x00 };
	void *out;
	size_t outlen;

	cl_git_fail(git_delta_apply(&out, &outlen, base, sizeof(base), delta, sizeof(delta)));
}

void test_delta_apply__read_after_limit(void)
{
	unsigned char base[16] = { 0 }, delta[] = { 0x10, 0x70, 0xff };
	void *out;
	size_t outlen;

	cl_git_fail(git_delta_apply(&out, &outlen, base, sizeof(base), delta, sizeof(delta)));
}
