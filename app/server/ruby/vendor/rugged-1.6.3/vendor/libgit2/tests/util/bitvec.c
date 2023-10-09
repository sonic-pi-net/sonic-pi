#include "clar_libgit2.h"
#include "bitvec.h"

#if 0
static void print_bitvec(git_bitvec *bv)
{
	int b;

	if (!bv->length) {
		for (b = 63; b >= 0; --b)
			fprintf(stderr, "%d", (bv->u.bits & (1ul << b)) ? 1 : 0);
	} else {
		for (b = bv->length * 8; b >= 0; --b)
			fprintf(stderr, "%d", (bv->u.ptr[b >> 3] & (b & 0x0ff)) ? 1 : 0);
	}
	fprintf(stderr, "\n");
}
#endif

static void set_some_bits(git_bitvec *bv, size_t length)
{
	size_t i;

	for (i = 0; i < length; ++i) {
		if (i % 3 == 0 || i % 7 == 0)
			git_bitvec_set(bv, i, true);
	}
}

static void check_some_bits(git_bitvec *bv, size_t length)
{
	size_t i;

	for (i = 0; i < length; ++i)
		cl_assert_equal_b(i % 3 == 0 || i % 7 == 0, git_bitvec_get(bv, i));
}

void test_bitvec__0(void)
{
	git_bitvec bv;

	cl_git_pass(git_bitvec_init(&bv, 32));
	set_some_bits(&bv, 16);
	check_some_bits(&bv, 16);
	git_bitvec_clear(&bv);
	set_some_bits(&bv, 32);
	check_some_bits(&bv, 32);
	git_bitvec_clear(&bv);
	set_some_bits(&bv, 64);
	check_some_bits(&bv, 64);
	git_bitvec_free(&bv);

	cl_git_pass(git_bitvec_init(&bv, 128));
	set_some_bits(&bv, 32);
	check_some_bits(&bv, 32);
	set_some_bits(&bv, 128);
	check_some_bits(&bv, 128);
	git_bitvec_free(&bv);

	cl_git_pass(git_bitvec_init(&bv, 4000));
	set_some_bits(&bv, 4000);
	check_some_bits(&bv, 4000);
	git_bitvec_free(&bv);
}
