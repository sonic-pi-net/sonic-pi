#include "clar_libgit2.h"
#include "hash.h"

#define FIXTURE_DIR "sha1"

#ifdef GIT_SHA256_WIN32
static git_hash_win32_provider_t orig_provider;
#endif

void test_sha256__initialize(void)
{
#ifdef GIT_SHA256_WIN32
	orig_provider = git_hash_win32_provider();
#endif

	cl_fixture_sandbox(FIXTURE_DIR);
}

void test_sha256__cleanup(void)
{
#ifdef GIT_SHA256_WIN32
	git_hash_win32_set_provider(orig_provider);
#endif

	cl_fixture_cleanup(FIXTURE_DIR);
}

static int sha256_file(unsigned char *out, const char *filename)
{
	git_hash_ctx ctx;
	char buf[2048];
	int fd, ret;
	ssize_t read_len;

	fd = p_open(filename, O_RDONLY);
	cl_assert(fd >= 0);

	cl_git_pass(git_hash_ctx_init(&ctx, GIT_HASH_ALGORITHM_SHA256));

	while ((read_len = p_read(fd, buf, 2048)) > 0)
		cl_git_pass(git_hash_update(&ctx, buf, (size_t)read_len));

	cl_assert_equal_i(0, read_len);
	p_close(fd);

	ret = git_hash_final(out, &ctx);
	git_hash_ctx_cleanup(&ctx);

	return ret;
}

void test_sha256__empty(void)
{
	unsigned char expected[GIT_HASH_SHA256_SIZE] = {
		0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14,
		0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24,
		0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c,
		0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55
	};
	unsigned char actual[GIT_HASH_SHA256_SIZE];

	cl_git_pass(sha256_file(actual, FIXTURE_DIR "/empty"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA256_SIZE));
}

void test_sha256__hello(void)
{
	unsigned char expected[GIT_HASH_SHA256_SIZE] = {
		0xaa, 0x32, 0x7f, 0xae, 0x5c, 0x91, 0x58, 0x3a,
		0x4f, 0xb6, 0x54, 0xcc, 0xb6, 0xc2, 0xb1, 0x0c,
		0x77, 0xd7, 0x49, 0xc9, 0x91, 0x2a, 0x8d, 0x6b,
		0x47, 0x26, 0x13, 0xc0, 0xa0, 0x4b, 0x4d, 0xad
	};
	unsigned char actual[GIT_HASH_SHA256_SIZE];

	cl_git_pass(sha256_file(actual, FIXTURE_DIR "/hello_c"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA256_SIZE));
}

void test_sha256__pdf(void)
{
	unsigned char expected[GIT_HASH_SHA256_SIZE] = {
		0x2b, 0xb7, 0x87, 0xa7, 0x3e, 0x37, 0x35, 0x2f,
		0x92, 0x38, 0x3a, 0xbe, 0x7e, 0x29, 0x02, 0x93,
		0x6d, 0x10, 0x59, 0xad, 0x9f, 0x1b, 0xa6, 0xda,
		0xaa, 0x9c, 0x1e, 0x58, 0xee, 0x69, 0x70, 0xd0
	};
	unsigned char actual[GIT_HASH_SHA256_SIZE];

	cl_git_pass(sha256_file(actual, FIXTURE_DIR "/shattered-1.pdf"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA256_SIZE));
}

void test_sha256__win32_providers(void)
{
#ifdef GIT_SHA256_WIN32
	unsigned char expected[GIT_HASH_SHA256_SIZE] = {
		0x2b, 0xb7, 0x87, 0xa7, 0x3e, 0x37, 0x35, 0x2f,
		0x92, 0x38, 0x3a, 0xbe, 0x7e, 0x29, 0x02, 0x93,
		0x6d, 0x10, 0x59, 0xad, 0x9f, 0x1b, 0xa6, 0xda,
		0xaa, 0x9c, 0x1e, 0x58, 0xee, 0x69, 0x70, 0xd0
	};
	unsigned char actual[GIT_HASH_SHA256_SIZE];

	git_hash_win32_set_provider(GIT_HASH_WIN32_CRYPTOAPI);
	cl_git_pass(sha256_file(actual, FIXTURE_DIR "/shattered-1.pdf"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA256_SIZE));

	git_hash_win32_set_provider(GIT_HASH_WIN32_CNG);
	cl_git_pass(sha256_file(actual, FIXTURE_DIR "/shattered-1.pdf"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA256_SIZE));
#endif
}
