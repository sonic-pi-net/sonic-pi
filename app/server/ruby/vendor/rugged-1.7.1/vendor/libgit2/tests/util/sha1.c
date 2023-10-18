#include "clar_libgit2.h"
#include "hash.h"

#define FIXTURE_DIR "sha1"

#ifdef GIT_SHA1_WIN32
static git_hash_win32_provider_t orig_provider;
#endif

void test_sha1__initialize(void)
{
#ifdef GIT_SHA1_WIN32
	orig_provider = git_hash_win32_provider();
#endif

	cl_fixture_sandbox(FIXTURE_DIR);
}

void test_sha1__cleanup(void)
{
#ifdef GIT_SHA1_WIN32
	git_hash_win32_set_provider(orig_provider);
#endif

	cl_fixture_cleanup(FIXTURE_DIR);
}

static int sha1_file(unsigned char *out, const char *filename)
{
	git_hash_ctx ctx;
	char buf[2048];
	int fd, ret;
	ssize_t read_len;

	fd = p_open(filename, O_RDONLY);
	cl_assert(fd >= 0);

	cl_git_pass(git_hash_ctx_init(&ctx, GIT_HASH_ALGORITHM_SHA1));

	while ((read_len = p_read(fd, buf, 2048)) > 0)
		cl_git_pass(git_hash_update(&ctx, buf, (size_t)read_len));

	cl_assert_equal_i(0, read_len);
	p_close(fd);

	ret = git_hash_final(out, &ctx);
	git_hash_ctx_cleanup(&ctx);

	return ret;
}

void test_sha1__sum(void)
{
	unsigned char expected[GIT_HASH_SHA1_SIZE] = {
		0x4e, 0x72, 0x67, 0x9e, 0x3e, 0xa4, 0xd0, 0x4e, 0x0c, 0x64,
		0x2f, 0x02, 0x9e, 0x61, 0xeb, 0x80, 0x56, 0xc7, 0xed, 0x94
	};
	unsigned char actual[GIT_HASH_SHA1_SIZE];

	cl_git_pass(sha1_file(actual, FIXTURE_DIR "/hello_c"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA1_SIZE));
}

/* test that sha1 collision detection works when enabled */
void test_sha1__detect_collision_attack(void)
{
	unsigned char actual[GIT_HASH_SHA1_SIZE];
	unsigned char expected[GIT_HASH_SHA1_SIZE] = {
		0x38, 0x76, 0x2c, 0xf7, 0xf5, 0x59, 0x34, 0xb3, 0x4d, 0x17,
		0x9a, 0xe6, 0xa4, 0xc8, 0x0c, 0xad, 0xcc, 0xbb, 0x7f, 0x0a
	};

#ifdef GIT_SHA1_COLLISIONDETECT
	GIT_UNUSED(&expected);
	cl_git_fail(sha1_file(actual, FIXTURE_DIR "/shattered-1.pdf"));
	cl_assert_equal_s("SHA1 collision attack detected", git_error_last()->message);
#else
	cl_git_pass(sha1_file(actual, FIXTURE_DIR "/shattered-1.pdf"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA1_SIZE));
#endif
}

void test_sha1__win32_providers(void)
{
#ifdef GIT_SHA1_WIN32
	unsigned char expected[GIT_HASH_SHA1_SIZE] = {
		0x38, 0x76, 0x2c, 0xf7, 0xf5, 0x59, 0x34, 0xb3, 0x4d, 0x17,
		0x9a, 0xe6, 0xa4, 0xc8, 0x0c, 0xad, 0xcc, 0xbb, 0x7f, 0x0a
	};
	unsigned char actual[GIT_HASH_SHA1_SIZE];

	git_hash_win32_set_provider(GIT_HASH_WIN32_CRYPTOAPI);
	cl_git_pass(sha1_file(actual, FIXTURE_DIR "/shattered-1.pdf"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA1_SIZE));

	git_hash_win32_set_provider(GIT_HASH_WIN32_CNG);
	cl_git_pass(sha1_file(actual, FIXTURE_DIR "/shattered-1.pdf"));
	cl_assert_equal_i(0, memcmp(expected, actual, GIT_HASH_SHA1_SIZE));
#endif
}
