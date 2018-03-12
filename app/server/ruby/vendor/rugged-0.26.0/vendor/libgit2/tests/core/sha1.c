#include "clar_libgit2.h"
#include "hash.h"

#define FIXTURE_DIR "sha1"

void test_core_sha1__initialize(void)
{
	cl_fixture_sandbox(FIXTURE_DIR);
}

void test_core_sha1__cleanup(void)
{
	cl_fixture_cleanup(FIXTURE_DIR);
}

static int sha1_file(git_oid *oid, const char *filename)
{
	git_hash_ctx ctx;
	char buf[2048];
	int fd, ret;
	ssize_t read_len;

	fd = p_open(filename, O_RDONLY);
	cl_assert(fd >= 0);

	cl_git_pass(git_hash_ctx_init(&ctx));

	while ((read_len = p_read(fd, buf, 2048)) > 0)
		cl_git_pass(git_hash_update(&ctx, buf, (size_t)read_len));

	cl_assert_equal_i(0, read_len);
	p_close(fd);

	ret = git_hash_final(oid, &ctx);
	git_hash_ctx_cleanup(&ctx);

	return ret;
}

void test_core_sha1__sum(void)
{
	git_oid oid, expected;

	cl_git_pass(sha1_file(&oid, FIXTURE_DIR "/hello_c"));
	git_oid_fromstr(&expected, "4e72679e3ea4d04e0c642f029e61eb8056c7ed94");
	cl_assert_equal_oid(&expected, &oid);
}

/* test that sha1 collision detection works when enabled */
void test_core_sha1__detect_collision_attack(void)
{
	git_oid oid, expected;

#ifdef GIT_SHA1_COLLISIONDETECT
	GIT_UNUSED(expected);
	cl_git_fail(sha1_file(&oid, FIXTURE_DIR "/shattered-1.pdf"));
	cl_assert_equal_s("SHA1 collision attack detected", giterr_last()->message);
#else
	cl_git_pass(sha1_file(&oid, FIXTURE_DIR "/shattered-1.pdf"));
	git_oid_fromstr(&expected, "38762cf7f55934b34d179ae6a4c80cadccbb7f0a");
	cl_assert_equal_oid(&expected, &oid);
#endif
}

