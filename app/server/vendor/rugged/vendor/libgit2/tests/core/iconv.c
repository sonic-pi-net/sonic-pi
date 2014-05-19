#include "clar_libgit2.h"
#include "path.h"

#ifdef GIT_USE_ICONV
static git_path_iconv_t ic;
static char *nfc = "\xC3\x85\x73\x74\x72\xC3\xB6\x6D";
static char *nfd = "\x41\xCC\x8A\x73\x74\x72\x6F\xCC\x88\x6D";
#endif

void test_core_iconv__initialize(void)
{
#ifdef GIT_USE_ICONV
	cl_git_pass(git_path_iconv_init_precompose(&ic));
#endif
}

void test_core_iconv__cleanup(void)
{
#ifdef GIT_USE_ICONV
	git_path_iconv_clear(&ic);
#endif
}

void test_core_iconv__unchanged(void)
{
#ifdef GIT_USE_ICONV
	char *data = "Ascii data", *original = data;
	size_t datalen = strlen(data);

	cl_git_pass(git_path_iconv(&ic, &data, &datalen));
	GIT_UNUSED(datalen);

	/* There are no high bits set, so this should leave data untouched */
	cl_assert(data == original);
#endif
}

void test_core_iconv__decomposed_to_precomposed(void)
{
#ifdef GIT_USE_ICONV
	char *data = nfd;
	size_t datalen, nfdlen = strlen(nfd);

	datalen = nfdlen;
	cl_git_pass(git_path_iconv(&ic, &data, &datalen));
	GIT_UNUSED(datalen);

	/* The decomposed nfd string should be transformed to the nfc form
	 * (on platforms where iconv is enabled, of course).
	 */
	cl_assert_equal_s(nfc, data);

	/* should be able to do it multiple times with the same git_path_iconv_t */
	data = nfd; datalen = nfdlen;
	cl_git_pass(git_path_iconv(&ic, &data, &datalen));
	cl_assert_equal_s(nfc, data);

	data = nfd; datalen = nfdlen;
	cl_git_pass(git_path_iconv(&ic, &data, &datalen));
	cl_assert_equal_s(nfc, data);
#endif
}

void test_core_iconv__precomposed_is_unmodified(void)
{
#ifdef GIT_USE_ICONV
	char *data = nfc;
	size_t datalen = strlen(nfc);

	cl_git_pass(git_path_iconv(&ic, &data, &datalen));
	GIT_UNUSED(datalen);

	/* data is already in precomposed form, so even though some bytes have
	 * the high-bit set, the iconv transform should result in no change.
	 */
	cl_assert_equal_s(nfc, data);
#endif
}
