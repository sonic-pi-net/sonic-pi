#include "clar_libgit2.h"
#include "posix.h"
#include "blob.h"
#include "filter.h"
#include "git2/sys/filter.h"
#include "git2/sys/repository.h"
#include "custom_helpers.h"

/* going TO_WORKDIR, filters are executed low to high
 * going TO_ODB, filters are executed high to low
 */
#define BITFLIP_FILTER_PRIORITY -1
#define REVERSE_FILTER_PRIORITY -2

#ifdef GIT_WIN32
# define NEWLINE "\r\n"
#else
# define NEWLINE "\n"
#endif

static char workdir_data[] =
	"some simple" NEWLINE
	"data" NEWLINE
	"that will be" NEWLINE
	"trivially" NEWLINE
	"scrambled." NEWLINE;

#define REVERSED_DATA_LEN 51

/* Represents the data above scrambled (bits flipped) after \r\n -> \n
 * conversion, then bytewise reversed
 */
static unsigned char bitflipped_and_reversed_data[] =
	{ 0xf5, 0xd1, 0x9b, 0x9a, 0x93, 0x9d, 0x92, 0x9e, 0x8d, 0x9c, 0x8c,
	  0xf5, 0x86, 0x93, 0x93, 0x9e, 0x96, 0x89, 0x96, 0x8d, 0x8b, 0xf5,
	  0x9a, 0x9d, 0xdf, 0x93, 0x93, 0x96, 0x88, 0xdf, 0x8b, 0x9e, 0x97,
	  0x8b, 0xf5, 0x9e, 0x8b, 0x9e, 0x9b, 0xf5, 0x9a, 0x93, 0x8f, 0x92,
	  0x96, 0x8c, 0xdf, 0x9a, 0x92, 0x90, 0x8c };

#define BITFLIPPED_AND_REVERSED_DATA_LEN 51

static git_repository *g_repo = NULL;

static void register_custom_filters(void);

void test_filter_custom__initialize(void)
{
	register_custom_filters();

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_mkfile(
		"empty_standard_repo/.gitattributes",
		"hero* bitflip reverse\n"
		"herofile text\n"
		"heroflip -reverse binary\n"
		"villain erroneous\n"
		"*.bin binary\n");
}

void test_filter_custom__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

static void register_custom_filters(void)
{
	static int filters_registered = 0;

	if (!filters_registered) {
		cl_git_pass(git_filter_register(
			"bitflip", create_bitflip_filter(), BITFLIP_FILTER_PRIORITY));

		cl_git_pass(git_filter_register(
			"reverse", create_reverse_filter("+reverse"),
			REVERSE_FILTER_PRIORITY));

		/* re-register reverse filter with standard filter=xyz priority */
		cl_git_pass(git_filter_register(
			"pre-reverse",
			create_reverse_filter("+prereverse"),
			GIT_FILTER_DRIVER_PRIORITY));

		cl_git_pass(git_filter_register(
			"erroneous",
			create_erroneous_filter("+erroneous"),
			GIT_FILTER_DRIVER_PRIORITY));

		filters_registered = 1;
	}
}

void test_filter_custom__to_odb(void)
{
	git_filter_list *fl;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "herofile", GIT_FILTER_TO_ODB, 0));

	in = workdir_data;
	in_len = strlen(workdir_data);

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));

	cl_assert_equal_i(BITFLIPPED_AND_REVERSED_DATA_LEN, out.size);

	cl_assert_equal_i(
		0, memcmp(bitflipped_and_reversed_data, out.ptr, out.size));

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_custom__to_workdir(void)
{
	git_filter_list *fl;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "herofile", GIT_FILTER_TO_WORKTREE, 0));

	in = (char *)bitflipped_and_reversed_data;
	in_len = BITFLIPPED_AND_REVERSED_DATA_LEN;

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, in, in_len));

	cl_assert_equal_i(strlen(workdir_data), out.size);

	cl_assert_equal_i(
		0, memcmp(workdir_data, out.ptr, out.size));

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_custom__can_register_a_custom_filter_in_the_repository(void)
{
	git_filter_list *fl;

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "herofile", GIT_FILTER_TO_WORKTREE, 0));
	/* expect: bitflip, reverse, crlf */
	cl_assert_equal_sz(3, git_filter_list_length(fl));
	git_filter_list_free(fl);

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "herocorp", GIT_FILTER_TO_WORKTREE, 0));
	/* expect: bitflip, reverse - possibly crlf depending on global config */
	{
		size_t flen = git_filter_list_length(fl);
		cl_assert(flen == 2 || flen == 3);
	}
	git_filter_list_free(fl);

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "hero.bin", GIT_FILTER_TO_WORKTREE, 0));
	/* expect: bitflip, reverse */
	cl_assert_equal_sz(2, git_filter_list_length(fl));
	git_filter_list_free(fl);

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "heroflip", GIT_FILTER_TO_WORKTREE, 0));
	/* expect: bitflip (because of -reverse) */
	cl_assert_equal_sz(1, git_filter_list_length(fl));
	git_filter_list_free(fl);

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "doesntapplytome.bin",
		GIT_FILTER_TO_WORKTREE, 0));
	/* expect: none */
	cl_assert_equal_sz(0, git_filter_list_length(fl));
	git_filter_list_free(fl);
}

void test_filter_custom__order_dependency(void)
{
	git_index *index;
	git_blob *blob;
	git_buf buf = { 0 };

	/* so if ident and reverse are used together, an interesting thing
	 * happens - a reversed "$Id$" string is no longer going to trigger
	 * ident correctly.  When checking out, the filters should be applied
	 * in order CLRF, then ident, then reverse, so ident expansion should
	 * work correctly.  On check in, the content should be reversed, then
	 * ident, then CRLF filtered.  Let's make sure that works...
	 */

	cl_git_mkfile(
		"empty_standard_repo/.gitattributes",
		"hero.*.rev-ident text ident prereverse eol=lf\n");

	cl_git_mkfile(
		"empty_standard_repo/hero.1.rev-ident",
		"This is a test\n$Id$\nHave fun!\n");

	cl_git_mkfile(
		"empty_standard_repo/hero.2.rev-ident",
		"Another test\n$dI$\nCrazy!\n");

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "hero.1.rev-ident"));
	cl_git_pass(git_index_add_bypath(index, "hero.2.rev-ident"));
	cl_repo_commit_from_index(NULL, g_repo, NULL, 0, "Filter chains\n");
	git_index_free(index);

	cl_git_pass(git_blob_lookup(&blob, g_repo,
		& git_index_get_bypath(index, "hero.1.rev-ident", 0)->id));
	cl_assert_equal_s(
		"\n!nuf evaH\n$dI$\ntset a si sihT", git_blob_rawcontent(blob));
	cl_git_pass(git_blob_filter(&buf, blob, "hero.1.rev-ident", NULL));
	/* no expansion because id was reversed at checkin and now at ident
	 * time, reverse is not applied yet */
	cl_assert_equal_s(
		"This is a test\n$Id$\nHave fun!\n", buf.ptr);
	git_blob_free(blob);

	cl_git_pass(git_blob_lookup(&blob, g_repo,
		& git_index_get_bypath(index, "hero.2.rev-ident", 0)->id));
	cl_assert_equal_s(
		"\n!yzarC\n$Id$\ntset rehtonA", git_blob_rawcontent(blob));
	cl_git_pass(git_blob_filter(&buf, blob, "hero.2.rev-ident", NULL));
	/* expansion because reverse was applied at checkin and at ident time,
	 * reverse is not applied yet */
	cl_assert_equal_s(
		"Another test\n$ 59001fe193103b1016b27027c0c827d036fd0ac8 :dI$\nCrazy!\n", buf.ptr);
	cl_assert_equal_i(0, git_oid_strcmp(
		git_blob_id(blob), "8ca0df630d728c0c72072b6101b301391ef10095"));
	git_blob_free(blob);

	git_buf_dispose(&buf);
}

void test_filter_custom__filter_registry_failure_cases(void)
{
	git_filter fake = { GIT_FILTER_VERSION, 0 };

	cl_assert_equal_i(GIT_EEXISTS, git_filter_register("bitflip", &fake, 0));

	cl_git_fail(git_filter_unregister(GIT_FILTER_CRLF));
	cl_git_fail(git_filter_unregister(GIT_FILTER_IDENT));
	cl_assert_equal_i(GIT_ENOTFOUND, git_filter_unregister("not-a-filter"));
}

void test_filter_custom__erroneous_filter_fails(void)
{
	git_filter_list *filters;
	git_buf out = GIT_BUF_INIT;
	const char *in;
	size_t in_len;

	cl_git_pass(git_filter_list_load(
		&filters, g_repo, NULL, "villain", GIT_FILTER_TO_WORKTREE, 0));

	in = workdir_data;
	in_len = strlen(workdir_data);

	cl_git_fail(git_filter_list_apply_to_buffer(&out, filters, in, in_len));

	git_filter_list_free(filters);
	git_buf_dispose(&out);
}
