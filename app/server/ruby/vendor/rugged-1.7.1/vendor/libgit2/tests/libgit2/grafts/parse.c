#include "clar_libgit2.h"

#include "grafts.h"

#define OID0 "c0368f9f9743e950e6cfe1f45a649f8a9dfcd97e"
#define OID1 "cfc50a0db87ce908fb8a8c5b8f7b4ab96eee8643"
#define OID2 "6914d97cd08b9edf5e855fca211c750fa82fd80a"
#define OID3 "516521937d0e9ce9d0d836149a0702671f326b4a"
#define OID4 "e2c29d67ef2f217650196f94c796f0532b8caad6"
#define OID5 "79bcb936596cb50353fe7be28b7444e66e4a2842"
#define OID6 "b9c54107d57c17dbcaf646c4d52f66eb9e69d23d"
#define OID7 "9f8a746e9ad7b58cc840016bc3944d5ad262acb5"
#define OID8 "392f4beef7d0d15b2bc5b1abe1a754eba0ec36da"

#define OID_TRUNCATED "392f4beef7d0d15b2bc5b1abe1a754eba0ec36d"
#define OID_NONHEX    "9f8a746e9ax7b58cc840016bc3944d5ad262acb5"

static git_grafts *grafts;

void test_grafts_parse__initialize(void)
{
	cl_git_pass(git_grafts_new(&grafts, GIT_OID_SHA1));
}

void test_grafts_parse__cleanup(void)
{
	git_grafts_free(grafts);
	grafts = NULL;
}

static void assert_parse_succeeds(git_grafts *grafts, const char *string, size_t n)
{
	cl_git_pass(git_grafts_parse(grafts, string, strlen(string)));
	cl_assert_equal_i(git_grafts_size(grafts), n);
}

static void assert_parse_fails(git_grafts *grafts, const char *string)
{
	cl_git_fail(git_grafts_parse(grafts, string, strlen(string)));
}

static void assert_graft_contains(git_grafts *grafts, const char *graft, size_t n, ...)
{
	git_commit_graft *commit;
	git_oid oid;
	va_list ap;
	size_t i = 0;

	cl_git_pass(git_oid__fromstr(&oid, graft, GIT_OID_SHA1));
	cl_git_pass(git_grafts_get(&commit, grafts, &oid));
	cl_assert_equal_oid(&commit->oid, &oid);
	cl_assert_equal_i(commit->parents.size, n);

	va_start(ap, n);
	while (i < n) {
		cl_git_pass(git_oid__fromstr(&oid, va_arg(ap, const char *), GIT_OID_SHA1));
		cl_assert_equal_oid(&commit->parents.ptr[i], &oid);
		i++;
	}
	va_end(ap);
}

void test_grafts_parse__single_oid(void)
{
	assert_parse_succeeds(grafts, OID1, 1);
	assert_graft_contains(grafts, OID1, 0);
}

void test_grafts_parse__single_oid_with_newline(void)
{
	assert_parse_succeeds(grafts, OID1 "\n", 1);
	assert_graft_contains(grafts, OID1, 0);
}

void test_grafts_parse__multiple_oids(void)
{
	assert_parse_succeeds(grafts, OID1 "\n" OID2 "\n" OID3, 3);
	assert_graft_contains(grafts, OID1, 0);
	assert_graft_contains(grafts, OID2, 0);
	assert_graft_contains(grafts, OID3, 0);
}

void test_grafts_parse__same_oid(void)
{
	assert_parse_succeeds(grafts, OID1 "\n" OID1, 1);
	assert_graft_contains(grafts, OID1, 0);
}

void test_grafts_parse__oid_with_parent(void)
{
	assert_parse_succeeds(grafts, OID1 " " OID2, 1);
	assert_graft_contains(grafts, OID1, 1, OID2);
}

void test_grafts_parse__oid_with_parent_and_newline(void)
{
	assert_parse_succeeds(grafts, OID1 " " OID2 "\n", 1);
	assert_graft_contains(grafts, OID1, 1, OID2);
}

void test_grafts_parse__oid_with_multiple_parents(void)
{
	assert_parse_succeeds(grafts, OID1 " " OID2 " " OID3 " " OID4 " " OID5, 1);
	assert_graft_contains(grafts, OID1, 4, OID2, OID3, OID4, OID5);
}

void test_grafts_parse__multiple_oids_with_multiple_parents(void)
{
	assert_parse_succeeds(grafts,
		OID1 " " OID2 " " OID3 " " OID4 " " OID5 "\n"
		OID6 " " OID7 " " OID8 "\n" , 2);
	assert_graft_contains(grafts, OID1, 4, OID2, OID3, OID4, OID5);
	assert_graft_contains(grafts, OID6, 2, OID7, OID8);
}

void test_grafts_parse__multiple_spaces_fails(void)
{
	assert_parse_fails(grafts, OID1 "  " OID2);
}

void test_grafts_parse__trailing_space_fails(void)
{
	assert_parse_fails(grafts, OID1 " " OID2 " ");
}

void test_grafts_parse__invalid_character_inbetween_fails(void)
{
	assert_parse_fails(grafts, OID1 " x " OID2);
}

void test_grafts_parse__truncated_oid_fails(void)
{
	assert_parse_fails(grafts, OID_TRUNCATED);
}

void test_grafts_parse__truncated_parent_fails(void)
{
	assert_parse_fails(grafts, OID1 " " OID_TRUNCATED);
}

void test_grafts_parse__invalid_oid_fails(void)
{
	assert_parse_fails(grafts, OID_NONHEX);
}

void test_grafts_parse__invalid_parent_fails(void)
{
	assert_parse_fails(grafts, OID1 " " OID_NONHEX);
}
