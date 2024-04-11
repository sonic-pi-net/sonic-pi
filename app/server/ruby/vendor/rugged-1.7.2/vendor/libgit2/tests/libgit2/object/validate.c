#include "clar_libgit2.h"

#define VALID_COMMIT_SHA1 \
	"tree bdd24e358576f1baa275df98cdcaf3ac9a3f4233\n" \
	"parent d6d956f1d66210bfcd0484166befab33b5987a39\n" \
	"author Edward Thomson <ethomson@edwardthomson.com> 1638286404 -0500\n" \
	"committer Edward Thomson <ethomson@edwardthomson.com> 1638324642 -0500\n" \
	"\n" \
	"commit go here.\n"

#define VALID_TREE_SHA1 \
	"100644 HEADER\0\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42"

#define INVALID_COMMIT_SHA1 \
	"tree bdd24e358576f1baa275df98cdcaf3ac9a3f4233\n" \
	"parent d6d956f1d66210bfcd0484166befab33b5987a39\n" \
	"committer Edward Thomson <ethomson@edwardthomson.com> 1638324642 -0500\n" \
	"\n" \
	"commit go here.\n"

#define INVALID_TREE_SHA1 \
	"100644 HEADER \x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42"

#define VALID_COMMIT_SHA256 \
	"tree d0fc7f52dc42358506e7f3f3be72f5271994abb104b9397ab3e19bb42361504d\n" \
	"parent 652412419a24ba62a1d897f40aeb80eecbf873797b04a1bbb8d71918653ef65b\n" \
	"author Edward Thomson <ethomson@edwardthomson.com> 1638286404 -0500\n" \
	"committer Edward Thomson <ethomson@edwardthomson.com> 1638324642 -0500\n" \
	"\n" \
	"commit go here.\n"

#define VALID_TREE_SHA256 \
	"100644 HEADER\0\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42"

#define INVALID_COMMIT_SHA256 \
	"tree d0fc7f52dc42358506e7f3f3be72f5271994abb104b9397ab3e19bb42361504d\n" \
	"parent 652412419a24ba62a1d897f40aeb80eecbf873797b04a1bbb8d71918653ef65b\n" \
	"committer Edward Thomson <ethomson@edwardthomson.com> 1638324642 -0500\n" \
	"\n" \
	"commit go here.\n"

#define INVALID_TREE_SHA256 \
	"100644 HEADER \x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42"

#ifdef GIT_EXPERIMENTAL_SHA256
# define sha1_rawcontent_is_valid(v, c, l, t) \
         git_object_rawcontent_is_valid(v, c, l, t, GIT_OID_SHA1)
#else
# define sha1_rawcontent_is_valid(v, c, l, t) \
         git_object_rawcontent_is_valid(v, c, l, t)
#endif

void test_object_validate__valid_sha1(void)
{
	int valid;

	cl_git_pass(sha1_rawcontent_is_valid(&valid, "", 0, GIT_OBJECT_BLOB));
	cl_assert_equal_i(1, valid);

	cl_git_pass(sha1_rawcontent_is_valid(&valid, "foobar", 0, GIT_OBJECT_BLOB));
	cl_assert_equal_i(1, valid);

	cl_git_pass(sha1_rawcontent_is_valid(&valid, VALID_COMMIT_SHA1, CONST_STRLEN(VALID_COMMIT_SHA1), GIT_OBJECT_COMMIT));
	cl_assert_equal_i(1, valid);

	cl_git_pass(sha1_rawcontent_is_valid(&valid, VALID_TREE_SHA1, CONST_STRLEN(VALID_TREE_SHA1), GIT_OBJECT_TREE));
	cl_assert_equal_i(1, valid);
}

void test_object_validate__cannot_parse_sha256_as_sha1(void)
{
	int valid;

	cl_git_pass(sha1_rawcontent_is_valid(&valid, VALID_COMMIT_SHA256, CONST_STRLEN(INVALID_COMMIT_SHA256), GIT_OBJECT_COMMIT));
	cl_assert_equal_i(0, valid);

	cl_git_pass(sha1_rawcontent_is_valid(&valid, INVALID_TREE_SHA256, CONST_STRLEN(INVALID_TREE_SHA256), GIT_OBJECT_TREE));
	cl_assert_equal_i(0, valid);
}

void test_object_validate__invalid_sha1(void)
{
	int valid;

	cl_git_pass(sha1_rawcontent_is_valid(&valid, "", 0, GIT_OBJECT_COMMIT));
	cl_assert_equal_i(0, valid);

	cl_git_pass(sha1_rawcontent_is_valid(&valid, "foobar", 0, GIT_OBJECT_COMMIT));
	cl_assert_equal_i(0, valid);

	cl_git_pass(sha1_rawcontent_is_valid(&valid, INVALID_COMMIT_SHA1, CONST_STRLEN(INVALID_COMMIT_SHA1), GIT_OBJECT_COMMIT));
	cl_assert_equal_i(0, valid);

	cl_git_pass(sha1_rawcontent_is_valid(&valid, INVALID_TREE_SHA1, CONST_STRLEN(INVALID_TREE_SHA1), GIT_OBJECT_TREE));
	cl_assert_equal_i(0, valid);
}


void test_object_validate__valid_sha256(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	int valid;

	cl_git_pass(git_object_rawcontent_is_valid(&valid, "", 0, GIT_OBJECT_BLOB, GIT_OID_SHA256));
	cl_assert_equal_i(1, valid);

	cl_git_pass(git_object_rawcontent_is_valid(&valid, "foobar", 0, GIT_OBJECT_BLOB, GIT_OID_SHA256));
	cl_assert_equal_i(1, valid);

	cl_git_pass(git_object_rawcontent_is_valid(&valid, VALID_COMMIT_SHA256, CONST_STRLEN(VALID_COMMIT_SHA256), GIT_OBJECT_COMMIT, GIT_OID_SHA256));
	cl_assert_equal_i(1, valid);

	cl_git_pass(git_object_rawcontent_is_valid(&valid, VALID_TREE_SHA256, CONST_STRLEN(VALID_TREE_SHA256), GIT_OBJECT_TREE, GIT_OID_SHA256));
	cl_assert_equal_i(1, valid);
#endif
}

void test_object_validate__invalid_sha256(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	int valid;

	cl_git_pass(git_object_rawcontent_is_valid(&valid, "", 0, GIT_OBJECT_COMMIT, GIT_OID_SHA256));
	cl_assert_equal_i(0, valid);

	cl_git_pass(git_object_rawcontent_is_valid(&valid, "foobar", 0, GIT_OBJECT_COMMIT, GIT_OID_SHA256));
	cl_assert_equal_i(0, valid);

	cl_git_pass(git_object_rawcontent_is_valid(&valid, INVALID_COMMIT_SHA256, CONST_STRLEN(INVALID_COMMIT_SHA256), GIT_OBJECT_COMMIT, GIT_OID_SHA256));
	cl_assert_equal_i(0, valid);

	cl_git_pass(git_object_rawcontent_is_valid(&valid, INVALID_TREE_SHA256, CONST_STRLEN(INVALID_TREE_SHA256), GIT_OBJECT_TREE, GIT_OID_SHA256));
	cl_assert_equal_i(0, valid);
#endif
}

void test_object_validate__cannot_parse_sha1_as_sha256(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	int valid;

	cl_git_pass(git_object_rawcontent_is_valid(&valid, VALID_COMMIT_SHA1, CONST_STRLEN(INVALID_COMMIT_SHA1), GIT_OBJECT_COMMIT, GIT_OID_SHA256));
	cl_assert_equal_i(0, valid);

	cl_git_pass(git_object_rawcontent_is_valid(&valid, INVALID_TREE_SHA1, CONST_STRLEN(INVALID_TREE_SHA1), GIT_OBJECT_TREE, GIT_OID_SHA256));
	cl_assert_equal_i(0, valid);
#endif
}
