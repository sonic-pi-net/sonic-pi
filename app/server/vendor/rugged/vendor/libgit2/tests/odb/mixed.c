#include "clar_libgit2.h"
#include "odb.h"

static git_odb *_odb;

void test_odb_mixed__initialize(void)
{
	cl_git_pass(git_odb_open(&_odb, cl_fixture("duplicate.git/objects")));
}

void test_odb_mixed__cleanup(void)
{
	git_odb_free(_odb);
	_odb = NULL;
}

void test_odb_mixed__dup_oid(void) {
	const char hex[] = "ce013625030ba8dba906f756967f9e9ca394464a";
	const char short_hex[] = "ce01362";
	git_oid oid;
	git_odb_object *obj;

	cl_git_pass(git_oid_fromstr(&oid, hex));
	cl_git_pass(git_odb_read_prefix(&obj, _odb, &oid, GIT_OID_HEXSZ));
	git_odb_object_free(obj);

	cl_git_pass(git_odb_exists_prefix(NULL, _odb, &oid, GIT_OID_HEXSZ));

	cl_git_pass(git_oid_fromstrn(&oid, short_hex, sizeof(short_hex) - 1));
	cl_git_pass(git_odb_read_prefix(&obj, _odb, &oid, sizeof(short_hex) - 1));
	git_odb_object_free(obj);

	cl_git_pass(git_odb_exists_prefix(NULL, _odb, &oid, sizeof(short_hex) - 1));
}

/* some known sha collisions of file content:
 *   'aabqhq' and 'aaazvc' with prefix 'dea509d0' (+ '9' and + 'b')
 *   'aaeufo' and 'aaaohs' with prefix '81b5bff5' (+ 'f' and + 'b')
 *   'aafewy' and 'aaepta' with prefix '739e3c4c'
 *   'aahsyn' and 'aadrjg' with prefix '0ddeaded' (+ '9' and + 'e')
 */

void test_odb_mixed__dup_oid_prefix_0(void) {
	char hex[10];
	git_oid oid, found;
	git_odb_object *obj;

	/* ambiguous in the same pack file */

	strncpy(hex, "dea509d0", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_assert_equal_i(
		GIT_EAMBIGUOUS, git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	cl_assert_equal_i(
		GIT_EAMBIGUOUS, git_odb_exists_prefix(&found, _odb, &oid, strlen(hex)));

	strncpy(hex, "dea509d09", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_git_pass(git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	cl_git_pass(git_odb_exists_prefix(&found, _odb, &oid, strlen(hex)));
	cl_assert_equal_oid(&found, git_odb_object_id(obj));
	git_odb_object_free(obj);

	strncpy(hex, "dea509d0b", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_git_pass(git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	git_odb_object_free(obj);

	/* ambiguous in different pack files */

	strncpy(hex, "81b5bff5", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_assert_equal_i(
		GIT_EAMBIGUOUS, git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	cl_assert_equal_i(
		GIT_EAMBIGUOUS, git_odb_exists_prefix(&found, _odb, &oid, strlen(hex)));

	strncpy(hex, "81b5bff5b", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_git_pass(git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	cl_git_pass(git_odb_exists_prefix(&found, _odb, &oid, strlen(hex)));
	cl_assert_equal_oid(&found, git_odb_object_id(obj));
	git_odb_object_free(obj);

	strncpy(hex, "81b5bff5f", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_git_pass(git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	git_odb_object_free(obj);

	/* ambiguous in pack file and loose */

	strncpy(hex, "0ddeaded", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_assert_equal_i(
		GIT_EAMBIGUOUS, git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	cl_assert_equal_i(
		GIT_EAMBIGUOUS, git_odb_exists_prefix(&found, _odb, &oid, strlen(hex)));

	strncpy(hex, "0ddeaded9", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_git_pass(git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	cl_git_pass(git_odb_exists_prefix(&found, _odb, &oid, strlen(hex)));
	cl_assert_equal_oid(&found, git_odb_object_id(obj));
	git_odb_object_free(obj);

	strncpy(hex, "0ddeadede", sizeof(hex));
	cl_git_pass(git_oid_fromstrn(&oid, hex, strlen(hex)));
	cl_git_pass(git_odb_read_prefix(&obj, _odb, &oid, strlen(hex)));
	git_odb_object_free(obj);
}
