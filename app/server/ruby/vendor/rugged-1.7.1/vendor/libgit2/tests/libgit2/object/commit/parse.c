#include "clar_libgit2.h"
#include "commit.h"
#include "object.h"
#include "signature.h"

static void assert_commit_parses(
	const char *data,
	size_t datalen,
	git_oid_t oid_type,
	const char *expected_treeid,
	const char *expected_author,
	const char *expected_committer,
	const char *expected_encoding,
	const char *expected_message,
	size_t expected_parents)
{
	git_commit *commit;
	if (!datalen)
		datalen = strlen(data);
	cl_git_pass(git_object__from_raw((git_object **) &commit, data, datalen, GIT_OBJECT_COMMIT, oid_type));

	if (expected_author) {
		git_signature *author;
		cl_git_pass(git_signature_from_buffer(&author, expected_author));
		cl_assert(git_signature__equal(author, commit->author));
		cl_assert_equal_s(author->name, commit->author->name);
		cl_assert_equal_s(author->email, commit->author->email);
		cl_assert_equal_i(author->when.time, commit->author->when.time);
		cl_assert_equal_i(author->when.offset, commit->author->when.offset);
		cl_assert_equal_i(author->when.sign, commit->author->when.sign);
		git_signature_free(author);
	}

	if (expected_committer) {
		git_signature *committer;
		cl_git_pass(git_signature_from_buffer(&committer, expected_committer));
		cl_assert_equal_s(committer->name, commit->committer->name);
		cl_assert_equal_s(committer->email, commit->committer->email);
		cl_assert_equal_i(committer->when.time, commit->committer->when.time);
		cl_assert_equal_i(committer->when.offset, commit->committer->when.offset);
		cl_assert_equal_i(committer->when.sign, commit->committer->when.sign);
		git_signature_free(committer);
	}

	if (expected_encoding)
		cl_assert_equal_s(commit->message_encoding, expected_encoding);
	else
		cl_assert_equal_p(commit->message_encoding, NULL);

	if (expected_message)
		cl_assert_equal_s(commit->raw_message, expected_message);
	else
		cl_assert_equal_p(commit->message_encoding, NULL);

	if (expected_treeid) {
		git_oid tree_oid;
		cl_git_pass(git_oid__fromstr(&tree_oid, expected_treeid, oid_type));
		cl_assert_equal_oid(&tree_oid, &commit->tree_id);
	}

	cl_assert_equal_i(commit->parent_ids.size, expected_parents);

	git_object__free(&commit->object);
}

static void assert_commit_fails(
	const char *data,
	size_t datalen,
	git_oid_t oid_type)
{
	git_object *object;
	if (!datalen)
		datalen = strlen(data);
	cl_git_fail(git_object__from_raw(&object, data, datalen, GIT_OBJECT_COMMIT, oid_type));
}

void test_object_commit_parse__sha1_parsing_commit_succeeds(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"encoding Encoding\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA1,
		"3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8",
		"Author <author@example.com>",
		"Committer <committer@example.com>",
		"Encoding",
		"Message", 0);
}

void test_object_commit_parse__sha1_parsing_commit_without_encoding_succeeds(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA1,
		"3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8",
		"Author <author@example.com>",
		"Committer <committer@example.com>",
		NULL,
		"Message", 0);
}

void test_object_commit_parse__sha1_parsing_commit_with_multiple_authors_succeeds(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author Author1 <author@example.com>\n"
		"author Author2 <author@example.com>\n"
		"author Author3 <author@example.com>\n"
		"author Author4 <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA1,
		"3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8",
		"Author1 <author@example.com>",
		"Committer <committer@example.com>",
		NULL,
		"Message", 0);
}

void test_object_commit_parse__sha1_parsing_commit_with_multiple_committers_succeeds(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author Author <author@example.com>\n"
		"committer Committer1 <committer@example.com>\n"
		"committer Committer2 <committer@example.com>\n"
		"committer Committer3 <committer@example.com>\n"
		"committer Committer4 <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA1,
		"3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8",
		"Author <author@example.com>",
		"Committer1 <committer@example.com>",
		NULL,
		"Message", 0);
}

void test_object_commit_parse__sha1_parsing_commit_without_message_succeeds(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n";
	assert_commit_parses(commit, 0, GIT_OID_SHA1,
		"3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8",
		"Author <author@example.com>",
		"Committer <committer@example.com>",
		NULL,
		"", 0);
}

void test_object_commit_parse__sha1_parsing_commit_with_unknown_fields_succeeds(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"foo bar\n"
		"more garbage\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA1,
		"3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8",
		"Author <author@example.com>",
		"Committer <committer@example.com>",
		NULL,
		"Message", 0);
}

void test_object_commit_parse__sha1_parsing_commit_with_invalid_tree_fails(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1xxx5f3445895b71d9cb0f8\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA1);
}

void test_object_commit_parse__sha1_parsing_commit_with_sha256_tree_fails(void)
{
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA1);
}

void test_object_commit_parse__sha1_parsing_commit_without_tree_fails(void)
{
	const char *commit =
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA1);
}

void test_object_commit_parse__sha1_parsing_commit_without_author_fails(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA1);
}

void test_object_commit_parse__sha1_parsing_commit_without_committer_fails(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author Author <author@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA1);
}

void test_object_commit_parse__sha1_parsing_encoding_will_not_cause_oob_read(void)
{
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author <>\n"
		"committer <>\n"
		"encoding foo\n";
	/*
	 * As we ignore unknown fields, the cut-off encoding field will be
	 * parsed just fine.
	 */
	assert_commit_parses(
		commit, strlen(commit) - strlen("ncoding foo\n"),
		GIT_OID_SHA1,
		"3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8",
		"<>",
		"<>",
		NULL,
		"", 0);
}


void test_object_commit_parse__sha256_parsing_commit_succeeds(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"encoding Encoding\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA256,
		"f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736",
		"Author <author@example.com>",
		"Committer <committer@example.com>",
		"Encoding",
		"Message", 0);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_without_encoding_succeeds(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA256,
		"f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736",
		"Author <author@example.com>",
		"Committer <committer@example.com>",
		NULL,
		"Message", 0);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_with_multiple_authors_succeeds(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author1 <author@example.com>\n"
		"author Author2 <author@example.com>\n"
		"author Author3 <author@example.com>\n"
		"author Author4 <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA256,
		"f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736",
		"Author1 <author@example.com>",
		"Committer <committer@example.com>",
		NULL,
		"Message", 0);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_with_multiple_committers_succeeds(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author <author@example.com>\n"
		"committer Committer1 <committer@example.com>\n"
		"committer Committer2 <committer@example.com>\n"
		"committer Committer3 <committer@example.com>\n"
		"committer Committer4 <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA256,
		"f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736",
		"Author <author@example.com>",
		"Committer1 <committer@example.com>",
		NULL,
		"Message", 0);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_without_message_succeeds(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n";
	assert_commit_parses(commit, 0, GIT_OID_SHA256,
		"f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736",
		"Author <author@example.com>",
		"Committer <committer@example.com>",
		NULL,
		"", 0);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_with_unknown_fields_succeeds(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"foo bar\n"
		"more garbage\n"
		"\n"
		"Message";
	assert_commit_parses(commit, 0, GIT_OID_SHA256,
		"f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736",
		"Author <author@example.com>",
		"Committer <committer@example.com>",
		NULL,
		"Message", 0);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_with_invalid_tree_fails(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9adxxxd55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA256);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_with_sha1_tree_fails(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree 3e7ac388cadacccdf1c6c5f3445895b71d9cb0f8\n"
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA256);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_without_tree_fails(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"author Author <author@example.com>\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA256);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_without_author_fails(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"committer Committer <committer@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA256);
#endif
}

void test_object_commit_parse__sha256_parsing_commit_without_committer_fails(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author Author <author@example.com>\n"
		"\n"
		"Message";
	assert_commit_fails(commit, 0, GIT_OID_SHA256);
#endif
}

void test_object_commit_parse__sha256_parsing_encoding_will_not_cause_oob_read(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	const char *commit =
		"tree f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736\n"
		"author <>\n"
		"committer <>\n"
		"encoding foo\n";
	/*
	 * As we ignore unknown fields, the cut-off encoding field will be
	 * parsed just fine.
	 */
	assert_commit_parses(
		commit, strlen(commit) - strlen("ncoding foo\n"),
		GIT_OID_SHA256,
		"f2a108f86a3b4fd9ad75ed55e9cb3cb46e348fca3b9dba3db64f7c9f64b8a736",
		"<>",
		"<>",
		NULL,
		"", 0);
#endif
}
