#include "clar_libgit2.h"
#include "object.h"
#include "signature.h"
#include "tag.h"

static void assert_tag_parses(const char *data, size_t datalen,
	const char *expected_oid,
	const char *expected_name,
	const char *expected_tagger,
	const char *expected_message)
{
	git_tag *tag;

	if (!datalen)
		datalen = strlen(data);

	cl_git_pass(git_object__from_raw((git_object **) &tag, data, datalen, GIT_OBJECT_TAG));
	cl_assert_equal_i(tag->type, GIT_OBJECT_TAG);

	if (expected_oid) {
		git_oid oid;
		cl_git_pass(git_oid_fromstr(&oid, expected_oid));
		cl_assert_equal_oid(&oid, &tag->target);
	}

	if (expected_name)
		cl_assert_equal_s(expected_name, tag->tag_name);
	else
		cl_assert_equal_s(tag->message, NULL);

	if (expected_tagger) {
		git_signature *tagger;
		cl_git_pass(git_signature_from_buffer(&tagger, expected_tagger));
		cl_assert_equal_s(tagger->name, tag->tagger->name);
		cl_assert_equal_s(tagger->email, tag->tagger->email);
		cl_assert_equal_i(tagger->when.time, tag->tagger->when.time);
		cl_assert_equal_i(tagger->when.offset, tag->tagger->when.offset);
		cl_assert_equal_i(tagger->when.sign, tag->tagger->when.sign);
		git_signature_free(tagger);
	} else {
		cl_assert_equal_s(tag->tagger, NULL);
	}

	if (expected_message)
		cl_assert_equal_s(expected_message, tag->message);
	else
		cl_assert_equal_s(tag->message, NULL);

	git_object__free(&tag->object);
}

static void assert_tag_fails(const char *data, size_t datalen)
{
	git_object *object;
	if (!datalen)
		datalen = strlen(data);
	cl_git_fail(git_object__from_raw(&object, data, datalen, GIT_OBJECT_TAG));
}

void test_object_tag_parse__valid_tag_parses(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type tag\n"
		"tag tagname\n"
		"tagger Taggy Mr. Taggart <taggy@taggart.com>\n"
		"\n"
		"Message";
	assert_tag_parses(tag, 0,
		"a8d447f68076d1520f69649bb52629941be7031f",
		"tagname",
		"Taggy Mr. Taggart <taggy@taggart.com>",
		"Message");
}

void test_object_tag_parse__missing_tagger_parses(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type tag\n"
		"tag tagname\n"
		"\n"
		"Message";
	assert_tag_parses(tag, 0,
		"a8d447f68076d1520f69649bb52629941be7031f",
		"tagname",
		NULL,
		"Message");
}

void test_object_tag_parse__missing_message_parses(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type tag\n"
		"tag tagname\n"
		"tagger Taggy Mr. Taggart <taggy@taggart.com>\n";
	assert_tag_parses(tag, 0,
		"a8d447f68076d1520f69649bb52629941be7031f",
		"tagname",
		"Taggy Mr. Taggart <taggy@taggart.com>",
		NULL);
}

void test_object_tag_parse__unknown_field_parses(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type tag\n"
		"tag tagname\n"
		"tagger Taggy Mr. Taggart <taggy@taggart.com>\n"
		"foo bar\n"
		"frubble frabble\n"
		"\n"
		"Message";
	assert_tag_parses(tag, 0,
		"a8d447f68076d1520f69649bb52629941be7031f",
		"tagname",
		"Taggy Mr. Taggart <taggy@taggart.com>",
		"Message");
}

void test_object_tag_parse__missing_object_fails(void)
{
	const char *tag =
		"type tag\n"
		"tag tagname\n"
		"tagger Taggy Mr. Taggart <taggy@taggart.com>\n"
		"\n"
		"Message";
	assert_tag_fails(tag, 0);
}

void test_object_tag_parse__malformatted_object_fails(void)
{
	const char *tag =
		"object a8d447f68076d15xxxxxxxxxxxxxxxx41be7031f\n"
		"type tag\n"
		"tag tagname\n"
		"tagger Taggy Mr. Taggart <taggy@taggart.com>\n"
		"\n"
		"Message";
	assert_tag_fails(tag, 0);
}

void test_object_tag_parse__missing_type_fails(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"tag tagname\n"
		"tagger Taggy Mr. Taggart <taggy@taggart.com>\n"
		"\n"
		"Message";
	assert_tag_fails(tag, 0);
}

void test_object_tag_parse__invalid_type_fails(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type garbage\n"
		"tag tagname\n"
		"tagger Taggy Mr. Taggart <taggy@taggart.com>\n"
		"\n"
		"Message";
	assert_tag_fails(tag, 0);
}

void test_object_tag_parse__missing_tagname_fails(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type tag\n"
		"tagger Taggy Mr. Taggart <taggy@taggart.com>\n"
		"\n"
		"Message";
	assert_tag_fails(tag, 0);
}

void test_object_tag_parse__misformatted_tagger_fails(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type tag\n"
		"tag Tag\n"
		"tagger taggy@taggart.com>\n"
		"\n"
		"Message";
	assert_tag_fails(tag, 0);
}

void test_object_tag_parse__missing_message_fails(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type tag\n"
		"tag Tag\n"
		"tagger taggy@taggart.com>\n";
	assert_tag_fails(tag, 0);
}

void test_object_tag_parse__no_oob_read_when_searching_message(void)
{
	const char *tag =
		"object a8d447f68076d1520f69649bb52629941be7031f\n"
		"type tag\n"
		"tag \n"
		"tagger <>\n"
		" \n\n"
		"Message";
	/*
	 * The OOB read previously resulted in an OOM error. We
	 * thus want to make sure that the resulting error is the
	 * expected one.
	 */
	assert_tag_fails(tag, strlen(tag) - strlen("\n\nMessage"));
	cl_assert(strstr(git_error_last()->message, "tag contains no message"));
}
