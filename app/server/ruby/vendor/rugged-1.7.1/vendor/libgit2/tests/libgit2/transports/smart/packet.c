#include "clar_libgit2.h"
#include "transports/smart.h"

enum expected_status {
	PARSE_SUCCESS,
	PARSE_FAILURE
};

static void assert_flush_parses(const char *line)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt *pkt;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_FLUSH);
	cl_assert_equal_strn(endptr, line + 4, linelen - 4);

	git_pkt_free((git_pkt *) pkt);
}

static void assert_data_pkt_parses(const char *line, const char *expected_data, size_t expected_len)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt_data *pkt;
	git_pkt_parse_data pkt_parse_data = { 1, GIT_OID_SHA1 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_DATA);
	cl_assert_equal_i(pkt->len, expected_len);
	cl_assert_equal_strn(pkt->data, expected_data, expected_len);

	git_pkt_free((git_pkt *) pkt);
}

static void assert_sideband_progress_parses(const char *line, const char *expected_data, size_t expected_len)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt_progress *pkt;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_PROGRESS);
	cl_assert_equal_i(pkt->len, expected_len);
	cl_assert_equal_strn(pkt->data, expected_data, expected_len);

	git_pkt_free((git_pkt *) pkt);
}

static void assert_error_parses(const char *line, const char *expected_error, size_t expected_len)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt_err *pkt;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_ERR);
	cl_assert_equal_i(pkt->len, expected_len);
	cl_assert_equal_strn(pkt->error, expected_error, expected_len);

	git_pkt_free((git_pkt *) pkt);
}

static void assert_ack_parses(const char *line, const char *expected_oid, enum git_ack_status expected_status)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt_ack *pkt;
	git_oid oid;
	git_pkt_parse_data pkt_parse_data = { 1, GIT_OID_SHA1 };

	cl_git_pass(git_oid__fromstr(&oid, expected_oid, GIT_OID_SHA1));

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_ACK);
	cl_assert_equal_oid(&pkt->oid, &oid);
	cl_assert_equal_i(pkt->status, expected_status);

	git_pkt_free((git_pkt *) pkt);
}

static void assert_nak_parses(const char *line)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt *pkt;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_NAK);
	cl_assert_equal_strn(endptr, line + 7, linelen - 7);

	git_pkt_free((git_pkt *) pkt);
}

static void assert_comment_parses(const char *line, const char *expected_comment)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt_comment *pkt;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_COMMENT);
	cl_assert_equal_strn(pkt->comment, expected_comment, strlen(expected_comment));

	git_pkt_free((git_pkt *) pkt);
}

static void assert_ok_parses(const char *line, const char *expected_ref)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt_ok *pkt;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_OK);
	cl_assert_equal_strn(pkt->ref, expected_ref, strlen(expected_ref));

	git_pkt_free((git_pkt *) pkt);
}

static void assert_unpack_parses(const char *line, bool ok)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt_unpack *pkt;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_UNPACK);
	cl_assert_equal_i(pkt->unpack_ok, ok);

	git_pkt_free((git_pkt *) pkt);
}

static void assert_ng_parses(const char *line, const char *expected_ref, const char *expected_msg)
{
	size_t linelen = strlen(line) + 1;
	const char *endptr;
	git_pkt_ng *pkt;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_NG);
	cl_assert_equal_strn(pkt->ref, expected_ref, strlen(expected_ref));
	cl_assert_equal_strn(pkt->msg, expected_msg, strlen(expected_msg));

	git_pkt_free((git_pkt *) pkt);
}

#define assert_ref_parses(line, expected_oid, expected_ref, expected_capabilities) \
	assert_ref_parses_(line, sizeof(line), expected_oid, expected_ref, expected_capabilities)

static void assert_ref_parses_(const char *line, size_t linelen, const char *expected_oid,
	const char *expected_ref, const char *expected_capabilities)
{
	const char *endptr;
	git_pkt_ref *pkt;
	git_oid oid;
	git_pkt_parse_data pkt_parse_data = { 0 };

	cl_git_pass(git_oid__fromstr(&oid, expected_oid, GIT_OID_SHA1));

	cl_git_pass(git_pkt_parse_line((git_pkt **) &pkt, &endptr, line, linelen, &pkt_parse_data));
	cl_assert_equal_i(pkt->type, GIT_PKT_REF);
	cl_assert_equal_oid(&pkt->head.oid, &oid);
	cl_assert_equal_strn(pkt->head.name, expected_ref, strlen(expected_ref));
	if (expected_capabilities)
		cl_assert_equal_strn(pkt->capabilities, expected_capabilities, strlen(expected_capabilities));
	else
		cl_assert_equal_p(NULL, pkt->capabilities);

	git_pkt_free((git_pkt *) pkt);
}

static void assert_pkt_fails(const char *line)
{
	const char *endptr;
	git_pkt_parse_data pkt_parse_data = { 0 };

	git_pkt *pkt;
	cl_git_fail(git_pkt_parse_line(&pkt, &endptr, line, strlen(line) + 1, &pkt_parse_data));
}

void test_transports_smart_packet__parsing_garbage_fails(void)
{
	assert_pkt_fails("0foobar");
	assert_pkt_fails("00foobar");
	assert_pkt_fails("000foobar");
	assert_pkt_fails("0001");
	assert_pkt_fails("");
	assert_pkt_fails("0");
	assert_pkt_fails("0i00");
	assert_pkt_fails("f");
}

void test_transports_smart_packet__flush_parses(void)
{
	assert_flush_parses("0000");
	assert_flush_parses("0000foobar");
}

void test_transports_smart_packet__data_pkt(void)
{
	assert_pkt_fails("000foobar");
	assert_pkt_fails("0001o");
	assert_pkt_fails("0001\1");
	assert_data_pkt_parses("0005\1", "", 0);
	assert_pkt_fails("0009\1o");
	assert_data_pkt_parses("0009\1data", "data", 4);
	assert_data_pkt_parses("000a\1data", "data", 5);
}

void test_transports_smart_packet__sideband_progress_pkt(void)
{
	assert_pkt_fails("0001\2");
	assert_sideband_progress_parses("0005\2", "", 0);
	assert_pkt_fails("0009\2o");
	assert_sideband_progress_parses("0009\2data", "data", 4);
	assert_sideband_progress_parses("000a\2data", "data", 5);
}

void test_transports_smart_packet__sideband_err_pkt(void)
{
	assert_pkt_fails("0001\3");
	assert_error_parses("0005\3", "", 0);
	assert_pkt_fails("0009\3o");
	assert_error_parses("0009\3data", "data", 4);
	assert_error_parses("000a\3data", "data", 5);
}

void test_transports_smart_packet__ack_pkt(void)
{
	assert_ack_parses("0030ACK 0000000000000000000000000000000000000000",
			  "0000000000000000000000000000000000000000", 0);
	assert_ack_parses("0039ACK 0000000000000000000000000000000000000000 continue",
			  "0000000000000000000000000000000000000000",
			  GIT_ACK_CONTINUE);
	assert_ack_parses("0037ACK 0000000000000000000000000000000000000000 common",
			  "0000000000000000000000000000000000000000",
			  GIT_ACK_COMMON);
	assert_ack_parses("0037ACK 0000000000000000000000000000000000000000 ready",
			  "0000000000000000000000000000000000000000",
			  GIT_ACK_READY);

	/* these should fail as they don't have OIDs */
	assert_pkt_fails("0007ACK");
	assert_pkt_fails("0008ACK ");

	/* this one is missing a space and should thus fail */
	assert_pkt_fails("0036ACK00000000000000000x0000000000000000000000 ready");

	/* the following ones have invalid OIDs and should thus fail */
	assert_pkt_fails("0037ACK 00000000000000000x0000000000000000000000 ready");
	assert_pkt_fails("0036ACK 000000000000000000000000000000000000000 ready");
	assert_pkt_fails("0036ACK 00000000000000000x0000000000000000000000ready");

	/* this one has an invalid status and should thus fail */
	assert_pkt_fails("0036ACK 0000000000000000000000000000000000000000 read");
}

void test_transports_smart_packet__nak_pkt(void)
{
	assert_nak_parses("0007NAK");
	assert_pkt_fails("0007NaK");
	assert_pkt_fails("0007nak");
	assert_nak_parses("0007NAKfoobar");
	assert_pkt_fails("0007nakfoobar");
	assert_pkt_fails("0007 NAK");
}

void test_transports_smart_packet__error_pkt(void)
{
	assert_pkt_fails("0007ERR");
	assert_pkt_fails("0008ERRx");
	assert_error_parses("0008ERR ", "", 0);
	assert_error_parses("000EERR ERRMSG", "ERRMSG", 6);
}

void test_transports_smart_packet__comment_pkt(void)
{
	assert_comment_parses("0005#", "");
	assert_comment_parses("000B#foobar", "#fooba");
	assert_comment_parses("000C#foobar", "#foobar");
	assert_comment_parses("001A#this is a comment\nfoo", "#this is a comment\nfoo");
}

void test_transports_smart_packet__ok_pkt(void)
{
	assert_pkt_fails("0007ok\n");
	assert_ok_parses("0007ok ", "");
	assert_ok_parses("0008ok \n", "");
	assert_ok_parses("0008ok x", "x");
	assert_ok_parses("0009ok x\n", "x");
	assert_pkt_fails("001OK ref/foo/bar");
	assert_ok_parses("0012ok ref/foo/bar", "ref/foo/bar");
	assert_pkt_fails("0013OK ref/foo/bar\n");
	assert_ok_parses("0013ok ref/foo/bar\n", "ref/foo/bar");
}

void test_transports_smart_packet__ng_pkt(void)
{
	/* TODO: same as for ok pkt */
	assert_pkt_fails("0007ng\n");
	assert_pkt_fails("0008ng \n");
	assert_pkt_fails("000Bng ref\n");
	assert_pkt_fails("000Bng ref\n");
	/* TODO: is this a valid packet line? Probably not. */
	assert_ng_parses("000Ang  x\n", "", "x");
	assert_ng_parses("000Fng ref msg\n", "ref", "msg");
	assert_ng_parses("000Fng ref msg\n", "ref", "msg");
}

void test_transports_smart_packet__unpack_pkt(void)
{
	assert_unpack_parses("000Dunpack ok", 1);
	assert_unpack_parses("000Dunpack ng error-msg", 0);
	/* TODO: the following tests should fail */
	assert_unpack_parses("000Aunpack", 0);
	assert_unpack_parses("0011unpack foobar", 0);
	assert_unpack_parses("0010unpack ng ok", 0);
	assert_unpack_parses("0010unpack okfoo", 1);
}

void test_transports_smart_packet__ref_pkt(void)
{
	assert_pkt_fails("002C0000000000000000000000000000000000000000");
	assert_pkt_fails("002D0000000000000000000000000000000000000000\n");
	assert_pkt_fails("00300000000000000000000000000000000000000000HEAD");
	assert_pkt_fails("004800000000x0000000000000000000000000000000 refs/heads/master\0multi_ack");
	assert_ref_parses(
		"003F0000000000000000000000000000000000000000 refs/heads/master\0",
		"0000000000000000000000000000000000000000", "refs/heads/master", "");
	assert_ref_parses(
		"00480000000000000000000000000000000000000000 refs/heads/master\0multi_ack",
		"0000000000000000000000000000000000000000", "refs/heads/master", "multi_ack");
	assert_ref_parses(
		"00460000000000000000000000000000000000000000 refs/heads/master\0one two",
		"0000000000000000000000000000000000000000", "refs/heads/master", "one two");
	assert_ref_parses(
		"00310000000000000000000000000000000000000000 HEAD",
		"0000000000000000000000000000000000000000", "HEAD", NULL);
	assert_pkt_fails("0031000000000000000000000000000000000000000 HEAD");
	assert_ref_parses(
		"00360000000000000000000000000000000000000000 HEAD HEAD",
		"0000000000000000000000000000000000000000", "HEAD HEAD", NULL);
}
