/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/types.h"
#include "git2/errors.h"
#include "git2/refs.h"
#include "git2/revwalk.h"

#include "smart.h"
#include "util.h"
#include "netops.h"
#include "posix.h"
#include "buffer.h"

#include <ctype.h>

#define PKT_LEN_SIZE 4
static const char pkt_done_str[] = "0009done\n";
static const char pkt_flush_str[] = "0000";
static const char pkt_have_prefix[] = "0032have ";
static const char pkt_want_prefix[] = "0032want ";

static int flush_pkt(git_pkt **out)
{
	git_pkt *pkt;

	pkt = git__malloc(sizeof(git_pkt));
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_FLUSH;
	*out = pkt;

	return 0;
}

/* the rest of the line will be useful for multi_ack and multi_ack_detailed */
static int ack_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_ack *pkt;
	GIT_UNUSED(line);
	GIT_UNUSED(len);

	pkt = git__calloc(1, sizeof(git_pkt_ack));
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_ACK;
	line += 3;
	len -= 3;

	if (len >= GIT_OID_HEXSZ) {
		git_oid_fromstr(&pkt->oid, line + 1);
		line += GIT_OID_HEXSZ + 1;
		len -= GIT_OID_HEXSZ + 1;
	}

	if (len >= 7) {
		if (!git__prefixcmp(line + 1, "continue"))
			pkt->status = GIT_ACK_CONTINUE;
		if (!git__prefixcmp(line + 1, "common"))
			pkt->status = GIT_ACK_COMMON;
		if (!git__prefixcmp(line + 1, "ready"))
			pkt->status = GIT_ACK_READY;
	}

	*out = (git_pkt *) pkt;

	return 0;
}

static int nak_pkt(git_pkt **out)
{
	git_pkt *pkt;

	pkt = git__malloc(sizeof(git_pkt));
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_NAK;
	*out = pkt;

	return 0;
}

static int pack_pkt(git_pkt **out)
{
	git_pkt *pkt;

	pkt = git__malloc(sizeof(git_pkt));
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_PACK;
	*out = pkt;

	return 0;
}

static int comment_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_comment *pkt;

	pkt = git__malloc(sizeof(git_pkt_comment) + len + 1);
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_COMMENT;
	memcpy(pkt->comment, line, len);
	pkt->comment[len] = '\0';

	*out = (git_pkt *) pkt;

	return 0;
}

static int err_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_err *pkt;

	/* Remove "ERR " from the line */
	line += 4;
	len -= 4;
	pkt = git__malloc(sizeof(git_pkt_err) + len + 1);
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_ERR;
	pkt->len = (int)len;
	memcpy(pkt->error, line, len);
	pkt->error[len] = '\0';

	*out = (git_pkt *) pkt;

	return 0;
}

static int data_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_data *pkt;

	line++;
	len--;
	pkt = git__malloc(sizeof(git_pkt_data) + len);
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_DATA;
	pkt->len = (int) len;
	memcpy(pkt->data, line, len);

	*out = (git_pkt *) pkt;

	return 0;
}

static int sideband_progress_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_progress *pkt;

	line++;
	len--;
	pkt = git__malloc(sizeof(git_pkt_progress) + len);
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_PROGRESS;
	pkt->len = (int) len;
	memcpy(pkt->data, line, len);

	*out = (git_pkt *) pkt;

	return 0;
}

static int sideband_error_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_err *pkt;

	line++;
	len--;
	pkt = git__malloc(sizeof(git_pkt_err) + len + 1);
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_ERR;
	pkt->len = (int)len;
	memcpy(pkt->error, line, len);
	pkt->error[len] = '\0';

	*out = (git_pkt *)pkt;

	return 0;
}

/*
 * Parse an other-ref line.
 */
static int ref_pkt(git_pkt **out, const char *line, size_t len)
{
	int error;
	git_pkt_ref *pkt;

	pkt = git__malloc(sizeof(git_pkt_ref));
	GITERR_CHECK_ALLOC(pkt);

	memset(pkt, 0x0, sizeof(git_pkt_ref));
	pkt->type = GIT_PKT_REF;
	if ((error = git_oid_fromstr(&pkt->head.oid, line)) < 0)
		goto error_out;

	/* Check for a bit of consistency */
	if (line[GIT_OID_HEXSZ] != ' ') {
		giterr_set(GITERR_NET, "Error parsing pkt-line");
		error = -1;
		goto error_out;
	}

	/* Jump from the name */
	line += GIT_OID_HEXSZ + 1;
	len -= (GIT_OID_HEXSZ + 1);

	if (line[len - 1] == '\n')
		--len;

	pkt->head.name = git__malloc(len + 1);
	GITERR_CHECK_ALLOC(pkt->head.name);

	memcpy(pkt->head.name, line, len);
	pkt->head.name[len] = '\0';

	if (strlen(pkt->head.name) < len) {
		pkt->capabilities = strchr(pkt->head.name, '\0') + 1;
	}

	*out = (git_pkt *)pkt;
	return 0;

error_out:
	git__free(pkt);
	return error;
}

static int ok_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_ok *pkt;
	const char *ptr;

	pkt = git__malloc(sizeof(*pkt));
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_OK;

	line += 3; /* skip "ok " */
	ptr = strchr(line, '\n');
	len = ptr - line;

	pkt->ref = git__malloc(len + 1);
	GITERR_CHECK_ALLOC(pkt->ref);

	memcpy(pkt->ref, line, len);
	pkt->ref[len] = '\0';

	*out = (git_pkt *)pkt;
	return 0;
}

static int ng_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_ng *pkt;
	const char *ptr;

	pkt = git__malloc(sizeof(*pkt));
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_NG;

	line += 3; /* skip "ng " */
	ptr = strchr(line, ' ');
	len = ptr - line;

	pkt->ref = git__malloc(len + 1);
	GITERR_CHECK_ALLOC(pkt->ref);

	memcpy(pkt->ref, line, len);
	pkt->ref[len] = '\0';

	line = ptr + 1;
	ptr = strchr(line, '\n');
	len = ptr - line;

	pkt->msg = git__malloc(len + 1);
	GITERR_CHECK_ALLOC(pkt->msg);

	memcpy(pkt->msg, line, len);
	pkt->msg[len] = '\0';

	*out = (git_pkt *)pkt;
	return 0;
}

static int unpack_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_unpack *pkt;

	GIT_UNUSED(len);

	pkt = git__malloc(sizeof(*pkt));
	GITERR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_UNPACK;
	if (!git__prefixcmp(line, "unpack ok"))
		pkt->unpack_ok = 1;
	else
		pkt->unpack_ok = 0;

	*out = (git_pkt *)pkt;
	return 0;
}

static int32_t parse_len(const char *line)
{
	char num[PKT_LEN_SIZE + 1];
	int i, error;
	int32_t len;
	const char *num_end;

	memcpy(num, line, PKT_LEN_SIZE);
	num[PKT_LEN_SIZE] = '\0';

	for (i = 0; i < PKT_LEN_SIZE; ++i) {
		if (!isxdigit(num[i])) {
			giterr_set(GITERR_NET, "Found invalid hex digit in length");
			return -1;
		}
	}

	if ((error = git__strtol32(&len, num, &num_end, 16)) < 0)
		return error;

	return len;
}

/*
 * As per the documentation, the syntax is:
 *
 * pkt-line	= data-pkt / flush-pkt
 * data-pkt	= pkt-len pkt-payload
 * pkt-len		= 4*(HEXDIG)
 * pkt-payload = (pkt-len -4)*(OCTET)
 * flush-pkt	= "0000"
 *
 * Which means that the first four bytes are the length of the line,
 * in ASCII hexadecimal (including itself)
 */

int git_pkt_parse_line(
	git_pkt **head, const char *line, const char **out, size_t bufflen)
{
	int ret;
	int32_t len;

	/* Not even enough for the length */
	if (bufflen > 0 && bufflen < PKT_LEN_SIZE)
		return GIT_EBUFS;

	len = parse_len(line);
	if (len < 0) {
		/*
		 * If we fail to parse the length, it might be because the
		 * server is trying to send us the packfile already.
		 */
		if (bufflen >= 4 && !git__prefixcmp(line, "PACK")) {
			giterr_clear();
			*out = line;
			return pack_pkt(head);
		}

		return (int)len;
	}

	/*
	 * If we were given a buffer length, then make sure there is
	 * enough in the buffer to satisfy this line
	 */
	if (bufflen > 0 && bufflen < (size_t)len)
		return GIT_EBUFS;

	line += PKT_LEN_SIZE;
	/*
	 * TODO: How do we deal with empty lines? Try again? with the next
	 * line?
	 */
	if (len == PKT_LEN_SIZE) {
		*out = line;
		return 0;
	}

	if (len == 0) { /* Flush pkt */
		*out = line;
		return flush_pkt(head);
	}

	len -= PKT_LEN_SIZE; /* the encoded length includes its own size */

	if (*line == GIT_SIDE_BAND_DATA)
		ret = data_pkt(head, line, len);
	else if (*line == GIT_SIDE_BAND_PROGRESS)
		ret = sideband_progress_pkt(head, line, len);
	else if (*line == GIT_SIDE_BAND_ERROR)
		ret = sideband_error_pkt(head, line, len);
	else if (!git__prefixcmp(line, "ACK"))
		ret = ack_pkt(head, line, len);
	else if (!git__prefixcmp(line, "NAK"))
		ret = nak_pkt(head);
	else if (!git__prefixcmp(line, "ERR "))
		ret = err_pkt(head, line, len);
	else if (*line == '#')
		ret = comment_pkt(head, line, len);
	else if (!git__prefixcmp(line, "ok"))
		ret = ok_pkt(head, line, len);
	else if (!git__prefixcmp(line, "ng"))
		ret = ng_pkt(head, line, len);
	else if (!git__prefixcmp(line, "unpack"))
		ret = unpack_pkt(head, line, len);
	else
		ret = ref_pkt(head, line, len);

	*out = line + len;

	return ret;
}

void git_pkt_free(git_pkt *pkt)
{
	if (pkt->type == GIT_PKT_REF) {
		git_pkt_ref *p = (git_pkt_ref *) pkt;
		git__free(p->head.name);
	}

	if (pkt->type == GIT_PKT_OK) {
		git_pkt_ok *p = (git_pkt_ok *) pkt;
		git__free(p->ref);
	}

	if (pkt->type == GIT_PKT_NG) {
		git_pkt_ng *p = (git_pkt_ng *) pkt;
		git__free(p->ref);
		git__free(p->msg);
	}

	git__free(pkt);
}

int git_pkt_buffer_flush(git_buf *buf)
{
	return git_buf_put(buf, pkt_flush_str, strlen(pkt_flush_str));
}

static int buffer_want_with_caps(const git_remote_head *head, transport_smart_caps *caps, git_buf *buf)
{
	git_buf str = GIT_BUF_INIT;
	char oid[GIT_OID_HEXSZ +1] = {0};
	unsigned int len;

	/* Prefer multi_ack_detailed */
	if (caps->multi_ack_detailed)
		git_buf_puts(&str, GIT_CAP_MULTI_ACK_DETAILED " ");
	else if (caps->multi_ack)
		git_buf_puts(&str, GIT_CAP_MULTI_ACK " ");

	/* Prefer side-band-64k if the server supports both */
	if (caps->side_band_64k)
		git_buf_printf(&str, "%s ", GIT_CAP_SIDE_BAND_64K);
	else if (caps->side_band)
		git_buf_printf(&str, "%s ", GIT_CAP_SIDE_BAND);

	if (caps->include_tag)
		git_buf_puts(&str, GIT_CAP_INCLUDE_TAG " ");

	if (caps->thin_pack)
		git_buf_puts(&str, GIT_CAP_THIN_PACK " ");

	if (caps->ofs_delta)
		git_buf_puts(&str, GIT_CAP_OFS_DELTA " ");

	if (git_buf_oom(&str))
		return -1;

	len = (unsigned int)
		(strlen("XXXXwant ") + GIT_OID_HEXSZ + 1 /* NUL */ +
		 git_buf_len(&str) + 1 /* LF */);
	git_buf_grow(buf, git_buf_len(buf) + len);
	git_oid_fmt(oid, &head->oid);
	git_buf_printf(buf, "%04xwant %s %s\n", len, oid, git_buf_cstr(&str));
	git_buf_free(&str);

	return git_buf_oom(buf);
}

/*
 * All "want" packets have the same length and format, so what we do
 * is overwrite the OID each time.
 */

int git_pkt_buffer_wants(
	const git_remote_head * const *refs,
	size_t count,
	transport_smart_caps *caps,
	git_buf *buf)
{
	size_t i = 0;
	const git_remote_head *head;

	if (caps->common) {
		for (; i < count; ++i) {
			head = refs[i];
			if (!head->local)
				break;
		}

		if (buffer_want_with_caps(refs[i], caps, buf) < 0)
			return -1;

		i++;
	}

	for (; i < count; ++i) {
		char oid[GIT_OID_HEXSZ];

		head = refs[i];
		if (head->local)
			continue;

		git_oid_fmt(oid, &head->oid);
		git_buf_put(buf, pkt_want_prefix, strlen(pkt_want_prefix));
		git_buf_put(buf, oid, GIT_OID_HEXSZ);
		git_buf_putc(buf, '\n');
		if (git_buf_oom(buf))
			return -1;
	}

	return git_pkt_buffer_flush(buf);
}

int git_pkt_buffer_have(git_oid *oid, git_buf *buf)
{
	char oidhex[GIT_OID_HEXSZ + 1];

	memset(oidhex, 0x0, sizeof(oidhex));
	git_oid_fmt(oidhex, oid);
	return git_buf_printf(buf, "%s%s\n", pkt_have_prefix, oidhex);
}

int git_pkt_buffer_done(git_buf *buf)
{
	return git_buf_puts(buf, pkt_done_str);
}
