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
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_FLUSH;
	*out = pkt;

	return 0;
}

/* the rest of the line will be useful for multi_ack and multi_ack_detailed */
static int ack_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_ack *pkt;

	pkt = git__calloc(1, sizeof(git_pkt_ack));
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_ACK;

	if (git__prefixncmp(line, len, "ACK "))
		goto out_err;
	line += 4;
	len -= 4;

	if (len < GIT_OID_HEXSZ || git_oid_fromstr(&pkt->oid, line) < 0)
		goto out_err;
	line += GIT_OID_HEXSZ;
	len -= GIT_OID_HEXSZ;

	if (len && line[0] == ' ') {
		line++;
		len--;

		if (!git__prefixncmp(line, len, "continue"))
			pkt->status = GIT_ACK_CONTINUE;
		else if (!git__prefixncmp(line, len, "common"))
			pkt->status = GIT_ACK_COMMON;
		else if (!git__prefixncmp(line, len, "ready"))
			pkt->status = GIT_ACK_READY;
		else
			goto out_err;
	}

	*out = (git_pkt *) pkt;

	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "error parsing ACK pkt-line");
	git__free(pkt);
	return -1;
}

static int nak_pkt(git_pkt **out)
{
	git_pkt *pkt;

	pkt = git__malloc(sizeof(git_pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_NAK;
	*out = pkt;

	return 0;
}

static int comment_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_comment *pkt;
	size_t alloclen;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_pkt_comment), len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	pkt = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_COMMENT;
	memcpy(pkt->comment, line, len);
	pkt->comment[len] = '\0';

	*out = (git_pkt *) pkt;

	return 0;
}

static int err_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_err *pkt = NULL;
	size_t alloclen;

	/* Remove "ERR " from the line */
	if (git__prefixncmp(line, len, "ERR "))
		goto out_err;
	line += 4;
	len -= 4;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_pkt_progress), len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	pkt = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_ERR;
	pkt->len = len;

	memcpy(pkt->error, line, len);
	pkt->error[len] = '\0';

	*out = (git_pkt *) pkt;

	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "error parsing ERR pkt-line");
	git__free(pkt);
	return -1;
}

static int data_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_data *pkt;
	size_t alloclen;

	line++;
	len--;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_pkt_progress), len);
	pkt = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_DATA;
	pkt->len = len;
	memcpy(pkt->data, line, len);

	*out = (git_pkt *) pkt;

	return 0;
}

static int sideband_progress_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_progress *pkt;
	size_t alloclen;

	line++;
	len--;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_pkt_progress), len);
	pkt = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_PROGRESS;
	pkt->len = len;
	memcpy(pkt->data, line, len);

	*out = (git_pkt *) pkt;

	return 0;
}

static int sideband_error_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_err *pkt;
	size_t alloc_len;

	line++;
	len--;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, sizeof(git_pkt_err), len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 1);
	pkt = git__malloc(alloc_len);
	GIT_ERROR_CHECK_ALLOC(pkt);

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
	git_pkt_ref *pkt;
	size_t alloclen;

	pkt = git__calloc(1, sizeof(git_pkt_ref));
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_REF;

	if (len < GIT_OID_HEXSZ || git_oid_fromstr(&pkt->head.oid, line) < 0)
		goto out_err;
	line += GIT_OID_HEXSZ;
	len -= GIT_OID_HEXSZ;

	if (git__prefixncmp(line, len, " "))
		goto out_err;
	line++;
	len--;

	if (!len)
		goto out_err;

	if (line[len - 1] == '\n')
		--len;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, len, 1);
	pkt->head.name = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt->head.name);

	memcpy(pkt->head.name, line, len);
	pkt->head.name[len] = '\0';

	if (strlen(pkt->head.name) < len)
		pkt->capabilities = strchr(pkt->head.name, '\0') + 1;

	*out = (git_pkt *)pkt;
	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "error parsing REF pkt-line");
	if (pkt)
		git__free(pkt->head.name);
	git__free(pkt);
	return -1;
}

static int ok_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_ok *pkt;
	size_t alloc_len;

	pkt = git__malloc(sizeof(*pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_OK;

	if (git__prefixncmp(line, len, "ok "))
		goto out_err;
	line += 3;
	len -= 3;

	if (len && line[len - 1] == '\n')
		--len;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, len, 1);
	pkt->ref = git__malloc(alloc_len);
	GIT_ERROR_CHECK_ALLOC(pkt->ref);

	memcpy(pkt->ref, line, len);
	pkt->ref[len] = '\0';

	*out = (git_pkt *)pkt;
	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "error parsing OK pkt-line");
	git__free(pkt);
	return -1;
}

static int ng_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_ng *pkt;
	const char *ptr, *eol;
	size_t alloclen;

	pkt = git__malloc(sizeof(*pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->ref = NULL;
	pkt->type = GIT_PKT_NG;

	eol = line + len;

	if (git__prefixncmp(line, len, "ng "))
		goto out_err;
	line += 3;

	if (!(ptr = memchr(line, ' ', eol - line)))
		goto out_err;
	len = ptr - line;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, len, 1);
	pkt->ref = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt->ref);

	memcpy(pkt->ref, line, len);
	pkt->ref[len] = '\0';

	line = ptr + 1;
	if (line >= eol)
		goto out_err;

	if (!(ptr = memchr(line, '\n', eol - line)))
		goto out_err;
	len = ptr - line;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, len, 1);
	pkt->msg = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt->msg);

	memcpy(pkt->msg, line, len);
	pkt->msg[len] = '\0';

	*out = (git_pkt *)pkt;
	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "invalid packet line");
	git__free(pkt->ref);
	git__free(pkt);
	return -1;
}

static int unpack_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_unpack *pkt;

	pkt = git__malloc(sizeof(*pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_UNPACK;

	if (!git__prefixncmp(line, len, "unpack ok"))
		pkt->unpack_ok = 1;
	else
		pkt->unpack_ok = 0;

	*out = (git_pkt *)pkt;
	return 0;
}

static int parse_len(size_t *out, const char *line, size_t linelen)
{
	char num[PKT_LEN_SIZE + 1];
	int i, k, error;
	int32_t len;
	const char *num_end;

	/* Not even enough for the length */
	if (linelen < PKT_LEN_SIZE)
		return GIT_EBUFS;

	memcpy(num, line, PKT_LEN_SIZE);
	num[PKT_LEN_SIZE] = '\0';

	for (i = 0; i < PKT_LEN_SIZE; ++i) {
		if (!isxdigit(num[i])) {
			/* Make sure there are no special characters before passing to error message */
			for (k = 0; k < PKT_LEN_SIZE; ++k) {
				if(!isprint(num[k])) {
					num[k] = '.';
				}
			}

			git_error_set(GIT_ERROR_NET, "invalid hex digit in length: '%s'", num);
			return -1;
		}
	}

	if ((error = git__strntol32(&len, num, PKT_LEN_SIZE, &num_end, 16)) < 0)
		return error;

	if (len < 0)
		return -1;

	*out = (size_t) len;
	return 0;
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
	git_pkt **pkt, const char **endptr, const char *line, size_t linelen)
{
	int error;
	size_t len;

	if ((error = parse_len(&len, line, linelen)) < 0) {
		/*
		 * If we fail to parse the length, it might be
		 * because the server is trying to send us the
		 * packfile already or because we do not yet have
		 * enough data.
		 */
		if (error == GIT_EBUFS)
			;
		else if (!git__prefixncmp(line, linelen, "PACK"))
			git_error_set(GIT_ERROR_NET, "unexpected pack file");
		else
			git_error_set(GIT_ERROR_NET, "bad packet length");
		return error;
	}

	/*
	 * Make sure there is enough in the buffer to satisfy
	 * this line.
	 */
	if (linelen < len)
		return GIT_EBUFS;

	/*
	 * The length has to be exactly 0 in case of a flush
	 * packet or greater than PKT_LEN_SIZE, as the decoded
	 * length includes its own encoded length of four bytes.
	 */
	if (len != 0 && len < PKT_LEN_SIZE)
		return GIT_ERROR;

	line += PKT_LEN_SIZE;
	/*
	 * The Git protocol does not specify empty lines as part
	 * of the protocol. Not knowing what to do with an empty
	 * line, we should return an error upon hitting one.
	 */
	if (len == PKT_LEN_SIZE) {
		git_error_set_str(GIT_ERROR_NET, "Invalid empty packet");
		return GIT_ERROR;
	}

	if (len == 0) { /* Flush pkt */
		*endptr = line;
		return flush_pkt(pkt);
	}

	len -= PKT_LEN_SIZE; /* the encoded length includes its own size */

	if (*line == GIT_SIDE_BAND_DATA)
		error = data_pkt(pkt, line, len);
	else if (*line == GIT_SIDE_BAND_PROGRESS)
		error = sideband_progress_pkt(pkt, line, len);
	else if (*line == GIT_SIDE_BAND_ERROR)
		error = sideband_error_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "ACK"))
		error = ack_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "NAK"))
		error = nak_pkt(pkt);
	else if (!git__prefixncmp(line, len, "ERR"))
		error = err_pkt(pkt, line, len);
	else if (*line == '#')
		error = comment_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "ok"))
		error = ok_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "ng"))
		error = ng_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "unpack"))
		error = unpack_pkt(pkt, line, len);
	else
		error = ref_pkt(pkt, line, len);

	*endptr = line + len;

	return error;
}

void git_pkt_free(git_pkt *pkt)
{
	if (pkt == NULL) {
		return;
	}
	if (pkt->type == GIT_PKT_REF) {
		git_pkt_ref *p = (git_pkt_ref *) pkt;
		git__free(p->head.name);
		git__free(p->head.symref_target);
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
	size_t len;

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

	len = strlen("XXXXwant ") + GIT_OID_HEXSZ + 1 /* NUL */ +
		 git_buf_len(&str) + 1 /* LF */;

	if (len > 0xffff) {
		git_error_set(GIT_ERROR_NET,
			"tried to produce packet with invalid length %" PRIuZ, len);
		return -1;
	}

	git_buf_grow_by(buf, len);
	git_oid_fmt(oid, &head->oid);
	git_buf_printf(buf,
		"%04xwant %s %s\n", (unsigned int)len, oid, git_buf_cstr(&str));
	git_buf_dispose(&str);

	GIT_ERROR_CHECK_ALLOC_BUF(buf);

	return 0;
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
