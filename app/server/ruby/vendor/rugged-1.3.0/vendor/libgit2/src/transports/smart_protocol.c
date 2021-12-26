/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2.h"
#include "git2/odb_backend.h"

#include "smart.h"
#include "refs.h"
#include "repository.h"
#include "push.h"
#include "pack-objects.h"
#include "remote.h"
#include "util.h"
#include "revwalk.h"

#define NETWORK_XFER_THRESHOLD (100*1024)
/* The minimal interval between progress updates (in seconds). */
#define MIN_PROGRESS_UPDATE_INTERVAL 0.5

bool git_smart__ofs_delta_enabled = true;

int git_smart__store_refs(transport_smart *t, int flushes)
{
	gitno_buffer *buf = &t->buffer;
	git_vector *refs = &t->refs;
	int error, flush = 0, recvd;
	const char *line_end = NULL;
	git_pkt *pkt = NULL;
	size_t i;

	/* Clear existing refs in case git_remote_connect() is called again
	 * after git_remote_disconnect().
	 */
	git_vector_foreach(refs, i, pkt) {
		git_pkt_free(pkt);
	}
	git_vector_clear(refs);
	pkt = NULL;

	do {
		if (buf->offset > 0)
			error = git_pkt_parse_line(&pkt, &line_end, buf->data, buf->offset);
		else
			error = GIT_EBUFS;

		if (error < 0 && error != GIT_EBUFS)
			return error;

		if (error == GIT_EBUFS) {
			if ((recvd = gitno_recv(buf)) < 0)
				return recvd;

			if (recvd == 0) {
				git_error_set(GIT_ERROR_NET, "early EOF");
				return GIT_EEOF;
			}

			continue;
		}

		if (gitno_consume(buf, line_end) < 0)
			return -1;

		if (pkt->type == GIT_PKT_ERR) {
			git_error_set(GIT_ERROR_NET, "remote error: %s", ((git_pkt_err *)pkt)->error);
			git__free(pkt);
			return -1;
		}

		if (pkt->type != GIT_PKT_FLUSH && git_vector_insert(refs, pkt) < 0)
			return -1;

		if (pkt->type == GIT_PKT_FLUSH) {
			flush++;
			git_pkt_free(pkt);
		}
	} while (flush < flushes);

	return flush;
}

static int append_symref(const char **out, git_vector *symrefs, const char *ptr)
{
	int error;
	const char *end;
	git_buf buf = GIT_BUF_INIT;
	git_refspec *mapping = NULL;

	ptr += strlen(GIT_CAP_SYMREF);
	if (*ptr != '=')
		goto on_invalid;

	ptr++;
	if (!(end = strchr(ptr, ' ')) &&
	    !(end = strchr(ptr, '\0')))
		goto on_invalid;

	if ((error = git_buf_put(&buf, ptr, end - ptr)) < 0)
		return error;

	/* symref mapping has refspec format */
	mapping = git__calloc(1, sizeof(git_refspec));
	GIT_ERROR_CHECK_ALLOC(mapping);

	error = git_refspec__parse(mapping, git_buf_cstr(&buf), true);
	git_buf_dispose(&buf);

	/* if the error isn't OOM, then it's a parse error; let's use a nicer message */
	if (error < 0) {
		if (git_error_last()->klass != GIT_ERROR_NOMEMORY)
			goto on_invalid;

		git__free(mapping);
		return error;
	}

	if ((error = git_vector_insert(symrefs, mapping)) < 0)
		return error;

	*out = end;
	return 0;

on_invalid:
	git_error_set(GIT_ERROR_NET, "remote sent invalid symref");
	git_refspec__dispose(mapping);
	git__free(mapping);
	return -1;
}

int git_smart__detect_caps(git_pkt_ref *pkt, transport_smart_caps *caps, git_vector *symrefs)
{
	const char *ptr;

	/* No refs or capabilites, odd but not a problem */
	if (pkt == NULL || pkt->capabilities == NULL)
		return GIT_ENOTFOUND;

	ptr = pkt->capabilities;
	while (ptr != NULL && *ptr != '\0') {
		if (*ptr == ' ')
			ptr++;

		if (git_smart__ofs_delta_enabled && !git__prefixcmp(ptr, GIT_CAP_OFS_DELTA)) {
			caps->common = caps->ofs_delta = 1;
			ptr += strlen(GIT_CAP_OFS_DELTA);
			continue;
		}

		/* Keep multi_ack_detailed before multi_ack */
		if (!git__prefixcmp(ptr, GIT_CAP_MULTI_ACK_DETAILED)) {
			caps->common = caps->multi_ack_detailed = 1;
			ptr += strlen(GIT_CAP_MULTI_ACK_DETAILED);
			continue;
		}

		if (!git__prefixcmp(ptr, GIT_CAP_MULTI_ACK)) {
			caps->common = caps->multi_ack = 1;
			ptr += strlen(GIT_CAP_MULTI_ACK);
			continue;
		}

		if (!git__prefixcmp(ptr, GIT_CAP_INCLUDE_TAG)) {
			caps->common = caps->include_tag = 1;
			ptr += strlen(GIT_CAP_INCLUDE_TAG);
			continue;
		}

		/* Keep side-band check after side-band-64k */
		if (!git__prefixcmp(ptr, GIT_CAP_SIDE_BAND_64K)) {
			caps->common = caps->side_band_64k = 1;
			ptr += strlen(GIT_CAP_SIDE_BAND_64K);
			continue;
		}

		if (!git__prefixcmp(ptr, GIT_CAP_SIDE_BAND)) {
			caps->common = caps->side_band = 1;
			ptr += strlen(GIT_CAP_SIDE_BAND);
			continue;
		}

		if (!git__prefixcmp(ptr, GIT_CAP_DELETE_REFS)) {
			caps->common = caps->delete_refs = 1;
			ptr += strlen(GIT_CAP_DELETE_REFS);
			continue;
		}

		if (!git__prefixcmp(ptr, GIT_CAP_THIN_PACK)) {
			caps->common = caps->thin_pack = 1;
			ptr += strlen(GIT_CAP_THIN_PACK);
			continue;
		}

		if (!git__prefixcmp(ptr, GIT_CAP_SYMREF)) {
			int error;

			if ((error = append_symref(&ptr, symrefs, ptr)) < 0)
				return error;

			continue;
		}

		/* We don't know this capability, so skip it */
		ptr = strchr(ptr, ' ');
	}

	return 0;
}

static int recv_pkt(git_pkt **out_pkt, git_pkt_type *out_type, gitno_buffer *buf)
{
	const char *ptr = buf->data, *line_end = ptr;
	git_pkt *pkt = NULL;
	int error = 0, ret;

	do {
		if (buf->offset > 0)
			error = git_pkt_parse_line(&pkt, &line_end, ptr, buf->offset);
		else
			error = GIT_EBUFS;

		if (error == 0)
			break; /* return the pkt */

		if (error < 0 && error != GIT_EBUFS)
			return error;

		if ((ret = gitno_recv(buf)) < 0) {
			return ret;
		} else if (ret == 0) {
			git_error_set(GIT_ERROR_NET, "early EOF");
			return GIT_EEOF;
		}
	} while (error);

	if (gitno_consume(buf, line_end) < 0)
		return -1;

	if (out_type != NULL)
		*out_type = pkt->type;
	if (out_pkt != NULL)
		*out_pkt = pkt;
	else
		git__free(pkt);

	return error;
}

static int store_common(transport_smart *t)
{
	git_pkt *pkt = NULL;
	gitno_buffer *buf = &t->buffer;
	int error;

	do {
		if ((error = recv_pkt(&pkt, NULL, buf)) < 0)
			return error;

		if (pkt->type != GIT_PKT_ACK) {
			git__free(pkt);
			return 0;
		}

		if (git_vector_insert(&t->common, pkt) < 0) {
			git__free(pkt);
			return -1;
		}
	} while (1);

	return 0;
}

static int wait_while_ack(gitno_buffer *buf)
{
	int error;
	git_pkt *pkt = NULL;
	git_pkt_ack *ack = NULL;

	while (1) {
		git_pkt_free(pkt);

		if ((error = recv_pkt(&pkt, NULL, buf)) < 0)
			return error;

		if (pkt->type == GIT_PKT_NAK)
			break;
		if (pkt->type != GIT_PKT_ACK)
			continue;

		ack = (git_pkt_ack*)pkt;

		if (ack->status != GIT_ACK_CONTINUE &&
		    ack->status != GIT_ACK_COMMON &&
		    ack->status != GIT_ACK_READY) {
			break;
		}
	}

	git_pkt_free(pkt);
	return 0;
}

int git_smart__negotiate_fetch(git_transport *transport, git_repository *repo, const git_remote_head * const *wants, size_t count)
{
	transport_smart *t = (transport_smart *)transport;
	git_revwalk__push_options opts = GIT_REVWALK__PUSH_OPTIONS_INIT;
	gitno_buffer *buf = &t->buffer;
	git_buf data = GIT_BUF_INIT;
	git_revwalk *walk = NULL;
	int error = -1;
	git_pkt_type pkt_type;
	unsigned int i;
	git_oid oid;

	if ((error = git_pkt_buffer_wants(wants, count, &t->caps, &data)) < 0)
		return error;

	if ((error = git_revwalk_new(&walk, repo)) < 0)
		goto on_error;

	opts.insert_by_date = 1;
	if ((error = git_revwalk__push_glob(walk, "refs/*", &opts)) < 0)
		goto on_error;

	/*
	 * Our support for ACK extensions is simply to parse them. On
	 * the first ACK we will accept that as enough common
	 * objects. We give up if we haven't found an answer in the
	 * first 256 we send.
	 */
	i = 0;
	while (i < 256) {
		error = git_revwalk_next(&oid, walk);

		if (error < 0) {
			if (GIT_ITEROVER == error)
				break;

			goto on_error;
		}

		git_pkt_buffer_have(&oid, &data);
		i++;
		if (i % 20 == 0) {
			if (t->cancelled.val) {
				git_error_set(GIT_ERROR_NET, "The fetch was cancelled by the user");
				error = GIT_EUSER;
				goto on_error;
			}

			git_pkt_buffer_flush(&data);
			if (git_buf_oom(&data)) {
				error = -1;
				goto on_error;
			}

			if ((error = git_smart__negotiation_step(&t->parent, data.ptr, data.size)) < 0)
				goto on_error;

			git_buf_clear(&data);
			if (t->caps.multi_ack || t->caps.multi_ack_detailed) {
				if ((error = store_common(t)) < 0)
					goto on_error;
			} else {
				if ((error = recv_pkt(NULL, &pkt_type, buf)) < 0)
					goto on_error;

				if (pkt_type == GIT_PKT_ACK) {
					break;
				} else if (pkt_type == GIT_PKT_NAK) {
					continue;
				} else {
					git_error_set(GIT_ERROR_NET, "unexpected pkt type");
					error = -1;
					goto on_error;
				}
			}
		}

		if (t->common.length > 0)
			break;

		if (i % 20 == 0 && t->rpc) {
			git_pkt_ack *pkt;
			unsigned int j;

			if ((error = git_pkt_buffer_wants(wants, count, &t->caps, &data)) < 0)
				goto on_error;

			git_vector_foreach(&t->common, j, pkt) {
				if ((error = git_pkt_buffer_have(&pkt->oid, &data)) < 0)
					goto on_error;
			}

			if (git_buf_oom(&data)) {
				error = -1;
				goto on_error;
			}
		}
	}

	/* Tell the other end that we're done negotiating */
	if (t->rpc && t->common.length > 0) {
		git_pkt_ack *pkt;
		unsigned int j;

		if ((error = git_pkt_buffer_wants(wants, count, &t->caps, &data)) < 0)
			goto on_error;

		git_vector_foreach(&t->common, j, pkt) {
			if ((error = git_pkt_buffer_have(&pkt->oid, &data)) < 0)
				goto on_error;
		}

		if (git_buf_oom(&data)) {
			error = -1;
			goto on_error;
		}
	}

	if ((error = git_pkt_buffer_done(&data)) < 0)
		goto on_error;

	if (t->cancelled.val) {
		git_error_set(GIT_ERROR_NET, "The fetch was cancelled by the user");
		error = GIT_EUSER;
		goto on_error;
	}
	if ((error = git_smart__negotiation_step(&t->parent, data.ptr, data.size)) < 0)
		goto on_error;

	git_buf_dispose(&data);
	git_revwalk_free(walk);

	/* Now let's eat up whatever the server gives us */
	if (!t->caps.multi_ack && !t->caps.multi_ack_detailed) {
		if ((error = recv_pkt(NULL, &pkt_type, buf)) < 0)
			return error;

		if (pkt_type != GIT_PKT_ACK && pkt_type != GIT_PKT_NAK) {
			git_error_set(GIT_ERROR_NET, "unexpected pkt type");
			return -1;
		}
	} else {
		error = wait_while_ack(buf);
	}

	return error;

on_error:
	git_revwalk_free(walk);
	git_buf_dispose(&data);
	return error;
}

static int no_sideband(transport_smart *t, struct git_odb_writepack *writepack, gitno_buffer *buf, git_indexer_progress *stats)
{
	int recvd;

	do {
		if (t->cancelled.val) {
			git_error_set(GIT_ERROR_NET, "the fetch was cancelled by the user");
			return GIT_EUSER;
		}

		if (writepack->append(writepack, buf->data, buf->offset, stats) < 0)
			return -1;

		gitno_consume_n(buf, buf->offset);

		if ((recvd = gitno_recv(buf)) < 0)
			return recvd;
	} while(recvd > 0);

	if (writepack->commit(writepack, stats) < 0)
		return -1;

	return 0;
}

struct network_packetsize_payload
{
	git_indexer_progress_cb callback;
	void *payload;
	git_indexer_progress *stats;
	size_t last_fired_bytes;
};

static int network_packetsize(size_t received, void *payload)
{
	struct network_packetsize_payload *npp = (struct network_packetsize_payload*)payload;

	/* Accumulate bytes */
	npp->stats->received_bytes += received;

	/* Fire notification if the threshold is reached */
	if ((npp->stats->received_bytes - npp->last_fired_bytes) > NETWORK_XFER_THRESHOLD) {
		npp->last_fired_bytes = npp->stats->received_bytes;

		if (npp->callback(npp->stats, npp->payload))
			return GIT_EUSER;
	}

	return 0;
}

int git_smart__download_pack(
	git_transport *transport,
	git_repository *repo,
	git_indexer_progress *stats,
	git_indexer_progress_cb progress_cb,
	void *progress_payload)
{
	transport_smart *t = (transport_smart *)transport;
	gitno_buffer *buf = &t->buffer;
	git_odb *odb;
	struct git_odb_writepack *writepack = NULL;
	int error = 0;
	struct network_packetsize_payload npp = {0};

	memset(stats, 0, sizeof(git_indexer_progress));

	if (progress_cb) {
		npp.callback = progress_cb;
		npp.payload = progress_payload;
		npp.stats = stats;
		t->packetsize_cb = &network_packetsize;
		t->packetsize_payload = &npp;

		/* We might have something in the buffer already from negotiate_fetch */
		if (t->buffer.offset > 0 && !t->cancelled.val)
			if (t->packetsize_cb(t->buffer.offset, t->packetsize_payload))
				git_atomic32_set(&t->cancelled, 1);
	}

	if ((error = git_repository_odb__weakptr(&odb, repo)) < 0 ||
		((error = git_odb_write_pack(&writepack, odb, progress_cb, progress_payload)) != 0))
		goto done;

	/*
	 * If the remote doesn't support the side-band, we can feed
	 * the data directly to the pack writer. Otherwise, we need to
	 * check which one belongs there.
	 */
	if (!t->caps.side_band && !t->caps.side_band_64k) {
		error = no_sideband(t, writepack, buf, stats);
		goto done;
	}

	do {
		git_pkt *pkt = NULL;

		/* Check cancellation before network call */
		if (t->cancelled.val) {
			git_error_clear();
			error = GIT_EUSER;
			goto done;
		}

		if ((error = recv_pkt(&pkt, NULL, buf)) >= 0) {
			/* Check cancellation after network call */
			if (t->cancelled.val) {
				git_error_clear();
				error = GIT_EUSER;
			} else if (pkt->type == GIT_PKT_PROGRESS) {
				if (t->progress_cb) {
					git_pkt_progress *p = (git_pkt_progress *) pkt;

					if (p->len > INT_MAX) {
						git_error_set(GIT_ERROR_NET, "oversized progress message");
						error = GIT_ERROR;
						goto done;
					}

					error = t->progress_cb(p->data, (int)p->len, t->message_cb_payload);
				}
			} else if (pkt->type == GIT_PKT_DATA) {
				git_pkt_data *p = (git_pkt_data *) pkt;

				if (p->len)
					error = writepack->append(writepack, p->data, p->len, stats);
			} else if (pkt->type == GIT_PKT_FLUSH) {
				/* A flush indicates the end of the packfile */
				git__free(pkt);
				break;
			}
		}

		git_pkt_free(pkt);

		if (error < 0)
			goto done;

	} while (1);

	/*
	 * Trailing execution of progress_cb, if necessary...
	 * Only the callback through the npp datastructure currently
	 * updates the last_fired_bytes value. It is possible that
	 * progress has already been reported with the correct
	 * "received_bytes" value, but until (if?) this is unified
	 * then we will report progress again to be sure that the
	 * correct last received_bytes value is reported.
	 */
	if (npp.callback && npp.stats->received_bytes > npp.last_fired_bytes) {
		error = npp.callback(npp.stats, npp.payload);
		if (error != 0)
			goto done;
	}

	error = writepack->commit(writepack, stats);

done:
	if (writepack)
		writepack->free(writepack);
	if (progress_cb) {
		t->packetsize_cb = NULL;
		t->packetsize_payload = NULL;
	}

	return error;
}

static int gen_pktline(git_buf *buf, git_push *push)
{
	push_spec *spec;
	size_t i, len;
	char old_id[GIT_OID_HEXSZ+1], new_id[GIT_OID_HEXSZ+1];

	old_id[GIT_OID_HEXSZ] = '\0'; new_id[GIT_OID_HEXSZ] = '\0';

	git_vector_foreach(&push->specs, i, spec) {
		len = 2*GIT_OID_HEXSZ + 7 + strlen(spec->refspec.dst);

		if (i == 0) {
			++len; /* '\0' */
			if (push->report_status)
				len += strlen(GIT_CAP_REPORT_STATUS) + 1;
			len += strlen(GIT_CAP_SIDE_BAND_64K) + 1;
		}

		git_oid_fmt(old_id, &spec->roid);
		git_oid_fmt(new_id, &spec->loid);

		git_buf_printf(buf, "%04"PRIxZ"%s %s %s", len, old_id, new_id, spec->refspec.dst);

		if (i == 0) {
			git_buf_putc(buf, '\0');
			/* Core git always starts their capabilities string with a space */
			if (push->report_status) {
				git_buf_putc(buf, ' ');
				git_buf_printf(buf, GIT_CAP_REPORT_STATUS);
			}
			git_buf_putc(buf, ' ');
			git_buf_printf(buf, GIT_CAP_SIDE_BAND_64K);
		}

		git_buf_putc(buf, '\n');
	}

	git_buf_puts(buf, "0000");
	return git_buf_oom(buf) ? -1 : 0;
}

static int add_push_report_pkt(git_push *push, git_pkt *pkt)
{
	push_status *status;

	switch (pkt->type) {
		case GIT_PKT_OK:
			status = git__calloc(1, sizeof(push_status));
			GIT_ERROR_CHECK_ALLOC(status);
			status->msg = NULL;
			status->ref = git__strdup(((git_pkt_ok *)pkt)->ref);
			if (!status->ref ||
				git_vector_insert(&push->status, status) < 0) {
				git_push_status_free(status);
				return -1;
			}
			break;
		case GIT_PKT_NG:
			status = git__calloc(1, sizeof(push_status));
			GIT_ERROR_CHECK_ALLOC(status);
			status->ref = git__strdup(((git_pkt_ng *)pkt)->ref);
			status->msg = git__strdup(((git_pkt_ng *)pkt)->msg);
			if (!status->ref || !status->msg ||
				git_vector_insert(&push->status, status) < 0) {
				git_push_status_free(status);
				return -1;
			}
			break;
		case GIT_PKT_UNPACK:
			push->unpack_ok = ((git_pkt_unpack *)pkt)->unpack_ok;
			break;
		case GIT_PKT_FLUSH:
			return GIT_ITEROVER;
		default:
			git_error_set(GIT_ERROR_NET, "report-status: protocol error");
			return -1;
	}

	return 0;
}

static int add_push_report_sideband_pkt(git_push *push, git_pkt_data *data_pkt, git_buf *data_pkt_buf)
{
	git_pkt *pkt;
	const char *line, *line_end = NULL;
	size_t line_len;
	int error;
	int reading_from_buf = data_pkt_buf->size > 0;

	if (reading_from_buf) {
		/* We had an existing partial packet, so add the new
		 * packet to the buffer and parse the whole thing */
		git_buf_put(data_pkt_buf, data_pkt->data, data_pkt->len);
		line = data_pkt_buf->ptr;
		line_len = data_pkt_buf->size;
	}
	else {
		line = data_pkt->data;
		line_len = data_pkt->len;
	}

	while (line_len > 0) {
		error = git_pkt_parse_line(&pkt, &line_end, line, line_len);

		if (error == GIT_EBUFS) {
			/* Buffer the data when the inner packet is split
			 * across multiple sideband packets */
			if (!reading_from_buf)
				git_buf_put(data_pkt_buf, line, line_len);
			error = 0;
			goto done;
		}
		else if (error < 0)
			goto done;

		/* Advance in the buffer */
		line_len -= (line_end - line);
		line = line_end;

		error = add_push_report_pkt(push, pkt);

		git_pkt_free(pkt);

		if (error < 0 && error != GIT_ITEROVER)
			goto done;
	}

	error = 0;

done:
	if (reading_from_buf)
		git_buf_consume(data_pkt_buf, line_end);
	return error;
}

static int parse_report(transport_smart *transport, git_push *push)
{
	git_pkt *pkt = NULL;
	const char *line_end = NULL;
	gitno_buffer *buf = &transport->buffer;
	int error, recvd;
	git_buf data_pkt_buf = GIT_BUF_INIT;

	for (;;) {
		if (buf->offset > 0)
			error = git_pkt_parse_line(&pkt, &line_end,
						   buf->data, buf->offset);
		else
			error = GIT_EBUFS;

		if (error < 0 && error != GIT_EBUFS) {
			error = -1;
			goto done;
		}

		if (error == GIT_EBUFS) {
			if ((recvd = gitno_recv(buf)) < 0) {
				error = recvd;
				goto done;
			}

			if (recvd == 0) {
				git_error_set(GIT_ERROR_NET, "early EOF");
				error = GIT_EEOF;
				goto done;
			}
			continue;
		}

		if (gitno_consume(buf, line_end) < 0)
			return -1;

		error = 0;

		switch (pkt->type) {
			case GIT_PKT_DATA:
				/* This is a sideband packet which contains other packets */
				error = add_push_report_sideband_pkt(push, (git_pkt_data *)pkt, &data_pkt_buf);
				break;
			case GIT_PKT_ERR:
				git_error_set(GIT_ERROR_NET, "report-status: Error reported: %s",
					((git_pkt_err *)pkt)->error);
				error = -1;
				break;
			case GIT_PKT_PROGRESS:
				if (transport->progress_cb) {
					git_pkt_progress *p = (git_pkt_progress *) pkt;

					if (p->len > INT_MAX) {
						git_error_set(GIT_ERROR_NET, "oversized progress message");
						error = GIT_ERROR;
						goto done;
					}

					error = transport->progress_cb(p->data, (int)p->len, transport->message_cb_payload);
				}
				break;
			default:
				error = add_push_report_pkt(push, pkt);
				break;
		}

		git_pkt_free(pkt);

		/* add_push_report_pkt returns GIT_ITEROVER when it receives a flush */
		if (error == GIT_ITEROVER) {
			error = 0;
			if (data_pkt_buf.size > 0) {
				/* If there was data remaining in the pack data buffer,
				 * then the server sent a partial pkt-line */
				git_error_set(GIT_ERROR_NET, "incomplete pack data pkt-line");
				error = GIT_ERROR;
			}
			goto done;
		}

		if (error < 0) {
			goto done;
		}
	}
done:
	git_buf_dispose(&data_pkt_buf);
	return error;
}

static int add_ref_from_push_spec(git_vector *refs, push_spec *push_spec)
{
	git_pkt_ref *added = git__calloc(1, sizeof(git_pkt_ref));
	GIT_ERROR_CHECK_ALLOC(added);

	added->type = GIT_PKT_REF;
	git_oid_cpy(&added->head.oid, &push_spec->loid);
	added->head.name = git__strdup(push_spec->refspec.dst);

	if (!added->head.name ||
		git_vector_insert(refs, added) < 0) {
		git_pkt_free((git_pkt *)added);
		return -1;
	}

	return 0;
}

static int update_refs_from_report(
	git_vector *refs,
	git_vector *push_specs,
	git_vector *push_report)
{
	git_pkt_ref *ref;
	push_spec *push_spec;
	push_status *push_status;
	size_t i, j, refs_len;
	int cmp;

	/* For each push spec we sent to the server, we should have
	 * gotten back a status packet in the push report */
	if (push_specs->length != push_report->length) {
		git_error_set(GIT_ERROR_NET, "report-status: protocol error");
		return -1;
	}

	/* We require that push_specs be sorted with push_spec_rref_cmp,
	 * and that push_report be sorted with push_status_ref_cmp */
	git_vector_sort(push_specs);
	git_vector_sort(push_report);

	git_vector_foreach(push_specs, i, push_spec) {
		push_status = git_vector_get(push_report, i);

		/* For each push spec we sent to the server, we should have
		 * gotten back a status packet in the push report which matches */
		if (strcmp(push_spec->refspec.dst, push_status->ref)) {
			git_error_set(GIT_ERROR_NET, "report-status: protocol error");
			return -1;
		}
	}

	/* We require that refs be sorted with ref_name_cmp */
	git_vector_sort(refs);
	i = j = 0;
	refs_len = refs->length;

	/* Merge join push_specs with refs */
	while (i < push_specs->length && j < refs_len) {
		push_spec = git_vector_get(push_specs, i);
		push_status = git_vector_get(push_report, i);
		ref = git_vector_get(refs, j);

		cmp = strcmp(push_spec->refspec.dst, ref->head.name);

		/* Iterate appropriately */
		if (cmp <= 0) i++;
		if (cmp >= 0) j++;

		/* Add case */
		if (cmp < 0 &&
			!push_status->msg &&
			add_ref_from_push_spec(refs, push_spec) < 0)
			return -1;

		/* Update case, delete case */
		if (cmp == 0 &&
			!push_status->msg)
			git_oid_cpy(&ref->head.oid, &push_spec->loid);
	}

	for (; i < push_specs->length; i++) {
		push_spec = git_vector_get(push_specs, i);
		push_status = git_vector_get(push_report, i);

		/* Add case */
		if (!push_status->msg &&
			add_ref_from_push_spec(refs, push_spec) < 0)
			return -1;
	}

	/* Remove any refs which we updated to have a zero OID. */
	git_vector_rforeach(refs, i, ref) {
		if (git_oid_is_zero(&ref->head.oid)) {
			git_vector_remove(refs, i);
			git_pkt_free((git_pkt *)ref);
		}
	}

	git_vector_sort(refs);

	return 0;
}

struct push_packbuilder_payload
{
	git_smart_subtransport_stream *stream;
	git_packbuilder *pb;
	git_push_transfer_progress_cb cb;
	void *cb_payload;
	size_t last_bytes;
	double last_progress_report_time;
};

static int stream_thunk(void *buf, size_t size, void *data)
{
	int error = 0;
	struct push_packbuilder_payload *payload = data;

	if ((error = payload->stream->write(payload->stream, (const char *)buf, size)) < 0)
		return error;

	if (payload->cb) {
		double current_time = git__timer();
		double elapsed = current_time - payload->last_progress_report_time;
		payload->last_bytes += size;

		if (elapsed < 0 || elapsed >= MIN_PROGRESS_UPDATE_INTERVAL) {
			payload->last_progress_report_time = current_time;
			error = payload->cb(payload->pb->nr_written, payload->pb->nr_objects, payload->last_bytes, payload->cb_payload);
		}
	}

	return error;
}

int git_smart__push(git_transport *transport, git_push *push, const git_remote_callbacks *cbs)
{
	transport_smart *t = (transport_smart *)transport;
	struct push_packbuilder_payload packbuilder_payload = {0};
	git_buf pktline = GIT_BUF_INIT;
	int error = 0, need_pack = 0;
	push_spec *spec;
	unsigned int i;

	packbuilder_payload.pb = push->pb;

	if (cbs && cbs->push_transfer_progress) {
		packbuilder_payload.cb = cbs->push_transfer_progress;
		packbuilder_payload.cb_payload = cbs->payload;
	}

#ifdef PUSH_DEBUG
{
	git_remote_head *head;
	char hex[GIT_OID_HEXSZ+1]; hex[GIT_OID_HEXSZ] = '\0';

	git_vector_foreach(&push->remote->refs, i, head) {
		git_oid_fmt(hex, &head->oid);
		fprintf(stderr, "%s (%s)\n", hex, head->name);
	}

	git_vector_foreach(&push->specs, i, spec) {
		git_oid_fmt(hex, &spec->roid);
		fprintf(stderr, "%s (%s) -> ", hex, spec->lref);
		git_oid_fmt(hex, &spec->loid);
		fprintf(stderr, "%s (%s)\n", hex, spec->rref ?
			spec->rref : spec->lref);
	}
}
#endif

	/*
	 * Figure out if we need to send a packfile; which is in all
	 * cases except when we only send delete commands
	 */
	git_vector_foreach(&push->specs, i, spec) {
		if (spec->refspec.src && spec->refspec.src[0] != '\0') {
			need_pack = 1;
			break;
		}
	}

	if ((error = git_smart__get_push_stream(t, &packbuilder_payload.stream)) < 0 ||
		(error = gen_pktline(&pktline, push)) < 0 ||
		(error = packbuilder_payload.stream->write(packbuilder_payload.stream, git_buf_cstr(&pktline), git_buf_len(&pktline))) < 0)
		goto done;

	if (need_pack &&
		(error = git_packbuilder_foreach(push->pb, &stream_thunk, &packbuilder_payload)) < 0)
		goto done;

	/* If we sent nothing or the server doesn't support report-status, then
	 * we consider the pack to have been unpacked successfully */
	if (!push->specs.length || !push->report_status)
		push->unpack_ok = 1;
	else if ((error = parse_report(t, push)) < 0)
		goto done;

	/* If progress is being reported write the final report */
	if (cbs && cbs->push_transfer_progress) {
		error = cbs->push_transfer_progress(
					push->pb->nr_written,
					push->pb->nr_objects,
					packbuilder_payload.last_bytes,
					cbs->payload);

		if (error < 0)
			goto done;
	}

	if (push->status.length) {
		error = update_refs_from_report(&t->refs, &push->specs, &push->status);
		if (error < 0)
			goto done;

		error = git_smart__update_heads(t, NULL);
	}

done:
	git_buf_dispose(&pktline);
	return error;
}
