/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2.h"
#include "buffer.h"
#include "netops.h"

#define OWNING_SUBTRANSPORT(s) ((git_subtransport *)(s)->parent.subtransport)

static const char prefix_git[] = "git://";
static const char cmd_uploadpack[] = "git-upload-pack";
static const char cmd_receivepack[] = "git-receive-pack";

typedef struct {
	git_smart_subtransport_stream parent;
	gitno_socket socket;
	const char *cmd;
	char *url;
	unsigned sent_command : 1;
} git_stream;

typedef struct {
	git_smart_subtransport parent;
	git_transport *owner;
	git_stream *current_stream;
} git_subtransport;

/*
 * Create a git protocol request.
 *
 * For example: 0035git-upload-pack /libgit2/libgit2\0host=github.com\0
 */
static int gen_proto(git_buf *request, const char *cmd, const char *url)
{
	char *delim, *repo;
	char host[] = "host=";
	size_t len;

	delim = strchr(url, '/');
	if (delim == NULL) {
		giterr_set(GITERR_NET, "Malformed URL");
		return -1;
	}

	repo = delim;

	delim = strchr(url, ':');
	if (delim == NULL)
		delim = strchr(url, '/');

	len = 4 + strlen(cmd) + 1 + strlen(repo) + 1 + strlen(host) + (delim - url) + 1;

	git_buf_grow(request, len);
	git_buf_printf(request, "%04x%s %s%c%s",
		(unsigned int)(len & 0x0FFFF), cmd, repo, 0, host);
	git_buf_put(request, url, delim - url);
	git_buf_putc(request, '\0');

	if (git_buf_oom(request))
		return -1;

	return 0;
}

static int send_command(git_stream *s)
{
	int error;
	git_buf request = GIT_BUF_INIT;

	error = gen_proto(&request, s->cmd, s->url);
	if (error < 0)
		goto cleanup;

	/* It looks like negative values are errors here, and positive values
	 * are the number of bytes sent. */
	error = gitno_send(&s->socket, request.ptr, request.size, 0);

	if (error >= 0)
		s->sent_command = 1;

cleanup:
	git_buf_free(&request);
	return error;
}

static int git_stream_read(
	git_smart_subtransport_stream *stream,
	char *buffer,
	size_t buf_size,
	size_t *bytes_read)
{
	int error;
	git_stream *s = (git_stream *)stream;
	gitno_buffer buf;

	*bytes_read = 0;

	if (!s->sent_command && (error = send_command(s)) < 0)
		return error;

	gitno_buffer_setup(&s->socket, &buf, buffer, buf_size);

	if ((error = gitno_recv(&buf)) < 0)
		return error;

	*bytes_read = buf.offset;

	return 0;
}

static int git_stream_write(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	int error;
	git_stream *s = (git_stream *)stream;

	if (!s->sent_command && (error = send_command(s)) < 0)
		return error;

	return gitno_send(&s->socket, buffer, len, 0);
}

static void git_stream_free(git_smart_subtransport_stream *stream)
{
	git_stream *s = (git_stream *)stream;
	git_subtransport *t = OWNING_SUBTRANSPORT(s);
	int ret;

	GIT_UNUSED(ret);

	t->current_stream = NULL;

	if (s->socket.socket) {
		ret = gitno_close(&s->socket);
		assert(!ret);
	}

	git__free(s->url);
	git__free(s);
}

static int git_stream_alloc(
	git_subtransport *t,
	const char *url,
	const char *cmd,
	git_smart_subtransport_stream **stream)
{
	git_stream *s;

	if (!stream)
		return -1;

	s = git__calloc(sizeof(git_stream), 1);
	GITERR_CHECK_ALLOC(s);

	s->parent.subtransport = &t->parent;
	s->parent.read = git_stream_read;
	s->parent.write = git_stream_write;
	s->parent.free = git_stream_free;

	s->cmd = cmd;
	s->url = git__strdup(url);

	if (!s->url) {
		git__free(s);
		return -1;
	}

	*stream = &s->parent;
	return 0;
}

static int _git_uploadpack_ls(
	git_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	char *host=NULL, *port=NULL, *path=NULL, *user=NULL, *pass=NULL;
	const char *stream_url = url;
	git_stream *s;
	int error;

	*stream = NULL;

	if (!git__prefixcmp(url, prefix_git))
		stream_url += strlen(prefix_git);

	if ((error = git_stream_alloc(t, stream_url, cmd_uploadpack, stream)) < 0)
		return error;

	s = (git_stream *)*stream;

	if (!(error = gitno_extract_url_parts(
			&host, &port, &path, &user, &pass, url, GIT_DEFAULT_PORT))) {

		if (!(error = gitno_connect(&s->socket, host, port, 0)))
			t->current_stream = s;

		git__free(host);
		git__free(port);
		git__free(path);
		git__free(user);
		git__free(pass);
	} else if (*stream)
		git_stream_free(*stream);

	return error;
}

static int _git_uploadpack(
	git_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	GIT_UNUSED(url);

	if (t->current_stream) {
		*stream = &t->current_stream->parent;
		return 0;
	}

	giterr_set(GITERR_NET, "Must call UPLOADPACK_LS before UPLOADPACK");
	return -1;
}

static int _git_receivepack_ls(
	git_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	char *host=NULL, *port=NULL, *path=NULL, *user=NULL, *pass=NULL;
	const char *stream_url = url;
	git_stream *s;
	int error;

	*stream = NULL;
	if (!git__prefixcmp(url, prefix_git))
		stream_url += strlen(prefix_git);

	if (git_stream_alloc(t, stream_url, cmd_receivepack, stream) < 0)
		return -1;

	s = (git_stream *)*stream;

	if (!(error = gitno_extract_url_parts(&host, &port, &path, &user, &pass, url, GIT_DEFAULT_PORT))) {
		if (!(error = gitno_connect(&s->socket, host, port, 0)))
			t->current_stream = s;

		git__free(host);
		git__free(port);
		git__free(path);
		git__free(user);
		git__free(pass);
	} else if (*stream)
		git_stream_free(*stream);

	return error;
}

static int _git_receivepack(
	git_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	GIT_UNUSED(url);

	if (t->current_stream) {
		*stream = &t->current_stream->parent;
		return 0;
	}

	giterr_set(GITERR_NET, "Must call RECEIVEPACK_LS before RECEIVEPACK");
	return -1;
}

static int _git_action(
	git_smart_subtransport_stream **stream,
	git_smart_subtransport *subtransport,
	const char *url,
	git_smart_service_t action)
{
	git_subtransport *t = (git_subtransport *) subtransport;

	switch (action) {
		case GIT_SERVICE_UPLOADPACK_LS:
			return _git_uploadpack_ls(t, url, stream);

		case GIT_SERVICE_UPLOADPACK:
			return _git_uploadpack(t, url, stream);

		case GIT_SERVICE_RECEIVEPACK_LS:
			return _git_receivepack_ls(t, url, stream);

		case GIT_SERVICE_RECEIVEPACK:
			return _git_receivepack(t, url, stream);
	}

	*stream = NULL;
	return -1;
}

static int _git_close(git_smart_subtransport *subtransport)
{
	git_subtransport *t = (git_subtransport *) subtransport;

	assert(!t->current_stream);

	GIT_UNUSED(t);

	return 0;
}

static void _git_free(git_smart_subtransport *subtransport)
{
	git_subtransport *t = (git_subtransport *) subtransport;

	assert(!t->current_stream);

	git__free(t);
}

int git_smart_subtransport_git(git_smart_subtransport **out, git_transport *owner)
{
	git_subtransport *t;

	if (!out)
		return -1;

	t = git__calloc(sizeof(git_subtransport), 1);
	GITERR_CHECK_ALLOC(t);

	t->owner = owner;
	t->parent.action = _git_action;
	t->parent.close = _git_close;
	t->parent.free = _git_free;

	*out = (git_smart_subtransport *) t;
	return 0;
}
