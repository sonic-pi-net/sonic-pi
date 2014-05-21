/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2.h"
#include "buffer.h"
#include "netops.h"
#include "smart.h"

#ifdef GIT_SSH

#include <libssh2.h>

#define OWNING_SUBTRANSPORT(s) ((ssh_subtransport *)(s)->parent.subtransport)

static const char prefix_ssh[] = "ssh://";
static const char cmd_uploadpack[] = "git-upload-pack";
static const char cmd_receivepack[] = "git-receive-pack";

typedef struct {
	git_smart_subtransport_stream parent;
	gitno_socket socket;
	LIBSSH2_SESSION *session;
	LIBSSH2_CHANNEL *channel;
	const char *cmd;
	char *url;
	unsigned sent_command : 1;
} ssh_stream;

typedef struct {
	git_smart_subtransport parent;
	transport_smart *owner;
	ssh_stream *current_stream;
	git_cred *cred;
} ssh_subtransport;

static void ssh_error(LIBSSH2_SESSION *session, const char *errmsg)
{
	char *ssherr;
	libssh2_session_last_error(session, &ssherr, NULL, 0);

	giterr_set(GITERR_SSH, "%s: %s", errmsg, ssherr);
}

/*
 * Create a git protocol request.
 *
 * For example: git-upload-pack '/libgit2/libgit2'
 */
static int gen_proto(git_buf *request, const char *cmd, const char *url)
{
	char *repo;
	int len;

	if (!git__prefixcmp(url, prefix_ssh)) {
		url = url + strlen(prefix_ssh);
		repo = strchr(url, '/');
	} else {
		repo = strchr(url, ':');
		if (repo) repo++;
	}

	if (!repo) {
		giterr_set(GITERR_NET, "Malformed git protocol URL");
		return -1;
	}

	len = strlen(cmd) + 1 /* Space */ + 1 /* Quote */ + strlen(repo) + 1 /* Quote */ + 1;

	git_buf_grow(request, len);
	git_buf_printf(request, "%s '%s'", cmd, repo);
	git_buf_putc(request, '\0');

	if (git_buf_oom(request))
		return -1;

	return 0;
}

static int send_command(ssh_stream *s)
{
	int error;
	git_buf request = GIT_BUF_INIT;

	error = gen_proto(&request, s->cmd, s->url);
	if (error < 0)
		goto cleanup;

	error = libssh2_channel_exec(s->channel, request.ptr);
	if (error < LIBSSH2_ERROR_NONE) {
		ssh_error(s->session, "SSH could not execute request");
		goto cleanup;
	}

	s->sent_command = 1;

cleanup:
	git_buf_free(&request);
	return error;
}

static int ssh_stream_read(
	git_smart_subtransport_stream *stream,
	char *buffer,
	size_t buf_size,
	size_t *bytes_read)
{
	int rc;
	ssh_stream *s = (ssh_stream *)stream;

	*bytes_read = 0;

	if (!s->sent_command && send_command(s) < 0)
		return -1;

	if ((rc = libssh2_channel_read(s->channel, buffer, buf_size)) < LIBSSH2_ERROR_NONE) {
		ssh_error(s->session, "SSH could not read data");;
		return -1;
	}

	*bytes_read = rc;

	return 0;
}

static int ssh_stream_write(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	ssh_stream *s = (ssh_stream *)stream;

	if (!s->sent_command && send_command(s) < 0)
		return -1;

	if (libssh2_channel_write(s->channel, buffer, len) < LIBSSH2_ERROR_NONE) {
		ssh_error(s->session, "SSH could not write data");
		return -1;
	}

	return 0;
}

static void ssh_stream_free(git_smart_subtransport_stream *stream)
{
	ssh_stream *s = (ssh_stream *)stream;
	ssh_subtransport *t = OWNING_SUBTRANSPORT(s);
	int ret;

	GIT_UNUSED(ret);

	t->current_stream = NULL;

	if (s->channel) {
		libssh2_channel_close(s->channel);
		libssh2_channel_free(s->channel);
		s->channel = NULL;
	}

	if (s->session) {
		libssh2_session_free(s->session);
		s->session = NULL;
	}

	if (s->socket.socket) {
		(void)gitno_close(&s->socket);
		/* can't do anything here with error return value */
	}

	git__free(s->url);
	git__free(s);
}

static int ssh_stream_alloc(
	ssh_subtransport *t,
	const char *url,
	const char *cmd,
	git_smart_subtransport_stream **stream)
{
	ssh_stream *s;

	assert(stream);

	s = git__calloc(sizeof(ssh_stream), 1);
	GITERR_CHECK_ALLOC(s);

	s->parent.subtransport = &t->parent;
	s->parent.read = ssh_stream_read;
	s->parent.write = ssh_stream_write;
	s->parent.free = ssh_stream_free;

	s->cmd = cmd;

	s->url = git__strdup(url);
	if (!s->url) {
		git__free(s);
		return -1;
	}

	*stream = &s->parent;
	return 0;
}

static int git_ssh_extract_url_parts(
	char **host,
	char **username,
	const char *url)
{
	char *colon, *at;
	const char *start;

	colon = strchr(url, ':');


	at = strchr(url, '@');
	if (at) {
		start = at + 1;
		*username = git__substrdup(url, at - url);
		GITERR_CHECK_ALLOC(*username);
	} else {
		start = url;
		*username = NULL;
	}

	if (colon == NULL || (colon < start)) {
		giterr_set(GITERR_NET, "Malformed URL");
		return -1;
	}

	*host = git__substrdup(start, colon - start);
	GITERR_CHECK_ALLOC(*host);

	return 0;
}

static int ssh_agent_auth(LIBSSH2_SESSION *session, git_cred_ssh_key *c) {
	int rc = LIBSSH2_ERROR_NONE;

	struct libssh2_agent_publickey *curr, *prev = NULL;

	LIBSSH2_AGENT *agent = libssh2_agent_init(session);

	if (agent == NULL)
		return -1;

	rc = libssh2_agent_connect(agent);

	if (rc != LIBSSH2_ERROR_NONE)
		goto shutdown;

	rc = libssh2_agent_list_identities(agent);

	if (rc != LIBSSH2_ERROR_NONE)
		goto shutdown;

	while (1) {
		rc = libssh2_agent_get_identity(agent, &curr, prev);

		if (rc < 0)
			goto shutdown;

		if (rc == 1)
			goto shutdown;

		rc = libssh2_agent_userauth(agent, c->username, curr);

		if (rc == 0)
			break;

		prev = curr;
	}

shutdown:
	libssh2_agent_disconnect(agent);
	libssh2_agent_free(agent);

	return rc;
}

static int _git_ssh_authenticate_session(
	LIBSSH2_SESSION* session,
	git_cred* cred)
{
	int rc;

	do {
		switch (cred->credtype) {
		case GIT_CREDTYPE_USERPASS_PLAINTEXT: {
			git_cred_userpass_plaintext *c = (git_cred_userpass_plaintext *)cred;
			rc = libssh2_userauth_password(session, c->username, c->password);
			break;
		}
		case GIT_CREDTYPE_SSH_KEY: {
			git_cred_ssh_key *c = (git_cred_ssh_key *)cred;

			if (c->privatekey)
				rc = libssh2_userauth_publickey_fromfile(
					session, c->username, c->publickey,
					c->privatekey, c->passphrase);
			else
				rc = ssh_agent_auth(session, c);

			break;
		}
		case GIT_CREDTYPE_SSH_CUSTOM: {
			git_cred_ssh_custom *c = (git_cred_ssh_custom *)cred;

			rc = libssh2_userauth_publickey(
				session, c->username, (const unsigned char *)c->publickey,
				c->publickey_len, c->sign_callback, &c->payload);
			break;
		}
		case GIT_CREDTYPE_SSH_INTERACTIVE: {
			void **abstract = libssh2_session_abstract(session);
			git_cred_ssh_interactive *c = (git_cred_ssh_interactive *)cred;

			/* ideally, we should be able to set this by calling
			 * libssh2_session_init_ex() instead of libssh2_session_init().
			 * libssh2's API is inconsistent here i.e. libssh2_userauth_publickey()
			 * allows you to pass the `abstract` as part of the call, whereas
			 * libssh2_userauth_keyboard_interactive() does not!
			 *
			 * The only way to set the `abstract` pointer is by calling
			 * libssh2_session_abstract(), which will replace the existing
			 * pointer as is done below. This is safe for now (at time of writing),
			 * but may not be valid in future.
			 */
			*abstract = c->payload;

			rc = libssh2_userauth_keyboard_interactive(
				session, c->username, c->prompt_callback);
			break;
		}
		default:
			rc = LIBSSH2_ERROR_AUTHENTICATION_FAILED;
		}
	} while (LIBSSH2_ERROR_EAGAIN == rc || LIBSSH2_ERROR_TIMEOUT == rc);

	if (rc != LIBSSH2_ERROR_NONE) {
		ssh_error(session, "Failed to authenticate SSH session");
		return -1;
	}

	return 0;
}

static int _git_ssh_session_create(
	LIBSSH2_SESSION** session,
	gitno_socket socket)
{
	int rc = 0;
	LIBSSH2_SESSION* s;

	assert(session);

	s = libssh2_session_init();
	if (!s) {
		giterr_set(GITERR_NET, "Failed to initialize SSH session");
		return -1;
	}

	do {
		rc = libssh2_session_startup(s, socket.socket);
	} while (LIBSSH2_ERROR_EAGAIN == rc || LIBSSH2_ERROR_TIMEOUT == rc);

	if (rc != LIBSSH2_ERROR_NONE) {
		ssh_error(s, "Failed to start SSH session");
		libssh2_session_free(s);
		return -1;
	}

	libssh2_session_set_blocking(s, 1);

	*session = s;

	return 0;
}

static int _git_ssh_setup_conn(
	ssh_subtransport *t,
	const char *url,
	const char *cmd,
	git_smart_subtransport_stream **stream)
{
	char *host=NULL, *port=NULL, *path=NULL, *user=NULL, *pass=NULL;
	const char *default_port="22";
	int no_callback = 0;
	ssh_stream *s;
	LIBSSH2_SESSION* session=NULL;
	LIBSSH2_CHANNEL* channel=NULL;

	*stream = NULL;
	if (ssh_stream_alloc(t, url, cmd, stream) < 0)
		return -1;

	s = (ssh_stream *)*stream;

	if (!git__prefixcmp(url, prefix_ssh)) {
		if (gitno_extract_url_parts(&host, &port, &path, &user, &pass, url, default_port) < 0)
			goto on_error;
	} else {
		if (git_ssh_extract_url_parts(&host, &user, url) < 0)
			goto on_error;
		port = git__strdup(default_port);
		GITERR_CHECK_ALLOC(port);
	}

	if (gitno_connect(&s->socket, host, port, 0) < 0)
		goto on_error;

	if (user && pass) {
		if (git_cred_userpass_plaintext_new(&t->cred, user, pass) < 0)
			goto on_error;
	} else if (!t->owner->cred_acquire_cb) {
		no_callback = 1;
	} else {
		int error;
		error = t->owner->cred_acquire_cb(&t->cred, t->owner->url, user,
			GIT_CREDTYPE_USERPASS_PLAINTEXT |
			GIT_CREDTYPE_SSH_KEY | GIT_CREDTYPE_SSH_CUSTOM |
			GIT_CREDTYPE_SSH_INTERACTIVE,
			t->owner->cred_acquire_payload);

		if (error == GIT_PASSTHROUGH)
			no_callback = 1;
		else if (error < 0)
			goto on_error;
		else if (!t->cred) {
			giterr_set(GITERR_SSH, "Callback failed to initialize SSH credentials");
			goto on_error;
		}
	}

	if (no_callback) {
		giterr_set(GITERR_SSH, "authentication required but no callback set");
		goto on_error;
	}

	assert(t->cred);

	if (_git_ssh_session_create(&session, s->socket) < 0)
		goto on_error;

	if (_git_ssh_authenticate_session(session, t->cred) < 0)
		goto on_error;

	channel = libssh2_channel_open_session(session);
	if (!channel) {
		ssh_error(session, "Failed to open SSH channel");
		goto on_error;
	}

	libssh2_channel_set_blocking(channel, 1);

	s->session = session;
	s->channel = channel;

	t->current_stream = s;
	git__free(host);
	git__free(port);
	git__free(path);
	git__free(user);
	git__free(pass);

	return 0;

on_error:
	s->session = NULL;
	s->channel = NULL;
	t->current_stream = NULL;

	if (*stream)
		ssh_stream_free(*stream);

	git__free(host);
	git__free(port);
	git__free(user);
	git__free(pass);

	if (session)
		libssh2_session_free(session);

	return -1;
}

static int ssh_uploadpack_ls(
	ssh_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	if (_git_ssh_setup_conn(t, url, cmd_uploadpack, stream) < 0)
		return -1;

	return 0;
}

static int ssh_uploadpack(
	ssh_subtransport *t,
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

static int ssh_receivepack_ls(
	ssh_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	if (_git_ssh_setup_conn(t, url, cmd_receivepack, stream) < 0)
		return -1;

	return 0;
}

static int ssh_receivepack(
	ssh_subtransport *t,
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

static int _ssh_action(
	git_smart_subtransport_stream **stream,
	git_smart_subtransport *subtransport,
	const char *url,
	git_smart_service_t action)
{
	ssh_subtransport *t = (ssh_subtransport *) subtransport;

	switch (action) {
		case GIT_SERVICE_UPLOADPACK_LS:
			return ssh_uploadpack_ls(t, url, stream);

		case GIT_SERVICE_UPLOADPACK:
			return ssh_uploadpack(t, url, stream);

		case GIT_SERVICE_RECEIVEPACK_LS:
			return ssh_receivepack_ls(t, url, stream);

		case GIT_SERVICE_RECEIVEPACK:
			return ssh_receivepack(t, url, stream);
	}

	*stream = NULL;
	return -1;
}

static int _ssh_close(git_smart_subtransport *subtransport)
{
	ssh_subtransport *t = (ssh_subtransport *) subtransport;

	assert(!t->current_stream);

	GIT_UNUSED(t);

	return 0;
}

static void _ssh_free(git_smart_subtransport *subtransport)
{
	ssh_subtransport *t = (ssh_subtransport *) subtransport;

	assert(!t->current_stream);

	git__free(t);
}
#endif

int git_smart_subtransport_ssh(
	git_smart_subtransport **out, git_transport *owner)
{
#ifdef GIT_SSH
	ssh_subtransport *t;

	assert(out);

	t = git__calloc(sizeof(ssh_subtransport), 1);
	GITERR_CHECK_ALLOC(t);

	t->owner = (transport_smart *)owner;
	t->parent.action = _ssh_action;
	t->parent.close = _ssh_close;
	t->parent.free = _ssh_free;

	*out = (git_smart_subtransport *) t;
	return 0;
#else
	GIT_UNUSED(owner);

	assert(out);
	*out = NULL;

	giterr_set(GITERR_INVALID, "Cannot create SSH transport. Library was built without SSH support");
	return -1;
#endif
}
