/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2/config.h"
#include "git2/types.h"
#include "git2/oid.h"
#include "git2/net.h"

#include "common.h"
#include "config.h"
#include "repository.h"
#include "remote.h"
#include "fetch.h"
#include "refs.h"
#include "refspec.h"
#include "fetchhead.h"

static int dwim_refspecs(git_vector *out, git_vector *refspecs, git_vector *refs);

static int add_refspec_to(git_vector *vector, const char *string, bool is_fetch)
{
	git_refspec *spec;

	spec = git__calloc(1, sizeof(git_refspec));
	GITERR_CHECK_ALLOC(spec);

	if (git_refspec__parse(spec, string, is_fetch) < 0) {
		git__free(spec);
		return -1;
	}

	spec->push = !is_fetch;
	if (git_vector_insert(vector, spec) < 0) {
		git_refspec__free(spec);
		git__free(spec);
		return -1;
	}

	return 0;
}

static int add_refspec(git_remote *remote, const char *string, bool is_fetch)
{
	return add_refspec_to(&remote->refspecs, string, is_fetch);
}

static int download_tags_value(git_remote *remote, git_config *cfg)
{
	const git_config_entry *ce;
	git_buf buf = GIT_BUF_INIT;
	int error;

	/* The 0 value is the default (auto), let's see if we need to change it */
	if (git_buf_printf(&buf, "remote.%s.tagopt", remote->name) < 0)
		return -1;

	error = git_config__lookup_entry(&ce, cfg, git_buf_cstr(&buf), false);
	git_buf_free(&buf);

	if (!error && ce && ce->value) {
		if (!strcmp(ce->value, "--no-tags"))
			remote->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_NONE;
		else if (!strcmp(ce->value, "--tags"))
			remote->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_ALL;
	}

	return error;
}

static int ensure_remote_name_is_valid(const char *name)
{
	int error = 0;

	if (!git_remote_is_valid_name(name)) {
		giterr_set(
			GITERR_CONFIG,
			"'%s' is not a valid remote name.", name ? name : "(null)");
		error = GIT_EINVALIDSPEC;
	}

	return error;
}

#if 0
/* We could export this as a helper */
static int get_check_cert(int *out, git_repository *repo)
{
	git_config *cfg;
	const char *val;
	int error = 0;

	assert(out && repo);

	/* By default, we *DO* want to verify the certificate. */
	*out = 1;

	/* Go through the possible sources for SSL verification settings, from
	 * most specific to least specific. */

	/* GIT_SSL_NO_VERIFY environment variable */
	if ((val = getenv("GIT_SSL_NO_VERIFY")) != NULL)
		return git_config_parse_bool(out, val);

	/* http.sslVerify config setting */
	if ((error = git_repository_config__weakptr(&cfg, repo)) < 0)
		return error;

	*out = git_config__get_bool_force(cfg, "http.sslverify", 1);
	return 0;
}
#endif

static int canonicalize_url(git_buf *out, const char *in)
{
#ifdef GIT_WIN32
	const char *c;

	/* Given a UNC path like \\server\path, we need to convert this
	 * to //server/path for compatibility with core git.
	 */
	if (in[0] == '\\' && in[1] == '\\' &&
		(git__isalpha(in[2]) || git__isdigit(in[2]))) {
		for (c = in; *c; c++)
			git_buf_putc(out, *c == '\\' ? '/' : *c);

		return git_buf_oom(out) ? -1 : 0;
	}
#endif

	return git_buf_puts(out, in);
}

static int create_internal(git_remote **out, git_repository *repo, const char *name, const char *url, const char *fetch)
{
	git_remote *remote;
	git_buf canonical_url = GIT_BUF_INIT, fetchbuf = GIT_BUF_INIT;
	int error = -1;

	/* name is optional */
	assert(out && repo && url);

	remote = git__calloc(1, sizeof(git_remote));
	GITERR_CHECK_ALLOC(remote);

	remote->repo = repo;
	remote->update_fetchhead = 1;

	if (git_vector_init(&remote->refs, 32, NULL) < 0 ||
		canonicalize_url(&canonical_url, url) < 0)
		goto on_error;

	remote->url = git_buf_detach(&canonical_url);

	if (name != NULL) {
		remote->name = git__strdup(name);
		GITERR_CHECK_ALLOC(remote->name);
	}

	if (fetch != NULL) {
		if (add_refspec(remote, fetch, true) < 0)
			goto on_error;

		/* Move the data over to where the matching functions can find them */
		if (dwim_refspecs(&remote->active_refspecs, &remote->refspecs, &remote->refs) < 0)
			goto on_error;
	}

	if (!name)
		/* A remote without a name doesn't download tags */
		remote->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_NONE;

	*out = remote;
	git_buf_free(&fetchbuf);
	git_buf_free(&canonical_url);
	return 0;

on_error:
	git_remote_free(remote);
	git_buf_free(&fetchbuf);
	git_buf_free(&canonical_url);
	return error;
}

static int ensure_remote_doesnot_exist(git_repository *repo, const char *name)
{
	int error;
	git_remote *remote;

	error = git_remote_lookup(&remote, repo, name);

	if (error == GIT_ENOTFOUND)
		return 0;

	if (error < 0)
		return error;

	git_remote_free(remote);

	giterr_set(
		GITERR_CONFIG,
		"Remote '%s' already exists.", name);

	return GIT_EEXISTS;
}


int git_remote_create(git_remote **out, git_repository *repo, const char *name, const char *url)
{
	git_buf buf = GIT_BUF_INIT;
	int error;

	if (git_buf_printf(&buf, "+refs/heads/*:refs/remotes/%s/*", name) < 0)
		return -1;

	error = git_remote_create_with_fetchspec(out, repo, name, url, git_buf_cstr(&buf));
	git_buf_free(&buf);

	return error;
}

int git_remote_create_with_fetchspec(git_remote **out, git_repository *repo, const char *name, const char *url, const char *fetch)
{
	git_remote *remote = NULL;
	int error;

	if ((error = ensure_remote_name_is_valid(name)) < 0)
		return error;

	if ((error = ensure_remote_doesnot_exist(repo, name)) < 0)
		return error;

	if (create_internal(&remote, repo, name, url, fetch) < 0)
		goto on_error;

	if (git_remote_save(remote) < 0)
		goto on_error;

	*out = remote;

	return 0;

on_error:
	git_remote_free(remote);
	return -1;
}

int git_remote_create_anonymous(git_remote **out, git_repository *repo, const char *url, const char *fetch)
{
	int error;
	git_remote *remote;

	if ((error = create_internal(&remote, repo, NULL, url, fetch)) < 0)
		return error;

	*out = remote;
	return 0;
}

int git_remote_dup(git_remote **dest, git_remote *source)
{
	int error = 0;
	git_strarray refspecs = { 0 };
	git_remote *remote = git__calloc(1, sizeof(git_remote));
	GITERR_CHECK_ALLOC(remote);

	if (source->name != NULL) {
		remote->name = git__strdup(source->name);
		GITERR_CHECK_ALLOC(remote->name);
	}

	if (source->url != NULL) {
		remote->url = git__strdup(source->url);
		GITERR_CHECK_ALLOC(remote->url);		
	}

	if (source->pushurl != NULL) {
		remote->pushurl = git__strdup(source->pushurl);
		GITERR_CHECK_ALLOC(remote->pushurl);
	}

	remote->transport_cb = source->transport_cb;
	remote->transport_cb_payload = source->transport_cb_payload;
	remote->repo = source->repo;
	remote->download_tags = source->download_tags;
	remote->update_fetchhead = source->update_fetchhead;

	if (git_vector_init(&remote->refs, 32, NULL) < 0 ||
	    git_vector_init(&remote->refspecs, 2, NULL) < 0 ||
	    git_vector_init(&remote->active_refspecs, 2, NULL) < 0) {
		error = -1;
		goto cleanup;
	}

	if ((error = git_remote_get_fetch_refspecs(&refspecs, source)) < 0 ||
	    (error = git_remote_set_fetch_refspecs(remote, &refspecs)) < 0)
		goto cleanup;

	git_strarray_free(&refspecs);

	if ((error = git_remote_get_push_refspecs(&refspecs, source)) < 0 ||
	    (error = git_remote_set_push_refspecs(remote, &refspecs)) < 0)
		goto cleanup;

	*dest = remote;

cleanup:

	git_strarray_free(&refspecs);

	if (error < 0)
		git__free(remote);

	return error;
}

struct refspec_cb_data {
	git_remote *remote;
	int fetch;
};

static int refspec_cb(const git_config_entry *entry, void *payload)
{
	struct refspec_cb_data *data = (struct refspec_cb_data *)payload;
	return add_refspec(data->remote, entry->value, data->fetch);
}

static int get_optional_config(
	bool *found, git_config *config, git_buf *buf,
	git_config_foreach_cb cb, void *payload)
{
	int error = 0;
	const char *key = git_buf_cstr(buf);

	if (git_buf_oom(buf))
		return -1;

	if (cb != NULL)
		error = git_config_get_multivar_foreach(config, key, NULL, cb, payload);
	else
		error = git_config_get_string(payload, config, key);

	if (found)
		*found = !error;

	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		error = 0;
	}

	return error;
}

int git_remote_lookup(git_remote **out, git_repository *repo, const char *name)
{
	git_remote *remote;
	git_buf buf = GIT_BUF_INIT;
	const char *val;
	int error = 0;
	git_config *config;
	struct refspec_cb_data data = { NULL };
	bool optional_setting_found = false, found;

	assert(out && repo && name);

	if ((error = ensure_remote_name_is_valid(name)) < 0)
		return error;

	if ((error = git_repository_config_snapshot(&config, repo)) < 0)
		return error;

	remote = git__malloc(sizeof(git_remote));
	GITERR_CHECK_ALLOC(remote);

	memset(remote, 0x0, sizeof(git_remote));
	remote->update_fetchhead = 1;
	remote->name = git__strdup(name);
	GITERR_CHECK_ALLOC(remote->name);

	if (git_vector_init(&remote->refs, 32, NULL) < 0 ||
	    git_vector_init(&remote->refspecs, 2, NULL) < 0 ||
	    git_vector_init(&remote->passive_refspecs, 2, NULL) < 0 ||
	    git_vector_init(&remote->active_refspecs, 2, NULL) < 0) {
		error = -1;
		goto cleanup;
	}

	if ((error = git_buf_printf(&buf, "remote.%s.url", name)) < 0)
		goto cleanup;

	if ((error = get_optional_config(&found, config, &buf, NULL, (void *)&val)) < 0)
		goto cleanup;

	optional_setting_found |= found;

	remote->repo = repo;

	if (found && strlen(val) > 0) {
		remote->url = git__strdup(val);
		GITERR_CHECK_ALLOC(remote->url);
	}

	val = NULL;
	git_buf_clear(&buf);
	git_buf_printf(&buf, "remote.%s.pushurl", name);

	if ((error = get_optional_config(&found, config, &buf, NULL, (void *)&val)) < 0)
		goto cleanup;

	optional_setting_found |= found;

	if (!optional_setting_found) {
		error = GIT_ENOTFOUND;
		giterr_set(GITERR_CONFIG, "Remote '%s' does not exist.", name);
		goto cleanup;
	}

	if (found && strlen(val) > 0) {
		remote->pushurl = git__strdup(val);
		GITERR_CHECK_ALLOC(remote->pushurl);
	}

	data.remote = remote;
	data.fetch = true;

	git_buf_clear(&buf);
	git_buf_printf(&buf, "remote.%s.fetch", name);

	if ((error = get_optional_config(NULL, config, &buf, refspec_cb, &data)) < 0)
		goto cleanup;

	data.fetch = false;
	git_buf_clear(&buf);
	git_buf_printf(&buf, "remote.%s.push", name);

	if ((error = get_optional_config(NULL, config, &buf, refspec_cb, &data)) < 0)
		goto cleanup;

	if (download_tags_value(remote, config) < 0)
		goto cleanup;

	/* Move the data over to where the matching functions can find them */
	if (dwim_refspecs(&remote->active_refspecs, &remote->refspecs, &remote->refs) < 0)
		goto cleanup;

	*out = remote;

cleanup:
	git_config_free(config);
	git_buf_free(&buf);

	if (error < 0)
		git_remote_free(remote);

	return error;
}

static int update_config_refspec(const git_remote *remote, git_config *config, int direction)
{
	git_buf name = GIT_BUF_INIT;
	unsigned int push;
	const char *dir;
	size_t i;
	int error = 0;
	const char *cname;

	push = direction == GIT_DIRECTION_PUSH;
	dir = push ? "push" : "fetch";

	if (git_buf_printf(&name, "remote.%s.%s", remote->name, dir) < 0)
		return -1;
	cname = git_buf_cstr(&name);

	/* Clear out the existing config */
	while (!error)
		error = git_config_delete_multivar(config, cname, ".*");

	if (error != GIT_ENOTFOUND)
		return error;

	for (i = 0; i < remote->refspecs.length; i++) {
		git_refspec *spec = git_vector_get(&remote->refspecs, i);

		if (spec->push != push)
			continue;

		// "$^" is a unmatcheable regexp: it will not match anything at all, so
		// all values will be considered new and we will not replace any
		// present value.
		if ((error = git_config_set_multivar(
				config, cname, "$^", spec->string)) < 0) {
			goto cleanup;
		}
	}

	giterr_clear();
	error = 0;

cleanup:
	git_buf_free(&name);

	return error;
}

int git_remote_save(const git_remote *remote)
{
	int error;
	git_config *cfg;
	const char *tagopt = NULL;
	git_buf buf = GIT_BUF_INIT;
	const git_config_entry *existing;

	assert(remote);

	if (!remote->name) {
		giterr_set(GITERR_INVALID, "Can't save an anonymous remote.");
		return GIT_EINVALIDSPEC;
	}

	if ((error = ensure_remote_name_is_valid(remote->name)) < 0)
		return error;

	if ((error = git_repository_config__weakptr(&cfg, remote->repo)) < 0)
		return error;

	if ((error = git_buf_printf(&buf, "remote.%s.url", remote->name)) < 0)
		return error;

	/* after this point, buffer is allocated so end with cleanup */

	if ((error = git_config_set_string(
			cfg, git_buf_cstr(&buf), remote->url)) < 0)
		goto cleanup;

	git_buf_clear(&buf);
	if ((error = git_buf_printf(&buf, "remote.%s.pushurl", remote->name)) < 0)
		goto cleanup;

	if ((error = git_config__update_entry(
			cfg, git_buf_cstr(&buf), remote->pushurl, true, false)) < 0)
		goto cleanup;

	if ((error = update_config_refspec(remote, cfg, GIT_DIRECTION_FETCH)) < 0)
		goto cleanup;

	if ((error = update_config_refspec(remote, cfg, GIT_DIRECTION_PUSH)) < 0)
		goto cleanup;

	/*
	 * What action to take depends on the old and new values. This
	 * is describes by the table below. tagopt means whether the
	 * is already a value set in the config
	 *
	 *            AUTO     ALL or NONE
	 *         +-----------------------+
	 *  tagopt | remove  |     set     |
	 *         +---------+-------------|
	 * !tagopt | nothing |     set     |
	 *         +---------+-------------+
	 */

	git_buf_clear(&buf);
	if ((error = git_buf_printf(&buf, "remote.%s.tagopt", remote->name)) < 0)
		goto cleanup;

	if ((error = git_config__lookup_entry(
			&existing, cfg, git_buf_cstr(&buf), false)) < 0)
		goto cleanup;

	if (remote->download_tags == GIT_REMOTE_DOWNLOAD_TAGS_ALL)
		tagopt = "--tags";
	else if (remote->download_tags == GIT_REMOTE_DOWNLOAD_TAGS_NONE)
		tagopt = "--no-tags";
	else if (existing != NULL)
		tagopt = NULL;

	error = git_config__update_entry(
		cfg, git_buf_cstr(&buf), tagopt, true, false);

cleanup:
	git_buf_free(&buf);
	return error;
}

const char *git_remote_name(const git_remote *remote)
{
	assert(remote);
	return remote->name;
}

git_repository *git_remote_owner(const git_remote *remote)
{
	assert(remote);
	return remote->repo;
}

const char *git_remote_url(const git_remote *remote)
{
	assert(remote);
	return remote->url;
}

int git_remote_set_url(git_remote *remote, const char* url)
{
	assert(remote);
	assert(url);

	git__free(remote->url);
	remote->url = git__strdup(url);
	GITERR_CHECK_ALLOC(remote->url);

	return 0;
}

const char *git_remote_pushurl(const git_remote *remote)
{
	assert(remote);
	return remote->pushurl;
}

int git_remote_set_pushurl(git_remote *remote, const char* url)
{
	assert(remote);

	git__free(remote->pushurl);
	if (url) {
		remote->pushurl = git__strdup(url);
		GITERR_CHECK_ALLOC(remote->pushurl);
	} else {
		remote->pushurl = NULL;
	}
	return 0;
}

const char* git_remote__urlfordirection(git_remote *remote, int direction)
{
	assert(remote);

	assert(direction == GIT_DIRECTION_FETCH || direction == GIT_DIRECTION_PUSH);

	if (direction == GIT_DIRECTION_FETCH) {
		return remote->url;
	}

	if (direction == GIT_DIRECTION_PUSH) {
		return remote->pushurl ? remote->pushurl : remote->url;
	}

	return NULL;
}

int git_remote_connect(git_remote *remote, git_direction direction)
{
	git_transport *t;
	const char *url;
	int flags = GIT_TRANSPORTFLAGS_NONE;
	int error;

	assert(remote);

	t = remote->transport;

	url = git_remote__urlfordirection(remote, direction);
	if (url == NULL) {
		giterr_set(GITERR_INVALID,
			"Malformed remote '%s' - missing URL", remote->name);
		return -1;
	}

	/* If we don't have a transport object yet, and the caller specified a
	 * custom transport factory, use that */
	if (!t && remote->transport_cb &&
		(error = remote->transport_cb(&t, remote, remote->transport_cb_payload)) < 0)
		return error;

	/* If we still don't have a transport, then use the global
	 * transport registrations which map URI schemes to transport factories */
	if (!t && (error = git_transport_new(&t, remote, url)) < 0)
		return error;

	if (t->set_callbacks &&
	    (error = t->set_callbacks(t, remote->callbacks.sideband_progress, NULL, remote->callbacks.certificate_check, remote->callbacks.payload)) < 0)
		goto on_error;

	if ((error = t->connect(t, url, remote->callbacks.credentials, remote->callbacks.payload, direction, flags)) != 0)
		goto on_error;

	remote->transport = t;

	return 0;

on_error:
	t->free(t);

	if (t == remote->transport)
		remote->transport = NULL;

	return error;
}

int git_remote_ls(const git_remote_head ***out, size_t *size, git_remote *remote)
{
	assert(remote);

	if (!remote->transport) {
		giterr_set(GITERR_NET, "this remote has never connected");
		return -1;
	}

	return remote->transport->ls(out, size, remote->transport);
}

int git_remote__get_http_proxy(git_remote *remote, bool use_ssl, char **proxy_url)
{
	git_config *cfg;
	const git_config_entry *ce;
	const char *val = NULL;
	int error;

	assert(remote);

	if (!proxy_url || !remote->repo)
		return -1;

	*proxy_url = NULL;

	if ((error = git_repository_config__weakptr(&cfg, remote->repo)) < 0)
		return error;

	/* Go through the possible sources for proxy configuration, from most specific
	 * to least specific. */

	/* remote.<name>.proxy config setting */
	if (remote->name && remote->name[0]) {
		git_buf buf = GIT_BUF_INIT;

		if ((error = git_buf_printf(&buf, "remote.%s.proxy", remote->name)) < 0)
			return error;

		error = git_config__lookup_entry(&ce, cfg, git_buf_cstr(&buf), false);
		git_buf_free(&buf);

		if (error < 0)
			return error;

		if (ce && ce->value) {
			val = ce->value;
			goto found;
		}
	}

	/* http.proxy config setting */
	if ((error = git_config__lookup_entry(&ce, cfg, "http.proxy", false)) < 0)
		return error;
	if (ce && ce->value) {
		val = ce->value;
		goto found;
	}

	/* HTTP_PROXY / HTTPS_PROXY environment variables */
	val = use_ssl ? getenv("HTTPS_PROXY") : getenv("HTTP_PROXY");

found:
	if (val && val[0]) {
		*proxy_url = git__strdup(val);
		GITERR_CHECK_ALLOC(*proxy_url);
	}

	return 0;
}

/* DWIM `refspecs` based on `refs` and append the output to `out` */
static int dwim_refspecs(git_vector *out, git_vector *refspecs, git_vector *refs)
{
	size_t i;
	git_refspec *spec;

	git_vector_foreach(refspecs, i, spec) {
		if (git_refspec__dwim_one(out, spec, refs) < 0)
			return -1;
	}

	return 0;
}

static void free_refspecs(git_vector *vec)
{
	size_t i;
	git_refspec *spec;

	git_vector_foreach(vec, i, spec) {
		git_refspec__free(spec);
		git__free(spec);
	}

	git_vector_clear(vec);
}

static int remote_head_cmp(const void *_a, const void *_b)
{
	const git_remote_head *a = (git_remote_head *) _a;
	const git_remote_head *b = (git_remote_head *) _b;

	return git__strcmp_cb(a->name, b->name);
}

static int ls_to_vector(git_vector *out, git_remote *remote)
{
	git_remote_head **heads;
	size_t heads_len, i;

	if (git_remote_ls((const git_remote_head ***)&heads, &heads_len, remote) < 0)
		return -1;

	if (git_vector_init(out, heads_len, remote_head_cmp) < 0)
		return -1;

	for (i = 0; i < heads_len; i++) {
		if (git_vector_insert(out, heads[i]) < 0)
			return -1;
	}

	return 0;
}

int git_remote_download(git_remote *remote, const git_strarray *refspecs)
{
	int error = -1;
	size_t i;
	git_vector refs, specs, *to_active;

	assert(remote);

	if (ls_to_vector(&refs, remote) < 0)
		return -1;

	if ((git_vector_init(&specs, 0, NULL)) < 0)
		goto on_error;

	remote->passed_refspecs = 0;
	if (!refspecs || !refspecs->count) {
		to_active = &remote->refspecs;
	} else {
		for (i = 0; i < refspecs->count; i++) {
			if ((error = add_refspec_to(&specs, refspecs->strings[i], true)) < 0)
				goto on_error;
		}

		to_active = &specs;
		remote->passed_refspecs = 1;
	}

	free_refspecs(&remote->passive_refspecs);
	if ((error = dwim_refspecs(&remote->passive_refspecs, &remote->refspecs, &refs)) < 0)
		goto on_error;

	free_refspecs(&remote->active_refspecs);
	error = dwim_refspecs(&remote->active_refspecs, to_active, &refs);

	git_vector_free(&refs);
	free_refspecs(&specs);
	git_vector_free(&specs);

	if (error < 0)
		return error;

	if ((error = git_fetch_negotiate(remote)) < 0)
		return error;

	return git_fetch_download_pack(remote);

on_error:
	git_vector_free(&refs);
	free_refspecs(&specs);
	git_vector_free(&specs);
	return error;
}

int git_remote_fetch(
		git_remote *remote,
		const git_strarray *refspecs,
		const git_signature *signature,
		const char *reflog_message)
{
	int error;
	git_buf reflog_msg_buf = GIT_BUF_INIT;

	/* Connect and download everything */
	if ((error = git_remote_connect(remote, GIT_DIRECTION_FETCH)) != 0)
		return error;

	error = git_remote_download(remote, refspecs);

	/* We don't need to be connected anymore */
	git_remote_disconnect(remote);

	/* If the download failed, return the error */
	if (error != 0)
		return error;

	/* Default reflog message */
	if (reflog_message)
		git_buf_sets(&reflog_msg_buf, reflog_message);
	else {
		git_buf_printf(&reflog_msg_buf, "fetch %s",
				remote->name ? remote->name : remote->url);
	}

	/* Create "remote/foo" branches for all remote branches */
	error = git_remote_update_tips(remote, signature, git_buf_cstr(&reflog_msg_buf));
	git_buf_free(&reflog_msg_buf);
	return error;
}

static int remote_head_for_fetchspec_src(git_remote_head **out, git_vector *update_heads, const char *fetchspec_src)
{
	unsigned int i;
	git_remote_head *remote_ref;

	assert(update_heads && fetchspec_src);

	*out = NULL;

	git_vector_foreach(update_heads, i, remote_ref) {
		if (strcmp(remote_ref->name, fetchspec_src) == 0) {
			*out = remote_ref;
			break;
		}
	}

	return 0;
}

static int ref_to_update(int *update, git_buf *remote_name, git_remote *remote, git_refspec *spec, const char *ref_name)
{
	int error = 0;
	git_repository *repo;
	git_buf upstream_remote = GIT_BUF_INIT;
	git_buf upstream_name = GIT_BUF_INIT;

	repo = git_remote_owner(remote);

	if ((!git_reference__is_branch(ref_name)) ||
	    !git_remote_name(remote) ||
	    (error = git_branch_upstream_remote(&upstream_remote, repo, ref_name) < 0) ||
	    git__strcmp(git_remote_name(remote), git_buf_cstr(&upstream_remote)) ||
	    (error = git_branch_upstream_name(&upstream_name, repo, ref_name)) < 0 ||
	    !git_refspec_dst_matches(spec, git_buf_cstr(&upstream_name)) ||
	    (error = git_refspec_rtransform(remote_name, spec, upstream_name.ptr)) < 0) {
		/* Not an error if there is no upstream */
		if (error == GIT_ENOTFOUND) {
			giterr_clear();
			error = 0;
		}

		*update = 0;
	} else {
		*update = 1;
	}

	git_buf_free(&upstream_remote);
	git_buf_free(&upstream_name);
	return error;
}

static int remote_head_for_ref(git_remote_head **out, git_remote *remote, git_refspec *spec, git_vector *update_heads, git_reference *ref)
{
	git_reference *resolved_ref = NULL;
	git_buf remote_name = GIT_BUF_INIT;
	git_config *config = NULL;
	const char *ref_name;
	int error = 0, update;

	assert(out && spec && ref);

	*out = NULL;

	error = git_reference_resolve(&resolved_ref, ref);

	/* If we're in an unborn branch, let's pretend nothing happened */
	if (error == GIT_ENOTFOUND && git_reference_type(ref) == GIT_REF_SYMBOLIC) {
		ref_name = git_reference_symbolic_target(ref);
		error = 0;
	} else {
		ref_name = git_reference_name(resolved_ref);
	}

	if ((error = ref_to_update(&update, &remote_name, remote, spec, ref_name)) < 0)
		goto cleanup;

	if (update)
		error = remote_head_for_fetchspec_src(out, update_heads, git_buf_cstr(&remote_name));

cleanup:
	git_buf_free(&remote_name);
	git_reference_free(resolved_ref);
	git_config_free(config);
	return error;
}

static int git_remote_write_fetchhead(git_remote *remote, git_refspec *spec, git_vector *update_heads)
{
	git_reference *head_ref = NULL;
	git_fetchhead_ref *fetchhead_ref;
	git_remote_head *remote_ref, *merge_remote_ref;
	git_vector fetchhead_refs;
	bool include_all_fetchheads;
	unsigned int i = 0;
	int error = 0;

	assert(remote);

	/* no heads, nothing to do */
	if (update_heads->length == 0)
		return 0;

	if (git_vector_init(&fetchhead_refs, update_heads->length, git_fetchhead_ref_cmp) < 0)
		return -1;

	/* Iff refspec is * (but not subdir slash star), include tags */
	include_all_fetchheads = (strcmp(GIT_REFS_HEADS_DIR "*", git_refspec_src(spec)) == 0);

	/* Determine what to merge: if refspec was a wildcard, just use HEAD */
	if (git_refspec_is_wildcard(spec)) {
		if ((error = git_reference_lookup(&head_ref, remote->repo, GIT_HEAD_FILE)) < 0 ||
			(error = remote_head_for_ref(&merge_remote_ref, remote, spec, update_heads, head_ref)) < 0)
				goto cleanup;
	} else {
		/* If we're fetching a single refspec, that's the only thing that should be in FETCH_HEAD. */
		if ((error = remote_head_for_fetchspec_src(&merge_remote_ref, update_heads, git_refspec_src(spec))) < 0)
			goto cleanup;
	}

	/* Create the FETCH_HEAD file */
	git_vector_foreach(update_heads, i, remote_ref) {
		int merge_this_fetchhead = (merge_remote_ref == remote_ref);

		if (!include_all_fetchheads &&
			!git_refspec_src_matches(spec, remote_ref->name) &&
			!merge_this_fetchhead)
			continue;

		if (git_fetchhead_ref_create(&fetchhead_ref,
			&remote_ref->oid,
			merge_this_fetchhead,
			remote_ref->name,
			git_remote_url(remote)) < 0)
			goto cleanup;

		if (git_vector_insert(&fetchhead_refs, fetchhead_ref) < 0)
			goto cleanup;
	}

	git_fetchhead_write(remote->repo, &fetchhead_refs);

cleanup:
	for (i = 0; i < fetchhead_refs.length; ++i)
		git_fetchhead_ref_free(fetchhead_refs.contents[i]);

	git_vector_free(&fetchhead_refs);
	git_reference_free(head_ref);

	return error;
}

static int update_tips_for_spec(
		git_remote *remote,
		git_refspec *spec,
		git_vector *refs,
		const git_signature *signature,
		const char *log_message)
{
	int error = 0, autotag;
	unsigned int i = 0;
	git_buf refname = GIT_BUF_INIT;
	git_oid old;
	git_odb *odb;
	git_remote_head *head;
	git_reference *ref;
	git_refspec tagspec;
	git_vector update_heads;

	assert(remote);

	if (git_repository_odb__weakptr(&odb, remote->repo) < 0)
		return -1;

	if (git_refspec__parse(&tagspec, GIT_REFSPEC_TAGS, true) < 0)
		return -1;

	/* Make a copy of the transport's refs */
	if (git_vector_init(&update_heads, 16, NULL) < 0)
		return -1;

	for (; i < refs->length; ++i) {
		head = git_vector_get(refs, i);
		autotag = 0;

		/* Ignore malformed ref names (which also saves us from tag^{} */
		if (!git_reference_is_valid_name(head->name))
			continue;

		if (git_refspec_src_matches(&tagspec, head->name)) {
			if (remote->download_tags != GIT_REMOTE_DOWNLOAD_TAGS_NONE) {

				if (remote->download_tags == GIT_REMOTE_DOWNLOAD_TAGS_AUTO)
					autotag = 1;

				git_buf_clear(&refname);
				if (git_buf_puts(&refname, head->name) < 0)
					goto on_error;
			} else {
				continue;
			}
		} else if (git_refspec_src_matches(spec, head->name) && spec->dst) {
			if (git_refspec_transform(&refname, spec, head->name) < 0)
				goto on_error;
		} else {
			continue;
		}

		/* In autotag mode, only create tags for objects already in db */
		if (autotag && !git_odb_exists(odb, &head->oid))
			continue;

		if (!autotag && git_vector_insert(&update_heads, head) < 0)
			goto on_error;

		error = git_reference_name_to_id(&old, remote->repo, refname.ptr);
		if (error < 0 && error != GIT_ENOTFOUND)
			goto on_error;

		if (error == GIT_ENOTFOUND) {
			memset(&old, 0, GIT_OID_RAWSZ);

			if (autotag && git_vector_insert(&update_heads, head) < 0)
				goto on_error;
		}

		if (!git_oid__cmp(&old, &head->oid))
			continue;

		/* In autotag mode, don't overwrite any locally-existing tags */
		error = git_reference_create(&ref, remote->repo, refname.ptr, &head->oid, !autotag, 
				signature, log_message);
		if (error < 0 && error != GIT_EEXISTS)
			goto on_error;

		git_reference_free(ref);

		if (remote->callbacks.update_tips != NULL) {
			if (remote->callbacks.update_tips(refname.ptr, &old, &head->oid, remote->callbacks.payload) < 0)
				goto on_error;
		}
	}

	if (git_remote_update_fetchhead(remote) &&
	    (error = git_remote_write_fetchhead(remote, spec, &update_heads)) < 0)
		goto on_error;

	git_vector_free(&update_heads);
	git_refspec__free(&tagspec);
	git_buf_free(&refname);
	return 0;

on_error:
	git_vector_free(&update_heads);
	git_refspec__free(&tagspec);
	git_buf_free(&refname);
	return -1;

}

/**
 * Iteration over the three vectors, with a pause whenever we find a match
 *
 * On each stop, we store the iteration stat in the inout i,j,k
 * parameters, and return the currently matching passive refspec as
 * well as the head which we matched.
 */
static int next_head(const git_remote *remote, git_vector *refs,
		     git_refspec **out_spec, git_remote_head **out_head,
		     size_t *out_i, size_t *out_j, size_t *out_k)
{
	const git_vector *active, *passive;
	git_remote_head *head;
	git_refspec *spec, *passive_spec;
	size_t i, j, k;

	active = &remote->active_refspecs;
	passive = &remote->passive_refspecs;

	i = *out_i;
	j = *out_j;
	k = *out_k;

	for (; i < refs->length; i++) {
		head = git_vector_get(refs, i);

		if (!git_reference_is_valid_name(head->name))
			continue;

		for (; j < active->length; j++) {
			spec = git_vector_get(active, j);

			if (!git_refspec_src_matches(spec, head->name))
				continue;

			for (; k < passive->length; k++) {
				passive_spec = git_vector_get(passive, k);

				if (!git_refspec_src_matches(passive_spec, head->name))
				    continue;

				*out_spec = passive_spec;
				*out_head = head;
				*out_i = i;
				*out_j = j;
				*out_k = k + 1;
				return 0;

			}
			k = 0;
		}
		j = 0;
	}

	return GIT_ITEROVER;
}

static int opportunistic_updates(const git_remote *remote, git_vector *refs, const git_signature *sig, const char *msg)
{
	size_t i, j, k;
	git_refspec *spec;
	git_remote_head *head;
	git_reference *ref;
	git_buf refname = GIT_BUF_INIT;
	int error;

	i = j = k = 0;

	while ((error = next_head(remote, refs, &spec, &head, &i, &j, &k)) == 0) {
		/*
		 * If we got here, there is a refspec which was used
		 * for fetching which matches the source of one of the
		 * passive refspecs, so we should update that
		 * remote-tracking branch, but not add it to
		 * FETCH_HEAD
		 */

		if ((error = git_refspec_transform(&refname, spec, head->name)) < 0)
			return error;

		error = git_reference_create(&ref, remote->repo, refname.ptr, &head->oid, true, sig, msg);
		git_buf_free(&refname);
		git_reference_free(ref);

		if (error < 0)
			return error;
	}

	return 0;
}

int git_remote_update_tips(
		git_remote *remote,
		const git_signature *signature,
		const char *reflog_message)
{
	git_refspec *spec, tagspec;
	git_vector refs;
	int error;
	size_t i;

	if (git_refspec__parse(&tagspec, GIT_REFSPEC_TAGS, true) < 0)
		return -1;


	if ((error = ls_to_vector(&refs, remote)) < 0)
		goto out;

	if (remote->download_tags == GIT_REMOTE_DOWNLOAD_TAGS_ALL) {
		if ((error = update_tips_for_spec(remote, &tagspec, &refs, signature, reflog_message)) < 0)
			goto out;
	}

	git_vector_foreach(&remote->active_refspecs, i, spec) {
		if (spec->push)
			continue;

		if ((error = update_tips_for_spec(remote, spec, &refs, signature, reflog_message)) < 0)
			goto out;
	}

	/* only try to do opportunisitic updates if the refpec lists differ */
	if (remote->passed_refspecs)
		error = opportunistic_updates(remote, &refs, signature, reflog_message);

out:
	git_vector_free(&refs);
	git_refspec__free(&tagspec);
	return error;
}

int git_remote_connected(const git_remote *remote)
{
	assert(remote);

	if (!remote->transport || !remote->transport->is_connected)
		return 0;

	/* Ask the transport if it's connected. */
	return remote->transport->is_connected(remote->transport);
}

void git_remote_stop(git_remote *remote)
{
	assert(remote);

	if (remote->transport && remote->transport->cancel)
		remote->transport->cancel(remote->transport);
}

void git_remote_disconnect(git_remote *remote)
{
	assert(remote);

	if (git_remote_connected(remote))
		remote->transport->close(remote->transport);
}

void git_remote_free(git_remote *remote)
{
	if (remote == NULL)
		return;

	if (remote->transport != NULL) {
		git_remote_disconnect(remote);

		remote->transport->free(remote->transport);
		remote->transport = NULL;
	}

	git_vector_free(&remote->refs);

	free_refspecs(&remote->refspecs);
	git_vector_free(&remote->refspecs);

	free_refspecs(&remote->active_refspecs);
	git_vector_free(&remote->active_refspecs);

	free_refspecs(&remote->passive_refspecs);
	git_vector_free(&remote->passive_refspecs);

	git__free(remote->url);
	git__free(remote->pushurl);
	git__free(remote->name);
	git__free(remote);
}

static int remote_list_cb(const git_config_entry *entry, void *payload)
{
	git_vector *list = payload;
	const char *name = entry->name + strlen("remote.");
	size_t namelen = strlen(name);
	char *remote_name;

	/* we know name matches "remote.<stuff>.(push)?url" */

	if (!strcmp(&name[namelen - 4], ".url"))
		remote_name = git__strndup(name, namelen - 4); /* strip ".url" */
	else
		remote_name = git__strndup(name, namelen - 8); /* strip ".pushurl" */
	GITERR_CHECK_ALLOC(remote_name);

	return git_vector_insert(list, remote_name);
}

int git_remote_list(git_strarray *remotes_list, git_repository *repo)
{
	int error;
	git_config *cfg;
	git_vector list = GIT_VECTOR_INIT;

	if ((error = git_repository_config__weakptr(&cfg, repo)) < 0)
		return error;

	if ((error = git_vector_init(&list, 4, git__strcmp_cb)) < 0)
		return error;

	error = git_config_foreach_match(
		cfg, "^remote\\..*\\.(push)?url$", remote_list_cb, &list);

	if (error < 0) {
		git_vector_free_deep(&list);
		return error;
	}

	git_vector_uniq(&list, git__free);

	remotes_list->strings =
		(char **)git_vector_detach(&remotes_list->count, NULL, &list);

	return 0;
}

int git_remote_set_callbacks(git_remote *remote, const git_remote_callbacks *callbacks)
{
	assert(remote && callbacks);

	GITERR_CHECK_VERSION(callbacks, GIT_REMOTE_CALLBACKS_VERSION, "git_remote_callbacks");

	memcpy(&remote->callbacks, callbacks, sizeof(git_remote_callbacks));

	if (remote->transport && remote->transport->set_callbacks)
		return remote->transport->set_callbacks(remote->transport,
			remote->callbacks.sideband_progress,
			NULL,
			remote->callbacks.certificate_check,
			remote->callbacks.payload);

	return 0;
}

const git_remote_callbacks *git_remote_get_callbacks(git_remote *remote)
{
	assert(remote);

	return &remote->callbacks;
}

int git_remote_set_transport(
	git_remote *remote,
	git_transport_cb transport_cb,
	void *payload)
{
	assert(remote);

	if (remote->transport) {
		giterr_set(GITERR_NET, "A transport is already bound to this remote");
		return -1;
	}

	remote->transport_cb = transport_cb;
	remote->transport_cb_payload = payload;
	return 0;
}

const git_transfer_progress* git_remote_stats(git_remote *remote)
{
	assert(remote);
	return &remote->stats;
}

git_remote_autotag_option_t git_remote_autotag(const git_remote *remote)
{
	return remote->download_tags;
}

void git_remote_set_autotag(git_remote *remote, git_remote_autotag_option_t value)
{
	remote->download_tags = value;
}

static int rename_remote_config_section(
	git_repository *repo,
	const char *old_name,
	const char *new_name)
{
	git_buf old_section_name = GIT_BUF_INIT,
		new_section_name = GIT_BUF_INIT;
	int error = -1;

	if (git_buf_printf(&old_section_name, "remote.%s", old_name) < 0)
		goto cleanup;

	if (new_name &&
		(git_buf_printf(&new_section_name, "remote.%s", new_name) < 0))
			goto cleanup;

	error = git_config_rename_section(
		repo,
		git_buf_cstr(&old_section_name),
		new_name ? git_buf_cstr(&new_section_name) : NULL);

cleanup:
	git_buf_free(&old_section_name);
	git_buf_free(&new_section_name);

	return error;
}

struct update_data {
	git_config *config;
	const char *old_remote_name;
	const char *new_remote_name;
};

static int update_config_entries_cb(
	const git_config_entry *entry,
	void *payload)
{
	struct update_data *data = (struct update_data *)payload;

	if (strcmp(entry->value, data->old_remote_name))
		return 0;

	return git_config_set_string(
		data->config, entry->name, data->new_remote_name);
}

static int update_branch_remote_config_entry(
	git_repository *repo,
	const char *old_name,
	const char *new_name)
{
	int error;
	struct update_data data = { NULL };

	if ((error = git_repository_config__weakptr(&data.config, repo)) < 0)
		return error;

	data.old_remote_name = old_name;
	data.new_remote_name = new_name;

	return git_config_foreach_match(
		data.config, "branch\\..+\\.remote", update_config_entries_cb, &data);
}

static int rename_one_remote_reference(
	git_reference *reference_in,
	const char *old_remote_name,
	const char *new_remote_name)
{
	int error;
	git_reference *ref = NULL, *dummy = NULL;
	git_buf namespace = GIT_BUF_INIT, old_namespace = GIT_BUF_INIT;
	git_buf new_name = GIT_BUF_INIT;
	git_buf log_message = GIT_BUF_INIT;
	size_t pfx_len;
	const char *target;

	if ((error = git_buf_printf(&namespace, GIT_REFS_REMOTES_DIR "%s/", new_remote_name)) < 0)
		return error;

	pfx_len = strlen(GIT_REFS_REMOTES_DIR) + strlen(old_remote_name) + 1;
	git_buf_puts(&new_name, namespace.ptr);
	if ((error = git_buf_puts(&new_name, git_reference_name(reference_in) + pfx_len)) < 0)
		goto cleanup;

	if ((error = git_buf_printf(&log_message,
					"renamed remote %s to %s",
					old_remote_name, new_remote_name)) < 0)
		goto cleanup;

	if ((error = git_reference_rename(&ref, reference_in, git_buf_cstr(&new_name), 1,
					  NULL, git_buf_cstr(&log_message))) < 0)
		goto cleanup;

	if (git_reference_type(ref) != GIT_REF_SYMBOLIC)
		goto cleanup;

	/* Handle refs like origin/HEAD -> origin/master */
	target = git_reference_symbolic_target(ref);
	if ((error = git_buf_printf(&old_namespace, GIT_REFS_REMOTES_DIR "%s/", old_remote_name)) < 0)
		goto cleanup;

	if (git__prefixcmp(target, old_namespace.ptr))
		goto cleanup;

	git_buf_clear(&new_name);
	git_buf_puts(&new_name, namespace.ptr);
	if ((error = git_buf_puts(&new_name, target + pfx_len)) < 0)
		goto cleanup;

	error = git_reference_symbolic_set_target(&dummy, ref, git_buf_cstr(&new_name),
						  NULL, git_buf_cstr(&log_message));

	git_reference_free(dummy);

cleanup:
	git_reference_free(reference_in);
	git_reference_free(ref);
	git_buf_free(&namespace);
	git_buf_free(&old_namespace);
	git_buf_free(&new_name);
	git_buf_free(&log_message);
	return error;
}

static int rename_remote_references(
	git_repository *repo,
	const char *old_name,
	const char *new_name)
{
	int error;
	git_buf buf = GIT_BUF_INIT;
	git_reference *ref;
	git_reference_iterator *iter;

	if ((error = git_buf_printf(&buf, GIT_REFS_REMOTES_DIR "%s/*", old_name)) < 0)
		return error;

	error = git_reference_iterator_glob_new(&iter, repo, git_buf_cstr(&buf));
	git_buf_free(&buf);

	if (error < 0)
		return error;

	while ((error = git_reference_next(&ref, iter)) == 0) {
		if ((error = rename_one_remote_reference(ref, old_name, new_name)) < 0)
			break;
	}

	git_reference_iterator_free(iter);

	return (error == GIT_ITEROVER) ? 0 : error;
}

static int rename_fetch_refspecs(git_vector *problems, git_remote *remote, const char *new_name)
{
	git_config *config;
	git_buf base = GIT_BUF_INIT, var = GIT_BUF_INIT, val = GIT_BUF_INIT;
	const git_refspec *spec;
	size_t i;
	int error = 0;

	if ((error = git_repository_config__weakptr(&config, remote->repo)) < 0)
		return error;

	if ((error = git_vector_init(problems, 1, NULL)) < 0)
		return error;

	if ((error = git_buf_printf(
			&base, "+refs/heads/*:refs/remotes/%s/*", remote->name)) < 0)
		return error;

	git_vector_foreach(&remote->refspecs, i, spec) {
		if (spec->push)
			continue;

		/* Does the dst part of the refspec follow the expected format? */
		if (strcmp(git_buf_cstr(&base), spec->string)) {
			char *dup;

			dup = git__strdup(spec->string);
			GITERR_CHECK_ALLOC(dup);

			if ((error = git_vector_insert(problems, dup)) < 0)
				break;

			continue;
		}

		/* If we do want to move it to the new section */

		git_buf_clear(&val);
		git_buf_clear(&var);

		if (git_buf_printf(
				&val, "+refs/heads/*:refs/remotes/%s/*", new_name) < 0 ||
			git_buf_printf(&var, "remote.%s.fetch", new_name) < 0)
		{
			error = -1;
			break;
		}

		if ((error = git_config_set_string(
				config, git_buf_cstr(&var), git_buf_cstr(&val))) < 0)
			break;
	}

	git_buf_free(&base);
	git_buf_free(&var);
	git_buf_free(&val);

	if (error < 0) {
		char *str;
		git_vector_foreach(problems, i, str)
			git__free(str);

		git_vector_free(problems);
	}

	return error;
}

int git_remote_rename(git_strarray *out, git_repository *repo, const char *name, const char *new_name)
{
	int error;
	git_vector problem_refspecs = GIT_VECTOR_INIT;
	git_remote *remote = NULL;

	assert(out && repo && name && new_name);

	if ((error = git_remote_lookup(&remote, repo, name)) < 0)
		return error;

	if ((error = ensure_remote_name_is_valid(new_name)) < 0)
		goto cleanup;

	if ((error = ensure_remote_doesnot_exist(repo, new_name)) < 0)
		goto cleanup;

	if ((error = rename_remote_config_section(repo, name, new_name)) < 0)
		goto cleanup;

	if ((error = update_branch_remote_config_entry(repo, name, new_name)) < 0)
		goto cleanup;

	if ((error = rename_remote_references(repo, name, new_name)) < 0)
		goto cleanup;

	if ((error = rename_fetch_refspecs(&problem_refspecs, remote, new_name)) < 0)
		goto cleanup;

	out->count = problem_refspecs.length;
	out->strings = (char **) problem_refspecs.contents;

cleanup:
	if (error < 0)
		git_vector_free(&problem_refspecs);

	git_remote_free(remote);
	return error;
}

int git_remote_update_fetchhead(git_remote *remote)
{
	return (remote->update_fetchhead != 0);
}

void git_remote_set_update_fetchhead(git_remote *remote, int value)
{
	remote->update_fetchhead = (value != 0);
}

int git_remote_is_valid_name(
	const char *remote_name)
{
	git_buf buf = GIT_BUF_INIT;
	git_refspec refspec;
	int error = -1;

	if (!remote_name || *remote_name == '\0')
		return 0;

	git_buf_printf(&buf, "refs/heads/test:refs/remotes/%s/test", remote_name);
	error = git_refspec__parse(&refspec, git_buf_cstr(&buf), true);

	git_buf_free(&buf);
	git_refspec__free(&refspec);

	giterr_clear();
	return error == 0;
}

git_refspec *git_remote__matching_refspec(git_remote *remote, const char *refname)
{
	git_refspec *spec;
	size_t i;

	git_vector_foreach(&remote->active_refspecs, i, spec) {
		if (spec->push)
			continue;

		if (git_refspec_src_matches(spec, refname))
			return spec;
	}

	return NULL;
}

git_refspec *git_remote__matching_dst_refspec(git_remote *remote, const char *refname)
{
	git_refspec *spec;
	size_t i;

	git_vector_foreach(&remote->active_refspecs, i, spec) {
		if (spec->push)
			continue;

		if (git_refspec_dst_matches(spec, refname))
			return spec;
	}

	return NULL;
}

void git_remote_clear_refspecs(git_remote *remote)
{
	git_refspec *spec;
	size_t i;

	git_vector_foreach(&remote->refspecs, i, spec) {
		git_refspec__free(spec);
		git__free(spec);
	}
	git_vector_clear(&remote->refspecs);
}

int git_remote_add_fetch(git_remote *remote, const char *refspec)
{
	return add_refspec(remote, refspec, true);
}

int git_remote_add_push(git_remote *remote, const char *refspec)
{
	return add_refspec(remote, refspec, false);
}

static int set_refspecs(git_remote *remote, git_strarray *array, int push)
{
	git_vector *vec = &remote->refspecs;
	git_refspec *spec;
	size_t i;

	/* Start by removing any refspecs of the same type */
	for (i = 0; i < vec->length; i++) {
		spec = git_vector_get(vec, i);
		if (spec->push != push)
			continue;

		git_refspec__free(spec);
		git__free(spec);
		git_vector_remove(vec, i);
		i--;
	}

	/* And now we add the new ones */

	for (i = 0; i < array->count; i++) {
		if (add_refspec(remote, array->strings[i], !push) < 0)
			return -1;
	}

	return 0;
}

int git_remote_set_fetch_refspecs(git_remote *remote, git_strarray *array)
{
	return set_refspecs(remote, array, false);
}

int git_remote_set_push_refspecs(git_remote *remote, git_strarray *array)
{
	return set_refspecs(remote, array, true);
}

static int copy_refspecs(git_strarray *array, const git_remote *remote, unsigned int push)
{
	size_t i;
	git_vector refspecs;
	git_refspec *spec;
	char *dup;

	if (git_vector_init(&refspecs, remote->refspecs.length, NULL) < 0)
		return -1;

	git_vector_foreach(&remote->refspecs, i, spec) {
		if (spec->push != push)
			continue;

		if ((dup = git__strdup(spec->string)) == NULL)
			goto on_error;

		if (git_vector_insert(&refspecs, dup) < 0) {
			git__free(dup);
			goto on_error;
		}
	}

	array->strings = (char **)refspecs.contents;
	array->count = refspecs.length;

	return 0;

on_error:
	git_vector_free_deep(&refspecs);

	return -1;
}

int git_remote_get_fetch_refspecs(git_strarray *array, const git_remote *remote)
{
	return copy_refspecs(array, remote, false);
}

int git_remote_get_push_refspecs(git_strarray *array, const git_remote *remote)
{
	return copy_refspecs(array, remote, true);
}

size_t git_remote_refspec_count(const git_remote *remote)
{
	return remote->refspecs.length;
}

const git_refspec *git_remote_get_refspec(const git_remote *remote, size_t n)
{
	return git_vector_get(&remote->refspecs, n);
}

int git_remote_init_callbacks(git_remote_callbacks *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_remote_callbacks, GIT_REMOTE_CALLBACKS_INIT);
	return 0;
}

/* asserts a branch.<foo>.remote format */
static const char *name_offset(size_t *len_out, const char *name)
{
	size_t prefix_len;
	const char *dot;

	prefix_len = strlen("remote.");
	dot = strchr(name + prefix_len, '.');

	assert(dot);

	*len_out = dot - name - prefix_len;
	return name + prefix_len;
}

static int remove_branch_config_related_entries(
	git_repository *repo,
	const char *remote_name)
{
	int error;
	git_config *config;
	git_config_entry *entry;
	git_config_iterator *iter;
	git_buf buf = GIT_BUF_INIT;

	if ((error = git_repository_config__weakptr(&config, repo)) < 0)
		return error;

	if ((error = git_config_iterator_glob_new(&iter, config, "branch\\..+\\.remote")) < 0)
		return error;

	/* find any branches with us as upstream and remove that config */
	while ((error = git_config_next(&entry, iter)) == 0) {
		const char *branch;
		size_t branch_len;

		if (strcmp(remote_name, entry->value))
			continue;

		branch = name_offset(&branch_len, entry->name);

		git_buf_clear(&buf);
		if (git_buf_printf(&buf, "branch.%.*s.merge", (int)branch_len, branch) < 0)
			break;

		if ((error = git_config_delete_entry(config, git_buf_cstr(&buf))) < 0)
			break;

		git_buf_clear(&buf);
		if (git_buf_printf(&buf, "branch.%.*s.remote", (int)branch_len, branch) < 0)
			break;

		if ((error = git_config_delete_entry(config, git_buf_cstr(&buf))) < 0)
			break;
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_buf_free(&buf);
	git_config_iterator_free(iter);
	return error;
}

static int remove_refs(git_repository *repo, const git_refspec *spec)
{
	git_reference_iterator *iter = NULL;
	git_vector refs;
	const char *name;
	char *dup;
	int error;
	size_t i;

	if ((error = git_vector_init(&refs, 8, NULL)) < 0)
		return error;

	if ((error = git_reference_iterator_new(&iter, repo)) < 0)
		goto cleanup;

	while ((error = git_reference_next_name(&name, iter)) == 0) {
		if (!git_refspec_dst_matches(spec, name))
			continue;

		dup = git__strdup(name);
		if (!dup) {
			error = -1;
			goto cleanup;
		}

		if ((error = git_vector_insert(&refs, dup)) < 0)
			goto cleanup;
	}
	if (error == GIT_ITEROVER)
		error = 0;
	if (error < 0)
		goto cleanup;

	git_vector_foreach(&refs, i, name) {
		if ((error = git_reference_remove(repo, name)) < 0)
			break;
	}

cleanup:
	git_reference_iterator_free(iter);
	git_vector_foreach(&refs, i, dup) {
		git__free(dup);
	}
	git_vector_free(&refs);
	return error;
}

static int remove_remote_tracking(git_repository *repo, const char *remote_name)
{
	git_remote *remote;
	int error;
	size_t i, count;

	/* we want to use what's on the config, regardless of changes to the instance in memory */
	if ((error = git_remote_lookup(&remote, repo, remote_name)) < 0)
		return error;

	count = git_remote_refspec_count(remote);
	for (i = 0; i < count; i++) {
		const git_refspec *refspec = git_remote_get_refspec(remote, i);

		/* shouldn't ever actually happen */
		if (refspec == NULL)
			continue;

		if ((error = remove_refs(repo, refspec)) < 0)
			break;
	}

	git_remote_free(remote);
	return error;
}

int git_remote_delete(git_repository *repo, const char *name)
{
	int error;

	assert(repo && name);

	if ((error = remove_branch_config_related_entries(repo, name)) < 0 ||
	    (error = remove_remote_tracking(repo, name)) < 0 ||
	    (error = rename_remote_config_section(repo, name, NULL)) < 0)
		return error;

	return 0;
}

int git_remote_default_branch(git_buf *out, git_remote *remote)
{
	const git_remote_head **heads;
	const git_remote_head *guess = NULL;
	const git_oid *head_id;
	size_t heads_len, i;
	int error;

	assert(out);

	if ((error = git_remote_ls(&heads, &heads_len, remote)) < 0)
		return error;

	if (heads_len == 0)
		return GIT_ENOTFOUND;

	if (strcmp(heads[0]->name, GIT_HEAD_FILE))
		return GIT_ENOTFOUND;

	git_buf_sanitize(out);
	/* the first one must be HEAD so if that has the symref info, we're done */
	if (heads[0]->symref_target)
		return git_buf_puts(out, heads[0]->symref_target);

	/*
	 * If there's no symref information, we have to look over them
	 * and guess. We return the first match unless the master
	 * branch is a candidate. Then we return the master branch.
	 */
	head_id = &heads[0]->oid;

	for (i = 1; i < heads_len; i++) {
		if (git_oid_cmp(head_id, &heads[i]->oid))
			continue;

		if (git__prefixcmp(heads[i]->name, GIT_REFS_HEADS_DIR))
			continue;

		if (!guess) {
			guess = heads[i];
			continue;
		}

		if (!git__strcmp(GIT_REFS_HEADS_MASTER_FILE, heads[i]->name)) {
			guess = heads[i];
			break;
		}
	}

	if (!guess)
		return GIT_ENOTFOUND;

	return git_buf_puts(out, guess->name);
}

int git_remote_push(git_remote *remote, git_strarray *refspecs, const git_push_options *opts,
		    const git_signature *signature, const char *reflog_message)
{
	int error;
	size_t i;
	git_push *push = NULL;
	git_remote_callbacks *cbs;
	git_refspec *spec;

	assert(remote && refspecs);

	if ((error = git_remote_connect(remote, GIT_DIRECTION_PUSH)) < 0)
		return error;

	if ((error = git_push_new(&push, remote)) < 0)
		goto cleanup;

	if (opts && (error = git_push_set_options(push, opts)) < 0)
		goto cleanup;

	if (refspecs && refspecs->count > 0) {
		for (i = 0; i < refspecs->count; i++) {
			if ((error = git_push_add_refspec(push, refspecs->strings[i])) < 0)
				goto cleanup;
		}
	} else {
		git_vector_foreach(&remote->refspecs, i, spec) {
			if (!spec->push)
				continue;
			if ((error = git_push_add_refspec(push, spec->string)) < 0)
				goto cleanup;
		}
	}

	cbs = &remote->callbacks;
	if ((error = git_push_set_callbacks(push,
					    cbs->pack_progress, cbs->payload,
					    cbs->push_transfer_progress, cbs->payload)) < 0)
		goto cleanup;

	if ((error = git_push_finish(push)) < 0)
		goto cleanup;

	if (cbs->push_update_reference &&
	    (error = git_push_status_foreach(push, cbs->push_update_reference, cbs->payload)) < 0)
		goto cleanup;

	error = git_push_update_tips(push, signature, reflog_message);

cleanup:
	git_remote_disconnect(remote);
	git_push_free(push);
	return error;
}
