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

static int add_refspec(git_remote *remote, const char *string, bool is_fetch)
{
	git_refspec *spec;

	spec = git__calloc(1, sizeof(git_refspec));
	GITERR_CHECK_ALLOC(spec);

	if (git_refspec__parse(spec, string, is_fetch) < 0) {
		git__free(spec);
		return -1;
	}

	spec->push = !is_fetch;
	if (git_vector_insert(&remote->refspecs, spec) < 0) {
		git_refspec__free(spec);
		git__free(spec);
		return -1;
	}

	return 0;
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

static int create_internal(git_remote **out, git_repository *repo, const char *name, const char *url, const char *fetch)
{
	git_remote *remote;
	git_buf fetchbuf = GIT_BUF_INIT;
	int error = -1;

	/* name is optional */
	assert(out && repo && url);

	remote = git__calloc(1, sizeof(git_remote));
	GITERR_CHECK_ALLOC(remote);

	remote->repo = repo;
	remote->update_fetchhead = 1;

	if (get_check_cert(&remote->check_cert, repo) < 0)
		goto on_error;

	if (git_vector_init(&remote->refs, 32, NULL) < 0)
		goto on_error;

	remote->url = git__strdup(url);
	GITERR_CHECK_ALLOC(remote->url);

	if (name != NULL) {
		remote->name = git__strdup(name);
		GITERR_CHECK_ALLOC(remote->name);
	}

	if (fetch != NULL) {
		if (add_refspec(remote, fetch, true) < 0)
			goto on_error;
	}

	if (!name)
		/* A remote without a name doesn't download tags */
		remote->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_NONE;

	*out = remote;
	git_buf_free(&fetchbuf);
	return 0;

on_error:
	git_remote_free(remote);
	git_buf_free(&fetchbuf);
	return error;
}

static int ensure_remote_doesnot_exist(git_repository *repo, const char *name)
{
	int error;
	git_remote *remote;

	error = git_remote_load(&remote, repo, name);

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
	git_remote *remote = NULL;
	int error;

	if ((error = ensure_remote_name_is_valid(name)) < 0)
		return error;

	if ((error = ensure_remote_doesnot_exist(repo, name)) < 0)
		return error;

	if (git_buf_printf(&buf, "+refs/heads/*:refs/remotes/%s/*", name) < 0)
		return -1;

	if (create_internal(&remote, repo, name, url, git_buf_cstr(&buf)) < 0)
		goto on_error;

	git_buf_free(&buf);

	if (git_remote_save(remote) < 0)
		goto on_error;

	*out = remote;

	return 0;

on_error:
	git_buf_free(&buf);
	git_remote_free(remote);
	return -1;
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

	remote->repo = source->repo;
	remote->download_tags = source->download_tags;
	remote->check_cert = source->check_cert;
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

int git_remote_load(git_remote **out, git_repository *repo, const char *name)
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

	if ((error = get_check_cert(&remote->check_cert, repo)) < 0)
		goto cleanup;

	if (git_vector_init(&remote->refs, 32, NULL) < 0 ||
	    git_vector_init(&remote->refspecs, 2, NULL) < 0 ||
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

	/* A transport could have been supplied in advance with
	 * git_remote_set_transport */
	if (!t && (error = git_transport_new(&t, remote, url)) < 0)
		return error;

	if (t->set_callbacks &&
		(error = t->set_callbacks(t, remote->callbacks.sideband_progress, NULL, remote->callbacks.payload)) < 0)
		goto on_error;

	if (!remote->check_cert)
		flags |= GIT_TRANSPORTFLAGS_NO_CHECK_CERT;

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

int git_remote_download(git_remote *remote)
{
	int error;
	git_vector refs;

	assert(remote);

	if (ls_to_vector(&refs, remote) < 0)
		return -1;

	free_refspecs(&remote->active_refspecs);

	error = dwim_refspecs(&remote->active_refspecs, &remote->refspecs, &refs);
	git_vector_free(&refs);

	if (error < 0)
		return error;

	if ((error = git_fetch_negotiate(remote)) < 0)
		return error;

	return git_fetch_download_pack(remote);
}

int git_remote_fetch(
		git_remote *remote,
		const git_signature *signature,
		const char *reflog_message)
{
	int error;
	git_buf reflog_msg_buf = GIT_BUF_INIT;

	/* Connect and download everything */
	if ((error = git_remote_connect(remote, GIT_DIRECTION_FETCH)) != 0)
		return error;

	error = git_remote_download(remote);

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

static int remote_head_for_ref(git_remote_head **out, git_refspec *spec, git_vector *update_heads, git_reference *ref)
{
	git_reference *resolved_ref = NULL;
	git_buf remote_name = GIT_BUF_INIT;
	git_buf upstream_name = GIT_BUF_INIT;
	git_repository *repo;
	const char *ref_name;
	int error = 0;

	assert(out && spec && ref);

	*out = NULL;

	repo = git_reference_owner(ref);

	error = git_reference_resolve(&resolved_ref, ref);

	/* If we're in an unborn branch, let's pretend nothing happened */
	if (error == GIT_ENOTFOUND && git_reference_type(ref) == GIT_REF_SYMBOLIC) {
		ref_name = git_reference_symbolic_target(ref);
		error = 0;
	} else {
		ref_name = git_reference_name(resolved_ref);
	}

	if ((!git_reference__is_branch(ref_name)) ||
	    (error = git_branch_upstream_name(&upstream_name, repo, ref_name)) < 0 ||
	    (error = git_refspec_rtransform(&remote_name, spec, upstream_name.ptr)) < 0) {
		/* Not an error if there is no upstream */
		if (error == GIT_ENOTFOUND)
			error = 0;

		goto cleanup;
	}

	error = remote_head_for_fetchspec_src(out, update_heads, git_buf_cstr(&remote_name));

cleanup:
	git_reference_free(resolved_ref);
	git_buf_free(&remote_name);
	git_buf_free(&upstream_name);
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
			(error = remote_head_for_ref(&merge_remote_ref, spec, update_heads, head_ref)) < 0)
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

		if (git_refspec_src_matches(spec, head->name) && spec->dst) {
			if (git_refspec_transform(&refname, spec, head->name) < 0)
				goto on_error;
		} else if (remote->download_tags != GIT_REMOTE_DOWNLOAD_TAGS_NONE) {

			if (remote->download_tags != GIT_REMOTE_DOWNLOAD_TAGS_ALL)
				autotag = 1;

			if (!git_refspec_src_matches(&tagspec, head->name))
				continue;

			git_buf_clear(&refname);
			if (git_buf_puts(&refname, head->name) < 0)
				goto on_error;
		} else {
			continue;
		}

		if (autotag && !git_odb_exists(odb, &head->oid))
			continue;

		if (git_vector_insert(&update_heads, head) < 0)
			goto on_error;

		error = git_reference_name_to_id(&old, remote->repo, refname.ptr);
		if (error < 0 && error != GIT_ENOTFOUND)
			goto on_error;

		if (error == GIT_ENOTFOUND)
			memset(&old, 0, GIT_OID_RAWSZ);

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
		error = update_tips_for_spec(remote, &tagspec, &refs, signature, reflog_message);
		goto out;
	}

	git_vector_foreach(&remote->active_refspecs, i, spec) {
		if (spec->push)
			continue;

		if ((error = update_tips_for_spec(remote, spec, &refs, signature, reflog_message)) < 0)
			goto out;
	}

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

void git_remote_check_cert(git_remote *remote, int check)
{
	assert(remote);

	remote->check_cert = check;
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
			remote->callbacks.payload);

	return 0;
}

const git_remote_callbacks *git_remote_get_callbacks(git_remote *remote)
{
	assert(remote);

	return &remote->callbacks;
}

int git_remote_set_transport(git_remote *remote, git_transport *transport)
{
	assert(remote && transport);

	GITERR_CHECK_VERSION(transport, GIT_TRANSPORT_VERSION, "git_transport");

	if (remote->transport) {
		giterr_set(GITERR_NET, "A transport is already bound to this remote");
		return -1;
	}

	remote->transport = transport;
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
	git_reference *reference,
	const char *old_remote_name,
	const char *new_remote_name)
{
	int error;
	git_buf new_name = GIT_BUF_INIT;
	git_buf log_message = GIT_BUF_INIT;

	if ((error = git_buf_printf(
					&new_name,
					GIT_REFS_REMOTES_DIR "%s%s",
					new_remote_name,
					reference->name + strlen(GIT_REFS_REMOTES_DIR) + strlen(old_remote_name))) < 0)
		goto cleanup;

	if ((error = git_buf_printf(&log_message,
					"renamed remote %s to %s",
					old_remote_name, new_remote_name)) < 0)
		goto cleanup;

	error = git_reference_rename(
		NULL, reference, git_buf_cstr(&new_name), 0,
		NULL, git_buf_cstr(&log_message));
	git_reference_free(reference);

cleanup:
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
	git_reference *ref;
	git_reference_iterator *iter;

	if ((error = git_reference_iterator_new(&iter, repo)) < 0)
		return error;

	while ((error = git_reference_next(&ref, iter)) == 0) {
		if (git__prefixcmp(ref->name, GIT_REFS_REMOTES_DIR)) {
			git_reference_free(ref);
			continue;
		}

		if ((error = rename_one_remote_reference(ref, old_name, new_name)) < 0)
			break;
	}

	git_reference_iterator_free(iter);

	return (error == GIT_ITEROVER) ? 0 : error;
}

static int rename_fetch_refspecs(
	git_remote *remote,
	const char *new_name,
	int (*callback)(const char *problematic_refspec, void *payload),
	void *payload)
{
	git_config *config;
	git_buf base = GIT_BUF_INIT, var = GIT_BUF_INIT, val = GIT_BUF_INIT;
	const git_refspec *spec;
	size_t i;
	int error = 0;

	if ((error = git_repository_config__weakptr(&config, remote->repo)) < 0)
		return error;

	if ((error = git_buf_printf(
			&base, "+refs/heads/*:refs/remotes/%s/*", remote->name)) < 0)
		return error;

	git_vector_foreach(&remote->refspecs, i, spec) {
		if (spec->push)
			continue;

		/* Every refspec is a problem refspec for an anonymous remote, OR */
		/* Does the dst part of the refspec follow the expected format? */
		if (!remote->name ||
			strcmp(git_buf_cstr(&base), spec->string)) {

			if ((error = callback(spec->string, payload)) != 0) {
				giterr_set_after_callback(error);
				break;
			}

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
	return error;
}

int git_remote_rename(
	git_remote *remote,
	const char *new_name,
	git_remote_rename_problem_cb callback,
	void *payload)
{
	int error;

	assert(remote && new_name);

	if (!remote->name) {
		giterr_set(GITERR_INVALID, "Can't rename an anonymous remote.");
		return GIT_EINVALIDSPEC;
	}

	if ((error = ensure_remote_name_is_valid(new_name)) < 0)
		return error;

	if (remote->repo) {
		if ((error = ensure_remote_doesnot_exist(remote->repo, new_name)) < 0)
			return error;

		if (!remote->name) {
			if ((error = rename_fetch_refspecs(
				remote,
				new_name,
				callback,
				payload)) < 0)
					return error;

			remote->name = git__strdup(new_name);
			GITERR_CHECK_ALLOC(remote->name);

			return git_remote_save(remote);
		}

		if ((error = rename_remote_config_section(
			remote->repo,
			remote->name,
			new_name)) < 0)
				return error;

		if ((error = update_branch_remote_config_entry(
			remote->repo,
			remote->name,
			new_name)) < 0)
				return error;

		if ((error = rename_remote_references(
			remote->repo,
			remote->name,
			new_name)) < 0)
				return error;

		if ((error = rename_fetch_refspecs(
			remote,
			new_name,
			callback,
			payload)) < 0)
				return error;
	}

	git__free(remote->name);

	remote->name = git__strdup(new_name);
	GITERR_CHECK_ALLOC(remote->name);

	return 0;
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

static int add_and_dwim(git_remote *remote, const char *str, int push)
{
	git_refspec *spec;
	git_vector *vec;

	if (add_refspec(remote, str, !push) < 0)
		return -1;

	vec = &remote->refspecs;
	spec = git_vector_get(vec, vec->length - 1);
	return git_refspec__dwim_one(&remote->active_refspecs, spec, &remote->refs);
}

int git_remote_add_fetch(git_remote *remote, const char *refspec)
{
	return add_and_dwim(remote, refspec, false);
}

int git_remote_add_push(git_remote *remote, const char *refspec)
{
	return add_and_dwim(remote, refspec, true);
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

	free_refspecs(&remote->active_refspecs);
	git_vector_clear(&remote->active_refspecs);

	return dwim_refspecs(&remote->active_refspecs, &remote->refspecs, &remote->refs);
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

static int remove_refs(git_repository *repo, const char *glob)
{
	git_reference_iterator *iter;
	const char *name;
	int error;

	if ((error = git_reference_iterator_glob_new(&iter, repo, glob)) < 0)
		return error;

	while ((error = git_reference_next_name(&name, iter)) == 0) {
		if ((error = git_reference_remove(repo, name)) < 0)
			break;
	}
	git_reference_iterator_free(iter);

	if (error == GIT_ITEROVER)
		error = 0;

	return error;
}

static int remove_remote_tracking(git_repository *repo, const char *remote_name)
{
	git_remote *remote;
	int error;
	size_t i, count;

	/* we want to use what's on the config, regardless of changes to the instance in memory */
	if ((error = git_remote_load(&remote, repo, remote_name)) < 0)
		return error;

	count = git_remote_refspec_count(remote);
	for (i = 0; i < count; i++) {
		const git_refspec *refspec = git_remote_get_refspec(remote, i);

		/* shouldn't ever actually happen */
		if (refspec == NULL)
			continue;

		if ((error = remove_refs(repo, git_refspec_dst(refspec))) < 0)
			break;
	}

	git_remote_free(remote);
	return error;
}

int git_remote_delete(git_remote *remote)
{
	int error;
	git_repository *repo;

	assert(remote);

	if (!remote->name) {
		giterr_set(GITERR_INVALID, "Can't delete an anonymous remote.");
		return -1;
	}

	repo = git_remote_owner(remote);

	if ((error = remove_branch_config_related_entries(repo,
		git_remote_name(remote))) < 0)
		return error;

	if ((error = remove_remote_tracking(repo, git_remote_name(remote))) < 0)
		return error;

	if ((error = rename_remote_config_section(
		repo, git_remote_name(remote), NULL)) < 0)
		return error;

	git_remote_free(remote);

	return 0;
}
