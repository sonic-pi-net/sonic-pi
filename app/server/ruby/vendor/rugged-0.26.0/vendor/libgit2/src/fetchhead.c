/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "fetchhead.h"

#include "git2/types.h"
#include "git2/oid.h"

#include "buffer.h"
#include "fileops.h"
#include "filebuf.h"
#include "refs.h"
#include "repository.h"

int git_fetchhead_ref_cmp(const void *a, const void *b)
{
	const git_fetchhead_ref *one = (const git_fetchhead_ref *)a;
	const git_fetchhead_ref *two = (const git_fetchhead_ref *)b;

	if (one->is_merge && !two->is_merge)
		return -1;
	if (two->is_merge && !one->is_merge)
		return 1;

	if (one->ref_name && two->ref_name)
		return strcmp(one->ref_name, two->ref_name);
	else if (one->ref_name)
		return -1;
	else if (two->ref_name)
		return 1;

	return 0;
}

int git_fetchhead_ref_create(
	git_fetchhead_ref **out,
	git_oid *oid,
	unsigned int is_merge,
	const char *ref_name,
	const char *remote_url)
{
	git_fetchhead_ref *fetchhead_ref;

	assert(out && oid);

	*out = NULL;

	fetchhead_ref = git__malloc(sizeof(git_fetchhead_ref));
	GITERR_CHECK_ALLOC(fetchhead_ref);

	memset(fetchhead_ref, 0x0, sizeof(git_fetchhead_ref));

	git_oid_cpy(&fetchhead_ref->oid, oid);
	fetchhead_ref->is_merge = is_merge;

	if (ref_name)
		fetchhead_ref->ref_name = git__strdup(ref_name);

	if (remote_url)
		fetchhead_ref->remote_url = git__strdup(remote_url);

	*out = fetchhead_ref;

	return 0;
}

static int fetchhead_ref_write(
	git_filebuf *file,
	git_fetchhead_ref *fetchhead_ref)
{
	char oid[GIT_OID_HEXSZ + 1];
	const char *type, *name;
	int head = 0;

	assert(file && fetchhead_ref);

	git_oid_fmt(oid, &fetchhead_ref->oid);
	oid[GIT_OID_HEXSZ] = '\0';

	if (git__prefixcmp(fetchhead_ref->ref_name, GIT_REFS_HEADS_DIR) == 0) {
		type = "branch ";
		name = fetchhead_ref->ref_name + strlen(GIT_REFS_HEADS_DIR);
	} else if(git__prefixcmp(fetchhead_ref->ref_name,
		GIT_REFS_TAGS_DIR) == 0) {
		type = "tag ";
		name = fetchhead_ref->ref_name + strlen(GIT_REFS_TAGS_DIR);
	} else if (!git__strcmp(fetchhead_ref->ref_name, GIT_HEAD_FILE)) {
		head = 1;
	} else {
		type = "";
		name = fetchhead_ref->ref_name;
	}

	if (head)
		return git_filebuf_printf(file, "%s\t\t%s\n", oid, fetchhead_ref->remote_url);

	return git_filebuf_printf(file, "%s\t%s\t%s'%s' of %s\n",
		oid,
		(fetchhead_ref->is_merge) ? "" : "not-for-merge",
		type,
		name,
		fetchhead_ref->remote_url);
}

int git_fetchhead_write(git_repository *repo, git_vector *fetchhead_refs)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	git_buf path = GIT_BUF_INIT;
	unsigned int i;
	git_fetchhead_ref *fetchhead_ref;

	assert(repo && fetchhead_refs);

	if (git_buf_joinpath(&path, repo->gitdir, GIT_FETCH_HEAD_FILE) < 0)
		return -1;

	if (git_filebuf_open(&file, path.ptr, GIT_FILEBUF_FORCE, GIT_REFS_FILE_MODE) < 0) {
		git_buf_free(&path);
		return -1;
	}

	git_buf_free(&path);

	git_vector_sort(fetchhead_refs);

	git_vector_foreach(fetchhead_refs, i, fetchhead_ref)
		fetchhead_ref_write(&file, fetchhead_ref);

	return git_filebuf_commit(&file);
}

static int fetchhead_ref_parse(
	git_oid *oid,
	unsigned int *is_merge,
	git_buf *ref_name,
	const char **remote_url,
	char *line,
	size_t line_num)
{
	char *oid_str, *is_merge_str, *desc, *name = NULL;
	const char *type = NULL;
	int error = 0;

	*remote_url = NULL;

	if (!*line) {
		giterr_set(GITERR_FETCHHEAD,
			"empty line in FETCH_HEAD line %"PRIuZ, line_num);
		return -1;
	}

	/* Compat with old git clients that wrote FETCH_HEAD like a loose ref. */
	if ((oid_str = git__strsep(&line, "\t")) == NULL) {
		oid_str = line;
		line += strlen(line);

		*is_merge = 1;
	}

	if (strlen(oid_str) != GIT_OID_HEXSZ) {
		giterr_set(GITERR_FETCHHEAD,
			"invalid object ID in FETCH_HEAD line %"PRIuZ, line_num);
		return -1;
	}

	if (git_oid_fromstr(oid, oid_str) < 0) {
		const git_error *oid_err = giterr_last();
		const char *err_msg = oid_err ? oid_err->message : "invalid object ID";

		giterr_set(GITERR_FETCHHEAD, "%s in FETCH_HEAD line %"PRIuZ,
			err_msg, line_num);
		return -1;
	}

	/* Parse new data from newer git clients */
	if (*line) {
		if ((is_merge_str = git__strsep(&line, "\t")) == NULL) {
			giterr_set(GITERR_FETCHHEAD,
				"invalid description data in FETCH_HEAD line %"PRIuZ, line_num);
			return -1;
		}

		if (*is_merge_str == '\0')
			*is_merge = 1;
		else if (strcmp(is_merge_str, "not-for-merge") == 0)
			*is_merge = 0;
		else {
			giterr_set(GITERR_FETCHHEAD,
				"invalid for-merge entry in FETCH_HEAD line %"PRIuZ, line_num);
			return -1;
		}

		if ((desc = line) == NULL) {
			giterr_set(GITERR_FETCHHEAD,
				"invalid description in FETCH_HEAD line %"PRIuZ, line_num);
			return -1;
		}

		if (git__prefixcmp(desc, "branch '") == 0) {
			type = GIT_REFS_HEADS_DIR;
			name = desc + 8;
		} else if (git__prefixcmp(desc, "tag '") == 0) {
			type = GIT_REFS_TAGS_DIR;
			name = desc + 5;
		} else if (git__prefixcmp(desc, "'") == 0)
			name = desc + 1;

		if (name) {
			if ((desc = strstr(name, "' ")) == NULL ||
				git__prefixcmp(desc, "' of ") != 0) {
				giterr_set(GITERR_FETCHHEAD,
					"invalid description in FETCH_HEAD line %"PRIuZ, line_num);
				return -1;
			}

			*desc = '\0';
			desc += 5;
		}

		*remote_url = desc;
	}

	git_buf_clear(ref_name);

	if (type)
		git_buf_join(ref_name, '/', type, name);
	else if(name)
		git_buf_puts(ref_name, name);

	return error;
}

int git_repository_fetchhead_foreach(git_repository *repo,
	git_repository_fetchhead_foreach_cb cb,
	void *payload)
{
	git_buf path = GIT_BUF_INIT, file = GIT_BUF_INIT, name = GIT_BUF_INIT;
	const char *ref_name;
	git_oid oid;
	const char *remote_url;
	unsigned int is_merge = 0;
	char *buffer, *line;
	size_t line_num = 0;
	int error = 0;

	assert(repo && cb);

	if (git_buf_joinpath(&path, repo->gitdir, GIT_FETCH_HEAD_FILE) < 0)
		return -1;

	if ((error = git_futils_readbuffer(&file, git_buf_cstr(&path))) < 0)
		goto done;

	buffer = file.ptr;

	while ((line = git__strsep(&buffer, "\n")) != NULL) {
		++line_num;

		if ((error = fetchhead_ref_parse(
				&oid, &is_merge, &name, &remote_url, line, line_num)) < 0)
			goto done;

		if (git_buf_len(&name) > 0)
			ref_name = git_buf_cstr(&name);
		else
			ref_name = NULL;

		error = cb(ref_name, remote_url, &oid, is_merge, payload);
		if (error) {
			giterr_set_after_callback(error);
			goto done;
		}
	}

	if (*buffer) {
		giterr_set(GITERR_FETCHHEAD, "no EOL at line %"PRIuZ, line_num+1);
		error = -1;
		goto done;
	}

done:
	git_buf_free(&file);
	git_buf_free(&path);
	git_buf_free(&name);

	return error;
}

void git_fetchhead_ref_free(git_fetchhead_ref *fetchhead_ref)
{
	if (fetchhead_ref == NULL)
		return;

	git__free(fetchhead_ref->remote_url);
	git__free(fetchhead_ref->ref_name);
	git__free(fetchhead_ref);
}

