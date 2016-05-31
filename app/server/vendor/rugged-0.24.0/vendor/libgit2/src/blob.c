/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2/common.h"
#include "git2/object.h"
#include "git2/repository.h"
#include "git2/odb_backend.h"

#include "common.h"
#include "filebuf.h"
#include "blob.h"
#include "filter.h"
#include "buf_text.h"

const void *git_blob_rawcontent(const git_blob *blob)
{
	assert(blob);
	return git_odb_object_data(blob->odb_object);
}

git_off_t git_blob_rawsize(const git_blob *blob)
{
	assert(blob);
	return (git_off_t)git_odb_object_size(blob->odb_object);
}

int git_blob__getbuf(git_buf *buffer, git_blob *blob)
{
	return git_buf_set(
		buffer,
		git_odb_object_data(blob->odb_object),
		git_odb_object_size(blob->odb_object));
}

void git_blob__free(void *blob)
{
	git_odb_object_free(((git_blob *)blob)->odb_object);
	git__free(blob);
}

int git_blob__parse(void *blob, git_odb_object *odb_obj)
{
	assert(blob);
	git_cached_obj_incref((git_cached_obj *)odb_obj);
	((git_blob *)blob)->odb_object = odb_obj;
	return 0;
}

int git_blob_create_frombuffer(
	git_oid *id, git_repository *repo, const void *buffer, size_t len)
{
	int error;
	git_odb *odb;
	git_odb_stream *stream;

	assert(id && repo);

	if ((error = git_repository_odb__weakptr(&odb, repo)) < 0 ||
		(error = git_odb_open_wstream(&stream, odb, len, GIT_OBJ_BLOB)) < 0)
		return error;

	if ((error = git_odb_stream_write(stream, buffer, len)) == 0)
		error = git_odb_stream_finalize_write(id, stream);

	git_odb_stream_free(stream);
	return error;
}

static int write_file_stream(
	git_oid *id, git_odb *odb, const char *path, git_off_t file_size)
{
	int fd, error;
	char buffer[FILEIO_BUFSIZE];
	git_odb_stream *stream = NULL;
	ssize_t read_len = -1;
	git_off_t written = 0;

	if ((error = git_odb_open_wstream(
			&stream, odb, file_size, GIT_OBJ_BLOB)) < 0)
		return error;

	if ((fd = git_futils_open_ro(path)) < 0) {
		git_odb_stream_free(stream);
		return -1;
	}

	while (!error && (read_len = p_read(fd, buffer, sizeof(buffer))) > 0) {
		error = git_odb_stream_write(stream, buffer, read_len);
		written += read_len;
	}

	p_close(fd);

	if (written != file_size || read_len < 0) {
		giterr_set(GITERR_OS, "Failed to read file into stream");
		error = -1;
	}

	if (!error)
		error = git_odb_stream_finalize_write(id, stream);

	git_odb_stream_free(stream);
	return error;
}

static int write_file_filtered(
	git_oid *id,
	git_off_t *size,
	git_odb *odb,
	const char *full_path,
	git_filter_list *fl)
{
	int error;
	git_buf tgt = GIT_BUF_INIT;

	error = git_filter_list_apply_to_file(&tgt, fl, NULL, full_path);

	/* Write the file to disk if it was properly filtered */
	if (!error) {
		*size = tgt.size;

		error = git_odb_write(id, odb, tgt.ptr, tgt.size, GIT_OBJ_BLOB);
	}

	git_buf_free(&tgt);
	return error;
}

static int write_symlink(
	git_oid *id, git_odb *odb, const char *path, size_t link_size)
{
	char *link_data;
	ssize_t read_len;
	int error;

	link_data = git__malloc(link_size);
	GITERR_CHECK_ALLOC(link_data);

	read_len = p_readlink(path, link_data, link_size);
	if (read_len != (ssize_t)link_size) {
		giterr_set(GITERR_OS, "Failed to create blob.  Can't read symlink '%s'", path);
		git__free(link_data);
		return -1;
	}

	error = git_odb_write(id, odb, (void *)link_data, link_size, GIT_OBJ_BLOB);
	git__free(link_data);
	return error;
}

int git_blob__create_from_paths(
	git_oid *id,
	struct stat *out_st,
	git_repository *repo,
	const char *content_path,
	const char *hint_path,
	mode_t hint_mode,
	bool try_load_filters)
{
	int error;
	struct stat st;
	git_odb *odb = NULL;
	git_off_t size;
	mode_t mode;
	git_buf path = GIT_BUF_INIT;

	assert(hint_path || !try_load_filters);

	if (!content_path) {
		if (git_repository__ensure_not_bare(repo, "create blob from file") < 0)
			return GIT_EBAREREPO;

		if (git_buf_joinpath(
				&path, git_repository_workdir(repo), hint_path) < 0)
			return -1;

		content_path = path.ptr;
	}

	if ((error = git_path_lstat(content_path, &st)) < 0 ||
		(error = git_repository_odb(&odb, repo)) < 0)
		goto done;

	if (S_ISDIR(st.st_mode)) {
		giterr_set(GITERR_ODB, "cannot create blob from '%s'; it is a directory", content_path);
		error = GIT_EDIRECTORY;
		goto done;
	}

	if (out_st)
		memcpy(out_st, &st, sizeof(st));

	size = st.st_size;
	mode = hint_mode ? hint_mode : st.st_mode;

	if (S_ISLNK(mode)) {
		error = write_symlink(id, odb, content_path, (size_t)size);
	} else {
		git_filter_list *fl = NULL;

		if (try_load_filters)
			/* Load the filters for writing this file to the ODB */
			error = git_filter_list_load(
				&fl, repo, NULL, hint_path,
				GIT_FILTER_TO_ODB, GIT_FILTER_DEFAULT);

		if (error < 0)
			/* well, that didn't work */;
		else if (fl == NULL)
			/* No filters need to be applied to the document: we can stream
			 * directly from disk */
			error = write_file_stream(id, odb, content_path, size);
		else {
			/* We need to apply one or more filters */
			error = write_file_filtered(id, &size, odb, content_path, fl);

			git_filter_list_free(fl);
		}

		/*
		 * TODO: eventually support streaming filtered files, for files
		 * which are bigger than a given threshold. This is not a priority
		 * because applying a filter in streaming mode changes the final
		 * size of the blob, and without knowing its final size, the blob
		 * cannot be written in stream mode to the ODB.
		 *
		 * The plan is to do streaming writes to a tempfile on disk and then
		 * opening streaming that file to the ODB, using
		 * `write_file_stream`.
		 *
		 * CAREFULLY DESIGNED APIS YO
		 */
	}

done:
	git_odb_free(odb);
	git_buf_free(&path);

	return error;
}

int git_blob_create_fromworkdir(
	git_oid *id, git_repository *repo, const char *path)
{
	return git_blob__create_from_paths(id, NULL, repo, NULL, path, 0, true);
}

int git_blob_create_fromdisk(
	git_oid *id, git_repository *repo, const char *path)
{
	int error;
	git_buf full_path = GIT_BUF_INIT;
	const char *workdir, *hintpath;

	if ((error = git_path_prettify(&full_path, path, NULL)) < 0) {
		git_buf_free(&full_path);
		return error;
	}

	hintpath = git_buf_cstr(&full_path);
	workdir  = git_repository_workdir(repo);

	if (workdir && !git__prefixcmp(hintpath, workdir))
		hintpath += strlen(workdir);

	error = git_blob__create_from_paths(
		id, NULL, repo, git_buf_cstr(&full_path), hintpath, 0, true);

	git_buf_free(&full_path);
	return error;
}

#define BUFFER_SIZE 4096

int git_blob_create_fromchunks(
	git_oid *id,
	git_repository *repo,
	const char *hintpath,
	int (*source_cb)(char *content, size_t max_length, void *payload),
	void *payload)
{
	int error;
	char *content = NULL;
	git_filebuf file = GIT_FILEBUF_INIT;
	git_buf path = GIT_BUF_INIT;

	assert(id && repo && source_cb);

	if ((error = git_buf_joinpath(
			&path, git_repository_path(repo), GIT_OBJECTS_DIR "streamed")) < 0)
		goto cleanup;

	content = git__malloc(BUFFER_SIZE);
	GITERR_CHECK_ALLOC(content);

	if ((error = git_filebuf_open(
			&file, git_buf_cstr(&path), GIT_FILEBUF_TEMPORARY, 0666)) < 0)
		goto cleanup;

	while (1) {
		int read_bytes = source_cb(content, BUFFER_SIZE, payload);

		if (!read_bytes)
			break;

		if (read_bytes > BUFFER_SIZE) {
			giterr_set(GITERR_OBJECT, "Invalid chunk size while creating blob");
			error = GIT_EBUFS;
		} else if (read_bytes < 0) {
			error = giterr_set_after_callback(read_bytes);
		} else {
			error = git_filebuf_write(&file, content, read_bytes);
		}

		if (error < 0)
			goto cleanup;
	}

	if ((error = git_filebuf_flush(&file)) < 0)
		goto cleanup;

	error = git_blob__create_from_paths(
		id, NULL, repo, file.path_lock, hintpath, 0, hintpath != NULL);

cleanup:
	git_buf_free(&path);
	git_filebuf_cleanup(&file);
	git__free(content);

	return error;
}

int git_blob_is_binary(const git_blob *blob)
{
	git_buf content = GIT_BUF_INIT;

	assert(blob);

	git_buf_attach_notowned(&content, blob->odb_object->buffer,
		min(blob->odb_object->cached.size,
		GIT_FILTER_BYTES_TO_CHECK_NUL));
	return git_buf_text_is_binary(&content);
}

int git_blob_filtered_content(
	git_buf *out,
	git_blob *blob,
	const char *path,
	int check_for_binary_data)
{
	int error = 0;
	git_filter_list *fl = NULL;

	assert(blob && path && out);

	git_buf_sanitize(out);

	if (check_for_binary_data && git_blob_is_binary(blob))
		return 0;

	if (!(error = git_filter_list_load(
			&fl, git_blob_owner(blob), blob, path,
			GIT_FILTER_TO_WORKTREE, GIT_FILTER_DEFAULT))) {

		error = git_filter_list_apply_to_blob(out, fl, blob);

		git_filter_list_free(fl);
	}

	return error;
}
