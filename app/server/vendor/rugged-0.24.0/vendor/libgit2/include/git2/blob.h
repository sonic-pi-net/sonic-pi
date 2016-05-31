/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_blob_h__
#define INCLUDE_git_blob_h__

#include "common.h"
#include "types.h"
#include "oid.h"
#include "object.h"
#include "buffer.h"

/**
 * @file git2/blob.h
 * @brief Git blob load and write routines
 * @defgroup git_blob Git blob load and write routines
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

/**
 * Lookup a blob object from a repository.
 *
 * @param blob pointer to the looked up blob
 * @param repo the repo to use when locating the blob.
 * @param id identity of the blob to locate.
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_blob_lookup(git_blob **blob, git_repository *repo, const git_oid *id);

/**
 * Lookup a blob object from a repository,
 * given a prefix of its identifier (short id).
 *
 * @see git_object_lookup_prefix
 *
 * @param blob pointer to the looked up blob
 * @param repo the repo to use when locating the blob.
 * @param id identity of the blob to locate.
 * @param len the length of the short identifier
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_blob_lookup_prefix(git_blob **blob, git_repository *repo, const git_oid *id, size_t len);

/**
 * Close an open blob
 *
 * This is a wrapper around git_object_free()
 *
 * IMPORTANT:
 * It *is* necessary to call this method when you stop
 * using a blob. Failure to do so will cause a memory leak.
 *
 * @param blob the blob to close
 */
GIT_EXTERN(void) git_blob_free(git_blob *blob);

/**
 * Get the id of a blob.
 *
 * @param blob a previously loaded blob.
 * @return SHA1 hash for this blob.
 */
GIT_EXTERN(const git_oid *) git_blob_id(const git_blob *blob);

/**
 * Get the repository that contains the blob.
 *
 * @param blob A previously loaded blob.
 * @return Repository that contains this blob.
 */
GIT_EXTERN(git_repository *) git_blob_owner(const git_blob *blob);

/**
 * Get a read-only buffer with the raw content of a blob.
 *
 * A pointer to the raw content of a blob is returned;
 * this pointer is owned internally by the object and shall
 * not be free'd. The pointer may be invalidated at a later
 * time.
 *
 * @param blob pointer to the blob
 * @return the pointer
 */
GIT_EXTERN(const void *) git_blob_rawcontent(const git_blob *blob);

/**
 * Get the size in bytes of the contents of a blob
 *
 * @param blob pointer to the blob
 * @return size on bytes
 */
GIT_EXTERN(git_off_t) git_blob_rawsize(const git_blob *blob);

/**
 * Get a buffer with the filtered content of a blob.
 *
 * This applies filters as if the blob was being checked out to the
 * working directory under the specified filename.  This may apply
 * CRLF filtering or other types of changes depending on the file
 * attributes set for the blob and the content detected in it.
 *
 * The output is written into a `git_buf` which the caller must free
 * when done (via `git_buf_free`).
 *
 * If no filters need to be applied, then the `out` buffer will just
 * be populated with a pointer to the raw content of the blob.  In
 * that case, be careful to *not* free the blob until done with the
 * buffer or copy it into memory you own.
 *
 * @param out The git_buf to be filled in
 * @param blob Pointer to the blob
 * @param as_path Path used for file attribute lookups, etc.
 * @param check_for_binary_data Should this test if blob content contains
 *        NUL bytes / looks like binary data before applying filters?
 * @return 0 on success or an error code
 */
GIT_EXTERN(int) git_blob_filtered_content(
	git_buf *out,
	git_blob *blob,
	const char *as_path,
	int check_for_binary_data);

/**
 * Read a file from the working folder of a repository
 * and write it to the Object Database as a loose blob
 *
 * @param id return the id of the written blob
 * @param repo repository where the blob will be written.
 *	this repository cannot be bare
 * @param relative_path file from which the blob will be created,
 *	relative to the repository's working dir
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_blob_create_fromworkdir(git_oid *id, git_repository *repo, const char *relative_path);

/**
 * Read a file from the filesystem and write its content
 * to the Object Database as a loose blob
 *
 * @param id return the id of the written blob
 * @param repo repository where the blob will be written.
 *	this repository can be bare or not
 * @param path file from which the blob will be created
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_blob_create_fromdisk(git_oid *id, git_repository *repo, const char *path);


typedef int (*git_blob_chunk_cb)(char *content, size_t max_length, void *payload);

/**
 * Write a loose blob to the Object Database from a
 * provider of chunks of data.
 *
 * If the `hintpath` parameter is filled, it will be used to determine
 * what git filters should be applied to the object before it is written
 * to the object database.
 *
 * The implementation of the callback MUST respect the following rules:
 *
 *  - `content` must be filled by the callback. The maximum number of
 *    bytes that the buffer can accept per call is defined by the
 *    `max_length` parameter. Allocation and freeing of the buffer will
 *    be taken care of by libgit2.
 *
 *  - The `callback` must return the number of bytes that have been
 *    written to the `content` buffer.
 *
 *  - When there is no more data to stream, `callback` should return 0.
 *    This will prevent it from being invoked anymore.
 *
 *  - If an error occurs, the callback should return a negative value.
 *    This value will be returned to the caller.
 *
 * @param id Return the id of the written blob
 * @param repo Repository where the blob will be written.
 *        This repository can be bare or not.
 * @param hintpath If not NULL, will be used to select data filters
 *        to apply onto the content of the blob to be created.
 * @return 0 or error code (from either libgit2 or callback function)
 */
GIT_EXTERN(int) git_blob_create_fromchunks(
	git_oid *id,
	git_repository *repo,
	const char *hintpath,
	git_blob_chunk_cb callback,
	void *payload);

/**
 * Write an in-memory buffer to the ODB as a blob
 *
 * @param id return the id of the written blob
 * @param repo repository where to blob will be written
 * @param buffer data to be written into the blob
 * @param len length of the data
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_blob_create_frombuffer(
	git_oid *id, git_repository *repo, const void *buffer, size_t len);

/**
 * Determine if the blob content is most certainly binary or not.
 *
 * The heuristic used to guess if a file is binary is taken from core git:
 * Searching for NUL bytes and looking for a reasonable ratio of printable
 * to non-printable characters among the first 8000 bytes.
 *
 * @param blob The blob which content should be analyzed
 * @return 1 if the content of the blob is detected
 * as binary; 0 otherwise.
 */
GIT_EXTERN(int) git_blob_is_binary(const git_blob *blob);

/** @} */
GIT_END_DECL
#endif
