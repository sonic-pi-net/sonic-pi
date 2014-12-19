/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_push_h__
#define INCLUDE_git_push_h__

#include "common.h"
#include "pack.h"

/**
 * @file git2/push.h
 * @brief Git push management functions
 * @defgroup git_push push management functions
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

/**
 * Controls the behavior of a git_push object.
 */
typedef struct {
	unsigned int version;

	/**
	 * If the transport being used to push to the remote requires the creation
	 * of a pack file, this controls the number of worker threads used by
	 * the packbuilder when creating that pack file to be sent to the remote.
	 *
	 * If set to 0, the packbuilder will auto-detect the number of threads
	 * to create. The default value is 1.
	 */
	unsigned int pb_parallelism;
} git_push_options;

#define GIT_PUSH_OPTIONS_VERSION 1
#define GIT_PUSH_OPTIONS_INIT { GIT_PUSH_OPTIONS_VERSION }

/**
 * Initializes a `git_push_options` with default values. Equivalent to
 * creating an instance with GIT_PUSH_OPTIONS_INIT.
 *
 * @param opts the `git_push_options` instance to initialize.
 * @param version the version of the struct; you should pass
 *        `GIT_PUSH_OPTIONS_VERSION` here.
 * @return Zero on success; -1 on failure.
 */
GIT_EXTERN(int) git_push_init_options(
	git_push_options *opts,
	unsigned int version);

/** Push network progress notification function */
typedef int (*git_push_transfer_progress)(
	unsigned int current,
	unsigned int total,
	size_t bytes,
	void* payload);

/**
 * Create a new push object
 *
 * @param out New push object
 * @param remote Remote instance
 *
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_push_new(git_push **out, git_remote *remote);

/**
 * Set options on a push object
 *
 * @param push The push object
 * @param opts The options to set on the push object
 *
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_push_set_options(
	git_push *push,
	const git_push_options *opts);

/**
 * Set the callbacks for a push
 *
 * @param push The push object
 * @param pack_progress_cb Function to call with progress information during
 * pack building. Be aware that this is called inline with pack building
 * operations, so performance may be affected.
 * @param pack_progress_cb_payload Payload for the pack progress callback.
 * @param transfer_progress_cb Function to call with progress information during
 * the upload portion of a push. Be aware that this is called inline with
 * pack building operations, so performance may be affected.
 * @param transfer_progress_cb_payload Payload for the network progress callback.
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_push_set_callbacks(
	git_push *push,
	git_packbuilder_progress pack_progress_cb,
	void *pack_progress_cb_payload,
	git_push_transfer_progress transfer_progress_cb,
	void *transfer_progress_cb_payload);

/**
 * Add a refspec to be pushed
 *
 * @param push The push object
 * @param refspec Refspec string
 *
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_push_add_refspec(git_push *push, const char *refspec);

/**
 * Update remote tips after a push
 *
 * @param push The push object
 * @param signature The identity to use when updating reflogs
 * @param reflog_message The message to insert into the reflogs. If NULL, the
 *                       default is "update by push".
 *
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_push_update_tips(
		git_push *push,
		const git_signature *signature,
		const char *reflog_message);

/**
 * Perform the push
 *
 * This function will return an error in case of a protocol error or
 * the server being unable to unpack the data we sent.
 *
 * The return value does not reflect whether the server accepted or
 * refused any reference updates. Use `git_push_status_foreach()` in
 * order to find out which updates were accepted or rejected.
 *
 * @param push The push object
 *
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_push_finish(git_push *push);

/**
 * Invoke callback `cb' on each status entry
 *
 * For each of the updated references, we receive a status report in the
 * form of `ok refs/heads/master` or `ng refs/heads/master <msg>`.
 * `msg != NULL` means the reference has not been updated for the given
 * reason.
 *
 * Return a non-zero value from the callback to stop the loop.
 *
 * @param push The push object
 * @param cb The callback to call on each object
 *
 * @return 0 on success, non-zero callback return value, or error code
 */
GIT_EXTERN(int) git_push_status_foreach(git_push *push,
			int (*cb)(const char *ref, const char *msg, void *data),
			void *data);

/**
 * Free the given push object
 *
 * @param push The push object
 */
GIT_EXTERN(void) git_push_free(git_push *push);

/** @} */
GIT_END_DECL
#endif
