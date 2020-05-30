/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_sys_git_refdb_backend_h__
#define INCLUDE_sys_git_refdb_backend_h__

#include "git2/common.h"
#include "git2/types.h"
#include "git2/oid.h"

/**
 * @file git2/refdb_backend.h
 * @brief Git custom refs backend functions
 * @defgroup git_refdb_backend Git custom refs backend API
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL


/**
 * Every backend's iterator must have a pointer to itself as the first
 * element, so the API can talk to it. You'd define your iterator as
 *
 *     struct my_iterator {
 *             git_reference_iterator parent;
 *             ...
 *     }
 *
 * and assign `iter->parent.backend` to your `git_refdb_backend`.
 */
struct git_reference_iterator {
	git_refdb *db;

	/**
	 * Return the current reference and advance the iterator.
	 */
	int GIT_CALLBACK(next)(
		git_reference **ref,
		git_reference_iterator *iter);

	/**
	 * Return the name of the current reference and advance the iterator
	 */
	int GIT_CALLBACK(next_name)(
		const char **ref_name,
		git_reference_iterator *iter);

	/**
	 * Free the iterator
	 */
	void GIT_CALLBACK(free)(
		git_reference_iterator *iter);
};

/** An instance for a custom backend */
struct git_refdb_backend {
	unsigned int version; /**< The backend API version */

	/**
	 * Queries the refdb backend for the existence of a reference.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(exists)(
		int *exists,
		git_refdb_backend *backend,
		const char *ref_name);

	/**
	 * Queries the refdb backend for a given reference.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(lookup)(
		git_reference **out,
		git_refdb_backend *backend,
		const char *ref_name);

	/**
	 * Allocate an iterator object for the backend.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(iterator)(
		git_reference_iterator **iter,
		struct git_refdb_backend *backend,
		const char *glob);

	/**
	 * Writes the given reference to the refdb.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(write)(git_refdb_backend *backend,
		     const git_reference *ref, int force,
		     const git_signature *who, const char *message,
		     const git_oid *old, const char *old_target);

	/**
	 * Rename a reference in the refdb.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(rename)(
		git_reference **out, git_refdb_backend *backend,
		const char *old_name, const char *new_name, int force,
		const git_signature *who, const char *message);

	/**
	 * Deletes the given reference from the refdb.
	 *
	 * If it exists, its reflog should be deleted as well.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(del)(git_refdb_backend *backend, const char *ref_name, const git_oid *old_id, const char *old_target);

	/**
	 * Suggests that the given refdb compress or optimize its references.
	 *
	 * This mechanism is implementation specific. For on-disk reference
	 * databases, this may pack all loose references.
	 *
	 * A refdb implementation may provide this function; if it is not
	 * provided, nothing will be done.
	 */
	int GIT_CALLBACK(compress)(git_refdb_backend *backend);

	/**
	 * Query whether a particular reference has a log (may be empty)
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(has_log)(git_refdb_backend *backend, const char *refname);

	/**
	 * Make sure a particular reference will have a reflog which
	 * will be appended to on writes.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(ensure_log)(git_refdb_backend *backend, const char *refname);

	/**
	 * Frees any resources held by the refdb (including the `git_refdb_backend`
	 * itself).
	 *
	 * A refdb backend implementation must provide this function.
	 */
	void GIT_CALLBACK(free)(git_refdb_backend *backend);

	/**
	 * Read the reflog for the given reference name.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(reflog_read)(git_reflog **out, git_refdb_backend *backend, const char *name);

	/**
	 * Write a reflog to disk.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(reflog_write)(git_refdb_backend *backend, git_reflog *reflog);

	/**
	 * Rename a reflog.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(reflog_rename)(git_refdb_backend *_backend, const char *old_name, const char *new_name);

	/**
	 * Remove a reflog.
	 *
	 * A refdb implementation must provide this function.
	 */
	int GIT_CALLBACK(reflog_delete)(git_refdb_backend *backend, const char *name);

	/**
	 * Lock a reference.
	 *
	 * The opaque parameter will be passed to the unlock function.
	 *
	 * A refdb implementation may provide this function; if it is not
	 * provided, the transaction API will fail to work.
	 */
	int GIT_CALLBACK(lock)(void **payload_out, git_refdb_backend *backend, const char *refname);

	/**
	 * Unlock a reference.
	 *
	 * Only one of target or symbolic_target will be set.
	 * `success` will be true if the reference should be update, false if
	 * the lock must be discarded.
	 *
	 * A refdb implementation must provide this function if a `lock`
	 * implementation is provided.
	 */
	int GIT_CALLBACK(unlock)(git_refdb_backend *backend, void *payload, int success, int update_reflog,
		      const git_reference *ref, const git_signature *sig, const char *message);
};

#define GIT_REFDB_BACKEND_VERSION 1
#define GIT_REFDB_BACKEND_INIT {GIT_REFDB_BACKEND_VERSION}

/**
 * Initializes a `git_refdb_backend` with default values. Equivalent to
 * creating an instance with GIT_REFDB_BACKEND_INIT.
 *
 * @param backend the `git_refdb_backend` struct to initialize
 * @param version Version of struct; pass `GIT_REFDB_BACKEND_VERSION`
 * @return Zero on success; -1 on failure.
 */
GIT_EXTERN(int) git_refdb_init_backend(
	git_refdb_backend *backend,
	unsigned int version);

/**
 * Constructors for default filesystem-based refdb backend
 *
 * Under normal usage, this is called for you when the repository is
 * opened / created, but you can use this to explicitly construct a
 * filesystem refdb backend for a repository.
 *
 * @param backend_out Output pointer to the git_refdb_backend object
 * @param repo Git repository to access
 * @return 0 on success, <0 error code on failure
 */
GIT_EXTERN(int) git_refdb_backend_fs(
	git_refdb_backend **backend_out,
	git_repository *repo);

/**
 * Sets the custom backend to an existing reference DB
 *
 * The `git_refdb` will take ownership of the `git_refdb_backend` so you
 * should NOT free it after calling this function.
 *
 * @param refdb database to add the backend to
 * @param backend pointer to a git_refdb_backend instance
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_refdb_set_backend(
	git_refdb *refdb,
	git_refdb_backend *backend);

GIT_END_DECL

#endif
