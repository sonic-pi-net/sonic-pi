/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_clone_h__
#define INCLUDE_git_clone_h__

#include "common.h"
#include "types.h"
#include "indexer.h"
#include "checkout.h"
#include "remote.h"


/**
 * @file git2/clone.h
 * @brief Git cloning routines
 * @defgroup git_clone Git cloning routines
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

/**
 * Options for bypassing the git-aware transport on clone. Bypassing
 * it means that instead of a fetch, libgit2 will copy the object
 * database directory instead of figuring out what it needs, which is
 * faster. If possible, it will hardlink the files to save space.
 */
typedef enum {
	/**
	 * Auto-detect (default), libgit2 will bypass the git-aware
	 * transport for local paths, but use a normal fetch for
	 * `file://` urls.
	 */
	GIT_CLONE_LOCAL_AUTO,
	/**
	 * Bypass the git-aware transport even for a `file://` url.
	 */
	GIT_CLONE_LOCAL,
	/**
	 * Do no bypass the git-aware transport
	 */
	GIT_CLONE_NO_LOCAL,
	/**
	 * Bypass the git-aware transport, but do not try to use
	 * hardlinks.
	 */
	GIT_CLONE_LOCAL_NO_LINKS,
} git_clone_local_t;

/**
 * Clone options structure
 *
 * Use the GIT_CLONE_OPTIONS_INIT to get the default settings, like this:
 *
 *		git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
 */

typedef struct git_clone_options {
	unsigned int version;

	/**
	 * These options are passed to the checkout step. To disable
	 * checkout, set the `checkout_strategy` to
	 * `GIT_CHECKOUT_NONE`. Generally you will want the use
	 * GIT_CHECKOUT_SAFE_CREATE to create all files in the working
	 * directory for the newly cloned repository.
	 */
	git_checkout_options checkout_opts;

	/**
	 * Callbacks to use for reporting fetch progress.
	 */
	git_remote_callbacks remote_callbacks;

	/**
	 * Set to zero (false) to create a standard repo, or non-zero
	 * for a bare repo
	 */
	int bare;

	/**
	 * Set to 1 if errors validating the remote host's certificate
	 * should be ignored.
	 */
	int ignore_cert_errors;

	/**
	 * Whether to use a fetch or copy the object database.
	 */
	git_clone_local_t local;

	/**
	 * The name to be given to the remote that will be
	 * created. The default is "origin".
	 */
	const char *remote_name;

	/**
	 * The name of the branch to checkout. NULL means use the
	 * remote's default branch.
	 */
	const char* checkout_branch;

	/**
	 * The identity used when updating the reflog. NULL means to
	 * use the default signature using the config.
	 */
	git_signature *signature;
} git_clone_options;

#define GIT_CLONE_OPTIONS_VERSION 1
#define GIT_CLONE_OPTIONS_INIT {GIT_CLONE_OPTIONS_VERSION, {GIT_CHECKOUT_OPTIONS_VERSION, GIT_CHECKOUT_SAFE_CREATE}, GIT_REMOTE_CALLBACKS_INIT}

/**
 * Initializes a `git_clone_options` with default values. Equivalent to
 * creating an instance with GIT_CLONE_OPTIONS_INIT.
 *
 * @param opts The `git_clone_options` struct to initialize
 * @param version Version of struct; pass `GIT_CLONE_OPTIONS_VERSION`
 * @return Zero on success; -1 on failure.
 */
GIT_EXTERN(int) git_clone_init_options(
	git_clone_options *opts,
	unsigned int version);

/**
 * Clone a remote repository.
 *
 * This version handles the simple case. If you'd like to create the
 * repository or remote with non-default settings, you can create and
 * configure them and then use `git_clone_into()`.
 *
 * @param out pointer that will receive the resulting repository object
 * @param url the remote repository to clone
 * @param local_path local directory to clone to
 * @param options configuration options for the clone.  If NULL, the
 *        function works as though GIT_OPTIONS_INIT were passed.
 * @return 0 on success, any non-zero return value from a callback
 *         function, or a negative value to indicate an error (use
 *         `giterr_last` for a detailed error message)
 */
GIT_EXTERN(int) git_clone(
	git_repository **out,
	const char *url,
	const char *local_path,
	const git_clone_options *options);

/**
 * Clone into a repository
 *
 * After creating the repository and remote and configuring them for
 * paths and callbacks respectively, you can call this function to
 * perform the clone operation and optionally checkout files.
 *
 * @param repo the repository to use
 * @param remote the remote repository to clone from
 * @param co_opts options to use during checkout
 * @param branch the branch to checkout after the clone, pass NULL for the
 *        remote's default branch
 * @param signature The identity used when updating the reflog.
 * @return 0 on success, any non-zero return value from a callback
 *         function, or a negative value to indicate an error (use
 *         `giterr_last` for a detailed error message)
 */
GIT_EXTERN(int) git_clone_into(
	git_repository *repo,
	git_remote *remote,
	const git_checkout_options *co_opts,
	const char *branch,
	const git_signature *signature);

/**
 * Perform a local clone into a repository
 *
 * A "local clone" bypasses any git-aware protocols and simply copies
 * over the object database from the source repository. It is often
 * faster than a git-aware clone, but no verification of the data is
 * performed, and can copy over too much data.
 *
 * @param repo the repository to use
 * @param remote the remote repository to clone from
 * @param co_opts options to use during checkout
 * @param branch the branch to checkout after the clone, pass NULL for the
 *        remote's default branch
 * @param link wether to use hardlinks instead of copying
 * objects. This is only possible if both repositories are on the same
 * filesystem.
 * @param signature the identity used when updating the reflog
 * @return 0 on success, any non-zero return value from a callback
 *         function, or a negative value to indicate an error (use
 *         `giterr_last` for a detailed error message)
 */
GIT_EXTERN(int) git_clone_local_into(
	git_repository *repo,
	git_remote *remote,
	const git_checkout_options *co_opts,
	const char *branch,
	int link,
	const git_signature *signature);

/** @} */
GIT_END_DECL
#endif
