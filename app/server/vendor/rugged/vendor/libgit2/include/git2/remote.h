/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_remote_h__
#define INCLUDE_git_remote_h__

#include "common.h"
#include "repository.h"
#include "refspec.h"
#include "net.h"
#include "indexer.h"
#include "strarray.h"
#include "transport.h"

/**
 * @file git2/remote.h
 * @brief Git remote management functions
 * @defgroup git_remote remote management functions
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

typedef int (*git_remote_rename_problem_cb)(const char *problematic_refspec, void *payload);

/**
 * Add a remote with the default fetch refspec to the repository's configuration.  This
 * calls git_remote_save before returning.
 *
 * @param out the resulting remote
 * @param repo the repository in which to create the remote
 * @param name the remote's name
 * @param url the remote's url
 * @return 0, GIT_EINVALIDSPEC, GIT_EEXISTS or an error code
 */
GIT_EXTERN(int) git_remote_create(
		git_remote **out,
		git_repository *repo,
		const char *name,
		const char *url);

/**
 * Add a remote with the provided fetch refspec (or default if NULL) to the repository's
 * configuration.  This
 * calls git_remote_save before returning.
 *
 * @param out the resulting remote
 * @param repo the repository in which to create the remote
 * @param name the remote's name
 * @param url the remote's url
 * @param fetch the remote fetch value
 * @return 0, GIT_EINVALIDSPEC, GIT_EEXISTS or an error code
 */
GIT_EXTERN(int) git_remote_create_with_fetchspec(
		git_remote **out,
		git_repository *repo,
		const char *name,
		const char *url,
		const char *fetch);

/**
 * Create an anonymous remote
 *
 * Create a remote with the given url and refspec in memory. You can use
 * this when you have a URL instead of a remote's name.  Note that anonymous
 * remotes cannot be converted to persisted remotes.
 *
 * The name, when provided, will be checked for validity.
 * See `git_tag_create()` for rules about valid names.
 *
 * @param out pointer to the new remote object
 * @param repo the associated repository
 * @param url the remote repository's URL
 * @param fetch the fetch refspec to use for this remote.
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_create_anonymous(
		git_remote **out,
		git_repository *repo,
		const char *url,
		const char *fetch);

/**
 * Get the information for a particular remote
 *
 * The name will be checked for validity.
 * See `git_tag_create()` for rules about valid names.
 *
 * @param out pointer to the new remote object
 * @param repo the associated repository
 * @param name the remote's name
 * @return 0, GIT_ENOTFOUND, GIT_EINVALIDSPEC or an error code
 */
GIT_EXTERN(int) git_remote_load(git_remote **out, git_repository *repo, const char *name);

/**
 * Save a remote to its repository's configuration
 *
 * One can't save a in-memory remote. Doing so will
 * result in a GIT_EINVALIDSPEC being returned.
 *
 * @param remote the remote to save to config
 * @return 0, GIT_EINVALIDSPEC or an error code
 */
GIT_EXTERN(int) git_remote_save(const git_remote *remote);

/**
 * Create a copy of an existing remote.  All internal strings are also
 * duplicated. Callbacks are not duplicated.
 *
 * Call `git_remote_free` to free the data.
 *
 * @param dest pointer where to store the copy
 * @param source object to copy
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_dup(git_remote **dest, git_remote *source);

/**
 * Get the remote's repository
 *
 * @param remote the remote
 * @return a pointer to the repository
 */
GIT_EXTERN(git_repository *) git_remote_owner(const git_remote *remote);

/**
 * Get the remote's name
 *
 * @param remote the remote
 * @return a pointer to the name or NULL for in-memory remotes
 */
GIT_EXTERN(const char *) git_remote_name(const git_remote *remote);

/**
 * Get the remote's url
 *
 * @param remote the remote
 * @return a pointer to the url
 */
GIT_EXTERN(const char *) git_remote_url(const git_remote *remote);

/**
 * Get the remote's url for pushing
 *
 * @param remote the remote
 * @return a pointer to the url or NULL if no special url for pushing is set
 */
GIT_EXTERN(const char *) git_remote_pushurl(const git_remote *remote);

/**
 * Set the remote's url
 *
 * Existing connections will not be updated.
 *
 * @param remote the remote
 * @param url the url to set
 * @return 0 or an error value
 */
GIT_EXTERN(int) git_remote_set_url(git_remote *remote, const char* url);

/**
 * Set the remote's url for pushing
 *
 * Existing connections will not be updated.
 *
 * @param remote the remote
 * @param url the url to set or NULL to clear the pushurl
 * @return 0 or an error value
 */
GIT_EXTERN(int) git_remote_set_pushurl(git_remote *remote, const char* url);

/**
 * Add a fetch refspec to the remote
 *
 * Convenience function for adding a single fetch refspec to the
 * current list in the remote.
 *
 * @param remote the remote
 * @param refspec the new fetch refspec
 * @return 0 or an error value
 */
GIT_EXTERN(int) git_remote_add_fetch(git_remote *remote, const char *refspec);

/**
 * Get the remote's list of fetch refspecs
 *
 * The memory is owned by the user and should be freed with
 * `git_strarray_free`.
 *
 * @param array pointer to the array in which to store the strings
 * @param remote the remote to query
 */
GIT_EXTERN(int) git_remote_get_fetch_refspecs(git_strarray *array, const git_remote *remote);

/**
 * Set the remote's list of fetch refspecs
 *
 * The contents of the string array are copied.
 *
 * @param remote the remote to modify
 * @param array the new list of fetch resfpecs
 */
GIT_EXTERN(int) git_remote_set_fetch_refspecs(git_remote *remote, git_strarray *array);

/**
 * Add a push refspec to the remote
 *
 * Convenience function for adding a single push refspec to the
 * current list in the remote.
 *
 * @param remote the remote
 * @param refspec the new push refspec
 * @return 0 or an error value
 */
GIT_EXTERN(int) git_remote_add_push(git_remote *remote, const char *refspec);

/**
 * Get the remote's list of push refspecs
 *
 * The memory is owned by the user and should be freed with
 * `git_strarray_free`.
 *
 * @param array pointer to the array in which to store the strings
 * @param remote the remote to query
 */
GIT_EXTERN(int) git_remote_get_push_refspecs(git_strarray *array, const git_remote *remote);

/**
 * Set the remote's list of push refspecs
 *
 * The contents of the string array are copied.
 *
 * @param remote the remote to modify
 * @param array the new list of push resfpecs
 */
GIT_EXTERN(int) git_remote_set_push_refspecs(git_remote *remote, git_strarray *array);

/**
 * Clear the refspecs
 *
 * Remove all configured fetch and push refspecs from the remote.
 *
 * @param remote the remote
 */
GIT_EXTERN(void) git_remote_clear_refspecs(git_remote *remote);

/**
 * Get the number of refspecs for a remote
 *
 * @param remote the remote
 * @return the amount of refspecs configured in this remote
 */
GIT_EXTERN(size_t) git_remote_refspec_count(const git_remote *remote);

/**
 * Get a refspec from the remote
 *
 * @param remote the remote to query
 * @param n the refspec to get
 * @return the nth refspec
 */
GIT_EXTERN(const git_refspec *)git_remote_get_refspec(const git_remote *remote, size_t n);

/**
 * Open a connection to a remote
 *
 * The transport is selected based on the URL. The direction argument
 * is due to a limitation of the git protocol (over TCP or SSH) which
 * starts up a specific binary which can only do the one or the other.
 *
 * @param remote the remote to connect to
 * @param direction GIT_DIRECTION_FETCH if you want to fetch or
 * GIT_DIRECTION_PUSH if you want to push
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_connect(git_remote *remote, git_direction direction);

/**
 * Get a list of refs at the remote
 *
 * The remote (or more exactly its transport) must be connected. The
 * memory belongs to the remote.
 *
 * The array will stay valid as long as the remote object exists and
 * its transport isn't changed, but a copy is recommended for usage of
 * the data.
 *
 * @param out pointer to the array
 * @param size the number of remote heads
 * @param remote the remote
 * @return 0 on success, or an error code
 */
GIT_EXTERN(int) git_remote_ls(const git_remote_head ***out,  size_t *size, git_remote *remote);

/**
 * Download and index the packfile
 *
 * Connect to the remote if it hasn't been done yet, negotiate with
 * the remote git which objects are missing, download and index the
 * packfile.
 *
 * The .idx file will be created and both it and the packfile with be
 * renamed to their final name.
 *
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_download(git_remote *remote);

/**
 * Check whether the remote is connected
 *
 * Check whether the remote's underlying transport is connected to the
 * remote host.
 *
 * @param remote the remote
 * @return 1 if it's connected, 0 otherwise.
 */
GIT_EXTERN(int) git_remote_connected(const git_remote *remote);

/**
 * Cancel the operation
 *
 * At certain points in its operation, the network code checks whether
 * the operation has been cancelled and if so stops the operation.
 *
 * @param remote the remote
 */
GIT_EXTERN(void) git_remote_stop(git_remote *remote);

/**
 * Disconnect from the remote
 *
 * Close the connection to the remote and free the underlying
 * transport.
 *
 * @param remote the remote to disconnect from
 */
GIT_EXTERN(void) git_remote_disconnect(git_remote *remote);

/**
 * Free the memory associated with a remote
 *
 * This also disconnects from the remote, if the connection
 * has not been closed yet (using git_remote_disconnect).
 *
 * @param remote the remote to free
 */
GIT_EXTERN(void) git_remote_free(git_remote *remote);

/**
 * Update the tips to the new state
 *
 * @param remote the remote to update
 * @param signature The identity to use when updating reflogs
 * @param reflog_message The message to insert into the reflogs. If NULL, the
 *                       default is "fetch <name>", where <name> is the name of
 *                       the remote (or its url, for in-memory remotes).
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_update_tips(
		git_remote *remote,
		const git_signature *signature,
		const char *reflog_message);

/**
 * Download new data and update tips
 *
 * Convenience function to connect to a remote, download the data,
 * disconnect and update the remote-tracking branches.
 *
 * @param remote the remote to fetch from
 * @param signature The identity to use when updating reflogs
 * @param reflog_message The message to insert into the reflogs. If NULL, the
 *								 default is "fetch"
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_fetch(
		git_remote *remote,
		const git_signature *signature,
		const char *reflog_message);

/**
 * Return whether a string is a valid remote URL
 *
 * @param url the url to check
 * @return 1 if the url is valid, 0 otherwise
 */
GIT_EXTERN(int) git_remote_valid_url(const char *url);

/**
 * Return whether the passed URL is supported by this version of the library.
 *
 * @param url the url to check
 * @return 1 if the url is supported, 0 otherwise
*/
GIT_EXTERN(int) git_remote_supported_url(const char* url);

/**
 * Get a list of the configured remotes for a repo
 *
 * The string array must be freed by the user.
 *
 * @param out a string array which receives the names of the remotes
 * @param repo the repository to query
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_list(git_strarray *out, git_repository *repo);

/**
 * Choose whether to check the server's certificate (applies to HTTPS only)
 *
 * @param remote the remote to configure
 * @param check whether to check the server's certificate (defaults to yes)
 */
GIT_EXTERN(void) git_remote_check_cert(git_remote *remote, int check);

/**
 * Sets a custom transport for the remote. The caller can use this function
 * to bypass the automatic discovery of a transport by URL scheme (i.e.
 * http://, https://, git://) and supply their own transport to be used
 * instead. After providing the transport to a remote using this function,
 * the transport object belongs exclusively to that remote, and the remote will
 * free it when it is freed with git_remote_free.
 *
 * @param remote the remote to configure
 * @param transport the transport object for the remote to use
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_set_transport(
	git_remote *remote,
	git_transport *transport);

/**
 * Argument to the completion callback which tells it which operation
 * finished.
 */
typedef enum git_remote_completion_type {
	GIT_REMOTE_COMPLETION_DOWNLOAD,
	GIT_REMOTE_COMPLETION_INDEXING,
	GIT_REMOTE_COMPLETION_ERROR,
} git_remote_completion_type;

/**
 * The callback settings structure
 *
 * Set the callbacks to be called by the remote when informing the user
 * about the progress of the network operations.
 */
struct git_remote_callbacks {
	unsigned int version;
	/**
	 * Textual progress from the remote. Text send over the
	 * progress side-band will be passed to this function (this is
	 * the 'counting objects' output.
	 */
	git_transport_message_cb sideband_progress;

	/**
	 * Completion is called when different parts of the download
	 * process are done (currently unused).
	 */
	int (*completion)(git_remote_completion_type type, void *data);

	/**
	 * This will be called if the remote host requires
	 * authentication in order to connect to it.
	 *
	 * Returning GIT_PASSTHROUGH will make libgit2 behave as
	 * though this field isn't set.
	 */
	git_cred_acquire_cb credentials;

	/**
	 * During the download of new data, this will be regularly
	 * called with the current count of progress done by the
	 * indexer.
	 */
	git_transfer_progress_cb transfer_progress;

	/**
	 * Each time a reference is updated locally, this function
	 * will be called with information about it.
	 */
	int (*update_tips)(const char *refname, const git_oid *a, const git_oid *b, void *data);

	/**
	 * This will be passed to each of the callbacks in this struct
	 * as the last parameter.
	 */
	void *payload;
};

#define GIT_REMOTE_CALLBACKS_VERSION 1
#define GIT_REMOTE_CALLBACKS_INIT {GIT_REMOTE_CALLBACKS_VERSION}

/**
 * Initializes a `git_remote_callbacks` with default values. Equivalent to
 * creating an instance with GIT_REMOTE_CALLBACKS_INIT.
 *
 * @param opts the `git_remote_callbacks` struct to initialize
 * @param version Version of struct; pass `GIT_REMOTE_CALLBACKS_VERSION`
 * @return Zero on success; -1 on failure.
 */
GIT_EXTERN(int) git_remote_init_callbacks(
	git_remote_callbacks *opts,
	unsigned int version);

/**
 * Set the callbacks for a remote
 *
 * Note that the remote keeps its own copy of the data and you need to
 * call this function again if you want to change the callbacks.
 *
 * @param remote the remote to configure
 * @param callbacks a pointer to the user's callback settings
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_set_callbacks(git_remote *remote, const git_remote_callbacks *callbacks);

/**
 * Retrieve the current callback structure
 *
 * This provides read access to the callbacks structure as the remote
 * sees it.
 *
 * @param remote the remote to query
 * @return a pointer to the callbacks structure
 */
GIT_EXTERN(const git_remote_callbacks *) git_remote_get_callbacks(git_remote *remote);

/**
 * Get the statistics structure that is filled in by the fetch operation.
 */
GIT_EXTERN(const git_transfer_progress *) git_remote_stats(git_remote *remote);

typedef enum {
	GIT_REMOTE_DOWNLOAD_TAGS_AUTO = 0,
	GIT_REMOTE_DOWNLOAD_TAGS_NONE = 1,
	GIT_REMOTE_DOWNLOAD_TAGS_ALL = 2
} git_remote_autotag_option_t;

/**
 * Retrieve the tag auto-follow setting
 *
 * @param remote the remote to query
 * @return the auto-follow setting
 */
GIT_EXTERN(git_remote_autotag_option_t) git_remote_autotag(const git_remote *remote);

/**
 * Set the tag auto-follow setting
 *
 * @param remote the remote to configure
 * @param value a GIT_REMOTE_DOWNLOAD_TAGS value
 */
GIT_EXTERN(void) git_remote_set_autotag(
	git_remote *remote,
	git_remote_autotag_option_t value);

/**
 * Give the remote a new name
 *
 * All remote-tracking branches and configuration settings
 * for the remote are updated.
 *
 * The new name will be checked for validity.
 * See `git_tag_create()` for rules about valid names.
 *
 * A temporary in-memory remote cannot be given a name with this method.
 *
 * @param problems non-default refspecs cannot be renamed and will be
 * stored here for further processing by the caller. Always free this
 * strarray on succesful return.
 * @param remote the remote to rename
 * @param new_name the new name the remote should bear
 * @return 0, GIT_EINVALIDSPEC, GIT_EEXISTS or an error code
 */
GIT_EXTERN(int) git_remote_rename(
	git_strarray *problems,
	git_remote *remote,
	const char *new_name);

/**
 * Retrieve the update FETCH_HEAD setting.
 *
 * @param remote the remote to query
 * @return the update FETCH_HEAD setting
 */
GIT_EXTERN(int) git_remote_update_fetchhead(git_remote *remote);

/**
 * Sets the update FETCH_HEAD setting.  By default, FETCH_HEAD will be
 * updated on every fetch.  Set to 0 to disable.
 *
 * @param remote the remote to configure
 * @param value 0 to disable updating FETCH_HEAD
 */
GIT_EXTERN(void) git_remote_set_update_fetchhead(git_remote *remote, int value);

/**
 * Ensure the remote name is well-formed.
 *
 * @param remote_name name to be checked.
 * @return 1 if the reference name is acceptable; 0 if it isn't
 */
GIT_EXTERN(int) git_remote_is_valid_name(const char *remote_name);

/**
* Delete an existing persisted remote.
*
* All remote-tracking branches and configuration settings
* for the remote will be removed.
*
* @param remote A valid remote
* @return 0 on success, or an error code.
*/
GIT_EXTERN(int) git_remote_delete(git_remote *remote);

/**
 * Retrieve the name of the remote's default branch
 *
 * The default branch of a repository is the branch which HEAD points
 * to. If the remote does not support reporting this information
 * directly, it performs the guess as git does; that is, if there are
 * multiple branches which point to the same commit, the first one is
 * chosen. If the master branch is a candidate, it wins.
 *
 * This function must only be called after connecting.
 *
 * @param out the buffern in which to store the reference name
 * @param remote the remote
 * @return 0, GIT_ENOTFOUND if the remote does not have any references
 * or none of them point to HEAD's commit, or an error message.
 */
GIT_EXTERN(int) git_remote_default_branch(git_buf *out, git_remote *remote);

/** @} */
GIT_END_DECL
#endif
