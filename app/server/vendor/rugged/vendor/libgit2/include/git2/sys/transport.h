/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_sys_git_transport_h
#define INCLUDE_sys_git_transport_h

#include "git2/net.h"
#include "git2/types.h"

/**
 * @file git2/sys/transport.h
 * @brief Git custom transport registration interfaces and functions
 * @defgroup git_transport Git custom transport registration
 * @ingroup Git
 * @{
 */

GIT_BEGIN_DECL

/**
 * Flags to pass to transport
 *
 * Currently unused.
 */
typedef enum {
	GIT_TRANSPORTFLAGS_NONE = 0,
} git_transport_flags_t;

typedef struct git_transport git_transport;

struct git_transport {
	unsigned int version;
	/* Set progress and error callbacks */
	int (*set_callbacks)(
		git_transport *transport,
		git_transport_message_cb progress_cb,
		git_transport_message_cb error_cb,
		git_transport_certificate_check_cb certificate_check_cb,
		void *payload);

	/* Connect the transport to the remote repository, using the given
	 * direction. */
	int (*connect)(
		git_transport *transport,
		const char *url,
		git_cred_acquire_cb cred_acquire_cb,
		void *cred_acquire_payload,
		int direction,
		int flags);

	/* This function may be called after a successful call to
	 * connect(). The array returned is owned by the transport and
	 * is guaranteed until the next call of a transport function. */
	int (*ls)(
		const git_remote_head ***out,
		size_t *size,
		git_transport *transport);

	/* Executes the push whose context is in the git_push object. */
	int (*push)(git_transport *transport, git_push *push);

	/* This function may be called after a successful call to connect(), when
	 * the direction is FETCH. The function performs a negotiation to calculate
	 * the wants list for the fetch. */
	int (*negotiate_fetch)(
		git_transport *transport,
		git_repository *repo,
		const git_remote_head * const *refs,
		size_t count);

	/* This function may be called after a successful call to negotiate_fetch(),
	 * when the direction is FETCH. This function retrieves the pack file for
	 * the fetch from the remote end. */
	int (*download_pack)(
		git_transport *transport,
		git_repository *repo,
		git_transfer_progress *stats,
		git_transfer_progress_cb progress_cb,
		void *progress_payload);

	/* Checks to see if the transport is connected */
	int (*is_connected)(git_transport *transport);

	/* Reads the flags value previously passed into connect() */
	int (*read_flags)(git_transport *transport, int *flags);

	/* Cancels any outstanding transport operation */
	void (*cancel)(git_transport *transport);

	/* This function is the reverse of connect() -- it terminates the
	 * connection to the remote end. */
	int (*close)(git_transport *transport);

	/* Frees/destructs the git_transport object. */
	void (*free)(git_transport *transport);
};

#define GIT_TRANSPORT_VERSION 1
#define GIT_TRANSPORT_INIT {GIT_TRANSPORT_VERSION}

/**
 * Initializes a `git_transport` with default values. Equivalent to
 * creating an instance with GIT_TRANSPORT_INIT.
 *
 * @param opts the `git_transport` struct to initialize
 * @param version Version of struct; pass `GIT_TRANSPORT_VERSION`
 * @return Zero on success; -1 on failure.
 */
GIT_EXTERN(int) git_transport_init(
	git_transport *opts,
	unsigned int version);

/**
 * Function to use to create a transport from a URL. The transport database
 * is scanned to find a transport that implements the scheme of the URI (i.e.
 * git:// or http://) and a transport object is returned to the caller.
 *
 * @param out The newly created transport (out)
 * @param owner The git_remote which will own this transport
 * @param url The URL to connect to
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_new(git_transport **out, git_remote *owner, const char *url);

/**
 * Create an ssh transport with custom git command paths
 *
 * This is a factory function suitable for setting as the transport
 * callback in a remote (or for a clone in the options).
 *
 * The payload argument must be a strarray pointer with the paths for
 * the `git-upload-pack` and `git-receive-pack` at index 0 and 1.
 *
 * @param out the resulting transport
 * @param owner the owning remote
 * @param payload a strarray with the paths
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_ssh_with_paths(git_transport **out, git_remote *owner, void *payload);

/* Signature of a function which creates a transport */
typedef int (*git_transport_cb)(git_transport **out, git_remote *owner, void *param);

/**
 * Add a custom transport definition, to be used in addition to the built-in
 * set of transports that come with libgit2.
 *
 * The caller is responsible for synchronizing calls to git_transport_register
 * and git_transport_unregister with other calls to the library that
 * instantiate transports.
 *
 * @param prefix The scheme (ending in "://") to match, i.e. "git://"
 * @param cb The callback used to create an instance of the transport
 * @param param A fixed parameter to pass to cb at creation time
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_register(
	const char *prefix,
	git_transport_cb cb,
	void *param);

/**
 *
 * Unregister a custom transport definition which was previously registered
 * with git_transport_register.
 *
 * @param prefix From the previous call to git_transport_register
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_unregister(
	const char *prefix);

/* Transports which come with libgit2 (match git_transport_cb). The expected
 * value for "param" is listed in-line below. */

/**
 * Create an instance of the dummy transport.
 *
 * @param out The newly created transport (out)
 * @param owner The git_remote which will own this transport
 * @param payload You must pass NULL for this parameter.
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_dummy(
	git_transport **out,
	git_remote *owner,
	/* NULL */ void *payload);

/**
 * Create an instance of the local transport.
 *
 * @param out The newly created transport (out)
 * @param owner The git_remote which will own this transport
 * @param payload You must pass NULL for this parameter.
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_local(
	git_transport **out,
	git_remote *owner,
	/* NULL */ void *payload);

/**
 * Create an instance of the smart transport.
 *
 * @param out The newly created transport (out)
 * @param owner The git_remote which will own this transport
 * @param payload A pointer to a git_smart_subtransport_definition
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_smart(
	git_transport **out,
	git_remote *owner,
	/* (git_smart_subtransport_definition *) */ void *payload);

/*
 *** End of base transport interface ***
 *** Begin interface for subtransports for the smart transport ***
 */

/* The smart transport knows how to speak the git protocol, but it has no
 * knowledge of how to establish a connection between it and another endpoint,
 * or how to move data back and forth. For this, a subtransport interface is
 * declared, and the smart transport delegates this work to the subtransports.
 * Three subtransports are implemented: git, http, and winhttp. (The http and
 * winhttp transports each implement both http and https.) */

/* Subtransports can either be RPC = 0 (persistent connection) or RPC = 1
 * (request/response). The smart transport handles the differences in its own
 * logic. The git subtransport is RPC = 0, while http and winhttp are both
 * RPC = 1. */

/* Actions that the smart transport can ask
 * a subtransport to perform */
typedef enum {
	GIT_SERVICE_UPLOADPACK_LS = 1,
	GIT_SERVICE_UPLOADPACK = 2,
	GIT_SERVICE_RECEIVEPACK_LS = 3,
	GIT_SERVICE_RECEIVEPACK = 4,
} git_smart_service_t;

typedef struct git_smart_subtransport git_smart_subtransport;
typedef struct git_smart_subtransport_stream git_smart_subtransport_stream;

/* A stream used by the smart transport to read and write data
 * from a subtransport */
struct git_smart_subtransport_stream {
	/* The owning subtransport */
	git_smart_subtransport *subtransport;

	int (*read)(
		git_smart_subtransport_stream *stream,
		char *buffer,
		size_t buf_size,
		size_t *bytes_read);

	int (*write)(
		git_smart_subtransport_stream *stream,
		const char *buffer,
		size_t len);

	void (*free)(
		git_smart_subtransport_stream *stream);
};

/* An implementation of a subtransport which carries data for the
 * smart transport */
struct git_smart_subtransport {
	int (* action)(
			git_smart_subtransport_stream **out,
			git_smart_subtransport *transport,
			const char *url,
			git_smart_service_t action);

	/* Subtransports are guaranteed a call to close() between
	 * calls to action(), except for the following two "natural" progressions
	 * of actions against a constant URL.
	 *
	 * 1. UPLOADPACK_LS -> UPLOADPACK
	 * 2. RECEIVEPACK_LS -> RECEIVEPACK */
	int (*close)(git_smart_subtransport *transport);

	void (*free)(git_smart_subtransport *transport);
};

/* A function which creates a new subtransport for the smart transport */
typedef int (*git_smart_subtransport_cb)(
	git_smart_subtransport **out,
	git_transport* owner);

/**
 * Definition for a "subtransport"
 *
 * This is used to let the smart protocol code know about the protocol
 * which you are implementing.
 */
typedef struct git_smart_subtransport_definition {
	/** The function to use to create the git_smart_subtransport */
	git_smart_subtransport_cb callback;

	/**
	 * True if the protocol is stateless; false otherwise. For example,
	 * http:// is stateless, but git:// is not.
	 */
	unsigned rpc;
} git_smart_subtransport_definition;

/* Smart transport subtransports that come with libgit2 */

/**
 * Create an instance of the http subtransport. This subtransport
 * also supports https. On Win32, this subtransport may be implemented
 * using the WinHTTP library.
 *
 * @param out The newly created subtransport
 * @param owner The smart transport to own this subtransport
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_smart_subtransport_http(
	git_smart_subtransport **out,
	git_transport* owner);

/**
 * Create an instance of the git subtransport.
 *
 * @param out The newly created subtransport
 * @param owner The smart transport to own this subtransport
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_smart_subtransport_git(
	git_smart_subtransport **out,
	git_transport* owner);

/**
 * Create an instance of the ssh subtransport.
 *
 * @param out The newly created subtransport
 * @param owner The smart transport to own this subtransport
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_smart_subtransport_ssh(
	git_smart_subtransport **out,
	git_transport* owner);

/**
 * Sets a custom transport factory for the remote. The caller can use this
 * function to override the transport used for this remote when performing
 * network operations.
 *
 * @param remote the remote to configure
 * @param transport_cb the function to use to create a transport
 * @param payload opaque parameter passed to transport_cb
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_remote_set_transport(
	git_remote *remote,
	git_transport_cb transport_cb,
	void *payload);

/** @} */
GIT_END_DECL
#endif
