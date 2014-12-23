/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_transport_h__
#define INCLUDE_git_transport_h__

#include "indexer.h"
#include "net.h"
#include "types.h"

/**
 * @file git2/transport.h
 * @brief Git transport interfaces and functions
 * @defgroup git_transport interfaces and functions
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

/*
 *** Begin interface for credentials acquisition ***
 */

/** Authentication type requested */
typedef enum {
	/* git_cred_userpass_plaintext */
	GIT_CREDTYPE_USERPASS_PLAINTEXT = (1u << 0),

	/* git_cred_ssh_key */
	GIT_CREDTYPE_SSH_KEY = (1u << 1),

	/* git_cred_ssh_custom */
	GIT_CREDTYPE_SSH_CUSTOM = (1u << 2),

	/* git_cred_default */
	GIT_CREDTYPE_DEFAULT = (1u << 3),

	/* git_cred_ssh_interactive */
	GIT_CREDTYPE_SSH_INTERACTIVE = (1u << 4),
} git_credtype_t;

/* The base structure for all credential types */
typedef struct git_cred git_cred;

struct git_cred {
	git_credtype_t credtype;
	void (*free)(git_cred *cred);
};

/** A plaintext username and password */
typedef struct {
	git_cred parent;
	char *username;
	char *password;
} git_cred_userpass_plaintext;


/*
 * If the user hasn't included libssh2.h before git2.h, we need to
 * define a few types for the callback signatures.
 */
#ifndef LIBSSH2_VERSION
typedef struct _LIBSSH2_SESSION LIBSSH2_SESSION;
typedef struct _LIBSSH2_USERAUTH_KBDINT_PROMPT LIBSSH2_USERAUTH_KBDINT_PROMPT;
typedef struct _LIBSSH2_USERAUTH_KBDINT_RESPONSE LIBSSH2_USERAUTH_KBDINT_RESPONSE;
#endif

typedef int (*git_cred_sign_callback)(LIBSSH2_SESSION *session, unsigned char **sig, size_t *sig_len, const unsigned char *data, size_t data_len, void **abstract);
typedef void (*git_cred_ssh_interactive_callback)(const char* name, int name_len, const char* instruction, int instruction_len, int num_prompts, const LIBSSH2_USERAUTH_KBDINT_PROMPT* prompts, LIBSSH2_USERAUTH_KBDINT_RESPONSE* responses, void **abstract);

/**
 * A ssh key from disk
 */
typedef struct git_cred_ssh_key {
	git_cred parent;
	char *username;
	char *publickey;
	char *privatekey;
	char *passphrase;
} git_cred_ssh_key;

/**
 * Keyboard-interactive based ssh authentication
 */
typedef struct git_cred_ssh_interactive {
	git_cred parent;
	char *username;
	git_cred_ssh_interactive_callback prompt_callback;
	void *payload;
} git_cred_ssh_interactive;

/**
 * A key with a custom signature function
 */
typedef struct git_cred_ssh_custom {
	git_cred parent;
	char *username;
	char *publickey;
	size_t publickey_len;
	git_cred_sign_callback sign_callback;
	void *payload;
} git_cred_ssh_custom;

/** A key for NTLM/Kerberos "default" credentials */
typedef struct git_cred git_cred_default;

/**
 * Check whether a credential object contains username information.
 *
 * @param cred object to check
 * @return 1 if the credential object has non-NULL username, 0 otherwise
 */
GIT_EXTERN(int) git_cred_has_username(git_cred *cred);

/**
 * Create a new plain-text username and password credential object.
 * The supplied credential parameter will be internally duplicated.
 *
 * @param out The newly created credential object.
 * @param username The username of the credential.
 * @param password The password of the credential.
 * @return 0 for success or an error code for failure
 */
GIT_EXTERN(int) git_cred_userpass_plaintext_new(
	git_cred **out,
	const char *username,
	const char *password);

/**
 * Create a new passphrase-protected ssh key credential object.
 * The supplied credential parameter will be internally duplicated.
 *
 * @param out The newly created credential object.
 * @param username username to use to authenticate
 * @param publickey The path to the public key of the credential.
 * @param privatekey The path to the private key of the credential.
 * @param passphrase The passphrase of the credential.
 * @return 0 for success or an error code for failure
 */
GIT_EXTERN(int) git_cred_ssh_key_new(
	git_cred **out,
	const char *username,
	const char *publickey,
	const char *privatekey,
	const char *passphrase);

/**
 * Create a new ssh keyboard-interactive based credential object.
 * The supplied credential parameter will be internally duplicated.
 *
 * @param username Username to use to authenticate.
 * @param prompt_callback The callback method used for prompts.
 * @param payload Additional data to pass to the callback.
 * @return 0 for success or an error code for failure.
 */
GIT_EXTERN(int) git_cred_ssh_interactive_new(
	git_cred **out,
	const char *username,
	git_cred_ssh_interactive_callback prompt_callback,
	void *payload);

/**
 * Create a new ssh key credential object used for querying an ssh-agent.
 * The supplied credential parameter will be internally duplicated.
 *
 * @param out The newly created credential object.
 * @param username username to use to authenticate
 * @return 0 for success or an error code for failure
 */
GIT_EXTERN(int) git_cred_ssh_key_from_agent(
	git_cred **out,
	const char *username);

/**
 * Create an ssh key credential with a custom signing function.
 *
 * This lets you use your own function to sign the challenge.
 *
 * This function and its credential type is provided for completeness
 * and wraps `libssh2_userauth_publickey()`, which is undocumented.
 *
 * The supplied credential parameter will be internally duplicated.
 *
 * @param out The newly created credential object.
 * @param username username to use to authenticate
 * @param publickey The bytes of the public key.
 * @param publickey_len The length of the public key in bytes.
 * @param sign_callback The callback method to sign the data during the challenge.
 * @param payload Additional data to pass to the callback.
 * @return 0 for success or an error code for failure
 */
GIT_EXTERN(int) git_cred_ssh_custom_new(
	git_cred **out,
	const char *username,
	const char *publickey,
	size_t publickey_len,
	git_cred_sign_callback sign_callback,
	void *payload);

/**
 * Create a "default" credential usable for Negotiate mechanisms like NTLM
 * or Kerberos authentication.
 *
 * @return 0 for success or an error code for failure
 */
GIT_EXTERN(int) git_cred_default_new(git_cred **out);

/**
 * Signature of a function which acquires a credential object.
 *
 * - cred: The newly created credential object.
 * - url: The resource for which we are demanding a credential.
 * - username_from_url: The username that was embedded in a "user@host"
 *                          remote url, or NULL if not included.
 * - allowed_types: A bitmask stating which cred types are OK to return.
 * - payload: The payload provided when specifying this callback.
 * - returns 0 for success, < 0 to indicate an error, > 0 to indicate
 *       no credential was acquired
 */
typedef int (*git_cred_acquire_cb)(
	git_cred **cred,
	const char *url,
	const char *username_from_url,
	unsigned int allowed_types,
	void *payload);

/*
 *** End interface for credentials acquisition ***
 *** Begin base transport interface ***
 */

typedef enum {
	GIT_TRANSPORTFLAGS_NONE = 0,
	/* If the connection is secured with SSL/TLS, the authenticity
	 * of the server certificate should not be verified. */
	GIT_TRANSPORTFLAGS_NO_CHECK_CERT = 1
} git_transport_flags_t;

typedef int (*git_transport_message_cb)(const char *str, int len, void *data);

typedef struct git_transport git_transport;

struct git_transport {
	unsigned int version;
	/* Set progress and error callbacks */
	int (*set_callbacks)(
		git_transport *transport,
		git_transport_message_cb progress_cb,
		git_transport_message_cb error_cb,
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
	 * is guranteed until the next call of a transport function. */
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
 * @param priority The priority of this transport relative to others with
 *		the same prefix. Built-in transports have a priority of 1.
 * @param cb The callback used to create an instance of the transport
 * @param param A fixed parameter to pass to cb at creation time
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_register(
	const char *prefix,
	unsigned priority,
	git_transport_cb cb,
	void *param);

/**
 *
 * Unregister a custom transport definition which was previously registered
 * with git_transport_register.
 *
 * @param prefix From the previous call to git_transport_register
 * @param priority From the previous call to git_transport_register
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_transport_unregister(
	const char *prefix,
	unsigned priority);

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

typedef struct git_smart_subtransport_definition {
	/* The function to use to create the git_smart_subtransport */
	git_smart_subtransport_cb callback;

	/* True if the protocol is stateless; false otherwise. For example,
	 * http:// is stateless, but git:// is not. */
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

/*
 *** End interface for subtransports for the smart transport ***
 */

/** @} */
GIT_END_DECL
#endif
