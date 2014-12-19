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

/**
 * Type of SSH host fingerprint
 */
typedef enum {
	/** MD5 is available */
	GIT_CERT_SSH_MD5 = (1 << 0),
	/** SHA-1 is available */
	GIT_CERT_SSH_SHA1 = (1 << 1),
} git_cert_ssh_t;

/**
 * Hostkey information taken from libssh2
 */
typedef struct {
	/**
	 * Type of certificate. Here to share the header with
	 * `git_cert`.
	 */
	git_cert_t cert_type;
        /**
         * A hostkey type from libssh2, either
         * `GIT_CERT_SSH_MD5` or `GIT_CERT_SSH_SHA1`
         */
	git_cert_ssh_t type;

        /**
         * Hostkey hash. If type has `GIT_CERT_SSH_MD5` set, this will
         * have the MD5 hash of the hostkey.
         */
	unsigned char hash_md5[16];

        /**
         * Hostkey hash. If type has `GIT_CERT_SSH_SHA1` set, this will
         * have the SHA-1 hash of the hostkey.
         */
        unsigned char hash_sha1[20];
} git_cert_hostkey;

/**
 * X.509 certificate information
 */
typedef struct {
	/**
	 * Type of certificate. Here to share the header with
	 * `git_cert`.
	 */
	git_cert_t cert_type;
	/**
	 * Pointer to the X.509 certificate data
	 */
	void *data;
	/**
	 * Length of the memory block pointed to by `data`.
	 */
	size_t len;
} git_cert_x509;

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

	/**
	 * Username-only information
	 *
	 * If the SSH transport does not know which username to use,
	 * it will ask via this credential type.
	 */
	GIT_CREDTYPE_USERNAME = (1u << 5),
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

/** Username-only credential information */
typedef struct git_cred_username {
	git_cred parent;
	char username[1];
} git_cred_username;

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
 * Create a credential to specify a username.
 *
 * This is used with ssh authentication to query for the username if
 * none is specified in the url.
 */
GIT_EXTERN(int) git_cred_username_new(git_cred **cred, const char *username);

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

/** @} */
GIT_END_DECL
#endif
