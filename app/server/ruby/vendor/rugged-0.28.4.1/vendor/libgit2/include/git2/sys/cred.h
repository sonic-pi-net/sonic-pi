/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_sys_git_cred_h__
#define INCLUDE_sys_git_cred_h__

#include "git2/common.h"
#include "git2/cred.h"

/**
 * @file git2/sys/cred.h
 * @brief Git credentials low-level implementation
 * @defgroup git_cred Git credentials low-level implementation
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

/**
 * The base structure for all credential types
 */
struct git_cred {
	git_credtype_t credtype; /**< A type of credential */

	/** The deallocator for this type of credentials */
	void GIT_CALLBACK(free)(git_cred *cred);
};

/** A plaintext username and password */
struct git_cred_userpass_plaintext {
	git_cred parent; /**< The parent cred */
	char *username; /**< The username to authenticate as */
	char *password; /**< The password to use */
};

/** Username-only credential information */
struct git_cred_username {
	git_cred parent; /**< The parent cred */
	char username[1]; /**< The username to authenticate as */
};

/**
 * A ssh key from disk
 */
struct git_cred_ssh_key {
	git_cred parent; /**< The parent cred */
	char *username; /**< The username to authenticate as */
	char *publickey; /**< The path to a public key */
	char *privatekey; /**< The path to a private key */
	char *passphrase; /**< Passphrase used to decrypt the private key */
};

/**
 * Keyboard-interactive based ssh authentication
 */
struct git_cred_ssh_interactive {
	git_cred parent; /**< The parent cred */
	char *username; /**< The username to authenticate as */

	/**
	 * Callback used for authentication.
	 */
	git_cred_ssh_interactive_cb prompt_callback;

	void *payload; /**< Payload passed to prompt_callback */
};

/**
 * A key with a custom signature function
 */
struct git_cred_ssh_custom {
	git_cred parent; /**< The parent cred */
	char *username; /**< The username to authenticate as */
	char *publickey; /**< The public key data */
	size_t publickey_len; /**< Length of the public key */

	/**
	 * Callback used to sign the data.
	 */
	git_cred_sign_cb sign_callback;

	void *payload; /**< Payload passed to prompt_callback */
};

GIT_END_DECL

#endif
