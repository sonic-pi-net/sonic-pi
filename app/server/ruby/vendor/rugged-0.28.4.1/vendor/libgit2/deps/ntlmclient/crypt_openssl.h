/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_CRYPT_OPENSSL_H__
#define PRIVATE_CRYPT_OPENSSL_H__

#include <openssl/hmac.h>

/* OpenSSL 1.1.0 uses opaque structs, we'll reuse these. */
#if OPENSSL_VERSION_NUMBER < 0x10100000L
typedef struct hmac_ctx_st ntlm_hmac_ctx;
#else
# define ntlm_hmac_ctx HMAC_CTX
#endif

#endif /* PRIVATE_CRYPT_OPENSSL_H__ */
