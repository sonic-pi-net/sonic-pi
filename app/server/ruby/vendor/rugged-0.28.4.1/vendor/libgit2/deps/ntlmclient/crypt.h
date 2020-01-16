/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_CRYPT_COMMON_H__
#define PRIVATE_CRYPT_COMMON_H__

#if defined(CRYPT_OPENSSL)
# include "crypt_openssl.h"
#elif defined(CRYPT_MBEDTLS)
# include "crypt_mbedtls.h"
#elif defined(CRYPT_COMMONCRYPTO)
# include "crypt_commoncrypto.h"
#else
# error "no crypto support"
#endif

#define CRYPT_DES_BLOCKSIZE 8
#define CRYPT_MD4_DIGESTSIZE 16
#define CRYPT_MD5_DIGESTSIZE 16

typedef unsigned char ntlm_des_block[CRYPT_DES_BLOCKSIZE];

extern bool ntlm_random_bytes(
	ntlm_client *ntlm,
	unsigned char *out,
	size_t len);

extern bool ntlm_des_encrypt(
	ntlm_des_block *out,
	ntlm_des_block *plaintext,
	ntlm_des_block *key);

extern bool ntlm_md4_digest(
	unsigned char out[CRYPT_MD4_DIGESTSIZE],
	const unsigned char *in,
	size_t in_len);

extern ntlm_hmac_ctx *ntlm_hmac_ctx_init(void);

extern bool ntlm_hmac_ctx_reset(ntlm_hmac_ctx *ctx);

extern bool ntlm_hmac_md5_init(
	ntlm_hmac_ctx *ctx,
	const unsigned char *key,
	size_t key_len);

extern bool ntlm_hmac_md5_update(
	ntlm_hmac_ctx *ctx,
	const unsigned char *data,
	size_t data_len);

extern bool ntlm_hmac_md5_final(
	unsigned char *out,
	size_t *out_len,
	ntlm_hmac_ctx *ctx);

extern void ntlm_hmac_ctx_free(ntlm_hmac_ctx *ctx);

#endif /* PRIVATE_CRYPT_COMMON_H__ */
