/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <string.h>

#include <openssl/rand.h>
#include <openssl/des.h>
#include <openssl/md4.h>
#include <openssl/hmac.h>
#include <openssl/err.h>

#include "ntlm.h"
#include "compat.h"
#include "util.h"
#include "crypt.h"

bool ntlm_random_bytes(
	ntlm_client *ntlm,
	unsigned char *out,
	size_t len)
{
	int rc = RAND_bytes(out, len);

	if (rc != 1) {
		ntlm_client_set_errmsg(ntlm, ERR_lib_error_string(ERR_get_error()));
		return false;
	}

	return true;
}

bool ntlm_des_encrypt(
	ntlm_des_block *out,
	ntlm_des_block *plaintext,
	ntlm_des_block *key)
{
	DES_key_schedule keysched;

	memset(out, 0, sizeof(ntlm_des_block));

	DES_set_key(key, &keysched);
	DES_ecb_encrypt(plaintext, out, &keysched, DES_ENCRYPT);

	return true;
}

bool ntlm_md4_digest(
	unsigned char out[CRYPT_MD4_DIGESTSIZE],
	const unsigned char *in,
	size_t in_len)
{
	MD4(in, in_len, out);
	return true;
}

#if OPENSSL_VERSION_NUMBER < 0x10100000L
static inline void HMAC_CTX_free(HMAC_CTX *ctx)
{
	if (ctx)
		HMAC_CTX_cleanup(ctx);

	free(ctx);
}

static inline int HMAC_CTX_reset(HMAC_CTX *ctx)
{
	HMAC_CTX_cleanup(ctx);
	memzero(ctx, sizeof(HMAC_CTX));
	return 1;
}

static inline HMAC_CTX *HMAC_CTX_new(void)
{
	return calloc(1, sizeof(HMAC_CTX));
}
#endif

ntlm_hmac_ctx *ntlm_hmac_ctx_init(void)
{
	return HMAC_CTX_new();
}

bool ntlm_hmac_ctx_reset(ntlm_hmac_ctx *ctx)
{
	return HMAC_CTX_reset(ctx);
}

bool ntlm_hmac_md5_init(
	ntlm_hmac_ctx *ctx,
	const unsigned char *key,
	size_t key_len)
{
	return HMAC_Init_ex(ctx, key, key_len, EVP_md5(), NULL);
}

bool ntlm_hmac_md5_update(
	ntlm_hmac_ctx *ctx,
	const unsigned char *in,
	size_t in_len)
{
	return HMAC_Update(ctx, in, in_len);
}

bool ntlm_hmac_md5_final(
	unsigned char *out,
	size_t *out_len,
	ntlm_hmac_ctx *ctx)
{
	unsigned int len;

	if (*out_len < CRYPT_MD5_DIGESTSIZE)
		return false;

	if (!HMAC_Final(ctx, out, &len))
		return false;

	*out_len = len;
	return true;
}

void ntlm_hmac_ctx_free(ntlm_hmac_ctx *ctx)
{
	HMAC_CTX_free(ctx);
}
