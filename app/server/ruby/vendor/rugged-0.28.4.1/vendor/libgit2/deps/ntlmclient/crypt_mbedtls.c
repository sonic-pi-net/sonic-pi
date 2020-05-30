/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <string.h>

#include "mbedtls/ctr_drbg.h"
#include "mbedtls/des.h"
#include "mbedtls/entropy.h"
#include "mbedtls/md4.h"

#include "ntlm.h"
#include "crypt.h"

bool ntlm_random_bytes(
	ntlm_client *ntlm,
	unsigned char *out,
	size_t len)
{
	mbedtls_ctr_drbg_context ctr_drbg;
	mbedtls_entropy_context entropy;
	bool ret = true;

	const unsigned char personalization[] = {
		0xec, 0xb5, 0xd1, 0x0b, 0x8f, 0x15, 0x1f, 0xc2,
		0xe4, 0x8e, 0xec, 0x36, 0xf7, 0x0a, 0x45, 0x9a,
		0x1f, 0xe1, 0x35, 0x58, 0xb1, 0xcb, 0xfd, 0x8a,
		0x57, 0x5c, 0x75, 0x7d, 0x2f, 0xc9, 0x70, 0xac
	};

	mbedtls_ctr_drbg_init(&ctr_drbg);
	mbedtls_entropy_init(&entropy);

	if (mbedtls_ctr_drbg_seed(&ctr_drbg, mbedtls_entropy_func,
		&entropy, personalization, sizeof(personalization)) ||
		mbedtls_ctr_drbg_random(&ctr_drbg, out, len)) {
		ntlm_client_set_errmsg(ntlm, "random generation failed");
		ret = false;
	}

	mbedtls_entropy_free(&entropy);
	mbedtls_ctr_drbg_free(&ctr_drbg);

	return ret;
}

bool ntlm_des_encrypt(
	ntlm_des_block *out,
	ntlm_des_block *plaintext,
	ntlm_des_block *key)
{
	mbedtls_des_context ctx;
	bool success = false;

	mbedtls_des_init(&ctx);

	if (mbedtls_des_setkey_enc(&ctx, *key) ||
		mbedtls_des_crypt_ecb(&ctx, *plaintext, *out))
		goto done;

	success = true;

done:
	mbedtls_des_free(&ctx);
	return success;
}

bool ntlm_md4_digest(
	unsigned char out[CRYPT_MD4_DIGESTSIZE],
	const unsigned char *in,
	size_t in_len)
{
	mbedtls_md4_context ctx;

	mbedtls_md4_init(&ctx);
	mbedtls_md4_starts(&ctx);
	mbedtls_md4_update(&ctx, in, in_len);
	mbedtls_md4_finish(&ctx, out);
	mbedtls_md4_free(&ctx);

	return true;
}

ntlm_hmac_ctx *ntlm_hmac_ctx_init(void)
{
	ntlm_hmac_ctx *ctx;
	const mbedtls_md_info_t *info = mbedtls_md_info_from_type(MBEDTLS_MD_MD5);

	if ((ctx = calloc(1, sizeof(ntlm_hmac_ctx))) == NULL)
		return NULL;

	mbedtls_md_init(&ctx->mbed);

	if (mbedtls_md_setup(&ctx->mbed, info, 1) != 0) {
		free(ctx);
		return false;
	}

	return ctx;
}

bool ntlm_hmac_ctx_reset(ntlm_hmac_ctx *ctx)
{
	return !mbedtls_md_hmac_reset(&ctx->mbed);
}

bool ntlm_hmac_md5_init(
	ntlm_hmac_ctx *ctx,
	const unsigned char *key,
	size_t key_len)
{
	return !mbedtls_md_hmac_starts(&ctx->mbed, key, key_len);
}

bool ntlm_hmac_md5_update(
	ntlm_hmac_ctx *ctx,
	const unsigned char *in,
	size_t in_len)
{
	return !mbedtls_md_hmac_update(&ctx->mbed, in, in_len);
}

bool ntlm_hmac_md5_final(
	unsigned char *out,
	size_t *out_len,
	ntlm_hmac_ctx *ctx)
{
	if (*out_len < CRYPT_MD5_DIGESTSIZE)
		return false;

	return !mbedtls_md_hmac_finish(&ctx->mbed, out);
}

void ntlm_hmac_ctx_free(ntlm_hmac_ctx *ctx)
{
	if (ctx) {
		mbedtls_md_free(&ctx->mbed);
		free(ctx);
	}
}
