/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include <CommonCrypto/CommonCrypto.h>

#include "ntlm.h"
#include "crypt.h"

bool ntlm_random_bytes(
	ntlm_client *ntlm,
	unsigned char *out,
	size_t len)
{
	int fd, ret;
	size_t total = 0;

	if ((fd = open("/dev/urandom", O_RDONLY)) < 0) {
		ntlm_client_set_errmsg(ntlm, strerror(errno));
		return false;
	}

	while (total < len) {
		if ((ret = read(fd, out, (len - total))) < 0) {
			ntlm_client_set_errmsg(ntlm, strerror(errno));
			return false;
		} else if (ret == 0) {
			ntlm_client_set_errmsg(ntlm, "unexpected eof on random device");
			return false;
		}

		total += ret;
	}

	close(fd);
	return true;
}

bool ntlm_des_encrypt(
	ntlm_des_block *out,
	ntlm_des_block *plaintext,
	ntlm_des_block *key)
{
	size_t written;

	CCCryptorStatus result = CCCrypt(kCCEncrypt,
		kCCAlgorithmDES, kCCOptionECBMode,
		key, sizeof(ntlm_des_block), NULL,
		plaintext, sizeof(ntlm_des_block),
		out, sizeof(ntlm_des_block), &written);

	return (result == kCCSuccess) ? true : false;
}

bool ntlm_md4_digest(
	unsigned char out[CRYPT_MD4_DIGESTSIZE],
	const unsigned char *in,
	size_t in_len)
{
	return !!CC_MD4(in, in_len, out);
}

ntlm_hmac_ctx *ntlm_hmac_ctx_init(void)
{
	return calloc(1, sizeof(ntlm_hmac_ctx));
}

bool ntlm_hmac_ctx_reset(ntlm_hmac_ctx *ctx)
{
	memset(ctx, 0, sizeof(ntlm_hmac_ctx));
	return true;
}

bool ntlm_hmac_md5_init(
	ntlm_hmac_ctx *ctx,
	const unsigned char *key,
	size_t key_len)
{
	CCHmacInit(&ctx->native, kCCHmacAlgMD5, key, key_len);
	return true;
}

bool ntlm_hmac_md5_update(
	ntlm_hmac_ctx *ctx,
	const unsigned char *data,
	size_t data_len)
{
	CCHmacUpdate(&ctx->native, data, data_len);
	return true;
}

bool ntlm_hmac_md5_final(
	unsigned char *out,
	size_t *out_len,
	ntlm_hmac_ctx *ctx)
{
	if (*out_len < CRYPT_MD5_DIGESTSIZE)
		return false;

	CCHmacFinal(&ctx->native, out);

	*out_len = CRYPT_MD5_DIGESTSIZE;
	return true;
}

void ntlm_hmac_ctx_free(ntlm_hmac_ctx *ctx)
{
	free(ctx);
}
