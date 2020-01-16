/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <locale.h>
#include <iconv.h>
#include <string.h>
#include <errno.h>

#include "ntlmclient.h"
#include "unicode.h"
#include "ntlm.h"
#include "compat.h"

struct ntlm_unicode_ctx {
	ntlm_client *ntlm;
	iconv_t utf8_to_16;
	iconv_t utf16_to_8;
};

ntlm_unicode_ctx *ntlm_unicode_ctx_init(ntlm_client *ntlm)
{
	ntlm_unicode_ctx *ctx;

	if ((ctx = calloc(1, sizeof(ntlm_unicode_ctx))) == NULL)
		return NULL;

	ctx->ntlm = ntlm;
	ctx->utf8_to_16 = (iconv_t)-1;
	ctx->utf16_to_8 = (iconv_t)-1;

	return ctx;
}

typedef enum {
	unicode_iconv_utf8_to_16,
	unicode_iconv_utf16_to_8
} unicode_iconv_encoding_direction;

static inline bool unicode_iconv_init(ntlm_unicode_ctx *ctx)
{
	if (ctx->utf8_to_16 != (iconv_t)-1 || ctx->utf16_to_8 != (iconv_t)-1)
		return true;

	if ((ctx->utf8_to_16 = iconv_open("UTF-16LE", "UTF-8")) == (iconv_t)-1 ||
		(ctx->utf16_to_8 = iconv_open("UTF-8", "UTF-16LE")) == (iconv_t)-1) {
		if (errno == EINVAL)
			ntlm_client_set_errmsg(ctx->ntlm,
				"iconv does not support UTF8 <-> UTF16 conversion");
		else
			ntlm_client_set_errmsg(ctx->ntlm, strerror(errno));

		return false;
	}

	return true;
}

static inline bool unicode_iconv_encoding_convert(
	char **converted,
	size_t *converted_len,
	ntlm_unicode_ctx *ctx,
	const char *string,
	size_t string_len,
	unicode_iconv_encoding_direction direction)
{
	char *in_start, *out_start, *out, *new_out;
	size_t in_start_len, out_start_len, out_size, nul_size, ret, written = 0;
	iconv_t converter;

	*converted = NULL;
	*converted_len = 0;

	if (!unicode_iconv_init(ctx))
		return false;

	/*
	 * When translating UTF8 to UTF16, these strings are only used
	 * internally, and we obey the given length, so we can simply
	 * use a buffer that is 2x the size.  When translating from UTF16
	 * to UTF8, we may need to return to callers, so we need to NUL
	 * terminate and expect an extra byte for UTF8, two for UTF16.
	 */
	if (direction == unicode_iconv_utf8_to_16) {
		converter = ctx->utf8_to_16;
		out_size = (string_len * 2) + 2;
		nul_size = 2;
	} else {
		converter = ctx->utf16_to_8;
		out_size = (string_len / 2) + 1;
		nul_size = 1;
	}

	/* Round to the nearest multiple of 8 */
	out_size = (out_size + 7) & ~7;

	if ((out = malloc(out_size)) == NULL) {
		ntlm_client_set_errmsg(ctx->ntlm, "out of memory");
		return false;
	}

	in_start = (char *)string;
	in_start_len = string_len;

	while (true) {
		out_start = out + written;
		out_start_len = (out_size - nul_size) - written;

		ret = iconv(converter, &in_start, &in_start_len, &out_start, &out_start_len);
		written = (out_size - nul_size) - out_start_len;

		if (ret == 0)
			break;

		if (ret == (size_t)-1 && errno != E2BIG) {
			ntlm_client_set_errmsg(ctx->ntlm, strerror(errno));
			goto on_error;
		}

		/* Grow buffer size by 1.5 (rounded up to a multiple of 8) */
		out_size = ((((out_size << 1) - (out_size >> 1)) + 7) & ~7);

		if (out_size > NTLM_UNICODE_MAX_LEN) {
			ntlm_client_set_errmsg(ctx->ntlm,
				"unicode conversion too large");
			goto on_error;
		}

		if ((new_out = realloc(out, out_size)) == NULL) {
			ntlm_client_set_errmsg(ctx->ntlm, "out of memory");
			goto on_error;
		}

		out = new_out;
	}

	if (in_start_len != 0) {
		ntlm_client_set_errmsg(ctx->ntlm,
			"invalid unicode string; trailing data remains");
		goto on_error;
	}

	/* NUL terminate */
	out[written] = '\0';

	if (direction == unicode_iconv_utf8_to_16)
		out[written + 1] = '\0';

	*converted = out;

	if (converted_len)
		*converted_len = written;

	return true;

on_error:
	free(out);
	return false;
}

bool ntlm_unicode_utf8_to_16(
	char **converted,
	size_t *converted_len,
	ntlm_unicode_ctx *ctx,
	const char *string,
	size_t string_len)
{
	return unicode_iconv_encoding_convert(
		converted, converted_len, ctx, string, string_len,
		unicode_iconv_utf8_to_16);
}

bool ntlm_unicode_utf16_to_8(
	char **converted,
	size_t *converted_len,
	ntlm_unicode_ctx *ctx,
	const char *string,
	size_t string_len)
{
	return unicode_iconv_encoding_convert(
		converted, converted_len, ctx, string, string_len,
		unicode_iconv_utf16_to_8);
}

void ntlm_unicode_ctx_free(ntlm_unicode_ctx *ctx)
{
	if (!ctx)
		return;

	if (ctx->utf16_to_8 != (iconv_t)-1)
		iconv_close(ctx->utf16_to_8);

	if (ctx->utf8_to_16 != (iconv_t)-1)
		iconv_close(ctx->utf8_to_16);

	free(ctx);
}
