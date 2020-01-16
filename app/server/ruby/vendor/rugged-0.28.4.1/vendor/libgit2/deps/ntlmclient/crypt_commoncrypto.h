/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_CRYPT_COMMONCRYPTO_H__
#define PRIVATE_CRYPT_COMMONCRYPTO_H__

#include <CommonCrypto/CommonCrypto.h>

typedef struct {
	CCHmacContext native;
} ntlm_hmac_ctx;

#endif /* PRIVATE_CRYPT_COMMONCRYPTO_H__ */
