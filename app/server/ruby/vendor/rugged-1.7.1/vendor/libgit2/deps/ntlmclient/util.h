/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_UTIL_H__
#define PRIVATE_UTIL_H__

extern void ntlm_memzero(void *data, size_t size);
extern uint64_t ntlm_htonll(uint64_t value);

#endif /* PRIVATE_UTIL_H__ */
