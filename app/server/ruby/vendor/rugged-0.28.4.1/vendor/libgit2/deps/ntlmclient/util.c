/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <stdint.h>

#include "compat.h"
#include "util.h"

void memzero(void *data, size_t size)
{
	volatile uint8_t *scan = (volatile uint8_t *)data;

	while (size--)
		*scan++ = 0x0;
}
