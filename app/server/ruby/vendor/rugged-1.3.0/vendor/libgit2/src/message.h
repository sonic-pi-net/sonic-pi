/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_message_h__
#define INCLUDE_message_h__

#include "common.h"

#include "git2/message.h"
#include "buffer.h"

int git_message__prettify(git_buf *message_out, const char *message, int strip_comments);

#endif
