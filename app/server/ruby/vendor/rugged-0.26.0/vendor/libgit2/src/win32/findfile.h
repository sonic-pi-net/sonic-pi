/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_git_findfile_h__
#define INCLUDE_git_findfile_h__

#include "common.h"

extern int git_win32__find_system_dirs(git_buf *out, const wchar_t *subpath);
extern int git_win32__find_global_dirs(git_buf *out);
extern int git_win32__find_xdg_dirs(git_buf *out);
extern int git_win32__find_programdata_dirs(git_buf *out);

#endif

