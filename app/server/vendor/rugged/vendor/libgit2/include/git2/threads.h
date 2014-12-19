/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_threads_h__
#define INCLUDE_git_threads_h__

#include "common.h"

/**
 * @file git2/threads.h
 * @brief Library level thread functions
 * @defgroup git_thread Threading functions
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

/**
 * Initialize the OpenSSL locks
 *
 * OpenSSL requires the application to determine how it performs
 * locking. This is a convenience function which libgit2 provides for
 * allocating and initializing the locks as well as setting the
 * locking function to use the system's native locking functions.
 *
 * The locking function will be cleared and the memory will be freed
 * when you call git_threads_sutdown().
 *
 * @return 0 on success, -1 if there are errors or if libgit2 was not
 * built with OpenSSL and threading support.
 */
GIT_EXTERN(int) git_openssl_set_locking(void);

/** @} */
GIT_END_DECL
#endif

