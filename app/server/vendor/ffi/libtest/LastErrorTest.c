/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#if defined(_WIN32) || defined(__WIN32__)
# include <windows.h>
#else
# include <errno.h>
#endif

int setLastError(int error) {
#if defined(_WIN32) || defined(__WIN32__)
    SetLastError(error);
#else
    errno = error;
#endif
    return -1;
}

