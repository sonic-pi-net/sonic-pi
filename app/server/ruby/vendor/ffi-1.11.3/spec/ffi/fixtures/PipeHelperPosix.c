/*
 * Copyright (c) 2015 Lars Kanis. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#ifndef _WIN32
#include <unistd.h>
#include <sys/time.h>
#include "PipeHelper.h"

int pipeHelperCreatePipe(FD_TYPE pipefd[2])
{
    return pipe(pipefd);
}

char pipeHelperReadChar(FD_TYPE fd, int timeout)
{
    char d;
    struct timeval time = {timeout, 0}; // timeout after x seconds
    fd_set read_fds;
    FD_ZERO(&read_fds);
    FD_SET(fd, &read_fds);

    if(select(fd + 1, &read_fds, NULL, NULL, &time) <= 0)
        return 0;

    if( read(fd, &d, 1) != 1)
        return 0;
    return d;
}

int pipeHelperWriteChar(FD_TYPE fd, char c)
{
    return write(fd, &c, 1);
}

void pipeHelperClosePipe(FD_TYPE fd) {
    close(fd);
}
#endif
