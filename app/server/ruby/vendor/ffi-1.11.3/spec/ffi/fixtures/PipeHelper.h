/*
 * Copyright (c) 2015 Lars Kanis. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#ifndef PIPEHELPER_H
#define PIPEHELPER_H

#ifdef _WIN32
#define FD_TYPE HANDLE
#else
#define FD_TYPE int
#endif

int pipeHelperCreatePipe(FD_TYPE pipefd[2]);
char pipeHelperReadChar(FD_TYPE fd, int timeout);
int pipeHelperWriteChar(FD_TYPE fd, char c);
void pipeHelperClosePipe(FD_TYPE fd);

#endif
