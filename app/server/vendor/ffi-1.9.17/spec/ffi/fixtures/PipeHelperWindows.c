/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#ifdef _WIN32
#include <windows.h>
#include "PipeHelper.h"

int pipeHelperCreatePipe(FD_TYPE pipefd[2])
{
    char name[ MAX_PATH ];
    static int pipe_idx = 0;
    sprintf( name, "\\\\.\\Pipe\\pipeHelper-%u-%i",
             (unsigned int)GetCurrentProcessId(), pipe_idx++ );

    pipefd[0] = CreateNamedPipe( name, PIPE_ACCESS_INBOUND | FILE_FLAG_OVERLAPPED,
                         PIPE_TYPE_BYTE | PIPE_WAIT,
                         1,             // Number of pipes
                         5,         // Out buffer size
                         5,         // In buffer size
                         60 * 1000,    // Timeout in ms
                         NULL );
    if(pipefd[0] == INVALID_HANDLE_VALUE)
        return -1;

    pipefd[1] = CreateFile( name, GENERIC_WRITE, 0, NULL,
                        OPEN_EXISTING,
                        FILE_ATTRIBUTE_NORMAL,
                        NULL);

    if(pipefd[1] == INVALID_HANDLE_VALUE) {
        CloseHandle( pipefd[0] );
        return -1;
    }
    return 0;
}

char pipeHelperReadChar(FD_TYPE fd, int timeout)
{
    char d;
    OVERLAPPED ovl;
    ZeroMemory(&ovl, sizeof(ovl));
    ovl.hEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    if( ReadFile(fd, &d, 1, NULL, &ovl) == 0) {
        DWORD recvd = 0;;
        DWORD res = WaitForSingleObject(ovl.hEvent, timeout * 1000);
        if( res != WAIT_OBJECT_0 ) {
            CloseHandle(ovl.hEvent);
            return 0;
        }
        if( GetOverlappedResult(fd, &ovl, &recvd, FALSE) == 0 ) {
            CloseHandle(ovl.hEvent);
            return 0;
        }
    }
    CloseHandle(ovl.hEvent);
    return d;
}

int pipeHelperWriteChar(FD_TYPE fd, char c)
{
    DWORD written;
    return WriteFile(fd, &c, 1, &written, NULL) == 0 ? 0 : 1;
}

void pipeHelperClosePipe(FD_TYPE fd) {
    CloseHandle(fd);
}

#endif
