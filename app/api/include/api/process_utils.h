#pragma once

#ifndef WIN32
#include <sys/stat.h>
#endif

namespace SonicPi
{

inline void raise_process_priority(int pid)
{

#ifdef WIN32
    auto hProcess = OpenProcess(PROCESS_SET_INFORMATION, TRUE, pid);
    if (hProcess)
    {
        if (!SetPriorityClass(hProcess, ABOVE_NORMAL_PRIORITY_CLASS))
        {
            char err[256];
            FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(),
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), err, 255, NULL);

            LOG(ERR, "Failed to raise ruby process priority: " << err);
        }
        CloseHandle(hProcess);
    }
#endif
}

inline bool process_running(int pid)
{
#ifdef WIN32
    LOG(INFO, "WaitForSingleObject on " << pid);
    auto hProcess = OpenProcess(SYNCHRONIZE, TRUE, pid);
    if (hProcess)
    {
        auto res = WaitForSingleObject(hProcess, 0);
        CloseHandle(hProcess);
        if (res == WAIT_TIMEOUT)
        {
            return true;
        }
        return false;
    }
    else
    {
        return false;
    }
#else
    struct stat sts;
    auto strProc = std::string("/proc/") + std::to_string(pid);
    if (stat(strProc.c_str(), &sts) == -1 && errno == ENOENT)
    {
        return false;
    }
    return true;
#endif
}


} // namespace SonicPi
