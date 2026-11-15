//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <chrono>
#include <cstdio>
#include <ctime>
#include <mutex>
#include <streambuf>
#include <string>

namespace SonicPi
{

// The sink behind std::cout once it points at gui.log. Prefixes every line
// with "[HH:MM:SS.mmm] " so gui.log shares the format of the daemon, Spider
// and SuperSonic logs (stamped by the daemon's ProcessBooter).
//
// Several threads write to cout - the window's thread in pieces
// ("[GUI] - Change Volume to " << val << "%"), the API's threads a line at a
// time - so each thread's text is gathered until its line is complete, and
// whole lines go to the file under one lock. Nothing of one thread's line
// lands inside another's.
class TimestampLineBuf : public std::streambuf
{
public:
    explicit TimestampLineBuf(std::streambuf* dest)
        : m_dest(dest)
    {
    }

protected:
    int overflow(int ch) override
    {
        if (ch == traits_type::eof())
            return sync();
        const char c = static_cast<char>(ch);
        xsputn(&c, 1);
        return ch;
    }

    std::streamsize xsputn(const char* s, std::streamsize n) override
    {
        std::string& line = pending();
        line.append(s, static_cast<size_t>(n));
        if (line.find('\n') == std::string::npos)
            return n;

        std::lock_guard<std::mutex> lock(m_mutex);
        size_t start = 0;
        for (size_t nl = line.find('\n'); nl != std::string::npos; nl = line.find('\n', start))
        {
            emit(line.data() + start, nl + 1 - start);
            start = nl + 1;
        }
        line.erase(0, start);
        return n;
    }

    // A flush: whatever this thread has of a line goes out as it stands, so a
    // crash straight after a flush still leaves the text in the file.
    int sync() override
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        std::string& line = pending();
        if (!line.empty())
        {
            emit(line.data(), line.size());
            line.clear();
        }
        return m_dest->pubsync();
    }

private:
    // One partial line per thread. There is one of these buffers per process
    // (behind cout), so a single thread-local serves it.
    static std::string& pending()
    {
        static thread_local std::string line;
        return line;
    }

    // Under m_mutex. Writes text that is at most one line, stamping it when
    // the file is at the start of a line.
    void emit(const char* s, size_t n)
    {
        if (m_atLineStart)
            writeStamp();
        m_dest->sputn(s, static_cast<std::streamsize>(n));
        m_atLineStart = (s[n - 1] == '\n');
    }

    void writeStamp()
    {
        using namespace std::chrono;
        const auto now = system_clock::now();
        const auto ms = duration_cast<milliseconds>(now.time_since_epoch()) % 1000;
        const std::time_t t = system_clock::to_time_t(now);
        std::tm tmBuf;
#ifdef WIN32
        localtime_s(&tmBuf, &t);
#else
        localtime_r(&t, &tmBuf);
#endif
        char buf[24];
        const int len = snprintf(buf, sizeof(buf), "[%02d:%02d:%02d.%03d] ",
                                 tmBuf.tm_hour, tmBuf.tm_min, tmBuf.tm_sec,
                                 static_cast<int>(ms.count()));
        if (len > 0)
            m_dest->sputn(buf, len);
    }

    std::streambuf* m_dest;
    std::mutex m_mutex;
    bool m_atLineStart = true;
};

} // namespace SonicPi
