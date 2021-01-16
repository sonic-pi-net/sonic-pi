#pragma once

#include <iostream>
#include <memory>
#include "spdlog/spdlog.h"

class MonitorLogger {
public:
    MonitorLogger(MonitorLogger const&) = delete;
    void operator=(MonitorLogger const&) = delete;

    inline static MonitorLogger& getInstance()
    {
        static MonitorLogger instance;
        return instance;
    }

    void setLogLevel(int level) { spdlog::set_level(static_cast<spdlog::level::level_enum>(level)); }

    template <typename... Args>
    inline void trace(const char* fmt, const Args&... args)
    {
        m_console->trace(fmt, args...);
    }

    template <typename... Args>
    inline void debug(const char* fmt, const Args&... args)
    {
        m_console->debug(fmt, args...);
    }

    template <typename... Args>
    inline void info(const char* fmt, const Args&... args)
    {
        m_console->info(fmt, args...);
    }

    template <typename... Args>
    inline void warn(const char* fmt, const Args&... args)
    {
        m_console->warn(fmt, args...);
    }

    template <typename... Args>
    inline void error(const char* fmt, const Args&... args)
    {
        m_console->error(fmt, args...);
    }

    template <typename... Args>
    inline void critical(const char* fmt, const Args&... args)
    {
        m_console->critical(fmt, args...);
    }

private:
    MonitorLogger()
    {
        m_console = spdlog::stdout_logger_mt("console");
    }

    std::shared_ptr<spdlog::logger> m_console;
};
