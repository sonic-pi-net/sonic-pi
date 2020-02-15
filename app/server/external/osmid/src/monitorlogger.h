#pragma once

#include <iostream>
#include <memory>
#include "oscout.h"
#include "spdlog/spdlog.h"
#include "osc_sink.h"

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
    void setSendToOSC(bool send) { m_sendToOSC = send; }
    void setOscOutput(std::shared_ptr<OscOutput> oscOutput)
    {
        m_osc = spdlog::create<spdlog::sinks::osc_sink_mt>("osc", oscOutput);
    }

    template <typename... Args>
    inline void trace(const char* fmt, const Args&... args)
    {
        m_console->trace(fmt, args...);
        if (m_sendToOSC) {
            m_osc->trace(fmt, args...);
        }
    }

    template <typename... Args>
    inline void debug(const char* fmt, const Args&... args)
    {
        m_console->debug(fmt, args...);
        if (m_sendToOSC) {
            m_osc->debug(fmt, args...);
        }
    }

    template <typename... Args>
    inline void info(const char* fmt, const Args&... args)
    {
        m_console->info(fmt, args...);
        if (m_sendToOSC) {
            m_osc->info(fmt, args...);
        }
    }

    template <typename... Args>
    inline void warn(const char* fmt, const Args&... args)
    {
        m_console->warn(fmt, args...);
        if (m_sendToOSC) {
            m_osc->warn(fmt, args...);
        }
    }

    template <typename... Args>
    inline void error(const char* fmt, const Args&... args)
    {
        m_console->error(fmt, args...);
        if (m_sendToOSC) {
            m_osc->error(fmt, args...);
        }
    }

    template <typename... Args>
    inline void critical(const char* fmt, const Args&... args)
    {
        m_console->critical(fmt, args...);
        if (m_sendToOSC) {
            m_osc->critical(fmt, args...);
        }
    }

private:
    MonitorLogger()
    {
        m_sendToOSC = true;
        m_console = spdlog::stdout_logger_mt("console");
    }

    std::shared_ptr<spdlog::logger> m_console;
    std::shared_ptr<spdlog::logger> m_osc;
    bool m_sendToOSC;
};
