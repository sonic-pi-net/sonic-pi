
#pragma once

#include <spdlog/sinks/base_sink.h>
#include <spdlog/details/null_mutex.h>

#include <mutex>
#include <string>
#include <memory>

#include "osc/OscOutboundPacketStream.h"
#include "oscout.h"

namespace spdlog {
namespace sinks {
    template <class Mutex>
    class osc_sink : public base_sink<Mutex> {
    public:
        explicit osc_sink(std::shared_ptr<OscOutput> oscOutput)
            : m_oscOutput(oscOutput)
        {
        }

        void flush() override
        {
        }

    protected:
        void _sink_it(const details::log_msg& msg) override
        {
            char buffer[2048];
            osc::OutboundPacketStream p(buffer, 2048);

            p << osc::BeginMessage("/logging");
            p << msg.formatted.c_str();
            p << osc::EndMessage;
            m_oscOutput->sendUDP(p.Data(), p.Size());
        }

        std::shared_ptr<OscOutput> m_oscOutput;
    };

    typedef osc_sink<std::mutex> osc_sink_mt;
    typedef osc_sink<details::null_mutex> osc_sink_st;
}
}
