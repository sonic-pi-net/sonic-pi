//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "api/osc/tcp_osc_sender.h"

#include <atomic>
#include <cstring>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

#include <kissnet.hpp>

#include "api/logger.h"

using namespace oscpkt;

namespace SonicPi
{

struct TcpOscSender::Impl
{
    explicit Impl(int port)
        : port(port)
    {
    }

    ~Impl()
    {
        running = false;
        {
            std::lock_guard<std::mutex> lk(mutex);
            closeLocked();
        }
        if (drain.joinable())
            drain.join();
    }

    // Caller holds mutex.
    bool ensureConnectedLocked()
    {
        if (socket)
            return true;
        try
        {
            auto s = std::make_unique<kissnet::tcp_socket>(
                kissnet::endpoint("127.0.0.1", static_cast<kissnet::port_t>(port)));
            if (s->connect() != kissnet::socket_status::valid)
            {
                LOG(ERR, "[TCP OSC Sender] - could not connect to engine port " << port);
                return false;
            }
            s->set_tcp_no_delay(true);
            socket = std::move(s);
            // Fresh drain thread per connection generation: it exits when
            // the socket it was born to serve dies.
            if (drain.joinable())
                drain.join();
            drain = std::thread([this, sock = socket.get()] { drainLoop(sock); });
            LOG(INFO, "[TCP OSC Sender] - connected to engine port " << port);
            return true;
        }
        catch (const std::exception& e)
        {
            LOG(ERR, "[TCP OSC Sender] - connect error: " << e.what());
            return false;
        }
    }

    // Caller holds mutex.
    void closeLocked()
    {
        if (socket)
        {
            try
            {
                socket->close();
            }
            catch (...)
            {
            }
            socket.reset();
        }
    }

    // Reads and discards inbound bytes so the engine's writes to this
    // connection can never accumulate unread and backpressure it. Runs
    // until the connection dies; sendOSC reconnects lazily.
    void drainLoop(kissnet::tcp_socket* sock)
    {
        kissnet::buffer<4096> buff;
        while (running)
        {
            // recv blocks; returns 0/invalid on close or error.
            const auto [len, status] = sock->recv(buff);
            if (!running || status != kissnet::socket_status::valid || len == 0)
                break;
        }
    }

    int port;
    std::mutex mutex;
    std::unique_ptr<kissnet::tcp_socket> socket;
    std::atomic<bool> running{true};
    std::thread drain;
};

TcpOscSender::TcpOscSender(int port)
    : m_impl(std::make_unique<Impl>(port))
{
}

TcpOscSender::~TcpOscSender() = default;

bool TcpOscSender::sendOSC(Message m)
{
    LOG(DBG, "TcpSendOSC " << m.addressPattern());

    PacketWriter pw;
    pw.addMessage(m);
    const size_t size = pw.packetSize();
    // Engine stream framing severs the connection on frames > its MAX_FRAME
    // (256KB) — refuse here with a log line instead.
    if (size == 0 || size > (256u * 1024u))
    {
        LOG(ERR, "[TCP OSC Sender] - refusing to send frame of " << size << " bytes: " << m.addressPattern());
        return false;
    }

    // One buffer, one write: length prefix (4-byte big-endian) + payload,
    // so a frame can never interleave with another sender's.
    std::vector<std::byte> frame(4 + size);
    frame[0] = static_cast<std::byte>((size >> 24) & 0xFF);
    frame[1] = static_cast<std::byte>((size >> 16) & 0xFF);
    frame[2] = static_cast<std::byte>((size >> 8) & 0xFF);
    frame[3] = static_cast<std::byte>(size & 0xFF);
    std::memcpy(frame.data() + 4, pw.packetData(), size);

    std::lock_guard<std::mutex> lk(m_impl->mutex);
    for (int attempt = 0; attempt < 2; ++attempt)
    {
        if (!m_impl->ensureConnectedLocked())
            return false;
        const auto [sent, status] = m_impl->socket->send(frame.data(), frame.size());
        if (status == kissnet::socket_status::valid && sent == frame.size())
            return true;
        // Connection died (engine restart?) — drop it and retry once.
        LOG(ERR, "[TCP OSC Sender] - send failed, reconnecting");
        m_impl->closeLocked();
    }
    return false;
}

} // namespace SonicPi
