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

#ifndef TCPOSCSENDER_H
#define TCPOSCSENDER_H

#include <memory>

#include "osc_pkt.hh"

namespace SonicPi
{

// Fire-and-forget OSC sender over the engine's TCP command transport
// (4-byte big-endian length-prefixed frames, TCP_NODELAY). Replaces the
// per-message UDP OscSender for engine-bound traffic: TCP gives delivery
// (no silent datagram loss — see the 2026-08-05 scheduler-LATE
// investigation) and immediate death detection.
//
// Connection is lazy: the engine binds its stream transport only after
// device init, so the first sendOSC after boot establishes it and later
// failures reconnect on the next send. A background drain thread reads
// and discards anything the engine writes back on this connection — this
// sender awaits no replies (they flow GUI-ward via the daemon relay), but
// an unread socket would eventually backpressure the engine's send path.
class TcpOscSender
{
public:
    explicit TcpOscSender(int port);
    ~TcpOscSender();

    TcpOscSender(const TcpOscSender&) = delete;
    TcpOscSender& operator=(const TcpOscSender&) = delete;

    // Thread-safe; frames are written atomically under a lock. Returns
    // false if the engine is unreachable (caller-visible, as with the UDP
    // sender's failure path).
    bool sendOSC(oscpkt::Message m);

private:
    struct Impl;
    std::unique_ptr<Impl> m_impl;
};

} // namespace SonicPi

#endif // TCPOSCSENDER_H
