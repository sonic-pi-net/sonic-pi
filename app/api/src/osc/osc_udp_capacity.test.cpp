//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// Buffers travel GUI -> spider as a single UDP datagram, so the largest
// runnable buffer is bounded by the OS datagram limit. Regression guarded
// against: macOS caps UDP sends at net.inet.udp.maxdgram (9216 bytes) unless
// the socket raises SO_SNDBUF, silently breaking Run for buffers over ~9KB.

#include <string>

#include <catch2/catch_test_macros.hpp>

#include <api/osc/osc_pkt.hh>
#include <api/osc/udp.hh>
#include <api/osc/osc_sender.h>

TEST_CASE("OscSender delivers a 64KB code buffer in one datagram", "[osc]")
{
    oscpkt::UdpSocket receiver;
    REQUIRE(receiver.bindTo(0));

    std::string code;
    while (code.size() < 65000)
        code += "play 60 # udp capacity test padding\n";

    oscpkt::Message msg("/run-code");
    msg.pushInt32(1234);
    msg.pushStr(code);

    SonicPi::OscSender sender(receiver.boundPort());
    REQUIRE(sender.sendOSC(msg));

    REQUIRE(receiver.receiveNextPacket(5000));
    oscpkt::PacketReader pr;
    pr.init(receiver.packetData(), receiver.packetSize());
    oscpkt::Message* received = pr.popMessage();
    REQUIRE(received != nullptr);

    int token = 0;
    std::string receivedCode;
    REQUIRE(received->match("/run-code").popInt32(token).popStr(receivedCode).isOkNoMoreArgs());
    REQUIRE(token == 1234);
    REQUIRE(receivedCode == code);
}

TEST_CASE("OscSender rejects messages beyond the UDP datagram limit", "[osc]")
{
    oscpkt::UdpSocket receiver;
    REQUIRE(receiver.bindTo(0));

    oscpkt::Message msg("/run-code");
    msg.pushInt32(1234);
    msg.pushStr(std::string(66000, 'x'));

    SonicPi::OscSender sender(receiver.boundPort());
    REQUIRE_FALSE(sender.sendOSC(msg));
}
