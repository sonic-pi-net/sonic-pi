//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++
#pragma once

#include <vector>
#include <array>

#include <api/osc/osc_pkt.hh>

namespace SonicPi
{

struct IAPIClient;

class OscHandler
{

public:
    OscHandler(IAPIClient* pClient);
    void oscMessage(std::vector<char> buffer);
    bool m_signal_server_stop = false;
    bool m_server_started = false;

private:
    std::array<int, 20> m_last_incoming_path_lens;

    oscpkt::PacketReader pr;
    oscpkt::PacketWriter pw;
    IAPIClient* m_pClient = nullptr;
    uint64_t m_currentQueueIndex = 0;
};

} // namespace SonicPi
