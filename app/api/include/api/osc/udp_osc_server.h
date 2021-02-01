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

#include "osc_handler.h"
#include "osc_server.h"

namespace SonicPi
{
class OscServerUDP : public OscServer
{
public:
    explicit OscServerUDP(IAPIClient* pClient, std::shared_ptr<OscHandler> spHandler, int port);

    void stop();
    void start();
};
} // namespace SonicPi
