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
#include <memory>

namespace SonicPi
{

struct IAPIClient;

class OscServer
{
public:
    explicit OscServer(IAPIClient* pClient, std::shared_ptr<OscHandler> spHandler, int port_num);
    bool waitForServer();
    bool isIncomingPortOpen();
    bool isServerStarted();

    virtual void stop();
    virtual void start();

protected:
    IAPIClient* m_pClient = nullptr;
    std::shared_ptr<OscHandler> handler;
    bool osc_incoming_port_open;
    bool stop_server;
    int port_num;
    bool continueListening();

};
} // namespace SonicPi
