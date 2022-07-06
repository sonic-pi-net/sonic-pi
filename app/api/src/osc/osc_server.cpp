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

#include "api/osc/osc_server.h"
#include <iostream>

namespace SonicPi
{
OscServer::OscServer(IAPIClient* pClient, std::shared_ptr<OscHandler> oscHandler, int port)
    : handler(oscHandler)
    , port_num(port)
    , m_pClient(pClient)
{
    osc_incoming_port_open = false;
    stop_server = false;
}

bool OscServer::waitForServer()
{
    return !handler->m_server_started && continueListening();
}

bool OscServer::continueListening()
{
    return !handler->m_signal_server_stop && !stop_server;
}

bool OscServer::isIncomingPortOpen()
{
    return osc_incoming_port_open;
}

bool OscServer::isServerStarted()
{
    return handler->m_server_started;
}

void OscServer::stop() {}
void OscServer::start() {}

} // namespace SonicPi
