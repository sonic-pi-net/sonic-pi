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

#include "api/sonicpi_api.h"
#include "api/logger.h"

#include "api/osc/udp_osc_server.h"
#include "api/osc/udp.hh"


namespace SonicPi
{

OscServerUDP::OscServerUDP(IAPIClient* pClient, std::shared_ptr<OscHandler> spHandler, int port)
    : OscServer(pClient, spHandler, port)
{
    osc_incoming_port_open = false;
    stop_server = false;
}

void OscServerUDP::stop()
{
    LOG(INFO, "Stopping UDP OSC Server...");
    stop_server = true;
}

void OscServerUDP::start()
{
    LOG(INFO, "Starting UDP OSC Server on port " << port_num << "...");
    oscpkt::UdpSocket sock;
    sock.bindTo(port_num);
    if (!sock.isOk())
    {
        LOG(ERR, "Unable to listen to UDP OSC messages on port " << port_num);

        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "Is Sonic Pi already running?  Can't open UDP port: " + std::to_string(port_num);
        m_pClient->Report(message);
        return;
    }

    LOG(INFO, "UDP OSC Server ready and listening");

    osc_incoming_port_open = true;

    while (sock.isOk() && continueListening())
    {
        if (sock.receiveNextPacket(30 /* timeout, in ms */))
        {
            handler->oscMessage(sock.buffer);
            std::vector<char>().swap(sock.buffer);
        }
    }

    if(!sock.isOk()) {
      LOG(INFO, "UDP OSC Socket no longer OK");
    }

    if(stop_server) {
      LOG(INFO, "UDP OSC Server has been asked to stop listening");
    }

    LOG(INFO, "UDP OSC Server is no longer listening");

}

} // namespace SonicPi
