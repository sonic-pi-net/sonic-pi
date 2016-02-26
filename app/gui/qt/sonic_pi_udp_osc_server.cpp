//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "sonic_pi_udp_osc_server.h"
#include "sonic_pi_osc_server.h"
#include "udp.hh"

SonicPiUDPOSCServer::SonicPiUDPOSCServer(MainWindow *sonicPiWindow, OscHandler *oscHandler) : SonicPiOSCServer(sonicPiWindow, oscHandler)
{
  handler = oscHandler;
  osc_incoming_port_open = false;
  parent = sonicPiWindow;
  stop_server = false;
}

void SonicPiUDPOSCServer::stop(){
  stop_server = true;
}

void SonicPiUDPOSCServer::start(){
  std::cout << "[GUI] - starting UDP OSC Server on port 4558..." << std::endl;
  int PORT_NUM = 4558;
  oscpkt::UdpSocket sock;
  sock.bindTo(PORT_NUM);
  if (!sock.isOk()) {
    std::cout << "[GUI] - unable to listen to UDP OSC messages on port 4558" << std::endl;
    parent->invokeStartupError(tr("Is Sonic Pi already running?  Can't open UDP port 4558."));
    return;
  }

  std::cout << "[GUI] - UDP OSC Server ready and listening" << std::endl << std::flush;

  osc_incoming_port_open = true;

  while (sock.isOk() && continueListening()) {
    if (sock.receiveNextPacket(30 /* timeout, in ms */)) {
      handler->oscMessage(sock.buffer);
      std::vector<char>().swap(sock.buffer);
      std::cout << std::flush;
    }
  }
}
