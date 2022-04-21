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


#include "sonic_pi_osc_server.h"
#include <iostream>

SonicPiOSCServer::SonicPiOSCServer(MainWindow *sonicPiWindow, OscHandler *oscHandler, int port_num) :
    QObject(sonicPiWindow)
{
    handler = oscHandler;
    osc_incoming_port_open = false;
    parent = sonicPiWindow;
    stop_server = false;
}

bool SonicPiOSCServer::waitForServer(){
  return !handler->server_started && continueListening();
}

bool SonicPiOSCServer::continueListening(){
  return !handler->signal_server_stop && !stop_server;
}

bool SonicPiOSCServer::isIncomingPortOpen(){
  return osc_incoming_port_open;
}

bool SonicPiOSCServer::isServerStarted(){
  return handler->server_started;
}

void SonicPiOSCServer::stop(){}
void SonicPiOSCServer::start(){}
