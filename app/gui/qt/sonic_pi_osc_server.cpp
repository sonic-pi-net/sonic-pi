#include "sonic_pi_osc_server.h"

SonicPiOSCServer::SonicPiOSCServer(MainWindow *sonicPiWindow, OscHandler *oscHandler) :
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
