#include "sonicpiserver.h"

SonicPiServer::SonicPiServer(MainWindow *sonicPiWindow, OscHandler *oscHandler) :
    QObject(sonicPiWindow)
{
    handler = oscHandler;
    osc_incoming_port_open = false;
    parent = sonicPiWindow;
    stop_server = false;
}

bool SonicPiServer::waitForServer(){
  return !handler->server_started && continueListening();
}

bool SonicPiServer::continueListening(){
  return !handler->signal_server_stop && !stop_server;
}

bool SonicPiServer::isIncomingPortOpen(){
  return osc_incoming_port_open;
}

bool SonicPiServer::isServerStarted(){
  return handler->server_started;
}

void SonicPiServer::stopServer(){}
void SonicPiServer::startServer(){}
