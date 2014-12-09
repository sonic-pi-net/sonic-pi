#include "sonicpiudpserver.h"
#include "udp.hh"

SonicPiUDPServer::SonicPiUDPServer(MainWindow *sonicPiWindow, OscHandler *oscHandler)
{
  handler = oscHandler;
  osc_incoming_port_open = false;
  parent = sonicPiWindow;
  stop_server = false;
}

bool SonicPiUDPServer::waitForServer(){
  return !handler->server_started && continueListening();
}

bool SonicPiUDPServer::continueListening(){
  return !handler->signal_server_stop && !stop_server;
}

bool SonicPiUDPServer::isIncomingPortOpen(){
  return osc_incoming_port_open;
}

bool SonicPiUDPServer::isServerStarted(){
  return handler->server_started;
}

void SonicPiUDPServer::stopServer(){
  stop_server = true;
}

void SonicPiUDPServer::startServer(){
    qDebug() << "starting OSC Server";
    int PORT_NUM = 4558;
    oscpkt::UdpSocket sock;
    sock.bindTo(PORT_NUM);
    qDebug()<< "Listening on port 4558";
    if (!sock.isOk()) {
      std::cout << "Unable to listen to OSC messages on port 4558";
      parent->invokeStartupError(tr("Is Sonic Pi already running?  Can't open UDP port 4558."));
      return;
    }

    osc_incoming_port_open = true;

    while (sock.isOk() && continueListening()) {
      if (sock.receiveNextPacket(30 /* timeout, in ms */)) {
        handler->oscMessage(sock.buffer);
        std::vector<char>().swap(sock.buffer);
      }
    }
}
