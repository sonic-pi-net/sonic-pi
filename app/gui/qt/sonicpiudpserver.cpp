#include "sonicpiudpserver.h"
#include "udp.hh"

SonicPiUDPServer::SonicPiUDPServer(OscHandler* oscHandler)
{
  handler = oscHandler;
}

bool SonicPiUDPServer::isServerReady(){
  return handler->server_started && handler->cont_listening_for_osc;
}

bool SonicPiUDPServer::continueListening(){
  handler->cont_listening_for_osc;
}

bool SonicPiUDPServer::isIncomingPortOpen(){
  return handler->osc_incoming_port_open;
}

bool SonicPiUDPServer::isServerStarted(){
  return handler->server_started;
}

void SonicPiUDPServer::startServer(){
    qDebug() << "STARTING SERVER";
    qDebug() << "starting OSC Server";
    int PORT_NUM = 4558;
    oscpkt::UdpSocket sock;
    sock.bindTo(PORT_NUM);
    qDebug()<< "Listening on port 4558";
    if (!sock.isOk()) {
      qDebug() << "Unable to listen to OSC messages on port 4558";
      //invokeStartupError(tr("Is Sonic Pi already running?  Can't open UDP port 4558."));
      return;
    }

    handler->osc_incoming_port_open = true;

    while (sock.isOk() && handler->cont_listening_for_osc) {
      if (sock.receiveNextPacket(30 /* timeout, in ms */)) {
        handler->oscMessage(sock.buffer);
        //std::vector<char>().swap(sock.buffer);
      }
    }
}
