#include "sonicpiudpserver.h"
#include "sonicpiserver.h"
#include "udp.hh"

SonicPiUDPServer::SonicPiUDPServer(MainWindow *sonicPiWindow, OscHandler *oscHandler) : SonicPiServer(sonicPiWindow, oscHandler)
{
  handler = oscHandler;
  osc_incoming_port_open = false;
  parent = sonicPiWindow;
  stop_server = false;
}

void SonicPiUDPServer::stopServer(){
  stop_server = true;
}

void SonicPiUDPServer::startServer(){
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
