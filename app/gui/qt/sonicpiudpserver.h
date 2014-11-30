#ifndef SONICPIUDPSERVER_H
#define SONICPIUDPSERVER_H

#include "sonicpiserver.h"
#include "oschandler.h"

class SonicPiUDPServer : public SonicPiServer
{

public:
    SonicPiUDPServer(OscHandler *parent = 0);
    bool isServerReady();
    bool isIncomingPortOpen();
    bool continueListening();
    bool isServerStarted();

public slots:
    void startServer();

private:
    OscHandler* handler;

};

#endif // SONICPIUDPSERVER_H
