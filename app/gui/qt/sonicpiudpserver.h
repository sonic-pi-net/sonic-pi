#ifndef SONICPIUDPSERVER_H
#define SONICPIUDPSERVER_H

#include "oschandler.h"
#include "mainwindow.h"

class SonicPiUDPServer : public QObject
{
    Q_OBJECT

public:
    SonicPiUDPServer(MainWindow *parent, OscHandler *handler = 0);
    bool waitForServer();
    bool isIncomingPortOpen();
    bool isServerStarted();
    void stopServer();

public slots:
    void startServer();

private:
    OscHandler* handler;
    MainWindow* parent;
    bool osc_incoming_port_open;
    bool stop_server;

    bool continueListening();
};

#endif // SONICPIUDPSERVER_H
