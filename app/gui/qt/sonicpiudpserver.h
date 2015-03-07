#ifndef SONICPIUDPSERVER_H
#define SONICPIUDPSERVER_H

#include "oschandler.h"
#include "sonicpiserver.h"
#include "mainwindow.h"

class SonicPiUDPServer : public SonicPiServer
{
    Q_OBJECT

public:
    explicit SonicPiUDPServer(MainWindow *parent, OscHandler *handler = 0);

public slots:
    void stopServer();
    void startServer();

};

#endif // SONICPIUDPSERVER_H
