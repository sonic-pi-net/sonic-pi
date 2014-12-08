#ifndef SONICPISERVER_H
#define SONICPISERVER_H

#include <QObject>
#include "oschandler.h"

class SonicPiServer : public QObject
{
    Q_OBJECT
public:
    explicit SonicPiServer(MainWindow *parent = 0, OscHandler *handler = 0);
    bool waitForServer();
    bool isIncomingPortOpen();
    bool isServerStarted();


signals:

public slots:
   virtual void stopServer();
   virtual void startServer();

protected:
    OscHandler* handler;
    MainWindow* parent;
    bool osc_incoming_port_open;
    bool stop_server;

    bool continueListening();


};

#endif // SONICPISERVER_H
