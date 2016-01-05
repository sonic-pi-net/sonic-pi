#ifndef SONICPIOSCSERVER_H
#define SONICPIOSCSERVER_H

#include <QObject>
#include "oschandler.h"

class SonicPiOSCServer : public QObject
{
    Q_OBJECT
public:
    explicit SonicPiOSCServer(MainWindow *parent = 0, OscHandler *handler = 0);
    bool waitForServer();
    bool isIncomingPortOpen();
    bool isServerStarted();


signals:

public slots:
   virtual void stop();
   virtual void start();

protected:
    OscHandler* handler;
    MainWindow* parent;
    bool osc_incoming_port_open;
    bool stop_server;

    bool continueListening();


};

#endif // SONICPIOSCSERVER_H
