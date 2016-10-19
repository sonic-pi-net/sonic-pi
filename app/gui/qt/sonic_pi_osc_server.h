//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONICPIOSCSERVER_H
#define SONICPIOSCSERVER_H

#include <QObject>
#include "oschandler.h"

class SonicPiOSCServer : public QObject
{
    Q_OBJECT
public:
    explicit SonicPiOSCServer(MainWindow *parent = 0, OscHandler *handler = 0, int port_num = 4558);
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
    int port_num;
    bool continueListening();


};

#endif // SONICPIOSCSERVER_H
