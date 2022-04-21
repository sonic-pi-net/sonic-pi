//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONIC_PI_UDP_OSC_SERVER_H
#define SONIC_PI_UDP_OSC_SERVER_H

#include "oschandler.h"
#include "sonic_pi_osc_server.h"
#include "mainwindow.h"

class SonicPiUDPOSCServer : public SonicPiOSCServer
{
    Q_OBJECT

public:
    explicit SonicPiUDPOSCServer(MainWindow *parent, OscHandler *handler = 0, int  port = 4558);

public slots:
    void stop();
    void start();

};

#endif // SONIC_PI_UDP_OSC_SERVER_H
