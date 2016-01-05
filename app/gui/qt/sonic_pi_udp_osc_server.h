#ifndef SONIC_PI_UDP_OSC_SERVER_H
#define SONIC_PI_UDP_OSC_SERVER_H

#include "oschandler.h"
#include "sonic_pi_osc_server.h"
#include "mainwindow.h"

class SonicPiUDPOSCServer : public SonicPiOSCServer
{
    Q_OBJECT

public:
    explicit SonicPiUDPOSCServer(MainWindow *parent, OscHandler *handler = 0);

public slots:
    void stop();
    void start();

};

#endif // SONIC_PI_UDP_OSC_SERVER_H
