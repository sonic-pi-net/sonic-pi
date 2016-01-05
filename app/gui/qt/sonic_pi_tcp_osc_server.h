#ifndef SONIC_PI_TCP_OSC_SERVER_H
#define SONIC_PI_TCP_OSC_SERVER_H

#include "oschandler.h"
#include "sonic_pi_osc_server.h"
#include "mainwindow.h"

#include <QtCore>
#include <QtNetwork>
#include <QTcpSocket>

class SonicPiTCPOSCServer :  public SonicPiOSCServer
{
    Q_OBJECT

public:
    explicit SonicPiTCPOSCServer(MainWindow *parent, OscHandler *handler = 0);

    quint32 blockSize;

public slots:
    void stop();
    void start();
    void readMessage();
    void client();
    void logError(QAbstractSocket::SocketError);

private:
    void handleMessage();

    QTcpServer *tcpServer;
    QTcpSocket *socket;
    std::vector<char> buffer;
};

#endif // SONIC_PI_TCP_OSC_SERVER_H
