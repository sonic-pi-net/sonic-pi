#ifndef SONICPITCPSERVER_H
#define SONICPITCPSERVER_H

#include "oschandler.h"
#include "sonicpiserver.h"
#include "mainwindow.h"

#include <QtCore>
#include <QtNetwork>
#include <QTcpSocket>

#include "oschandler.h"
#include "sonicpiserver.h"
#include "mainwindow.h"

class SonicPiTCPServer :  public SonicPiServer
{
    Q_OBJECT

public:
    explicit SonicPiTCPServer(MainWindow *parent, OscHandler *handler = 0);

    quint32 blockSize;

public slots:
    void stopServer();
    void startServer();
    void readMessage();
    void client();
    void logError(QAbstractSocket::SocketError);

private:
    void handleMessage();

    QTcpServer *tcpServer;
    QTcpSocket *socket;
    std::vector<char> buffer;
};

#endif // SONICPITCPSERVER_H
