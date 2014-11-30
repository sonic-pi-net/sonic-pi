#ifndef SONICPITCPSERVER_H
#define SONICPITCPSERVER_H

#include <QtCore>
#include <QtNetwork>
#include <QTcpSocket>
#include "mainwindow.h"
#include <QMainWindow>
#include <QDialog>
#include <QLabel>
#include <QSplashScreen>
#include <QCheckBox>
#include <QListWidgetItem>
#include <QListWidget>
#include <QProcess>
#include <QFuture>
#include <QShortcut>
#include <QHash>
#include <QTcpSocket>
#include "oscpkt.hh"
#include <iostream>
#include <sstream>
#include <fstream>

// OS specific stuff
#if defined(Q_OS_WIN)
  #include <QtConcurrent/QtConcurrentRun>
  void sleep(int x) { Sleep((x)*1000); }
#elif defined(Q_OS_MAC)
  #include <QtConcurrent/QtConcurrentRun>
#else
  //assuming Raspberry Pi
  #include <cmath>
  #include <QtConcurrentRun>
#endif

#include "sonicpiserver.h"
#include "oschandler.h"

// OSC stuff
#include "oscpkt.hh"

class SonicPiTCPServer :  public SonicPiServer
{

public:
    SonicPiTCPServer(OscHandler *parent = 0);
    void startServer();
    bool isServerReady();
    bool isIncomingPortOpen();
    bool continueListening();
    bool isServerStarted();

public slots:
    void readMessage();
    void client();
    void logError(QAbstractSocket::SocketError);

private:
    void handleMessage();

    OscHandler* handler;

    QTcpServer *tcpServer;
    QTcpSocket *socket;
    std::vector<char> buffer;
};

#endif // SONICPITCPSERVER_H
