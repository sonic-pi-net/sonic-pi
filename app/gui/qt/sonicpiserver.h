#ifndef SONICPISERVER_H
#define SONICPISERVER_H

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

// OSC stuff
#include "oscpkt.hh"

class SonicPiServer : public QObject
{
    Q_OBJECT

public:
    SonicPiServer(MainWindow *parent = 0);
    bool server_started;
    bool osc_incoming_port_open;
    bool cont_listening_for_osc;

public slots:
    void startServer();
    void readMessage();
    void client();
    void logError(QAbstractSocket::SocketError);

private:
    void handleMessage();

    MainWindow *parent;
    QTextEdit *out;
    QTextEdit *error;

    QTcpServer *tcpServer;
    QTcpSocket *socket;
    std::vector<char> buffer;
    oscpkt::PacketReader pr;
    oscpkt::PacketWriter pw;
};

#endif // SONICPISERVER_H
