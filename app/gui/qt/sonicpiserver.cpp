#include "sonicpiserver.h"
#include <QtNetwork>
#include <QTcpSocket>
#include "mainwindow.h"

// Standard stuff
#include <iostream>
#include <math.h>
#include <sstream>
#include <fstream>

// Qt stuff
#include <QDir>
#include <QAction>
#include <QApplication>
#include <QCloseEvent>
#include <QFile>
#include <QFileInfo>
#include <QFileDialog>
#include <QIcon>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QDockWidget>
#include <QPoint>
#include <QSettings>
#include <QSize>
#include <QStatusBar>
#include <QTextEdit>
#include <QTextBrowser>
#include <QToolBar>
#include <QProcess>
#include <QFont>
#include <QTabWidget>
#include <QString>
#include <QStringList>
#include <QTextStream>
#include <QSplashScreen>
#include <QPixmap>
#include <QLabel>
#include <QSlider>
#include <QPushButton>
#include <QGridLayout>
#include <QGroupBox>
#include <QRadioButton>
#include <QCheckBox>
#include <QScrollArea>
#include <QShortcut>
#include <QToolButton>

// QScintilla stuff
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>
#include "sonicpilexer.h"
#include "sonicpiapis.h"
#include "sonicpiscintilla.h"
#include "sonicpiserver.h"

#include <QtNetwork>
#include <QTcpSocket>

// OSC stuff
#include "oscpkt.hh"


// OSC stuff
#include "oscpkt.hh"

SonicPiServer::SonicPiServer(MainWindow *window)
{
    server_started = false;
    cont_listening_for_osc = true;
    osc_incoming_port_open = false;

    out = window->outputPane;
    error = window->errorPane;
    tcpServer = new QTcpServer();
    buffer.empty();
    connect(tcpServer, SIGNAL(newConnection()), this, SLOT(client()));
    //connect(tcpSocket, SIGNAL(error(QAbstractSocket::SocketError)));
}

void SonicPiServer::startServer(){
    int PORT_NUM = 4558;
    qDebug() << "Try and start";
    if(tcpServer->listen(QHostAddress::LocalHost, PORT_NUM)){
      qDebug() << "Server started: " << PORT_NUM;

    }
    else{
      qDebug() << "Server: not started!";
    }

 }

void SonicPiServer::client(){
    qDebug() << "Client!";
     socket = tcpServer->nextPendingConnection();
    connect(socket, SIGNAL(readyRead()), this, SLOT(readMessage()));
    std::vector<char>().swap(buffer);
}

void SonicPiServer::readMessage()
{
    qDebug() << "Ready Read!";
    if (socket->bytesAvailable() < 0) {return;}
    int totalBytesRead = buffer.size();

    buffer.resize(totalBytesRead + socket->bytesAvailable());
    int bytesRead = socket->read(&buffer[0+totalBytesRead], (int)buffer.size() - totalBytesRead);
      qDebug() << "read ping:" << bytesRead << "\n";
      if(bytesRead < 0) {
        qDebug() << "bytes received:" << bytesRead<< "\n";
        return;
      }
      totalBytesRead += bytesRead;
      if (buffer[totalBytesRead-1] == '\00'){
           qDebug() << "end meet:" << bytesRead << "\n";
          buffer.resize(totalBytesRead);
          std::vector<char> tmp(buffer);
          tmp.swap(buffer);
          handleMessage();
      }
}

void SonicPiServer::handleMessage(){
    pr.init(&buffer[0], buffer.size());

    if(pr.isOk()){
         qDebug("good data");
    }
    else{
         qDebug("bad data");
    }

    oscpkt::Message *msg;
    while (pr.isOk() && (msg = pr.popMessage()) != 0) {
      if (msg->match("/multi_message")){
        int msg_count;
        int msg_type;
        int job_id;
        std::string thread_name;
        std::string runtime;
        std::string s;
        std::ostringstream ss;

        oscpkt::Message::ArgReader ar = msg->arg();
        ar.popInt32(job_id);
        ar.popStr(thread_name);
        ar.popStr(runtime);
        ar.popInt32(msg_count);
        QMetaObject::invokeMethod(out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
        ss << "[Run " << job_id;
        ss << ", Time " << runtime;
        if(!thread_name.empty()) {
          ss << ", Thread :" << thread_name;
        }
        ss << "]";
        QMetaObject::invokeMethod(out, "append", Qt::QueuedConnection,
                                   Q_ARG(QString, QString::fromStdString(ss.str())) );

        for(int i = 0 ; i < msg_count ; i++) {
          ss.str("");
          ss.clear();
          ar.popInt32(msg_type);
          ar.popStr(s);

#if defined(Q_OS_WIN)
          if(i == (msg_count - 1)) {
            ss << " └─ ";
          } else {
            ss << " ├─ ";
          }
#elif defined(Q_OS_MAC)
          if(i == (msg_count - 1)) {
            ss << " └─ ";
          } else {
            ss << " ├─ ";
          }
#else
          //assuming Raspberry Pi
          if(i == (msg_count - 1)) {
            ss << " +- ";
          } else {
            ss << " |- ";
          }
#endif


          QMetaObject::invokeMethod(out, "append", Qt::QueuedConnection,
                                     Q_ARG(QString, QString::fromStdString(ss.str())) );


          ss.str("");
          ss.clear();

          switch(msg_type)
          {
          case 0:
            QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("deeppink")));
            break;
          case 1:
            QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("dodgerblue")));
            break;
          case 2:
            QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("darkorange")));
            break;
          case 3:
            QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("red")));
            break;
          case 4:
            QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
            QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("deeppink")));
            break;
          case 5:
            QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
            QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("dodgerblue")));
            break;
          case 6:
            QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
            QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("darkorange")));
            break;
          default:
            QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("green")));
          }

          ss << s;

          QMetaObject::invokeMethod( out, "insertPlainText", Qt::QueuedConnection,
                                     Q_ARG(QString, QString::fromStdString(ss.str())) );

          QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));



        }
        QMetaObject::invokeMethod( out, "append", Qt::QueuedConnection,
                                   Q_ARG(QString,  QString::fromStdString(" ")) );
      }
      else if (msg->match("/info")) {
        std::string s;
        if (msg->arg().popStr(s).isOkNoMoreArgs()) {
          // Evil nasties!
          // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html

          QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));

          QMetaObject::invokeMethod( out, "append", Qt::QueuedConnection,
                                     Q_ARG(QString, QString::fromStdString("=> " + s + "\n")) );

          QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
        } else {
          std::cout << "Server: unhandled info message: "<< std::endl;
        }
      }
      else if (msg->match("/error")) {
        int job_id;
        std::string desc;
        std::string backtrace;
        if (msg->arg().popInt32(job_id).popStr(desc).popStr(backtrace).isOkNoMoreArgs()) {
          // Evil nasties!
          // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html
          QMetaObject::invokeMethod( error, "show", Qt::QueuedConnection);
          QMetaObject::invokeMethod( error, "clear", Qt::QueuedConnection);
          QMetaObject::invokeMethod( error, "setHtml", Qt::QueuedConnection,
                                     Q_ARG(QString, "<table width=\"100%\"> cellpadding=\"2\"><tr><td bgcolor=\"#FFE4E1\"><h3><font color=\"black\"><pre>Error: " + QString::fromStdString(desc) + "</pre></font></h3></td></tr><tr><td bgcolor=\"#E8E8E8\"><h4><font color=\"#5e5e5e\", background-color=\"black\"><pre>" + QString::fromStdString(backtrace) + "</pre></font></h4></td></tr></table>") );

        } else {
          std::cout << "Server: unhandled error: "<< std::endl;
        }
      }
      else if (msg->match("/replace-buffer")) {
        std::string id;
        std::string content;
        if (msg->arg().popStr(id).popStr(content).isOkNoMoreArgs()) {

          QMetaObject::invokeMethod( parent, "replaceBuffer", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(id)), Q_ARG(QString, QString::fromStdString(content)));
        } else {
          std::cout << "Server: unhandled replace-buffer: "<< std::endl;
        }
      }
      else if (msg->match("/exited")) {
        if (msg->arg().isOkNoMoreArgs()) {
          std::cout << "server asked us to exit" << std::endl;
          cont_listening_for_osc = false;
        } else {
          std::cout << "Server: unhandled exited: "<< std::endl;
        }
      }
      else if (msg->match("/ack")) {
        std::string id;
        if (msg->arg().popStr(id).isOkNoMoreArgs()) {
          server_started = true;
        } else
          std::cout << "Server: unhandled ack " << std::endl;
      }
      else {
        std::cout << "Unknown message" << std::endl;
      }
    }
}
