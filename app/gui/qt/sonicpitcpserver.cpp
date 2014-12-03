#include "sonicpitcpserver.h"
#include "mainwindow.h"

// Standard stuff
#include <iostream>
#include <math.h>
#include <sstream>
#include <fstream>

// Qt stuff
#include <QtNetwork>
#include <QTcpSocket>

// QScintilla stuff
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>
#include "sonicpilexer.h"
#include "sonicpiapis.h"
#include "sonicpiscintilla.h"
#include "sonicpiserver.h"

// OSC stuff
#include "oscpkt.hh"

SonicPiTCPServer::SonicPiTCPServer(OscHandler *oscHandler)
{
    handler = oscHandler;

    tcpServer = new QTcpServer();
    buffer.clear();

    connect(tcpServer, SIGNAL(newConnection()), this, SLOT(client()));
}

bool SonicPiTCPServer::isServerReady(){
  return handler->server_started && handler->cont_listening_for_osc;
}

bool SonicPiTCPServer::isIncomingPortOpen(){
  return handler->osc_incoming_port_open;
}

bool SonicPiTCPServer::continueListening(){
  handler->cont_listening_for_osc;
}

bool SonicPiTCPServer::isServerStarted(){
  return handler->server_started;
}

void SonicPiTCPServer::startServer(){
    int PORT_NUM = 4558;
    if(tcpServer->listen(QHostAddress::LocalHost, PORT_NUM)){
      qDebug() << "Server started: " << PORT_NUM;
      handler->server_started = true;
    }
    else{
      qDebug() << "Server: not started!";
    }

 }

void SonicPiTCPServer::logError(QAbstractSocket::SocketError e){
    qDebug() << e;
}

void SonicPiTCPServer::client(){
    socket = tcpServer->nextPendingConnection();
    connect(socket, SIGNAL(readyRead()), this, SLOT(readMessage()));
    connect(socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(logError(QAbstractSocket::SocketError)));
    connect(socket, SIGNAL(disconnected()), socket, SLOT(deleteLater()));
    std::vector<char>().swap(buffer);
}

void SonicPiTCPServer::readMessage()
{
    if (socket->bytesAvailable() < 0) {return;}
    int totalBytesRead = buffer.size();

    buffer.resize(totalBytesRead + socket->bytesAvailable());
    int bytesRead = socket->read(&buffer[0+totalBytesRead], (int)buffer.size() - totalBytesRead);
      if(bytesRead < 0) {
        return;
      }
      totalBytesRead += bytesRead;
      if (buffer[totalBytesRead-1] == '\00'){
          buffer.resize(totalBytesRead);
          std::vector<char> tmp(buffer);
          tmp.swap(buffer);
          handler->oscMessage(buffer);
          std::vector<char>().swap(buffer);
      }
}
