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


// OSC stuff
#include "oscpkt.hh"
#include "udp.hh"

#include "oscsender.h"
using namespace oscpkt;

OscSender::OscSender(int port)
{
  this->port = port;
}

bool OscSender::sendOSC(Message m) {
  UdpSocket sock;
  sock.connectTo("127.0.0.1", port);
  if (!sock.isOk()) {
    std::cerr << "[OSC Sender] - Error connecting to port " << port << ": " << sock.errorMessage() << "\n";
    return false;
  } else {
    PacketWriter pw;
    pw.addMessage(m);
    return sock.sendPacket(pw.packetData(), pw.packetSize());
  }
}


void OscSender::bufferNewlineAndIndent(int point_line, int point_index, int first_line, std::string code, std::string fileName, std::string id) {

  Message msg("/buffer-newline-and-indent");
  msg.pushStr(id);
  msg.pushStr(fileName);
  msg.pushStr(code);
  msg.pushInt32(point_line);
  msg.pushInt32(point_index);
  msg.pushInt32(first_line);
  sendOSC(msg);
}
