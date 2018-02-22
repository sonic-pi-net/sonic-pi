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

#ifndef OSCHANDLER_H
#define OSCHANDLER_H

#include "oscpkt.hh"
#include "sonicpitheme.h"
#include "mainwindow.h"

class OscHandler
{

public:
  OscHandler(MainWindow *parent = 0, SonicPiLog *out = 0, SonicPiLog *incoming = 0, SonicPiTheme *theme = 0);
    void oscMessage(std::vector<char> buffer);
    bool signal_server_stop;
    bool server_started;

private:
    SonicPiTheme *theme;
    MainWindow *window;
    SonicPiLog  *out;
    SonicPiLog  *incoming;
    int last_incoming_path_lens [20];

    oscpkt::PacketReader pr;
    oscpkt::PacketWriter pw;
};

#endif // OSCHANDLER_H
