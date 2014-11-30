#ifndef OSCHANDLER_H
#define OSCHANDLER_H

#include "mainwindow.h"
#include "oscpkt.hh"

class OscHandler
{

public:
    OscHandler(MainWindow *parent = 0, QTextEdit *out = 0, QTextEdit *error = 0);
    void oscMessage(std::vector<char> buffer);
    bool cont_listening_for_osc;
    bool osc_incoming_port_open;
    bool server_started;

private:
    MainWindow *window;
    QTextEdit  *out;
    QTextEdit  *error;

    oscpkt::PacketReader pr;
    oscpkt::PacketWriter pw;
};

#endif // OSCHANDLER_H
