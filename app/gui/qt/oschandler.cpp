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


// OSC stuff
#include "oscpkt.hh"
#include "oschandler.h"
#include "mainwindow.h"
#include "sonicpilog.h"

#include <QTextEdit>

OscHandler::OscHandler(MainWindow *parent, SonicPiLog *outPane,  SonicPiLog *incomingPane, SonicPiTheme *theme)
{
    window = parent;
    out = outPane;
    incoming = incomingPane;
    signal_server_stop = false;
    server_started = false;
    int last_incoming_path_lens [20] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    this->theme = theme;
}

void OscHandler::oscMessage(std::vector<char> buffer){
    pr.init(&buffer[0], buffer.size());

    oscpkt::Message *msg;
    while (pr.isOk() && (msg = pr.popMessage()) != 0) {
      if (msg->match("/log/multi_message")){
        int msg_count;
        SonicPiLog::MultiMessage mm;
        mm.theme = theme;

        oscpkt::Message::ArgReader ar = msg->arg();
        ar.popInt32(mm.job_id);
        ar.popStr(mm.thread_name);
        ar.popStr(mm.runtime);
        ar.popInt32(msg_count);

        for(int i = 0 ; i < msg_count ; i++) {
          SonicPiLog::Message message;
          ar.popInt32(message.msg_type);
          ar.popStr(message.s);
          mm.messages.push_back(message);
        }

        QMetaObject::invokeMethod( out, "handleMultiMessage", Qt::QueuedConnection,
                                   Q_ARG(SonicPiLog::MultiMessage, mm ) );
      }
      else if (msg->match("/incoming/osc")) {
        std::string time;
        int id;
        std::string address;
        std::string args;
        if (msg->arg().popStr(time).popInt32(id).popStr(address).popStr(args).isOkNoMoreArgs()) {
          int max_path_len = 0;
          for (int i = 0; i < 20 ; i++) {
            if (last_incoming_path_lens[i] > max_path_len) {
              max_path_len = last_incoming_path_lens[i];
            }
          }
          int len_diff = max_path_len - address.length();
          len_diff = (len_diff < 10) ? len_diff : 0;
          len_diff = std::max(len_diff, 0);
          len_diff = len_diff + 1;
          int idmod = ((id * 3) % 200);
          idmod = 155 + ((idmod < 100) ? idmod : 200 - idmod);

          QString qs_address =  QString::fromStdString(address);
          if(!qs_address.startsWith(":")) {
              QMetaObject::invokeMethod( incoming, "setTextBgFgColors",      Qt::QueuedConnection, Q_ARG(QColor, QColor(255, 20, 147, idmod)), Q_ARG(QColor, "white"));

              QMetaObject::invokeMethod( incoming, "appendPlainText",        Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(" " + address) ) );

              QMetaObject::invokeMethod( incoming, "insertPlainText",        Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(std::string(len_diff, ' ')) ) );

              QMetaObject::invokeMethod( incoming, "setTextBgFgColors",      Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogBackground")), Q_ARG(QColor, "white"));

              QMetaObject::invokeMethod( incoming, "insertPlainText",        Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(" ")));

              QMetaObject::invokeMethod( incoming, "setTextBgFgColors",      Qt::QueuedConnection, Q_ARG(QColor, QColor(255, 153, 0, idmod)), Q_ARG(QColor, "white"));
              QMetaObject::invokeMethod( incoming, "insertPlainText",        Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(args) ) );
              last_incoming_path_lens[id % 20] = address.length();
            }
          QMetaObject::invokeMethod( window, "addCuePath", Qt::QueuedConnection, Q_ARG(QString, qs_address), Q_ARG(QString, QString::fromStdString(args)));
            } else {
              std::cout << "[GUI] - unhandled OSC msg /incoming/osc: "<< std::endl;
        }
      }
      else if (msg->match("/log/info")) {
        std::string s;
        int style;
        if (msg->arg().popInt32(style).popStr(s).isOkNoMoreArgs()) {
          // Evil nasties!
          // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html

          QMetaObject::invokeMethod( out, "setTextColor",           Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogInfoForeground")));
          if(style == 1) {
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogInfoBackgroundStyle1")));
          } else {
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogInfoBackground")));
          }

          QMetaObject::invokeMethod( out, "appendPlainText",        Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString("=> " + s + "\n")) );

          QMetaObject::invokeMethod( out, "setTextColor",           Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogDefaultForeground")));
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogBackground")));
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /info "<< std::endl;
        }
      }
      else if (msg->match("/error")) {
        int job_id;
        int line;
        std::string desc;
        std::string backtrace;
        if (msg->arg().popInt32(job_id).popStr(desc).popStr(backtrace).popInt32(line).isOkNoMoreArgs()) {
          // Evil nasties!
          // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html
          QMetaObject::invokeMethod( window, "setLineMarkerinCurrentWorkspace", Qt::QueuedConnection, Q_ARG(int, line));
          QMetaObject::invokeMethod( window, "showError", Q_ARG(QString, "<h2 class=\"error_description\"><pre>Runtime Error: " + QString::fromStdString(desc) + "</pre></h2><pre class=\"backtrace\">" + QString::fromStdString(backtrace) + "</pre>"));
        } else {
          std::cout << "[GUI] - unhandled OSC msg /error: "<< std::endl;
        }
      }
      else if (msg->match("/syntax_error")) {
        int job_id;
        int line;
        std::string desc;
        std::string error_line;
        std::string line_num_s;
        if (msg->arg().popInt32(job_id).popStr(desc).popStr(error_line).popInt32(line).popStr(line_num_s).isOkNoMoreArgs()) {
          // Evil nasties!
          // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html
          QMetaObject::invokeMethod( window, "setLineMarkerinCurrentWorkspace", Qt::QueuedConnection, Q_ARG(int, line));

          QString html_response = "<h2 class=\"syntax_error_description\"><pre>Syntax Error: " + QString::fromStdString(desc) + "</pre></h2><pre class=\"error_msg\">";
          if(line == -1) {
            html_response = html_response + "</span></pre>";
          } else {
            html_response = html_response + "[Line " + QString::fromStdString(line_num_s) + "]: <span class=\"error_line\">" + QString::fromStdString(error_line) + "</span></pre>";
              }
          QMetaObject::invokeMethod( window, "showError", Q_ARG(QString, html_response));
        } else {
          std::cout << "[GUI] - unhandled OSC msg /error: "<< std::endl;
        }
      }
      else if (msg->match("/buffer/replace")) {
        std::string id;
        std::string content;
        int line;
        int index;
        int line_number;
        if (msg->arg().popStr(id).popStr(content).popInt32(line).popInt32(index).popInt32(line_number).isOkNoMoreArgs()) {

          QMetaObject::invokeMethod( window, "replaceBuffer", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(id)), Q_ARG(QString, QString::fromStdString(content)), Q_ARG(int, line), Q_ARG(int, index), Q_ARG(int, line_number));
	  window->loaded_workspaces = true; // it's now safe to save the buffers
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /replace-buffer: "<< std::endl;
        }
      }
      else if (msg->match("/buffer/replace-idx")) {
        int buf_idx;
        std::string content;
        int line;
        int index;
        int line_number;
        if (msg->arg().popInt32(buf_idx).popStr(content).popInt32(line).popInt32(index).popInt32(line_number).isOkNoMoreArgs()) {

          QMetaObject::invokeMethod( window, "replaceBufferIdx", Qt::QueuedConnection, Q_ARG(int, buf_idx), Q_ARG(QString, QString::fromStdString(content)), Q_ARG(int, line), Q_ARG(int, index), Q_ARG(int, line_number));
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /replace-buffer: "<< std::endl;
        }
      }
      else if (msg->match("/update-info-text")) {
        std::string content;
        if (msg->arg().popStr(content).isOkNoMoreArgs()) {
          QMetaObject::invokeMethod( window, "setUpdateInfoText", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(content)));
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /update_info_text: "<< std::endl;
        }
      }
      else if (msg->match("/buffer/replace-lines")) {
        std::string id;
        std::string content;
        int start_line;
        int finish_line;
        int point_line;
        int point_index;
        if (msg->arg().popStr(id).popStr(content).popInt32(start_line).popInt32(finish_line).popInt32(point_line).popInt32(point_index).isOkNoMoreArgs()) {

          QMetaObject::invokeMethod( window, "replaceLines", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(id)), Q_ARG(QString, QString::fromStdString(content)), Q_ARG(int, start_line),Q_ARG(int, finish_line), Q_ARG(int, point_line), Q_ARG(int, point_index));
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /replace-lines: "<< std::endl;
        }
      }
      else if (msg->match("/buffer/run-idx")) {
        int buf_idx;
        if (msg->arg().popInt32(buf_idx).isOkNoMoreArgs()) {
          QMetaObject::invokeMethod( window, "runBufferIdx", Qt::QueuedConnection, Q_ARG(int, buf_idx));
        } else {
         std::cout << "[GUI] - error: unhandled OSC msg /buffer/run-idx: "<< std::endl;
        }
      }
      else if (msg->match("/exited")) {
        if (msg->arg().isOkNoMoreArgs()) {
          std::cout << "[GUI] - server asked us to exit" << std::endl;
          signal_server_stop = true;
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /exited: "<< std::endl;
        }
      }
      else if (msg->match("/exited-with-boot-error")) {
        std::string error_message;
        if (msg->arg().popStr(error_message).isOkNoMoreArgs()) {
          std::cout << std::endl << "[GUI] - Sonic Pi Server failed to start with this error message: " << std::endl;
          std::cout << "      > " << error_message << std::endl;
          signal_server_stop = true;
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /exited-with-boot-error: "<< std::endl;
        }
      }
      else if (msg->match("/ack")) {
        std::string id;
        if (msg->arg().popStr(id).isOkNoMoreArgs()) {
          server_started = true;
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /ack " << std::endl;
        }
      }
      else if (msg->match("/midi/out-ports")) {
        std::string port_info;
        if (msg->arg().popStr(port_info).isOkNoMoreArgs()) {
          QMetaObject::invokeMethod( window, "updateMIDIOutPorts", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(port_info)));
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /midi/out-ports: "<< std::endl;
        }
      }
      else if (msg->match("/midi/in-ports")) {
        std::string port_info;
        if (msg->arg().popStr(port_info).isOkNoMoreArgs()) {
          QMetaObject::invokeMethod( window, "updateMIDIInPorts", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(port_info)));
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /midi/in-ports: "<< std::endl;
        }
      } else if (msg->match("/version")) {
        std::string version;
        int version_num;
        std::string latest_version;
        int latest_version_num;
        int last_checked_day;
        int last_checked_month;
        int last_checked_year;
        std::string platform;

        if (msg->arg().popStr(version).popInt32(version_num).popStr(latest_version).popInt32(latest_version_num).popInt32(last_checked_day).popInt32(last_checked_month).popInt32(last_checked_year).popStr(platform).isOkNoMoreArgs()) {
          QDate date = QDate(last_checked_year, last_checked_month, last_checked_day);
          QMetaObject::invokeMethod( window, "updateVersionNumber", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(version)), Q_ARG(int, version_num), Q_ARG(QString, QString::fromStdString(latest_version)), Q_ARG(int, latest_version_num),Q_ARG(QDate, date), Q_ARG(QString, QString::fromStdString(platform)));
        } else
          std::cout << "[GUI] - error: unhandled OSC msg /version " << std::endl;
      }
      else if (msg->match("/runs/all-completed")) {
        if (msg->arg().isOkNoMoreArgs()) {
          QMetaObject::invokeMethod( window, "allJobsCompleted", Qt::QueuedConnection);
        } else
          std::cout << "[GUI] - error: unhandled OSC msg /runs/all-completed " << std::endl;
      }
      else {
        std::cout << "[GUI] - error: unhandled OSC message" << std::endl;
      }
    }

}
