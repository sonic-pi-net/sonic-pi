// OSC stuff
#include "oscpkt.hh"
#include "oschandler.h"
#include "mainwindow.h"

#include <QTextEdit>

OscHandler::OscHandler(MainWindow *parent, QTextEdit *outPane, QTextEdit *errorPane, SonicPiTheme *theme)
{
    window = parent;
    out = outPane;
    error = errorPane;
    signal_server_stop = false;
    server_started = false;
    this->theme = theme;
}

void OscHandler::oscMessage(std::vector<char> buffer){
    pr.init(&buffer[0], buffer.size());

    oscpkt::Message *msg;
    while (pr.isOk() && (msg = pr.popMessage()) != 0) {
      if (msg->match("/multi_message")){
        int msg_count;
        int msg_type;
        int job_id;
        std::string thread_name;
        std::string runtime;
        std::string s;
        QString ss;

        oscpkt::Message::ArgReader ar = msg->arg();
        ar.popInt32(job_id);
        ar.popStr(thread_name);
        ar.popStr(runtime);
        ar.popInt32(msg_count);
        QMetaObject::invokeMethod(out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor(theme->color("LogDefaultForeground"))));
        ss.append("[Run ").append(QString::number(job_id));
        ss.append(", Time ").append(QString::fromStdString(runtime));
        if(!thread_name.empty()) {
          ss.append(", Thread :").append(QString::fromStdString(thread_name));
        }
        ss.append("]");
        QMetaObject::invokeMethod(out, "append", Qt::QueuedConnection,
                                   Q_ARG(QString, ss) );

        for(int i = 0 ; i < msg_count ; i++) {
          ss = "";
          ar.popInt32(msg_type);
          ar.popStr(s);

          if(i == (msg_count - 1)) {
            ss.append(QString::fromUtf8(" └─ "));
          } else {
            ss.append(QString::fromUtf8(" ├─ "));
          }

          QMetaObject::invokeMethod(out, "append", Qt::QueuedConnection,
                                     Q_ARG(QString, ss) );


          ss = "";


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

          ss.append(QString::fromUtf8(s.c_str()));

          QMetaObject::invokeMethod( out, "insertPlainText", Qt::QueuedConnection,
                                     Q_ARG(QString, ss) );

          QMetaObject::invokeMethod( out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor(theme->color("LogDefaultForeground"))));
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogBackground")));



        }
        QMetaObject::invokeMethod( out, "append", Qt::QueuedConnection,
                                   Q_ARG(QString,  QString::fromStdString(" ")) );
      }
      else if (msg->match("/info")) {
        std::string s;
        if (msg->arg().popStr(s).isOkNoMoreArgs()) {
          // Evil nasties!
          // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html

          QMetaObject::invokeMethod( out, "setTextColor",           Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogInfoForeground")));
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogInfoBackground")));

          QMetaObject::invokeMethod( out, "append",                 Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString("=> " + s + "\n")) );

          QMetaObject::invokeMethod( out, "setTextColor",           Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, theme->color("LogBackground")));
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /info "<< std::endl;
        }
      }
      else if (msg->match("/error")) {
        int job_id;
        std::string desc;
        std::string backtrace;
        QString style_sheet = "qrc:///html/styles.css";
        if(window->dark_mode->isChecked()) {
          style_sheet = "qrc:///html/dark_styles.css";
        }
        if (msg->arg().popInt32(job_id).popStr(desc).popStr(backtrace).isOkNoMoreArgs()) {
          // Evil nasties!
          // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html
          QMetaObject::invokeMethod( error, "show", Qt::QueuedConnection);
          QMetaObject::invokeMethod( error, "clear", Qt::QueuedConnection);
          QMetaObject::invokeMethod( error, "setHtml", Qt::QueuedConnection,
                                     Q_ARG(QString, "<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"" + style_sheet + "\"/></head><body><h1><pre>Error: " + QString::fromStdString(desc) + "</pre></h1><pre class=\"backtrace\">" + QString::fromStdString(backtrace) + "</pre></body></html>") );

        } else {
          std::cout << "[GUI] - unhandled OSC msg /error: "<< std::endl;
        }
      }
      else if (msg->match("/replace-buffer")) {
        std::string id;
        std::string content;
        int line;
        int index;
        int line_number;
        if (msg->arg().popStr(id).popStr(content).popInt32(line).popInt32(index).popInt32(line_number).isOkNoMoreArgs()) {

          QMetaObject::invokeMethod( window, "replaceBuffer", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(id)), Q_ARG(QString, QString::fromStdString(content)), Q_ARG(int, line), Q_ARG(int, index), Q_ARG(int, line_number));
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /replace-buffer: "<< std::endl;
        }
      }
      else if (msg->match("/replace-lines")) {
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
      else if (msg->match("/exited")) {
        if (msg->arg().isOkNoMoreArgs()) {
          std::cout << "[GUI] - server asked us to exit" << std::endl;
          signal_server_stop = true;
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /exited: "<< std::endl;
        }
      }
      else if (msg->match("/exited_with_boot_error")) {
        std::string error_message;
        if (msg->arg().popStr(error_message).isOkNoMoreArgs()) {
          std::cout << "[GUI] - server failed to start with this error message: " << std::endl;
          std::cout << "      > " << error_message << std::endl;
          signal_server_stop = true;
        } else {
          std::cout << "[GUI] - error: unhandled OSC msg /exited_with_error: "<< std::endl;
        }
      }
      else if (msg->match("/ack")) {
        std::string id;
        if (msg->arg().popStr(id).isOkNoMoreArgs()) {
          if(!server_started) {
            QMetaObject::invokeMethod(window, "serverStarted", Qt::QueuedConnection);
          }
          server_started = true;

        } else
          std::cout << "[GUI] - error: unhandled OSC msg /ack " << std::endl;
      }
      else {
        std::cout << "[GUI] - error: unhandled OSC message" << std::endl;
      }
    }

}
