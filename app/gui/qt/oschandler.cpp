// OSC stuff
#include "oscpkt.hh"
#include "oschandler.h"
#include "mainwindow.h"

#include <QTextEdit>

OscHandler::OscHandler(MainWindow *parent, QTextEdit *outPane, QTextEdit *errorPane)
{
    window = parent;
    out = outPane;
    error = errorPane;
    signal_server_stop = false;
    server_started = false;
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
        QMetaObject::invokeMethod(out, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
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

          QMetaObject::invokeMethod( out, "setTextColor",           Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
          QMetaObject::invokeMethod( out, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));

          QMetaObject::invokeMethod( out, "append",                 Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString("=> " + s + "\n")) );

          QMetaObject::invokeMethod( out, "setTextColor",           Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
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
                                     Q_ARG(QString, "<table width=\"100%\"> border=\"1\" bgcolor=\"deeppink\" cellpadding=\"0\"><tr><td bgcolor=\"white\"><h3><font color=\"deeppink\"><pre>Error: " + QString::fromStdString(desc) + "</pre></font></h3></td></tr><tr><td bgcolor=\"white\"><h4><font color=\"#5e5e5e\"><pre>" + QString::fromStdString(backtrace) + "</pre></font></h4></td></tr></table>") );

        } else {
          std::cout << "Server: unhandled error: "<< std::endl;
        }
      }
      else if (msg->match("/replace-buffer")) {
        std::string id;
        std::string content;
        if (msg->arg().popStr(id).popStr(content).isOkNoMoreArgs()) {

          QMetaObject::invokeMethod( window, "replaceBuffer", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(id)), Q_ARG(QString, QString::fromStdString(content)));
        } else {
          std::cout << "Server: unhandled replace-buffer: "<< std::endl;
        }
      }
      else if (msg->match("/exited")) {
        if (msg->arg().isOkNoMoreArgs()) {
          std::cout << "server asked us to exit" << std::endl;
          signal_server_stop = true;
        } else {
          std::cout << "Server: unhandled exited: "<< std::endl;
        }
      }
      else if (msg->match("/ack")) {
        std::string id;
        if (msg->arg().popStr(id).isOkNoMoreArgs()) {
          server_started = true;
          QMetaObject::invokeMethod(window, "serverStarted", Qt::QueuedConnection);
        } else
          std::cout << "Server: unhandled ack " << std::endl;
      }
      else {
        std::cout << "Unknown message" << std::endl;
      }
    }

}
