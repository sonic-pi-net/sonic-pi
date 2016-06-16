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


#include "sonicpilog.h"
#include "mainwindow.h"
#include "oscpkt.hh"

// Standard stuff
#include <vector>
#include "sonicpitheme.h"
#include <QScrollBar>

SonicPiLog::SonicPiLog(MainWindow *window, QWidget *parent) : QPlainTextEdit(parent)
{
  forceScroll = true;
  setFocusPolicy(Qt::StrongFocus);
  this->window = window;
}

void SonicPiLog::keyPressEvent(QKeyEvent *event)
{
  keyEvent(event);
}

void SonicPiLog::keyReleaseEvent(QKeyEvent *event)
{
  keyEvent(event);
}

void SonicPiLog::keyEvent(QKeyEvent *event) {
  event->ignore();
  if (
    (event->modifiers() == Qt::NoModifier) ||
    (event->modifiers() == Qt::ShiftModifier) ||
    (event->modifiers() == Qt::KeypadModifier) ||
    (event->modifiers() == Qt::ShiftModifier + Qt::KeypadModifier)
    // ...but ignore any other keypad modifier combination
    ) {
    QString s = "";
    switch (event->key()) {
      // Special keys
      case Qt::Key_Escape:    s = "[escape]";    break;
      case Qt::Key_Up:        s = "[up]";        break;
      case Qt::Key_Left:      s = "[left]";      break;
      case Qt::Key_Right:     s = "[right]";     break;
      case Qt::Key_Down:      s = "[down]";      break;
      case Qt::Key_Home:      s = "[home]";      break;
      case Qt::Key_End:       s = "[end]";       break;
      case Qt::Key_PageUp:    s = "[pageup]";    break;
      case Qt::Key_PageDown:  s = "[pagedown]";  break;
      case Qt::Key_Insert:    s = "[insert]";    break;
      case Qt::Key_Delete:    s = "[delete]";    break;
      case Qt::Key_Backspace: s = "[backspace]"; break;
      case Qt::Key_Return:    s = "[return]";    break;
      case Qt::Key_Enter:     s = "[enter]";     break;
      default: break;
    }
    if (s != "") {
      // Handle shift key
      if (event->modifiers() == Qt::ShiftModifier) s = s.toUpper();
    } else {
      // Normal key on keyboard, let system handle shift modifier
      s = event->text();
    }
    if (s != "") {
      oscpkt::Message msg(event->type() == QEvent::KeyPress ? "/keyboard/press" : "/keyboard/release");
      msg.pushStr(s.toStdString());
      window->sendOSC(msg);
      event->accept();
    }
  }
}

void SonicPiLog::forceScrollDown(bool force)
{
  forceScroll = force;
}


void SonicPiLog::setTextColor(QColor c)
{
  QTextCharFormat tf;
  tf.setForeground(c);
  setCurrentCharFormat(tf);
}

void SonicPiLog::setTextBackgroundColor(QColor c)
{
  QTextCharFormat tf;
  tf.setBackground(c);
  setCurrentCharFormat(tf);
}

void SonicPiLog::setFontFamily(QString font_name)
{
  setFont(QFont(font_name));
}

void SonicPiLog::handleMultiMessage(SonicPiLog::MultiMessage mm)
{
    int msg_count = mm.messages.size();
    SonicPiTheme *theme = mm.theme;

    QTextCharFormat tf;
    QString ss;

    tf.setForeground(theme->color("LogDefaultForeground"));
    tf.setBackground(theme->color("LogBackground"));
    setCurrentCharFormat(tf);

    ss.append("{run: ").append(QString::number(mm.job_id));
    ss.append(", time: ").append(QString::fromStdString(mm.runtime));
    if(!mm.thread_name.empty()) {
      ss.append(", thread: \"").append(QString::fromStdString(mm.thread_name)).append("\"");
    }
    ss.append("}");
    appendPlainText(ss);

    for(int i = 0 ; i < msg_count ; i++) {
      ss = "";
      int msg_type = mm.messages[i].msg_type;
      std::string s = mm.messages[i].s;

      if (s.empty()) {
          ss.append(QString::fromUtf8(" │"));
        }
      else if(i == (msg_count - 1)) {
        ss.append(QString::fromUtf8(" └─ "));
      } else {
        ss.append(QString::fromUtf8(" ├─ "));
      }

      appendPlainText(ss);

      ss = "";
      switch(msg_type)
      {
      case 0:
        tf.setForeground(QColor("deeppink"));
        break;
      case 1:
        tf.setForeground(QColor("dodgerblue"));
        break;
      case 2:
        tf.setForeground(QColor("darkorange"));
        break;
      case 3:
        tf.setForeground(QColor("red"));
        break;
      case 4:
        tf.setForeground(QColor("white"));
        tf.setBackground(QColor("deeppink"));
        break;
      case 5:
        tf.setForeground(QColor("white"));
        tf.setBackground(QColor("dodgerblue"));
        break;
      case 6:
        tf.setForeground(QColor("white"));
        tf.setBackground(QColor("darkorange"));
        break;
      default:
        tf.setForeground(QColor("green"));
      }

      setCurrentCharFormat(tf);

      ss.append(QString::fromUtf8(s.c_str()));

      insertPlainText(ss);

      tf.setForeground(theme->color("LogDefaultForeground"));
      tf.setBackground(theme->color("LogBackground"));
      setCurrentCharFormat(tf);
    }
    appendPlainText(QString::fromStdString(" "));

    if(forceScroll) {
      QScrollBar *sb = verticalScrollBar();
      sb->setValue(sb->maximum());
    }
}
