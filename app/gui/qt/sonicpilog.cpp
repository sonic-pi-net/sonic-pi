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

// Standard stuff
#include <vector>
#include "sonicpitheme.h"
#include <QScrollBar>

SonicPiLog::SonicPiLog(QWidget *parent) : QPlainTextEdit(parent)
{
  forceScroll = true;
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

void SonicPiLog::setTextBgFgColors(QColor bg, QColor fg)
{
  QTextCharFormat tf;
  tf.setBackground(bg);
  tf.setForeground(fg);
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

void SonicPiLog::appendPlainText(QString text)
{
  QPlainTextEdit::appendPlainText(text);
  if(forceScroll) {
    QScrollBar *sb = verticalScrollBar();
    sb->setValue(sb->maximum());
  }
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
    if(! (mm.thread_name == "\"\"")) {
      ss.append(", thread: ").append(QString::fromStdString(mm.thread_name));
    }
    ss.append("}");
    appendPlainText(ss);

    for(int i = 0 ; i < msg_count ; i++) {
      ss = "";
      int msg_type = mm.messages[i].msg_type;
      std::string s = mm.messages[i].s;

      QStringList lines = QString::fromUtf8(s.c_str()).split(QRegExp("\\n"));

      if (s.empty()) {
          ss.append(QString::fromUtf8(" │"));
        }
      else if(i == (msg_count - 1)) {
        ss.append(QString::fromUtf8(" └─ "));
      } else {
        ss.append(QString::fromUtf8(" ├─ "));
      }

      appendPlainText(ss);

      for (int j = 0; j < lines.size(); ++j) {
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
        insertPlainText(lines.at(j));
        if ((j + 1) < lines.size()) {
          tf.setForeground(QColor("white"));
          setCurrentCharFormat(tf);
          if (i == (msg_count - 1)) {
            // we are the last message
            // so don't print joining lines
            insertPlainText("\n  ");
          } else {
            insertPlainText("\n │");
          }
        }
      }

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
