#include "sonicpilog.h"

// Standard stuff
#include <vector>
#include "sonicpitheme.h"

SonicPiLog::SonicPiLog(QWidget *parent) : QPlainTextEdit(parent)
{
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

    ss.append("[Run ").append(QString::number(mm.job_id));
    ss.append(", Time ").append(QString::fromStdString(mm.runtime));
    if(!mm.thread_name.empty()) {
      ss.append(", Thread :").append(QString::fromStdString(mm.thread_name));
    }
    ss.append("]");
    appendPlainText(ss);

    for(int i = 0 ; i < msg_count ; i++) {
      ss = "";
      int msg_type = mm.messages[i].msg_type;
      std::string s = mm.messages[i].s;

      if(i == (msg_count - 1)) {
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
}
