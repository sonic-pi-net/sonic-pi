#include "sonicpilog.h"

// Standard stuff
#include <vector>
#include "sonicpitheme.h"

SonicPiLog::SonicPiLog(QWidget *parent) : QTextEdit(parent)
{
}

void SonicPiLog::handleMultiMessage(SonicPiLog::MultiMessage mm)
{
    int msg_count = mm.messages.size();
    SonicPiTheme *theme = mm.theme;

    QString ss;

    setTextColor(theme->color("LogDefaultForeground"));
    ss.append("[Run ").append(QString::number(mm.job_id));
    ss.append(", Time ").append(QString::fromStdString(mm.runtime));
    if(!mm.thread_name.empty()) {
      ss.append(", Thread :").append(QString::fromStdString(mm.thread_name));
    }
    ss.append("]");
    append(ss);

    for(int i = 0 ; i < msg_count ; i++) {
      ss = "";
      int msg_type = mm.messages[i].msg_type;
      std::string s = mm.messages[i].s;

      if(i == (msg_count - 1)) {
        ss.append(QString::fromUtf8(" └─ "));
      } else {
        ss.append(QString::fromUtf8(" ├─ "));
      }

      append(ss);

      ss = "";
      switch(msg_type)
      {
      case 0:
        setTextColor(QColor("deeppink"));
        break;
      case 1:
        setTextColor(QColor("dodgerblue"));
        break;
      case 2:
        setTextColor(QColor("darkorange"));
        break;
      case 3:
        setTextColor(QColor("red"));
        break;
      case 4:
        setTextColor(QColor("white"));
        setTextBackgroundColor(QColor("deeppink"));
        break;
      case 5:
        setTextColor(QColor("white"));
        setTextBackgroundColor(QColor("dodgerblue"));
        break;
      case 6:
        setTextColor(QColor("white"));
        setTextBackgroundColor(QColor("darkorange"));
        break;
      default:
        setTextColor(QColor("green"));
      }

      ss.append(QString::fromUtf8(s.c_str()));

      insertPlainText(ss);

      setTextColor(theme->color("LogDefaultForeground"));
      setTextBackgroundColor(theme->color("LogBackground"));
    }
    append(QString::fromStdString(" "));
}
