#ifndef SONICPILOG_H
#define SONICPILOG_H

#include <QTextEdit>

class SonicPiTheme;

class SonicPiLog : public QTextEdit
{
    Q_OBJECT
public:
    explicit SonicPiLog(QWidget *parent = 0);

    struct Message
    {
        int msg_type;
        std::string s;
    };
    typedef std::vector<Message> Messages;

    struct MultiMessage
    {
        SonicPiTheme *theme;
        int job_id;
        std::string thread_name;
        std::string runtime;
        Messages messages;
    };

signals:

public slots:
    void handleMultiMessage(SonicPiLog::MultiMessage mm);

protected:
};

Q_DECLARE_METATYPE(SonicPiLog::MultiMessage)

#endif // SONICPILOG_H
