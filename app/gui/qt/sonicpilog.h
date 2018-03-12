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

#ifndef SONICPILOG_H
#define SONICPILOG_H

#include <QPlainTextEdit>

class SonicPiTheme;

class SonicPiLog : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit SonicPiLog(QWidget *parent = 0);
    bool forceScroll;

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
    void setTextColor(QColor c);
    void setTextBackgroundColor(QColor c);
    void setTextBgFgColors(QColor fg, QColor bg);
    void setFontFamily(QString font_name);
    void handleMultiMessage(SonicPiLog::MultiMessage mm);
    void forceScrollDown(bool force);
    void appendPlainText(QString text);

protected:
};

Q_DECLARE_METATYPE(SonicPiLog::MultiMessage)

#endif // SONICPILOG_H
