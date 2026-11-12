//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
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

#include <deque>
#include <variant>
#include <vector>

class SonicPiTheme;
class QContextMenuEvent;
class QWheelEvent;

// A log, read-only: each write goes in at the end on the frame pacer's tick,
// in colours of its own, never through the reader's cursor, so a click or a
// selection in the log is the reader's alone.
class SonicPiLog : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit SonicPiLog(QWidget *parent = 0);
    bool forceScroll;

    // The log's text is the code's point size less the points chosen in Preferences, never under kMinPointSize.
    static constexpr int kDefaultSmaller = 3;
    static constexpr int kMaxSmaller = 8;
    static constexpr int kMinPointSize = 6;
    void followCode(int codePointSize, int smaller);

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

    // A run of a line, coloured by theme roles (recorded for recolour()).
    struct Run
    {
        QString text;
        QString bgKey;
        QString fgKey;
        int bgAlpha = 255;
    };
    // One line in coloured runs, such as a cue: its address, then its arguments.
    void appendLine(SonicPiTheme* theme, std::vector<Run> runs);

    // Write at the reader's cursor: never for a log.
    void insertPlainText(const QString&) = delete;
    void setCurrentCharFormat(const QTextCharFormat&) = delete;

signals:

public slots:
    void recolour(SonicPiTheme* theme);
    void setFontFamily(QString font_name);
    // Each write waits for the frame pacer's next tick, with everything else
    // that arrived since the last one, so the log costs one repaint per tick
    // however busy the runs are. Order is kept across every entry point, and
    // of a burst only what the log keeps is written.
    void handleMultiMessage(SonicPiLog::MultiMessage mm);
    void forceScrollDown(bool force);
    // A line in the log's own colours.
    void appendPlainText(QString text);
    // Drops what is waiting to be written as well as what is shown.
    void clear();


protected:
    void contextMenuEvent(QContextMenuEvent* event) override;
    void wheelEvent(QWheelEvent* event) override;

private:
    int m_pointSize = 14;   // until the code's size reaches it (MainWindow::applyLogSize)
    struct Line
    {
        SonicPiTheme* theme;
        std::vector<Run> runs;
    };
    // What waits for the tick, in order: a run's message, or a line in runs.
    using Pending = std::variant<MultiMessage, Line>;
    void enqueue(Pending p);
    void writePending();
    void writeMessage(const MultiMessage& mm);
    void writeLine(const Line& line);
    std::deque<Pending> m_pending;
};

Q_DECLARE_METATYPE(SonicPiLog::MultiMessage)

#endif // SONICPILOG_H
