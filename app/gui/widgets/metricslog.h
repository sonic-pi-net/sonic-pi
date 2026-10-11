//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <QAbstractScrollArea>
#include <QColor>
#include <QFont>
#include <QImage>
#include <QMutex>
#include <QString>
#include <QVector>
#include <QWaitCondition>

#include <atomic>
#include <functional>
#include <thread>

// One coloured piece of a log line. `role` is the theme key the colour came
// from, so the line can be re-tinted when the theme changes; empty for a
// colour that is nobody's business but the line's.
struct LogRun
{
    QColor color;
    QString text;
    QString role;
};

namespace SonicPi
{

// A log card in the Debug pane: the OSC traffic each way, and the engine's
// own lines. Thousands of coloured runs a second arrive while loops play.
// Held as lines of runs in a ring, and drawn the way the node tree graph is:
// a worker thread renders the lines on view into an image, and the GUI
// thread blits it. The GUI thread lays out no text at all, so a flood costs
// the worker, not the interface. It was a rich-text QTextEdit, re-laying its
// document out on every refresh and again on every paint: a fifth of the
// GUI's main thread with two live loops running.
//
// Kept from the text edit: the newest kKeptLines lines; a batch over
// kMaxLinesPerAppend keeps its newest and says how many went; the view stays
// at the bottom while it is there and stays put once scrolled up; short
// content sits at the bottom; runs with a role follow the theme. Gone: mouse
// selection. The lines copy whole, from the menu or Ctrl+C.
class MetricsLog : public QAbstractScrollArea
{
    Q_OBJECT
public:
    static constexpr int kKeptLines = 2000;
    static constexpr int kMaxLinesPerAppend = 256;

    explicit MetricsLog(QWidget* parent = nullptr);
    ~MetricsLog() override;
    MetricsLog(const MetricsLog&) = delete;
    MetricsLog& operator=(const MetricsLog&) = delete;

    void append(QVector<QVector<LogRun>> lines);
    // Replaces everything with the text, one line per '\n', in the text colour.
    void setPlainText(const QString& text);
    void clear();
    // Every run with a role takes the colour the theme now gives that role.
    void recolour(const std::function<QColor(const QString& role)>& colourForRole);
    void setColours(const QColor& background, const QColor& text);

    int lineCount() const { return m_lines.size(); }
    const QVector<LogRun>& line(int i) const { return m_lines.at(i); }
    QString plainText() const;
    bool atBottom() const { return m_pinned; }
    // Frames the worker has delivered, and lines it has shaped: a test waits
    // on the one and checks the other.
    int framesRendered() const { return m_framesRendered.load(std::memory_order_relaxed); }
    int linesShaped() const { return m_linesShaped.load(std::memory_order_relaxed); }

protected:
    void paintEvent(QPaintEvent*) override;
    void resizeEvent(QResizeEvent*) override;
    void scrollContentsBy(int dx, int dy) override;
    void changeEvent(QEvent*) override;
    void contextMenuEvent(QContextMenuEvent*) override;
    void keyPressEvent(QKeyEvent*) override;

private:
    // What the worker draws: the lines on view and how to draw them. Each
    // line carries its sequence number, which the worker keys its layouts by:
    // a line is shaped once and drawn every frame it is on view.
    struct Frame
    {
        QVector<QVector<LogRun>> lines;
        quint64 firstSeq = 0;    // sequence number of lines[0]
        quint64 look = 0;        // bumped when a font or colour change makes old layouts wrong
        int    xOffset = 0;      // horizontal scroll, logical px
        int    topGap = 0;       // short content sits at the bottom
        QSize  size;
        qreal  dpr = 1.0;
        QFont  font;
        QColor background;
        QColor text;
        bool   valid = false;
    };
    void updateScrollRange();
    void requestFrame();
    void renderLoop();
    class Layouts;   // the worker's shaped lines
    void renderFrame(QImage& image, const Frame& frame, Layouts& layouts);
    void copyAll();
    qreal lineHeight() const;

    QVector<QVector<LogRun>> m_lines;
    quint64 m_firstSeq = 0;    // sequence number of m_lines[0]; lines are numbered from 0 for life
    quint64 m_look = 0;
    qreal  m_widest = 0.0;     // logical px, for the horizontal range
    bool   m_pinned = true;
    bool   m_adjusting = false;
    QColor m_background;
    QColor m_text;

    QMutex         m_frameMutex;
    QWaitCondition m_frameReady;
    Frame          m_pending;
    bool           m_quit = false;
    QMutex         m_frontMutex;
    QImage         m_front;
    std::atomic<int> m_framesRendered{ 0 };
    std::atomic<int> m_linesShaped{ 0 };
    std::thread    m_renderThread;
};

} // namespace SonicPi
