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
#include <QElapsedTimer>
#include <QFont>
#include <QImage>
#include <QMutex>
#include <QString>
#include <QVector>
#include <QWaitCondition>

#include <algorithm>
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
// content sits at the bottom; runs with a role follow the theme. And the
// text selects as a text edit's does (a drag, shift-click, a double-click
// for a word and a third click for the line, Select All), the view holding
// still while the button is down; Copy takes the selection, or every line
// when nothing is selected.
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

    // The text selected, by mouse or Select All, as text: lines joined by '\n'.
    bool hasSelection() const;
    QString selectedText() const;
    void selectAll();
    void clearSelection();
    void setSelectionColours(const QColor& background, const QColor& text);

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
    bool event(QEvent*) override;
    void mousePressEvent(QMouseEvent*) override;
    void mouseMoveEvent(QMouseEvent*) override;
    void mouseReleaseEvent(QMouseEvent*) override;
    void mouseDoubleClickEvent(QMouseEvent*) override;

private:
    // A place in the text: a line, by its sequence number, and the gap before
    // one of its characters. Held by sequence number, a selection stays on
    // its text as lines arrive and the oldest go.
    struct Pos
    {
        quint64 seq = 0;
        int     at = 0;
        bool operator<(const Pos& o) const { return seq < o.seq || (seq == o.seq && at < o.at); }
        bool operator==(const Pos& o) const { return seq == o.seq && at == o.at; }
    };
    // What a press takes, and a drag from it keeps whole.
    enum class Unit { Chars, Words, Lines };

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
        Pos    selFrom;          // the selection; none when the two are equal
        Pos    selTo;
        QColor selBackground;
        QColor selText;
        bool   valid = false;
    };
    void updateScrollRange();
    void requestFrame();
    void renderLoop();
    class Layouts;   // the worker's shaped lines
    void renderFrame(QImage& image, const Frame& frame, Layouts& layouts);
    void copyAll();
    void copy();
    qreal lineHeight() const;
    int topGap() const;
    Pos posAt(const QPointF& p) const;
    void unitAt(const Pos& p, Unit unit, Pos& from, Pos& to) const;
    void selectTo(const QPointF& p);
    Pos selFrom() const { return std::min(m_anchor, m_caret); }
    Pos selTo() const { return std::max(m_anchor, m_caret); }
    void resetSelection();

    QVector<QVector<LogRun>> m_lines;
    quint64 m_firstSeq = 0;    // sequence number of m_lines[0]; lines are numbered from 0 for life
    quint64 m_look = 0;
    qreal  m_widest = 0.0;     // logical px, for the horizontal range
    bool   m_pinned = true;
    bool   m_adjusting = false;
    QColor m_background;
    QColor m_text;
    QColor m_selBackground;
    QColor m_selText;

    Pos  m_anchor;            // where the selection began
    Pos  m_caret;             // where it ends
    Pos  m_unitFrom;          // the word or line the press took
    Pos  m_unitTo;
    Unit m_unit = Unit::Chars;
    bool m_selecting = false; // the button is down: the view holds still
    QElapsedTimer m_sinceDoubleClick;   // a press soon after takes the line

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
