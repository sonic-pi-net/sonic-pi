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

#include "metricslog.h"

#include "utils/framepacer.h"
#include "utils/opaque.h"

#include <QApplication>
#include <QClipboard>
#include <QContextMenuEvent>
#include <QFontMetricsF>
#include <QKeyEvent>
#include <QKeySequence>
#include <QMenu>
#include <QPainter>
#include <QTextLayout>
#include <QPaintEvent>
#include <QScrollBar>

#include <algorithm>
#include <map>
#include <memory>

namespace SonicPi
{

namespace
{
constexpr int kLeftPad = 2;   // logical px before the first glyph
} // namespace

MetricsLog::MetricsLog(QWidget* parent)
    : QAbstractScrollArea(parent)
{
    setFrameShape(QFrame::NoFrame);
    setFocusPolicy(Qt::ClickFocus);
    viewport()->setAttribute(Qt::WA_OpaquePaintEvent, true);
    m_background = palette().base().color();
    m_text = palette().text().color();
    horizontalScrollBar()->setSingleStep(16);
    // Pinned to the bottom while the user leaves it there; a scroll up by the
    // user unpins, a scroll back to the end pins again. Our own moves are
    // guarded so they never change that.
    connect(verticalScrollBar(), &QScrollBar::valueChanged, this, [this](int v) {
        if (m_adjusting)
            return;
        m_pinned = v >= verticalScrollBar()->maximum();
    });
    m_renderThread = std::thread([this] { renderLoop(); });
}

MetricsLog::~MetricsLog()
{
    {
        QMutexLocker lock(&m_frameMutex);
        m_quit = true;
        m_frameReady.wakeAll();
    }
    m_renderThread.join();
}

qreal MetricsLog::lineHeight() const
{
    return QFontMetricsF(font()).height();
}

void MetricsLog::append(QVector<QVector<LogRun>> lines)
{
    if (lines.isEmpty())
        return;
    if (lines.size() > kMaxLinesPerAppend)
    {
        const int dropped = lines.size() - kMaxLinesPerAppend;
        lines.remove(0, dropped);   // keep the newest
        lines.prepend({ { QColor(128, 128, 128),
                          QStringLiteral("… %1 lines suppressed (flood)").arg(dropped) } });
    }
    // A '\n' inside a run is a line break: the engine's multi-line debug
    // text arrives as one entry.
    for (int i = 0; i < lines.size(); ++i)
    {
        bool broken = false;
        for (const LogRun& r : lines.at(i))
            if (r.text.contains(QLatin1Char('\n'))) { broken = true; break; }
        if (!broken)
            continue;
        QVector<QVector<LogRun>> split;
        QVector<LogRun> current;
        for (const LogRun& r : lines.at(i))
        {
            const QStringList parts = r.text.split(QLatin1Char('\n'));
            for (int k = 0; k < parts.size(); ++k)
            {
                if (k > 0) { split.append(current); current.clear(); }
                current.append({ r.color, parts.at(k), r.role });
            }
        }
        split.append(current);
        lines.remove(i);
        for (int k = 0; k < split.size(); ++k)
            lines.insert(i + k, split.at(k));
        i += split.size() - 1;
    }
    const QFontMetricsF fm(font());
    for (const QVector<LogRun>& runs : lines)
    {
        qreal w = 0.0;
        for (const LogRun& r : runs)
            w += fm.horizontalAdvance(r.text);
        m_widest = std::max(m_widest, w);
    }
    m_lines += lines;
    if (m_lines.size() > kKeptLines)
    {
        const int gone = m_lines.size() - kKeptLines;
        m_lines.remove(0, gone);   // the widest may be gone: the range shrinks on the next clear
        m_firstSeq += gone;
    }
    updateScrollRange();
    requestFrame();
}

void MetricsLog::setPlainText(const QString& text)
{
    m_firstSeq += m_lines.size();
    m_lines.clear();
    m_widest = 0.0;
    QVector<QVector<LogRun>> lines;
    for (const QString& l : text.split(QLatin1Char('\n')))
        lines.append({ { QColor(), l, QString() } });
    append(std::move(lines));
}

void MetricsLog::clear()
{
    m_firstSeq += m_lines.size();
    m_lines.clear();
    m_widest = 0.0;
    m_pinned = true;
    updateScrollRange();
    requestFrame();
}

void MetricsLog::recolour(const std::function<QColor(const QString& role)>& colourForRole)
{
    for (QVector<LogRun>& runs : m_lines)
        for (LogRun& r : runs)
            if (!r.role.isEmpty())
                r.color = colourForRole(r.role);
    ++m_look;
    requestFrame();
}

void MetricsLog::setColours(const QColor& background, const QColor& text)
{
    m_background = background;
    m_text = text;
    ++m_look;
    requestFrame();
}

QString MetricsLog::plainText() const
{
    QStringList out;
    out.reserve(m_lines.size());
    for (const QVector<LogRun>& runs : m_lines)
    {
        QString l;
        for (const LogRun& r : runs)
            l += r.text;
        out << l;
    }
    return out.join(QLatin1Char('\n'));
}

void MetricsLog::updateScrollRange()
{
    const int visible = std::max(1, int(viewport()->height() / lineHeight()));
    const int maxTop = std::max(0, int(m_lines.size()) - visible);
    m_adjusting = true;
    verticalScrollBar()->setRange(0, maxTop);
    verticalScrollBar()->setPageStep(visible);
    if (m_pinned)
        verticalScrollBar()->setValue(maxTop);
    const int maxX = std::max(0, int(m_widest) + kLeftPad - viewport()->width());
    horizontalScrollBar()->setRange(0, maxX);
    horizontalScrollBar()->setPageStep(viewport()->width());
    m_adjusting = false;
}

void MetricsLog::requestFrame()
{
    const qreal lineH = lineHeight();
    const int visible = std::max(1, int(viewport()->height() / lineH)) + 1;   // a part line at the bottom
    const int top = verticalScrollBar()->value();
    Frame f;
    f.lines = m_lines.mid(top, visible);
    f.firstSeq = m_firstSeq + top;
    f.look = m_look;
    f.xOffset = horizontalScrollBar()->value();
    const int shown = std::min(int(m_lines.size()), visible);
    f.topGap = m_lines.size() < visible - 1 ? std::max(0, int(viewport()->height() - shown * lineH)) : 0;
    f.size = viewport()->size();
    f.dpr = devicePixelRatioF();
    f.font = font();
    f.background = m_background;
    f.text = m_text;
    f.valid = true;
    QMutexLocker lock(&m_frameMutex);
    m_pending = std::move(f);   // a frame not yet drawn is replaced: only the newest is worth drawing
    m_frameReady.wakeOne();
}

// The worker's shaped lines, by sequence number. A line is laid out once,
// with its runs' colours as format ranges, and drawn from the layout every
// frame it is on view: shaping is the expensive part of drawing text, and
// doing it per run per frame cost more off the GUI thread than the text edit
// had on it. Lines that have scrolled off the view are dropped; a change of
// look (font or colours) starts afresh.
class MetricsLog::Layouts
{
public:
    QTextLayout& of(quint64 seq, const QVector<LogRun>& runs, const Frame& frame, std::atomic<int>& shaped)
    {
        if (frame.look != m_look)
        {
            m_lines.clear();
            m_look = frame.look;
        }
        auto it = m_lines.find(seq);
        if (it == m_lines.end())
        {
            auto layout = std::make_unique<QTextLayout>();
            layout->setFont(frame.font);
            layout->setCacheEnabled(true);
            QString text;
            QList<QTextLayout::FormatRange> formats;
            for (const LogRun& r : runs)
            {
                QTextLayout::FormatRange f;
                f.start = text.size();
                f.length = r.text.size();
                f.format.setForeground(r.color.isValid() ? r.color : frame.text);
                formats.append(f);
                text += r.text;
            }
            layout->setText(text);
            layout->setFormats(formats);
            layout->beginLayout();
            QTextLine line = layout->createLine();
            line.setPosition(QPointF(0, 0));
            layout->endLayout();
            it = m_lines.emplace(seq, std::move(layout)).first;
            shaped.fetch_add(1, std::memory_order_relaxed);
        }
        return *it->second;
    }
    // Keep the lines on view and a screen's worth either side; drop the rest.
    void keep(quint64 from, quint64 to)
    {
        for (auto it = m_lines.begin(); it != m_lines.end();)
        {
            if (it->first < from || it->first > to)
                it = m_lines.erase(it);
            else
                ++it;
        }
    }

private:
    std::map<quint64, std::unique_ptr<QTextLayout>> m_lines;
    quint64 m_look = 0;
};

void MetricsLog::renderLoop()
{
    QImage back;
    Layouts layouts;
    for (;;)
    {
        Frame frame;
        {
            QMutexLocker lock(&m_frameMutex);
            while (!m_quit && !m_pending.valid)
                m_frameReady.wait(&m_frameMutex);
            if (m_quit)
                return;
            frame = std::move(m_pending);
            m_pending.valid = false;
        }
        const QSize px = frame.size * frame.dpr;
        if (px.isEmpty())
            continue;
        if (back.size() != px)
            back = QImage(px, QImage::Format_ARGB32_Premultiplied);
        back.setDevicePixelRatio(frame.dpr);
        renderFrame(back, frame, layouts);
        {
            QMutexLocker lock(&m_frontMutex);
            std::swap(m_front, back);
        }
        m_framesRendered.fetch_add(1, std::memory_order_relaxed);
        // The blit goes with the frame pacer's tick, in the same pass as the
        // scopes' repaints. Queued: safe from here, and dropped if the widget
        // has gone.
        QMetaObject::invokeMethod(this, [this] {
            FramePacer::instance()->defer(this, [this] { viewport()->update(); });
        }, Qt::QueuedConnection);
    }
}

void MetricsLog::renderFrame(QImage& image, const Frame& frame, Layouts& layouts)
{
    QPainter p(&image);
    // Opaque whatever the theme says, so the frame can be copied, not blended.
    QColor background = frame.background;
    background.setAlpha(255);
    p.fillRect(QRectF(QPointF(0, 0), QSizeF(frame.size)), background);
    const qreal lineH = QFontMetricsF(frame.font).height();
    const qreal x = kLeftPad - frame.xOffset;
    qreal y = frame.topGap;
    quint64 seq = frame.firstSeq;
    for (const QVector<LogRun>& runs : frame.lines)
    {
        layouts.of(seq, runs, frame, m_linesShaped).draw(&p, QPointF(x, y));
        ++seq;
        y += lineH;
        if (y > frame.size.height())
            break;
    }
    const quint64 margin = quint64(frame.lines.size()) + 1;
    layouts.keep(frame.firstSeq > margin ? frame.firstSeq - margin : 0, seq + margin);
}

void MetricsLog::paintEvent(QPaintEvent*)
{
    QPainter p(viewport());
    QMutexLocker lock(&m_frontMutex);
    if (m_front.isNull() || m_front.deviceIndependentSize() != viewport()->size())
    {
        // Nothing drawn yet, or a frame of another size on its way.
        p.fillRect(viewport()->rect(), QColor(m_background.red(), m_background.green(), m_background.blue()));
        return;
    }
    drawOpaqueFrame(p, QPointF(0, 0), m_front);   // opaque throughout (renderFrame)
}

void MetricsLog::resizeEvent(QResizeEvent* e)
{
    QAbstractScrollArea::resizeEvent(e);
    updateScrollRange();
    requestFrame();
}

void MetricsLog::scrollContentsBy(int, int)
{
    requestFrame();
}

void MetricsLog::changeEvent(QEvent* e)
{
    QAbstractScrollArea::changeEvent(e);
    if (e->type() == QEvent::FontChange)
    {
        const QFontMetricsF fm(font());
        m_widest = 0.0;
        for (const QVector<LogRun>& runs : m_lines)
        {
            qreal w = 0.0;
            for (const LogRun& r : runs)
                w += fm.horizontalAdvance(r.text);
            m_widest = std::max(m_widest, w);
        }
        updateScrollRange();
        requestFrame();
    }
}

void MetricsLog::copyAll()
{
    QApplication::clipboard()->setText(plainText());
}

void MetricsLog::contextMenuEvent(QContextMenuEvent* e)
{
    QMenu menu(this);
    QAction* copy = menu.addAction(tr("Copy all"));
    copy->setEnabled(!m_lines.isEmpty());
    if (menu.exec(e->globalPos()) == copy)
        copyAll();
}

void MetricsLog::keyPressEvent(QKeyEvent* e)
{
    if (e->matches(QKeySequence::Copy))
    {
        copyAll();
        return;
    }
    QAbstractScrollArea::keyPressEvent(e);
}

} // namespace SonicPi
