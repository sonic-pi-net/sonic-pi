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

#include "sonicpilog.h"
#include "utils/framepacer.h"

// Standard stuff
#include "model/sonicpitheme.h"
#include <QScrollBar>
#include <algorithm>
#include <QWheelEvent>
#include <QMenu>
#include <QContextMenuEvent>
#include <QTextBlock>
#include <QTextFragment>
#include <vector>

namespace {
// Custom QTextCharFormat properties recording the theme roles a fragment was
// coloured from, so the log can be re-resolved after a theme change instead of
// keeping the baked (and now stale) colours.
constexpr int FgKeyProp   = QTextFormat::UserProperty + 1;
constexpr int BgKeyProp   = QTextFormat::UserProperty + 2;
constexpr int BgAlphaProp = QTextFormat::UserProperty + 3;

// The foreground sentinel "@contrast" means "black or white, whichever reads on
// the background" — for text on a coloured fill (e.g. cue path/data chips).
static const char* const kContrastFg = "@contrast";

// Sets fg/bg on a char format from theme keys AND records those keys (+ bg
// alpha) so recolour() can re-derive them. An empty key leaves that channel.
// bgKey is resolved first so the fg can auto-contrast against it.
void applyRole(QTextCharFormat& tf, SonicPiTheme* theme,
               const QString& fgKey, const QString& bgKey, int bgAlpha = 255) {
    if (!bgKey.isEmpty()) {
        QColor bg = theme->color(bgKey);
        if (bgAlpha != 255) bg.setAlpha(bgAlpha);
        tf.setBackground(bg);
        tf.setProperty(BgKeyProp, bgKey);
        tf.setProperty(BgAlphaProp, bgAlpha);
    }
    if (!fgKey.isEmpty()) {
        const QColor fg = (fgKey == kContrastFg && !bgKey.isEmpty())
                              ? theme->contrastingText(theme->color(bgKey))
                              : theme->color(fgKey);
        tf.setForeground(fg);
        tf.setProperty(FgKeyProp, fgKey);
    }
}
} // namespace

SonicPiLog::SonicPiLog(QWidget* parent)
    : QPlainTextEdit(parent)
{
    forceScroll = true;
}

void SonicPiLog::followCode(int codePointSize, int smaller)
{
    m_pointSize = std::max(kMinPointSize, codePointSize - smaller);
    QFont f = font();
    f.setPointSize(m_pointSize);
    setFont(f);
}

void SonicPiLog::wheelEvent(QWheelEvent* event)
{
    // A read-only QPlainTextEdit zooms itself on a Control-scroll; the log's size is the code's (followCode).
    if (event->modifiers() & Qt::ControlModifier)
    {
        event->ignore();
        return;
    }
    QPlainTextEdit::wheelEvent(event);
}

void SonicPiLog::contextMenuEvent(QContextMenuEvent* event)
{
    // Standard menu (copy/select-all) plus Clear — the idiomatic action for a
    // log/output view.
    QMenu* menu = createStandardContextMenu();
    if (!menu) menu = new QMenu(this);
    menu->addSeparator();
    menu->addAction(tr("Clear"), this, [this] { clear(); });
    menu->exec(event->globalPos());
    delete menu;
}

void SonicPiLog::forceScrollDown(bool force)
{
    forceScroll = force;
}

// Re-resolve every fragment's recorded theme roles against the current theme
// (scheme + hue rotation + monochrome), so log history already on screen tracks
// the theme instead of keeping the colours it was written with.
void SonicPiLog::recolour(SonicPiTheme* theme)
{
    QTextDocument* doc = document();
    QTextCursor edit(doc);
    edit.beginEditBlock();
    for (QTextBlock block = doc->begin(); block.isValid(); block = block.next()) {
        for (QTextBlock::iterator it = block.begin(); !it.atEnd(); ++it) {
            const QTextFragment frag = it.fragment();
            if (!frag.isValid()) continue;
            QTextCharFormat f = frag.charFormat();
            bool changed = false;
            if (f.hasProperty(BgKeyProp)) {
                QColor bg = theme->color(f.property(BgKeyProp).toString());
                const int a = f.hasProperty(BgAlphaProp) ? f.property(BgAlphaProp).toInt() : 255;
                if (a != 255) bg.setAlpha(a);
                f.setBackground(bg);
                changed = true;
            }
            if (f.hasProperty(FgKeyProp)) {
                const QString fk = f.property(FgKeyProp).toString();
                if (fk == kContrastFg && f.hasProperty(BgKeyProp))
                    f.setForeground(theme->contrastingText(theme->color(f.property(BgKeyProp).toString())));
                else
                    f.setForeground(theme->color(fk));
                changed = true;
            }
            if (changed) {
                QTextCursor fc(doc);
                fc.setPosition(frag.position());
                fc.setPosition(frag.position() + frag.length(), QTextCursor::KeepAnchor);
                fc.setCharFormat(f);
            }
        }
    }
    edit.endEditBlock();
}

void SonicPiLog::setFontFamily(QString font_name)
{
    setFont(QFont(font_name, m_pointSize, -1, false));
}

void SonicPiLog::appendPlainText(QString text)
{
    enqueue(Line{ nullptr, { { std::move(text) } } });
}

void SonicPiLog::appendLine(SonicPiTheme* theme, std::vector<Run> runs)
{
    enqueue(Line{ theme, std::move(runs) });
}

void SonicPiLog::handleMultiMessage(SonicPiLog::MultiMessage mm)
{
    enqueue(std::move(mm));
}

void SonicPiLog::clear()
{
    m_pending.clear();
    QPlainTextEdit::clear();
}

void SonicPiLog::enqueue(Pending p)
{
    const bool first = m_pending.empty();
    m_pending.push_back(std::move(p));
    // Each write is a line at least, so past the log's cap the oldest waiting
    // would be trimmed as soon as it was written.
    const int cap = document()->maximumBlockCount();
    if (cap > 0 && m_pending.size() > size_t(cap))
        m_pending.pop_front();
    if (first)
        SonicPi::FramePacer::instance()->defer(this, [this] { writePending(); });
}

void SonicPiLog::writePending()
{
    std::deque<Pending> due;
    due.swap(m_pending);
    for (const Pending& p : due)
    {
        QTextCursor edit(document());
        edit.beginEditBlock();   // one layout per write, however many lines
        if (const MultiMessage* mm = std::get_if<MultiMessage>(&p))
            writeMessage(*mm);
        else
            writeLine(std::get<Line>(p));
        edit.endEditBlock();
    }
    if (forceScroll)
    {
        QScrollBar* sb = verticalScrollBar();
        sb->setValue(sb->maximum());
    }
}

// A new line at the end, started as QPlainTextEdit appends one (kept to the
// cap, the view held), its runs written through a cursor of its own.
void SonicPiLog::writeLine(const Line& line)
{
    QPlainTextEdit::appendPlainText(QString());
    QTextCursor end(document());
    end.movePosition(QTextCursor::End);
    for (const Run& r : line.runs)
    {
        QTextCharFormat f;
        applyRole(f, line.theme, r.fgKey, r.bgKey, r.bgAlpha);
        end.insertText(r.text, f);
    }
}

// A run's messages under a header line: each message's first line led by a
// branch (" ├─ ", the last " └─ ", an empty one " │"), the lines after it by
// the branch's continuation on the message's own background, then a line of
// one space to close the block.
void SonicPiLog::writeMessage(const MultiMessage& mm)
{
    QString head = "{run: " + QString::number(mm.job_id) + ", time: " + QString::fromStdString(mm.runtime);
    if (!(mm.thread_name == "\"\""))
        head += ", thread: " + QString::fromStdString(mm.thread_name);
    head += "}";
    writeLine({ mm.theme, { { head, "LogBackground", "LogForeground" } } });

    for (size_t i = 0; i < mm.messages.size(); i++)
    {
        const Message& m = mm.messages[i];
        const bool last = i + 1 == mm.messages.size();
        // msg_type 1-6 map to the numbered log-stream roles; 0/other use the default log role.
        const QString sfx = (m.msg_type >= 1 && m.msg_type <= 6) ? QString("_%1").arg(m.msg_type) : QString();
        const QStringList parts = QString::fromUtf8(m.s.c_str()).split('\n');
        for (qsizetype j = 0; j < parts.size(); j++)
        {
            const Run lead = j > 0      ? Run{ last ? "  " : " │", "LogBackground" + sfx, "LogForeground" }
                             : m.s.empty() ? Run{ " │", "LogBackground", "LogForeground" }
                             : Run{ last ? " └─ " : " ├─ ", "LogBackground", "LogForeground" };
            writeLine({ mm.theme, { lead, { parts[j], "LogBackground" + sfx, "LogForeground" + sfx } } });
        }
    }
    writeLine({ mm.theme, { { " ", "LogBackground", "LogForeground" } } });
}
