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

// Standard stuff
#include "model/sonicpitheme.h"
#include <QScrollBar>
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
    zoomLevel = 0;
}

void SonicPiLog::zoomIn()
{
    setZoomLevel(zoomLevel + 1);
}

void SonicPiLog::zoomOut()
{
    setZoomLevel(zoomLevel - 1);
}

void SonicPiLog::setZoomLevel(int zoom)
{
    const int MIN_ZOOM = -10;
    const int MAX_ZOOM = 10;

    // Clamp the desired zoom to your range
    int targetZoom = std::clamp(zoom, MIN_ZOOM, MAX_ZOOM);

    int delta = targetZoom - zoomLevel;

    if (delta > 0)
    {
        QPlainTextEdit::zoomIn(delta);
    }
    else if (delta < 0)
    {

        QPlainTextEdit::zoomOut(-delta);
    }
    zoomLevel = targetZoom;
}

int SonicPiLog::currentZoomLevel() const
{
    return zoomLevel;
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

void SonicPiLog::setTextColorKey(SonicPiTheme* theme, const QString& fgKey)
{
    QTextCharFormat tf;
    applyRole(tf, theme, fgKey, QString());
    setCurrentCharFormat(tf);
}

void SonicPiLog::setTextBgFgColorKeys(SonicPiTheme* theme, const QString& bgKey,
                                      const QString& fgKey, int bgAlpha)
{
    QTextCharFormat tf;
    applyRole(tf, theme, fgKey, bgKey, bgAlpha);
    setCurrentCharFormat(tf);
}

void SonicPiLog::setTextBackgroundColorKey(SonicPiTheme* theme, const QString& bgKey, int bgAlpha)
{
    QTextCharFormat tf;
    applyRole(tf, theme, QString(), bgKey, bgAlpha);
    setCurrentCharFormat(tf);
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
#ifdef __APPLE__
    setFont(QFont(font_name, 14, -1, false));
#elif __linux__
    setFont(QFont(font_name, 12, -1, false));
#else
    setFont(QFont(font_name, 8, -1, false));
#endif
}

void SonicPiLog::appendPlainText(QString text)
{
    QPlainTextEdit::appendPlainText(text);
    if (forceScroll)
    {
        QScrollBar* sb = verticalScrollBar();
        sb->setValue(sb->maximum());
    }
}

void SonicPiLog::handleMultiMessage(SonicPiLog::MultiMessage mm)
{
    int msg_count = int(mm.messages.size());
    SonicPiTheme* theme = mm.theme;

    QTextCharFormat tf;
    QString ss;

    // Coalesce every insert below (one message can be many lines) into a single
    // document edit block, so the layout/relayout runs once instead of per line.
    QTextCursor editBlock = textCursor();
    editBlock.beginEditBlock();

    applyRole(tf, theme, "LogForeground", "LogBackground");
    setCurrentCharFormat(tf);

    ss.append("{run: ").append(QString::number(mm.job_id));
    ss.append(", time: ").append(QString::fromStdString(mm.runtime));
    if (!(mm.thread_name == "\"\""))
    {
        ss.append(", thread: ").append(QString::fromStdString(mm.thread_name));
    }
    ss.append("}");
    appendPlainText(ss);

    for (int i = 0; i < msg_count; i++)
    {
        ss = "";
        int msg_type = mm.messages[i].msg_type;
        std::string s = mm.messages[i].s;

        QStringList lines = QString::fromUtf8(s.c_str()).split(QRegularExpression("\\n"));

        if (s.empty())
        {
            ss.append(QString::fromUtf8(" │"));
        }
        else if (i == (msg_count - 1))
        {
            ss.append(QString::fromUtf8(" └─ "));
        }
        else
        {
            ss.append(QString::fromUtf8(" ├─ "));
        }

        appendPlainText(ss);

        for (int j = 0; j < lines.size(); ++j)
        {
            // msg_type 1-6 map to the numbered log-stream roles; 0/other use the
            // default log role. The role is recorded on the format for recolour().
            const QString sfx = (msg_type >= 1 && msg_type <= 6) ? QString("_%1").arg(msg_type) : QString();
            applyRole(tf, theme, "LogForeground" + sfx, "LogBackground" + sfx);
            setCurrentCharFormat(tf);
            insertPlainText(lines.at(j));
            if ((j + 1) < lines.size())
            {
                applyRole(tf, theme, "LogForeground", QString());
                setCurrentCharFormat(tf);
                if (i == (msg_count - 1))
                {
                    // we are the last message
                    // so don't print joining lines
                    insertPlainText("\n  ");
                }
                else
                {
                    insertPlainText("\n │");
                }
            }
        }

        applyRole(tf, theme, "LogForeground", "LogBackground");
        setCurrentCharFormat(tf);
    }
    appendPlainText(QString::fromStdString(" "));

    editBlock.endEditBlock();

    if (forceScroll)
    {
        QScrollBar* sb = verticalScrollBar();
        sb->setValue(sb->maximum());
    }
}
