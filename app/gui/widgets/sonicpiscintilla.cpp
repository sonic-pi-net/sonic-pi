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

#include "sonicpiscintilla.h"
#include "completionpopup.h"
#include "utils/scintilla_api.h"
#include "utils/completion_context.h"
#include "dpi.h"
#include <algorithm>
#include <iostream>
#include <QAccessible>
#include <QCheckBox>
#include <QKeyEvent>
#include <QFocusEvent>
#include <QMouseEvent>
#include <QWheelEvent>

#include <QDrag>
#include <QDragEnterEvent>
#include <QDropEvent>
#include <QRegularExpression>
#include <QSet>
#include <QMenu>
#include <QContextMenuEvent>
#include <QPainter>
#include <QImage>
#include <QColor>
#include <QFont>
#include <QPolygonF>
#include <QSettings>
#include <QShortcut>
#include <Qsci/qscicommandset.h>
#include <Qsci/qscilexer.h>
#include <QPainter>
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
#include <QRecursiveMutex>
#endif

// Container indicator (>= INDICATOR_CONTAINER) for the error underline.
static const int kErrorIndicator = 20;

SonicPiScintilla::SonicPiScintilla(SonicPiLexer* lexer, SonicPiTheme* theme, QString fileName, bool autoIndent)
    : QsciScintilla()
{
    setAcceptDrops(true);

    this->theme = theme;
    this->fileName = fileName;
    this->autoIndent = autoIndent;
    this->selectionMode = false;
    standardCommands()->clearKeys();
    standardCommands()->clearAlternateKeys();
    QString skey;
    QSettings settings(QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "scintilla-key-bindings");
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
    mutex = new QRecursiveMutex();
#else
    mutex = new QMutex(QMutex::Recursive);
#endif

#if defined(Q_OS_MAC)
    int SPi_CTRL = Qt::META;
    int SPi_META = Qt::CTRL;
#else
    int SPi_CTRL = Qt::CTRL;
    int SPi_META = Qt::ALT;
#endif

    // basic navigation
    addKeyBinding(settings, QsciCommand::PageDown, Qt::Key_PageDown);
    addKeyBinding(settings, QsciCommand::PageUp, Qt::Key_PageUp);
    addOtherKeyBinding(settings, QsciCommand::LineDown, Qt::Key_Down);
    addKeyBinding(settings, QsciCommand::LineDownExtend, Qt::Key_Down | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::LineUp, Qt::Key_Up);
    addKeyBinding(settings, QsciCommand::LineUpExtend, Qt::Key_Up | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::CharRight, Qt::Key_Right);
    addKeyBinding(settings, QsciCommand::CharRightExtend, Qt::Key_Right | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::CharLeft, Qt::Key_Left);
    addKeyBinding(settings, QsciCommand::CharLeftExtend, Qt::Key_Left | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::Delete, Qt::Key_Delete);
    addOtherKeyBinding(settings, QsciCommand::DeleteBack, Qt::Key_Backspace);
    addKeyBinding(settings, QsciCommand::VCHome, Qt::Key_Home);
    addKeyBinding(settings, QsciCommand::VCHomeExtend, Qt::Key_Home | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::LineEnd, Qt::Key_End);
    addKeyBinding(settings, QsciCommand::LineEndExtend, Qt::Key_End | Qt::SHIFT);
    addKeyBinding(settings, QsciCommand::Backtab, Qt::Key_Tab | Qt::SHIFT);

    standardCommands()->readSettings(settings);

    this->setMatchedBraceBackgroundColor(theme->color("MatchedBraceBackground"));
    this->setMatchedBraceForegroundColor(theme->color("MatchedBraceForeground"));

    setIndentationWidth(ScaleHeightForDPI(2));
    setIndentationGuides(true);
    setIndentationGuidesForegroundColor(theme->color("IndentationGuidesForeground"));
    setBraceMatching(SonicPiScintilla::SloppyBraceMatch);

    // Drop the default sunken frame so the editor's scrollbars sit flush to the
    // edge, matching every other pane's shared 2dx scrollbar offset.
    setFrameShape(QFrame::NoFrame);

    // TODO: add preference toggle for this:
    // this->setFolding(SonicPiScintilla::CircledTreeFoldStyle, 2);
    setCaretLineVisible(true);
    setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
    setFoldMarginColors(theme->color("FoldMarginForeground"), theme->color("FoldMarginForeground"));
    setMarginLineNumbers(0, true);

    setMarginsBackgroundColor(theme->color("MarginBackground"));
    setMarginsForegroundColor(theme->color("MarginForeground"));
    setMarginsFont(QFont("Hack", 15, -1, true));
    setUtf8(true);
    setText("# Loading previous buffer contents. Please wait...");
    setLexer((QsciLexer*)lexer);

    // marker 9: translucent full-line wash over the code area.
    markerDefine(QsciScintilla::Background, 9);
    setMarkerBackgroundColor(theme->color("MarkerBackground"), 9);
    SendScintilla(SCI_MARKERSETALPHA, 9, 40);

    // No blank inset before the text, so the error line's wash meets the symbol
    // margin with no untinted seam.
    SendScintilla(SCI_SETMARGINLEFT, (unsigned long)0, (long)0);

    // marker 8 (gutter dot) and marker 10 (number-margin tint) are RGBA images
    // sized to the live line height / margin width, so setLineErrorMarker builds
    // them. Route marker 10 to the number margin only, not the symbol margin.
    SendScintilla(SCI_SETMARGINMASKN, (unsigned long)0, (long)(1 << 10));
    long errSymMask = SendScintilla(SCI_GETMARGINMASKN, (unsigned long)1);
    SendScintilla(SCI_SETMARGINMASKN, (unsigned long)1, (long)(errSymMask & ~(1 << 10)));

    // Zig-zag (squiggle) underline beneath the offending code on the error line.
    // An indicator is vector-drawn by Scintilla, so it tracks zoom and edits for
    // free; its colour is set per error (pink runtime / blue syntax) in
    // applyErrorMarkers. The squiggle itself is Sonic Pi's enlarged smooth
    // zig-zag (patched in QScintilla_src .../Indicator.cpp) matching the error
    // card's; the extra descent below grants it room to sit clear of the text.
    SendScintilla(SCI_INDICSETSTYLE, (unsigned long)kErrorIndicator, (long)INDIC_SQUIGGLE);
    SendScintilla(SCI_SETEXTRADESCENT, (long)ScaleHeightForDPI(8));

    // Drive completion through our own popup (CompletionPopup) rather than
    // Scintilla's built-in list, so each row can show a kind badge + summary.
    setAutoCompletionSource(SonicPiScintilla::AcsNone);
    setAutoCompletionThreshold(-1);
    setAutoCompletionCaseSensitivity(false);
    m_completion = new CompletionPopup(this);
    m_completion->applyTheme(theme->color("Background"), theme->color("Foreground"),
                             theme->color("HighlightedBackground"),
                             theme->contrastingText(theme->color("HighlightedBackground")));
    // Clicking the mini piano accepts that note like Tab/Return.
    connect(m_completion, &CompletionPopup::accepted, this, [this]() { acceptCompletion(); });
    // Live-preview the selected entry (list navigation, note, slider drag) in the
    // buffer in place of the typed word.
    connect(m_completion, &CompletionPopup::previewChanged, this,
            [this](const QString& text) { applyPreview(text); });
    // Backstop refresh for buffer changes that don't arrive via the keypress path
    // (e.g. programmatic edits). The keypress path is the authoritative trigger
    // (it runs after the caret settles); this only re-filters an already-open
    // popup, guarded by isShowing(). Skip our own preview edits (m_pvGuard).
    connect(this, &QsciScintilla::textChanged, this, [this]() {
        if (m_pvGuard) return;
        if (m_completion && m_completion->isShowing()) updateCompletion();
    });
    // The popup's "Docs" button opens the help pane (handled by MainWindow).
    connect(m_completion, &CompletionPopup::docsRequested, this,
            [this](const QString& name) { m_completion->hidePopup(); emit docsRequested(name); });
    // Relay popup navigation announcements up to MainWindow's screen-reader helper.
    connect(m_completion, &CompletionPopup::announceRequested, this,
            &SonicPiScintilla::announceRequested);

    setSelectionBackgroundColor(theme->color("SelectionBackground"));
    setSelectionForegroundColor(theme->contrastingText(theme->color("SelectionBackground")));
    setCaretWidth(ScaleHeightForDPI(5));
    setCaretForegroundColor(theme->color("CaretForeground"));
    setEolMode(EolUnix);

    SendScintilla(SCI_SETWORDCHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789:_?!");
}

void SonicPiScintilla::redraw()
{
    mutex->lock();
    setMarginsBackgroundColor(theme->color("MarginBackground"));
    setMarginsForegroundColor(theme->color("MarginForeground"));
    setSelectionBackgroundColor(theme->color("SelectionBackground"));
    setSelectionForegroundColor(theme->contrastingText(theme->color("SelectionBackground")));
    setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
    setFoldMarginColors(theme->color("FoldMarginForeground"), theme->color("FoldMarginForeground"));
    setIndentationGuidesForegroundColor(theme->color("IndentationGuidesForeground"));
    setMatchedBraceBackgroundColor(theme->color("MatchedBraceBackground"));
    setMatchedBraceForegroundColor(theme->color("MatchedBraceForeground"));
    if (m_completion)
    {
        m_completion->applyTheme(theme->color("Background"), theme->color("Foreground"),
                                 theme->color("HighlightedBackground"),
                                 theme->contrastingText(theme->color("HighlightedBackground")));
    }
    // Re-tint the error-line markers (gutter dot/washes + squiggle) so a visible
    // error tracks the new theme (applyErrorMarkers runs under the held mutex).
    if (m_errorLine >= 0)
        applyErrorMarkers(m_errorLine);
    mutex->unlock();
}

void SonicPiScintilla::highlightCurrentLine()
{
    mutex->lock();
    setCaretLineBackgroundColor(theme->color("SelectionBackground"));
    mutex->unlock();
}

void SonicPiScintilla::unhighlightCurrentLine()
{
    mutex->lock();
    setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
    mutex->unlock();
}

void SonicPiScintilla::hideLineNumbers()
{
    mutex->lock();
    setMarginLineNumbers(0, false);
    setMarginWidth(0, "0");
    updateErrorMarginWidth();
    SendScintilla(SCI_HIDELINES);
    mutex->unlock();
}

void SonicPiScintilla::showLineNumbers()
{
    mutex->lock();
    setMarginLineNumbers(0, true);
    setMarginWidth(0, "1000");
    updateErrorMarginWidth();
    SendScintilla(SCI_SHOWLINES);
    mutex->unlock();
}

void SonicPiScintilla::addOtherKeyBinding(QSettings& qs, int cmd, int key)
{
    mutex->lock();
    QString skey;
    QTextStream(&skey) << "/Scintilla/keymap/c" << cmd << "/alt";
    qs.setValue(skey, key);
    mutex->unlock();
}

void SonicPiScintilla::addKeyBinding(QSettings& qs, int cmd, int key)
{
    mutex->lock();
    QString skey;
    QTextStream(&skey) << "/Scintilla/keymap/c" << cmd << "/key";
    qs.setValue(skey, key);
    mutex->unlock();
}

void SonicPiScintilla::cutLineFromPoint()
{
    mutex->lock();
    int linenum, index;
    getCursorPosition(&linenum, &index);

    if (text(linenum).mid(index).contains(QRegularExpression("^\\s*\\n")))
    {
        setSelection(linenum, index, linenum + 1, 0);
        SendScintilla(SCI_CUT);
    }
    else
    {
        //  SendScintilla(SCI_CLEARSELECTIONS);
        int pos = SendScintilla(SCI_GETCURRENTPOS);

        SendScintilla(SCI_LINEEND);
        SendScintilla(SCI_SETANCHOR, pos);
        SendScintilla(SCI_CUT);
    }
    mutex->unlock();
}

void SonicPiScintilla::tabCompleteifList()
{
    mutex->lock();
    if (isListActive())
    {
        SendScintilla(QsciCommand::Tab);
    }
    mutex->unlock();
}

void SonicPiScintilla::transposeChars()
{
    mutex->lock();
    int linenum, index;
    getCursorPosition(&linenum, &index);
    setSelection(linenum, 0, linenum + 1, 0);
    int lineLength = selectedText().size();

    // transpose chars
    if (index > 0)
    {
        if (index < (lineLength - 1))
        {
            index = index + 1;
        }
        setSelection(linenum, index - 2, linenum, index);
        QString text = selectedText();
        QChar a, b;
        a = text.at(0);
        b = text.at(1);
        QString replacement = "";
        replacement.append(b);
        replacement.append(a);
        replaceSelectedText(replacement);
    }

    setCursorPosition(linenum, index);
    mutex->unlock();
}

void SonicPiScintilla::setMark()
{
    mutex->lock();
    int pos = SendScintilla(SCI_GETCURRENTPOS);
    SendScintilla(SCI_SETEMPTYSELECTION, pos);
    SendScintilla(SCI_SETSELECTIONMODE, 0);
    this->selectionMode = true;
    mutex->unlock();
}

void SonicPiScintilla::escapeAndCancelSelection()
{
    // Escape (a QShortcut, so it never reaches event()) first dismisses the
    // completion popup if it's open, reverting the preview: a slider goes back to
    // its original value, a list back to the text typed before the popup opened.
    if (m_completion && m_completion->isShowing())
    {
        if (m_pvSlider) restoreOriginal();
        else clearPreview();
        endPreview();
        m_completion->hidePopup();
        return;
    }
    mutex->lock();
    int pos = SendScintilla(SCI_GETCURRENTPOS);
    SendScintilla(SCI_SETEMPTYSELECTION, pos);
    SendScintilla(SCI_CANCEL);
    this->selectionMode = false;
    mutex->unlock();
}

void SonicPiScintilla::deselect()
{
    mutex->lock();
    int pos = SendScintilla(SCI_GETCURRENTPOS);
    SendScintilla(SCI_SETEMPTYSELECTION, pos);
    this->selectionMode = false;
    mutex->unlock();
}

void SonicPiScintilla::copyClear()
{
    mutex->lock();
    QsciScintilla::copy();
    deselect();
    mutex->unlock();
}

void SonicPiScintilla::replaceLine(int lineNumber, QString newLine)
{
    mutex->lock();
    setSelection(lineNumber, 0, lineNumber + 1, 0);
    replaceSelectedText(newLine);
    mutex->unlock();
}

void SonicPiScintilla::replaceLines(int lineStart, int lineFinish, QString newLines)
{
    mutex->lock();
    setSelection(lineStart, 0, lineFinish + 1, 0);
    replaceSelectedText(newLines);
    mutex->unlock();
}

void SonicPiScintilla::forwardLines(int numLines)
{
    mutex->lock();
    int idx;
    if (numLines > 0)
    {
        for (idx = 0; idx < numLines; idx++)
        {
            if (selectionMode)
            {
                SendScintilla(SCI_LINEDOWNEXTEND);
            }
            else
            {
                SendScintilla(SCI_LINEDOWN);
            }
        }
    }
    else
    {
        for (idx = 0; idx > numLines; idx--)
        {
            if (selectionMode)
            {
                SendScintilla(SCI_LINEUPEXTEND);
            }
            else
            {
                SendScintilla(SCI_LINEUP);
            }
        }
    }
    mutex->unlock();
}

void SonicPiScintilla::forwardOneLine()
{
    // While the completion popup is open, the "move down" shortcut (whatever it
    // is bound to) moves the highlight instead of the caret.
    if (m_completion && m_completion->isShowing()) { m_completion->moveSelection(+1); return; }
    forwardLines(1);
}

void SonicPiScintilla::backOneLine()
{
    if (m_completion && m_completion->isShowing()) { m_completion->moveSelection(-1); return; }
    forwardLines(-1);
}

void SonicPiScintilla::forwardTenLines()
{
    if (m_completion && m_completion->isShowing()) { m_completion->moveSelection(+10); return; }
    mutex->lock();
    forwardLines(10);
    mutex->unlock();
}

void SonicPiScintilla::backTenLines()
{
    if (m_completion && m_completion->isShowing()) { m_completion->moveSelection(-10); return; }
    mutex->lock();
    forwardLines(-10);
    mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelectionUp()
{
    mutex->lock();
    moveLineOrSelection(-1);
    mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelectionDown()
{
    mutex->lock();
    moveLineOrSelection(1);
    mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelection(int numLines)
{
    mutex->lock();
    beginUndoAction();

    int linenum, cursor, origLinenum, origCursor;
    getCursorPosition(&linenum, &cursor);
    origLinenum = linenum;
    origCursor = cursor;

    bool hadSelectedText = hasSelectedText();

    if (!hadSelectedText)
    {
        setSelection(linenum, 0, linenum + 1, 0);
    }

    int lineFrom, indexFrom, lineTo, indexTo, lineOffset;
    getSelection(&lineFrom, &indexFrom, &lineTo, &indexTo);
    lineOffset = lineTo - origLinenum;
    linenum = lineFrom;

    QString selection = selectedText();

    if (selection[selection.length() - 1] != '\n')
    {
        selection = selection + "\n";
        lineTo += 1;
        lineOffset += 1;
        indexTo = 0;
        replaceSelectedText("");
        setCursorPosition(linenum, 0);
        SendScintilla(SCI_DELETEBACK);
    }
    else
    {
        replaceSelectedText("");
    }
    setCursorPosition(linenum, 0);

    moveLines(numLines);

    getCursorPosition(&linenum, &cursor);
    setCursorPosition(linenum, 0);
    insert(selection);

    setCursorPosition(linenum + lineOffset, origCursor);

    int diffLine = lineTo - lineFrom;
    int diffIndex = indexTo - indexFrom;

    setSelection(linenum + diffLine, diffIndex, linenum, 0);

    endUndoAction();
    mutex->unlock();
}

QStringList SonicPiScintilla::apiContext(int pos, int& context_start,
    int& last_word_start)
{
    // sampl|  /  sample |  /  chord :E3,|
    int linenum, cursor;
    getCursorPosition(&linenum, &cursor);

    context_start = 0;
    last_word_start = pos;

    // The token reduction is a pure function (utils/completion_context) so the
    // completion detection it drives can be tested from hardcoded text + cursor.
    return SonicPi::lineToContext(text(linenum), cursor);
}

int SonicPiScintilla::tokenEndForCaret(int pos)
{
    const int len = SendScintilla(SCI_GETLENGTH);
    int end = pos;
    while (end < len)
    {
        const char c = (char)SendScintilla(SCI_GETCHARAT, end);
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' ||
            c == '(' || c == ')' || c == '{' || c == '}' ||
            c == '[' || c == ']' || c == '"' || c == '\'' || c == '#')
            break;
        ++end;
    }
    return end;
}

int SonicPiScintilla::incLineNumWithinBounds(int linenum, int inc)
{
    mutex->lock();
    linenum += inc;
    int maxBufferIndex = lines() - 1;

    if (linenum < 0)
    {
        linenum = 0;
    }

    if (linenum > maxBufferIndex)
    {
        linenum = maxBufferIndex;
    }

    return linenum;
    mutex->unlock();
}

void SonicPiScintilla::moveLines(int numLines)
{
    mutex->lock();
    if (numLines > 0)
    {
        for (int i = 0; i < numLines; i++)
        {
            SendScintilla(SCI_LINEDOWN);
        }
    }
    else
    {
        for (int i = 0; i > numLines; i--)
        {
            SendScintilla(SCI_LINEUP);
        }
    }
    mutex->unlock();
}

void SonicPiScintilla::upcaseWordOrSelection()
{
    mutex->lock();
    if (hasSelectedText())
    {
        SendScintilla(SCI_UPPERCASE);
    }
    else
    {
        setMark();
        SendScintilla(SCI_WORDRIGHT);
        SendScintilla(SCI_UPPERCASE);
        deselect();
    }
    mutex->unlock();
}

void SonicPiScintilla::downcaseWordOrSelection()
{
    mutex->lock();
    if (hasSelectedText())
    {
        SendScintilla(SCI_LOWERCASE);
    }
    else
    {
        setMark();
        SendScintilla(SCI_WORDRIGHT);
        SendScintilla(SCI_LOWERCASE);
        deselect();
    }
    mutex->unlock();
}

void SonicPiScintilla::setLineErrorMarker(int lineNumber, bool isSyntaxError, const QString& errorToken, int colStart, int colEnd)
{
    mutex->lock();
    m_errorLine = lineNumber;
    m_errorIsSyntax = isSyntaxError;
    m_errorToken = errorToken;
    m_errorColStart = colStart;
    m_errorColEnd = colEnd;
    applyErrorMarkers(lineNumber);

    // Perhaps consider a more manual way of returning this functionality:
    // int currlinenum, index;
    // getCursorPosition(&currlinenum, &index);
    // if (lineNumber != currlinenum) {
    //   setCursorPosition(lineNumber, 0);
    // }

    mutex->unlock();
}

// Rebuild the error line's markers at the current zoom, so the image-based
// margin washes and dot track the line height like the dynamic code wash does.
void SonicPiScintilla::refreshErrorMarkers()
{
    mutex->lock();
    if (m_errorLine >= 0)
        applyErrorMarkers(m_errorLine);
    mutex->unlock();
}

void SonicPiScintilla::applyErrorMarkers(int lineNumber)
{
    markerDeleteAll(-1);

    // Runtime errors are pink, syntax errors blue; the dot, washes and underline
    // all take this colour. Marker 9 is the translucent line wash — its alpha
    // comes from the colour (setMarkerBackgroundColor also sets the marker alpha),
    // so pass a 40-alpha colour, not the opaque one.
    QColor errCol = theme->color(m_errorIsSyntax ? "MarkerBackgroundSyntax" : "MarkerBackground");
    QColor errWash = errCol;
    errWash.setAlpha(40);
    setMarkerBackgroundColor(errWash, 9);

    int errH = SendScintilla(SCI_TEXTHEIGHT, (unsigned long)0);
    // The arrowhead fills the symbol-margin gap between the number and the code.
    // The gap is kept at this same char-proportional width in every state, so
    // showing or dismissing an error never shifts the code horizontally.
    int gapW = updateErrorMarginWidth();

    qreal errDpr = devicePixelRatioF();
    if (errDpr < 1.0) errDpr = 1.0;

    // symbol margin: left at its default background; only the coloured dot sits
    // there (no solid fill).

    // number margin (10): solid fill + black right-justified line number, drawn
    // ourselves since the opaque fill covers Scintilla's own (grey) number.
    int numW = SendScintilla(SCI_GETMARGINWIDTHN, (unsigned long)0);
    if (numW > 0 && errH > 0) {
        QImage img(qRound(numW * errDpr), qRound(errH * errDpr), QImage::Format_ARGB32);
        img.setDevicePixelRatio(errDpr);
        img.fill(errCol);
        {
            QPainter p(&img);
            p.setRenderHint(QPainter::TextAntialiasing, true);
            int zoom = SendScintilla(SCI_GETZOOM);
            QFont f("Hack", qMax(1, 15 + zoom), -1, true);
            p.setFont(f);
            p.setPen(Qt::black);
            p.drawText(QRect(0, 0, numW - 3, errH), Qt::AlignRight | Qt::AlignVCenter,
                       QString::number(lineNumber + 1));
        }
        SendScintilla(SCI_RGBAIMAGESETSCALE, (unsigned long)qRound(errDpr * 100));
        markerDefine(img, 10);
    }

    // gutter marker: full-line-height arrowhead pointing right toward the code
    // (error colour), sized to the one-character gap so it scales with the font.
    if (errH >= 8) {
        int hPx = qRound(errH * errDpr);
        int wPx = qRound(gapW * errDpr);
        QImage dot(wPx, hPx, QImage::Format_ARGB32);
        dot.fill(Qt::transparent);
        {
            QPainter dp(&dot);
            dp.setRenderHint(QPainter::Antialiasing, true);
            dp.setPen(Qt::NoPen);
            dp.setBrush(errCol);
            // Inset the base a little so there's a small gap between the triangle
            // and the highlighted number margin to its left.
            int leftPx = qRound((gapW / 6.0) * errDpr);
            QPolygonF tri;
            tri << QPointF(leftPx, 0) << QPointF(wPx, hPx / 2.0) << QPointF(leftPx, hPx);
            dp.drawPolygon(tri);
        }
        dot.setDevicePixelRatio(errDpr);
        SendScintilla(SCI_RGBAIMAGESETSCALE, (unsigned long)qRound(errDpr * 100));
        markerDefine(dot, 8);
    }

    // Dashed underline in the error colour. Underline just the offending
    // identifier when the exception named one (found whole-word in the line's
    // byte range so multi-byte characters don't shift it); otherwise underline
    // the whole line's code, skipping the leading indentation.
    int lineStart = SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)lineNumber);
    int lineEnd = SendScintilla(SCI_GETLINEENDPOSITION, (unsigned long)lineNumber);
    int errFrom = -1;
    int errLen = 0;
    // 1. Exact byte-column span from error_highlight (clamped to the line).
    if (m_errorColStart >= 0 && m_errorColEnd > m_errorColStart) {
        int from = lineStart + m_errorColStart;
        int to = lineStart + m_errorColEnd;
        if (to > lineEnd) to = lineEnd;
        if (from < lineEnd) {
            errFrom = from;
            errLen = to - from;
        }
    }
    // 2. Fall back to the identifier named in the message (whole-word search).
    if (errFrom < 0 && !m_errorToken.isEmpty()) {
        QByteArray tok = m_errorToken.toUtf8();
        SendScintilla(SCI_SETSEARCHFLAGS, (unsigned long)SCFIND_WHOLEWORD);
        SendScintilla(SCI_SETTARGETSTART, (unsigned long)lineStart);
        SendScintilla(SCI_SETTARGETEND, (unsigned long)lineEnd);
        int found = SendScintilla(SCI_SEARCHINTARGET, static_cast<uintptr_t>(tok.length()), tok.constData());
        if (found >= 0) {
            errFrom = found;
            errLen = tok.length();
        }
    }
    // 3. Otherwise underline the whole line's code.
    if (errFrom < 0) {
        errFrom = SendScintilla(SCI_GETLINEINDENTPOSITION, (unsigned long)lineNumber);
        errLen = lineEnd - errFrom;
    }
    SendScintilla(SCI_INDICSETFORE, (unsigned long)kErrorIndicator,
                  (long)((errCol.blue() << 16) | (errCol.green() << 8) | errCol.red()));
    SendScintilla(SCI_SETINDICATORCURRENT, (unsigned long)kErrorIndicator);
    SendScintilla(SCI_INDICATORCLEARRANGE, (unsigned long)0, (long)SendScintilla(SCI_GETLENGTH));
    if (errLen > 0)
        SendScintilla(SCI_INDICATORFILLRANGE, (unsigned long)errFrom, (long)errLen);

    markerAdd(lineNumber, 8);
    markerAdd(lineNumber, 9);
    markerAdd(lineNumber, 10);
}

void SonicPiScintilla::clearLineMarkers()
{
    mutex->lock();
    m_errorLine = -1;
    markerDeleteAll(-1);
    SendScintilla(SCI_SETINDICATORCURRENT, (unsigned long)kErrorIndicator);
    SendScintilla(SCI_INDICATORCLEARRANGE, (unsigned long)0, (long)SendScintilla(SCI_GETLENGTH));
    mutex->unlock();
}

int SonicPiScintilla::updateErrorMarginWidth()
{
    int gapW = (SendScintilla(SCI_TEXTWIDTH, static_cast<uintptr_t>(STYLE_DEFAULT), "0") * 13) / 10;
    if (gapW < 4)
        gapW = (SendScintilla(SCI_TEXTHEIGHT, (unsigned long)0) * 4) / 5;
    setMarginWidth(1, gapW);
    return gapW;
}

void SonicPiScintilla::zoomFontIn()
{
    mutex->lock();
    int zoom = property("zoom").toInt();
    zoom++;
    if (zoom > 20)
        zoom = 20;
    setProperty("zoom", QVariant(zoom));
    zoomTo(zoom);
    updateErrorMarginWidth();
    mutex->unlock();
    refreshErrorMarkers();
}

void SonicPiScintilla::zoomFontOut()
{
    mutex->lock();
    int zoom = property("zoom").toInt();
    zoom--;
    if (zoom < -10)
        zoom = -10;
    setProperty("zoom", QVariant(zoom));
    zoomTo(zoom);
    updateErrorMarginWidth();
    mutex->unlock();
    refreshErrorMarkers();
}

void SonicPiScintilla::wheelEvent(QWheelEvent* event)
{
    QsciScintilla::wheelEvent(event);
    // Ctrl+wheel zooms the code; keep the gap and error markers at the new line height.
    if (event->modifiers() & Qt::ControlModifier)
    {
        mutex->lock();
        updateErrorMarginWidth();
        mutex->unlock();
        refreshErrorMarkers();
    }
}

void SonicPiScintilla::newLine()
{
    mutex->lock();
    SendScintilla(QsciCommand::Newline);
    mutex->unlock();
}

void SonicPiScintilla::replaceBuffer(QString content, int line, int index, int first_line)
{
    mutex->lock();
    beginUndoAction();
    insert(" ");
    SendScintilla(QsciCommand::Delete);
    selectAll();
    replaceSelectedText(content);
    setCursorPosition(line, index);
    setFirstVisibleLine(first_line);
    endUndoAction();
    mutex->unlock();
}

void SonicPiScintilla::completeListOrNewlineAndIndent()
{
    // The completion popup owns Return when it is open (accept the highlight).
    if (m_completion && m_completion->isShowing())
    {
        acceptCompletion();
        return;
    }
    mutex->lock();
    if (isListActive())
    {
        tabCompleteifList();
    }
    else
    {
        if (autoIndent)
        {
            newlineAndIndent();
        }
        else
        {
            newLine();
        }
    }
    mutex->unlock();
}

void SonicPiScintilla::newlineAndIndent()
{
    mutex->lock();
    int point_line, point_index, first_line;
    getCursorPosition(&point_line, &point_index);
    first_line = firstVisibleLine();

    std::string code = text().toStdString();

    emit bufferNewlineAndIndent(point_line, point_index, first_line, code, fileName.toStdString());
    mutex->unlock();
}

void SonicPiScintilla::dragEnterEvent(QDragEnterEvent* event)
{
    mutex->lock();
    if (event->mimeData()->hasFormat("text/uri-list"))
    {
        event->acceptProposedAction();
    }
    mutex->unlock();
}

void SonicPiScintilla::dragMoveEvent(QDragMoveEvent* event)
{
    mutex->lock();
    if (event->mimeData()->hasFormat("text/uri-list"))
    {
        event->acceptProposedAction();
    }
    mutex->unlock();
}

void SonicPiScintilla::focusOutEvent(QFocusEvent* e)
{
    // Dismiss the completion popup when the editor loses focus (clicking away,
    // switching apps) — but not when the cursor is over the popup itself (e.g.
    // clicking a piano key), which would cancel the click.
    if (m_completion && m_completion->isShowing() && !m_completion->underMouse())
    {
        clearPreview();   // drop a list preview (keeps a committed slider value)
        endPreview();
        m_completion->hidePopup();
    }
    QsciScintilla::focusOutEvent(e);
}

void SonicPiScintilla::contextMenuEvent(QContextMenuEvent* event)
{
    // Move the caret under the click (unless there's a selection) so word/line
    // actions like "Show Docs for Current Word" act on what was right-clicked.
    if (!hasSelectedText())
    {
        const int pos = (int)SendScintilla(SCI_POSITIONFROMPOINT,
                                           (unsigned long)event->pos().x(),
                                           (long)event->pos().y());
        if (pos >= 0) SendScintilla(SCI_SETEMPTYSELECTION, pos);
    }
    // Standard edit menu (cut/copy/paste/…); MainWindow appends the idiomatic
    // code actions (Show Docs for word, Comment/Uncomment, Align) via the signal.
    QMenu* menu = createStandardContextMenu();
    if (!menu) menu = new QMenu(this);
    emit extendContextMenu(menu);
    menu->exec(event->globalPos());
    delete menu;
}

bool SonicPiScintilla::event(QEvent* evt)
{
    if (evt->type() == QEvent::KeyPress)
    {
        QKeyEvent* key = static_cast<QKeyEvent*>(evt);
        const int k = key->key();

        // Raw arrow / page / escape keys (from Scintilla's keymap) drive the
        // popup directly. The configured nav/accept shortcuts (Ctrl+n, Tab,
        // Return, …) are handled by their existing slots, which are popup-aware,
        // so the popup automatically honours whatever the user has bound.
        if (m_completion && m_completion->isShowing())
        {
            switch (k)
            {
            case Qt::Key_Up:       m_completion->moveSelection(-1);  return true;
            case Qt::Key_Down:     m_completion->moveSelection(+1);  return true;
            case Qt::Key_PageUp:   m_completion->moveSelection(-10); return true;
            case Qt::Key_PageDown: m_completion->moveSelection(+10); return true;
            // On the value slider, Left/Right step logarithmically (proportional)
            // while Up/Down step linearly; elsewhere they move the caret as usual.
            case Qt::Key_Left:
                if (m_completion->isSliderMode()) { m_completion->sliderNudgeLog(-1); return true; }
                break;
            case Qt::Key_Right:
                if (m_completion->isSliderMode()) { m_completion->sliderNudgeLog(+1); return true; }
                break;
            case Qt::Key_Space:
                // Space commits a previewed entry, then types the space; a plain
                // space otherwise (no preview shown yet).
                if (m_pvLive)
                {
                    SendScintilla(SCI_BEGINUNDOACTION);
                    acceptCompletion();
                    replaceSelectedText(" ");
                    SendScintilla(SCI_ENDUNDOACTION);
                    return true;
                }
                break;
            case Qt::Key_Escape:
                if (m_pvSlider) restoreOriginal(); else clearPreview();
                endPreview(); m_completion->hidePopup();             return true;
            default: break;
            }
        }

        // Default Return: accept the popup (handled inside) or newline+indent.
        if (k == Qt::Key_Return || k == Qt::Key_Enter)
        {
            completeListOrNewlineAndIndent();
            return true;
        }

        // Coalesce preview-restore + the edit + re-preview into one undo step, so
        // a single Cmd-Z removes the whole keystroke (not each preview edit).
        SendScintilla(SCI_BEGINUNDOACTION);
        // Drop the preview first so the keystroke edits the user's typed text (or,
        // for a slider, leaves its committed value), then let the editor process it.
        if (m_pvStart >= 0) clearPreview();

        // Let the editor insert/delete the character, then refresh the popup. This
        // path runs after the edit fully settles (cursor advanced), so the context
        // is correct — unlike textChanged, which fires mid-edit before the caret
        // moves (so e.g. `play ` would still read as `play`).
        bool res = QsciScintilla::event(evt);
        const QString t = key->text();
        const bool printable = !t.isEmpty() && t[0].isPrint();
        if (printable || k == Qt::Key_Backspace)
        {
            updateCompletion();
        }
        else if (m_completion && m_completion->isShowing())
        {
            // A bare modifier press (Ctrl/Shift/Alt/Meta) is the start of a chord
            // such as C-n — keep the popup open so the chord's shortcut can drive
            // it. Only genuinely dismiss on other non-text keys.
            switch (k)
            {
            case Qt::Key_Control:
            case Qt::Key_Shift:
            case Qt::Key_Alt:
            case Qt::Key_Meta:
            case Qt::Key_AltGr:
            case Qt::Key_CapsLock:
                break;
            default:
                endPreview();
                m_completion->hidePopup();
                break;
            }
        }
        SendScintilla(SCI_ENDUNDOACTION);
        return res;
    }

    return QsciScintilla::event(evt);
}

// Append names the user has defined in this buffer (define/live_loop/cue/set) to
// the completion list — the static API can't know about them. `symbol` prefixes a
// ':' (for sync/cue value positions). Dedups against the existing items.
static void addBufferDefs(QList<CompletionItem>& items, const QString& buffer,
                          const QRegularExpression& re, bool symbol)
{
    if (items.isEmpty()) return;
    const QString kind = items.first().kind;
    QSet<QString> have;
    for (const CompletionItem& it : items) have.insert(it.text);
    QRegularExpressionMatchIterator mi = re.globalMatch(buffer);
    while (mi.hasNext())
    {
        const QString name = mi.next().captured(1);
        if (name.isEmpty()) continue;
        const QString t = symbol ? (QStringLiteral(":") + name) : name;
        if (have.contains(t)) continue;
        have.insert(t);
        CompletionItem it;
        it.text = t;
        it.kind = kind;
        it.summary = QObject::tr("defined in this buffer");
        items.append(it);
    }
}

void SonicPiScintilla::triggerCompletion()
{
    // Explicit invocation (menu/shortcut): show suggestions at the caret even when
    // automatic completion is turned off, so it can be used purely on demand.
    updateCompletion(true);
}

void SonicPiScintilla::showCompletionDocs()
{
    // Surface the autocomplete docs for the current context: always (re)build the
    // popup for the CURRENT cursor position — like the trigger shortcut — so it
    // swaps when the cursor has moved to a different context, not just when hidden.
    // Then read the highlighted item's docstring for a screen reader.
    triggerCompletion();
    announceCompletionDetails();
}

void SonicPiScintilla::announceCompletionDetails()
{
    // Speak the highlighted item's full docstring on demand — the docs pane is
    // pruned from the accessibility tree, so this is how a screen reader hears it.
    if (!m_completion || !m_completion->isShowing()) return;
    const QString doc = m_completion->currentDoc();
    if (!doc.isEmpty()) emit announceRequested(doc);
}

void SonicPiScintilla::updateCompletion(bool force)
{
    if (!m_completion) return;
    if (!m_completionEnabled && !force)
    {
        clearPreview(); endPreview();
        m_completion->hidePopup();
        return;
    }

    // Work against the user's typed text / current value, not a stale preview.
    clearPreview();

    // Don't pop up inside a comment or a string literal: scan the line up to the
    // caret tracking quote state; bail on an unquoted '#' (comment) or if the
    // caret sits inside an unterminated string.
    {
        int gl, gc;
        getCursorPosition(&gl, &gc);
        const QString upto = text(gl).left(gc);
        QChar q;
        bool suppress = false;
        for (int i = 0; i < upto.length(); ++i)
        {
            const QChar c = upto[i];
            if (!q.isNull())
            {
                if (c == '\\') { ++i; continue; }   // skip escaped char in string
                if (c == q) q = QChar();
                continue;
            }
            if (c == '"' || c == '\'') { q = c; continue; }
            if (c == '#') { suppress = true; break; }   // comment to end of line
        }
        if (suppress || !q.isNull())
        {
            endPreview();
            m_completion->hidePopup();
            return;
        }
    }

    int pos = SendScintilla(SCI_GETCURRENTPOS);
    int context_start, last_word_start;
    QStringList context = apiContext(pos, context_start, last_word_start);
    QString partial = context.isEmpty() ? QString() : context.last();

    auto* api = dynamic_cast<ScintillaAPI*>(lexer() ? lexer()->apis() : nullptr);
    if (!api)
    {
        endPreview();
        m_completion->hidePopup();
        return;
    }

    // Remaining text on the line after the caret, so a chord/scale root
    // completion can look ahead to the name argument that follows it.
    int curLine, curCol;
    getCursorPosition(&curLine, &curCol);
    const QString afterCursor = text(curLine).mid(curCol);

    QList<CompletionItem> items = api->completionsFor(context, afterCursor);

    // Offer names defined in this buffer: user functions at a call position, and
    // live_loop/cue/set names where a cue symbol is expected (sync/cue/get/set).
    if (!items.isEmpty())
    {
        const QString k = items.first().kind;
        if (k == "fn")
        {
            static const QRegularExpression reDef(
                QStringLiteral("\\b(?:define|defonce)\\s+:([A-Za-z_][A-Za-z0-9_]*[?!]?)"));
            addBufferDefs(items, text(), reDef, false);
        }
        else if (k == "cue")
        {
            static const QRegularExpression reCue(
                QStringLiteral("\\b(?:live_loop|cue|set)\\s+:([A-Za-z_][A-Za-z0-9_]*)"));
            addBufferDefs(items, text(), reCue, true);
        }
    }

    // With no partial typed yet (e.g. just after a space), only pop up in a
    // "value" position — args/opts, synth/fx/sample/cue names — so `play :e3, `
    // shows the opts immediately. A bare space at the top level (the huge
    // function list, kind "fn") stays quiet.
    if (partial.isEmpty() && (items.isEmpty() || items.first().kind == "fn"))
    {
        endPreview();
        m_completion->hidePopup();
        return;
    }

    // Notes are an ordered instrument, never fuzzy-matched. Digits pick a number
    // directly; a note-name partial (c, cs, e3 …) resolves to the matching pitch
    // nearest middle C. Either way we show the full numeric list, ordered low→high,
    // and navigate it a semitone at a time — never a spray of name matches.
    const bool noteMode = !items.isEmpty() && items.first().kind == "note";
    bool useNumbers = noteMode;   // the numeric instrument, unless a name partial resolves nothing
    int preferNote = -1;
    if (noteMode && !partial.isEmpty())
    {
        // Note symbols carry a leading ':' (":c4"); match on the bare token so a
        // typed ":c" resolves the same as "c".
        const QString pat = partial.startsWith(':') ? partial.mid(1) : partial;
        const bool allDigits = !pat.isEmpty() &&
            std::all_of(pat.begin(), pat.end(), [](QChar c) { return c.isDigit(); });
        if (allDigits)
            preferNote = pat.toInt();
        else if (!pat.isEmpty())
        {
            // Resolve the typed note name to the matching pitch nearest middle C.
            int bestDist = 1000;
            for (const CompletionItem& it : items)
            {
                if (it.note < 0) continue;
                QString name = it.text.startsWith(':') ? it.text.mid(1) : it.text;
                if (name.startsWith(pat, Qt::CaseInsensitive))
                {
                    const int d = qAbs(it.note - 60);
                    if (d < bestDist) { bestDist = d; preferNote = it.note; }
                }
            }
            useNumbers = (preferNote >= 0);   // unresolved name → fall back to fuzzy
        }
    }
    if (useNumbers)
    {
        QList<CompletionItem> nums;
        for (const CompletionItem& it : items)
            if (!it.text.isEmpty() && it.text[0].isDigit()) nums.append(it);
        std::stable_sort(nums.begin(), nums.end(),
                         [](const CompletionItem& a, const CompletionItem& b) {
                             return a.note < b.note;
                         });
        items = nums;
    }

    QList<CompletionItem> filtered;
    if (items.size() == 1 && items.first().slider)
    {
        // A bounded opt offers a single slider value-picker — show it directly;
        // the partial (if any) is the value being typed and is replaced on accept.
        filtered = items;
    }
    else if (useNumbers)
    {
        // No fuzzy filtering — the full instrument stays, Up/Down walk it.
        filtered = items;
    }
    else
    {
        // Fuzzy match + rank: the partial's chars must appear in order, best first.
        QList<QPair<int, int>> ranked; // (score, index into items)
        for (int i = 0; i < items.size(); ++i)
        {
            int sc;
            if (SonicPi::fuzzyMatch(partial, items[i].text, sc))
                ranked.append(qMakePair(sc, i));
        }
        std::stable_sort(ranked.begin(), ranked.end(),
                         [](const QPair<int, int>& a, const QPair<int, int>& b) {
                             return a.first > b.first;
                         });
        for (const QPair<int, int>& r : ranked)
            filtered.append(items[r.second]);
    }

    if (filtered.isEmpty())
    {
        endPreview();
        m_completion->hidePopup();
        return;
    }

    // Track the editor's code font + live zoom. The list sits a touch below the
    // editor text size and is clamped; the docstring reads like prose, so it
    // tracks the editor's effective (zoomed) size directly — derived from the
    // same zoomed value, NOT the clamped list size, so it keeps following zoom
    // even once the list font saturates at its max. Notched down by a fixed
    // offset so the prose sits comfortably below the editor text size.
    constexpr double kDocFontOffset = 3.0;
    QFont codeFont = lexer() ? lexer()->defaultFont() : font();
    const double zoomed = codeFont.pointSize() + SendScintilla(SCI_GETZOOM);
    codeFont.setPointSizeF(qBound(8.0, zoomed * 0.82, 15.0));
    m_completion->setItemFont(codeFont, zoomed - kDocFontOffset);

    const int tokenEnd = tokenEndForCaret(pos);
    int wordStart = tokenEnd - partial.length();
    int x = SendScintilla(SCI_POINTXFROMPOSITION, 0, wordStart);
    int y = SendScintilla(SCI_POINTYFROMPOSITION, 0, pos);
    int line = SendScintilla(SCI_LINEFROMPOSITION, pos);
    int lh = SendScintilla(SCI_TEXTHEIGHT, line);
    // SCI coords are viewport-relative → map to global for the top-level popup.
    QPoint globalTop = viewport() ? viewport()->mapToGlobal(QPoint(x, y))
                                  : mapToGlobal(QPoint(x, y));
    m_completion->showItems(filtered, globalTop, lh, preferNote);

    // Arm the live preview but don't write anything yet: the buffer keeps the
    // user's typed text until they choose an entry (nav/click/drag), at which
    // point previewChanged → applyPreview() swaps it in. The managed span starts
    // at sepStart (eating whitespace a separating comma replaces) so the preview
    // shows the comma in place. m_pvOriginal is the Escape-restore text.
    const bool nowSlider = m_completion->isSliderMode();
    int sepStart;
    m_pvPrefix = nowSlider ? QString() : argSeparatorBefore(context, wordStart, sepStart);
    if (m_pvPrefix.isEmpty()) sepStart = wordStart;
    const QString original = text(sepStart, tokenEnd);   // whitespace + typed partial
    if (m_pvStart < 0 || nowSlider != m_pvSlider) m_pvOriginal = original;
    m_pvStart = sepStart;
    m_pvLen = original.length();
    m_pvSlider = nowSlider;
    m_pvRestore = original;
}

// A new argument (positional value or opt) typed after a preceding argument
// needs a separating comma: `scale 60 :a` → `scale 60, :aeolian`,
// `play 43 amp:` → `play 43, amp:`. Add it only when there's an argument before
// this one (not just the function name) and they aren't already comma/bracket
// separated — and not when the value belongs to a preceding opt (`note: 60`).
QString SonicPiScintilla::argSeparatorBefore(const QStringList& context,
                                             int wordStart, int& replaceStart)
{
    replaceStart = wordStart;

    // Locate the current call's argument run: skip a leading `lvalue =` (or any
    // operator token), then the function name. Tokens after that are existing
    // arguments — only then does a freshly-chosen one need a ", " to join them
    // (so `a = scale` completes the function, not `a =, scale`).
    QStringList words;
    for (int i = 0; i < context.size() - 1; ++i)
        if (!context[i].isEmpty()) words << context[i];
    int fnIdx = 0;
    for (int i = 0; i < words.size(); ++i) {
        const QChar c0 = words[i][0];
        const bool valueLike = c0.isLetterOrNumber() || c0 == ':' || c0 == '_'
                               || c0 == '\'' || c0 == '"';
        if (!valueLike) fnIdx = i + 1;   // an operator/assignment resets the call
    }
    if (words.size() - (fnIdx + 1) < 1)   // completing the function or its first arg
        return QString();

    int j = wordStart - 1;
    while (j >= 0) {
        const char c = (char)SendScintilla(SCI_GETCHARAT, j);
        if (c == ' ' || c == '\t') { --j; continue; }
        break;
    }
    if (j >= 0) {
        const char c = (char)SendScintilla(SCI_GETCHARAT, j);
        // ':' = the value belongs to a preceding opt (`note: 60`), not a new arg.
        if (c != ',' && c != '(' && c != '[' && c != '{' && c != ':') {
            replaceStart = j + 1;      // eat the whitespace after the value
            return QStringLiteral(", ");
        }
    }
    return QString();
}

void SonicPiScintilla::acceptCompletion()
{
    if (!m_completion) return;
    QString chosen = m_completion->currentText();
    clearPreview();   // restore the typed text, then do the real insert below
    endPreview();
    m_completion->hidePopup();
    if (chosen.isEmpty()) return;

    int pos = SendScintilla(SCI_GETCURRENTPOS);
    int context_start, last_word_start;
    QStringList context = apiContext(pos, context_start, last_word_start);
    QString partial = context.isEmpty() ? QString() : context.last();
    const int tokenEnd = tokenEndForCaret(pos);
    int wordStart = tokenEnd - partial.length();

    int selStart;
    QString insert = argSeparatorBefore(context, wordStart, selStart) + chosen;
    SendScintilla(SCI_SETSEL, selStart, tokenEnd);
    replaceSelectedText(insert);
}

bool SonicPiScintilla::completionActive() const
{
    return m_completion && m_completion->isShowing();
}

void SonicPiScintilla::acceptCompletionPopup()
{
    acceptCompletion();
}

// Replace the previewed span [m_pvStart, m_pvStart+m_pvLen) with `text`.
void SonicPiScintilla::replacePreviewSpan(const QString& text)
{
    m_pvGuard = true;
    SendScintilla(SCI_SETSEL, m_pvStart, m_pvStart + m_pvLen);
    replaceSelectedText(text);
    const int end = m_pvStart + (int)text.length();
    SendScintilla(SCI_SETSEL, end, end);
    m_pvLen = text.length();
    m_pvGuard = false;
}

void SonicPiScintilla::applyPreview(const QString& sel)
{
    if (m_pvStart < 0) return;
    // With a screen reader active, a list preview's buffer edit gets spoken on top
    // of the popup's own announcement (doubled/garbled speech). Skip the visual
    // preview for lists; the announcement conveys the selection and Enter still
    // commits. Slider values are committed live, so keep those.
    if (!m_pvSlider && QAccessible::isActive()) return;
    // Prepend any separating comma so the preview reads exactly as the commit will.
    replacePreviewSpan(m_pvPrefix + sel);
    m_pvLive = true;   // a selection is now shown in the buffer (Space can commit it)
    // A slider value is committed live, so the restore target tracks it (typing a
    // comma keeps the value); a list preview restores to the typed filter instead.
    if (m_pvSlider) m_pvRestore = sel;
}

void SonicPiScintilla::clearPreview()
{
    if (m_pvStart < 0 || !m_pvLive) return;   // nothing written to restore
    m_pvLive = false;
    replacePreviewSpan(m_pvRestore);
}

void SonicPiScintilla::restoreOriginal()
{
    m_pvLive = false;
    if (m_pvStart < 0) return;
    replacePreviewSpan(m_pvOriginal);
}

void SonicPiScintilla::endPreview()
{
    m_pvStart = -1;
    m_pvLen = 0;
    m_pvSlider = false;
    m_pvLive = false;
    m_pvRestore.clear();
    m_pvOriginal.clear();
}

void SonicPiScintilla::popCompletionOnClick()
{
    if (!m_completion || !m_completionEnabled || m_completion->isShowing()) return;
    auto* api = dynamic_cast<ScintillaAPI*>(lexer() ? lexer()->apis() : nullptr);
    if (!api) return;
    int pos = SendScintilla(SCI_GETCURRENTPOS);
    int cs, lws;
    QStringList context = apiContext(pos, cs, lws);
    if (context.isEmpty()) return;

    QList<CompletionItem> items = api->completionsFor(context);
    if (items.isEmpty()) return;

    // Only pop where the click lands in a tangible value slot — a note/chord/scale
    // (keyboard preview) or a bounded opt (`pan: 0.5` slider). Never the plain
    // function list, which would be intrusive on every click.
    const QString k = items.first().kind;
    const bool slider = (items.size() == 1 && items.first().slider);
    if (slider || k == "note" || k == "chord" || k == "scale" || k == "tuning")
        updateCompletion();
}

void SonicPiScintilla::mouseReleaseEvent(QMouseEvent* e)
{
    QsciScintilla::mouseReleaseEvent(e);
    popCompletionOnClick();   // clicking onto a note/chord/scale or `pan: 0.5` value opens its preview
}

void SonicPiScintilla::dropEvent(QDropEvent* dropEvent)
{
    mutex->lock();
    if (dropEvent->mimeData()->hasFormat("text/uri-list"))
    {
        dropEvent->acceptProposedAction();
        QList<QUrl> urlList = dropEvent->mimeData()->urls();
        QString text;
        for (int i = 0; i < urlList.size(); ++i)
        {
            text += "\"" + urlList.at(i).toLocalFile() + "\"" + QLatin1Char('\n');
        }
        insert(text);
    }
    mutex->unlock();
}

void SonicPiScintilla::sp_paste()
{
    mutex->lock();
    SendScintilla(QsciCommand::Paste);
    deselect();
    mutex->unlock();
}

void SonicPiScintilla::sp_cut()
{
    mutex->lock();
    SendScintilla(QsciCommand::SelectionCut);
    deselect();
    mutex->unlock();
}

void SonicPiScintilla::showAutoCompletion(bool val)
{
    // Toggles our custom popup; the native Scintilla list stays disabled.
    m_completionEnabled = val;
    if (!val && m_completion)
    {
        m_completion->hidePopup();
    }
}

void SonicPiScintilla::setCompletionHelp(bool val)
{
    // When off, the popup is a plain word list (no docstring/piano/slider panes).
    if (m_completion) m_completion->setShowHelp(val);
}

void SonicPiScintilla::setText(const QString& text)
{
    SendScintilla(SCI_CLEARALL);
    QByteArray bytes = textAsBytes(text);
    SendScintilla(SCI_ADDTEXT, bytes.size(), bytes.constData());
}

void SonicPiScintilla::setAutoIndentEnabled(bool enabled)
{
    this->autoIndent = enabled;
}

void SonicPiScintilla::charRight()
{
    mutex->lock();
    SendScintilla(QsciCommand::CharRight);
    mutex->unlock();
}

void SonicPiScintilla::charLeft()
{
    mutex->lock();
    SendScintilla(QsciCommand::CharLeft);
    mutex->unlock();
}

void SonicPiScintilla::deleteForward()
{
    mutex->lock();
    SendScintilla(QsciCommand::Delete);
    mutex->unlock();
}

void SonicPiScintilla::deleteBack()
{
    mutex->lock();
    SendScintilla(QsciCommand::DeleteBack);
    mutex->unlock();
}

void SonicPiScintilla::lineStart()
{
    mutex->lock();
    SendScintilla(QsciCommand::Home);
    mutex->unlock();
}

void SonicPiScintilla::lineEnd()
{
    mutex->lock();
    SendScintilla(QsciCommand::LineEnd);
    mutex->unlock();
}

void SonicPiScintilla::documentStart()
{
    mutex->lock();
    SendScintilla(QsciCommand::DocumentStart);
    mutex->unlock();
}

void SonicPiScintilla::documentEnd()
{
    mutex->lock();
    SendScintilla(QsciCommand::DocumentEnd);
    mutex->unlock();
}

void SonicPiScintilla::wordRight()
{
    mutex->lock();
    SendScintilla(QsciCommand::WordRight);
    mutex->unlock();
}

void SonicPiScintilla::wordLeft()
{
    mutex->lock();
    SendScintilla(QsciCommand::WordLeft);
    mutex->unlock();
}

void SonicPiScintilla::selectLineStart()
{
    mutex->lock();
    SendScintilla(QsciCommand::HomeExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectLineEnd()
{
    mutex->lock();
    SendScintilla(QsciCommand::LineEndExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectWordRight()
{
    mutex->lock();
    SendScintilla(QsciCommand::WordRightExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectWordLeft()
{
    mutex->lock();
    SendScintilla(QsciCommand::WordLeftExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectDocStart()
{
    mutex->lock();
    SendScintilla(QsciCommand::DocumentStartExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectDocEnd()
{
    mutex->lock();
    SendScintilla(QsciCommand::DocumentEndExtend);
    mutex->unlock();
}

void SonicPiScintilla::centerCaret()
{
    mutex->lock();
    SendScintilla(QsciCommand::VerticalCentreCaret);
    mutex->unlock();
}

void SonicPiScintilla::undo()
{
    mutex->lock();
    SendScintilla(QsciCommand::Undo);
    mutex->unlock();
}

void SonicPiScintilla::redo()
{
    mutex->lock();
    SendScintilla(QsciCommand::Redo);
    mutex->unlock();
}

void SonicPiScintilla::selectAll()
{
    mutex->lock();
    SendScintilla(QsciCommand::SelectAll);
    mutex->unlock();
}

void SonicPiScintilla::deleteWordRight()
{
    mutex->lock();
    SendScintilla(QsciCommand::DeleteWordRight);
    mutex->unlock();
}

void SonicPiScintilla::deleteWordLeft()
{
    mutex->lock();
    SendScintilla(QsciCommand::DeleteWordLeft);
    mutex->unlock();
}
