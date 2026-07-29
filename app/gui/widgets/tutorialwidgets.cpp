//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "tutorialwidgets.h"

#include <QAccessible>
#include <QAccessibleWidget>
#include <QApplication>
#include <QClipboard>
#include <QKeyEvent>
#include <QLabel>
#include <QMenu>
#include <QMouseEvent>
#include <QTextLayout>

#include <algorithm>

TutProseText::~TutProseText()
{
    if (m_group)
        m_group->remove(this);
}

void TutProseText::mousePressEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton || !m_group)
    {
        QWidget::mousePressEvent(e);
        return;
    }
    // A plain press starts a zero-length selection; a click that never drags
    // leaves it empty, so release still fires the link handler.
    const int pos = hitTest(e->position());
    m_group->beginDrag(this, pos);
    // The click also lands the keyboard caret, so arrow keys continue from
    // wherever was clicked (like a browser page).
    m_caret = qBound(0, pos, endPosition());
    QAccessibleTextCursorEvent ev(this, m_caret);
    QAccessible::updateAccessibility(&ev);
    update();
}

void TutProseText::mouseMoveEvent(QMouseEvent* e)
{
    if (m_group && m_group->dragging() && (e->buttons() & Qt::LeftButton))
    {
        m_group->extendTo(e->globalPosition().toPoint());
        return;
    }
    QString anchor = m_doc.documentLayout()->anchorAt(e->position());
    setCursor(anchor.isEmpty() ? Qt::IBeamCursor : Qt::PointingHandCursor);
    applyAnchorHover(anchor.startsWith(QLatin1String("opt:")) ? anchor : QString());
}

void TutProseText::leaveEvent(QEvent* e)
{
    applyAnchorHover(QString());
    QWidget::leaveEvent(e);
}

void TutProseText::applyAnchorHover(const QString& href)
{
    if (m_hoverAnchor == href)
        return;
    m_hoverAnchor = href;
    for (QTextBlock block = m_doc.begin(); block != m_doc.end(); block = block.next())
    {
        for (QTextBlock::iterator it = block.begin(); !it.atEnd(); ++it)
        {
            const QTextFragment frag = it.fragment();
            QTextCharFormat fmt = frag.charFormat();
            // Only editable-number anchors: prose hyperlinks keep their own
            // permanent underline from the markup.
            if (!fmt.isAnchor() || !fmt.anchorHref().startsWith(QLatin1String("opt:")))
                continue;
            const bool on = !href.isEmpty() && fmt.anchorHref() == href;
            if (fmt.fontUnderline() == on)
                continue;
            fmt.setFontUnderline(on);
            QTextCursor cursor(&m_doc);
            cursor.setPosition(frag.position());
            cursor.setPosition(frag.position() + frag.length(), QTextCursor::KeepAnchor);
            cursor.setCharFormat(fmt);
        }
    }
    update();
}

void TutProseText::mouseReleaseEvent(QMouseEvent* e)
{
    bool wasSelecting = m_group && m_group->dragging();
    if (m_group)
        m_group->finishDrag();
    // The caret follows the release point without disturbing the selection
    // the drag just made.
    m_caret = qBound(0, hitTest(e->position()), endPosition());
    QAccessibleTextCursorEvent caretEv(this, m_caret);
    QAccessible::updateAccessibility(&caretEv);
    update();
    // Treat as a link click only when nothing was selected (a plain click).
    if (!m_group || !m_group->hasAnySelection())
    {
        QString anchor = m_doc.documentLayout()->anchorAt(e->position());
        if (!anchor.isEmpty() && m_linkHandler)
            m_linkHandler(anchor);
    }
    Q_UNUSED(wasSelecting);
}

void TutProseText::mouseDoubleClickEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton || !m_group)
        return;
    m_group->clearAll();
    int pos = hitTest(e->position());
    m_group->setAnchor(this, pos);
    QTextCursor c(&m_doc);
    c.setPosition(pos);
    c.select(QTextCursor::WordUnderCursor);
    m_selection = c;
    update();
}

void TutProseText::setCaretPosition(int pos, bool keepAnchor)
{
    const int last = endPosition();
    pos = qBound(0, pos, last);
    int anchor = pos;
    if (keepAnchor)
        anchor = m_selection.hasSelection() ? m_selection.anchor() : m_caret;
    if (anchor != pos)
        setSelectionRange(anchor, pos); // fires the selection event itself
    else if (m_selection.hasSelection())
        clearSelection();
    m_caret = pos;
    update();
    QAccessibleTextCursorEvent ev(this, m_caret);
    QAccessible::updateAccessibility(&ev);
    // Entering a link is announced (once per link) so a caret user knows
    // Return will follow it — the keyboard equivalent of the hover cursor.
    const QString link = anchorAtPosition(m_caret);
    if (!link.isEmpty() && link != m_caretAnchor && m_announceHandler)
        m_announceHandler(tr("Link. Press Return to open."));
    m_caretAnchor = link;
    if (m_caretVisibleHandler)
        m_caretVisibleHandler(caretRectGlobal());
}

QString TutProseText::anchorAtPosition(int pos) const
{
    const QTextBlock blk = m_doc.findBlock(qBound(0, pos, endPosition()));
    if (!blk.isValid())
        return QString();
    for (QTextBlock::iterator it = blk.begin(); !it.atEnd(); ++it)
    {
        const QTextFragment frag = it.fragment();
        // Inclusive at the trailing edge so Return works at either end of
        // the link text.
        if (pos < frag.position() || pos > frag.position() + frag.length())
            continue;
        const QTextCharFormat fmt = frag.charFormat();
        if (fmt.isAnchor() && !fmt.anchorHref().isEmpty())
            return fmt.anchorHref();
    }
    return QString();
}

QRect TutProseText::textRectGlobal(int pos, bool thin) const
{
    heightForWidth(width()); // the rect must come from the rendered layout
    pos = qBound(0, pos, endPosition());
    const QTextBlock blk = m_doc.findBlock(pos);
    const int lineH = fontMetrics().height();
    if (!blk.isValid() || !blk.layout())
        return QRect(mapToGlobal(QPoint(0, 0)), QSize(1, lineH));
    const QTextLayout* lay = blk.layout();
    const int rel = pos - blk.position();
    const QTextLine line = lay->lineForTextPosition(qMin(rel, qMax(0, blk.length() - 1)));
    if (!line.isValid())
        return QRect(mapToGlobal(lay->position().toPoint()), QSize(1, lineH));
    const qreal x1 = line.cursorToX(rel);
    const qreal x2 = thin ? x1 + 1 : qMax(x1 + 1, line.cursorToX(rel + 1));
    const QPointF topLeft = lay->position() + QPointF(x1, line.y());
    return QRect(mapToGlobal(topLeft.toPoint()),
                 QSize(qMax(1, qRound(x2 - x1)), qCeil(line.height())));
}

void TutProseText::keyPressEvent(QKeyEvent* e)
{
    if (m_group && e->matches(QKeySequence::Copy))
    {
        m_group->copy();
        e->accept();
        return;
    }

    // Return follows the link under the caret — the keyboard equivalent of
    // clicking it (announced when the caret enters the link).
    if ((e->key() == Qt::Key_Return || e->key() == Qt::Key_Enter) && m_linkHandler)
    {
        const QString href = anchorAtPosition(m_caret);
        if (!href.isEmpty())
        {
            m_linkHandler(href);
            e->accept();
            return;
        }
    }

    // Caret navigation, so the page reads like a browser: arrows step by
    // character/line, word and Home/End moves work, Shift extends the
    // selection. Screen readers echo each step (the cursor events fired by
    // setCaretPosition), which is what lets a user re-read or spell out any
    // part of the docs.
    struct Move
    {
        QKeySequence::StandardKey key;
        QTextCursor::MoveOperation op;
        bool select;
    };
    static const Move moves[] = {
        { QKeySequence::MoveToNextChar, QTextCursor::NextCharacter, false },
        { QKeySequence::MoveToPreviousChar, QTextCursor::PreviousCharacter, false },
        { QKeySequence::MoveToNextWord, QTextCursor::NextWord, false },
        { QKeySequence::MoveToPreviousWord, QTextCursor::PreviousWord, false },
        { QKeySequence::MoveToNextLine, QTextCursor::Down, false },
        { QKeySequence::MoveToPreviousLine, QTextCursor::Up, false },
        { QKeySequence::MoveToStartOfLine, QTextCursor::StartOfLine, false },
        { QKeySequence::MoveToEndOfLine, QTextCursor::EndOfLine, false },
        { QKeySequence::MoveToStartOfDocument, QTextCursor::Start, false },
        { QKeySequence::MoveToEndOfDocument, QTextCursor::End, false },
        { QKeySequence::SelectNextChar, QTextCursor::NextCharacter, true },
        { QKeySequence::SelectPreviousChar, QTextCursor::PreviousCharacter, true },
        { QKeySequence::SelectNextWord, QTextCursor::NextWord, true },
        { QKeySequence::SelectPreviousWord, QTextCursor::PreviousWord, true },
        { QKeySequence::SelectNextLine, QTextCursor::Down, true },
        { QKeySequence::SelectPreviousLine, QTextCursor::Up, true },
        { QKeySequence::SelectStartOfLine, QTextCursor::StartOfLine, true },
        { QKeySequence::SelectEndOfLine, QTextCursor::EndOfLine, true },
    };
    heightForWidth(width()); // line up/down needs the current layout
    for (const Move& m : moves)
    {
        if (!e->matches(m.key))
            continue;
        QTextCursor c(&m_doc);
        c.setPosition(m.select && m_selection.hasSelection() ? m_selection.anchor() : m_caret);
        c.setPosition(m_caret, QTextCursor::KeepAnchor);
        const bool moved =
            c.movePosition(m.op, m.select ? QTextCursor::KeepAnchor : QTextCursor::MoveAnchor);
        if (moved)
        {
            setCaretPosition(c.position(), m.select);
        }
        else if (!m.select && m_caretExitHandler)
        {
            // Stepping off the block's edge: continuous reading hands the
            // caret to the neighbouring block rather than stopping dead.
            if (m.op == QTextCursor::NextCharacter || m.op == QTextCursor::NextWord
                || m.op == QTextCursor::Down)
                m_caretExitHandler(this, +1);
            else if (m.op == QTextCursor::PreviousCharacter
                     || m.op == QTextCursor::PreviousWord || m.op == QTextCursor::Up)
                m_caretExitHandler(this, -1);
        }
        e->accept();
        return;
    }
    QWidget::keyPressEvent(e);
}

// ---- assistive technology ----

namespace
{

// Prose blocks read as read-only text with a full text interface over the
// block's QTextDocument: a screen reader can follow the caret, step by
// character/word/line, query arbitrary ranges and track the selection —
// everything the QTextBrowser this pane replaced provided for free. The
// role is EditableText (with the readOnly state) because that is what the
// platform bridges map to a native text area, which is the shape assistive
// technology expects caret navigation from.
class TutProseTextAccessible : public QAccessibleWidget, public QAccessibleTextInterface
{
public:
    explicit TutProseTextAccessible(QWidget* w)
        : QAccessibleWidget(w, QAccessible::EditableText)
    {
    }

    void* interface_cast(QAccessible::InterfaceType t) override
    {
        if (t == QAccessible::TextInterface)
            return static_cast<QAccessibleTextInterface*>(this);
        return QAccessibleWidget::interface_cast(t);
    }

    QAccessible::State state() const override
    {
        QAccessible::State st = QAccessibleWidget::state();
        st.readOnly = true;
        st.selectableText = true;
        st.multiLine = true;
        return st;
    }

    QString text(QAccessible::Text t) const override
    {
        if (t == QAccessible::Value)
            return laidOut().text;
        return QAccessibleWidget::text(t);
    }

    // --- QAccessibleTextInterface ---
    // Offsets on this interface index the LAID-OUT text (see laidOut()), which
    // is the document plus a newline at every soft wrap. Document positions
    // and accessible offsets are therefore no longer the same number, so each
    // entry point converts at the boundary.
    void selection(int selectionIndex, int* startOffset, int* endOffset) const override
    {
        const bool has = selectionIndex == 0 && prose()->selectionStart() >= 0;
        const LaidOut lo = laidOut();
        *startOffset = has ? docToAx(prose()->selectionStart(), lo) : 0;
        *endOffset = has ? docToAx(prose()->selectionEnd(), lo) : 0;
    }

    int selectionCount() const override { return prose()->selectionStart() >= 0 ? 1 : 0; }

    void addSelection(int startOffset, int endOffset) override
    {
        const LaidOut lo = laidOut();
        prose()->setSelectionRange(axToDoc(startOffset, lo), axToDoc(endOffset, lo));
    }

    void removeSelection(int) override { prose()->clearSelection(); }

    void setSelection(int, int startOffset, int endOffset) override
    {
        const LaidOut lo = laidOut();
        prose()->setSelectionRange(axToDoc(startOffset, lo), axToDoc(endOffset, lo));
    }

    int cursorPosition() const override
    {
        return docToAx(prose()->caretPosition(), laidOut());
    }

    void setCursorPosition(int position) override
    {
        prose()->setCaretPosition(axToDoc(position, laidOut()));
    }

    QString text(int startOffset, int endOffset) const override
    {
        const QString all = laidOut().text;
        const int s = qBound(0, startOffset, all.size());
        const int e = qBound(s, endOffset, all.size());
        return all.mid(s, e - s);
    }

    int characterCount() const override { return laidOut().text.size(); }

    QRect characterRect(int offset) const override
    {
        return prose()->characterRectGlobal(axToDoc(offset, laidOut()));
    }

    int offsetAtPoint(const QPoint& point) const override
    {
        const QPoint local = prose()->mapFromGlobal(point);
        const int hit =
            prose()->document()->documentLayout()->hitTest(QPointF(local), Qt::ExactHit);
        return hit < 0 ? -1 : docToAx(hit, laidOut());
    }

    void scrollToSubstring(int startIndex, int) override
    {
        prose()->ensureOffsetVisible(axToDoc(startIndex, laidOut()));
    }

    QString attributes(int offset, int* startOffset, int* endOffset) const override
    {
        // One uniform run: the docs carry no per-character semantics that
        // assistive technology needs beyond the text itself.
        Q_UNUSED(offset);
        *startOffset = 0;
        *endOffset = characterCount();
        return QString();
    }

private:
    // --- laid-out text -----------------------------------------------------
    // Every platform bridge decides where a line ends by looking for '\n' in
    // the text this interface hands it. macOS counts them for AXLineForIndex
    // and AXInsertionPointLineNumber (while taking the line's *range* from
    // LineBoundary, so the two halves disagree); Windows UIA makes '\n' the
    // only TextUnit_Line separator and never consults the layout at all. A
    // word-wrapped paragraph holds no newline, so the caret is reported on
    // line 0 wherever it sits and a screen reader re-speaks that same line on
    // every arrow press — the bug the docs pane shipped with.
    //
    // Rather than work around each bridge in its own way (and macOS is the
    // only one that even could be), expose the text AS LAID OUT: a real '\n'
    // at every soft wrap. Line numbering and line boundaries then agree on
    // all platforms, and the inherited boundary implementations become
    // correct without overriding them. Code blocks are unaffected: their
    // newlines are already real, so nothing is inserted.
    struct LaidOut
    {
        QString text;      // document text, '\n' inserted at each soft wrap
        QList<int> wraps;  // document offsets that a soft-wrapped line starts at
    };

    // Document offsets index plainText() one-for-one (block separators and
    // U+2028/U+00A0 all normalise to a single character), which is what lets
    // the wrap offsets below double as insertion points.
    LaidOut laidOut() const
    {
        LaidOut lo;
        lo.text = prose()->plainText();
        prose()->heightForWidth(prose()->width()); // ensure the layout is current
        QTextDocument* doc = prose()->document();
        for (QTextBlock b = doc->begin(); b.isValid(); b = b.next())
        {
            const QTextLayout* lay = b.layout();
            if (!lay)
                continue;
            // Line 0 starts the block, so only lines 1.. are soft wraps.
            for (int i = 1; i < lay->lineCount(); ++i)
            {
                const QTextLine ln = lay->lineAt(i);
                if (!ln.isValid())
                    continue;
                const int at = b.position() + ln.textStart();
                if (at <= 0 || at > lo.text.size())
                    continue;
                // A <br> starts a new QTextLine too, but it already carries a
                // real newline — inserting there would speak a blank line
                // between every line of a code block.
                if (lo.text.at(at - 1) == QLatin1Char('\n'))
                    continue;
                lo.wraps << at;
            }
        }
        std::sort(lo.wraps.begin(), lo.wraps.end());
        for (int i = lo.wraps.size() - 1; i >= 0; --i)
            lo.text.insert(lo.wraps[i], QLatin1Char('\n'));
        return lo;
    }

    int docToAx(int docOffset, const LaidOut& lo) const
    {
        int shift = 0;
        for (int w : lo.wraps)
            if (w <= docOffset)
                ++shift;
        return docOffset + shift;
    }

    int axToDoc(int axOffset, const LaidOut& lo) const
    {
        int shift = 0;
        for (int i = 0; i < lo.wraps.size(); ++i)
        {
            // The i-th inserted newline sits at this accessible offset.
            if (axOffset <= lo.wraps[i] + i)
                break;
            ++shift;
        }
        return qMax(0, axOffset - shift);
    }

    TutProseText* prose() const { return static_cast<TutProseText*>(widget()); }
};

// Dials expose the standard value interface (current/min/max/step) so screen
// readers can read and adjust them like native sliders. The role is Slider,
// not Dial: platform bridges map sliders everywhere, whereas the Dial role
// has no macOS mapping and VoiceOver skips the element entirely — a silent
// knob.
class TutDialAccessible : public QAccessibleWidget, public QAccessibleValueInterface
{
public:
    explicit TutDialAccessible(QWidget* w)
        : QAccessibleWidget(w, QAccessible::Slider)
    {
    }

    void* interface_cast(QAccessible::InterfaceType t) override
    {
        if (t == QAccessible::ValueInterface)
            return static_cast<QAccessibleValueInterface*>(this);
        return QAccessibleWidget::interface_cast(t);
    }

    QString text(QAccessible::Text t) const override
    {
        TutDial* dial = static_cast<TutDial*>(widget());
        if (t == QAccessible::Name)
            return dial->optName();
        if (t == QAccessible::Value)
            return dial->valueText();
        return QAccessibleWidget::text(t);
    }

    QVariant currentValue() const override
    {
        return static_cast<TutDial*>(widget())->value();
    }

    void setCurrentValue(const QVariant& value) override
    {
        static_cast<TutDial*>(widget())->setValue(value.toDouble());
    }

    QVariant maximumValue() const override
    {
        return static_cast<TutDial*>(widget())->maximum();
    }

    QVariant minimumValue() const override
    {
        return static_cast<TutDial*>(widget())->minimum();
    }

    QVariant minimumStepSize() const override
    {
        return static_cast<TutDial*>(widget())->step();
    }
};

// Chapter/section titles read as headings, so a screen reader's rotor can
// jump section by section instead of walking every element — the docs
// equivalent of skimming. The heading level rides in the Value text (how
// the platform bridges expose it).
class TutHeadingAccessible : public QAccessibleWidget
{
public:
    explicit TutHeadingAccessible(QWidget* w)
        : QAccessibleWidget(w, QAccessible::Heading)
    {
    }

    QString text(QAccessible::Text t) const override
    {
        if (t == QAccessible::Name)
            return static_cast<TutHeading*>(widget())->text();
        if (t == QAccessible::Value)
            return QString::number(static_cast<TutHeading*>(widget())->headingLevel());
        return QAccessibleWidget::text(t);
    }
};

QAccessibleInterface* tutorialWidgetAccessibleFactory(const QString& className, QObject* object)
{
    QWidget* widget = qobject_cast<QWidget*>(object);
    if (!widget)
        return nullptr;
    if (className == QLatin1String("TutProseText"))
        return new TutProseTextAccessible(widget);
    if (className == QLatin1String("TutDial"))
        return new TutDialAccessible(widget);
    if (className == QLatin1String("TutHeading"))
        return new TutHeadingAccessible(widget);
    return nullptr;
}

} // namespace

void registerTutorialWidgetAccessibility()
{
    static bool installed = false;
    if (installed)
        return;
    installed = true;
    QAccessible::installFactory(tutorialWidgetAccessibleFactory);
}
