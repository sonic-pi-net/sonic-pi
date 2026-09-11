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

// The docs pane's custom widgets: cached-layout rich text with cross-block
// selection, a rotary opt dial, and the QWERTY-labelled piano. Kept free of
// QScintilla/theme dependencies so gui-tests can compile them, and each is
// exposed to assistive technology (see registerTutorialWidgetAccessibility).

#pragma once

#include "dpi.h"
#include "model/sonicpitheme.h"

#include <QAbstractTextDocumentLayout>
#include <QAccessible>
#include <QApplication>
#include <QClipboard>
#include <QColor>
#include <QHash>
#include <QLabel>
#include <QLineEdit>
#include <QLocale>
#include <QMouseEvent>
#include <QRegularExpression>
#include <QPainter>
#include <QPainterPath>
#include <QScrollArea>
#include <QScrollBar>
#include <QTextBlock>
#include <QTextCursor>
#include <QTextDocument>
#include <QTimer>
#include <QWidget>
#include <QtMath>

#include <cmath>
#include <functional>
#include <memory>

class TutSelectionGroup;

// Shared numeric parse for the inline value editors (the dial's own editor
// and the code-anchor editor): locale decimals first ("0,5" on de/fr), then
// the C form ("0.5"), so both editors accept the same input.
inline bool parseOptValue(const QString& text, double* out)
{
    bool ok = false;
    double v = QLocale().toDouble(text, &ok);
    if (!ok)
        v = text.toDouble(&ok);
    if (ok)
        *out = v;
    return ok;
}

// Wrapped rich-text display that caches its text layout per width. QLabel
// re-lays-out its document on every heightForWidth call, which made long
// chapters (~30 wrapped labels x several layout passes) take ~300ms to
// show; this makes those calls O(1). Selection (drag, double-click, copy)
// is coordinated by a shared TutSelectionGroup so a drag can span several
// blocks. Links via callback so it needs no moc.
//
// Keyboard: the block carries a real text caret (arrows, word steps,
// Home/End, Shift-selection) so the docs read like a web page rather than
// one opaque label per paragraph — a screen reader tracks the caret and can
// re-read or spell out at any granularity. Stepping off either end hands
// the caret to the neighbouring block (see setCaretExitHandler), so a whole
// chapter reads as one continuous document.
class TutProseText : public QWidget
{
    Q_OBJECT
public:
    explicit TutProseText(QWidget* parent)
        : QWidget(parent)
    {
        m_doc.setDocumentMargin(0);
        m_doc.setDefaultFont(font());
        QSizePolicy policy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        policy.setHeightForWidth(true);
        setSizePolicy(policy);
        setMouseTracking(true);
        setFocusPolicy(Qt::StrongFocus); // Tab and click both land the caret
        setCursor(Qt::IBeamCursor);
    }

    ~TutProseText() override;

    void setGroup(const std::shared_ptr<TutSelectionGroup>& group) { m_group = group; }

    void setHtml(const QString& html)
    {
        m_doc.setHtml(html);
        m_selection = QTextCursor();
        m_caret = 0;
        m_cachedWidth = -1;
        updateGeometry();
        update();
    }

    // Plain text of the whole block, for assistive technology. Normalised:
    // toPlainText() keeps U+2028 line separators (from <br>) which screen
    // readers speak as junk glyphs, so all separators become real newlines.
    QString plainText() const
    {
        QString text = m_doc.toPlainText();
        text.replace(QChar::LineSeparator, QLatin1Char('\n'));
        text.replace(QChar::ParagraphSeparator, QLatin1Char('\n'));
        text.replace(QChar(0xA0), QLatin1Char(' '));
        return text;
    }

    // The block's document, for the accessible text interface (offsets there
    // are document positions, so both sides index the same space).
    QTextDocument* document() const { return &m_doc; }

    QString selectedText() const
    {
        QString text = m_selection.selectedText();
        text.replace(QChar::ParagraphSeparator, '\n');
        text.replace(QChar::LineSeparator, '\n');
        text.replace(QChar(0xA0), ' ');
        return text;
    }

    void setLinkHandler(std::function<void(const QString&)> handler)
    {
        m_linkHandler = std::move(handler);
    }

    // Called when the caret steps off the block's start (-1) or end (+1), so
    // the owning pane can hand the caret to the neighbouring block and the
    // page reads as one continuous document.
    void setCaretExitHandler(std::function<void(TutProseText*, int)> handler)
    {
        m_caretExitHandler = std::move(handler);
    }

    // Called with the caret's global rect whenever it moves, so the owning
    // scroll area can keep it on screen.
    void setCaretVisibleHandler(std::function<void(const QRect&)> handler)
    {
        m_caretVisibleHandler = std::move(handler);
    }

    // Spoken feedback (e.g. "Link. Press Return to open.") relayed by the
    // owning pane to the screen reader.
    void setAnnounceHandler(std::function<void(const QString&)> handler)
    {
        m_announceHandler = std::move(handler);
    }

    // The anchor href at a document position (empty when none) — the caret
    // equivalent of hovering a link.
    QString anchorAtPosition(int pos) const;

    // --- keyboard caret (document positions; see keyPressEvent) ---
    int caretPosition() const { return m_caret; }
    // Places the caret (clamped), extends or clears the selection, notifies
    // assistive technology, and asks the pane to keep the caret visible.
    void setCaretPosition(int pos, bool keepAnchor = false);
    // Global-coordinate caret rect at the current position (line height).
    QRect caretRectGlobal() const { return textRectGlobal(m_caret, true); }
    // Global-coordinate cell of one character (the accessible text
    // interface's characterRect).
    QRect characterRectGlobal(int pos) const { return textRectGlobal(pos, false); }
    // Ask the pane to scroll this document position on screen.
    void ensureOffsetVisible(int pos)
    {
        if (m_caretVisibleHandler)
            m_caretVisibleHandler(characterRectGlobal(pos));
    }
    // Selection bounds in document positions (-1/-1 when nothing is selected).
    int selectionStart() const
    {
        return m_selection.hasSelection() ? m_selection.selectionStart() : -1;
    }
    int selectionEnd() const
    {
        return m_selection.hasSelection() ? m_selection.selectionEnd() : -1;
    }

    bool hasHeightForWidth() const override { return true; }

    int heightForWidth(int width) const override
    {
        // Clamp to maximumWidth: layouts ask at the full cell width, but the
        // widget renders no wider than its cap (the prose readable-measure) —
        // heights must be computed at the width actually rendered.
        width = qMin(width, maximumWidth());
        if (width < 1)
            return 0;
        if (width != m_cachedWidth)
        {
            m_doc.setTextWidth(width);
            m_cachedHeight = qCeil(m_doc.size().height());
            m_cachedWidth = width;
        }
        return m_cachedHeight;
    }

    QSize sizeHint() const override
    {
        int w = width() > 0 ? width() : ScaleWidthForDPI(400);
        return QSize(w, heightForWidth(w));
    }

    QSize minimumSizeHint() const override { return QSize(0, 0); }


    // --- selection API used by TutSelectionGroup ---
    bool hasSelection() const { return m_selection.hasSelection(); }

    void clearSelection()
    {
        if (m_selection.hasSelection())
        {
            m_selection.clearSelection();
            update();
        }
    }

    int endPosition() const
    {
        QTextCursor c(&m_doc);
        c.movePosition(QTextCursor::End);
        return c.position();
    }

    void setSelectionRange(int anchor, int pos)
    {
        int last = endPosition();
        QTextCursor c(&m_doc);
        c.setPosition(qBound(0, anchor, last));
        c.setPosition(qBound(0, pos, last), QTextCursor::KeepAnchor);
        m_selection = c;
        update();
        // Screen readers track the selection as it changes (mouse drags
        // included), the same as a native text view.
        QAccessibleTextSelectionEvent ev(this, m_selection.selectionStart(),
                                         m_selection.selectionEnd());
        QAccessible::updateAccessibility(&ev);
    }

    // Character position under a global-coordinate point, clamped to the
    // block's start/end when the point is above/below it (so a drag that has
    // left this block selects it fully up to the edge).
    int charAtGlobal(const QPoint& globalPos) const
    {
        QPoint local = mapFromGlobal(globalPos);
        if (local.y() <= 0)
            return 0;
        if (local.y() >= height())
            return endPosition();
        qreal x = qBound(0, local.x(), width());
        return m_doc.documentLayout()->hitTest(QPointF(x, local.y()), Qt::FuzzyHit);
    }

    int spanTop() const { return mapToGlobal(QPoint(0, 0)).y(); }
    int spanBottom() const { return mapToGlobal(QPoint(0, height())).y(); }

    int verticalDistanceTo(int globalY) const
    {
        if (globalY < spanTop())
            return spanTop() - globalY;
        if (globalY > spanBottom())
            return globalY - spanBottom();
        return 0;
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        heightForWidth(width());
        QPainter p(this);
        QAbstractTextDocumentLayout::PaintContext ctx;
        ctx.palette = palette();
        ctx.palette.setColor(QPalette::Text, palette().color(QPalette::WindowText));
        // The caret only shows while the block holds focus, so a page of
        // blocks never shows more than one.
        if (hasFocus())
            ctx.cursorPosition = m_caret;
        if (m_selection.hasSelection())
        {
            QAbstractTextDocumentLayout::Selection sel;
            sel.cursor = m_selection;
            sel.format.setBackground(palette().highlight());
            sel.format.setForeground(palette().highlightedText());
            ctx.selections.append(sel);
        }
        m_doc.documentLayout()->draw(&p, ctx);
    }

    void focusInEvent(QFocusEvent* e) override
    {
        QWidget::focusInEvent(e);
        update(); // show the caret
    }

    void focusOutEvent(QFocusEvent* e) override
    {
        QWidget::focusOutEvent(e);
        update(); // hide the caret
    }

    void resizeEvent(QResizeEvent*) override
    {
        heightForWidth(width());
        update();
    }

    void changeEvent(QEvent* e) override
    {
        if (e->type() == QEvent::FontChange)
        {
            m_doc.setDefaultFont(font());
            m_cachedWidth = -1;
            updateGeometry();
            update();
        }
        QWidget::changeEvent(e);
    }

    void mousePressEvent(QMouseEvent* e) override;
    void mouseMoveEvent(QMouseEvent* e) override;
    void mouseReleaseEvent(QMouseEvent* e) override;
    void mouseDoubleClickEvent(QMouseEvent* e) override;
    void keyPressEvent(QKeyEvent* e) override;
    void leaveEvent(QEvent* e) override;

private:
    int hitTest(const QPointF& pos) const
    {
        return m_doc.documentLayout()->hitTest(pos, Qt::FuzzyHit);
    }

    // Global rect of the character cell (or thin caret line) at `pos`.
    QRect textRectGlobal(int pos, bool thin) const;

    // Underline the hovered editable-number anchor (opt:*) — rich text has
    // no :hover, so the underline is toggled on the fragment's char format.
    void applyAnchorHover(const QString& href);

    mutable QTextDocument m_doc;
    mutable int m_cachedWidth = -1;
    mutable int m_cachedHeight = 0;
    QTextCursor m_selection;
    QString m_hoverAnchor;
    int m_caret = 0;        // keyboard caret, as a document position
    QString m_caretAnchor;  // anchor under the caret (announce on entry)
    std::shared_ptr<TutSelectionGroup> m_group;
    std::function<void(const QString&)> m_linkHandler;
    std::function<void(TutProseText*, int)> m_caretExitHandler;
    std::function<void(const QRect&)> m_caretVisibleHandler;
    std::function<void(const QString&)> m_announceHandler;
};

// A section title. Its own class (rather than a bare QLabel) so the
// accessible factory can key on the class name and expose it with the
// Heading role and its level — which is what feeds a screen reader's
// heading navigation (rotor / H key). Styling still keys on objectName
// (tutH1/tutH2/tutH3, qsCardTitle) as before.
class TutHeading : public QLabel
{
    Q_OBJECT
public:
    explicit TutHeading(const QString& text, int level, QWidget* parent = nullptr)
        : QLabel(text, parent)
        , m_level(level)
    {
    }

    int headingLevel() const { return m_level; }

private:
    int m_level = 1;
};

// Coordinates click-drag text selection across the stack of per-block text
// widgets, so a drag can span multiple paragraphs. Members register on
// construction; a drag maps the cursor to the block/character under it and
// sets each block's range: the anchor block's tail, whole blocks in between,
// and the target block's head. Copy concatenates them in visual order.
class TutSelectionGroup
{
public:
    void add(TutProseText* w) { m_members.append(w); }

    void remove(TutProseText* w)
    {
        m_members.removeAll(w);
        if (m_anchor == w)
        {
            m_anchor = nullptr;
            m_dragging = false;
        }
    }

    void clearAll()
    {
        for (TutProseText* w : m_members)
            w->clearSelection();
        m_anchor = nullptr;
        m_dragging = false;
    }

    void beginDrag(TutProseText* w, int pos)
    {
        clearAll();
        m_anchor = w;
        m_anchorPos = pos;
        m_dragging = true;
        w->setSelectionRange(pos, pos); // empty until the drag extends it
    }

    void setAnchor(TutProseText* w, int pos)
    {
        m_anchor = w;
        m_anchorPos = pos;
        m_dragging = false;
    }

    bool dragging() const { return m_dragging; }

    void finishDrag()
    {
        m_dragging = false;
        stopAutoScroll();
    }

    void setScrollArea(QScrollArea* scroll) { m_scroll = scroll; }

    bool hasAnySelection() const
    {
        for (TutProseText* w : m_members)
            if (w->hasSelection())
                return true;
        return false;
    }

    void extendTo(const QPoint& globalPos)
    {
        m_lastGlobalPos = globalPos;
        applySelectionAt(globalPos);
        updateAutoScroll(globalPos);
    }

    ~TutSelectionGroup()
    {
        if (m_scrollTimer)
        {
            m_scrollTimer->stop();
            delete m_scrollTimer;
        }
    }

    void applySelectionAt(const QPoint& globalPos)
    {
        if (!m_dragging || !m_anchor)
            return;
        QList<TutProseText*> ord = ordered();
        int ai = ord.indexOf(m_anchor);
        if (ai < 0)
            return;

        // Target block: the one under the cursor, else the nearest by
        // vertical distance (cursor in a gap or over a non-text block).
        int ti = 0;
        int best = -1;
        for (int i = 0; i < ord.size(); ++i)
        {
            int d = ord[i]->verticalDistanceTo(globalPos.y());
            if (best < 0 || d < best)
            {
                best = d;
                ti = i;
            }
        }
        int targetPos = ord[ti]->charAtGlobal(globalPos);

        int lo = qMin(ai, ti);
        int hi = qMax(ai, ti);
        for (int i = 0; i < ord.size(); ++i)
        {
            TutProseText* w = ord[i];
            if (i < lo || i > hi)
            {
                w->clearSelection();
            }
            else if (ai == ti)
            {
                w->setSelectionRange(m_anchorPos, targetPos);
            }
            else if (i > lo && i < hi)
            {
                w->setSelectionRange(0, w->endPosition());
            }
            else if (w == m_anchor)
            {
                w->setSelectionRange(m_anchorPos, ai < ti ? w->endPosition() : 0);
            }
            else // target block, distinct from the anchor
            {
                w->setSelectionRange(ti > ai ? 0 : w->endPosition(), targetPos);
            }
        }
    }

    QString selectedText() const
    {
        QStringList parts;
        for (TutProseText* w : ordered())
            if (w->hasSelection())
                parts << w->selectedText();
        return parts.join('\n');
    }

    void copy() const
    {
        QString text = selectedText();
        if (!text.isEmpty())
            QApplication::clipboard()->setText(text);
    }

private:
    QList<TutProseText*> ordered() const
    {
        QList<TutProseText*> ord = m_members;
        std::sort(ord.begin(), ord.end(), [](TutProseText* a, TutProseText* b) {
            return a->spanTop() < b->spanTop();
        });
        return ord;
    }

    // When the cursor is held within a margin of the viewport's top or bottom
    // edge during a drag, creep the scroll position so the selection can run
    // past the visible area. Speed ramps gently with depth into the margin and
    // stays deliberately slow.
    void updateAutoScroll(const QPoint& globalPos)
    {
        if (!m_scroll)
            return;
        QWidget* vp = m_scroll->viewport();
        int margin = ScaleHeightForDPI(32);
        int top = vp->mapToGlobal(QPoint(0, 0)).y();
        int bottom = top + vp->height();
        int maxStep = ScaleHeightForDPI(5); // px per 30ms tick (~165 px/s cap)

        int delta = 0;
        if (globalPos.y() < top + margin)
        {
            int depth = (top + margin) - qMax(globalPos.y(), top);
            delta = -stepForDepth(depth, margin, maxStep);
        }
        else if (globalPos.y() > bottom - margin)
        {
            int depth = qMin(globalPos.y(), bottom) - (bottom - margin);
            delta = stepForDepth(depth, margin, maxStep);
        }

        if (delta == 0)
        {
            stopAutoScroll();
            return;
        }
        m_scrollDelta = delta;
        if (!m_scrollTimer)
        {
            m_scrollTimer = new QTimer();
            m_scrollTimer->setInterval(30);
            QObject::connect(m_scrollTimer, &QTimer::timeout, [this]() { autoScrollTick(); });
        }
        if (!m_scrollTimer->isActive())
            m_scrollTimer->start();
    }

    static int stepForDepth(int depth, int margin, int maxStep)
    {
        double f = qBound(0.0, double(depth) / margin, 1.0);
        return qMax(1, int(qRound(f * maxStep)));
    }

    void stopAutoScroll()
    {
        m_scrollDelta = 0;
        if (m_scrollTimer)
            m_scrollTimer->stop();
    }

    void autoScrollTick()
    {
        if (!m_dragging || !m_scroll)
        {
            stopAutoScroll();
            return;
        }
        QScrollBar* vbar = m_scroll->verticalScrollBar();
        vbar->setValue(vbar->value() + m_scrollDelta);
        applySelectionAt(m_lastGlobalPos);
    }

    QList<TutProseText*> m_members;
    TutProseText* m_anchor = nullptr;
    int m_anchorPos = 0;
    bool m_dragging = false;
    QScrollArea* m_scroll = nullptr;
    QTimer* m_scrollTimer = nullptr;
    QPoint m_lastGlobalPos;
    int m_scrollDelta = 0;
};


// A small clickable piano labelled with the QWERTY keys that trigger each
// note (tracker layout: bottom row = white keys, top row = blacks), so the
// keyboard-playing feature is discoverable. flash() lights a key when its
// note sounds. Callback-based so it needs no moc.
class TutPiano : public QWidget
{
    Q_OBJECT
public:
    explicit TutPiano(std::function<void(int)> onKey, QWidget* parent)
        : QWidget(parent)
        , m_onKey(std::move(onKey))
    {
        applyScale();
        setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        setCursor(Qt::PointingHandCursor);
        setAccessibleName(tr("Piano keyboard — play with your computer keys"));
    }

    // The docs pane's text zoom. Painted like the dials, so the keys and
    // their QWERTY labels have to be scaled explicitly.
    void setUiScale(double scale)
    {
        if (qFuzzyCompare(m_scale, scale))
            return;
        m_scale = scale;
        applyScale();
        update();
    }


    void setColours(const QColor& fg, const QColor& bg, const QColor& accent, const QColor& muted)
    {
        m_fg = fg;
        m_bg = bg;
        m_accent = accent;
        m_muted = muted;
        update();
    }

private:
    void applyScale()
    {
        auto px = [this](int base, bool vertical) {
            const int dpi = vertical ? ScaleHeightForDPI(base) : ScaleWidthForDPI(base);
            return qMax(1, qRound(dpi * m_scale));
        };
        m_whiteW = px(32, false);
        m_whiteH = px(72, true);
        m_blackW = px(20, false);
        m_blackH = px(42, true);
        // Width-adaptive: at least the 9 QWERTY-labelled whites, growing to
        // fill whatever row width is available with more octaves. Capped at
        // keys()' 30-white clamp so the widget never outgrows the drawn
        // board and whatever sits beside it in the row stays adjacent.
        setMinimumSize(m_whiteW * 9 + 2, m_whiteH + 2);
        setMaximumWidth(m_whiteW * 30 + m_blackW / 2 + 2);
        setFixedHeight(m_whiteH + 2);
    }

public:

    // Lights the key for `ms`; callers pass the note's release time so the
    // key stays lit for as long as the note sounds. Polyphonic: each key
    // clears on its own timer, and the generation count stops an earlier
    // press's timer from dousing a retrigger's fresh light.
    void flash(int offset, int ms = 180)
    {
        const int gen = ++m_lit[offset];
        update();
        QTimer::singleShot(ms, this, [this, offset, gen]() {
            auto it = m_lit.find(offset);
            if (it != m_lit.end() && it.value() == gen)
            {
                m_lit.erase(it);
                update();
            }
        });
    }

public:
    // Test hooks: the QWERTY labels actually drawn at the current width, the
    // white-key count, and how far the rightmost key overhangs the widget.
    // Pin the labelled window against clipping (see stylesheet/zoom tests).
    QString qwertyLabelsForTest() const
    {
        QString out;
        for (const Key& k : keys())
            if (!k.label.isNull())
                out += k.label;
        return out;
    }
    int whiteCountForTest() const
    {
        int n = 0;
        for (const Key& k : keys())
            if (!k.black)
                n++;
        return n;
    }
    int rightOverflowForTest() const
    {
        double right = 0;
        for (const Key& k : keys())
            right = qMax(right, k.rect.right());
        return int(right) - (width() - 1);
    }

protected:
    // White keys: a s d f g h j k l; blacks w e t y u o p in the gaps
    struct Key
    {
        QChar label;
        int offset;
        QRectF rect;
        bool black;
    };

    QVector<Key> keys() const
    {
        // Whites walk the major-scale offsets from the keyboard's left edge;
        // a black key sits after every white except the E–F and B–C
        // boundaries. QWERTY labels are printed on the leftmost octave and
        // never move — the octave shift transposes the notes under them
        // (tracker layout: bottom row whites, top row blacks).
        static const int kMajor[] = { 0, 2, 4, 5, 7, 9, 11 };
        static const char kWhiteQwerty[] = { 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' };
        auto blackQwerty = [](int relWhite) -> char {
            switch (relWhite)
            {
            case 0: return 'w';
            case 1: return 'e';
            case 3: return 't';
            case 4: return 'y';
            case 5: return 'u';
            case 7: return 'o';
            case 8: return 'p';
            default: return 0;
            }
        };
        auto whiteOffset = [](int i) { return 12 * (i / 7) + kMajor[i % 7]; };

        const int avail = qMax(1, width() - 2);
        const int raw = avail / m_whiteW;
        const int whites = qBound(9, raw, 30);
        // The last white's black note straddles its right edge, so the board
        // needs half a black key of headroom for it. Reserve that before
        // dividing up the width — dropping the key instead would delete 'p'
        // from the QWERTY row exactly at the 9-key minimum, leaving a
        // keystroke that sounds a note with no key to show for it.
        const int lastStep = (whites - 1) % 7;
        const bool trailingBlack = !(lastStep == 2 || lastStep == 6); // none above E or B
        const double reserve = trailingBlack ? m_blackW / 2.0 : 0.0;
        // Spread the sub-key remainder across the whites so the board spans its
        // full width instead of stopping up to one key short of the edge. Only
        // when the count is unclamped — at the 9/30 stops the natural key width
        // wins, so a very narrow row doesn't squash and a very wide one doesn't
        // inflate the keys to fill it.
        const double whiteW = (raw >= 9 && raw <= 30) ? (double(avail) - reserve) / whites
                                                      : double(m_whiteW);
        QVector<Key> out;
        for (int i = 0; i < whites; i++)
        {
            const QChar label = i < 9 ? QChar::fromLatin1(kWhiteQwerty[i]) : QChar();
            out.append({ label, whiteOffset(i),
                         QRectF(1 + i * whiteW, 1, whiteW - 1, m_whiteH), false });
        }
        for (int i = 0; i < whites; i++)
        {
            const int step = i % 7;
            if (step == 2 || step == 6) // no black above E or B
                continue;
            const char qwerty = blackQwerty(i);
            out.append({ qwerty ? QChar::fromLatin1(qwerty) : QChar(), whiteOffset(i) + 1,
                         QRectF(1 + (i + 1) * whiteW - m_blackW / 2.0, 1,
                                m_blackW, m_blackH), true });
        }
        return out;
    }

    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        // Monospace labels read as terminal glyphs, matching the code blocks.
        QFont f = font();
        f.setFamily(QStringLiteral("Hack"));
        SetFontSizeValue(f, FontSizeValue(font()) * 0.7 * m_scale, 6.5);
        p.setFont(f);
        const int whiteInset = qMax(1, qRound(ScaleHeightForDPI(4) * m_scale));
        const int blackInset = qMax(1, qRound(ScaleHeightForDPI(3) * m_scale));

        // Tron-style: translucent key bodies traced with neutral light-lines —
        // dim traces on the whites, brighter on the blacks. Colour is reserved
        // for the struck key, which floods with the accent and casts a glow.
        const bool darkGround = m_bg.lightness() < 128;
        const QColor traceDim = SonicPiTheme::blend(m_bg, m_fg, 0.3);
        const QColor traceHot = SonicPiTheme::blend(m_bg, m_fg, 0.6);
        const QColor whiteFill = SonicPiTheme::blend(m_bg, m_fg, 0.05);
        const QColor blackFill = SonicPiTheme::blend(m_bg, m_fg, darkGround ? 0.22 : 0.75);
        const QColor whiteLabel = SonicPiTheme::blend(m_bg, m_fg, 0.55);
        // Black-key labels must contrast the key FILL (always darker than the
        // ground in light mode): near-ground light text on both grounds.
        const QColor blackLabel = darkGround ? SonicPiTheme::blend(m_bg, m_fg, 0.8)
                                             : SonicPiTheme::blend(m_bg, m_fg, 0.1);

        auto glow = [&p, this](const QRectF& r) {
            p.setBrush(Qt::NoBrush);
            for (int i = 1; i <= 3; i++)
            {
                QColor halo = m_accent;
                halo.setAlpha(70 - i * 18);
                p.setPen(QPen(halo, i * 2.0));
                p.drawRoundedRect(r, 1, 1);
            }
        };

        const QVector<Key> ks = keys();
        for (const Key& k : ks) // whites first (list order)
        {
            if (k.black)
                continue;
            bool lit = m_lit.contains(k.offset);
            p.setPen(QPen(lit ? m_accent : traceDim, 1));
            p.setBrush(lit ? m_accent : whiteFill);
            p.drawRoundedRect(k.rect, 1, 1);
            if (lit)
                glow(k.rect);
            if (k.label.isNull())
                continue;   // keys beyond the QWERTY window are unlabelled
            p.setPen(lit ? m_bg : whiteLabel);
            p.drawText(k.rect.adjusted(0, 0, 0, -whiteInset),
                       Qt::AlignHCenter | Qt::AlignBottom, QString(k.label));
        }
        for (const Key& k : ks)
        {
            if (!k.black)
                continue;
            bool lit = m_lit.contains(k.offset);
            p.setPen(QPen(lit ? m_accent : traceHot, 1));
            p.setBrush(lit ? m_accent : blackFill);
            p.drawRoundedRect(k.rect, 1, 1);
            if (lit)
                glow(k.rect);
            if (k.label.isNull())
                continue;
            p.setPen(lit ? m_bg : blackLabel);
            p.drawText(k.rect.adjusted(0, 0, 0, -blackInset),
                       Qt::AlignHCenter | Qt::AlignBottom, QString(k.label));
        }
    }

    void mousePressEvent(QMouseEvent* e) override
    {
        m_dragOffset = -1; // re-pressing the same key retriggers it
        pressAt(e->position());
    }

    // Held drags sweep the board: every key the pointer crosses sounds once.
    void mouseMoveEvent(QMouseEvent* e) override
    {
        if (e->buttons() & Qt::LeftButton)
            pressAt(e->position());
    }

    void pressAt(const QPointF& pos)
    {
        const QVector<Key> ks = keys();
        // Blacks hit-test first — they sit on top
        for (int pass = 0; pass < 2; pass++)
            for (const Key& k : ks)
                if (k.black == (pass == 0) && k.rect.contains(pos))
                {
                    if (k.offset != m_dragOffset)
                    {
                        m_dragOffset = k.offset;
                        if (m_onKey)
                            m_onKey(k.offset);
                    }
                    return;
                }
    }

private:
    std::function<void(int)> m_onKey;
    int m_dragOffset = -1; // key sounding under a held drag, -1 outside one
    double m_scale = 1.0;   // pane text zoom, see setUiScale
    int m_whiteW, m_whiteH, m_blackW, m_blackH;
    QHash<int, int> m_lit; // lit key offset -> flash generation
    QColor m_fg = Qt::white;
    QColor m_bg = Qt::black;
    QColor m_accent = QColor("#ff1493");
    QColor m_muted = Qt::gray;
};

// A painted rotary control for one numeric opt: 270-degree arc, vertical
// drag to change, double-click to reset, arrows/wheel to step. Reports
// changes through a callback so it needs no moc.
class TutDial : public QWidget
{
    Q_OBJECT
public:
    TutDial(const QString& optName, double lo, double hi, double def,
            std::function<void()> onChange, QWidget* parent,
            bool minExcl = false, bool maxExcl = false)
        : QWidget(parent)
        , m_name(optName)
        , m_lo(lo)
        , m_hi(hi)
        , m_def(def)
        , m_value(def)
        , m_onChange(std::move(onChange))
    {
        m_step = (hi - lo) >= 20.0 ? 1.0 : 0.01;
        // Open bounds (e.g. res must be < 1): pull the dial's range one step
        // inside so it can never select an invalid edge value.
        if (minExcl)
            m_lo += m_step;
        if (maxExcl)
            m_hi -= m_step;
        m_def = qBound(m_lo, m_def, m_hi);
        m_value = m_def;
        updateWidth();
        setFocusPolicy(Qt::TabFocus);
        setCursor(Qt::SizeVerCursor);
        setAccessibleName(optName);
        updateAccessibleValue();
    }

    QString optName() const { return m_name; }
    double minimum() const { return m_lo; }
    double maximum() const { return m_hi; }
    double step() const { return m_step; }
    double value() const { return m_value; }
    bool isDefault() const { return std::abs(m_value - m_def) < m_step / 2; }

    // This text lands verbatim in the generated code, so it must never round
    // to a value outside the dial's bounds (a 0.001..100 dial at its bottom
    // stop printing "0" would emit invalid code).
    QString valueText() const
    {
        if (m_step >= 1.0)
        {
            const double r = qBound(m_lo, (double)qRound(m_value), m_hi);
            if (r == (double)qRound(r))
                return QString::number(qRound(r));
            return QString::number(r);
        }
        QString s = QString::number(m_value, 'f', 2);
        while (s.endsWith('0'))
            s.chop(1);
        if (s.endsWith('.'))
            s += "0";
        if (s.toDouble() < m_lo || s.toDouble() > m_hi)
            return QString::number(m_value); // full precision at the bounds
        return s;
    }

    void setValue(double v, bool notify = true)
    {
        v = qBound(m_lo, std::round(v / m_step) * m_step, m_hi);
        if (qFuzzyCompare(v + 1.0, m_value + 1.0))
            return;
        m_value = v;
        updateAccessibleValue();
        update();
        // A screen reader speaks the new value as the dial turns — without
        // this event the knob is silent to it.
        QAccessibleValueChangeEvent ev(this, m_value);
        QAccessible::updateAccessibility(&ev);
        if (notify && m_onChange)
            m_onChange();
    }

    void reset(bool notify = true) { setValue(m_def, notify); }

    // The opt's reference doc, surfaced as a hover popup on the dial.
    void setDocText(const QString& doc)
    {
        m_doc = doc;
        updateToolTip();
    }

    // Test hook: the in-arc value rect (valid once painted) — where a click
    // opens the inline editor, and where that editor must sit.
    QRect valueRectForTest() const { return m_valRect; }

    // Roving focus across the playground's dial cluster: with a handler set,
    // Left/Right move to the neighbouring dial instead of adjusting, so the
    // whole cluster behaves as ONE Tab stop (arrows inside, Tab past). Set
    // by TutorialPane, which owns the dial order.
    void setNavigateHandler(std::function<void(TutDial*, int)> handler)
    {
        m_navigateHandler = std::move(handler);
    }

    void setColours(const QColor& fg, const QColor& dim, const QColor& accent, const QColor& track)
    {
        m_fg = fg;
        m_dim = dim;
        m_accent = accent;
        m_track = track;
        updateToolTip();   // inline-code spans in the doc use the accent
        update();
    }

    // The docs pane's text zoom (A-/A+). The dial is painted, not styled, so
    // it takes no font-size from the pane stylesheet — it has to be told.
    // Ring, value and name all key off this together: scaling the label alone
    // would push the name out of the widget's fixed height.
    void setUiScale(double scale)
    {
        if (qFuzzyCompare(m_scale, scale))
            return;
        m_scale = scale;
        QFont f = QApplication::font();
        SetFontSizeValue(f, FontSizeValue(f) * m_scale);
        setFont(f);   // FontChange → changeEvent → updateWidth()
        updateWidth();
        update();
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);

        int penW = dy(4);
        // Fixed arc diameter — never derived from text metrics, so every dial
        // on a page paints identically. Text rows below use real font metrics
        // so descenders (damp, sustain) never clip.
        const QFontMetrics fm(labelFont());
        const int nameH = fm.height();
        const int side = dx(54) - penW * 2;
        // Top inset leaves room for the pointer tick to cross the ring.
        QRectF arcRect((width() - side) / 2.0, penW * 2, side, side);

        // One weighted element: the accent value arc. The full track is a
        // hairline (context, not chrome) and the position marker is a round
        // handle sitting ON the arc's end — no crossing ticks, no dot inside.
        double frac = (m_value - m_lo) / (m_hi - m_lo);
        QPen trackPen(m_track, qMax<qreal>(1.0, penW * 0.35));
        p.setPen(trackPen);
        p.drawArc(arcRect, 225 * 16, -270 * 16);
        if (frac > 0.004)
        {
            p.setPen(QPen(m_accent, penW, Qt::SolidLine, Qt::RoundCap));
            p.drawArc(arcRect, 225 * 16, qRound(-270.0 * 16 * frac));
        }
        const double angle = qDegreesToRadians(225.0 - 270.0 * frac);
        const QPointF centre = arcRect.center();
        const double r = side / 2.0;
        p.setPen(Qt::NoPen);
        p.setBrush(m_accent);
        p.drawEllipse(QPointF(centre.x() + r * std::cos(angle),
                              centre.y() - r * std::sin(angle)),
                      penW * 0.85, penW * 0.85);

        // The value sits INSIDE the arc (its centre is otherwise empty), so
        // each dial spends vertical space only on its name below. Clicking
        // the centre opens the inline editor (m_valRect hit test). Hack bold
        // matches the code blocks; the size auto-fits the inner circle so
        // long values shrink rather than touching the ring.
        const int valInset = (penW * 3) / 2;
        m_valRect = arcRect.toRect().adjusted(valInset, valInset, -valInset, -valInset);
        const QString val = valueText();
        // Derived from the widget font so it inherits its unit — a bare
        // QFont("Hack") would be point-sized while the inherited size is in
        // pixels, silently mixing the two scales.
        QFont valFont = font();
        valFont.setFamily(QStringLiteral("Hack"));
        SetFontSizeValue(valFont, FontSizeValue(font()) * 0.9, 7.0);
        // Fit against a fixed worst-case ("00.00"), NOT the live value — so
        // ints and floats share one size and it never jumps mid-drag. The
        // loop counts down its own variable rather than reading the size
        // back from the font: a pixel-sized font rounds a half-step back up
        // to the same integer, which would spin this loop (and the GUI)
        // forever.
        const QString widest = QStringLiteral("00.00");
        double fit = FontSizeValue(valFont);
        while (fit > 6.0 && QFontMetricsF(valFont).horizontalAdvance(widest) > m_valRect.width())
        {
            fit -= 0.5;
            SetFontSizeValue(valFont, fit, 6.0);
        }
        p.setFont(valFont);
        p.setPen(m_fg);
        p.drawText(m_valRect, Qt::AlignCenter, val);
        p.setFont(labelFont());
        // Name below: one or two lines (long names wrap at an underscore).
        p.setPen(isDefault() ? m_dim : m_accent);
        QRect nameRect(0, qRound(arcRect.bottom()) + dy(2), width(), nameH);
        p.drawText(nameRect, Qt::AlignHCenter | Qt::AlignTop,
                   p.fontMetrics().elidedText(m_nameLine1, Qt::ElideRight, nameRect.width()));
        if (!m_nameLine2.isEmpty())
        {
            QRect nameRect2(0, nameRect.bottom(), width(), nameH);
            p.drawText(nameRect2, Qt::AlignHCenter | Qt::AlignTop,
                       p.fontMetrics().elidedText(m_nameLine2, Qt::ElideRight, nameRect2.width()));
        }

        if (hasFocus())
        {
            p.setPen(QPen(m_accent, 1));
            p.setBrush(Qt::NoBrush);
            p.drawRoundedRect(rect().adjusted(1, 1, -1, -1), 4, 4);
        }
    }

    void mousePressEvent(QMouseEvent* e) override
    {
        closeEditor();
        m_dragStartY = e->position().y();
        m_dragStartVal = m_value;
        m_pressPos = e->position();
        e->accept();
    }

    void mouseMoveEvent(QMouseEvent* e) override
    {
        if (m_dragStartY < 0)
            return;
        double dv = (m_dragStartY - e->position().y()) / 130.0 * (m_hi - m_lo);
        setValue(m_dragStartVal + dv);
    }

    // A clean click (no drag) on the value row opens an inline editor for
    // typing an exact value.
    void mouseReleaseEvent(QMouseEvent* e) override
    {
        const bool click = m_dragStartY >= 0
                           && (e->position() - m_pressPos).manhattanLength() < 4;
        m_dragStartY = -1;
        if (click && m_valRect.contains(e->position().toPoint()))
            openEditor();
    }
    void mouseDoubleClickEvent(QMouseEvent* e) override
    {
        if (!m_valRect.contains(e->position().toPoint()))
            reset();
    }

    bool eventFilter(QObject* obj, QEvent* ev) override
    {
        if (obj == m_editor && ev->type() == QEvent::KeyPress
            && static_cast<QKeyEvent*>(ev)->key() == Qt::Key_Escape)
        {
            commitEditor(false);
            return true;
        }
        // Qt only emits editingFinished on focus-out when the text changed,
        // so an untouched editor would linger open; close on any focus-out.
        if (obj == m_editor && ev->type() == QEvent::FocusOut)
            commitEditor(m_editor->isModified());
        return QWidget::eventFilter(obj, ev);
    }

    // The ctor's font is not the font the dial ends up painting with (the app
    // font propagates in afterwards) — re-fit the width when it lands, or the
    // label gets cropped.
    void changeEvent(QEvent* e) override
    {
        if (e->type() == QEvent::FontChange)
            updateWidth();
        QWidget::changeEvent(e);
    }

    void wheelEvent(QWheelEvent* e) override
    {
        closeEditor();
        setValue(m_value + (e->angleDelta().y() > 0 ? m_step : -m_step));
        e->accept();
    }

    void keyPressEvent(QKeyEvent* e) override
    {
        closeEditor();
        // In a cluster (navigate handler set): Left/Right walk the dials,
        // Up/Down adjust — the standard slider-group interaction, so a
        // keyboard user isn't forced to Tab through every knob.
        if (m_navigateHandler
            && (e->key() == Qt::Key_Left || e->key() == Qt::Key_Right)
            && !(e->modifiers() & Qt::ShiftModifier))
        {
            m_navigateHandler(this, e->key() == Qt::Key_Right ? +1 : -1);
            e->accept();
            return;
        }
        double big = (m_hi - m_lo) / 10.0;
        switch (e->key())
        {
        case Qt::Key_Up:
        case Qt::Key_Right:
            setValue(m_value + (e->modifiers() & Qt::ShiftModifier ? big : m_step));
            break;
        case Qt::Key_Down:
        case Qt::Key_Left:
            setValue(m_value - (e->modifiers() & Qt::ShiftModifier ? big : m_step));
            break;
        default:
            QWidget::keyPressEvent(e);
            return;
        }
        e->accept();
    }

private:
    // Dial metrics, DPI- and zoom-scaled (see setUiScale).
    int dx(int px) const { return qMax(1, qRound(ScaleWidthForDPI(px) * m_scale)); }
    int dy(int px) const { return qMax(1, qRound(ScaleHeightForDPI(px) * m_scale)); }

    QFont labelFont() const
    {
        QFont f = font();
        SetFontSizeValue(f, FontSizeValue(f) * 0.8, 7.0);
        return f;
    }

    // Inline value editor over the value row; Enter/focus-out commits,
    // Escape cancels (see eventFilter).
    void openEditor()
    {
        if (m_editor)
        {
            m_editor->setFocus();
            return;
        }
        m_editor = new QLineEdit(valueText(), this);
        m_editor->setFont(labelFont());
        m_editor->setAlignment(Qt::AlignHCenter);
        m_editor->setAccessibleName(tr("%1 value").arg(m_name));
        // Centred on the in-arc value it replaces — editing in place, never
        // sitting over the opt name below.
        const int editorH = QFontMetrics(labelFont()).height() + dy(6);
        m_editor->setGeometry(dx(4), m_valRect.center().y() - editorH / 2,
                              width() - dx(8), editorH);
        m_editor->installEventFilter(this);
        QObject::connect(m_editor, &QLineEdit::editingFinished, m_editor,
                         [this]() { commitEditor(true); });
        m_editor->show();
        m_editor->setFocus();
        m_editor->selectAll();
    }

    // Adjusting the dial directly (drag/wheel/keys) dismisses an open editor
    // so it can't linger showing a stale value over the name. Typed text is
    // never provisional: an edited value commits, an untouched one just closes.
    void closeEditor()
    {
        if (m_editor)
            commitEditor(m_editor->isModified());
    }

    void commitEditor(bool apply)
    {
        if (!m_editor)
            return;
        QLineEdit* editor = m_editor;
        m_editor = nullptr;   // guard: editingFinished re-fires on focus-out
        // Enter/Escape hand focus back to the dial; a focus-out commit must
        // not steal it from wherever the user clicked.
        const bool hadFocus = editor->hasFocus();
        if (apply)
        {
            double v = 0;
            if (parseOptValue(editor->text(), &v))
                setValue(v);
        }
        editor->deleteLater();
        if (hadFocus)
            setFocus();
    }

    // Uniform dial width — a tidy grid, whatever the opt names. Long names
    // wrap onto a second label line at the underscore nearest the middle;
    // height covers only the lines actually used (the value lives in-arc).
    void updateWidth()
    {
        const QFontMetrics fm(labelFont());
        const int w = dx(88);
        const int avail = w - dx(4);
        m_nameLine1 = m_name;
        m_nameLine2.clear();
        // A long name breaks at the separator nearest its middle: an
        // underscore, which stays on the first line (it is part of the opt),
        // or a space, which goes — a plugin's "Filter 1 Cutoff" breaks the
        // way a phrase does.
        if (fm.horizontalAdvance(m_name) > avail)
        {
            int best = -1;
            const int mid = m_name.length() / 2;
            for (int i = 0; i < m_name.length(); ++i)
                if ((m_name[i] == '_' || m_name[i] == ' ')
                    && (best < 0 || std::abs(i - mid) < std::abs(best - mid)))
                    best = i;
            if (best >= 0)
            {
                m_nameLine1 = m_name.left(m_name[best] == ' ' ? best : best + 1);
                m_nameLine2 = m_name.mid(best + 1);
            }
        }
        const int nameLines = m_nameLine2.isEmpty() ? 1 : 2;
        const int h = dy(8) + dx(54) + dy(2)
                      + fm.height() * nameLines + dy(2);
        setFixedSize(w, h);
        updateToolTip();
    }

    // Rich tooltip (name + doc): rich text makes Qt word-wrap long docs.
    // Backtick spans render as accent-coloured inline code (`60`, `:C2`).
    void updateToolTip()
    {
        QString tip = "<p><b>" + m_name.toHtmlEscaped() + "</b>";
        if (!m_doc.isEmpty())
        {
            QString doc = m_doc.toHtmlEscaped();
            static const QRegularExpression ticks(QStringLiteral("`([^`]+)`"));
            doc.replace(ticks,
                        QStringLiteral("<span style=\"font-family:'Hack'; color:%1;\">\\1</span>")
                            .arg(m_accent.name()));
            tip += " — " + doc;
        }
        tip += "</p>";
        setToolTip(tip);
    }

    void updateAccessibleValue()
    {
        setAccessibleDescription(QString("%1 %2").arg(m_name, valueText()));
    }

    QString m_name;
    QString m_nameLine1, m_nameLine2; // label split for the two-row band
    QString m_doc;
    std::function<void(TutDial*, int)> m_navigateHandler;
    double m_lo, m_hi, m_def, m_value, m_step;
    std::function<void()> m_onChange;
    QColor m_fg = Qt::white;
    QColor m_dim = Qt::gray;
    QColor m_accent = QColor("#ff1493");
    QColor m_track = Qt::darkGray;
    double m_scale = 1.0;   // pane text zoom, see setUiScale
    double m_dragStartY = -1;
    double m_dragStartVal = 0;
    QPointF m_pressPos;
    mutable QRect m_valRect;   // set during paint; hit-tested for click-to-edit
    QLineEdit* m_editor = nullptr;
};

// Install the accessible-interface factory for these widgets (idempotent).
void registerTutorialWidgetAccessibility();
