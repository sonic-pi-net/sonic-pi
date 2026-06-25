//--

// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "completionpopup.h"

#include <QListView>
#include <QStandardItemModel>
#include <QStyledItemDelegate>
#include <QTextEdit>
#include <QTextBrowser>
#include <QDesktopServices>
#include <QUrl>
#include <QToolButton>
#include <QItemSelectionModel>
#include <QPainter>
#include <QPaintEvent>
#include <QMouseEvent>
#include <QHBoxLayout>
#include <functional>
#include <QVBoxLayout>
#include <QApplication>
#include <QScreen>
#include <QFontMetrics>
#include <QPropertyAnimation>
#include <QVariantAnimation>
#include <QWheelEvent>
#include <QPolygonF>
#include <QSet>

#ifdef Q_OS_MACOS
#include "platform/macos.h"
#endif

namespace {

// Blend two colours: pct% of `a` plus (100-pct)% of `b`.
QColor mix(const QColor& a, const QColor& b, int pct) {
    return QColor((a.red()   * pct + b.red()   * (100 - pct)) / 100,
                  (a.green() * pct + b.green() * (100 - pct)) / 100,
                  (a.blue()  * pct + b.blue()  * (100 - pct)) / 100);
}

// Subtle per-kind badge colour so the eye can tell synths from fx from opts.
QColor kindColor(const QString& kind) {
    if (kind == "synth")   return QColor(0x9B, 0x59, 0xB6); // purple
    if (kind == "fx")      return QColor(0x34, 0x98, 0xDB); // blue
    if (kind == "sample")  return QColor(0xE6, 0x7E, 0x22); // orange
    if (kind == "opt")     return QColor(0x95, 0xA5, 0xA6); // grey
    if (kind == "cue" || kind == "peer" || kind == "channel" || kind == "port")
                           return QColor(0x16, 0xA0, 0x85); // teal
    if (kind == "chord" || kind == "scale" || kind == "tuning")
                           return QColor(0xE0, 0xA8, 0x00); // amber
    return QColor(0x27, 0xAE, 0x60); // green (fn / default)
}

const int kRowVPad = 5;
const int kRowHPad = 8;
const int kBadgeHPad = 6;
const int kGap = 10;
const int kDetailW = 380;   // fixed docstring-pane width (independent of the list)
const int kDetailMinH = 320; // min popup height when a docstring pane is shown

// Paints aligned columns: [kind badge]  name      dimmed summary (elided).
class CompletionDelegate : public QStyledItemDelegate {
public:
    explicit CompletionDelegate(CompletionPopup* popup, QObject* parent = nullptr)
        : QStyledItemDelegate(parent), m_popup(popup) {}

    QSize sizeHint(const QStyleOptionViewItem& opt, const QModelIndex& index) const override {
        QFontMetrics fm(opt.font);
        const int h = fm.height() + 2 * kRowVPad;
        const QString name = index.data(Qt::DisplayRole).toString();
        const QString kind = index.data(CompletionPopup::KindRole).toString();
        const QString summary = index.data(CompletionPopup::SummaryRole).toString();
        // Only notes carry an inline summary (name · MIDI · Hz); other kinds show
        // their docstring in the right-hand pane instead.
        const bool inlineSummary = (kind == "note") && !summary.isEmpty();
        int w = inlineSummary
                    ? m_popup->nameColumnX() + fm.horizontalAdvance(name) + 18
                          + fm.horizontalAdvance(summary)
                    : m_popup->nameColumnX() + fm.horizontalAdvance(name);
        return QSize(w + kRowHPad, h);
    }

    void paint(QPainter* p, const QStyleOptionViewItem& opt, const QModelIndex& index) const override {
        p->save();
        p->setRenderHint(QPainter::Antialiasing, true);

        const bool selected = opt.state & QStyle::State_Selected;
        const QPalette& pal = opt.palette;

        if (selected) {
            QRect r = opt.rect.adjusted(2, 1, -2, -1);
            p->setPen(Qt::NoPen);
            p->setBrush(pal.color(QPalette::Highlight));
            p->drawRoundedRect(r, 4, 4);
        }

        const QString kind = index.data(CompletionPopup::KindRole).toString();
        const QString name = index.data(Qt::DisplayRole).toString();
        const QString summary = index.data(CompletionPopup::SummaryRole).toString();

        QFontMetrics fm(opt.font);
        const int cy = opt.rect.center().y();

        // Kind badge — saturated colour, bright label for legibility. Notes skip
        // it: the piano below already signals "this is a note".
        if (!kind.isEmpty() && kind != "note") {
            QFont badgeFont = opt.font;
            badgeFont.setPointSizeF(opt.font.pointSizeF() * 0.8);
            QFontMetrics bfm(badgeFont);
            int bw = bfm.horizontalAdvance(kind) + 2 * kBadgeHPad;
            int bh = bfm.height() + 2;
            QRect badge(opt.rect.left() + kRowHPad, cy - bh / 2, bw, bh);
            QColor c = kindColor(kind);
            p->setPen(Qt::NoPen);
            p->setBrush(selected ? QColor(0, 0, 0, 90)
                                 : QColor(c.red(), c.green(), c.blue(), 70));
            p->drawRoundedRect(badge, 3, 3);
            p->setFont(badgeFont);
            p->setPen(selected ? c.lighter(135) : c.lighter(125));
            p->drawText(badge, Qt::AlignCenter, kind);
        }

        // Name — full-contrast theme foreground, at the aligned name column.
        const int nameX = opt.rect.left() + m_popup->nameColumnX();
        p->setFont(opt.font);
        p->setPen(selected ? pal.color(QPalette::HighlightedText) : pal.color(QPalette::Text));
        p->drawText(QRect(nameX, opt.rect.top(), fm.horizontalAdvance(name), opt.rect.height()),
                    Qt::AlignVCenter | Qt::AlignLeft, name);

        // Inline summary (notes only) — readable secondary tone, placed right
        // after the number with a small gap (not a far column) so it reads tight.
        if (kind == "note" && !summary.isEmpty()) {
            const int sx = nameX + fm.horizontalAdvance(name) + 18;
            int avail = opt.rect.right() - kRowHPad - sx;
            if (avail > 20) {
                const QColor dim = selected
                    ? pal.color(QPalette::HighlightedText)
                    : mix(pal.color(QPalette::Text), pal.color(QPalette::Base), 60);
                p->setPen(dim);
                QString elided = fm.elidedText(summary, Qt::ElideRight, avail);
                p->drawText(QRect(sx, opt.rect.top(), avail, opt.rect.height()),
                            Qt::AlignVCenter | Qt::AlignLeft, elided);
            }
        }

        p->restore();
    }

private:
    CompletionPopup* m_popup;
};

} // namespace

// A mini piano keyboard (~2 octaves) highlighting one MIDI note, with a caption
// line (note name · MIDI · Hz). Shown in the popup's detail area for note
// completions so pitch is tangible. Plain QWidget (no signals) — no MOC needed.
class NotePiano : public QWidget {
public:
    explicit NotePiano(QWidget* parent = nullptr) : QWidget(parent) {
        setMouseTracking(true);          // hover without a button held
        setFocusPolicy(Qt::NoFocus);     // never steal the editor's keyboard focus
        // Slides the keyboard window smoothly when it scrolls.
        m_slide = new QVariantAnimation(this);
        m_slide->setDuration(180);
        m_slide->setEasingCurve(QEasingCurve::OutCubic);
        connect(m_slide, &QVariantAnimation::valueChanged, this,
                [this](const QVariant& v) { m_startWhite = v.toDouble(); update(); });
    }

    // Wash the keys present in the dropdown so matches are visible. When every key
    // is in the list (nothing typed yet) the wash is uniform noise, so skip it.
    void setInList(const QSet<int>& notes) {
        m_inList = (notes.size() >= m_maxMidi - m_minMidi + 1) ? QSet<int>() : notes;
        update();
    }

    void setNote(int midi) {
        if (midi != m_note || m_startWhite < 0) {
            m_chordNotes.clear();
            m_note = midi;
            if (midi >= 0) centreOn(whitesBelow(midi));
        }
        update();
    }

    // Light up a whole chord/scale (its resolved MIDI notes), centred so the
    // span is visible. The keyboard becomes a read-only preview.
    void setChordNotes(const QList<int>& notes) {
        m_inList.clear();   // no dropdown-match wash in chord/scale mode
        m_chordNotes = QSet<int>(notes.begin(), notes.end());
        m_note = notes.isEmpty() ? -1 : notes.first();
        if (!notes.isEmpty()) {
            int lo = 127, hi = 0;
            for (int n : notes) { lo = qMin(lo, n); hi = qMax(hi, n); }
            centreOn((whitesBelow(lo) + whitesBelow(hi)) / 2.0);
        }
        update();
    }
    void setColors(const QColor& fg, const QColor& accent, const QColor& bg) {
        m_fg = fg;
        m_accent = accent;
        // Invert the keys in dark mode so they sit in the theme rather than glaring.
        if (bg.lightnessF() < 0.5) {
            m_white = QColor(30, 30, 30);
            m_black = QColor(225, 225, 225);
            m_edge  = QColor(79, 79, 79);
        } else {
            m_white = QColor(250, 250, 250);
            m_black = QColor(30, 30, 30);
            m_edge  = QColor(176, 176, 176);
        }
        update();
    }
    void setOnClick(std::function<void(int)> cb) { m_onClick = std::move(cb); }
    void setOnHover(std::function<void(int)> cb) { m_onHover = std::move(cb); }

protected:
    void paintEvent(QPaintEvent*) override {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        QRect kb = keyRect();
        if (m_note < 0 || kb.height() < 6) return;

        const double wW = double(kb.width()) / viewWhites();
        const double bW = wW * kBlackFrac, bH = kb.height() * kBlackLenFrac;
        const double rad = qBound(1.5, wW * 0.16, 3.0);
        auto xOf = [&](int m) { return kb.left() + (whitesBelow(m) - m_startWhite) * wW; };

        // A soft accent wash marks the keys present in the dropdown (without the
        // strong accent of the selected key): white keys tint toward the accent,
        // black keys lighten toward it.
        const QColor inWhite(mix(m_accent, m_white, 32));
        const QColor inBlack(mix(m_accent, m_black, 55));
        // Chord/scale degrees other than the root are lit in a lighter accent so
        // the root stands out and a full scale reads as a shape, not a block.
        const QColor toneWhite(mix(m_accent, m_white, 55));
        const QColor toneBlack(mix(m_accent, m_black, 72));

        p.setClipRect(kb);
        // White keys (cull off-screen). Selected = accent fill + darker accent edge
        // so the highlight never paints white over the neighbouring black keys.
        for (int m = m_minMidi; m <= m_maxMidi; ++m) {
            if (!isWhite(m)) continue;
            const double x = xOf(m);
            if (x + wW < kb.left() || x > kb.right()) continue;
            const bool root = (m == m_note);
            const bool tone = m_chordNotes.contains(m);
            QRectF r(x + 0.5, kb.top(), wW - 1.5, kb.height());
            p.setPen(QPen((root || tone) ? m_accent.darker(150) : m_edge, 1));
            p.setBrush(root ? m_accent
                            : tone ? toneWhite
                                   : (m_inList.contains(m) ? inWhite : m_white));
            p.drawRoundedRect(r, rad, rad);
        }
        // Black keys sit on the boundary between their neighbouring white keys.
        for (int m = m_minMidi; m <= m_maxMidi; ++m) {
            if (isWhite(m)) continue;
            const double cx = xOf(m);
            if (cx + bW < kb.left() || cx - bW > kb.right()) continue;
            const bool root = (m == m_note);
            const bool tone = m_chordNotes.contains(m);
            QRectF r(cx - bW / 2, kb.top(), bW, bH);
            p.setPen(Qt::NoPen);
            p.setBrush(root ? m_accent
                            : tone ? toneBlack
                                   : (m_inList.contains(m) ? inBlack : m_black));
            p.drawRoundedRect(r, rad, rad);
        }
        p.setClipping(false);

        // Octave labels (C4, C5 …) under each C, so the keyboard is orienting.
        QFont lf = font();
        lf.setPointSizeF(qMax(8.0, font().pointSizeF() - 1));
        p.setFont(lf);
        const int labelTop = kb.bottom() + 1;
        for (int m = m_minMidi; m <= m_maxMidi; m += 12) {     // every C
            const double x = xOf(m);
            if (x + wW < kb.left() || x > kb.right()) continue;
            QColor lc = m_fg; lc.setAlpha(m == m_note || m == (m_note - m_note % 12) ? 230 : 150);
            p.setPen(lc);
            p.drawText(QRectF(x, labelTop, wW, height() - labelTop),
                       Qt::AlignHCenter | Qt::AlignVCenter, QString("C%1").arg(m / 12 - 1));
        }

        // Scroll chevrons at the edges (only when there's more to see that way).
        const double cy = kb.center().y();
        if (canScroll(-1)) drawChevron(p, kb.left() + 7, cy, -1);
        if (canScroll(+1)) drawChevron(p, kb.right() - 7, cy, +1);
    }

    void mousePressEvent(QMouseEvent* e) override {
        const QRect kb = keyRect();
        const int x = e->pos().x();
        if (x < kb.left() + 18 && canScroll(-1)) { scrollBy(-1); return; }
        if (x > kb.right() - 18 && canScroll(+1)) { scrollBy(+1); return; }
        const int m = midiAt(e->pos());
        if (m >= 0 && m_onClick) m_onClick(m);
    }

    void wheelEvent(QWheelEvent* e) override {
        const int d = e->angleDelta().y() + e->angleDelta().x();
        if (d > 0) scrollBy(-1);
        else if (d < 0) scrollBy(+1);
        e->accept();
    }

    // Hovering a key highlights the matching completion (which in turn repaints
    // the keyboard with that key lit, via the popup's selection → setNote loop).
    void mouseMoveEvent(QMouseEvent* e) override {
        const int m = midiAt(e->pos());
        if (m != m_hoverMidi) {
            m_hoverMidi = m;
            if (m >= 0 && m_onHover) m_onHover(m);
        }
    }

private:
    static constexpr int kMaxView = 15;     // show at most ~2 octaves of white keys
    // Real-piano proportions: a black key is ~0.58 of a white key's width and
    // ~0.63 of its length (13.7mm/23.5mm, 95mm/150mm).
    static constexpr double kBlackFrac = 0.58;
    static constexpr double kBlackLenFrac = 0.63;

    static bool isWhite(int m) { int s = m % 12;
        return s == 0 || s == 2 || s == 4 || s == 5 || s == 7 || s == 9 || s == 11; }
    // Count of white keys below a MIDI note — a continuous horizontal coordinate.
    static int whitesBelow(int midi) {
        static const int wb[12] = {0,1,1,2,2,3,4,4,5,5,6,6};
        return (midi / 12) * 7 + wb[midi % 12];
    }
    // White keys shown at once: the whole range if it's small, else a 2-octave window.
    int viewWhites() const {
        const int rw = whitesBelow(m_maxMidi) + 1 - whitesBelow(m_minMidi);
        return qBound(1, qMin(rw, kMaxView), qMax(1, rw));
    }
    double clampStart(double s) const {
        const double lo = whitesBelow(m_minMidi);
        const double hi = whitesBelow(m_maxMidi) + 1 - viewWhites();
        return qBound(lo, s, qMax(lo, hi));
    }
    // Centre the window on a white-key position: snap on first show, otherwise
    // only slide when it's drifted to/past the visible edge (keeps it steady).
    void centreOn(double whitePos) {
        const int view = viewWhites();
        if (m_startWhite < 0) {
            m_targetWhite = clampStart(whitePos - view / 2.0);
            m_startWhite = m_targetWhite;
        } else if (whitePos < m_targetWhite + 1 || whitePos > m_targetWhite + view - 1) {
            slideTo(clampStart(whitePos - view / 2.0));
        }
    }
    bool canScroll(int dir) const {
        return dir < 0 ? m_targetWhite > clampStart(m_targetWhite - 1) + 0.01
                       : m_targetWhite < clampStart(m_targetWhite + 1) - 0.01;
    }
    void slideTo(double target) {
        m_targetWhite = target;
        m_slide->stop();
        m_slide->setStartValue(m_startWhite);
        m_slide->setEndValue(target);
        m_slide->start();
    }
    void scrollBy(int octaves) { slideTo(clampStart(m_targetWhite + octaves * 7.0)); }

    QRect keyRect() const {
        const int labelH = QFontMetrics(font()).height() + 2;
        return QRect(6, 5, width() - 12, height() - labelH - 8);
    }
    void drawChevron(QPainter& p, double x, double cy, int dir) const {
        const double s = 5;
        QPolygonF tri;
        tri << QPointF(x - dir * s, cy - s) << QPointF(x + dir * s, cy)
            << QPointF(x - dir * s, cy + s);
        p.setPen(Qt::NoPen);
        p.setBrush(QColor(120, 120, 120, 200));
        p.drawPolygon(tri);
    }

    // The MIDI note under a point, or -1. Black keys (drawn on top) win ties.
    int midiAt(const QPoint& pt) const {
        const QRect kb = keyRect();
        if (m_note < 0 || kb.height() < 6) return -1;
        const double wW = double(kb.width()) / viewWhites();
        const double bW = wW * kBlackFrac, bH = kb.height() * kBlackLenFrac;
        auto xOf = [&](int m) { return kb.left() + (whitesBelow(m) - m_startWhite) * wW; };

        if (pt.y() >= kb.top() && pt.y() <= kb.top() + bH) {
            for (int m = m_minMidi; m <= m_maxMidi; ++m) {
                if (isWhite(m)) continue;
                QRectF r(xOf(m) - bW / 2, kb.top(), bW, bH);
                if (r.contains(pt)) return m;
            }
        }
        if (pt.y() >= kb.top() && pt.y() <= kb.bottom()) {
            for (int m = m_minMidi; m <= m_maxMidi; ++m) {
                if (!isWhite(m)) continue;
                QRectF r(xOf(m), kb.top(), wW - 1, kb.height());
                if (pt.x() >= r.left() && pt.x() < r.right()) return m;
            }
        }
        return -1;
    }

    int m_note = -1;
    int m_minMidi = 36, m_maxMidi = 96;   // the full, fixed keyboard (C2–C7)
    QSet<int> m_inList;          // notes present in the current dropdown (washed)
    QSet<int> m_chordNotes;      // all notes of the previewed chord/scale (accent-lit)
    double m_startWhite = -1;   // animated render position (white-key units)
    double m_targetWhite = 0;   // logical window position
    int m_hoverMidi = -2;
    QColor m_fg = QColor(220, 220, 220);
    QColor m_accent = QColor(0x9B, 0x59, 0xB6);
    QColor m_white = QColor(250, 250, 250);   // white-key body (inverted in dark mode)
    QColor m_black = QColor(30, 30, 30);       // black-key body (inverted in dark mode)
    QColor m_edge = QColor(176, 176, 176);     // white-key outline
    QVariantAnimation* m_slide = nullptr;
    std::function<void(int)> m_onClick;
    std::function<void(int)> m_onHover;
};

// A horizontal value-picker for a bounded opt (e.g. pan: -1..1). Drag/click sets
// the value, Up/Down (routed via the popup) nudges it, release accepts. Plain
// QWidget (no signals); the popup wires accept via a callback.
class RangeSlider : public QWidget {
public:
    explicit RangeSlider(QWidget* parent = nullptr) : QWidget(parent) {
        setMouseTracking(true);
        setFocusPolicy(Qt::NoFocus);
    }
    void configure(double lo, double hi, double val, const QString& label) {
        m_min = lo; m_max = qMax(hi, lo + 1e-6); m_label = label;
        m_value = qBound(m_min, val, m_max);
        update();
    }
    void setColors(const QColor& fg, const QColor& accent) { m_fg = fg; m_accent = accent; update(); }
    void setOnAccept(std::function<void()> cb) { m_onAccept = std::move(cb); }
    void setOnChange(std::function<void()> cb) { m_onChange = std::move(cb); }
    void nudge(int steps) { setValue(m_value + steps * (m_max - m_min) / 40.0); }
    void setValue(double v) {
        v = qBound(m_min, v, m_max);
        if (qFuzzyCompare(v, m_value)) return;
        m_value = v;
        update();
        if (m_onChange) m_onChange();    // live-update the value in the buffer
    }
    QString valueText() const { return fmt(m_value); }

protected:
    void paintEvent(QPaintEvent*) override {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        const QRectF tr = trackRect();
        const double frac = (m_value - m_min) / (m_max - m_min);
        const double hx = tr.left() + frac * tr.width();

        p.setPen(Qt::NoPen);
        p.setBrush(QColor(150, 150, 150, 90));            // unfilled track
        p.drawRoundedRect(tr, tr.height() / 2, tr.height() / 2);
        p.setBrush(m_accent);                             // filled portion
        p.drawRoundedRect(QRectF(tr.left(), tr.top(), hx - tr.left(), tr.height()),
                          tr.height() / 2, tr.height() / 2);
        p.setPen(QPen(m_fg, 1.5));                        // handle
        p.setBrush(m_accent);
        p.drawEllipse(QPointF(hx, tr.center().y()), 8, 8);

        QFontMetrics fm(font());
        QFont vf = font(); vf.setBold(true); vf.setPointSizeF(font().pointSizeF() + 2);
        p.setFont(vf);
        p.setPen(m_fg);
        p.drawText(QRectF(0, 6, width(), fm.height() + 6), Qt::AlignHCenter,
                   (m_label.isEmpty() ? QString() : m_label + "  ") + valueText());
        p.setFont(font());
        QColor dim = m_fg; dim.setAlpha(150); p.setPen(dim);
        const double ly = tr.bottom() + 4;
        p.drawText(QRectF(tr.left(), ly, 120, fm.height()), Qt::AlignLeft, fmt(m_min));
        p.drawText(QRectF(tr.right() - 120, ly, 120, fm.height()), Qt::AlignRight, fmt(m_max));
    }
    void mousePressEvent(QMouseEvent* e) override { setValueFromX(e->pos().x()); }
    void mouseMoveEvent(QMouseEvent* e) override {
        if (e->buttons() & Qt::LeftButton) setValueFromX(e->pos().x());
    }
    void mouseReleaseEvent(QMouseEvent*) override { if (m_onAccept) m_onAccept(); }

private:
    static QString fmt(double v) {
        QString s = QString::number(v, 'f', 2);
        if (s.contains('.')) { while (s.endsWith('0')) s.chop(1); if (s.endsWith('.')) s.chop(1); }
        return s;
    }
    QRectF trackRect() const {
        const double pad = 16, y = height() * 0.56;
        return QRectF(pad, y - 3, qMax(1.0, width() - 2 * pad), 6);
    }
    void setValueFromX(int x) {
        const QRectF tr = trackRect();
        const double frac = qBound(0.0, (x - tr.left()) / tr.width(), 1.0);
        setValue(m_min + frac * (m_max - m_min));
    }
    double m_min = 0, m_max = 1, m_value = 0;
    QString m_label;
    QColor m_fg = QColor(220, 220, 220);
    QColor m_accent = QColor(0x9B, 0x59, 0xB6);
    std::function<void()> m_onAccept;
    std::function<void()> m_onChange;
};

CompletionPopup::CompletionPopup(QWidget* parent)
    : QWidget(parent)
{
    // Frameless, always-on-top, non-focus-stealing popup. A translucent window
    // background lets the stylesheet paint the rounded body + its border (without
    // it, a frameless top-level draws an opaque square and the border is lost).
    setWindowFlags(Qt::ToolTip | Qt::FramelessWindowHint | Qt::NoDropShadowWindowHint);
    setAttribute(Qt::WA_ShowWithoutActivating);
    setAttribute(Qt::WA_TranslucentBackground);
    setFocusPolicy(Qt::NoFocus);

    m_model = new QStandardItemModel(this);

    m_view = new QListView(this);
    m_view->setModel(m_model);
    m_view->setItemDelegate(new CompletionDelegate(this, m_view));
    m_view->setEditTriggers(QAbstractItemView::NoEditTriggers);
    m_view->setSelectionMode(QAbstractItemView::SingleSelection);
    m_view->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_view->setFocusPolicy(Qt::NoFocus);
    m_view->setUniformItemSizes(true);
    m_view->setObjectName("completionView");

    // Docstring pane: a wrapped description of the highlighted item, below the
    // list, separated by a hairline. Hidden when the item has no summary.
    // Scrollable rich-text docstring box (right of the list).
    m_detail = new QTextBrowser;
    m_detail->setObjectName("completionDetail");
    m_detail->setReadOnly(true);
    m_detail->setFrameShape(QFrame::NoFrame);
    m_detail->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_detail->setFocusPolicy(Qt::NoFocus);
    // Handle links ourselves: in-document `#opt` anchors (opt-name links in the
    // summary table) scroll to that opt's description; real URLs open in a browser.
    m_detail->setOpenLinks(false);
    connect(m_detail, &QTextBrowser::anchorClicked, this, [this](const QUrl& url) {
        if (url.scheme().isEmpty() && !url.fragment().isEmpty())
            m_detail->scrollToAnchor(url.fragment());
        else
            QDesktopServices::openUrl(url);
    });
    // Wrap to the viewport: the pane width is constant per session, so this never
    // jumps, and it can't clip the prose the way a fixed pixel width could.
    m_detail->setLineWrapMode(QTextEdit::WidgetWidth);

    m_piano = new NotePiano(this);
    m_piano->setObjectName("completionPiano");

    m_rangeSlider = new RangeSlider(this);
    m_rangeSlider->setObjectName("completionSlider");
    m_rangeSlider->setVisible(false);
    m_rangeSlider->setOnAccept([this]() { emit accepted(); });

    // "Docs" affordance pinned to the bottom-right of the docstring pane.
    m_docsButton = new QToolButton;
    m_docsButton->setObjectName("completionDocsButton");
    m_docsButton->setText(tr("Docs ↗"));
    m_docsButton->setCursor(Qt::PointingHandCursor);
    m_docsButton->setFocusPolicy(Qt::NoFocus);
    m_docsButton->setToolTip(tr("Open the help pane for this entry"));
    connect(m_docsButton, &QToolButton::clicked, this, [this]() {
        const QModelIndex idx = m_view->currentIndex();
        if (idx.isValid()) emit docsRequested(idx.data(Qt::DisplayRole).toString());
    });

    // Right column: the docstring above, the Docs button in a bottom row.
    m_detailPane = new QWidget(this);
    m_detailPane->setObjectName("completionDetailPane");
    auto* paneLayout = new QVBoxLayout(m_detailPane);
    paneLayout->setContentsMargins(0, 0, 0, 0);
    paneLayout->setSpacing(0);
    paneLayout->addWidget(m_detail, 1);
    auto* btnRow = new QHBoxLayout;
    btnRow->setContentsMargins(8, 6, 12, 10);
    btnRow->addStretch(1);
    btnRow->addWidget(m_docsButton);
    paneLayout->addLayout(btnRow);

    m_detailPane->setVisible(false);
    m_piano->setVisible(false);

    // No outer layout: the popup is a top-level whose size we pin with
    // setFixedSize, and a QLayout (with the QTextEdit's content-driven size hint)
    // fights that. resizeToContents() positions the three regions by hand.

    setObjectName("completionPopup");
    setStyleSheet(
        "#completionPopup { background: palette(base); border: 1px solid rgba(127,127,127,90);"
        " border-radius: 6px; }"
        "#completionView { background: transparent; border: none; outline: none; }"
        "#completionDetailPane { background: transparent;"
        " border-left: 1px solid rgba(127,127,127,60); }"
        "#completionDetail { background: transparent; padding: 6px 8px; border: none; }"
        "#completionPiano { background: transparent;"
        " border-top: 1px solid rgba(127,127,127,60); }");

    // Hide as soon as the app is no longer active (switching away, etc.).
    connect(qApp, &QApplication::applicationStateChanged, this,
            [this](Qt::ApplicationState s) { if (s != Qt::ApplicationActive) hidePopup(); });

    // Navigating only refreshes the detail/piano for the new row — the popup size
    // is fixed for the session, so no re-layout (or all-rows re-measure) is needed.
    // It also live-previews the new selection in the editor buffer.
    connect(m_view->selectionModel(), &QItemSelectionModel::currentChanged,
            this, [this](const QModelIndex&, const QModelIndex&) {
                m_noteOverride.clear();   // a real row selection supersedes any clicked-key override
                updateDetail();
                if (!m_sliderMode) emit previewChanged(currentText());
            });

    connect(m_view, &QListView::clicked, this, [this](const QModelIndex& idx) {
        if (idx.isValid()) { m_view->setCurrentIndex(idx); emit accepted(); }
    });

    // Hover a piano key → highlight the matching completion; click → insert it.
    // A key with no list entry is still clickable: it inserts that MIDI note
    // directly (the keyboard covers more notes than any filtered list). In
    // chord/scale mode the keyboard is a read-only preview, so clicks are inert.
    m_piano->setOnHover([this](int midi) { if (!m_chordMode) selectNote(midi); });
    m_piano->setOnClick([this](int midi) {
        if (m_chordMode) return;
        if (!selectNote(midi)) m_noteOverride = QString::number(midi);
        emit accepted();
    });
    // Dragging the slider live-previews its value in the buffer.
    m_rangeSlider->setOnChange([this]() { emit previewChanged(m_rangeSlider->valueText()); });
}

void CompletionPopup::paintEvent(QPaintEvent*)
{
    // A plain QWidget doesn't render a stylesheet border, so draw the rounded
    // body + border ourselves (the window is translucent outside the radius).
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing, true);
    QRectF r = QRectF(rect()).adjusted(0.75, 0.75, -0.75, -0.75);
    p.setPen(QPen(m_border, 1.5));
    p.setBrush(m_bg);
    p.drawRoundedRect(r, 6, 6);
}

bool CompletionPopup::selectNote(int midi)
{
    // Prefer the number item (numeric insert text) so a click inserts a number.
    for (int pass = 0; pass < 2; ++pass) {
        for (int i = 0; i < m_model->rowCount(); ++i) {
            const QModelIndex idx = m_model->index(i, 0);
            if (idx.data(NoteRole).toInt() != midi) continue;
            bool numeric = false;
            idx.data(InsertRole).toString().toInt(&numeric);
            if (pass == 0 && !numeric) continue; // first pass: numbers only
            m_view->setCurrentIndex(idx);
            return true;
        }
    }
    return false;
}

void CompletionPopup::applyTheme(const QColor& bg, const QColor& fg,
                                 const QColor& selBg, const QColor& selFg)
{
    QPalette pal = m_view->palette();
    pal.setColor(QPalette::Base, bg);
    pal.setColor(QPalette::Window, bg);
    pal.setColor(QPalette::Text, fg);
    pal.setColor(QPalette::WindowText, fg);
    pal.setColor(QPalette::Highlight, selBg);
    pal.setColor(QPalette::HighlightedText, selFg);
    m_view->setPalette(pal);
    setPalette(pal);

    // Border: a midpoint of fg/bg (leaning to fg) so it's clearly visible on both
    // dark and high-contrast light themes, where a bg-weighted blend washed out.
    QColor border = mix(fg, bg, 55);
    // The popup's rounded background + border are painted in paintEvent (a plain
    // QWidget doesn't render a stylesheet border), so keep the colours here.
    m_bg = bg;
    m_border = border;
    // Detail body: keep it bright (95% fg) so prose is easy to read.
    QColor detail = mix(fg, bg, 95);
    update();
    setStyleSheet(QString(
        "#completionPopup { background: transparent; }"
        "#completionView { background: transparent; border: none; outline: none; }"
        "#completionDetailPane { background: transparent; border-left: 1px solid %2; }"
        "#completionDetail { background: transparent; color: %3; padding: 12px 16px;"
        " border: none; }"
        "#completionPiano { background: transparent; border-top: 1px solid %2; }"
        "#completionDocsButton { background: transparent; color: %4; border: 1px solid %2;"
        " border-radius: 4px; padding: 3px 10px; font-size: 11px; }"
        "#completionDocsButton:hover { background: %4; color: %1; border-color: %4; }")
        .arg(bg.name(), border.name(), detail.name(), selBg.name()));
    // Accent-colour inline code + links so opt names stand out; code blocks stay neutral.
    QColor grid = mix(fg, bg, 28);   // faint opts-table lines
    m_detail->document()->setDefaultStyleSheet(QString(
        "h3 { color:%1; font-size:large; margin:0 0 14px 0; }"
        "p { margin:0 0 12px 0; }"
        "code { color:%2; }"
        "pre { color:%1; }"
        "a { color:%2; }"
        "table { border-collapse: collapse; }"
        "td { border: 1px solid %3; }")
        .arg(fg.name(), selBg.name(), grid.name()));
    if (m_piano) m_piano->setColors(fg, selBg, bg);
    if (m_rangeSlider) m_rangeSlider->setColors(fg, selBg);
    update();
}

void CompletionPopup::setItemFont(const QFont& font, double docPointSize)
{
    // Docstring pane: this widget is governed by the popup's stylesheet, so
    // QWidget::setFont() is ignored (Qt resolves a styled widget's font through
    // the style sheet). Drive its size with a stylesheet font-size instead, and
    // re-render so the imported HTML/markdown adopts it. Tracked separately from
    // the list font (and before the list early-return) so the docs keep tracking
    // zoom even when the list font is clamped at its max and stops changing.
    if (m_detail) {
        const int pt = qRound(qBound(9.0, docPointSize, 22.0));
        if (pt != m_docPointSize) {
            m_docPointSize = pt;
            // Own stylesheet, font-size only — the popup-wide sheet keeps styling
            // #completionDetail's colours/padding; these cascade together.
            m_detail->setStyleSheet(QStringLiteral("QTextBrowser { font-size: %1pt; }").arg(pt));
            m_detailKey.clear();  // force updateDetail() to re-render at the new size
        }
    }

    if (m_view->font() == font) return;
    m_view->setFont(font);
    if (m_piano) m_piano->setFont(font);
    if (m_rangeSlider) m_rangeSlider->setFont(font);
}

bool CompletionPopup::showItems(const QList<CompletionItem>& items,
                                const QPoint& caretTopLeft, int lineHeight,
                                int preferNote)
{
    // A fresh completion (popup hidden) or a change of mode (e.g. a function list
    // becoming a note list as the caret moves into an argument) starts a new
    // sizing session; within a session the width only grows, so it stays steady.
    const bool wasVisible = isVisible();
    const bool wasNote = m_noteMode;
    const bool wasChord = m_chordMode;
    const bool wasDetail = m_hasDetail;
    m_noteOverride.clear();

    const QString kind0 = items.isEmpty() ? QString() : items.first().kind;
    m_sliderMode = m_showHelp && items.size() == 1 && items.first().slider;
    m_noteMode = m_showHelp && !m_sliderMode && kind0 == "note";
    m_chordMode = m_showHelp && !m_sliderMode && (kind0 == "chord" || kind0 == "scale");
    m_hasDetail = false;
    if (m_showHelp && !m_noteMode && !m_chordMode && !m_sliderMode) {
        for (const CompletionItem& it : items) {
            if (!it.doc.isEmpty() || !it.summary.isEmpty()) { m_hasDetail = true; break; }
        }
    }
    if (!wasVisible || wasNote != m_noteMode || wasChord != m_chordMode || wasDetail != m_hasDetail)
        m_sessionListW = 0;

    if (m_noteMode) {
        // The keyboard stays a full, fixed instrument; just mark which keys are in
        // the filtered list so the typed query is visible (every key stays clickable).
        QSet<int> inList;
        for (const CompletionItem& it : items)
            if (it.note >= 0) inList.insert(it.note);
        m_piano->setInList(inList);
    }

    if (m_sliderMode) {
        const CompletionItem& it = items.first();
        m_rangeSlider->configure(it.rmin, it.rmax, it.rdefault, it.text);
    }

    m_model->clear();
    for (const CompletionItem& it : items) {
        auto* row = new QStandardItem(it.text);
        row->setData(it.kind, KindRole);
        row->setData(it.summary, SummaryRole);
        row->setData(it.text, InsertRole);
        row->setData(it.note, NoteRole);
        row->setData(it.doc, DocRole);
        row->setData(QVariant::fromValue(it.intervals), IntervalsRole);
        row->setEditable(false);
        m_model->appendRow(row);
    }
    if (m_model->rowCount() == 0) {
        hidePopup();
        return false;
    }

    // Notes select the typed number (preferNote) if given, else default to ~middle
    // C (MIDI 60) rather than the lowest entry.
    int defaultRow = 0;
    if (!items.isEmpty() && items.first().kind == "note") {
        const int target = preferNote >= 0 ? preferNote : 60;
        int bestDist = 1000;
        for (int i = 0; i < m_model->rowCount(); ++i) {
            const int d = qAbs(m_model->index(i, 0).data(NoteRole).toInt() - target);
            if (d < bestDist) { bestDist = d; defaultRow = i; }
        }
    }
    {
        QSignalBlocker b(m_view->selectionModel());
        m_view->setCurrentIndex(m_model->index(defaultRow, 0));
        m_view->scrollTo(m_model->index(defaultRow, 0), QAbstractItemView::PositionAtCenter);
    }
    computeColumns();
    updateDetail();
    resizeToContents();

    // Position below the caret line (caretTopLeft is global), flip above when
    // there's no room on screen. Use the target (not mid-tween) size.
    const int tw = m_targetSize.width(), th = m_targetSize.height();
    int x = caretTopLeft.x();
    int y = caretTopLeft.y() + lineHeight;
    if (QScreen* scr = QApplication::screenAt(caretTopLeft)) {
        const QRect g = scr->availableGeometry();
        if (y + th > g.bottom() && caretTopLeft.y() - th >= g.top())
            y = caretTopLeft.y() - th;
        if (x + tw > g.right()) x = qMax(g.left(), g.right() - tw);
    }
    move(x, y);
    if (!isVisible()) {
        show();
#ifdef Q_OS_MACOS
        // Qt::ToolTip sits above the Cmd-Tab switcher; lower it (on each show, as
        // Qt may reset the level) so the switcher draws on top.
        SonicPi::setPopupBelowSwitcher(reinterpret_cast<void*>(winId()));
#endif
    }
    return true;
}

void CompletionPopup::updateDetail()
{
    if (!m_detail || !m_piano) return;

    if (m_sliderMode) {
        // A single value slider replaces the list/detail/piano entirely.
        m_view->setVisible(false);
        m_detailPane->setVisible(false);
        m_piano->setVisible(false);
        m_rangeSlider->setVisible(true);
        return;
    }
    m_view->setVisible(true);
    m_rangeSlider->setVisible(false);

    const QModelIndex idx = m_view->currentIndex();
    const QString summary = idx.isValid() ? idx.data(SummaryRole).toString() : QString();
    const QString doc = idx.isValid() ? idx.data(DocRole).toString() : QString();
    const int note = idx.isValid() ? idx.data(NoteRole).toInt() : -1;

    if (m_chordMode) {
        // Plot the chord/scale's notes (tonic + offsets) on the keyboard.
        m_detailPane->setVisible(false);
        const QList<int> offs = idx.data(IntervalsRole).value<QList<int>>();
        QList<int> notes;
        if (note >= 0) for (int o : offs) notes.append(note + o);
        m_piano->setChordNotes(notes);
        m_piano->setVisible(true);
    } else if (m_noteMode && note >= 0) {
        // Tangible pitch: a mini keyboard highlighting the selected note. If a
        // chord/scale name follows on the line, the note carries its intervals,
        // so preview the whole chord/scale built on this root.
        m_detailPane->setVisible(false);
        const QList<int> offs = idx.data(IntervalsRole).value<QList<int>>();
        if (!offs.isEmpty()) {
            QList<int> notes;
            for (int o : offs) notes.append(note + o);
            m_piano->setChordNotes(notes);
        } else {
            m_piano->setNote(note);
        }
        m_piano->setVisible(true);
    } else if (m_hasDetail) {
        // The pane stays reserved for the whole session (stable width); each row
        // fills it with its summary heading + docstring, or a muted placeholder.
        m_piano->setVisible(false);
        // Skip the re-render (parse + layout) when the row's content is unchanged.
        const QString key = summary + QChar(0x1f) + doc;
        if (key != m_detailKey) {
            m_detailKey = key;
            // synth/fx docs are HTML (markdown can't colour code); else markdown.
            if (doc.trimmed().startsWith('<')) {
                QString html;
                if (!summary.isEmpty()) html += "<h3>" + summary.toHtmlEscaped() + "</h3>";
                html += doc;
                m_detail->setHtml(html);
            } else {
                QString md;
                if (!summary.isEmpty()) md += "### " + summary + "\n\n";
                md += doc;
                if (md.trimmed().isEmpty()) md = "_No documentation._";
                m_detail->setMarkdown(md);
            }
            m_detail->moveCursor(QTextCursor::Start);
        }
        // Offer a jump-to-docs affordance only when there's a real docstring.
        m_docsButton->setVisible(!doc.isEmpty());
        m_detailPane->setVisible(true);
    } else {
        m_detailPane->setVisible(false);
        m_piano->setVisible(false);
    }
}

void CompletionPopup::computeColumns()
{
    // The name column starts after the widest kind badge (notes have none).
    QFont badgeFont = m_view->font();
    badgeFont.setPointSizeF(badgeFont.pointSizeF() * 0.8);
    QFontMetrics bfm(badgeFont);

    int maxBadge = 0;
    for (int i = 0; i < m_model->rowCount(); ++i) {
        const QString kind = m_model->index(i, 0).data(KindRole).toString();
        if (!kind.isEmpty() && kind != "note")
            maxBadge = qMax(maxBadge, bfm.horizontalAdvance(kind) + 2 * kBadgeHPad);
    }
    m_nameColX = kRowHPad + (maxBadge > 0 ? maxBadge + kGap : 0);
}

void CompletionPopup::resizeToContents()
{
    const int rows = m_model->rowCount();
    if (rows == 0) return;

    QStyleOptionViewItem opt;
    opt.font = m_view->font();
    opt.palette = m_view->palette();

    int rowH = 0, contentW = 0;
    auto* delegate = static_cast<QStyledItemDelegate*>(m_view->itemDelegate());
    for (int i = 0; i < rows; ++i) {
        QSize s = delegate->sizeHint(opt, m_model->index(i, 0));
        rowH = qMax(rowH, s.height());
        contentW = qMax(contentW, s.width());
    }

    const int maxVisibleRows = 12;
    const int visible = qMin(rows, maxVisibleRows);
    const int listH = visible * rowH + 4;

    int maxListW = 560;
    if (QScreen* scr = QApplication::screenAt(pos()))
        maxListW = qMin(maxListW, scr->availableGeometry().width() / 2);
    int naturalW = qBound(160, contentW + 4, maxListW);
    if (rows > maxVisibleRows) naturalW += 16; // room for the scrollbar
    // Grow-only within a session: never shrink as filtering narrows the names,
    // so the popup width stops jittering keystroke to keystroke.
    m_sessionListW = qMax(m_sessionListW, naturalW);
    const int stickyW = m_sessionListW;

    // Manual geometry: pin the popup with setFixedSize and place each region by
    // hand so nothing (e.g. the QTextEdit's size hint) can renegotiate the width.
    // Gate on the session-mode flags, not isVisible() — a child reports invisible
    // until the top-level popup is first shown, which would skip placement.
    if (m_sliderMode) {
        // Just the slider — a compact, fixed-size value picker.
        const int w = 320;
        const int sh = QFontMetrics(m_rangeSlider->font()).height() * 3 + 22;
        m_rangeSlider->setGeometry(0, 0, w, sh);
        setPopupSize(w, sh);
    } else if (m_hasDetail) {
        // Give the docstring room even when only a few rows matched (a short list
        // shouldn't crop the docs); the list just gets empty space below. The pane
        // (and its min height) grow with the docstring font so larger zoom levels
        // don't crowd the prose — baseline ~12pt keeps the default layout, capped
        // at 2x and never narrower than half the screen.
        const double docScale = qBound(1.0, m_docPointSize / 12.0, 2.0);
        int detailW = int(kDetailW * docScale);
        if (QScreen* scr = QApplication::screenAt(pos()))
            detailW = qMin(detailW, scr->availableGeometry().width() / 2);
        const int h = qMax(listH, int(kDetailMinH * docScale));
        m_view->setGeometry(0, 0, stickyW, h);
        m_detailPane->setGeometry(stickyW, 0, detailW, h);
        setPopupSize(stickyW + detailW, h);
    } else if (m_noteMode || m_chordMode) {
        // Note / chord / scale lists are narrow with fixed content; use their own
        // width (not the grow-only session width, which can carry over from a wider
        // function list) and only widen enough to keep the keyboard usable. A
        // shorter list + a taller keyboard makes the piano the focus, not filler.
        const int w = qBound(300, naturalW, 380);
        const int nListH = qMin(rows, 9) * rowH + 4;
        const int pianoH = qBound(98, int(w * 0.36), 132);
        m_view->setGeometry(0, 0, w, nListH);
        m_piano->setGeometry(0, nListH, w, pianoH);
        setPopupSize(w, nListH + pianoH);
    } else {
        m_view->setGeometry(0, 0, stickyW, listH);
        setPopupSize(stickyW, listH);
    }
}

void CompletionPopup::setPopupSize(int w, int h)
{
    m_targetSize = QSize(w, h);
    // First appearance (or no change): snap. While on screen, tween between
    // shapes. setFixedSize pins it — the popup auto-sizes and isn't user-resizable.
    if (!isVisible() || size() == m_targetSize) {
        if (m_sizeAnim) m_sizeAnim->stop();
        setFixedSize(m_targetSize);
        return;
    }
    if (!m_sizeAnim) {
        m_sizeAnim = new QPropertyAnimation(this, "size", this);
        m_sizeAnim->setEasingCurve(QEasingCurve::OutCubic);
        // Re-pin once the tween settles (the relaxed constraints below let it run).
        connect(m_sizeAnim, &QPropertyAnimation::finished, this,
                [this]() { setFixedSize(m_targetSize); });
    }
    // Relax the fixed-size pin so the size property can be animated between shapes.
    setMinimumSize(0, 0);
    setMaximumSize(QWIDGETSIZE_MAX, QWIDGETSIZE_MAX);
    // Constant velocity: scale the duration to how far the popup actually has to
    // move, so a one-row filter and a full mode change feel like the same speed
    // (a fixed duration makes small changes crawl and big ones look instant).
    const int dist = qMax(qAbs(m_targetSize.width() - width()),
                          qAbs(m_targetSize.height() - height()));
    m_sizeAnim->stop();
    m_sizeAnim->setDuration(qBound(50, dist / 2, 160));
    m_sizeAnim->setStartValue(size());
    m_sizeAnim->setEndValue(m_targetSize);
    m_sizeAnim->start();
}

void CompletionPopup::moveSelection(int delta)
{
    if (m_sliderMode) { m_rangeSlider->nudge(delta); return; }
    const int rows = m_model->rowCount();
    if (rows == 0) return;
    int cur = m_view->currentIndex().row();
    if (cur < 0) cur = 0;
    int next = qBound(0, cur + delta, rows - 1);
    QModelIndex idx = m_model->index(next, 0);
    m_view->setCurrentIndex(idx);
    m_view->scrollTo(idx, QAbstractItemView::EnsureVisible);
}

QString CompletionPopup::currentText() const
{
    if (m_sliderMode) return m_rangeSlider->valueText();
    if (!m_noteOverride.isEmpty()) return m_noteOverride;  // a clicked off-list key
    QModelIndex idx = m_view->currentIndex();
    if (!idx.isValid()) return QString();
    return idx.data(InsertRole).toString();
}

bool CompletionPopup::isShowing() const
{
    return isVisible() && m_model->rowCount() > 0;
}

void CompletionPopup::hidePopup()
{
    if (m_sizeAnim) m_sizeAnim->stop();
    if (isVisible()) hide();
}
