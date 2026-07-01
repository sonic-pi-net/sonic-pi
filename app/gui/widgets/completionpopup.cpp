//--

// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "completionpopup.h"
#include "dpi.h"

#include <QListView>
#include <QStandardItemModel>
#include <QStyledItemDelegate>
#include <QTextEdit>
#include <QTextBrowser>
#include <QTextDocumentFragment>
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
#include <QPainterPath>
#include <QRadialGradient>
#include <QLinearGradient>
#include <QSet>
#include <QAccessible>
#include <QAccessibleWidget>
#include <cmath>

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
        // Notes and enum opt-values carry an inline summary (note pitch, or what an
        // enum value means); other kinds show their docstring in the pane instead.
        const bool inlineSummary = (kind == "note" || kind == "optval") && !summary.isEmpty();
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
        if (!kind.isEmpty() && kind != "note" && kind != "optval") {
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
        if ((kind == "note" || kind == "optval") && !summary.isEmpty()) {
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
    // Linear "text" step by a clean round amount (1/2/5 ×10ⁿ) so the value reads
    // tidily; Page keys pass ±10 for a coarse jump.
    void nudge(int steps) { setValue(m_value + steps * niceStep()); }
    // Logarithmic step: a proportional (multiplicative) move, natural for
    // frequency/amplitude ranges. Mirrors nudge()'s granularity in log space.
    void nudgeLog(int steps) {
        if (m_max <= 0) { nudge(steps); return; }   // log undefined → fall back
        const double lo = qMax(m_min, m_max * 1e-3);   // avoid log(0)/negatives
        const double v = qMax(m_value, lo);
        setValue(v * std::exp(steps * std::log(m_max / lo) / 40.0));
    }
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
    // A clean round increment (1/2/5 ×10ⁿ) ~1/40 of the range, so linear nudges
    // keep the value tidy (e.g. 0.05, not 0.025).
    double niceStep() const {
        const double raw = (m_max - m_min) / 40.0;
        if (!(raw > 0)) return 1.0;
        const double mag = std::pow(10.0, std::floor(std::log10(raw)));
        const double n = raw / mag;
        const double s = n < 1.5 ? 1.0 : n < 3.5 ? 2.0 : n < 7.5 ? 5.0 : 10.0;
        return s * mag;
    }
    double m_min = 0, m_max = 1, m_value = 0;
    QString m_label;
    QColor m_fg = QColor(220, 220, 220);
    QColor m_accent = QColor(0x9B, 0x59, 0xB6);
    std::function<void()> m_onAccept;
    std::function<void()> m_onChange;
};

// A live, QPainter-drawn diagram for a bounded opt, shown under the slider so a
// value is tangible (pan as a stereo field, cutoff as a filter response). Plain
// QWidget (no signals); the popup feeds it the slider's value via setValue().
class OptIllustration : public QWidget {
public:
    enum class Kind { None, Pan, Cutoff, Wave, Curve };

    // Registry: opt name → illustration. Add more here (res:, attack: …).
    static Kind kindFor(const QString& opt) {
        if (opt == "pan:")    return Kind::Pan;
        if (opt == "cutoff:") return Kind::Cutoff;
        return Kind::None;
    }
    // Enum-value illustrations (waveform / envelope-curve shapes), keyed by the
    // "illo" tag the API attaches to wave:/env_curve: value completions.
    static Kind enumKindFor(const QString& illo) {
        if (illo == "wave")  return Kind::Wave;
        if (illo == "curve") return Kind::Curve;
        return Kind::None;
    }

    explicit OptIllustration(QWidget* parent = nullptr) : QWidget(parent) {
        setFocusPolicy(Qt::NoFocus);
        setAttribute(Qt::WA_TransparentForMouseEvents);
    }
    void configure(Kind kind, double lo, double hi, double val) {
        m_kind = kind; m_min = lo; m_max = qMax(hi, lo + 1e-6);
        m_value = qBound(m_min, val, m_max);
        update();
    }
    void setValue(double v) { m_value = qBound(m_min, v, m_max); update(); }
    // A discrete enum-value shape (waveform/curve); `value` is the opt value.
    void configureEnum(Kind kind, int value) { m_kind = kind; m_enumValue = value; update(); }
    void setColors(const QColor& fg, const QColor& accent, const QColor& bg) {
        m_fg = fg; m_accent = accent; m_bg = bg; update();
    }

protected:
    void paintEvent(QPaintEvent*) override {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        const QRectF r = QRectF(rect()).adjusted(14, 8, -14, -10);
        if (r.width() < 20 || r.height() < 20) return;
        if (m_kind == Kind::Pan)         paintPan(p, r);
        else if (m_kind == Kind::Cutoff) paintCutoff(p, r);
        else if (m_kind == Kind::Wave)   paintWave(p, r);
        else if (m_kind == Kind::Curve)  paintCurve(p, r);
    }

private:
    // A hi-fi speaker: a sloped cabinet (narrower at the top, soft top→bottom
    // shade so it reads 3-D) with a tweeter and a woofer (surround + cone + dust
    // cap). `level` (0..1) lights the active side toward the accent as it pans.
    void drawSpeaker(QPainter& p, const QRectF& box, double level) const {
        const QColor base = mix(m_fg, m_bg, 34);
        const QColor lit  = mix(m_accent, base, int(12 + 60 * level));

        // Upright cabinet: a rounded rectangle (a real speaker box), with a soft
        // top→bottom shade and an inset front baffle so it reads 3-D.
        const double radius = box.width() * 0.16;
        QLinearGradient cg(box.topLeft(), box.bottomLeft());
        cg.setColorAt(0.0, mix(lit, m_bg, 78));
        cg.setColorAt(1.0, mix(lit, m_bg, 42));
        p.setPen(QPen(mix(m_fg, m_bg, 58), 1.4));
        p.setBrush(cg);
        p.drawRoundedRect(box, radius, radius);
        p.setPen(Qt::NoPen);
        p.setBrush(mix(m_bg, lit, 16));
        p.drawRoundedRect(box.adjusted(3, 3, -3, -3), radius * 0.7, radius * 0.7);

        const double cx = box.center().x();

        // Tweeter (small driver, upper third).
        const double twY = box.top() + box.height() * 0.26;
        const double twR = box.width() * 0.11;
        p.setBrush(mix(m_bg, base, 55)); p.setPen(QPen(mix(m_fg, m_bg, 50), 1.0));
        p.drawEllipse(QPointF(cx, twY), twR, twR);
        p.setBrush(mix(m_accent, base, int(25 + 55 * level))); p.setPen(Qt::NoPen);
        p.drawEllipse(QPointF(cx, twY), twR * 0.45, twR * 0.45);

        // Woofer (large driver, lower half): surround, cone, dust cap.
        const double wy = box.top() + box.height() * 0.62;
        const double wr = box.width() * 0.30;
        p.setPen(QPen(mix(m_fg, m_bg, 50), 1.0));
        p.setBrush(mix(m_bg, base, 62)); p.drawEllipse(QPointF(cx, wy), wr, wr);
        p.setPen(QPen(mix(m_fg, m_bg, 42), 0.8));
        p.setBrush(mix(m_bg, lit, 45));  p.drawEllipse(QPointF(cx, wy), wr * 0.64, wr * 0.64);
        p.setPen(Qt::NoPen);
        p.setBrush(mix(m_accent, m_fg, int(35 + 40 * level)));
        p.drawEllipse(QPointF(cx, wy), wr * 0.24, wr * 0.24);
    }

    // Concentric sound waves radiating from a speaker's inner edge toward the
    // centre. `dir` is +1 (rightward, from the left speaker) or -1 (leftward);
    // `level` (0..1) sets how bright/far they reach, so the louder side pushes
    // visibly more sound — the pan reads as relative strength, not a slider.
    void drawWaves(QPainter& p, const QPointF& origin, int dir, double level, double reach) const {
        if (level <= 0.02 || reach <= 4) return;
        const int rings = 4;
        const double span = 64.0;   // degrees of the radiating fan
        const double start = (dir > 0 ? -span / 2.0 : 180.0 - span / 2.0);
        p.setBrush(Qt::NoBrush);
        for (int i = 1; i <= rings; ++i) {
            const double t = double(i) / rings;
            const double rad = reach * t * (0.45 + 0.55 * level);
            QColor c = m_accent;
            c.setAlpha(int(170 * level * (1.0 - 0.6 * t)));
            p.setPen(QPen(c, 2.2));
            const QRectF arc(origin.x() - rad, origin.y() - rad, 2 * rad, 2 * rad);
            p.drawArc(arc, int(start * 16), int(span * 16));
        }
    }

    void paintPan(QPainter& p, const QRectF& r) const {
        const double frac = (m_value - m_min) / (m_max - m_min);   // 0=L .. 1=R
        const double lLevel = qBound(0.0, 1.0 - frac, 1.0);
        const double rLevel = qBound(0.0, frac, 1.0);

        const double spkW = qMin(52.0, r.width() * 0.20);
        const double spkH = qMin(r.height() * 0.66, spkW * 1.45);
        const double cy = r.top() + r.height() * 0.46;
        const QRectF lBox(r.left(), cy - spkH / 2, spkW, spkH);
        const QRectF rBox(r.right() - spkW, cy - spkH / 2, spkW, spkH);

        // Sound waves first (behind the cabinets): each side pushes sound toward
        // the centre in proportion to its level, so the pan reads as relative
        // loudness from the speakers — no draggable-looking track or dot.
        const double reach = (rBox.left() - lBox.right()) / 2.0 - 4;
        drawWaves(p, QPointF(lBox.right() + 2, cy), +1, lLevel, reach);
        drawWaves(p, QPointF(rBox.left() - 2, cy), -1, rLevel, reach);

        drawSpeaker(p, lBox, lLevel);
        drawSpeaker(p, rBox, rLevel);

        // L / R captions.
        QFont lf = font(); lf.setBold(true); p.setFont(lf);
        p.setPen(mix(m_fg, m_bg, 70));
        p.drawText(QRectF(lBox.left(), r.bottom() - 16, spkW, 16), Qt::AlignCenter, "L");
        p.drawText(QRectF(rBox.left(), r.bottom() - 16, spkW, 16), Qt::AlignCenter, "R");
    }

    static QString hzLabel(double hz) {
        if (hz >= 1000) return QString("≈ %1 kHz").arg(hz / 1000.0, 0, 'f', hz < 10000 ? 1 : 0);
        return QString("≈ %1 Hz").arg(hz, 0, 'f', 0);
    }

    void paintCutoff(QPainter& p, const QRectF& full) const {
        QFontMetrics fm(font());
        const QRectF r = full.adjusted(0, 0, 0, -fm.height() - 4);   // leave room for label

        // Cutoff is a MIDI note; midicps gives the real corner frequency.
        auto hzOf = [](double note) { return 440.0 * std::pow(2.0, (note - 69.0) / 12.0); };
        const double lLo = std::log10(hzOf(m_min)), lHi = std::log10(hzOf(m_max));
        const double fc = hzOf(m_value);
        const double lc = std::log10(fc);
        const double knee = r.left() + (lc - lLo) / (lHi - lLo) * r.width();

        // Illustrative spectrum (NOT a precise filter response): bars across the
        // frequency range, bright and full below the cutoff, fading away above it.
        // Synths use different filters (LPF/RLPF ~2-pole, BLowPass4 ~4-pole) so the
        // roll-off only conveys the idea — low cutoff = darker, high = brighter.
        const int bars = 28;
        const double gap = 2.0;
        const double bw = (r.width() - gap * (bars - 1)) / bars;
        for (int i = 0; i < bars; ++i) {
            const double bx = r.left() + i * (bw + gap);
            const double l = lLo + (i + 0.5) / bars * (lHi - lLo);   // this bar's log-freq
            const double pass = l <= lc ? 1.0 : std::exp(-(l - lc) * 3.0);
            const double h = r.height() * qBound(0.05, 0.10 + 0.90 * pass, 1.0);
            p.setPen(Qt::NoPen);
            p.setBrush(l <= lc ? m_accent : mix(m_accent, m_bg, 30));
            p.drawRoundedRect(QRectF(bx, r.bottom() - h, bw, h), 1.5, 1.5);
        }

        // Cutoff position + the real corner frequency.
        p.setPen(QPen(mix(m_fg, m_bg, 75), 1.4, Qt::DashLine));
        p.drawLine(QPointF(knee, r.top()), QPointF(knee, r.bottom()));
        QFont lf = font(); lf.setBold(true); p.setFont(lf);
        p.setPen(mix(m_fg, m_bg, 85));
        p.drawText(QRectF(full.left(), full.bottom() - fm.height(), full.width(), fm.height()),
                   Qt::AlignCenter, QString("cutoff %1  (%2)")
                       .arg(int(m_value)).arg(hzLabel(fc)));
    }

    // One cycle or two of the selected waveform (0 saw, 1 pulse, 2 triangle,
    // 3 sine, 4+ noise) drawn on a centre line — the shape of the timbre.
    void paintWave(QPainter& p, const QRectF& full) const {
        const QRectF r = full.adjusted(0, 0, 0, -2);
        const double midY = r.center().y(), amp = r.height() * 0.38, cycles = 2.0;
        p.setPen(QPen(mix(m_fg, m_bg, 24), 1.0));
        p.drawLine(QPointF(r.left(), midY), QPointF(r.right(), midY));   // baseline

        QPainterPath path;
        const int N = 280;
        for (int i = 0; i <= N; ++i) {
            const double t = double(i) / N;
            const double ph = t * cycles, frac = ph - std::floor(ph);
            double y;
            if (m_enumValue == 0)      y = 1.0 - 2.0 * frac;                                  // saw
            else if (m_enumValue == 1) y = frac < 0.5 ? 1.0 : -1.0;                           // pulse
            else if (m_enumValue == 2) y = frac < 0.5 ? 1.0 - 4.0 * frac : -3.0 + 4.0 * frac; // triangle
            else if (m_enumValue == 3) y = std::sin(ph * 2.0 * kPi);                          // sine
            else {                                                                            // noise
                const double s = std::sin((i + 1) * 12.9898) * 43758.5453;
                y = (s - std::floor(s)) * 2.0 - 1.0;
            }
            const QPointF pt(r.left() + t * r.width(), midY - y * amp);
            if (i == 0) path.moveTo(pt); else path.lineTo(pt);
        }
        p.setPen(QPen(m_accent, 2.2));
        p.setBrush(Qt::NoBrush);
        p.drawPath(path);
    }

    // The envelope-segment shape between two levels for the selected curve
    // (1 linear, 2 exponential, 3 sine, 4 welch, 6 squared, 7 cubed).
    void paintCurve(QPainter& p, const QRectF& full) const {
        const QRectF r = full.adjusted(0, 2, 0, -2);
        auto shape = [&](double t) -> double {
            switch (m_enumValue) {
                case 2:  return (std::exp(t * 2.2) - 1.0) / (std::exp(2.2) - 1.0); // exponential
                case 3:  return 0.5 - 0.5 * std::cos(t * kPi);                     // sine
                case 4:  return std::sin(t * kPi / 2.0);                           // welch
                case 6:  return t * t;                                            // squared
                case 7:  return t * t * t;                                        // cubed
                default: return t;                                                // linear (1)
            }
        };
        p.setPen(QPen(mix(m_fg, m_bg, 24), 1.0));
        p.drawLine(QPointF(r.left(), r.bottom()), QPointF(r.right(), r.bottom()));

        QPainterPath path;
        const int N = 160;
        for (int i = 0; i <= N; ++i) {
            const double t = double(i) / N;
            const QPointF pt(r.left() + t * r.width(), r.bottom() - shape(t) * r.height());
            if (i == 0) path.moveTo(pt); else path.lineTo(pt);
        }
        p.setPen(QPen(m_accent, 2.4));
        p.setBrush(Qt::NoBrush);
        p.drawPath(path);
        p.setPen(Qt::NoPen);
        p.setBrush(m_accent);
        p.drawEllipse(QPointF(r.left(), r.bottom()), 3, 3);
        p.drawEllipse(QPointF(r.right(), r.top()), 3, 3);
    }

    static constexpr double kPi = 3.14159265358979323846;
    Kind m_kind = Kind::None;
    double m_min = 0, m_max = 1, m_value = 0;
    int m_enumValue = 0;
    QColor m_fg = QColor(220, 220, 220);
    QColor m_accent = QColor(0x9B, 0x59, 0xB6);
    QColor m_bg = QColor(30, 30, 30);
};

namespace {

// The popup is a top-level window, so a screen reader otherwise reports it as a
// new "dialog" and shifts its review context onto it — silencing the editor's
// typed-character echo. This interface prunes the popup and its children from the
// accessibility tree on all platforms; focus stays in the editor and suggestions
// are spoken via announcements instead.
class IgnoredAccessible : public QAccessibleWidget
{
public:
    explicit IgnoredAccessible(QWidget* w) : QAccessibleWidget(w, QAccessible::NoRole) {}
    QAccessible::State state() const override
    {
        QAccessible::State s = QAccessibleWidget::state();
        s.invisible = 1;   // bridges skip invisible/offscreen nodes
        s.offscreen = 1;
        return s;
    }
    int childCount() const override { return 0; }              // prune children too
    QAccessibleInterface* child(int) const override { return nullptr; }
};

QAccessibleInterface* completionPopupAccessibleFactory(const QString& classname, QObject* object)
{
    if (object && object->isWidgetType()
        && classname == QLatin1String(CompletionPopup::staticMetaObject.className()))
        return new IgnoredAccessible(static_cast<QWidget*>(object));
    return nullptr;   // not ours — let Qt's default factory handle it
}

} // namespace

CompletionPopup::CompletionPopup(QWidget* parent)
    : QWidget(parent)
{
    // Register the accessibility-ignore factory once (the first popup created).
    [[maybe_unused]] static const bool s_a11yFactoryInstalled =
        (QAccessible::installFactory(completionPopupAccessibleFactory), true);

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
    m_detail->document()->setDocumentMargin(ScaleWidthForDPI(20));  // text inset; keeps the scrollbar flush
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

    m_optIllo = new OptIllustration(this);
    m_optIllo->setObjectName("completionIllo");
    m_optIllo->setVisible(false);

    // Shape illustration shown at the top of the detail pane for enum values that
    // have one (waveform for wave:, envelope shape for env_curve:).
    m_shapeIllo = new OptIllustration;
    m_shapeIllo->setObjectName("completionShapeIllo");
    m_shapeIllo->setMinimumHeight(70);
    m_shapeIllo->setVisible(false);

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
    paneLayout->addWidget(m_shapeIllo, 0);
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
        " border-top: 1px solid rgba(127,127,127,60); }"
        "#completionIllo { background: transparent;"
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
    // Dragging the slider live-previews its value in the buffer and drives the
    // illustration so its diagram tracks the value.
    m_rangeSlider->setOnChange([this]() {
        if (m_hasIllo) m_optIllo->setValue(m_rangeSlider->valueText().toDouble());
        emit previewChanged(m_rangeSlider->valueText());
    });

    // Realise the native window up front so the first move() (which happens
    // before the first show()) targets a real window: macOS drops pre-creation
    // geometry, so without this the first popup ignores its position and lands
    // over the caret instead of below it.
    //
    // macOS only: on Windows, realising the HWND this early — before the first
    // show() — creates the native window before the translucent/layered
    // compositing is set up for this frameless WA_TranslucentBackground tooltip,
    // so it "shows" at the right size/position but paints nothing (an invisible
    // popup). Windows keeps geometry across the pre-show move(), so it doesn't
    // need this anyway.
#ifdef Q_OS_MACOS
    createWinId();
#endif
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
        "#completionIllo { background: transparent; border-top: 1px solid %2; }"
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
    if (m_optIllo) m_optIllo->setColors(fg, selBg, bg);
    if (m_shapeIllo) m_shapeIllo->setColors(fg, selBg, bg);
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
    if (m_optIllo) m_optIllo->setFont(font);
    if (m_shapeIllo) m_shapeIllo->setFont(font);
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
    m_enumIllo = items.isEmpty() ? QString() : items.first().illo;
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
        const OptIllustration::Kind k = OptIllustration::kindFor(it.text);
        m_hasIllo = (k != OptIllustration::Kind::None);
        if (m_hasIllo) m_optIllo->configure(k, it.rmin, it.rmax, it.rdefault);
    } else {
        m_hasIllo = false;
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
        SonicPi::setWindowAccessibilityIgnored(reinterpret_cast<void*>(winId()));
#endif
    }
    return true;
}

void CompletionPopup::updateDetail()
{
    if (!m_detail || !m_piano) return;

    if (m_sliderMode) {
        // A single value slider replaces the list/detail/piano entirely; opts with
        // an illustration also show a live diagram beneath it.
        m_view->setVisible(false);
        m_detailPane->setVisible(false);
        m_piano->setVisible(false);
        m_rangeSlider->setVisible(true);
        m_optIllo->setVisible(m_hasIllo);
        return;
    }
    m_view->setVisible(true);
    m_rangeSlider->setVisible(false);
    m_optIllo->setVisible(false);

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
        // Enum value with a shape (waveform / envelope curve): draw it atop the doc.
        const OptIllustration::Kind sk = OptIllustration::enumKindFor(m_enumIllo);
        if (sk != OptIllustration::Kind::None && idx.isValid()) {
            m_shapeIllo->configureEnum(sk, idx.data(Qt::DisplayRole).toString().toInt());
            m_shapeIllo->setVisible(true);
        } else {
            m_shapeIllo->setVisible(false);
        }
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
        // The slider — a compact value picker — with the illustration (if any)
        // stacked beneath it so the diagram tracks the slider live.
        const int w = m_hasIllo ? 380 : 320;
        const int sh = QFontMetrics(m_rangeSlider->font()).height() * 3 + 22;
        m_rangeSlider->setGeometry(0, 0, w, sh);
        if (m_hasIllo) {
            const int ih = 168;
            m_optIllo->setGeometry(0, sh, w, ih);
            setPopupSize(w, sh + ih);
        } else {
            setPopupSize(w, sh);
        }
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

void CompletionPopup::announceSelection()
{
    emit announceRequested(currentAnnouncement());
}

void CompletionPopup::sliderNudgeLog(int steps)
{
    if (!m_sliderMode) return;
    const QString before = m_rangeSlider->valueText();
    m_rangeSlider->nudgeLog(steps);
    if (m_rangeSlider->valueText() != before) announceSelection();
}

void CompletionPopup::moveSelection(int delta)
{
    if (m_sliderMode) {
        const QString before = m_rangeSlider->valueText();
        m_rangeSlider->nudge(delta);
        if (m_rangeSlider->valueText() != before) announceSelection();
        return;
    }
    const int rows = m_model->rowCount();
    if (rows == 0) return;
    int cur = m_view->currentIndex().row();
    if (cur < 0) cur = 0;
    int next = qBound(0, cur + delta, rows - 1);
    if (next == cur) return;   // boundary: selection unchanged, don't re-announce
    QModelIndex idx = m_model->index(next, 0);
    m_view->setCurrentIndex(idx);
    m_view->scrollTo(idx, QAbstractItemView::EnsureVisible);
    announceSelection();
}

QString CompletionPopup::currentText() const
{
    if (m_sliderMode) return m_rangeSlider->valueText();
    if (!m_noteOverride.isEmpty()) return m_noteOverride;  // a clicked off-list key
    QModelIndex idx = m_view->currentIndex();
    if (!idx.isValid()) return QString();
    return idx.data(InsertRole).toString();
}

QString CompletionPopup::currentAnnouncement() const
{
    if (m_sliderMode) return m_rangeSlider->valueText();
    const int rows = m_model->rowCount();
    const QModelIndex idx = m_view->currentIndex();
    if (rows == 0 || !idx.isValid()) return QString();
    const QString name = idx.data(Qt::DisplayRole).toString();
    const QString kind = idx.data(KindRole).toString();
    const QString summary = idx.data(SummaryRole).toString();
    // "prophet, synth, analogue-style synth, 1 of 5" — name, kind, summary (the
    // detail a screen reader can't see in the docs pane), position.
    QString s = name;
    if (!kind.isEmpty()) s += QStringLiteral(", ") + kind;
    if (!summary.isEmpty()) s += QStringLiteral(", ") + summary;
    s += QStringLiteral(", ") + tr("%1 of %2").arg(idx.row() + 1).arg(rows);
    return s;
}

// The current item's full docstring as plain text — the same content shown in the
// docs pane, for a screen reader to hear on demand (the pane itself is pruned from
// the a11y tree). Empty when there's no doc or no selection.
QString CompletionPopup::currentDoc() const
{
    const QModelIndex idx = m_view->currentIndex();
    if (!idx.isValid()) return QString();
    const QString html = idx.data(DocRole).toString();
    if (html.isEmpty()) return QString();
    return QTextDocumentFragment::fromHtml(html).toPlainText().simplified();
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
