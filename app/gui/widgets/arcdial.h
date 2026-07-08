//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef ARCDIAL_H
#define ARCDIAL_H

#include <QDial>
#include <QMouseEvent>
#include <cmath>
#include <QPainter>
#include <QPaintEvent>
#include <QFont>
#include <QPen>
#include <QColor>

// A rotary control (used for the prefs volume + hue dials). Grab it anywhere and
// tune RELATIVELY: the click doesn't jump the value to the clicked angle — it
// anchors, and dragging around the wheel nudges the value up/down from where it
// was, so it feels tunable and the "off" position stays put. Angles near the hub
// are ignored (they swing wildly). Shift for fine control; scroll wheel and
// arrow keys inherited from QDial. Paints a palette-driven arc track + value
// arc + centre value.
class ArcDial : public QDial {
public:
    explicit ArcDial(QWidget* parent = nullptr) : QDial(parent) {
        setCursor(Qt::PointingHandCursor);
    }
    // Point size of the centre value text (default suits the prefs volume dial;
    // smaller dials should reduce it so the value fits).
    void setValueFontPt(int pt) { m_valueFontPt = pt; update(); }
    // Override the value-arc / text colour (default: the palette highlight). Used
    // by the hue dial to preview the (rotated) accent colour.
    void setArcColor(const QColor& c) { m_arcColor = c; update(); }
    // Append a suffix to the centre value text (e.g. "°").
    void setValueSuffix(const QString& s) { m_valueSuffix = s; update(); }
    // Whether to draw the numeric value in the centre (default on).
    void setShowValue(bool b) { m_showValue = b; update(); }
protected:
    void mousePressEvent(QMouseEvent* e) override {
        if (e->button() == Qt::LeftButton) {
            // Grab where you click WITHOUT changing the value — then tune from
            // there. (Anchoring, not jump-to-angle, so the current value keeps a
            // sense of "I can nudge this".)
            setSliderDown(true);
            anchorAngle(e->position());
            m_pendingSteps = 0.0;
            e->accept();
            return;
        }
        QDial::mousePressEvent(e);
    }
    void mouseMoveEvent(QMouseEvent* e) override {
        if (isSliderDown()) {
            // Ignore erratic angles near the hub (tiny moves there swing wildly);
            // re-anchor when the pointer comes back out.
            if (radiusFrac(e->position()) < 0.18) { m_haveAngle = false; e->accept(); return; }
            const double a = angleAt(e->position());
            if (m_haveAngle) {
                double d = a - m_lastAngle;
                while (d > 180.0)  d -= 360.0;   // shortest way round
                while (d < -180.0) d += 360.0;
                // The 270° arc runs clockwise (Qt angle decreasing) as the value
                // rises, so a clockwise drag (negative d) increases the value.
                double steps = -d / 270.0 * (maximum() - minimum());
                // Shift = fine control (quarter speed).
                if (e->modifiers() & Qt::ShiftModifier) steps *= 0.25;
                // Accumulate fractionally so slow drags (sub-step deltas per
                // event) still add up; consume only the whole steps and carry
                // the remainder.
                m_pendingSteps += steps;
                const double whole = std::trunc(m_pendingSteps);
                if (whole != 0.0) {
                    setValue(value() + static_cast<int>(whole));
                    m_pendingSteps -= whole;
                }
            }
            m_lastAngle = a;
            m_haveAngle = true;
            e->accept();
            return;
        }
        QDial::mouseMoveEvent(e);
    }
    void mouseReleaseEvent(QMouseEvent* e) override {
        if (e->button() == Qt::LeftButton && isSliderDown()) {
            setSliderDown(false);
            m_haveAngle = false;
            e->accept();
            return;
        }
        QDial::mouseReleaseEvent(e);
    }
    // Angle (Qt convention: 0deg = 3 o'clock, CCW positive) of a point about the
    // widget centre, in [0,360).
    double angleAt(const QPointF& pt) const {
        double a = std::atan2(height() / 2.0 - pt.y(), pt.x() - width() / 2.0)
                   * 180.0 / 3.14159265358979323846;
        return a < 0.0 ? a + 360.0 : a;
    }
    // Distance from centre as a fraction of the dial radius.
    double radiusFrac(const QPointF& pt) const {
        const double dx = pt.x() - width() / 2.0, dy = pt.y() - height() / 2.0;
        const double maxr = qMin(width(), height()) / 2.0;
        return maxr > 0.0 ? std::sqrt(dx * dx + dy * dy) / maxr : 0.0;
    }
    void anchorAngle(const QPointF& pt) {
        if (radiusFrac(pt) < 0.18) { m_haveAngle = false; return; }
        m_lastAngle = angleAt(pt);
        m_haveAngle = true;
    }
    void paintEvent(QPaintEvent*) override {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);

        int side = qMin(width(), height());
        int margin = 8;
        qreal ox = (width() - side) / 2.0;
        qreal oy = (height() - side) / 2.0;
        QRectF arc(ox + margin, oy + margin, side - 2 * margin, side - 2 * margin);

        // Palette-driven so all five themes render correctly (or an explicit
        // override for the hue-preview dial).
        QColor track(127, 127, 127, 70);
        QColor accent = m_arcColor.isValid() ? m_arcColor : palette().color(QPalette::Highlight);

        // Background track
        p.setPen(QPen(track, 6, Qt::SolidLine, Qt::RoundCap));
        p.drawArc(arc, 225 * 16, -270 * 16);

        // Value arc
        double frac = 0.0;
        if (maximum() > minimum())
            frac = double(value() - minimum()) / double(maximum() - minimum());
        int span = -static_cast<int>(frac * 270 * 16);
        p.setPen(QPen(accent, 6, Qt::SolidLine, Qt::RoundCap));
        p.drawArc(arc, 225 * 16, span);

        // Value text
        if (m_showValue) {
            p.setPen(accent);
            p.setFont(QFont("Hack", m_valueFontPt, QFont::Bold));
            p.drawText(rect(), Qt::AlignCenter, QString::number(value()) + m_valueSuffix);
        }
    }
private:
    double m_lastAngle = 0.0;     // last pointer angle during a relative drag
    bool m_haveAngle = false;     // false = re-anchor (drag start or near-hub)
    double m_pendingSteps = 0.0;  // fractional steps carried between move events
    int m_valueFontPt = 14;
    bool m_showValue = true;
    QColor m_arcColor;
    QString m_valueSuffix;
};

#endif // ARCDIAL_H
