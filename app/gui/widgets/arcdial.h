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
#include <QPainter>
#include <QPaintEvent>
#include <QFont>
#include <QPen>
#include <QColor>

// A rotary control (used for the prefs volume dial). DAW-style:
// drag vertically to change the value relative to where it started (no jump-to-
// angle, no circular drag), Shift for fine control; scroll wheel and arrow keys
// inherited from QDial. Paints a palette-driven arc track + value arc + the
// numeric value in the centre, so it reads correctly in every theme.
class ArcDial : public QDial {
public:
    explicit ArcDial(QWidget* parent = nullptr) : QDial(parent) {
        setCursor(Qt::SizeVerCursor);
    }
    // Point size of the centre value text (default suits the prefs volume dial;
    // smaller dials should reduce it so the value fits).
    void setValueFontPt(int pt) { m_valueFontPt = pt; update(); }
protected:
    void mousePressEvent(QMouseEvent* e) override {
        if (e->button() == Qt::LeftButton) {
            m_dragStartY = e->position().y();
            m_dragStartValue = value();
            setSliderDown(true);
            e->accept();
            return;
        }
        QDial::mousePressEvent(e);
    }
    void mouseMoveEvent(QMouseEvent* e) override {
        if (isSliderDown()) {
            double pixelsForFullRange =
                (e->modifiers() & Qt::ShiftModifier) ? 800.0 : 200.0;
            double delta = (m_dragStartY - e->position().y())
                * (maximum() - minimum()) / pixelsForFullRange;
            setValue(m_dragStartValue + static_cast<int>(delta));
            e->accept();
            return;
        }
        QDial::mouseMoveEvent(e);
    }
    void mouseReleaseEvent(QMouseEvent* e) override {
        if (e->button() == Qt::LeftButton && isSliderDown()) {
            setSliderDown(false);
            e->accept();
            return;
        }
        QDial::mouseReleaseEvent(e);
    }
    void paintEvent(QPaintEvent*) override {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);

        int side = qMin(width(), height());
        int margin = 8;
        QRectF arc(margin, margin, side - 2 * margin, side - 2 * margin);

        // Palette-driven so all five themes render correctly.
        QColor track(127, 127, 127, 70);
        QColor accent = palette().color(QPalette::Highlight);

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
        p.setPen(accent);
        p.setFont(QFont("Hack", m_valueFontPt, QFont::Bold));
        p.drawText(rect(), Qt::AlignCenter, QString::number(value()));
    }
private:
    double m_dragStartY = 0.0;
    int m_dragStartValue = 0;
    int m_valueFontPt = 14;
};

#endif // ARCDIAL_H
