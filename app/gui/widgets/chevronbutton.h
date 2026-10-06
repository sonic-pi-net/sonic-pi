//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef CHEVRONBUTTON_H
#define CHEVRONBUTTON_H

#include <QColor>
#include <QEnterEvent>
#include <QEvent>
#include <QMoveEvent>
#include <QPointF>
#include <QToolButton>

// THE chevron control: a pill knob with Tabler's chevron stroked on it, the
// web's divider grip. Every chevron in the GUI is one of these (the help and
// metrics divider grips, the find bar's previous/next, a device's move
// earlier/later) or is drawn by its paintChevron (the quickstart's page-back
// bar), so a change to the design here changes it everywhere. A grip on a
// divider is laid over it; the divider itself is painted by Divider::paint
// (divider.h), and setHovering lets the two reveal as one control.
class QPainter;
class QPaintEvent;

class ChevronButton : public QToolButton
{
    Q_OBJECT
public:
    enum Dir { Up, Down, Left, Right };

    explicit ChevronButton(QWidget* parent = nullptr);

    // The knob's fill at rest and under the pointer, and the chevron's colour at
    // rest and under the pointer (the same colour when only one is given).
    void setColors(const QColor& grip, const QColor& hoverGrip, const QColor& glyph,
                   const QColor& hoverGlyph = QColor());

    void setDir(Dir d);

    // Force the hover look so the knob and the divider it overlays can
    // highlight together as one control.
    void setHovering(bool v);

    // The chevron itself, for a control that is not a button (the quickstart's
    // page-back bar): centred on c and pointing d. Every chevron in the GUI is
    // drawn by this one function.
    static void paintChevron(QPainter& p, const QPointF& c, Dir d, const QColor& colour);

protected:
    void enterEvent(QEnterEvent*) override { update(); }
    void leaveEvent(QEvent*) override { update(); }
    void moveEvent(QMoveEvent*) override { update(); }   // the pointer may no longer be over it
    void paintEvent(QPaintEvent*) override;

private:
    QColor m_grip{ "#444444" };
    QColor m_hoverGrip{ "#666666" };
    QColor m_glyph{ "#cccccc" };
    QColor m_hoverGlyph{ "#cccccc" };
    Dir m_dir = Down;
    bool m_extHover = false;      // externally forced hover (synced with the divider it sits on)
};

#endif // CHEVRONBUTTON_H
