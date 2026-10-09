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

#ifndef AWAYDOCK_H
#define AWAYDOCK_H

#include <QDockWidget>
#include <QPalette>
#include <QColor>
#include <QMainWindow>
#include <QRect>
#include <QSize>
#include <QString>
#include <QWidget>

// With a panel away its dock separator would go with it; the web keeps the
// divider as the way back. A placeholder dock takes the panel's place in its
// area while it is away: one fixed pixel, painting nothing. Qt lays out a
// separator's extent beside a fixed-size dock but paints nothing there
// (QDockAreaLayoutInfo::paintSeparators: `!item.hasFixedSize(o)`), so a
// DividerOverlay (divider.h) is laid over that rect — awaySeparatorRect gives
// it, straight from Qt's own separatorRect — and paints the one divider, with
// the grip over it. gui-tests pins all of it by the pixel.
namespace SonicPi
{
class AwayDockBody : public QWidget
{
public:
    // Fixed at a pixel across the divider; free along it, so the dock — and
    // the separator Qt reserves beside it — runs the whole of its area.
    explicit AwayDockBody(Qt::Orientation across, QWidget* parent = nullptr) : QWidget(parent)
    {
        if (across == Qt::Vertical) setFixedWidth(1);    // a column at the side
        else                        setFixedHeight(1);   // a row at the foot
        setAutoFillBackground(true);   // the divider's background, set by setAwayDockColour
    }
};

// No title row: a row of no height. The dock adds its title's size hint to
// its own minimum, and a widget without a layout hints -1, which made the
// minimum (1, -1). Qt 6.4 sets that as it is, and warns.
class AwayDockTitle : public QWidget
{
public:
    explicit AwayDockTitle(QWidget* parent = nullptr) : QWidget(parent) { setFixedHeight(0); }
    QSize sizeHint() const override { return QSize(0, 0); }
};

inline QDockWidget* makeAwayDock(QMainWindow* window, const QString& name, Qt::DockWidgetArea area)
{
    auto* dock = new QDockWidget(window);
    dock->setObjectName(name);
    dock->setFeatures(QDockWidget::NoDockWidgetFeatures);
    dock->setAllowedAreas(area);
    dock->setTitleBarWidget(new AwayDockTitle(dock));
    const bool vertical = (area == Qt::LeftDockWidgetArea || area == Qt::RightDockWidgetArea);
    dock->setWidget(new AwayDockBody(vertical ? Qt::Vertical : Qt::Horizontal, dock));
    if (vertical) dock->setFixedWidth(1);
    else          dock->setFixedHeight(1);
    dock->setContentsMargins(0, 0, 0, 0);
    dock->setAccessibleName(QString());   // nothing to read: the grip is the control
    window->addDockWidget(area, dock);
    dock->hide();
    return dock;
}

// The placeholder's one pixel in the divider's background colour, so it is
// part of the divider and not a line of its own.
inline void setAwayDockColour(QDockWidget* placeholder, const QColor& background)
{
    for (QWidget* w : { static_cast<QWidget*>(placeholder), placeholder->widget() })
    {
        if (!w) continue;
        QPalette pal = w->palette();
        pal.setColor(QPalette::Window, background);
        w->setPalette(pal);
        w->setAutoFillBackground(true);
    }
}

// The separator Qt reserves beside the placeholder, in the window's
// coordinates: QDockAreaLayout::separatorRect, by area.
inline QRect awaySeparatorRect(const QDockWidget* placeholder, Qt::DockWidgetArea area, int extent)
{
    const QRect p = placeholder->geometry();
    switch (area)
    {
    case Qt::LeftDockWidgetArea:   return QRect(p.right() + 1, p.top(), extent, p.height());
    case Qt::RightDockWidgetArea:  return QRect(p.left() - extent, p.top(), extent, p.height());
    case Qt::TopDockWidgetArea:    return QRect(p.left(), p.bottom() + 1, p.width(), extent);
    case Qt::BottomDockWidgetArea: return QRect(p.left(), p.top() - extent, p.width(), extent);
    default:                       return QRect();
    }
}
} // namespace SonicPi

#endif // AWAYDOCK_H
