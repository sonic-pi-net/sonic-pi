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

#ifndef DIVIDER_H
#define DIVIDER_H

#include <QColor>
#include <QEvent>
#include <QPainter>
#include <QPaintEvent>
#include <QRect>
#include <QWidget>

// THE divider: one painter for every divider in the GUI. A wide grab band
// (kExtent, so a pointer finds it) that paints only a thin centred line at
// rest and reveals its full width in the hover colour when pointed at. QSS
// can't do this (a gradient handle renders solid), so each kind of divider
// — a QSplitter handle (ThinSplitter), a QMainWindow dock separator
// (DividerProxyStyle), a bar of its own (DividerBar) — calls Divider::paint
// and nothing else: a change here changes them all. The grips that sit on a
// divider are ChevronButton, the one chevron control.
namespace Divider
{
// Fixed px, not DPI-scaled, so the reveal stays clearly wider than the resting
// line on macOS's sub-1.0 display scale.
constexpr int kExtent = 7;   // the grab band
constexpr int kLine = 2;     // the resting line

// bg blends the band with the panes beside it; line is the resting centre
// line; hover fills the whole band when pointed at.
struct Colours
{
    QColor bg;
    QColor line{ 128, 128, 128 };
    QColor hover{ 170, 170, 170 };
};

// bar is the way the bar runs: Horizontal between stacked panes, Vertical
// between panes side by side. lineVisible false (a collapsed pane) paints no
// resting line; the band still reveals on hover, so the way back is pointed at.
inline void paint(QPainter& p, const QRect& r, Qt::Orientation bar, bool hover, bool lineVisible,
                  const Colours& c)
{
    if (hover)
    {
        p.fillRect(r, c.hover);   // reveal full thickness
        return;
    }
    if (c.bg.isValid())
        p.fillRect(r, c.bg);      // blend the wide grab area
    if (!lineVisible)
        return;
    if (bar == Qt::Horizontal)
    {
        const int lh = qMin(kLine, r.height());
        p.fillRect(r.x(), r.y() + (r.height() - lh) / 2, r.width(), lh, c.line);
    }
    else
    {
        const int lw = qMin(kLine, r.width());
        p.fillRect(r.x() + (r.width() - lw) / 2, r.y(), lw, r.height(), c.line);
    }
}
} // namespace Divider

// A divider that is a widget of its own, where there is no splitter or dock
// separator to paint: the bar at the foot of the editor while the help panel
// is away, which the web keeps there as the way back. Its grips overlay it
// (ChevronButton); setForcedHover lets them reveal it as one control.
class DividerBar : public QWidget
{
public:
    explicit DividerBar(Qt::Orientation bar, QWidget* parent = nullptr)
        : QWidget(parent), m_bar(bar)
    {
        setAttribute(Qt::WA_Hover, true);
        if (bar == Qt::Horizontal)
            setFixedHeight(Divider::kExtent);
        else
            setFixedWidth(Divider::kExtent);
    }

    void setColours(const Divider::Colours& c) { m_colours = c; update(); }
    void setLineVisible(bool v) { if (m_lineVisible != v) { m_lineVisible = v; update(); } }
    void setForcedHover(bool v) { if (m_forcedHover != v) { m_forcedHover = v; update(); } }
    Qt::Orientation bar() const { return m_bar; }

protected:
    bool event(QEvent* e) override
    {
        switch (e->type())
        {
        case QEvent::Enter:
        case QEvent::HoverEnter: m_hover = true;  update(); break;
        case QEvent::Leave:
        case QEvent::HoverLeave: m_hover = false; update(); break;
        default: break;
        }
        return QWidget::event(e);
    }
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        Divider::paint(p, rect(), m_bar, m_hover || m_forcedHover, m_lineVisible, m_colours);
    }

private:
    Qt::Orientation m_bar;
    Divider::Colours m_colours;
    bool m_hover = false;
    bool m_forcedHover = false;
    bool m_lineVisible = true;
};

#endif // DIVIDER_H
