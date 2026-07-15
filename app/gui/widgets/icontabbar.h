//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef ICONTABBAR_H
#define ICONTABBAR_H

#include <QStyleOptionTab>
#include <QStylePainter>
#include <QTabBar>
#include <QTabWidget>

// Tab bar for icon-only tabs: the style draws each tab's shape (so the
// theme stylesheet still applies), then the icon is painted dead-centre in
// the tab rect; QTabBar's own label layout start-aligns icons along the
// tab axis, which reads as misalignment on a vertical bar.
class IconTabBar : public QTabBar
{
public:
    using QTabBar::QTabBar;

    // Explicit square side for every tab (matches the transport/Link button
    // height so the sidebar thickness lines up with the rest of the chrome).
    void setSquareSide(int px)
    {
        m_side = px;
        updateGeometry();
    }

protected:
    // Force square tabs: the style's sizeHint (and QSS padding) transpose
    // awkwardly on a vertical (West) bar, so size each tab explicitly.
    QSize tabSizeHint(int index) const override
    {
        Q_UNUSED(index);
        const int side = m_side > 0 ? m_side : iconSize().width() * 3 / 2;
        return QSize(side, side);
    }

    QSize minimumTabSizeHint(int index) const override { return tabSizeHint(index); }

private:
    int m_side = 0;

    void paintEvent(QPaintEvent*) override
    {
        QStylePainter p(this);
        for (int i = 0; i < count(); ++i)
        {
            QStyleOptionTab opt;
            initStyleOption(&opt, i);
            const QIcon icon = opt.icon;
            opt.icon = QIcon();
            opt.text.clear();
            p.drawControl(QStyle::CE_TabBarTab, opt);
            if (icon.isNull())
                continue;
            const QSize isz = iconSize();
            const QPixmap pm = icon.pixmap(isz, devicePixelRatioF(), QIcon::Normal,
                                           i == currentIndex() ? QIcon::On : QIcon::Off);
            const QRect r = tabRect(i);
            const QRect target(r.x() + (r.width() - isz.width()) / 2,
                               r.y() + (r.height() - isz.height()) / 2,
                               isz.width(), isz.height());
            p.drawPixmap(target, pm);
        }
    }
};

// QTabWidget wired to an IconTabBar (setTabBar is protected).
class IconTabWidget : public QTabWidget
{
public:
    explicit IconTabWidget(QWidget* parent = nullptr)
        : QTabWidget(parent)
    {
        setTabBar(new IconTabBar(this));
    }
};

#endif // ICONTABBAR_H
