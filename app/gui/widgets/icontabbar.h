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

#include <QEvent>
#include <QStyleOptionTab>
#include <QStylePainter>
#include <QTabBar>
#include <QTabWidget>

#include <algorithm>

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

    // A vertical rail is never narrower than what sits at its foot
    // (IconTabWidget::setFootWidget): the tabs widen to it rather than the
    // foot hanging over the edge, which it does wherever the style draws the
    // rail a pixel narrower than the foot's buttons.
    void setMinimumRailWidth(int px)
    {
        if (px == m_railMin) return;
        m_railMin = px;
        updateGeometry();
    }

protected:
    // Force square tabs: the style's sizeHint (and QSS padding) transpose
    // awkwardly on a vertical (West) bar, so size each tab explicitly.
    QSize tabSizeHint(int index) const override
    {
        Q_UNUSED(index);
        const int side = m_side > 0 ? m_side : iconSize().width() * 3 / 2;
        const bool vertical = shape() == RoundedWest || shape() == RoundedEast
                           || shape() == TriangularWest || shape() == TriangularEast;
        return QSize(vertical ? std::max(side, m_railMin) : side, side);
    }

    QSize minimumTabSizeHint(int index) const override { return tabSizeHint(index); }

private:
    int m_side = 0;
    int m_railMin = 0;

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

// QTabWidget wired to an IconTabBar (setTabBar is protected), with an
// optional foot: a widget kept at the foot of the tab bar's column, as the
// web keeps the help's text-size controls at the foot of its rail. It sits
// as low as the column allows, centred across it, and never over a tab:
// when the column is too short it follows straight on from the last one.
class IconTabWidget : public QTabWidget
{
public:
    explicit IconTabWidget(QWidget* parent = nullptr)
        : QTabWidget(parent)
    {
        setTabBar(new IconTabBar(this));
        tabBar()->installEventFilter(this);
    }

    void setFootWidget(QWidget* foot)
    {
        m_foot = foot;
        m_foot->setParent(this);
        m_foot->installEventFilter(this);
        m_foot->show();
        placeFoot();
    }

    QWidget* footWidget() const { return m_foot; }

protected:
    void resizeEvent(QResizeEvent* event) override
    {
        QTabWidget::resizeEvent(event);
        placeFoot();
    }

    // The tab bar moves or resizes as tabs come and go or its tabs change
    // size; the foot's size changes as the controls in it are shown and
    // hidden. Either way the foot is placed again.
    bool eventFilter(QObject* obj, QEvent* event) override
    {
        const bool moved = obj == tabBar()
            && (event->type() == QEvent::Resize || event->type() == QEvent::Move);
        const bool refit = m_foot && obj == m_foot && event->type() == QEvent::LayoutRequest;
        if (moved || refit)
            placeFoot();
        return QTabWidget::eventFilter(obj, event);
    }

private:
    QWidget* m_foot = nullptr;

    void placeFoot()
    {
        if (!m_foot)
            return;
        // Polished first, so the foot is measured with the stylesheet that will
        // draw it, before the rail lays out its tabs to fit it.
        m_foot->ensurePolished();
        const QSize size = m_foot->sizeHint();
        static_cast<IconTabBar*>(tabBar())->setMinimumRailWidth(size.width());
        const QRect bar = tabBar()->geometry();
        const int x = bar.x() + (bar.width() - size.width()) / 2;
        const int y = std::max(bar.y() + bar.height(), height() - size.height());
        m_foot->setGeometry(x, y, size.width(), size.height());
        m_foot->raise();
    }
};

#endif // ICONTABBAR_H
