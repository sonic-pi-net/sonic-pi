//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron.
// All rights reserved.
//++

#include "linkvisibilitytoggle.h"

#include <QFontMetrics>
#include <QMouseEvent>
#include <QPainter>

LinkVisibilityToggle::LinkVisibilityToggle(QWidget* parent)
    : LinkVisibilityToggle(tr("Local"), tr("Network"), parent)
{
}

LinkVisibilityToggle::LinkVisibilityToggle(const QString& leftLabel,
                                           const QString& rightLabel,
                                           QWidget* parent)
    : QWidget(parent)
    , m_leftLabel(leftLabel)
    , m_rightLabel(rightLabel)
{
    setCursor(Qt::PointingHandCursor);
    setFocusPolicy(Qt::NoFocus);
    updateTooltip();
}

void LinkVisibilityToggle::updateTooltip()
{
    setToolTip(m_isRight
        ? tr("Public — visible to other devices on your network.\n"
             "Click to go local (private).")
        : tr("Local only — hidden from the network.\n"
             "Click to go public (visible on the network)."));
}

void LinkVisibilityToggle::setRight(bool right)
{
    if (m_isRight == right) return;
    m_isRight = right;
    updateTooltip();
    update();
}

void LinkVisibilityToggle::setLabels(const QString& left, const QString& right)
{
    if (m_leftLabel == left && m_rightLabel == right) return;
    m_leftLabel = left;
    m_rightLabel = right;
    updateGeometry();
    update();
}

void LinkVisibilityToggle::setAccent(const QColor& thumb, const QColor& activeIcon)
{
    if (m_thumb == thumb && m_activeIcon == activeIcon) return;
    m_thumb = thumb;
    m_activeIcon = activeIcon;
    update();
}

void LinkVisibilityToggle::setMuted(bool muted)
{
    if (m_muted == muted) return;
    m_muted = muted;
    update();
}

QSize LinkVisibilityToggle::sizeHint() const
{
    const QFontMetrics fm(font());
    const int textW = qMax(fm.horizontalAdvance(m_leftLabel),
                           fm.horizontalAdvance(m_rightLabel));
    return QSize(2 * (textW + 14), 25);  // 25 to match the panel controls
}

QSize LinkVisibilityToggle::minimumSizeHint() const
{
    return sizeHint();
}

void LinkVisibilityToggle::mousePressEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton) return;
    m_isRight = !m_isRight;
    updateTooltip();
    update();
    emit toggled(m_isRight);
}

void LinkVisibilityToggle::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing);

    // Local | Network sliding pill: the selected half highlights with the thumb
    // — pink when Link is live, grey when off — so whichever side is chosen
    // (incl. Local) goes pink. The accent is pushed in via setAccent (custom-
    // painted widgets don't reliably pick up the theme palette on macOS).
    const QColor border  = palette().color(QPalette::Mid);
    const QColor track   = palette().color(QPalette::Base);
    const QColor trackTx = palette().color(QPalette::Text);
    const QColor thumb   = m_muted ? palette().color(QPalette::Mid) : m_thumb;
    const QColor thumbTx = m_activeIcon;
    const qreal radius = 3.0;

    const QRectF r = QRectF(rect()).adjusted(0.5, 0.5, -0.5, -0.5);
    p.setBrush(track);
    p.setPen(QPen(border, 1));
    p.drawRoundedRect(r, radius, radius);

    const qreal halfW = r.width() / 2.0;
    const QRectF thumbRect = m_isRight
        ? QRectF(r.left() + halfW, r.top(), r.width() - halfW, r.height())
        : QRectF(r.left(), r.top(), halfW, r.height());
    p.setBrush(thumb);
    p.setPen(QPen(border, 1));
    p.drawRoundedRect(thumbRect, radius, radius);

    p.setPen(m_isRight ? trackTx : thumbTx);
    p.drawText(QRectF(r.left(), r.top(), halfW, r.height()),
               Qt::AlignCenter, m_leftLabel);
    p.setPen(m_isRight ? thumbTx : trackTx);
    p.drawText(QRectF(r.left() + halfW, r.top(), r.width() - halfW, r.height()),
               Qt::AlignCenter, m_rightLabel);
}
