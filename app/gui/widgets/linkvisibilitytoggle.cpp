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
    setToolTip(tr("Visibility scope: Local (loopback only) or Network (LAN). "
                  "Click to switch."));
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
}

void LinkVisibilityToggle::setRight(bool right)
{
    if (m_isRight == right) return;
    m_isRight = right;
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
    // Width = 2 * (text + side padding). Height 25 to match the metro-row
    // controls (QPushButton/QLineEdit in app.qss).
    return QSize(2 * (textW + 14), 25);
}

QSize LinkVisibilityToggle::minimumSizeHint() const
{
    return sizeHint();
}

void LinkVisibilityToggle::mousePressEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton) return;
    m_isRight = !m_isRight;
    update();
    emit toggled(m_isRight);
}

void LinkVisibilityToggle::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing);

    const QRectF r = QRectF(rect()).adjusted(0.5, 0.5, -0.5, -0.5);
    // border-radius 3 to match QPushButton/QLineEdit in app.qss.
    const qreal radius = 3.0;

    const QColor border   = palette().color(QPalette::Mid);
    const QColor idleBg   = palette().color(QPalette::Base);
    const QColor idleTx   = palette().color(QPalette::Text);
    // Thumb: Highlight when live, Mid (grey) when muted.
    const QColor thumb    = m_muted ? palette().color(QPalette::Mid)
                                    : palette().color(QPalette::Highlight);
    const QColor activeTx = palette().color(QPalette::HighlightedText);

    // Idle track.
    p.setBrush(idleBg);
    p.setPen(QPen(border, 1));
    p.drawRoundedRect(r, radius, radius);

    // Thumb covers half the width, slides between left/right.
    const qreal halfW = r.width() / 2.0;
    const QRectF thumbRect = m_isRight
        ? QRectF(r.left() + halfW, r.top(), r.width() - halfW, r.height())
        : QRectF(r.left(), r.top(), halfW, r.height());
    p.setBrush(thumb);
    p.setPen(QPen(border, 1));
    p.drawRoundedRect(thumbRect, radius, radius);

    // Labels.
    p.setPen(m_isRight ? idleTx : activeTx);
    p.drawText(QRectF(r.left(), r.top(), halfW, r.height()),
               Qt::AlignCenter, m_leftLabel);
    p.setPen(m_isRight ? activeTx : idleTx);
    p.drawText(QRectF(r.left() + halfW, r.top(), r.width() - halfW, r.height()),
               Qt::AlignCenter, m_rightLabel);
}
