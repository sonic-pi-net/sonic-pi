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
#include <QPainter>

LinkVisibilityToggle::LinkVisibilityToggle(QWidget* parent)
    : LinkVisibilityToggle(tr("Local"), tr("Network"), parent)
{
}

LinkVisibilityToggle::LinkVisibilityToggle(const QString& leftLabel,
                                           const QString& rightLabel,
                                           QWidget* parent)
    : QAbstractButton(parent)
    , m_leftLabel(leftLabel)
    , m_rightLabel(rightLabel)
{
    setCheckable(true);
    setCursor(Qt::PointingHandCursor);
    // TabFocus: keyboard-reachable without clicks stealing focus from the editor
    setFocusPolicy(Qt::TabFocus);
    setAccessibleName(tr("Link visibility"));
    // checkStateSet() is skipped by Qt on the user-click path (blockRefresh),
    // so sync the state text from toggled as well.
    connect(this, &QAbstractButton::toggled, this, [this](bool) { updateTooltip(); });
    updateTooltip();
}

void LinkVisibilityToggle::updateTooltip()
{
    const QString tip = isChecked()
        ? tr("Public — visible to other devices on your network.\n"
             "Click to go local (private).")
        : tr("Local only — hidden from the network.\n"
             "Click to go public (visible on the network).");
    setToolTip(tip);
    setAccessibleDescription(tip);
}

void LinkVisibilityToggle::checkStateSet()
{
    QAbstractButton::checkStateSet();
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

void LinkVisibilityToggle::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing);

    // Local | Network sliding pill: the selected half highlights with the thumb
    // — pink when Link is live, a visible mid-grey when off (so the selected mode
    // still reads on both light and dark tracks). The accent is pushed in via
    // setAccent (custom-painted widgets don't reliably get the palette on macOS).
    const QColor border  = palette().color(QPalette::Mid);
    const QColor track   = palette().color(QPalette::Base);
    const QColor trackTx = palette().color(QPalette::Text);
    const QColor thumb   = m_muted ? QColor(128, 128, 128) : m_thumb;
    const QColor thumbTx = m_activeIcon;
    const qreal radius = 3.0;
    const bool right = isChecked();

    const QRectF r = QRectF(rect()).adjusted(0.5, 0.5, -0.5, -0.5);
    p.setBrush(track);
    p.setPen(QPen(border, 1));
    p.drawRoundedRect(r, radius, radius);

    const qreal halfW = r.width() / 2.0;
    const QRectF thumbRect = right
        ? QRectF(r.left() + halfW, r.top(), r.width() - halfW, r.height())
        : QRectF(r.left(), r.top(), halfW, r.height());
    p.setBrush(thumb);
    p.setPen(QPen(border, 1));
    p.drawRoundedRect(thumbRect, radius, radius);

    p.setPen(right ? trackTx : thumbTx);
    p.drawText(QRectF(r.left(), r.top(), halfW, r.height()),
               Qt::AlignCenter, m_leftLabel);
    p.setPen(right ? thumbTx : trackTx);
    p.drawText(QRectF(r.left() + halfW, r.top(), r.width() - halfW, r.height()),
               Qt::AlignCenter, m_rightLabel);

    if (hasFocus()) {
        p.setBrush(Qt::NoBrush);
        p.setPen(QPen(thumb, 2));
        p.drawRoundedRect(QRectF(rect()).adjusted(1.0, 1.0, -1.0, -1.0),
                          radius, radius);
    }
}
