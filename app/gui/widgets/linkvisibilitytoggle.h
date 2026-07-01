//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron.
// All rights reserved.
//++

#ifndef LINKVISIBILITYTOGGLE_H
#define LINKVISIBILITYTOGGLE_H

#include <QColor>
#include <QString>
#include <QWidget>

// Local | Network sliding pill (Link Audio panel). The selected half fills with
// the thumb colour — pink (setAccent) when Link is live, grey when muted.
// setMuted(true) (e.g. Link off) greys it but it stays clickable. The accent is
// pushed in via setAccent (custom-painted widgets don't reliably pick up the
// theme palette on macOS).
class LinkVisibilityToggle : public QWidget
{
    Q_OBJECT
public:
    explicit LinkVisibilityToggle(QWidget* parent = nullptr);
    LinkVisibilityToggle(const QString& leftLabel,
                         const QString& rightLabel,
                         QWidget* parent = nullptr);

    bool isRight() const { return m_isRight; }
    void setRight(bool right);

    bool isNetwork() const { return m_isRight; }
    void setNetwork(bool net) { setRight(net); }

    void setLabels(const QString& left, const QString& right);

    // Selected-half thumb + selected-label colours (theme pink / white).
    void setAccent(const QColor& thumb, const QColor& activeIcon);

    // Visual-only muting (e.g. Link off): stays clickable.
    void setMuted(bool muted);

    QSize sizeHint() const override;
    QSize minimumSizeHint() const override;

signals:
    void toggled(bool isRight);

protected:
    void paintEvent(QPaintEvent* e) override;
    void mousePressEvent(QMouseEvent* e) override;

private:
    void updateTooltip();

    bool m_isRight = false;
    bool m_muted = false;
    QString m_leftLabel;
    QString m_rightLabel;
    QColor m_thumb = QColor("deeppink");
    QColor m_activeIcon = QColor("white");
};

#endif
