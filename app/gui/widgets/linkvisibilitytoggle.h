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

#include <QString>
#include <QWidget>

// Two-state sliding pill toggle with custom labels (default Local/Network).
// Click anywhere to flip. setMuted(true) greys the active half but stays
// clickable. Colours come from the active QPalette, tracking the theme.
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
    bool m_isRight = false;
    bool m_muted = false;
    QString m_leftLabel;
    QString m_rightLabel;
};

#endif
