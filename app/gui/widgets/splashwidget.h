//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef SPLASHWIDGET_H
#define SPLASHWIDGET_H

#include <QElapsedTimer>
#include <QVector>
#include <QWidget>

class QLabel;
class QTimer;

class SplashWidget : public QWidget
{
    Q_OBJECT
public:
    SplashWidget(QWidget* parent = nullptr);

    // Start the strapline entrance; idempotent. reduceMotion skips it.
    void startAnimation(bool reduceMotion);

    // ms from startAnimation() until the last word has landed.
    int introDurationMs() const;

    // Fade out and close; self-deletes via WA_DeleteOnClose.
    void finishAndClose();

protected:
    void showEvent(QShowEvent* event) override;
    void paintEvent(QPaintEvent* event) override;

private:
    void finishIntro(); // jump the animated elements to fully visible

    bool m_reduceMotion = false;
    bool m_introStarted = false;
    bool m_introDone = false;
    QVector<QPair<QWidget*, int>> m_introTargets; // widget, start delay ms
    QLabel* m_version = nullptr;      // pinned to the bottom-right corner by hand
    QWidget* m_poweredBy = nullptr;   // pinned to the bottom-left corner by hand

    // Glowing segment that sweeps around the border. Time-based so event-loop
    // hitches during boot skip the glow forward instead of slowing it down.
    QTimer* m_borderTimer = nullptr;
    QElapsedTimer m_borderClock;
    qreal m_borderPos = 0.0; // fraction of a revolution, 0 = top centre
};

#endif // SPLASHWIDGET_H
