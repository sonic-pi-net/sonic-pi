//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef WELCOMEWIDGET_H
#define WELCOMEWIDGET_H

#include <QVector>
#include <QWidget>

class SonicPiTheme;

// First-boot welcome window: logo, tagline (fading in word by word) and
// three starting points. Any key or click skips the entrance; Escape or
// the Get Started button closes it and hands focus to the editor.
class WelcomeWidget : public QWidget
{
    Q_OBJECT
public:
    WelcomeWidget(SonicPiTheme* theme, bool reduceMotion, QWidget* parent = nullptr);

signals:
    void dismissRequested();

protected:
    void showEvent(QShowEvent* event) override;
    void paintEvent(QPaintEvent* event) override; // background + faint wave arcs
    void keyPressEvent(QKeyEvent* event) override;
    void mousePressEvent(QMouseEvent* event) override;

private:
    void beginIntro();  // staggered fade-in of the collected targets
    void finishIntro(); // jump everything to fully visible

    SonicPiTheme* m_theme;
    bool m_reduceMotion;
    QVector<QPair<QWidget*, int>> m_introTargets; // widget, start delay ms
    bool m_introStarted = false;
    bool m_introDone = false;
};

#endif // WELCOMEWIDGET_H
