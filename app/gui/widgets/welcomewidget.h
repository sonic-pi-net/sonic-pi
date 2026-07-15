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

#include <QWidget>

class SonicPiTheme;

// First-boot welcome window. Escape or Get Started closes it and hands
// focus to the editor.
class WelcomeWidget : public QWidget
{
    Q_OBJECT
public:
    WelcomeWidget(SonicPiTheme* theme, bool reduceMotion, QWidget* parent = nullptr);

signals:
    void dismissRequested();

protected:
    void paintEvent(QPaintEvent* event) override; // background + faint wave arcs
    void keyPressEvent(QKeyEvent* event) override;

private:
    SonicPiTheme* m_theme;
};

#endif // WELCOMEWIDGET_H
