//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

// The code's size, the log's and the help's, as Preferences and the View menu both offer them: the same words and the
// same choices in each. MainWindow::setCodeZoom, setLogSmaller and setHelpZoom are where they change; these only show
// and choose.

#include <QList>
#include <QMenu>
#include <QString>
#include <QWidget>

class QActionGroup;
class QLabel;
class QPushButton;
class QShowEvent;
class QSlider;

namespace TextSizes {
// "17 pt": the code's point size at a zoom
QString codeSizeText(int zoom);
// "Same as the code", "3 pt smaller": how far under the code's size the log and the cues are
QString logSizeText(int smaller);
// "13 pt": the help's text at a zoom (TutorialPane::textPt)
QString helpSizeText(int zoom);
}

// A size in Preferences: its name, a slider, the size it reads, and optionally a Reset.
class SizeRow : public QWidget
{
    Q_OBJECT
public:
    // inverted: the slider's right end is its minimum (the log's: right is bigger)
    SizeRow(const QString& name, int min, int max, QString (*reading)(int), bool inverted = false, QWidget* parent = nullptr);

    // a size set elsewhere: shown and read out, not announced as chosen
    void setValue(int value);
    int value() const;
    QString reading() const;
    QSlider* slider() const { return m_slider; }
    // a Reset after the reading, choosing `to`
    QPushButton* addReset(int to, const QString& accessibleName);
    // rows shown together: their sliders start in line
    static void lineUp(std::initializer_list<SizeRow*> rows);

signals:
    void chosen(int value);

protected:
    void showEvent(QShowEvent* event) override;

private:
    void show(int value);

    QString (*m_reading)(int);
    QLabel* m_name;
    QSlider* m_slider;
    QLabel* m_value;
    QList<SizeRow*> m_inLine{ this };
};

// The View menu's Log Text Size: one ticked choice for each size Preferences' slider has.
class LogSizeMenu : public QMenu
{
    Q_OBJECT
public:
    explicit LogSizeMenu(QWidget* parent = nullptr);

    // a size set elsewhere: ticked, not announced as chosen
    void setSmaller(int smaller);

signals:
    void chosen(int smaller);

private:
    QActionGroup* m_group;
};
