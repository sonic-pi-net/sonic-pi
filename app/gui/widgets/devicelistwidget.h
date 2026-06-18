//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef DEVICELISTWIDGET_H
#define DEVICELISTWIDGET_H

#include <QStringList>
#include <QWidget>

class QVBoxLayout;

// Compact connected-device list for the IO preferences: one checkbox row per
// device (ticked = enabled), or a muted placeholder when nothing is
// connected. Shared by the MIDI in/out port lists and the game controller
// list.
class DeviceListWidget : public QWidget
{
    Q_OBJECT
public:
    explicit DeviceListWidget(const QString& emptyText, QWidget* parent = nullptr);

    // One device per line, "enabled<TAB>name" (enabled 0/1; a bare name is
    // treated as enabled). Empty/whitespace = none connected.
    void setDevices(const QString& devices);

signals:
    // User ticked/unticked a device row.
    void deviceToggled(const QString& name, bool enabled);

private:
    void rebuild();

    QString m_emptyText;
    QStringList m_lines;
    QVBoxLayout* m_rows = nullptr;
};

#endif
