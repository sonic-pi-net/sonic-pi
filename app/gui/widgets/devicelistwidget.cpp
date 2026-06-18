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

#include "devicelistwidget.h"

#include <QCheckBox>
#include <QFont>
#include <QLabel>
#include <QVBoxLayout>

DeviceListWidget::DeviceListWidget(const QString& emptyText, QWidget* parent)
    : QWidget(parent)
    , m_emptyText(emptyText)
{
    m_rows = new QVBoxLayout(this);
    m_rows->setContentsMargins(0, 0, 0, 0);
    m_rows->setSpacing(2);
    rebuild();
}

void DeviceListWidget::setDevices(const QString& devices)
{
    QStringList lines = devices.split('\n', Qt::SkipEmptyParts);
    for (QString& l : lines) l = l.trimmed();
    lines.removeAll(QString());
    if (lines == m_lines) return;
    m_lines = lines;
    rebuild();
}

void DeviceListWidget::rebuild()
{
    while (QLayoutItem* item = m_rows->takeAt(0)) {
        if (item->widget()) item->widget()->deleteLater();
        delete item;
    }

    if (m_lines.isEmpty()) {
        QLabel* none = new QLabel(m_emptyText, this);
        none->setStyleSheet("color: gray; font-style: italic;");
        m_rows->addWidget(none);
        return;
    }

    QFont mono("Hack");
    for (const QString& line : m_lines) {
        // "enabled<TAB>name"; a bare name is treated as enabled
        const int tab = line.indexOf('\t');
        const QString name = (tab >= 0) ? line.mid(tab + 1).trimmed() : line;
        const bool enabled = (tab < 0) || (line.left(tab).trimmed() != "0");
        if (name.isEmpty()) continue;

        QCheckBox* row = new QCheckBox(name, this);
        row->setFont(mono);
        row->setChecked(enabled);
        row->setToolTip(tr("Untick to ignore this device"));
        connect(row, &QCheckBox::clicked, this, [this, name](bool checked) {
            emit deviceToggled(name, checked);
        });
        m_rows->addWidget(row);
    }
}
