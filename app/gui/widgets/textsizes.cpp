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

#include "textsizes.h"

#include <QActionGroup>
#include <QFontMetrics>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSignalBlocker>
#include <QSlider>

#include <algorithm>

#include "sonicpilexer.h"
#include "sonicpilog.h"

QString TextSizes::codeSizeText(int zoom)
{
    return QObject::tr("%1 pt").arg(SonicPiLexer::kPointSize + zoom);
}

QString TextSizes::logSizeText(int smaller)
{
    return smaller == 0 ? QObject::tr("Same as the code") : QObject::tr("%n pt smaller", nullptr, smaller);
}

SizeRow::SizeRow(const QString& name, int min, int max, QString (*reading)(int), bool inverted, QWidget* parent)
    : QWidget(parent)
    , m_reading(reading)
    , m_name(new QLabel(name, this))
    , m_slider(new QSlider(Qt::Horizontal, this))
    , m_value(new QLabel(this))
{
    m_slider->setRange(min, max);
    m_slider->setPageStep(1);
    m_slider->setInvertedAppearance(inverted);
    m_slider->setAccessibleName(name);
    m_value->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

    auto* row = new QHBoxLayout(this);
    row->setContentsMargins(0, 0, 0, 0);
    row->addWidget(m_name);
    row->addWidget(m_slider, 1);
    row->addWidget(m_value);

    connect(m_slider, &QSlider::valueChanged, this, [this](int value) {
        show(value);
        emit chosen(value);
    });
    show(m_slider->value());
}

void SizeRow::setValue(int value)
{
    {
        QSignalBlocker block(m_slider);
        m_slider->setValue(value);
    }
    show(m_slider->value());
}

int SizeRow::value() const
{
    return m_slider->value();
}

QString SizeRow::reading() const
{
    return m_value->text();
}

QPushButton* SizeRow::addReset(int to, const QString& accessibleName)
{
    auto* reset = new QPushButton(tr("Reset"), this);
    reset->setFlat(true);
    reset->setAccessibleName(accessibleName);
    layout()->addWidget(reset);
    connect(reset, &QPushButton::clicked, m_slider, [this, to] { m_slider->setValue(to); });
    return reset;
}

void SizeRow::lineUp(std::initializer_list<SizeRow*> rows)
{
    for (SizeRow* row : rows)
        row->m_inLine = QList<SizeRow*>(rows);
}

// Room for the widest name in line and for this row's widest reading, so the slider neither moves nor changes length
// as its reading does. Measured here, once the style sheet has given the labels their fonts.
void SizeRow::showEvent(QShowEvent* event)
{
    QWidget::showEvent(event);
    int name = 0;
    for (SizeRow* row : m_inLine)
    {
        row->m_name->ensurePolished();
        name = std::max(name, row->m_name->sizeHint().width());
    }
    m_name->setMinimumWidth(name);

    m_value->ensurePolished();
    const QFontMetrics metrics(m_value->font());
    const int frame = m_value->sizeHint().width() - metrics.horizontalAdvance(m_value->text());
    int widest = 0;
    for (int value = m_slider->minimum(); value <= m_slider->maximum(); ++value)
        widest = std::max(widest, metrics.horizontalAdvance(m_reading(value)));
    m_value->setMinimumWidth(widest + frame);
}

void SizeRow::show(int value)
{
    m_value->setText(m_reading(value));
    m_slider->setAccessibleDescription(m_reading(value));
}

LogSizeMenu::LogSizeMenu(QWidget* parent)
    : QMenu(tr("Log Text Size"), parent)
    , m_group(new QActionGroup(this))
{
    m_group->setExclusive(true);
    for (int smaller = 0; smaller <= SonicPiLog::kMaxSmaller; ++smaller)
    {
        QAction* choice = addAction(TextSizes::logSizeText(smaller));
        choice->setCheckable(true);
        choice->setData(smaller);
        m_group->addAction(choice);
        connect(choice, &QAction::triggered, this, [this, smaller] { emit chosen(smaller); });
    }
}

void LogSizeMenu::setSmaller(int smaller)
{
    for (QAction* choice : m_group->actions())
        choice->setChecked(choice->data().toInt() == smaller);
}
