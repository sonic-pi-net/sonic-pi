//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "metricvalue.h"

#include <QFontMetricsF>
#include <QPainter>

#include <cmath>

namespace SonicPi
{

MetricValue::MetricValue(QWidget* parent)
    : KeptOpaque<QWidget>(parent)
    , m_background(palette().window().color())
{
}

void MetricValue::setRuns(QVector<LogRun> runs)
{
    bool same = runs.size() == m_runs.size();
    for (int i = 0; same && i < runs.size(); ++i)
        same = runs.at(i).text == m_runs.at(i).text && runs.at(i).color == m_runs.at(i).color;
    if (same)
        return;
    const bool widthMayChange = text().size() != [&] {
        int n = 0;
        for (const LogRun& r : runs)
            n += r.text.size();
        return n;
    }();
    m_runs = std::move(runs);
    if (widthMayChange)
        updateGeometry();
    update();
}

void MetricValue::setBackground(const QColor& colour)
{
    QColor opaque = colour;
    opaque.setAlpha(255);
    if (opaque == m_background)
        return;
    m_background = opaque;
    update();
}

QString MetricValue::text() const
{
    QString t;
    for (const LogRun& r : m_runs)
        t += r.text;
    return t;
}

QSize MetricValue::sizeHint() const
{
    const QFontMetricsF fm(font());
    return QSize(int(std::ceil(fm.horizontalAdvance(text()))), int(std::ceil(fm.height())));
}

QSize MetricValue::minimumSizeHint() const
{
    return QSize(0, int(std::ceil(QFontMetricsF(font()).height())));
}

void MetricValue::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.fillRect(rect(), m_background);
    const QFontMetricsF fm(font());
    qreal textWidth = 0.0;
    for (const LogRun& r : m_runs)
        textWidth += fm.horizontalAdvance(r.text);
    qreal x = width() - textWidth;   // right-aligned, as the label was
    const qreal y = (height() - fm.height()) / 2.0 + fm.ascent();
    for (const LogRun& r : m_runs)
    {
        p.setPen(r.color.isValid() ? r.color : palette().windowText().color());
        p.drawText(QPointF(x, y), r.text);
        x += fm.horizontalAdvance(r.text);
    }
}

} // namespace SonicPi
