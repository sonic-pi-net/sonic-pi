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

#pragma once

#include <QColor>
#include <QVector>
#include <QWidget>

#include "utils/opaque.h"
#include "widgets/metricslog.h"   // LogRun

namespace SonicPi
{

// One metric's value in the Debug pane: a few coloured runs, right-aligned
// ("12.5" then " / 40.0%"). Thirty-odd of them change several times a second
// while loops play. They were rich-text labels: each change parsed HTML, laid
// a document out, and — a label being see-through — repainted the styled
// grid cell behind it, borders and all. This paints every pixel of its own
// rectangle and draws its runs as they are, so a change repaints the value
// and nothing else, and an unchanged value repaints nothing.
class MetricValue : public KeptOpaque<QWidget>
{
    Q_OBJECT
public:
    explicit MetricValue(QWidget* parent = nullptr);

    // The same runs again change nothing and repaint nothing.
    void setRuns(QVector<LogRun> runs);
    // The cell's colour, which this paints under its text.
    void setBackground(const QColor& colour);

    const QVector<LogRun>& runs() const { return m_runs; }
    QString text() const;

    QSize sizeHint() const override;
    QSize minimumSizeHint() const override;

protected:
    void paintEvent(QPaintEvent*) override;

private:
    QVector<LogRun> m_runs;
    QColor m_background;
};

} // namespace SonicPi
