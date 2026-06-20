//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2024 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef NODETREEGRAPH_H
#define NODETREEGRAPH_H

#include <QColor>
#include <QElapsedTimer>
#include <QHash>
#include <QPointF>
#include <QString>
#include <QVector>
#include <QWidget>

class QTimer;
class QMouseEvent;

// Animated render of the scsynth node tree: a top-down hierarchy of coloured
// nodes joined by curved edges, easing to new positions as the tree changes.
class NodeTreeGraph : public QWidget
{
    Q_OBJECT
public:
    enum Kind { Group = 0, Synth, Fx, Sample };
    struct Node
    {
        int id = -1;
        int parent = -1;
        Kind kind = Synth;
        QString label;
    };

    explicit NodeTreeGraph(QWidget* parent = nullptr);

    // Replace the tree. Caller version-gates (only call on change).
    void setTree(const QVector<Node>& nodes);
    // Theme: base colours + a distinct colour per node kind (Sonic Pi palette).
    void applyTheme(const QColor& text, const QColor& bg, const QColor& border,
                    const QColor& group, const QColor& synth,
                    const QColor& fx, const QColor& sample);

    QSize sizeHint() const override;  // sensible default height; min stays small

protected:
    void paintEvent(QPaintEvent* e) override;
    void mouseMoveEvent(QMouseEvent* e) override;  // hover → node name tooltip

private slots:
    void animateStep();

private:
    struct Layout
    {
        float cx = 0.5f, cy = 0.f;   // current normalised position (eased)
        float tx = 0.5f, ty = 0.f;   // target normalised position
        float visc = 0.9f;           // viscosity: higher = stickier (slower to move)
        bool seeded = false;
    };
    void computeTargets();

    QVector<Node> m_nodes;
    QHash<int, int> m_index;         // id → index in m_nodes
    QHash<int, Layout> m_layout;     // id → eased layout
    QTimer* m_anim = nullptr;
    QElapsedTimer m_clock;           // wall-clock between steps → frame-rate-independent easing

    QColor m_text{ "#cccccc" };
    QColor m_bg{ "#1e1e1e" };
    QColor m_border{ "#444444" };
    QColor m_kindColor[4]{ QColor("#ff8c00"), QColor("#00ff88"), QColor("#ff5fff"), QColor("#00ffff") };

    QHash<int, QPointF> m_screenPos;  // id → last-painted centre, for hover hit-testing
};

#endif // NODETREEGRAPH_H
