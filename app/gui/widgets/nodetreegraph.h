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
#include <QImage>
#include <QMutex>
#include <QPointF>
#include <QString>
#include <QVector>
#include <QWaitCondition>
#include <QWidget>

#include <atomic>
#include <thread>

class QMouseEvent;

// Animated render of the scsynth node tree: a top-down hierarchy of coloured
// nodes joined by curved edges, easing to new positions as the tree changes.
//
// Painting runs on a worker thread into a QImage (the raster render is
// expensive — dominated by stroking the edge curves — and easing dirties the
// whole widget every frame); the GUI thread only blits the finished frame.
// Frames coalesce: the worker always renders the newest snapshot, so a slow
// render degrades to a lower frame rate, never a queue backlog.
class NodeTreeGraph : public QWidget
{
    Q_OBJECT
public:
    enum Kind { Group = 0, Synth, Fx, Sample };
    struct Node
    {
        int id = -1;
        int parent = -1;
        int head = -1;   // first child (groups only) — scsynth sibling chain
        int next = -1;   // next sibling in parent's child list
        Kind kind = Synth;
        QString label;
    };

    explicit NodeTreeGraph(QWidget* parent = nullptr);
    ~NodeTreeGraph() override;

    // Replace the tree. Caller version-gates (only call on change).
    void setTree(const QVector<Node>& nodes);
    // Theme: base colours + a distinct colour per node kind (Sonic Pi palette).
    void applyTheme(const QColor& text, const QColor& bg, const QColor& border,
                    const QColor& group, const QColor& synth,
                    const QColor& fx, const QColor& sample);

    QSize sizeHint() const override;  // sensible default height; min stays small

    // Diagnostic: wall time the worker spent rendering the latest frame.
    int lastRenderMicros() const { return m_lastRenderUs.load(); }

protected:
    void paintEvent(QPaintEvent* e) override;
    void resizeEvent(QResizeEvent* e) override;
    void showEvent(QShowEvent* e) override;
    void hideEvent(QHideEvent* e) override;
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

    // Everything the worker needs to draw one frame, in device-independent
    // coordinates. Plain data, copied under the snapshot mutex — the worker
    // never touches widget state.
    struct FrameSnapshot
    {
        QSize size;                  // logical widget size
        qreal dpr = 1.0;
        QColor bg, text;
        QColor kindColor[4];
        bool dense = false;          // straight edges + smaller dots
        struct Edge { QPointF a, b; };
        QVector<Edge> edges;
        struct Dot { QPointF c; int kind; };
        QVector<Dot> dots;
        bool valid = false;
    };

    void computeTargets();
    void startAnimating();           // subscribe to the shared FramePacer tick
    void stopAnimating();
    void requestRender();            // snapshot current state → wake the worker
    void renderLoop();               // worker thread body
    static void renderFrame(QImage& target, const FrameSnapshot& snap);

    QVector<Node> m_nodes;
    QHash<int, int> m_index;         // id → index in m_nodes
    QHash<int, Layout> m_layout;     // id → eased layout
    bool m_animating = false;        // holding a FramePacer retain
    QElapsedTimer m_clock;           // wall-clock between steps → frame-rate-independent easing

    QColor m_text{ "#cccccc" };
    QColor m_bg{ "#1e1e1e" };
    QColor m_border{ "#444444" };
    QColor m_kindColor[4]{ QColor("#ff8c00"), QColor("#00ff88"), QColor("#ff5fff"), QColor("#00ffff") };

    QHash<int, QPointF> m_screenPos;  // id → last-painted centre, for hover hit-testing

    // Worker-thread rendering. m_snapshotMutex guards m_pending; m_frontMutex
    // guards m_front. The worker waits on m_snapshotReady, renders the newest
    // pending snapshot, publishes it as m_front and queues update() on the
    // GUI thread.
    std::thread m_renderThread;
    QMutex m_snapshotMutex;
    QWaitCondition m_snapshotReady;
    FrameSnapshot m_pending;
    bool m_quit = false;
    QMutex m_frontMutex;
    QImage m_front;
    std::atomic<int> m_lastRenderUs{ 0 };
};

#endif // NODETREEGRAPH_H
