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

#include "utils/nodetreemotion.h"

#include <QColor>
#include <QElapsedTimer>
#include <QFont>
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
#include <vector>

class QMouseEvent;

// Animated render of the scsynth node tree: a top-down hierarchy of coloured
// nodes joined by curved edges, easing to new positions as the tree changes.
// Drawn as the web's Threads view draws its tree (app/web/app/src/
// process-tree.js): groups are circles, fx squares and sounds small diamonds;
// a freed node stays where it was, fading — a sound over a second, a group or
// fx over 2.5 s, shrinking as it goes — and the fx are labelled while there
// are few enough to read. The rules are NodeTreeMotion's.
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
    using Kind = SonicPi::NodeTreeMotion::Kind;
    using LiveNode = SonicPi::NodeTreeMotion::LiveNode;

    explicit NodeTreeGraph(QWidget* parent = nullptr);
    ~NodeTreeGraph() override;

    // A fresh read of the mirror: the nodes it has now. Caller version-gates
    // (only call on change); what has gone since the last read fades out.
    void setTree(const std::vector<LiveNode>& live);
    // Labels under the fx (on by default).
    void setLabelsShown(bool on);
    bool labelsShown() const { return m_labelsShown; }
    // Whether the labels are hidden whatever the switch says: too many to
    // read at once, or a tree too big to label at all.
    bool labelsHidden() const { return m_crowded || m_dense; }
    // Theme: the edges' and empty text's colour, the background, the outline
    // round each node, the labels' colour, and a colour per node kind.
    void applyTheme(const QColor& text, const QColor& bg, const QColor& outline, const QColor& labels,
                    const QColor& group, const QColor& synth,
                    const QColor& fx, const QColor& sample);

    QSize sizeHint() const override;  // sensible default height; min stays small

    // Diagnostic: wall time the worker spent rendering the latest frame.
    int lastRenderMicros() const { return m_lastRenderUs.load(); }

    // The tree as last drawn, for checks (as the web's processTree() snapshot):
    // each node's place, size and how faded it is. Nodes faded to nothing are
    // left out.
    struct Drawn { int id; Kind kind; QPointF c; qreal r; qreal alpha; QString label; };
    const QVector<Drawn>& drawn() const { return m_drawn; }

signals:
    // The labels have gone, or come back, because of how many there are.
    void labelsHiddenChanged(bool hidden);

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
        QColor bg, text, outline, labelColor;
        QColor kindColor[4];
        QFont font;
        bool dense = false;          // straight edges
        struct Edge { QPointF a, b; };
        QVector<Edge> edges;
        struct Dot { QPointF c; Kind kind; qreal r; qreal alpha; };
        QVector<Dot> dots;           // in paint order: groups first
        struct Label { QPointF top; QString text; qreal alpha; };
        QVector<Label> labels;
        bool valid = false;
    };

    double now() const;              // seconds since this widget was made
    bool snapping() const;           // positions snap: a big tree, or reduce motion
    void computeTargets();
    void kick();                     // a new read or a theme: draw it, animating while there is motion
    void startAnimating();           // subscribe to the shared FramePacer tick
    void stopAnimating();
    void requestRender();            // snapshot current state → wake the worker
    void renderLoop();               // worker thread body
    static void renderFrame(QImage& target, const FrameSnapshot& snap);

    SonicPi::NodeTreeMotion::Tree m_tree;
    std::vector<int32_t> m_order;    // drawn nodes, parents before children, siblings in order
    QHash<int, Layout> m_layout;     // id → eased layout
    bool m_animating = false;        // holding a FramePacer retain
    QElapsedTimer m_clock;           // wall-clock between steps → frame-rate-independent easing
    QElapsedTimer m_epoch;           // now(): when nodes ended, and how far they have faded
    bool m_labelsShown = true;
    bool m_crowded = false;          // too many to label (NodeTreeMotion::crowded)
    bool m_dense = false;            // too big to label at all (NodeTreeMotion::kMaxAnimated)

    QColor m_text{ "#cccccc" };
    QColor m_bg{ "#1e1e1e" };
    QColor m_outline{ "#000000" };
    QColor m_labelColor{ "#999999" };
    QColor m_kindColor[4]{ QColor("#ff8c00"), QColor("#00ff88"), QColor("#ff5fff"), QColor("#00ffff") };

    QHash<int, QPointF> m_screenPos;  // id → last-painted centre, for hover hit-testing
    QVector<Drawn> m_drawn;           // drawn(): what the last snapshot held

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
