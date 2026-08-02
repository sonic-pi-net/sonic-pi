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

#include "nodetreegraph.h"
#include "utils/framepacer.h"
#include "utils/reducedmotion.h"

#include <algorithm>
#include <functional>
#include <vector>

#include <api/audio/node_tree_order.hpp>

#include <QMouseEvent>
#include <QPainter>
#include <QPainterPath>
#include <QResizeEvent>
#include <QToolTip>

namespace
{
// Above this count, easing is skipped (positions snap) and edges draw straight.
constexpr int kMaxAnimatedNodes = 200;
}

NodeTreeGraph::NodeTreeGraph(QWidget* parent) : QWidget(parent)
{
    // Small minimum so the pane can be collapsed; the graph scales to its size.
    setMinimumSize(80, 24);
    setMouseTracking(true);  // hover tooltips without a pressed button
    // Every paint covers the full rect, so Qt needn't repaint ancestors.
    setAttribute(Qt::WA_OpaquePaintEvent);
    // Animation frames ride the shared pacer (see FramePacer) so this widget
    // and the scope dirty the window in the same composite pass. The easing
    // is wall-clock-based, so motion speed is independent of the tick rate.
    connect(SonicPi::FramePacer::instance(), &SonicPi::FramePacer::tick,
            this, &NodeTreeGraph::animateStep);
    m_renderThread = std::thread([this] { renderLoop(); });
}

NodeTreeGraph::~NodeTreeGraph()
{
    stopAnimating();
    {
        QMutexLocker lock(&m_snapshotMutex);
        m_quit = true;
    }
    m_snapshotReady.wakeAll();
    m_renderThread.join();
}

void NodeTreeGraph::startAnimating()
{
    if (m_animating) return;
    m_animating = true;
    m_clock.invalidate();
    SonicPi::FramePacer::instance()->retain();
}

void NodeTreeGraph::stopAnimating()
{
    if (!m_animating) return;
    m_animating = false;
    m_clock.invalidate();
    SonicPi::FramePacer::instance()->release();
}

QSize NodeTreeGraph::sizeHint() const
{
    return QSize(220, 150);
}

void NodeTreeGraph::applyTheme(const QColor& text, const QColor& bg, const QColor& border,
                               const QColor& group, const QColor& synth,
                               const QColor& fx, const QColor& sample)
{
    m_text = text;
    m_bg = bg;
    m_border = border;
    m_kindColor[Group]  = group;
    m_kindColor[Synth]  = synth;
    m_kindColor[Fx]     = fx;
    m_kindColor[Sample] = sample;
    requestRender();
    update();  // empty-tree text repaints even with no frame in flight
}

void NodeTreeGraph::computeTargets()
{
    m_index.clear();
    for (int i = 0; i < m_nodes.size(); ++i)
        m_index.insert(m_nodes[i].id, i);

    // Children in true scsynth execution order. The mirror's array/slot order is
    // allocation order (slots are freed and reused across runs), NOT sibling
    // order — relying on it makes identical sub-trees render in different orders.
    // order_children() walks each group's sibling chain (head → next → …) exactly
    // as the engine's depth-first audio traversal does. Shared with its tests so
    // the drawn order and the tested order can't drift apart.
    std::vector<sonic_pi::node_tree::OrderNode> flat;
    flat.reserve(m_nodes.size());
    for (const Node& n : m_nodes)
        flat.push_back({ n.id, n.parent, n.head, n.next, n.kind == Group });
    const sonic_pi::node_tree::OrderedTree ordered = sonic_pi::node_tree::order_children(flat);

    QHash<int, QVector<int>> children;
    for (const auto& kv : ordered.children)
    {
        QVector<int>& vec = children[kv.first];
        vec.reserve(static_cast<int>(kv.second.size()));
        for (int c : kv.second) vec.append(c);
    }
    QVector<int> roots;
    roots.reserve(static_cast<int>(ordered.roots.size()));
    for (int r : ordered.roots) roots.append(r);

    // Level-by-depth layout: leaves take sequential slots, parents centre over
    // their children.
    QHash<int, int> depth;
    QHash<int, double> xpos;
    double leaf = 0.0;
    int maxDepth = 0;
    std::function<void(int, int)> dfs = [&](int id, int d) {
        depth.insert(id, d);
        maxDepth = std::max(maxDepth, d);
        const QVector<int>& ch = children.value(id);
        if (ch.isEmpty())
        {
            xpos.insert(id, leaf);
            leaf += 1.0;
        }
        else
        {
            double sum = 0;
            for (int c : ch) { dfs(c, d + 1); sum += xpos.value(c); }
            xpos.insert(id, sum / ch.size());
        }
    };
    for (int r : roots) dfs(r, 0);

    const double maxX = std::max(1.0, leaf - 1.0);
    const int md = std::max(1, maxDepth);

    // Drop layout entries for nodes that no longer exist.
    for (auto it = m_layout.begin(); it != m_layout.end();)
        it = m_index.contains(it.key()) ? std::next(it) : m_layout.erase(it);

    for (const Node& n : m_nodes)
    {
        Layout& l = m_layout[n.id];
        l.tx = static_cast<float>(xpos.value(n.id, 0.0) / maxX);
        l.ty = static_cast<float>(static_cast<double>(depth.value(n.id, 0)) / md);
        // Per-type viscosity (higher = stickier): root pinned, then groups, FX,
        // then synth/sample.
        l.visc = (n.id == 0)            ? 1.00f
               : (n.kind == Group)      ? 0.98f
               : (n.kind == Fx)         ? 0.96f
                                        : 0.90f;   // synth / sample
        if (!l.seeded)
        {
            // New node eases in from its parent's current spot if there is one.
            auto pit = m_layout.find(n.parent);
            if (pit != m_layout.end()) { l.cx = pit->cx; l.cy = pit->cy; }
            else                       { l.cx = l.tx; l.cy = l.ty; }
            l.seeded = true;
        }
    }
}

void NodeTreeGraph::setTree(const QVector<Node>& nodes)
{
    m_nodes = nodes;
    computeTargets();
    if (m_nodes.size() > kMaxAnimatedNodes || SonicPi::prefersReducedMotion())
    {
        // Large tree (or the reduce-motion preference): snap to targets
        // instead of easing. The tree still tracks the live synth graph;
        // only the decorative glide between layouts is dropped.
        for (auto& l : m_layout) { l.cx = l.tx; l.cy = l.ty; }
        stopAnimating();
    }
    else if (isVisible())
    {
        startAnimating();
    }
    // While the animation ticks, the next step renders this tree; only snap
    // mode and hidden (no tick coming) render here.
    if (!m_animating) requestRender();
    if (m_nodes.isEmpty()) update();  // paint the placeholder text directly
}

void NodeTreeGraph::animateStep()
{
    if (!m_animating) return;   // shared tick fires for all pacer clients
    // Nothing to show and nothing driving new frames — stop ticking. setTree
    // and showEvent restart the animation.
    if (!isVisible()) { stopAnimating(); return; }

    // Ease by real elapsed time, not a fixed per-frame fraction, so motion speed
    // stays constant when frames arrive unevenly. Clamp dt so a long stall
    // (window hidden/backgrounded) catches up in one step rather than jumping.
    // frames == elapsed time expressed in 60fps frame-units.
    qint64 dtMs;
    if (m_clock.isValid()) dtMs = m_clock.restart();
    else { m_clock.start(); dtMs = 16; }
    dtMs = std::clamp<qint64>(dtMs, 1, 100);
    const float frames = static_cast<float>(dtMs) / 16.6667f;

    bool moving = false;
    for (auto& l : m_layout)
    {
        // visc^frames: the per-frame retained fraction compounded over real
        // elapsed time — same curve as a fixed 60fps step, correct off-cadence.
        const float step = 1.0f - std::pow(l.visc, frames);
        l.cx += (l.tx - l.cx) * step;
        l.cy += (l.ty - l.cy) * step;
        if (std::abs(l.tx - l.cx) > 0.0005f || std::abs(l.ty - l.cy) > 0.0005f) moving = true;
        else { l.cx = l.tx; l.cy = l.ty; }
    }
    requestRender();
    if (!moving) stopAnimating();
}

void NodeTreeGraph::requestRender()
{
    const int margin = 22;
    const int w = std::max(1, width() - 2 * margin);
    const int h = std::max(1, height() - 2 * margin);
    auto px = [&](float nx) { return margin + nx * w; };
    auto py = [&](float ny) { return margin + ny * h; };

    FrameSnapshot snap;
    snap.size = size();
    snap.dpr = devicePixelRatioF();
    snap.bg = m_bg;
    snap.text = m_text;
    for (int k = 0; k < 4; ++k) snap.kindColor[k] = m_kindColor[k];
    snap.dense = m_nodes.size() > kMaxAnimatedNodes;
    snap.valid = true;

    snap.edges.reserve(m_nodes.size());
    snap.dots.reserve(m_nodes.size());
    m_screenPos.clear();
    for (const Node& n : m_nodes)
    {
        const Layout& l = m_layout[n.id];
        const QPointF c(px(l.cx), py(l.cy));
        m_screenPos.insert(n.id, c);   // hover hit-testing, GUI thread only
        snap.dots.push_back({ c, n.kind });
        if (n.parent >= 0 && m_index.contains(n.parent))
        {
            const Layout& a = m_layout[n.parent];
            snap.edges.push_back({ QPointF(px(a.cx), py(a.cy)), c });
        }
    }

    {
        QMutexLocker lock(&m_snapshotMutex);
        m_pending = std::move(snap);   // coalesce: newest snapshot wins
    }
    m_snapshotReady.wakeAll();
}

void NodeTreeGraph::renderLoop()
{
    QImage back;
    for (;;)
    {
        FrameSnapshot snap;
        {
            QMutexLocker lock(&m_snapshotMutex);
            while (!m_quit && !m_pending.valid)
                m_snapshotReady.wait(&m_snapshotMutex);
            if (m_quit) return;
            snap = std::move(m_pending);
            m_pending.valid = false;
        }

        QElapsedTimer t;
        t.start();
        const QSize pxSize = snap.size * snap.dpr;
        if (back.size() != pxSize)
            back = QImage(pxSize, QImage::Format_ARGB32_Premultiplied);
        back.setDevicePixelRatio(snap.dpr);
        renderFrame(back, snap);
        m_lastRenderUs.store(static_cast<int>(t.nsecsElapsed() / 1000));

        {
            QMutexLocker lock(&m_frontMutex);
            std::swap(m_front, back);
        }
        // Blit on the GUI thread. Queued: safe from the worker, and Qt drops
        // the call if the widget is destroyed first.
        QMetaObject::invokeMethod(this, qOverload<>(&QWidget::update),
                                  Qt::QueuedConnection);
    }
}

void NodeTreeGraph::renderFrame(QImage& target, const FrameSnapshot& snap)
{
    QPainter p(&target);
    p.setRenderHint(QPainter::Antialiasing, true);
    p.fillRect(QRect(QPoint(0, 0), snap.size), snap.bg);

    // Edges first (parent → child), faint. S-curves when sparse, straight when
    // dense. All edges accumulate into one path and stroke in a single drawPath
    // — one antialiased pass instead of one (bezier) draw call per edge.
    QPainterPath edges;
    for (const FrameSnapshot::Edge& e : snap.edges)
    {
        edges.moveTo(e.a);
        if (snap.dense)
        {
            edges.lineTo(e.b);
        }
        else
        {
            const qreal midY = (e.a.y() + e.b.y()) / 2.0;  // vertical S-curve
            edges.cubicTo(QPointF(e.a.x(), midY), QPointF(e.b.x(), midY), e.b);
        }
    }
    QPen edgePen(QColor(snap.text.red(), snap.text.green(), snap.text.blue(), 90));
    edgePen.setWidthF(1.2);
    p.setPen(edgePen);
    p.setBrush(Qt::NoBrush);
    p.drawPath(edges);

    // Nodes (no labels; hover shows the name). Drawn grouped by kind so
    // brush/pen are set 4× rather than once per node; within a kind the
    // radius is constant. Groups paint first (under leaves), matching their
    // role as containers.
    for (int k = 0; k < 4; ++k)
    {
        const qreal r = snap.dense ? (k == Group ? 4.0 : 3.0)
                                   : (k == Group ? 6.0 : 4.5);
        QColor col = snap.kindColor[k];
        p.setBrush(col);
        p.setPen(QPen(col.darker(140), 1.0));
        for (const FrameSnapshot::Dot& d : snap.dots)
        {
            if (d.kind != k) continue;
            p.drawEllipse(d.c, r, r);
        }
    }
}

void NodeTreeGraph::paintEvent(QPaintEvent*)
{
    QPainter p(this);

    if (m_nodes.isEmpty())
    {
        p.fillRect(rect(), m_bg);
        p.setPen(QColor(m_text.red(), m_text.green(), m_text.blue(), 110));
        p.drawText(rect(), Qt::AlignCenter, tr("(no active nodes)"));
        return;
    }

    QMutexLocker lock(&m_frontMutex);
    // A resize can outpace the worker by a frame; backfill so WA_OpaquePaintEvent
    // never leaves stale pixels outside the blit.
    if (m_front.isNull() || m_front.deviceIndependentSize() != size())
        p.fillRect(rect(), m_bg);
    if (!m_front.isNull())
        p.drawImage(QPointF(0, 0), m_front);
}

void NodeTreeGraph::resizeEvent(QResizeEvent* e)
{
    QWidget::resizeEvent(e);
    requestRender();
}

void NodeTreeGraph::showEvent(QShowEvent* e)
{
    QWidget::showEvent(e);
    // Re-enter the animation loop (it stops itself while hidden); if the
    // layout is already settled it stops again after one step.
    if (m_nodes.size() <= kMaxAnimatedNodes && !SonicPi::prefersReducedMotion())
        startAnimating();
    requestRender();
}

void NodeTreeGraph::hideEvent(QHideEvent* e)
{
    QWidget::hideEvent(e);
    stopAnimating();
}

void NodeTreeGraph::mouseMoveEvent(QMouseEvent* e)
{
    const QPointF pos = e->position();
    int best = -1;
    qreal bestD2 = 100.0;  // within ~10px
    for (const Node& n : m_nodes)
    {
        auto it = m_screenPos.find(n.id);
        if (it == m_screenPos.end()) continue;
        const QPointF d = it.value() - pos;
        const qreal d2 = d.x() * d.x() + d.y() * d.y();
        if (d2 < bestD2) { bestD2 = d2; best = n.id; }
    }
    if (best >= 0)
    {
        auto idx = m_index.find(best);
        if (idx != m_index.end())
        {
            const Node& n = m_nodes[idx.value()];
            QString name = n.label.isEmpty() ? QStringLiteral("node") : n.label;
            QToolTip::showText(e->globalPosition().toPoint(),
                               QStringLiteral("%1 (%2)").arg(name).arg(n.id), this);
            return;
        }
    }
    QToolTip::hideText();
}
