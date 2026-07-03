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
#include "utils/reducedmotion.h"

#include <algorithm>
#include <functional>
#include <vector>

#include <api/audio/node_tree_order.hpp>

#include <QMouseEvent>
#include <QPainter>
#include <QPainterPath>
#include <QTimer>
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
    m_anim = new QTimer(this);
    m_anim->setInterval(16);  // ~60 fps easing
    m_anim->setTimerType(Qt::PreciseTimer);  // steadier interval than the default coarse timer
    connect(m_anim, &QTimer::timeout, this, &NodeTreeGraph::animateStep);
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
    update();
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
        if (m_anim->isActive()) m_anim->stop();
        m_clock.invalidate();
    }
    else if (!m_anim->isActive())
    {
        m_anim->start();
    }
    update();
}

void NodeTreeGraph::animateStep()
{
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
    if (!moving) { m_anim->stop(); m_clock.invalidate(); }
    update();
}

void NodeTreeGraph::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing, true);
    p.fillRect(rect(), m_bg);

    const int margin = 22;
    const int w = std::max(1, width() - 2 * margin);
    const int h = std::max(1, height() - 2 * margin);
    auto px = [&](float nx) { return margin + nx * w; };
    auto py = [&](float ny) { return margin + ny * h; };

    if (m_nodes.isEmpty())
    {
        p.setPen(QColor(m_text.red(), m_text.green(), m_text.blue(), 110));
        p.drawText(rect(), Qt::AlignCenter, tr("(no active nodes)"));
        return;
    }

    const bool dense = m_nodes.size() > kMaxAnimatedNodes;

    // Edges first (parent → child), faint. S-curves when sparse, straight when
    // dense. All edges accumulate into one path and stroke in a single drawPath
    // — one antialiased pass instead of one (bezier) draw call per edge.
    QPainterPath edges;
    for (const Node& n : m_nodes)
    {
        if (n.parent < 0 || !m_index.contains(n.parent)) continue;
        const Layout& a = m_layout[n.parent];
        const Layout& b = m_layout[n.id];
        QPointF p0(px(a.cx), py(a.cy)), p1(px(b.cx), py(b.cy));
        edges.moveTo(p0);
        if (dense)
        {
            edges.lineTo(p1);
        }
        else
        {
            const qreal midY = (p0.y() + p1.y()) / 2.0;    // vertical S-curve
            edges.cubicTo(QPointF(p0.x(), midY), QPointF(p1.x(), midY), p1);
        }
    }
    QPen edgePen(QColor(m_text.red(), m_text.green(), m_text.blue(), 90));
    edgePen.setWidthF(1.2);
    p.setPen(edgePen);
    p.setBrush(Qt::NoBrush);
    p.drawPath(edges);

    // Nodes (no labels; hover shows the name). Record screen centres for
    // hit-testing. Drawn grouped by kind so brush/pen are set 4× rather than
    // once per node; within a kind the radius is constant. Groups paint first
    // (under leaves), matching their role as containers.
    m_screenPos.clear();
    for (int k = 0; k < 4; ++k)
    {
        const qreal r = dense ? (k == Group ? 4.0 : 3.0)
                              : (k == Group ? 6.0 : 4.5);
        QColor col = m_kindColor[k];
        p.setBrush(col);
        p.setPen(QPen(col.darker(140), 1.0));
        for (const Node& n : m_nodes)
        {
            if (n.kind != k) continue;
            const Layout& l = m_layout[n.id];
            QPointF c(px(l.cx), py(l.cy));
            m_screenPos.insert(n.id, c);
            p.drawEllipse(c, r, r);
        }
    }
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
