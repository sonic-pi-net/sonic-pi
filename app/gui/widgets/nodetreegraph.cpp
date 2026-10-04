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
#include <cmath>
#include <unordered_set>

#include <QFontMetricsF>
#include <QMouseEvent>
#include <QPainter>
#include <QPainterPath>
#include <QResizeEvent>
#include <QToolTip>

namespace Motion = SonicPi::NodeTreeMotion;

NodeTreeGraph::NodeTreeGraph(QWidget* parent) : QWidget(parent)
{
    // Small minimum so the pane can be collapsed; the graph scales to its size.
    setMinimumSize(80, 24);
    setMouseTracking(true);  // hover tooltips without a pressed button
    // Every paint covers the full rect, so Qt needn't repaint ancestors.
    setAttribute(Qt::WA_OpaquePaintEvent);
    m_epoch.start();
    // Animation frames ride the shared pacer (see FramePacer) so this widget
    // and the scope dirty the window in the same composite pass. The easing
    // and the fades are wall-clock-based, so their speed is independent of the
    // tick rate.
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

double NodeTreeGraph::now() const
{
    return static_cast<double>(m_epoch.nsecsElapsed()) / 1e9;
}

bool NodeTreeGraph::snapping() const
{
    // A large tree (or the reduce-motion preference) snaps to its targets
    // instead of easing: the tree still tracks the live synth graph, and what
    // ends still fades; only the decorative glide between layouts is dropped.
    return static_cast<int>(m_tree.nodes().size()) > Motion::kMaxAnimated || SonicPi::prefersReducedMotion();
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

void NodeTreeGraph::applyTheme(const QColor& text, const QColor& bg, const QColor& outline, const QColor& labels,
                               const QColor& group, const QColor& synth,
                               const QColor& fx, const QColor& sample)
{
    m_text = text;
    m_bg = bg;
    m_outline = outline;
    m_labelColor = labels;
    m_kindColor[static_cast<int>(Kind::Group)]  = group;
    m_kindColor[static_cast<int>(Kind::Synth)]  = synth;
    m_kindColor[static_cast<int>(Kind::Fx)]     = fx;
    m_kindColor[static_cast<int>(Kind::Sample)] = sample;
    requestRender();
    update();  // empty-tree text repaints even with no frame in flight
}

void NodeTreeGraph::setLabelsShown(bool on)
{
    if (m_labelsShown == on) return;
    m_labelsShown = on;
    requestRender();
}

void NodeTreeGraph::computeTargets()
{
    // Where everything goes: NodeTreeMotion's layout, over scsynth's sibling
    // order with the freed nodes kept in their places.
    const auto targets = m_tree.targets();

    // Drop layout entries for nodes no longer drawn.
    for (auto it = m_layout.begin(); it != m_layout.end();)
        it = targets.count(it.key()) ? std::next(it) : m_layout.erase(it);

    // Parents before children, so a new node can start from where its parent
    // is now.
    m_order.clear();
    m_order.reserve(m_tree.nodes().size());
    std::vector<int32_t> stack(m_tree.roots().rbegin(), m_tree.roots().rend());
    std::unordered_set<int32_t> seen;
    while (!stack.empty())
    {
        const int32_t id = stack.back();
        stack.pop_back();
        if (!seen.insert(id).second) continue;
        m_order.push_back(id);
        const std::vector<int32_t>& ch = m_tree.children(id);
        for (auto c = ch.rbegin(); c != ch.rend(); ++c) stack.push_back(*c);
    }

    for (int32_t id : m_order)
    {
        const auto t = targets.find(id);
        const auto n = m_tree.nodes().find(id);
        if (t == targets.end() || n == m_tree.nodes().end()) continue;
        Layout& l = m_layout[id];
        l.tx = t->second.tx;
        l.ty = t->second.ty;
        // Per-type viscosity (higher = stickier): root pinned, then groups, FX,
        // then synth/sample.
        l.visc = (id == 0)                         ? 1.00f
               : (n->second.kind == Kind::Group)   ? 0.98f
               : (n->second.kind == Kind::Fx)      ? 0.96f
                                                   : 0.90f;   // synth / sample
        if (!l.seeded)
        {
            // A new node eases in from its parent's current spot if there is one.
            auto pit = m_layout.find(n->second.parent);
            if (pit != m_layout.end() && pit->seeded) { l.cx = pit->cx; l.cy = pit->cy; }
            else                                     { l.cx = l.tx; l.cy = l.ty; }
            l.seeded = true;
        }
    }
}

void NodeTreeGraph::setTree(const std::vector<LiveNode>& live)
{
    if (m_tree.observe(live, now())) computeTargets();
    kick();
}

void NodeTreeGraph::kick()
{
    if (snapping())
        for (auto& l : m_layout) { l.cx = l.tx; l.cy = l.ty; }
    // While the animation ticks, the next step renders this tree, and the
    // animation stops itself once nothing moves or fades; hidden (no tick
    // coming), render here.
    if (isVisible()) startAnimating();
    if (!m_animating) requestRender();
    if (m_tree.nodes().empty()) update();  // paint the placeholder text directly
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

    // What has faded goes, and what is left closes up.
    if (m_tree.expire(now())) computeTargets();

    const bool snap = snapping();
    bool moving = false;
    for (auto& l : m_layout)
    {
        if (snap) { l.cx = l.tx; l.cy = l.ty; continue; }
        // visc^frames: the per-frame retained fraction compounded over real
        // elapsed time — same curve as a fixed 60fps step, correct off-cadence.
        const float step = 1.0f - std::pow(l.visc, frames);
        l.cx += (l.tx - l.cx) * step;
        l.cy += (l.ty - l.cy) * step;
        if (std::abs(l.tx - l.cx) > 0.0005f || std::abs(l.ty - l.cy) > 0.0005f) moving = true;
        else { l.cx = l.tx; l.cy = l.ty; }
    }
    requestRender();
    if (!moving && !m_tree.fading()) stopAnimating();
}

void NodeTreeGraph::requestRender()
{
    const double t = now();
    const int count = static_cast<int>(m_tree.nodes().size());
    const bool dense = count > Motion::kMaxAnimated;

    // Labels while few enough to read them.
    int labelled = 0;
    for (const auto& kv : m_tree.nodes())
        if (Motion::labelled(kv.second.kind)) ++labelled;
    const bool wasHidden = labelsHidden();
    m_crowded = Motion::crowded(labelled, m_crowded);
    m_dense = dense;
    const bool labels = m_labelsShown && !labelsHidden();
    if (labelsHidden() != wasHidden) emit labelsHiddenChanged(labelsHidden());

    // Room under the lowest row for its labels while they are switched on, so
    // the plot does not jump as they hide and show with the count.
    const QFont labelFont = font();
    const qreal labelRoom = m_labelsShown ? QFontMetricsF(labelFont).height() : 0.0;
    const qreal margin = 26;
    const qreal w = std::max<qreal>(1, width() - 2 * margin);
    const qreal h = std::max<qreal>(1, height() - 2 * margin - labelRoom);
    auto px = [&](float nx) { return margin + nx * w; };
    auto py = [&](float ny) { return margin + ny * h; };

    FrameSnapshot snap;
    snap.size = size();
    snap.dpr = devicePixelRatioF();
    snap.bg = m_bg;
    snap.text = m_text;
    snap.outline = m_outline;
    snap.labelColor = m_labelColor;
    for (int k = 0; k < 4; ++k) snap.kindColor[k] = m_kindColor[k];
    snap.font = labelFont;
    snap.dense = dense;
    snap.valid = true;

    const std::unordered_set<int32_t> under = m_tree.parentsOfLive();
    snap.edges.reserve(count);
    snap.dots.reserve(count);
    QVector<FrameSnapshot::Dot> leaves;   // painted after the groups, over them
    leaves.reserve(count);
    m_screenPos.clear();
    m_drawn.clear();
    for (int32_t id : m_order)
    {
        const auto it = m_tree.nodes().find(id);
        const auto lit = m_layout.constFind(id);
        if (it == m_tree.nodes().end() || lit == m_layout.constEnd()) continue;
        const Motion::Node& n = it->second;
        const QPointF c(px(lit->cx), py(lit->cy));
        const auto pit = m_layout.constFind(n.parent);
        if (n.parent >= 0 && pit != m_layout.constEnd())
            snap.edges.push_back({ QPointF(px(pit->cx), py(pit->cy)), c });

        const bool parentOfLive = under.count(id) > 0;
        const qreal alpha = Motion::fade(n, t, parentOfLive);
        if (alpha <= 0) continue;
        const qreal r = Motion::drawnRadius(n, dense, alpha, parentOfLive);
        m_screenPos.insert(id, c);   // hover hit-testing, GUI thread only
        (n.kind == Kind::Group ? snap.dots : leaves).push_back({ c, n.kind, r, alpha });
        QString text;
        if (labels)
        {
            text = QString::fromStdString(Motion::label(n));
            if (!text.isEmpty())
                snap.labels.push_back({ QPointF(c.x(), c.y() + r + 3), text, alpha });
        }
        m_drawn.push_back({ id, n.kind, c, r, alpha, text });
    }
    snap.dots += leaves;

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

    // Nodes: groups are circles, fx squares, sounds small diamonds, each in
    // its kind's colour with a thin outline, faded as it ends. Groups come
    // first in the list (under what they hold); the brush changes only when
    // the kind does.
    p.setPen(QPen(snap.outline, 1.0));
    int brushKind = -1;
    for (const FrameSnapshot::Dot& d : snap.dots)
    {
        if (static_cast<int>(d.kind) != brushKind)
        {
            brushKind = static_cast<int>(d.kind);
            p.setBrush(snap.kindColor[brushKind]);
        }
        p.setOpacity(d.alpha);
        const qreal x = d.c.x(), y = d.c.y(), r = d.r;
        switch (d.kind)
        {
        case Kind::Fx:
            p.drawRect(QRectF(x - r, y - r, 2 * r, 2 * r));
            break;
        case Kind::Synth:
        case Kind::Sample:
        {
            const QPointF diamond[4] = { { x, y - r - 1 }, { x + r + 1, y }, { x, y + r + 1 }, { x - r - 1, y } };
            p.drawPolygon(diamond, 4);
            break;
        }
        case Kind::Group:
            p.drawEllipse(d.c, r, r);
            break;
        }
    }

    // Labels, centred under their nodes.
    if (!snap.labels.isEmpty())
    {
        p.setFont(snap.font);
        p.setPen(snap.labelColor);
        const QFontMetricsF fm(snap.font);
        for (const FrameSnapshot::Label& l : snap.labels)
        {
            p.setOpacity(l.alpha);
            p.drawText(QPointF(l.top.x() - fm.horizontalAdvance(l.text) / 2.0, l.top.y() + fm.ascent()), l.text);
        }
    }
    p.setOpacity(1.0);
}

void NodeTreeGraph::paintEvent(QPaintEvent*)
{
    QPainter p(this);

    if (m_tree.nodes().empty())
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
    // Re-enter the animation loop (it stops itself while hidden); if nothing
    // is moving or fading it stops again after one step.
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
    for (auto it = m_screenPos.constBegin(); it != m_screenPos.constEnd(); ++it)
    {
        const QPointF d = it.value() - pos;
        const qreal d2 = d.x() * d.x() + d.y() * d.y();
        if (d2 < bestD2) { bestD2 = d2; best = it.key(); }
    }
    const auto n = m_tree.nodes().find(best);
    if (best >= 0 && n != m_tree.nodes().end())
    {
        const Motion::Node& node = n->second;
        const QString name = !node.defName.empty() ? QString::fromStdString(node.defName)
                           : node.kind == Kind::Group ? QStringLiteral("group")
                                                      : QStringLiteral("node");
        const QString text = node.ended >= 0 ? tr("%1 (%2), freed").arg(name).arg(node.id)
                                             : QStringLiteral("%1 (%2)").arg(name).arg(node.id);
        QToolTip::showText(e->globalPosition().toPoint(), text, this);
        return;
    }
    QToolTip::hideText();
}
