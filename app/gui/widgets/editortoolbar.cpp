//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "editortoolbar.h"

#include <QAbstractScrollArea>
#include <QCursor>
#include <QEvent>
#include <QHBoxLayout>
#include <QMouseEvent>
#include <QPainter>
#include <QPainterPath>
#include <QTimer>
#include <QToolButton>
#include <QVariantAnimation>

#include "dpi.h"
#include "model/sonicpitheme.h"

EditorToolbar::EditorToolbar(QWidget* parent)
    : QWidget(parent)
{
    setObjectName("editorToolbar");

    // Dwell-to-wake: a ghosted pill is mouse-transparent, so the wake gesture
    // is watched on the viewport's pointer moves instead. The delay sits above
    // a normal aim-and-click pause, so a quick click on the code beneath lands
    // in the editor while a deliberate rest on the pill wakes the buttons.
    m_dwellTimer = new QTimer(this);
    m_dwellTimer->setSingleShot(true);
    m_dwellTimer->setInterval(450);
    connect(m_dwellTimer, &QTimer::timeout, this, &EditorToolbar::wakeFromGhost);

    // One mix value fades the pill fill, border and glyph tints between the
    // interactive chrome (0) and the faint ghost (1) — the fade doubles as the
    // signal that the corner has changed hands.
    m_ghostAnim = new QVariantAnimation(this);
    m_ghostAnim->setDuration(180);
    m_ghostAnim->setEasingCurve(QEasingCurve::OutCubic);
    connect(m_ghostAnim, &QVariantAnimation::valueChanged, this, [this](const QVariant& v) {
        m_ghostMix = v.toReal();
        applyZoomMetrics();
        update();
    });

    // The hide() below already delivers a QHideEvent, so every member the
    // event handlers touch must exist by this point.
    hide();

    QHBoxLayout* row = new QHBoxLayout(this);
    row->setContentsMargins(ScaleWidthForDPI(10), ScaleHeightForDPI(4),
                            ScaleWidthForDPI(10), ScaleHeightForDPI(4));
    row->setSpacing(ScaleWidthForDPI(2));

    QToolButton* undo = addButton(TablerIcons::Glyph::ArrowBackUp, tr("Undo"), row);
    QToolButton* redo = addButton(TablerIcons::Glyph::ArrowForwardUp, tr("Redo"), row);
    row->addSpacing(ScaleWidthForDPI(6));
    QToolButton* cut = addButton(TablerIcons::Glyph::Scissors, tr("Cut"), row);
    QToolButton* copy = addButton(TablerIcons::Glyph::Copy, tr("Copy"), row);
    QToolButton* paste = addButton(TablerIcons::Glyph::Clipboard, tr("Paste"), row);
    row->addSpacing(ScaleWidthForDPI(6));
    QToolButton* find = addButton(TablerIcons::Glyph::Search, tr("Find"), row);

    connect(undo, &QToolButton::clicked, this, [this] { emit undoRequested(); });
    connect(redo, &QToolButton::clicked, this, [this] { emit redoRequested(); });
    connect(cut, &QToolButton::clicked, this, [this] { emit cutRequested(); });
    connect(copy, &QToolButton::clicked, this, [this] { emit copyRequested(); });
    connect(paste, &QToolButton::clicked, this, [this] { emit pasteRequested(); });
    connect(find, &QToolButton::clicked, this, [this] { emit findRequested(); });

    applyZoomMetrics();

    // Reposition when the editor is resized (the bar hugs its top-right corner).
    // The anchor is computed from the VIEWPORT's geometry, which also moves
    // without the editor resizing — e.g. when the line-number margin widens as
    // a buffer loads, or a scrollbar appears — so watch the viewport too.
    parent->installEventFilter(this);
    if (auto* scroll = qobject_cast<QAbstractScrollArea*>(parent))
        scroll->viewport()->installEventFilter(this);
}

QToolButton* EditorToolbar::addButton(TablerIcons::Glyph glyph, const QString& tip,
                                      QHBoxLayout* row)
{
    QToolButton* b = new QToolButton(this);
    b->setObjectName("editorToolbarBtn");
    b->setToolTip(tip);
    b->setAccessibleName(tip);
    b->setFocusPolicy(Qt::NoFocus);
    row->addWidget(b);
    // The bar paints the hover highlight itself (a full-height section of the
    // pill, like a buffer tab) — it needs to know which button is under the
    // mouse.
    b->installEventFilter(this);
    m_buttons.append({ b, glyph });
    return b;
}

qreal EditorToolbar::zoomScale() const
{
    return qBound(0.8, 1.0 + m_zoom * 0.08, 2.2);
}

void EditorToolbar::applyZoomMetrics()
{
    const qreal s = zoomScale();
    const int px = ScaleWidthForDPI(qRound(18 * s));
    const qreal dpr = devicePixelRatioF();
    // Quiet at rest, full contrast under the mouse: the dimmed tint keeps the
    // bar subtle while it's just sitting there, and the contrast-picked colour
    // (m_fg) takes over on hover, when legibility over the frosted code matters.
    // The button whose section is hovered gets the colour contrast-picked
    // against the accent fill, exactly as buffer tab text does.
    // Ghosted, the glyphs sink most of the way into the background so the code
    // beneath reads first; SVG tinting carries no alpha, so faintness is a
    // deeper blend toward the editor background rather than transparency.
    const qreal sink = 0.3 + 0.48 * m_ghostMix;
    const QColor resting = m_hovered ? m_fg : SonicPiTheme::blend(m_fg, m_bg, sink);
    for (int i = 0; i < m_buttons.size(); ++i)
    {
        const Entry& e = m_buttons[i];
        const QColor tint = (i == m_hoverIndex) ? m_hoverFg : resting;
        e.button->setIconSize(QSize(px, px));
        e.button->setIcon(QIcon(TablerIcons::pixmap(e.glyph, tint, px, dpr)));
    }
    setFixedHeight(ScaleHeightForDPI(qRound(44 * s)));
    adjustSize();
}

void EditorToolbar::setZoom(int delta)
{
    if (delta == m_zoom)
        return;
    m_zoom = delta;
    applyZoomMetrics();
    if (isVisible())
        reposition();
}

void EditorToolbar::setShortcuts(const QStringList& native)
{
    for (int i = 0; i < m_buttons.size() && i < native.size(); ++i)
        m_buttons[i].button->setProperty("tipShortcut", native[i]);
}

void EditorToolbar::reposition()
{
    QWidget* par = parentWidget();
    if (!par)
        return;
    QRect area = par->rect();
    if (auto* scroll = qobject_cast<QAbstractScrollArea*>(par))
        area = scroll->viewport()->geometry();   // clear of the vertical scrollbar
    move(area.right() - width() - ScaleWidthForDPI(12), area.top() + ScaleHeightForDPI(8));
    emit geometryChanged();
}

void EditorToolbar::setOccluded(bool on)
{
    if (on == m_occluded)
        return;
    m_occluded = on;
    // Pointer already resting on the pill as text arrives beneath it (e.g. a
    // long line being typed): stay awake — ghosting under the mouse would
    // yank the buttons away mid-reach. It ghosts on the next leave.
    m_awake = on && rect().contains(mapFromGlobal(QCursor::pos()));
    setAttribute(Qt::WA_TransparentForMouseEvents, ghosted());
    if (ghosted())
    {
        m_hovered = false;
        m_hoverIndex = -1;
    }
    else
        m_dwellTimer->stop();
    animateGhost(ghosted() ? 1.0 : 0.0);
}

void EditorToolbar::wakeFromGhost()
{
    if (!ghosted() || !isVisible())
        return;
    if (!rect().contains(mapFromGlobal(QCursor::pos())))
        return;
    m_awake = true;
    m_hovered = true;
    setAttribute(Qt::WA_TransparentForMouseEvents, false);
    animateGhost(0.0);
}

void EditorToolbar::animateGhost(qreal target)
{
    if (qFuzzyCompare(m_ghostMix, target))
        return;
    m_ghostAnim->stop();
    m_ghostAnim->setStartValue(m_ghostMix);
    m_ghostAnim->setEndValue(target);
    m_ghostAnim->start();
}

void EditorToolbar::applyTheme(const QColor& surface, const QColor& surfaceText,
                               const QColor& border, const QColor& hoverBg,
                               const QColor& hoverText)
{
    m_bg = surface;
    m_fg = surfaceText;
    m_border = border;
    m_hoverBg = hoverBg;
    m_hoverFg = hoverText;
    restyle();            // stylesheet first so metrics settle before sizing
    applyZoomMetrics();   // re-render icons in the new tint
    update();
    if (isVisible())
        reposition();
}

void EditorToolbar::restyle()
{
    // margin/padding zeroed explicitly: the app-wide QToolButton rule's 6dx
    // margins + 4dx padding would otherwise consume these tiny buttons' boxes.
    // No :hover rule — the bar paints the hover highlight itself as a
    // full-height section of the pill (paintEvent), not a per-button box.
    setStyleSheet(
        "#editorToolbar { background: transparent; }"
        "#editorToolbarBtn { margin: 0; padding: 3px; background: transparent;"
        " border: none; }");
}

void EditorToolbar::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing, true);
    // The frosted pill: not opaque, not fully clear — a transparent bar
    // disappears into the code beneath it and the glyphs become unreadable,
    // while this keeps the editor faintly hinted.
    const qreal penW = 1.0;
    const QRectF r = QRectF(rect()).adjusted(penW / 2, penW / 2, -penW / 2, -penW / 2);
    // The shared pill radius every other pill uses — one visual identity.
    const qreal radius = ScaleHeightForDPI(kPillRadiusDx);
    QPainterPath pill;
    pill.addRoundedRect(r, radius, radius);
    QColor frosted = m_bg;
    // Interactive it frosts to ~85% — legible chrome, code still hinted
    // beneath. Ghosted the fill thins right out so the code reads first.
    frosted.setAlpha(qRound(216 - (216 - 48) * m_ghostMix));
    p.fillPath(pill, frosted);

    // Hover highlight: the hovered button's whole vertical section of the
    // pill fills with the accent — the manner of a buffer tab — rather than
    // a small internal box floating around the glyph.
    if (m_hoverIndex >= 0 && m_hoverIndex < m_buttons.size())
    {
        const QRect g = m_buttons[m_hoverIndex].button->geometry();
        // End sections run all the way into the pill's rounded caps (the clip
        // shapes them); inner edges bleed a couple of px into the gaps.
        const qreal x1 = (m_hoverIndex == 0) ? 0 : g.left() - 2;
        const qreal x2 = (m_hoverIndex == m_buttons.size() - 1)
                             ? width() : g.left() + g.width() + 2;
        p.save();
        p.setClipPath(pill);
        p.fillRect(QRectF(x1, 0, x2 - x1, height()), m_hoverBg);
        p.restore();
    }

    QColor edge = m_border;
    edge.setAlpha(qRound(255 - (255 - 64) * m_ghostMix));
    p.setPen(QPen(edge, penW));
    p.setBrush(Qt::NoBrush);
    p.drawRoundedRect(r, radius, radius);
}

void EditorToolbar::enterEvent(QEnterEvent*)
{
    m_hovered = true;
    applyZoomMetrics();   // re-render glyphs at full contrast
    update();
}

void EditorToolbar::leaveEvent(QEvent*)
{
    m_hovered = false;
    m_hoverIndex = -1;
    if (m_awake)
    {
        // A wake lasts exactly as long as the pointer stays: leaving an
        // occluded pill hands the corner straight back to the code.
        m_awake = false;
        setAttribute(Qt::WA_TransparentForMouseEvents, m_occluded);
        animateGhost(m_occluded ? 1.0 : 0.0);
    }
    applyZoomMetrics();   // back to the subtle resting tint
    update();
}

void EditorToolbar::showEvent(QShowEvent*)
{
    // Every show re-anchors: the corner may have moved while hidden (resizes
    // while the find bar owned the corner, buffer switches, ...).
    reposition();
}

void EditorToolbar::hideEvent(QHideEvent*)
{
    // A hidden pill can't be woken; drop any half-formed dwell and snap the
    // fade so the next show starts from its resting state.
    m_dwellTimer->stop();
    m_awake = false;
    setAttribute(Qt::WA_TransparentForMouseEvents, m_occluded);
    m_ghostAnim->stop();
    m_ghostMix = m_occluded ? 1.0 : 0.0;
    applyZoomMetrics();
}

bool EditorToolbar::eventFilter(QObject* obj, QEvent* ev)
{
    // Section hover: track which button the mouse is over so paintEvent can
    // fill its slice of the pill.
    for (int i = 0; i < m_buttons.size(); ++i)
    {
        if (obj != m_buttons[i].button)
            continue;
        if (ev->type() == QEvent::Enter)
        {
            m_hoverIndex = i;
            applyZoomMetrics();
            update();
        }
        else if (ev->type() == QEvent::Leave)
        {
            if (m_hoverIndex == i) m_hoverIndex = -1;
            applyZoomMetrics();
            update();
        }
        return false;
    }

    // Track both the editor and its viewport (margin/scrollbar changes move the
    // viewport without resizing the editor). Repositioning while hidden is a
    // cheap move() and keeps the anchor correct for the next show.
    if (ev->type() == QEvent::Resize || ev->type() == QEvent::Move)
        reposition();

    // Dwell-to-wake: while ghosted the pill is skipped by hit-testing, so the
    // pointer's moves arrive here via the viewport. Resting inside the pill's
    // rect for the dwell period wakes it; drifting out cancels the wake.
    if (ghosted() && isVisible() && ev->type() == QEvent::MouseMove)
    {
        const auto* me = static_cast<QMouseEvent*>(ev);
        if (rect().contains(mapFromGlobal(me->globalPosition().toPoint())))
        {
            if (!m_dwellTimer->isActive())
                m_dwellTimer->start();
        }
        else
            m_dwellTimer->stop();
    }
    return false;
}
