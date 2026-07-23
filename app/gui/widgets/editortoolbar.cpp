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
#include <QEvent>
#include <QHBoxLayout>
#include <QPainter>
#include <QPainterPath>
#include <QToolButton>

#include "dpi.h"
#include "model/sonicpitheme.h"

EditorToolbar::EditorToolbar(QWidget* parent)
    : QWidget(parent)
{
    setObjectName("editorToolbar");
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
    const QColor resting = m_hovered ? m_fg : SonicPiTheme::blend(m_fg, m_bg, 0.3);
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
    frosted.setAlpha(216);   // ~85%: legible chrome, code still hinted beneath
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

    p.setPen(QPen(m_border, penW));
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
    applyZoomMetrics();   // back to the subtle resting tint
    update();
}

void EditorToolbar::showEvent(QShowEvent*)
{
    // Every show re-anchors: the corner may have moved while hidden (resizes
    // while the find bar owned the corner, buffer switches, ...).
    reposition();
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
    return false;
}
