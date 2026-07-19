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
    parent->installEventFilter(this);
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
    // Quieter than the find bar: this is resting chrome, not an active mode.
    const QColor tint = SonicPiTheme::blend(m_fg, m_bg, 0.3);
    for (const Entry& e : m_buttons)
    {
        e.button->setIconSize(QSize(px, px));
        e.button->setIcon(TablerIcons::icon(e.glyph, tint, px, dpr));
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
                               const QColor& border)
{
    m_bg = surface;
    m_fg = surfaceText;
    m_border = border;
    applyZoomMetrics();   // re-render icons in the new tint
    restyle();
    update();
}

void EditorToolbar::restyle()
{
    const QColor hover = SonicPiTheme::blend(m_fg, m_bg, 0.85);
    // margin/padding zeroed explicitly: the app-wide QToolButton rule's 6dx
    // margins + 4dx padding would otherwise consume these tiny buttons' boxes.
    setStyleSheet(QString(
        "#editorToolbar { background: transparent; }"
        "#editorToolbarBtn { margin: 0; padding: 3px; background: transparent;"
        " border: none; border-radius: 4px; }"
        "#editorToolbarBtn:hover { background: %1; }")
        .arg(hover.name()));
}

void EditorToolbar::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing, true);
    // Hairline ring, unlike the find bar's bold one: the toolbar is resting
    // chrome and should sit quietly until hovered.
    const qreal penW = 1.0;
    const QRectF r = QRectF(rect()).adjusted(penW / 2, penW / 2, -penW / 2, -penW / 2);
    // The shared pill radius every other pill uses — one visual identity.
    const qreal radius = ScaleHeightForDPI(kPillRadiusDx);
    p.setPen(QPen(m_border, penW));
    p.setBrush(m_bg);
    p.drawRoundedRect(r, radius, radius);
}

bool EditorToolbar::eventFilter(QObject* obj, QEvent* ev)
{
    if (obj == parentWidget() && ev->type() == QEvent::Resize && isVisible())
        reposition();
    return false;
}
