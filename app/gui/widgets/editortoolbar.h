//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef EDITORTOOLBAR_H
#define EDITORTOOLBAR_H

#include <QColor>
#include <QVector>
#include <QWidget>

#include "utils/tablericons.h"

class QToolButton;

// A small optional edit toolbar floated over the editor's top-right corner:
// undo/redo, cut/copy/paste and find as tabler icon buttons in the same house
// pill as the find bar (which takes over this corner while it is open —
// SonicPiScintilla swaps the two). Never takes focus from the editor.
class EditorToolbar : public QWidget
{
    Q_OBJECT
public:
    explicit EditorToolbar(QWidget* parent);

    // Same surface/ring language as the find bar so the corner reads as one
    // widget family. Hover colours follow the buffer tab bar's idiom: accent
    // background with contrast-picked glyph colour.
    void applyTheme(const QColor& surface, const QColor& surfaceText,
                    const QColor& border, const QColor& hoverBg,
                    const QColor& hoverText);

    // Anchor to the parent viewport's top-right corner.
    void reposition();

    // Track the editor zoom (delta from the default zoom level).
    void setZoom(int delta);

    // Native-format shortcut display strings for the buttons in creation order
    // (undo, redo, cut, copy, paste, find) — shown as the tooltip's key-cap
    // chip (the "tipShortcut" property, see sonicpitooltip.h). Sourced from the
    // live QActions so remapped shortcuts stay truthful.
    void setShortcuts(const QStringList& native);

signals:
    void undoRequested();
    void redoRequested();
    void cutRequested();
    void copyRequested();
    void pasteRequested();
    void findRequested();

protected:
    void paintEvent(QPaintEvent*) override;   // frosted background + border
    void showEvent(QShowEvent*) override;     // anchor on every show
    // Resting chrome is subtle; the whole bar snaps to full contrast under the
    // mouse (the same quiet-until-hover manner as the buffer tabs).
    void enterEvent(QEnterEvent*) override;
    void leaveEvent(QEvent*) override;
    bool eventFilter(QObject* obj, QEvent* ev) override;   // parent/viewport resize

private:
    QToolButton* addButton(TablerIcons::Glyph glyph, const QString& tip,
                           class QHBoxLayout* row);
    qreal zoomScale() const;
    void applyZoomMetrics();   // size + icon metrics at the current zoom
    void restyle();

    struct Entry
    {
        QToolButton* button;
        TablerIcons::Glyph glyph;
    };
    QVector<Entry> m_buttons;

    int m_zoom = 0;
    bool m_hovered = false;
    int m_hoverIndex = -1;   // button whose full-height section is hovered
    QColor m_bg = QColor(30, 30, 30);
    QColor m_fg = QColor(220, 220, 220);
    QColor m_border = QColor(127, 127, 127);
    QColor m_hoverBg = QColor(68, 68, 68);
    QColor m_hoverFg = QColor(255, 255, 255);
};

#endif // EDITORTOOLBAR_H
