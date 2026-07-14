//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef TABLERICONS_H
#define TABLERICONS_H

#include <QColor>
#include <QIcon>
#include <QPainter>
#include <QPixmap>
#include <QRectF>
#include <QString>
#include <QSvgRenderer>

// The handful of tabler.io outline icons the GUI uses as vector glyphs. The
// verbatim tabler SVG paths (24x24 viewBox, 2px round strokes) are rendered via
// QSvgRenderer, tinted by swapping the stroke colour and drawn at the device
// pixel ratio so they stay crisp at any DPI.
namespace TablerIcons
{
enum class Glyph
{
    X,            // close
    SquareX,      // close (boxed)
    CirclePlus,   // zoom in
    CircleMinus   // zoom out
};

// The inner <path> markup for each glyph, straight from tabler.io.
inline QString glyphPaths(Glyph glyph)
{
    switch (glyph)
    {
    case Glyph::X:
        return QStringLiteral("<path d='M18 6l-12 12' /><path d='M6 6l12 12' />");
    case Glyph::SquareX:
        return QStringLiteral(
            "<path d='M3 5a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-14' />"
            "<path d='M9 9l6 6m0 -6l-6 6' />");
    case Glyph::CirclePlus:
        return QStringLiteral(
            "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0' />"
            "<path d='M9 12h6' /><path d='M12 9v6' />");
    case Glyph::CircleMinus:
        return QStringLiteral(
            "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0' /><path d='M9 12h6' />");
    }
    return QString();
}

inline QPixmap pixmap(Glyph glyph, const QColor& colour, int side, qreal dpr)
{
    const QString svg = QStringLiteral(
        "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
        "stroke='%1' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'>%2</svg>")
        .arg(colour.name(), glyphPaths(glyph));

    QSvgRenderer renderer(svg.toUtf8());
    QPixmap pm(QSize(side, side) * dpr);
    pm.fill(Qt::transparent);
    QPainter p(&pm);
    p.setRenderHint(QPainter::Antialiasing, true);
    renderer.render(&p, QRectF(0, 0, side * dpr, side * dpr));
    p.end();
    pm.setDevicePixelRatio(dpr);
    return pm;
}

inline QIcon icon(Glyph glyph, const QColor& colour, int side, qreal dpr)
{
    return QIcon(pixmap(glyph, colour, side, dpr));
}
}

#endif // TABLERICONS_H
