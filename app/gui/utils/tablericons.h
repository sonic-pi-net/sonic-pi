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
    X,               // close
    SquareX,         // close (boxed)
    CirclePlus,      // zoom in
    CircleMinus,     // zoom out
    GridDots,        // Cards tab
    Book,            // Docs tab
    Radioactive,     // Logs tab
    BinaryTree,      // Debug tab (mirrored left-right)
    PlayFilled,      // card play (solid)
    StopFilled,      // card stop (solid)
    SquareChevronsUp,// card insert-at-cursor
    Texture,         // card drag handle
    Copy,            // card copy-to-clipboard
    Check            // copied! confirmation tick
};

// Solid glyphs are tinted via fill; everything else via stroke.
inline bool glyphFilled(Glyph glyph)
{
    return glyph == Glyph::PlayFilled || glyph == Glyph::StopFilled;
}

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
    case Glyph::GridDots:
        return QStringLiteral(
            "<path d='M4 5a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
            "<path d='M11 5a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
            "<path d='M18 5a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
            "<path d='M4 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
            "<path d='M11 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
            "<path d='M18 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
            "<path d='M4 19a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
            "<path d='M11 19a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
            "<path d='M18 19a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />");
    case Glyph::Book:
        return QStringLiteral(
            "<path d='M3 19a9 9 0 0 1 9 0a9 9 0 0 1 9 0' />"
            "<path d='M3 6a9 9 0 0 1 9 0a9 9 0 0 1 9 0' />"
            "<path d='M3 6l0 13' /><path d='M12 6l0 13' /><path d='M21 6l0 13' />");
    case Glyph::Radioactive:
        return QStringLiteral(
            "<path d='M13.5 14.6l3 5.19a9 9 0 0 0 4.5 -7.79h-6a3 3 0 0 1 -1.5 2.6' />"
            "<path d='M13.5 9.4l3 -5.19a9 9 0 0 0 -9 0l3 5.19a3 3 0 0 1 3 0' />"
            "<path d='M10.5 14.6l-3 5.19a9 9 0 0 1 -4.5 -7.79h6a3 3 0 0 0 1.5 2.6' />");
    case Glyph::BinaryTree:
        return QStringLiteral(
            "<g transform='translate(24 0) scale(-1 1)'>"
            "<path d='M6 20a2 2 0 1 0 -4 0a2 2 0 0 0 4 0' />"
            "<path d='M16 4a2 2 0 1 0 -4 0a2 2 0 0 0 4 0' />"
            "<path d='M16 20a2 2 0 1 0 -4 0a2 2 0 0 0 4 0' />"
            "<path d='M11 12a2 2 0 1 0 -4 0a2 2 0 0 0 4 0' />"
            "<path d='M21 12a2 2 0 1 0 -4 0a2 2 0 0 0 4 0' />"
            "<path d='M5.058 18.306l2.88 -4.606' />"
            "<path d='M10.061 10.303l2.877 -4.604' />"
            "<path d='M10.065 13.705l2.876 4.6' />"
            "<path d='M15.063 5.7l2.881 4.61' />"
            "</g>");
    case Glyph::PlayFilled:
        return QStringLiteral(
            "<path d='M6 4v16a1 1 0 0 0 1.524 .852l13 -8a1 1 0 0 0 0 -1.704l-13 -8a1 1 0 0 0 "
            "-1.524 .852z' />");
    case Glyph::StopFilled:
        return QStringLiteral(
            "<path d='M17 4h-10a3 3 0 0 0 -3 3v10a3 3 0 0 0 3 3h10a3 3 0 0 0 3 -3v-10a3 3 0 0 0 "
            "-3 -3z' />");
    case Glyph::SquareChevronsUp:
        return QStringLiteral(
            "<path d='M9 16l3 -3l3 3' />"
            "<path d='M9 11l3 -3l3 3' />"
            "<path d='M3 5a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-14' />");
    case Glyph::Texture:
        return QStringLiteral(
            "<path d='M6 3l-3 3' /><path d='M21 18l-3 3' /><path d='M11 3l-8 8' />"
            "<path d='M16 3l-13 13' /><path d='M21 3l-18 18' /><path d='M21 8l-13 13' />"
            "<path d='M21 13l-8 8' />");
    case Glyph::Copy:
        return QStringLiteral(
            "<path d='M7 9.667a2.667 2.667 0 0 1 2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 "
            "2.667v8.666a2.667 2.667 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 "
            "-2.667l0 -8.666' />"
            "<path d='M4.012 16.737a2.005 2.005 0 0 1 -1.012 -1.737v-10c0 -1.1 .9 -2 2 -2h10c.75 "
            "0 1.158 .385 1.5 1' />");
    case Glyph::Check:
        return QStringLiteral("<path d='M5 12l5 5l10 -10' />");
    }
    return QString();
}

// The complete tinted SVG document for a glyph, for custom rendering (e.g.
// into a sub-rect of an existing painter); pixmap() below covers the usual case.
inline QString svgMarkup(Glyph glyph, const QColor& colour)
{
    const QString wrapper = glyphFilled(glyph)
        ? QStringLiteral("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' "
                         "fill='%1' stroke='none'>%2</svg>")
        : QStringLiteral("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
                         "stroke='%1' stroke-width='2' stroke-linecap='round' "
                         "stroke-linejoin='round'>%2</svg>");
    return wrapper.arg(colour.name(), glyphPaths(glyph));
}

inline QPixmap pixmap(Glyph glyph, const QColor& colour, int side, qreal dpr)
{
    const QString svg = svgMarkup(glyph, colour);

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
