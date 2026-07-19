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
    CirclePlus,      // zoom in / octave up
    CircleMinus,     // zoom out / octave down
    GridDots,        // Cards tab
    Book,            // Docs tab
    Radioactive,     // Logs tab
    BinaryTree,      // Debug tab (mirrored left-right)
    PlayFilled,      // card play (solid)
    StopFilled,      // card stop (solid)
    Play,            // jukebox play (outline, pairs with SquareChevronsUp)
    Stop,            // jukebox stop (outline)
    SquareChevronsUp,// card insert-at-cursor
    Upload,          // jukebox load-into-buffer
    Texture,         // card drag handle
    Copy,            // copy-to-clipboard
    Check,           // copied! confirmation tick
    Search,          // docs filter
    Restore,         // reset dials
    ChevronUp,       // find bar: previous match
    ChevronDown      // find bar: next match
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
    case Glyph::Play:
        return QStringLiteral("<path d='M7 4v16l13 -8l-13 -8' />");
    case Glyph::Stop:
        return QStringLiteral(
            "<path d='M5 7a2 2 0 0 1 2 -2h10a2 2 0 0 1 2 2v10a2 2 0 0 1 -2 2h-10a2 2 0 0 1 "
            "-2 -2l0 -10' />");
    case Glyph::SquareChevronsUp:
        return QStringLiteral(
            "<path d='M9 16l3 -3l3 3' />"
            "<path d='M9 11l3 -3l3 3' />"
            "<path d='M3 5a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-14' />");
    case Glyph::Upload:
        return QStringLiteral(
            "<path d='M4 17v2a2 2 0 0 0 2 2h12a2 2 0 0 0 2 -2v-2' />"
            "<path d='M7 9l5 -5l5 5' />"
            "<path d='M12 4l0 12' />");
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
    case Glyph::Search:
        return QStringLiteral(
            "<path d='M10 10m-7 0a7 7 0 1 0 14 0a7 7 0 1 0 -14 0' />"
            "<path d='M21 21l-6 -6' />");
    case Glyph::Restore:
        return QStringLiteral(
            "<path d='M3.06 13a9 9 0 1 0 .49 -4.087' />"
            "<path d='M3 4.001v5h5' />"
            "<path d='M11 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />");
    case Glyph::Check:
        return QStringLiteral("<path d='M5 12l5 5l10 -10' />");
    case Glyph::ChevronUp:
        return QStringLiteral("<path d='M6 15l6 -6l6 6' />");
    case Glyph::ChevronDown:
        return QStringLiteral("<path d='M6 9l6 6l6 -6' />");
    }
    return QString();
}

// The complete tinted SVG document for a glyph, for custom rendering (e.g.
// into a sub-rect of an existing painter); pixmap() below covers the usual
// case. strokeWidth applies to outline glyphs only (filled ones have no stroke).
inline QString svgMarkup(Glyph glyph, const QColor& colour, qreal strokeWidth = 2.0)
{
    if (glyphFilled(glyph))
        return QStringLiteral("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' "
                              "fill='%1' stroke='none'>%2</svg>")
            .arg(colour.name(), glyphPaths(glyph));
    return QStringLiteral("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
                          "stroke='%1' stroke-width='%3' stroke-linecap='round' "
                          "stroke-linejoin='round'>%2</svg>")
        .arg(colour.name(), glyphPaths(glyph), QString::number(strokeWidth));
}

inline QPixmap pixmap(Glyph glyph, const QColor& colour, int side, qreal dpr,
                      qreal strokeWidth = 2.0)
{
    const QString svg = svgMarkup(glyph, colour, strokeWidth);

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

inline QIcon icon(Glyph glyph, const QColor& colour, int side, qreal dpr,
                  qreal strokeWidth = 2.0)
{
    return QIcon(pixmap(glyph, colour, side, dpr, strokeWidth));
}

// The card transport ring rendered two-tone: tabler's circle-caret-right /
// circle-stop geometry (tabler has no circle-stop; the square is player-stop's
// grammar) with the circle filled solid in `disc` — its stroke kept in the
// same colour so the shape spans the full drawn ring — and the inner
// triangle/square filled in `glyphColour`. The solid badge colour scheme on
// the designed tabler shapes.
inline QPixmap transportRing(bool stop, const QColor& disc, const QColor& glyphColour,
                             int side, qreal dpr)
{
    const QString inner = stop ? QStringLiteral("<path d='M9.5 9.5h5v5h-5z' />")
                               : QStringLiteral("<path d='M15 12l-4 -4v8l4 -4' />");
    const QString svg = QStringLiteral(
        "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' "
        "stroke-width='2' stroke-linecap='round' stroke-linejoin='round'>"
        "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0' fill='%1' stroke='%1' />"
        "<g fill='%2' stroke='%2'>%3</g>"
        "</svg>")
        .arg(disc.name(), glyphColour.name(), inner);

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

// A filled glyph centred on a coloured disc — the transport badge shared by
// the quickstart cards and the docs snippets. A play triangle's centroid sits
// left of its geometric centre, so it gets a small optical nudge right.
inline QPixmap discBadge(Glyph glyph, const QColor& disc, const QColor& glyphColour,
                         int side, qreal dpr)
{
    const int dev = qRound(side * dpr);
    QPixmap pm(dev, dev);
    pm.fill(Qt::transparent);
    QPainter p(&pm);
    p.setRenderHint(QPainter::Antialiasing);
    p.setPen(Qt::NoPen);
    p.setBrush(disc);
    p.drawEllipse(QRectF(0, 0, dev, dev));
    QSvgRenderer renderer(svgMarkup(glyph, glyphColour).toUtf8());
    const qreal gs = dev * 0.58;
    const qreal nudge = (glyph == Glyph::PlayFilled) ? gs * 0.06 : 0.0;
    renderer.render(&p, QRectF((dev - gs) / 2.0 + nudge, (dev - gs) / 2.0, gs, gs));
    p.end();
    pm.setDevicePixelRatio(dpr);
    return pm;
}
}

#endif // TABLERICONS_H
