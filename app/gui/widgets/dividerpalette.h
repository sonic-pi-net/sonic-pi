//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef DIVIDERPALETTE_H
#define DIVIDERPALETTE_H

#include <QColor>

#include <algorithm>
#include <cmath>

#include "model/sonicpitheme.h"
#include "widgets/chevronbutton.h"
#include "widgets/divider.h"

// Every divider's colours and every grip's, from the theme, here and nowhere
// else, so a divider and the grips on it can't drift apart: they had, the
// dividers lighting in the scroll bar's hover colour and their grips in the
// accent, from colours worked out in eight places. As the web's (style.css:
// #drawer-divider, .divider-grip): the window border at rest, the accent under
// the pointer, for the divider and its grips alike.
namespace Divider
{
// bg is the panes' either side of it.
inline Colours colours(SonicPiTheme& theme, const QColor& bg)
{
    return { bg, theme.color("WindowBorder"), theme.color("HighlightedBackground") };
}

// A divider that is only a place to grab, the log's: unseen at rest, its muted
// text under the pointer, as the web's logs divider.
inline Colours quietColours(const QColor& fg, const QColor& bg)
{
    return { bg, bg, SonicPiTheme::blend(fg, bg, 0.5) };
}

// A grip's knob and chevron, at rest and under the pointer.
struct GripColours
{
    QColor grip, hoverGrip, glyph, hoverGlyph;
};

// WCAG's contrast ratio, 1 to 21: a control's marks want 3 against what they
// lie on.
inline double contrast(const QColor& a, const QColor& b)
{
    auto lum = [](const QColor& c) {
        auto lin = [](double v) { return v <= 0.03928 ? v / 12.92 : std::pow((v + 0.055) / 1.055, 2.4); };
        return 0.2126 * lin(c.redF()) + 0.7152 * lin(c.greenF()) + 0.0722 * lin(c.blueF());
    };
    const double la = lum(a), lb = lum(b);
    return (std::max(la, lb) + 0.05) / (std::min(la, lb) + 0.05);
}

// The knob is the divider's line at rest and its hover lit. The chevron at
// rest is the muted foreground, as the web's, wherever that reads on the knob;
// where it doesn't (high contrast's grey on grey, light's pale grey) it is
// whichever of black or white does.
inline GripColours gripColours(SonicPiTheme& theme)
{
    const QColor knob = theme.color("WindowBorder");
    const QColor accent = theme.color("HighlightedBackground");
    QColor glyph = theme.mutedForeground();
    if (contrast(glyph, knob) < 3.0)
        glyph = theme.contrastingText(knob);
    return { knob, accent, glyph, theme.contrastingText(accent) };
}

inline void colourGrip(ChevronButton* grip, const GripColours& c)
{
    if (grip)
        grip->setColors(c.grip, c.hoverGrip, c.glyph, c.hoverGlyph);
}
} // namespace Divider

#endif // DIVIDERPALETTE_H
