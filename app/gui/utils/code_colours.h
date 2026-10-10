//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include "model/sonicpitheme.h"
#include "utils/tutorialdocs.h"

namespace SonicPi
{

// The editor's token colours, from the theme, for every rendering of code
// outside the editor — the cards, the tutorial, the tracks panel — and the
// theme entries the web colours the same kinds with (highlight.js).
inline CodeColours codeColours(SonicPiTheme* theme)
{
    CodeColours c;
    c.keyword = theme->color("KeywordForeground").name();
    c.symbol = theme->color("SymbolForeground").name();
    c.number = theme->color("NumberForeground").name();
    c.string = theme->color("DoubleQuotedStringForeground").name();
    c.comment = theme->color("CommentForeground").name();
    c.regex = theme->color("RegexForeground").name();
    c.def = theme->color("FunctionMethodNameForeground").name();
    c.ivar = theme->color("InstanceVariableForeground").name();
    c.constant = theme->color("ClassNameForeground").name();
    return c;
}

} // namespace SonicPi
