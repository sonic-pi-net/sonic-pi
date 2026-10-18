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

#include <QApplication>

#include "model/sonicpitheme.h"

// The app's stylesheet on for one test, off afterwards. It is the whole
// process's, and left on it changes what later tests see: a styled box has a
// border, padding and scroll bars the unstyled one hasn't.
struct AppStylesheet
{
    SonicPiTheme theme{ nullptr, "", QStringLiteral(SP_ROOT) };
    AppStylesheet() { qApp->setStyleSheet(theme.getAppStylesheet()); }
    ~AppStylesheet() { qApp->setStyleSheet(QString()); }
    // As a theme switch does: the sheet set again over shown widgets.
    void again() { qApp->setStyleSheet(QString()); qApp->setStyleSheet(theme.getAppStylesheet()); }
};
