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

#include <QTemporaryDir>

#include "utils/gui_settings.h"

// A gui.ini of the test's own, put back as it was afterwards: what a widget
// keeps between launches never reaches the user's.
struct OwnSettings
{
    QTemporaryDir dir;
    QString before = SonicPi::guiSettingsPath();
    OwnSettings() { SonicPi::setGuiSettingsPath(dir.filePath("gui.ini")); }
    ~OwnSettings() { SonicPi::setGuiSettingsPath(before); }
};
