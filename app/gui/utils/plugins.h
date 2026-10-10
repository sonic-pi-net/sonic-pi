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

#include <QByteArray>
#include <QSet>
#include <QString>
#include <QVector>

#include "utils/tutorialdocs.h"

// Plugin hosting (VST3 and CLAP, on plugin tracks) is a build's:
// CLOCKWORK_PLUGINS (app/external/CMakeLists.txt) builds the engine with it and
// its bridge beside it, and tells the GUI so (SONIC_PI_PLUGINS). A build
// without it offers none of it: no Tracks tab, no track functions in the help,
// and Spider refuses them (server/ruby/lib/sonicpi/plugins.rb), told by the
// GUI as everything it starts is.
#ifndef SONIC_PI_PLUGINS
#define SONIC_PI_PLUGINS 0
#endif

namespace SonicPi
{

constexpr bool kPluginsBuilt = SONIC_PI_PLUGINS != 0;

// Before anything is started: the variable says what the build is, whatever
// it said before.
inline void adoptPluginsBuild()
{
    qputenv("SONIC_PI_PLUGINS", kPluginsBuilt ? "1" : "0");
}

// The Lang functions that need plugin hosting, as their docs say.
inline QSet<QString> pluginLangKeywords(const QVector<LangPage>& pages)
{
    QSet<QString> keys;
    for (const LangPage& p : pages)
        if (p.needs == QLatin1String("plugins"))
            keys.insert(p.key);
    return keys;
}

} // namespace SonicPi
