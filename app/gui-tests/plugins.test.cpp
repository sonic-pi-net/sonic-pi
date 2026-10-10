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

// Plugin hosting is a build's (CLOCKWORK_PLUGINS). The GUI hands the build's
// answer on as SONIC_PI_PLUGINS to everything it starts, and a build without it
// leaves out of the help the Lang functions that need it.

#include <catch2/catch_test_macros.hpp>

#include <QFile>

#include "utils/plugins.h"

namespace {

// The variable as a case found it, put back afterwards.
struct PluginsVariable
{
    QByteArray before = qgetenv("SONIC_PI_PLUGINS");
    bool wasSet = qEnvironmentVariableIsSet("SONIC_PI_PLUGINS");
    ~PluginsVariable()
    {
        if (wasSet) qputenv("SONIC_PI_PLUGINS", before);
        else qunsetenv("SONIC_PI_PLUGINS");
    }
};

} // namespace

TEST_CASE("plugins: the build says whether there are any, to everything the GUI starts", "[plugins]")
{
    // Whatever the variable said, by hand or left over: the build's answer
    // wins, so Spider never offers what the engine was built without.
    PluginsVariable keep;
    for (const char* before : { "1", "0", "" })
    {
        qputenv("SONIC_PI_PLUGINS", before);
        SonicPi::adoptPluginsBuild();
        CHECK(qgetenv("SONIC_PI_PLUGINS") == (SonicPi::kPluginsBuilt ? "1" : "0"));
    }
    qunsetenv("SONIC_PI_PLUGINS");
    SonicPi::adoptPluginsBuild();
    CHECK(qgetenv("SONIC_PI_PLUGINS") == (SonicPi::kPluginsBuilt ? "1" : "0"));
}

TEST_CASE("plugins: the Lang functions that need them are known by what their docs say", "[plugins]")
{
    QVector<SonicPi::LangPage> pages(3);
    pages[0].key = "play";
    pages[1].key = "live_track"; pages[1].needs = "plugins";
    pages[2].key = "with_send";  pages[2].needs = "plugins";
    CHECK(SonicPi::pluginLangKeywords(pages) == QSet<QString>{ "live_track", "with_send" });
}

TEST_CASE("plugins: the generated Lang reference marks every track function as needing them", "[plugins]")
{
    QFile f(QStringLiteral(SP_ROOT) + "/etc/doc/generated/native/reference/lang.json");
    REQUIRE(f.open(QIODevice::ReadOnly));
    const QSet<QString> needing = SonicPi::pluginLangKeywords(SonicPi::TutorialDocs::langPagesFromJson(f.readAll()));
    for (const char* fn : { "live_track", "with_send", "use_track", "with_track", "current_track",
                            "track_midi", "track_midi_note_on", "track_midi_note_off", "track_midi_cc",
                            "track_midi_pitch_bend", "track_midi_all_notes_off", "track_control", "tracks" })
        CHECK(needing.contains(QString::fromLatin1(fn)));
    CHECK_FALSE(needing.contains("play"));
}
