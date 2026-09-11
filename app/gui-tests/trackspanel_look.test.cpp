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

// The Tracks panel is built from the house styles and nothing else: every
// control on it carries an object name app.qss already dresses for the Docs
// column, the Cards deck or a device card, so it cannot drift into a look of
// its own. This pins that, and — with SP_GUI_SHOT_DIR set — renders the panel
// offscreen to PNGs so the look can be checked without launching the app.

#include <catch2/catch_test_macros.hpp>

#include <QAbstractButton>
#include <QApplication>
#include <QComboBox>
#include <QDir>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPixmap>
#include <QSet>
#include <QString>

#include "model/sonicpitheme.h"
#include "utils/fontroles.h"
#include "widgets/cardscope.h"
#include "widgets/trackspanel.h"

namespace
{
std::vector<SonicPi::TrackInfo> sampleTracks()
{
    SonicPi::TrackNodeInfo surge;
    surge.handle = 1; surge.instrument = true; surge.channel = 0;
    surge.name = "Surge XT"; surge.vendor = "Surge Synth Team"; surge.format = "vst3";
    surge.path = "/Library/Audio/Plug-Ins/VST3/Surge XT.vst3";

    SonicPi::TrackNodeInfo verb;
    verb.handle = 2; verb.instrument = false; verb.bypass = true;   // an off device: the card fades
    verb.name = "ValhallaSupermassive"; verb.vendor = "Valhalla DSP, LLC"; verb.format = "vst3";
    verb.path = "/Library/Audio/Plug-Ins/VST3/ValhallaSupermassive.vst3";

    SonicPi::TrackInfo a; a.id = 1; a.slot = 0; a.name = "surge";
    a.sendChannel = 32; a.returnChannel = 32; a.nodes = { surge, verb };
    SonicPi::TrackInfo b; b.id = 2; b.slot = 1; b.name = "verb";
    b.sendChannel = 34; b.returnChannel = 34; b.nodes = { verb };
    SonicPi::TrackInfo c; c.id = 3; c.slot = 2; c.name = "drums";
    c.sendChannel = 36; c.returnChannel = 36; c.mute = true;
    return { a, b, c };
}

std::vector<SonicPi::TrackPluginInfo> samplePlugins()
{
    std::vector<SonicPi::TrackPluginInfo> v;
    auto add = [&](const char* name, const char* vendor, bool inst) {
        SonicPi::TrackPluginInfo p;
        p.name = name; p.vendor = vendor; p.format = "vst3"; p.instrument = inst;
        p.path = std::string("/Library/Audio/Plug-Ins/VST3/") + name + ".vst3";
        v.push_back(p);
    };
    add("Surge XT", "Surge Synth Team", true);
    add("Surge XT Effects", "Surge Synth Team", false);
    add("ValhallaSupermassive", "Valhalla DSP, LLC", false);
    add("Vital", "Vital Audio", true);
    add("Dexed", "Digital Suburban", true);
    return v;
}

void settle()
{
    for (int i = 0; i < 8; i++)
        QApplication::processEvents();
}

void shoot(QWidget& w, const QString& name)
{
    const QString dir = qEnvironmentVariable("SP_GUI_SHOT_DIR");
    if (dir.isEmpty()) return;
    QDir().mkpath(dir);
    w.grab().save(dir + QStringLiteral("/") + name + QStringLiteral(".png"));
}
} // namespace

TEST_CASE("the Tracks panel is dressed only in house styles", "[tracks][style]")
{
    const QString root = QStringLiteral(SP_ROOT);
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", root);
    QApplication::setFont(RoleFont(FontRole::Base));   // as main.cpp does
    qApp->setStyleSheet(theme->getAppStylesheet());

    TracksPanel panel(nullptr);
    panel.applyTheme(theme);
    panel.resize(1500, 560);
    panel.show();
    settle();
    shoot(panel, "tracks-empty");

    panel.onTrackPlugins(5, 0, samplePlugins());
    panel.onTracks(32, sampleTracks());
    settle();
    shoot(panel, "tracks-rig");
    // Zoomed both ways: the columns grow with their words, nothing clips.
    panel.setUserZoom(4);
    settle();
    shoot(panel, "tracks-rig-zoom-in");
    panel.setUserZoom(-3);
    settle();
    shoot(panel, "tracks-rig-zoom-out");
    panel.setUserZoom(0);
    settle();

    // The names app.qss dresses. Anything else on the panel would be a
    // control in a look of its own.
    const QSet<QString> house = {
        QStringLiteral("qsDeckPill"), QStringLiteral("qsCardBtn"),
        QStringLiteral("docsFilter"),
        QStringLiteral("docsNavList"), QStringLiteral("qsCardCombo"),
    };
    QStringList strays;
    for (QAbstractButton* b : panel.findChildren<QAbstractButton*>())
    {
        // A QLineEdit's clear button is Qt's own, styled with the field.
        if (qobject_cast<QLineEdit*>(b->parentWidget())) continue;
        if (!house.contains(b->objectName()))
            strays << QStringLiteral("button '%1' (#%2)").arg(b->text(), b->objectName());
    }
    for (QLineEdit* e : panel.findChildren<QLineEdit*>())
        if (!house.contains(e->objectName()) && !qobject_cast<QComboBox*>(e->parentWidget()))
            strays << QStringLiteral("field '%1' (#%2)").arg(e->placeholderText(), e->objectName());
    for (QListWidget* l : panel.findChildren<QListWidget*>())
        if (!house.contains(l->objectName()))
            strays << QStringLiteral("list (#%1)").arg(l->objectName());
    INFO("Controls outside the house styles: " << strays.join(QStringLiteral("; ")).toStdString());
    CHECK(strays.isEmpty());

    delete theme;
}

// A device is as wide as its name: a plugin called ValhallaSupermassive is
// written out whole on its card, in the font the card actually paints it in.
TEST_CASE("a device card carries its plugin's whole name", "[tracks][style]")
{
    const QString root = QStringLiteral(SP_ROOT);
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", root);
    QApplication::setFont(RoleFont(FontRole::Base));
    qApp->setStyleSheet(theme->getAppStylesheet());

    TracksPanel panel(nullptr);
    panel.applyTheme(theme);
    panel.resize(1500, 560);
    panel.show();
    panel.onTrackPlugins(5, 0, samplePlugins());
    panel.onTracks(32, sampleTracks());
    settle();

    int seen = 0;
    for (QLabel* l : panel.findChildren<QLabel*>(QStringLiteral("qsCardTitle")))
    {
        if (!l->isVisible()) continue;
        ++seen;
        INFO("title: " << l->text().toStdString()
             << " (label " << l->width() << "px, text "
             << l->fontMetrics().horizontalAdvance(l->text()) << "px in "
             << l->font().family().toStdString() << ")");
        CHECK_FALSE(l->text().contains(QChar(0x2026)));
        // Room for the text in the label's own (styled) font, not just ours.
        CHECK(l->width() >= l->fontMetrics().horizontalAdvance(l->text()));
    }
    CHECK(seen >= 2);
    delete theme;
}

TEST_CASE("the track header carries the quickstart cards' ring scope", "[tracks][style]")
{
    const QString root = QStringLiteral(SP_ROOT);
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", root);
    QApplication::setFont(RoleFont(FontRole::Base));
    qApp->setStyleSheet(theme->getAppStylesheet());

    TracksPanel panel(nullptr);
    panel.applyTheme(theme);
    panel.resize(1500, 560);
    panel.show();
    panel.onTrackPlugins(5, 0, samplePlugins());
    panel.onTracks(32, sampleTracks());
    settle();

    // One ring, the same widget the cards draw, square, in the header row
    // (above the chain: the device cards sit lower).
    const auto scopes = panel.findChildren<QWidget*>(QStringLiteral("tracksScope"));
    REQUIRE(scopes.size() == 1);
    CardScope* scope = dynamic_cast<CardScope*>(scopes.first());
    REQUIRE(scope);
    CHECK(scope->isVisible());
    CHECK(scope->width() == scope->height());
    CHECK(scope->width() >= 48);
    const QPoint at = scope->mapTo(&panel, QPoint(0, 0));
    for (QLabel* l : panel.findChildren<QLabel*>(QStringLiteral("qsCardTitle")))
        if (l->isVisible())
            CHECK(l->mapTo(&panel, QPoint(0, 0)).y() > at.y() + scope->height());

    // No track, no scope: the header goes with it.
    panel.onTracks(32, {});
    settle();
    CHECK_FALSE(scope->isVisible());
    delete theme;
}
