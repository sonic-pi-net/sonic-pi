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

// Pins the docs pane's assistive-technology contract: prose blocks read as
// static text (the pane replaced a QTextBrowser, which gave this for free),
// and dials expose the standard value interface.

#include <catch2/catch_test_macros.hpp>

#include <QAccessible>

#include "widgets/tutorialwidgets.h"

TEST_CASE("prose blocks read their full text to screen readers", "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    TutProseText prose(nullptr);
    prose.setHtml("<p>Hello <b>world</b>, this is the tutorial.</p>");

    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&prose);
    REQUIRE(iface != nullptr);
    CHECK(iface->role() == QAccessible::StaticText);
    CHECK(iface->text(QAccessible::Name) == "Hello world, this is the tutorial.");
}

TEST_CASE("dials expose the value interface", "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    TutDial dial("cutoff", 30, 130, 110, nullptr, nullptr);

    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&dial);
    REQUIRE(iface != nullptr);
    CHECK(iface->role() == QAccessible::Dial);
    CHECK(iface->text(QAccessible::Name) == "cutoff");

    QAccessibleValueInterface* value = iface->valueInterface();
    REQUIRE(value != nullptr);
    CHECK(value->currentValue().toDouble() == 110.0);
    CHECK(value->minimumValue().toDouble() == 30.0);
    CHECK(value->maximumValue().toDouble() == 130.0);

    value->setCurrentValue(80.0);
    CHECK(dial.value() == 80.0);
    CHECK(iface->text(QAccessible::Value) == "80");
}

TEST_CASE("the piano advertises its keyboard mapping", "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    TutPiano piano(nullptr, nullptr);
    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&piano);
    REQUIRE(iface != nullptr);
    CHECK(iface->text(QAccessible::Name).contains("Piano"));
}
