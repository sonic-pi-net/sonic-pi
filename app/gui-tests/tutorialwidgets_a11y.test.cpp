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
// read-only text with a full text interface and keyboard caret (the pane
// replaced a QTextBrowser, which gave all of this for free), and dials
// expose the standard value interface.

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include <QAccessible>
#include <QLabel>
#include <QLineEdit>
#include <QTest>

#include "widgets/tutorialwidgets.h"

namespace {
// Whether QAccessible::updateAccessibility routes events to a test update
// handler varies by Qt version and platform: older Qt requires accessibility
// to be active first, and setActive(true) only sticks where the platform has
// an accessibility backend (offscreen grew one in Qt 6.5; bookworm's 6.4.2 on
// the i686 CI has none), while newer Qt calls the handler unconditionally.
// Probe with a real event instead of pinning versions.
bool a11yUpdateHandlerReceivesEvents()
{
    QAccessible::setActive(true);
    static bool delivered;
    delivered = false;
    QAccessible::UpdateHandler prev =
        QAccessible::installUpdateHandler([](QAccessibleEvent*) { delivered = true; });
    QLabel probe;
    QAccessibleEvent ev(&probe, QAccessible::NameChanged);
    QAccessible::updateAccessibility(&ev);
    QAccessible::installUpdateHandler(prev);
    return delivered;
}
} // namespace

TEST_CASE("prose blocks read as caret-navigable text to screen readers",
          "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    TutProseText prose(nullptr);
    prose.setHtml("<p>Hello <b>world</b>, this is the tutorial.</p>");

    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&prose);
    REQUIRE(iface != nullptr);
    // A text area (read-only), not a static label: that is the shape screen
    // readers offer caret navigation for.
    CHECK(iface->role() == QAccessible::EditableText);
    CHECK(iface->state().readOnly);
    CHECK(iface->state().selectableText);
    CHECK(iface->text(QAccessible::Value) == "Hello world, this is the tutorial.");

    QAccessibleTextInterface* text = iface->textInterface();
    REQUIRE(text != nullptr);
    CHECK(text->characterCount() == QString("Hello world, this is the tutorial.").size());
    CHECK(text->text(0, 5) == "Hello");
    CHECK(text->text(6, 11) == "world");

    // <br> produces U+2028 line separators, which screen readers speak as
    // junk glyphs — plainText must normalise them to real newlines.
    TutProseText code(nullptr);
    code.setHtml("<p>play 60<br>sleep 1</p>");
    CHECK(code.plainText() == "play 60\nsleep 1");
    CHECK(!code.plainText().contains(QChar::LineSeparator));

    // The interface drives the widget's caret and selection directly.
    text->setCursorPosition(6);
    CHECK(prose.caretPosition() == 6);
    CHECK(text->cursorPosition() == 6);
    text->setSelection(0, 6, 11);
    int start = -1, end = -1;
    text->selection(0, &start, &end);
    CHECK(start == 6);
    CHECK(end == 11);
    CHECK(prose.selectedText() == "world");
    text->removeSelection(0);
    CHECK(text->selectionCount() == 0);
}

TEST_CASE("arrow keys walk the prose caret and hand off at the edges",
          "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    const bool a11yEventsDeliverable = a11yUpdateHandlerReceivesEvents();
    TutProseText prose(nullptr);
    prose.resize(400, 60);
    prose.setHtml("<p>ab</p>");

    // Screen readers follow these caret moves via the cursor events the
    // widget fires; capture them through the test update handler.
    static QVector<int> caretEvents;
    caretEvents.clear();
    QAccessible::UpdateHandler prev = QAccessible::installUpdateHandler([](QAccessibleEvent* ev) {
        if (ev->type() == QAccessible::TextCaretMoved)
            caretEvents.append(static_cast<QAccessibleTextCursorEvent*>(ev)->cursorPosition());
    });

    QTest::keyClick(&prose, Qt::Key_Right);
    CHECK(prose.caretPosition() == 1);
    QTest::keyClick(&prose, Qt::Key_Right);
    CHECK(prose.caretPosition() == 2);
    if (a11yEventsDeliverable)
        CHECK(caretEvents == QVector<int>({ 1, 2 }));

    // Shift extends a selection as it moves.
    QTest::keyClick(&prose, Qt::Key_Left, Qt::ShiftModifier);
    CHECK(prose.selectedText() == "b");

    // Stepping off the end hands the caret to the next block (continuous
    // reading across the page).
    int exitedDirection = 0;
    prose.setCaretExitHandler(
        [&exitedDirection](TutProseText*, int direction) { exitedDirection = direction; });
    prose.setCaretPosition(2);
    QTest::keyClick(&prose, Qt::Key_Right);
    CHECK(exitedDirection == +1);
    prose.setCaretPosition(0);
    QTest::keyClick(&prose, Qt::Key_Left);
    CHECK(exitedDirection == -1);

    QAccessible::installUpdateHandler(prev);
}

TEST_CASE("doc titles read as headings with levels", "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    TutHeading h1("Synths", 1);
    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&h1);
    REQUIRE(iface != nullptr);
    // Heading role feeds the screen reader's section navigation (rotor / H key)
    CHECK(iface->role() == QAccessible::Heading);
    CHECK(iface->text(QAccessible::Name) == "Synths");
    CHECK(iface->text(QAccessible::Value) == "1");

    TutHeading h2("Examples", 2);
    QAccessibleInterface* iface2 = QAccessible::queryAccessibleInterface(&h2);
    REQUIRE(iface2 != nullptr);
    CHECK(iface2->role() == QAccessible::Heading);
    CHECK(iface2->text(QAccessible::Value) == "2");
}

TEST_CASE("Return follows the link under the caret", "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    TutProseText prose(nullptr);
    prose.resize(400, 60);
    prose.setHtml("<p>See <a href=\"sonicpi://help/synths\">the synths</a> page.</p>");
    QString opened;
    prose.setLinkHandler([&opened](const QString& href) { opened = href; });

    // Caret outside the link: Return does nothing.
    prose.setCaretPosition(0);
    QTest::keyClick(&prose, Qt::Key_Return);
    CHECK(opened.isEmpty());
    CHECK(prose.anchorAtPosition(0).isEmpty());

    // Caret entering the link announces it once, and Return follows it.
    QStringList announced;
    prose.setAnnounceHandler([&announced](const QString& msg) { announced << msg; });
    prose.setCaretPosition(6); // inside "the synths"
    CHECK(prose.anchorAtPosition(6) == "sonicpi://help/synths");
    CHECK(announced.size() == 1);
    prose.setCaretPosition(7); // still inside: no re-announcement
    CHECK(announced.size() == 1);
    QTest::keyClick(&prose, Qt::Key_Return);
    CHECK(opened == "sonicpi://help/synths");
}

TEST_CASE("dials expose the value interface", "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    const bool a11yEventsDeliverable = a11yUpdateHandlerReceivesEvents();
    TutDial dial("cutoff", 30, 130, 110, nullptr, nullptr);

    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&dial);
    REQUIRE(iface != nullptr);
    // Slider, not Dial: the Dial role has no macOS mapping (silent to VoiceOver)
    CHECK(iface->role() == QAccessible::Slider);
    CHECK(iface->text(QAccessible::Name) == "cutoff");

    QAccessibleValueInterface* value = iface->valueInterface();
    REQUIRE(value != nullptr);
    CHECK(value->currentValue().toDouble() == 110.0);
    CHECK(value->minimumValue().toDouble() == 30.0);
    CHECK(value->maximumValue().toDouble() == 130.0);

    value->setCurrentValue(80.0);
    CHECK(dial.value() == 80.0);
    CHECK(iface->text(QAccessible::Value) == "80");

    // Turning the knob fires the value-change event a screen reader speaks.
    static int valueEvents;
    valueEvents = 0;
    QAccessible::UpdateHandler prev = QAccessible::installUpdateHandler([](QAccessibleEvent* ev) {
        if (ev->type() == QAccessible::ValueChanged)
            ++valueEvents;
    });
    dial.setValue(90.0);
    if (a11yEventsDeliverable)
        CHECK(valueEvents == 1);
    dial.setValue(90.0); // unchanged: no re-announcement
    if (a11yEventsDeliverable)
        CHECK(valueEvents == 1);
    QAccessible::installUpdateHandler(prev);
}

TEST_CASE("dials in a cluster navigate with Left/Right and adjust with Up/Down",
          "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    TutDial dial("cutoff", 0, 100, 50, nullptr, nullptr);

    // Standalone (no handler): Left/Right adjust, as before.
    QTest::keyClick(&dial, Qt::Key_Right);
    CHECK(dial.value() > 50.0);

    // In a cluster: Left/Right hand off to navigation, Up/Down still adjust.
    dial.setValue(50.0, false);
    int navigated = 0;
    dial.setNavigateHandler([&navigated](TutDial*, int direction) { navigated = direction; });
    QTest::keyClick(&dial, Qt::Key_Right);
    CHECK(navigated == +1);
    CHECK(dial.value() == 50.0); // navigation must not change the value
    QTest::keyClick(&dial, Qt::Key_Left);
    CHECK(navigated == -1);
    QTest::keyClick(&dial, Qt::Key_Up);
    CHECK(dial.value() > 50.0);
    // Shift+Left stays a coarse adjustment even in a cluster
    const double before = dial.value();
    QTest::keyClick(&dial, Qt::Key_Left, Qt::ShiftModifier);
    CHECK(dial.value() < before);
}

TEST_CASE("the dial's value editor edits in place and closes on focus-out", "[tutorialwidgets]")
{
    TutDial dial("attack", 0, 4, 0.0, nullptr, nullptr);
    dial.grab(); // force a paint so the in-arc value rect exists
    const QRect valRect = dial.valueRectForTest();
    REQUIRE(!valRect.isEmpty());

    // A clean click on the value opens the inline editor, centred on the
    // value it replaces — never down over the opt name (the name row starts
    // below the value rect).
    QTest::mouseClick(&dial, Qt::LeftButton, Qt::KeyboardModifiers(), valRect.center());
    QLineEdit* editor = dial.findChild<QLineEdit*>();
    REQUIRE(editor != nullptr);
    CHECK(editor->geometry().center().y() == Catch::Approx(valRect.center().y()).margin(2));
    CHECK(editor->geometry().bottom() <= valRect.bottom() + valRect.height());

    // Focus-out without a modification must still close it: Qt only emits
    // editingFinished when the text changed, and the untouched editor used
    // to linger over the dial for good.
    QFocusEvent out(QEvent::FocusOut);
    QApplication::sendEvent(editor, &out);
    QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
    CHECK(dial.findChild<QLineEdit*>() == nullptr);
    CHECK(dial.value() == 0.0);

    // Typing a value and committing with Return applies it and closes.
    QTest::mouseClick(&dial, Qt::LeftButton, Qt::KeyboardModifiers(), valRect.center());
    editor = dial.findChild<QLineEdit*>();
    REQUIRE(editor != nullptr);
    QTest::keyClicks(editor, "2.5"); // replaces the select-all'd old value
    QTest::keyClick(editor, Qt::Key_Return);
    QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
    CHECK(dial.findChild<QLineEdit*>() == nullptr);
    CHECK(dial.value() == 2.5);
}

TEST_CASE("the piano advertises its keyboard mapping", "[tutorialwidgets][a11y]")
{
    registerTutorialWidgetAccessibility();
    TutPiano piano(nullptr, nullptr);
    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&piano);
    REQUIRE(iface != nullptr);
    CHECK(iface->text(QAccessible::Name).contains("Piano"));
}

TEST_CASE("line navigation yields each visual line once, not the paragraph",
          "[tutorialwidgets][a11y]")
{
    // VoiceOver's plain Up/Down arrows read by *visual* line via the text
    // interface's LineBoundary queries. The inherited implementations derive
    // lines from newline characters, so a wrapped paragraph (which has none)
    // came back whole for every visual line the cursor stepped through —
    // spoken 2-3 times per paragraph (user report). Lines must instead map to
    // the laid-out QTextLines.
    registerTutorialWidgetAccessibility();
    TutProseText prose(nullptr);
    prose.setHtml(
        "<p>One of the most exciting aspects of Sonic Pi is that it enables "
        "you to write and modify code live to make music, just like you "
        "might perform live with a guitar.</p>"
        "<p>Second paragraph closes the page.</p>");
    prose.resize(180, 800);   // narrow enough to force the first paragraph to wrap

    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&prose);
    REQUIRE(iface != nullptr);
    QAccessibleTextInterface* text = iface->textInterface();
    REQUIRE(text != nullptr);

    // Offset 0 must yield the first visual line — a fragment, not the paragraph.
    int s = -1, e = -1;
    const QString firstLine =
        text->textAtOffset(0, QAccessible::LineBoundary, &s, &e);
    REQUIRE(s == 0);
    REQUIRE(e > 0);
    CHECK(firstLine.size() < 60);   // the paragraph itself is ~160 chars

    // Walk the whole document line by line: ranges advance strictly, nothing
    // repeats, and both paragraphs are visited.
    QStringList lines;
    int offset = 0;
    for (int guard = 0; guard < 100; ++guard) {
        int ls = -1, le = -1;
        const QString line =
            text->textAtOffset(offset, QAccessible::LineBoundary, &ls, &le);
        if (ls < 0)
            break;
        lines << line;
        int as = -1, ae = -1;
        text->textAfterOffset(offset, QAccessible::LineBoundary, &as, &ae);
        if (as < 0)
            break;
        REQUIRE(as > ls);   // strictly forward - a repeat would loop here
        offset = as;
    }
    CHECK(lines.size() >= 3);   // wrapped paragraph (2+) plus the second one
    for (int i = 1; i < lines.size(); ++i)
        CHECK(lines[i] != lines[i - 1]);
    // Joined, the walk reconstructs both paragraphs (wrap points are
    // font-dependent, so no exact per-line text assertions).
    CHECK(lines.join(' ').contains("perform live with a guitar."));
    CHECK(lines.join(' ').contains("Second paragraph closes the page."));

    // An offset in the middle of the paragraph resolves to its own short
    // line too (this is the exact query the repetition bug got wrong).
    int ms = -1, me = -1;
    const QString mid =
        text->textAtOffset(text->characterCount() / 2, QAccessible::LineBoundary, &ms, &me);
    CHECK(mid.size() < 60);
    CHECK(ms > 0);

    // Stepping backwards from the second line lands on the first.
    int bs = -1, be = -1;
    int l2s = -1, l2e = -1;
    text->textAfterOffset(0, QAccessible::LineBoundary, &l2s, &l2e);
    REQUIRE(l2s > 0);
    const QString back =
        text->textBeforeOffset(l2s, QAccessible::LineBoundary, &bs, &be);
    CHECK(bs == 0);
    CHECK(back == firstLine);
}
