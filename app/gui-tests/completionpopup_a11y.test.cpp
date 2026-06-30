// Accessibility tests for the code-completion popup.
//
// These pin the two fixes for the v5-beta-4 screen-reader feedback without
// needing an actual screen reader: they assert the accessibility *data* that
// VoiceOver / NVDA / Narrator / Orca consume.
//
//   Move 1 — the popup must be absent from the accessibility tree, so a screen
//            reader never announces it as a "dialog" and never shifts its review
//            context off the editor (which used to silence the typing echo).
//   Move 2 — navigating the popup must emit a spoken description of the
//            highlighted suggestion, while *refiltering* (a keystroke) must stay
//            silent so announcements never talk over the user's typing.

#include <catch2/catch_test_macros.hpp>

#include <QAccessible>
#include <QList>
#include <QPoint>
#include <QSignalSpy>

#include "completionpopup.h"
#include "utils/scintilla_api.h"

namespace {

CompletionItem item(const QString& text, const QString& kind)
{
    CompletionItem it;
    it.text = text;
    it.kind = kind;
    return it;
}

QList<CompletionItem> synthList()
{
    return { item("pretty_bells", "synth"),
             item("prophet", "synth"),
             item("pulse", "synth") };
}

// A popup already showing the 3-item synth list — the common setup for the
// announcement tests below.
struct ShownPopup
{
    CompletionPopup popup;
    ShownPopup() { popup.showItems(synthList(), QPoint(0, 0), 12); }
};

} // namespace

TEST_CASE("popup is pruned from the accessibility tree", "[a11y][completion]")
{
    QAccessible::setActive(true);

    CompletionPopup popup;   // constructor installs the accessibility-ignore factory

    QAccessibleInterface* iface = QAccessible::queryAccessibleInterface(&popup);
    REQUIRE(iface != nullptr);

    // NoRole + invisible + no children => bridges skip the node and don't
    // descend into the popup's child widgets.
    CHECK(iface->role() == QAccessible::NoRole);
    CHECK(iface->state().invisible);
    CHECK(iface->childCount() == 0);
    CHECK(iface->child(0) == nullptr);
}

TEST_CASE_METHOD(ShownPopup, "highlighted suggestion has a screen-reader phrase", "[a11y][completion]")
{
    // Lists default to the first row.
    CHECK(popup.currentAnnouncement() == QStringLiteral("pretty_bells, synth, 1 of 3"));
}

TEST_CASE("announcement includes the summary, and the full doc is readable on demand", "[a11y][completion]")
{
    CompletionItem it;
    it.text = "prophet";
    it.kind = "synth";
    it.summary = "analogue-style synth";
    it.doc = "<p>The <b>Prophet</b> synth.</p>";

    CompletionPopup popup;
    popup.showItems({ it }, QPoint(0, 0), 12);

    // The summary (what a screen reader can't see in the docs pane) rides along.
    CHECK(popup.currentAnnouncement() == QStringLiteral("prophet, synth, analogue-style synth, 1 of 1"));
    // The full docstring is available as plain text for on-demand reading.
    CHECK(popup.currentDoc() == QStringLiteral("The Prophet synth."));
}

TEST_CASE_METHOD(ShownPopup, "navigation announces the new selection", "[a11y][completion]")
{
    QSignalSpy spy(&popup, &CompletionPopup::announceRequested);

    popup.moveSelection(+1);
    CHECK(popup.currentAnnouncement() == QStringLiteral("prophet, synth, 2 of 3"));
    REQUIRE(spy.count() == 1);
    CHECK(spy.takeFirst().at(0).toString() == QStringLiteral("prophet, synth, 2 of 3"));

    // Clamps at the end rather than wrapping; still announces.
    popup.moveSelection(+5);
    CHECK(popup.currentAnnouncement() == QStringLiteral("pulse, synth, 3 of 3"));
    REQUIRE(spy.count() == 1);
    CHECK(spy.takeFirst().at(0).toString() == QStringLiteral("pulse, synth, 3 of 3"));
}

TEST_CASE_METHOD(ShownPopup, "refiltering stays silent so it never talks over typing", "[a11y][completion]")
{
    QSignalSpy spy(&popup, &CompletionPopup::announceRequested);

    // A keystroke that narrows the list (here: down to two candidates) is a
    // re-show, not a navigation — it must not announce.
    popup.showItems({ item("prophet", "synth"), item("pulse", "synth") },
                    QPoint(0, 0), 12);

    CHECK(spy.count() == 0);
}
