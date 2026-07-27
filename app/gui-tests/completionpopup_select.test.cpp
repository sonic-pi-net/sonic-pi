// Pins the Return-key contract for note lists (v5-RC3 feedback: typing
// `play :c4` then Return silently rewrote the note as `60`).
//
// The popup tracks whether the user has actually moved the highlight since the
// list was (re)shown. The editor only lets Return commit a note completion when
// that's true — an untouched default highlight means "the typed text is already
// the value", so Return falls through to newline instead.

#include <catch2/catch_test_macros.hpp>

#include <QList>
#include <QPoint>

#include "completionpopup.h"
#include "utils/scintilla_api.h"

namespace {

QList<CompletionItem> noteList()
{
    QList<CompletionItem> items;
    for (int n = 58; n <= 62; ++n) {
        CompletionItem it;
        it.text = QString::number(n);
        it.kind = "note";
        it.note = n;
        items.append(it);
    }
    return items;
}

} // namespace

TEST_CASE("initial default highlight is not a user selection", "[completion][note]")
{
    CompletionPopup popup;
    popup.showItems(noteList(), QPoint(0, 0), 12, 60);

    CHECK(popup.isNoteMode());
    CHECK(popup.currentText() == "60");   // resolved highlight is armed…
    CHECK_FALSE(popup.hasUserSelection()); // …but the user never chose it
}

TEST_CASE("navigating the list is a user selection", "[completion][note]")
{
    CompletionPopup popup;
    popup.showItems(noteList(), QPoint(0, 0), 12, 60);

    popup.moveSelection(+1);
    CHECK(popup.hasUserSelection());
    CHECK(popup.currentText() == "61");
}

TEST_CASE("refiltering resets the user selection", "[completion][note]")
{
    CompletionPopup popup;
    popup.showItems(noteList(), QPoint(0, 0), 12, 60);
    popup.moveSelection(-1);
    REQUIRE(popup.hasUserSelection());

    // A keystroke re-derives the list → showItems runs again: the previous
    // navigation no longer reflects a choice about the new list.
    popup.showItems(noteList(), QPoint(0, 0), 12, 59);
    CHECK_FALSE(popup.hasUserSelection());
    CHECK(popup.currentText() == "59");
}
