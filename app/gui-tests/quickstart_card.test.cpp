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

// A quickstart card, as the pane builds it: what it is made of, what a screen
// reader meets, and what its controls say to the window. It is drawn as the
// web's code card is (app/web/app/src/ui/card.js): Play and Stop, each a disc
// in a ring; Edit, Reset, Copy, Add and Drag; a footer with the description,
// what the program puts and its error.

#include <catch2/catch_test_macros.hpp>

#include <QAbstractButton>
#include <QApplication>
#include <QLabel>
#include <QPlainTextEdit>
#include <QScrollArea>
#include <QSignalSpy>
#include <QTest>

#include "model/sonicpitheme.h"
#include "own_settings.h"
#include "utils/fontroles.h"
#include "widgets/cardscope.h"
#include "widgets/codecard.h"
#include "widgets/quickstartpane.h"
#include "widgets/tutorialwidgets.h"

namespace {

void settle()
{
    for (int i = 0; i < 5; i++) { QApplication::processEvents(); QTest::qWait(10); }
}

struct Deck
{
    OwnSettings settings;   // edits are kept: in a gui.ini of the test's own
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    QuickstartPane pane{ theme };

    Deck()
    {
        QApplication::setFont(RoleFont(FontRole::Base));
        qApp->setStyleSheet(theme->getAppStylesheet());
        load();
        pane.resize(1400, 520);
        pane.show();
        settle();
    }
    ~Deck() { delete theme; }

    void load() { pane.setCardsFile(QStringLiteral(SP_ROOT) + "/etc/quickstart/cards.txt"); }

    // The first card of the first deck: "Your First Note", play 60.
    CodeCard* firstCard() const
    {
        for (QWidget* w : pane.findChildren<QWidget*>(QStringLiteral("qsCard")))
            if (QLabel* t = w->findChild<QLabel*>(QStringLiteral("qsCardTitle")); t && t->text() == "Your First Note")
                return qobject_cast<CodeCard*>(w);
        return nullptr;
    }
};

QAbstractButton* button(QWidget* card, const QString& accessibleStart)
{
    for (QAbstractButton* b : card->findChildren<QAbstractButton*>())
        if (b->accessibleName().startsWith(accessibleStart)) return b;
    return nullptr;
}

QList<CardScope*> scopes(QWidget* card)
{
    QList<CardScope*> found;   // CardScope has no Q_OBJECT, so not findChild<CardScope*>
    for (QWidget* w : card->findChildren<QWidget*>())
        if (CardScope* s = dynamic_cast<CardScope*>(w)) found << s;
    return found;
}

QString shown(QWidget* card, const char* name)
{
    QLabel* l = card->findChild<QLabel*>(QString::fromLatin1(name));
    return l && l->isVisible() ? l->text() : QString();
}

const QString kWorkspace = QStringLiteral("sonic-pi-quickstart-0-0");

} // namespace

TEST_CASE("a quickstart card: header, code, footer, Play and Stop", "[quickstart][card]")
{
    Deck deck;
    CodeCard* card = deck.firstCard();
    REQUIRE(card);

    // Accent header with the title, a body of highlighted code lines, a footer
    // with the description.
    CHECK(card->findChild<QWidget*>(QStringLiteral("qsCardHeader")));
    QScrollArea* body = card->findChild<QScrollArea*>(QStringLiteral("qsCardBody"));
    REQUIRE(body);
    CHECK(body->accessibleName() == "Code");
    TutProseText* code = card->codeText();
    REQUIRE(code);
    CHECK(body->isAncestorOf(code));
    CHECK(code->plainText() == "play 60");   // the code, not its markup
    CHECK(code->document()->toHtml().contains("color:"));   // highlighted
    // In a deck the card is the one keyboard stop, and its whole face drags:
    // the code takes neither the focus nor the mouse.
    CHECK(code->focusPolicy() == Qt::NoFocus);
    CHECK(code->testAttribute(Qt::WA_TransparentForMouseEvents));
    QLabel* blurb = card->findChild<QLabel*>(QStringLiteral("qsCardBlurb"));
    REQUIRE(blurb);
    CHECK(blurb->accessibleName().startsWith("Use play to make a beep."));
    CHECK(card->findChild<QWidget*>(QStringLiteral("qsCardFooter")));

    // Play and Stop, each with a ring: the left channel round Play, the right
    // round Stop. Stop has nothing to stop until the card plays.
    QAbstractButton* play = button(card, "Play Your First Note");
    QAbstractButton* stop = button(card, "Stop Your First Note");
    REQUIRE(play);
    REQUIRE(stop);
    CHECK(play->isEnabled());
    CHECK_FALSE(stop->isEnabled());
    CHECK(scopes(card).size() == 2);

    // The actions, named for a screen reader after the card. Reset is only
    // there once the code was changed.
    CHECK(button(card, "Edit Your First Note"));
    CHECK(button(card, "Copy Your First Note to the clipboard"));
    CHECK(button(card, "Add Your First Note to the editor"));
    QAbstractButton* reset = button(card, "Reset Your First Note");
    REQUIRE(reset);
    CHECK_FALSE(reset->isVisible());
    // The card itself is the keyboard stop.
    CHECK(card->focusPolicy() == Qt::TabFocus);
}

TEST_CASE("a quickstart card plays, plays again, stops and hands its code over", "[quickstart][card]")
{
    Deck deck;
    CodeCard* card = deck.firstCard();
    REQUIRE(card);
    QSignalSpy runs(&deck.pane, &QuickstartPane::runRequested);
    QSignalSpy stops(&deck.pane, &QuickstartPane::stopJobRequested);
    QSignalSpy inserts(&deck.pane, &QuickstartPane::insertRequested);
    QSignalSpy copies(&deck.pane, &QuickstartPane::copyRequested);
    QAbstractButton* play = button(card, "Play Your First Note");
    QAbstractButton* stop = button(card, "Stop Your First Note");
    REQUIRE(play);
    REQUIRE(stop);

    play->click();
    REQUIRE(runs.count() == 1);
    CHECK(runs[0][0].toString() == "Your First Note");
    CHECK(runs[0][1].toString() == "play 60");
    CHECK(runs[0][2].toString() == kWorkspace);
    CHECK_FALSE(card->isPlaying());   // waiting on the engine

    // Playing: Stop is live and the border lit.
    deck.pane.runStarted(7, kWorkspace);
    settle();
    CHECK(card->isPlaying());
    CHECK(card->property("playing").toBool());
    CHECK(stop->isEnabled());

    // Play again while it plays runs the code again; it doesn't stop it.
    play->click();
    REQUIRE(runs.count() == 2);
    CHECK(stops.isEmpty());
    deck.pane.runStarted(8, kWorkspace);

    // Stop stops every run the card started.
    stop->click();
    REQUIRE(stops.count() == 2);
    CHECK(QSet<int>{ stops[0][0].toInt(), stops[1][0].toInt() } == QSet<int>{ 7, 8 });
    deck.pane.runEnded(7);
    deck.pane.runEnded(8);
    settle();
    CHECK_FALSE(card->isPlaying());
    CHECK_FALSE(stop->isEnabled());

    // Add hands the code over padded for the editor; Copy, as it is.
    button(card, "Add Your First Note")->click();
    REQUIRE(inserts.count() == 1);
    CHECK(inserts[0][1].toString() == "\nplay 60\n\n");
    button(card, "Copy Your First Note")->click();
    REQUIRE(copies.count() == 1);
    CHECK(copies[0][1].toString() == "play 60");
}

TEST_CASE("a quickstart card's code is yours to change, and is kept until Reset", "[quickstart][card]")
{
    Deck deck;
    CodeCard* card = deck.firstCard();
    REQUIRE(card);
    QSignalSpy runs(&deck.pane, &QuickstartPane::runRequested);

    // Edit makes the code an editor in place, holding the code; Edit is lit.
    QAbstractButton* edit = button(card, "Edit Your First Note");
    REQUIRE(edit);
    edit->click();
    settle();
    QPlainTextEdit* editor = card->findChild<QPlainTextEdit*>(QStringLiteral("qsCardEditor"));
    REQUIRE(editor);
    CHECK(editor->isVisible());
    CHECK(editor->toPlainText() == "play 60");
    CHECK(edit->property("armed").toBool());

    // Changed: Play plays the change, and Reset appears.
    editor->moveCursor(QTextCursor::End);
    QTest::keyClicks(editor, ", amp: 2");
    CHECK(card->code() == "play 60, amp: 2");
    CHECK(card->isEdited());
    QAbstractButton* reset = button(card, "Reset Your First Note");
    REQUIRE(reset);
    CHECK(reset->isVisible());
    button(card, "Play Your First Note")->click();
    REQUIRE(runs.count() == 1);
    CHECK(runs[0][1].toString() == "play 60, amp: 2");

    // Escape reads the code again, as changed.
    QTest::keyClick(editor, Qt::Key_Escape);
    settle();
    CHECK_FALSE(card->isEditing());
    CHECK_FALSE(edit->property("armed").toBool());
    REQUIRE(card->codeText());
    CHECK(card->codeText()->isVisible());
    CHECK(card->codeText()->plainText() == "play 60, amp: 2");

    // Kept: a rebuilt deck (a theme change, a zoom, another launch) still has it.
    deck.load();
    settle();
    card = deck.firstCard();
    REQUIRE(card);
    CHECK(card->code() == "play 60, amp: 2");
    reset = button(card, "Reset Your First Note");
    REQUIRE(reset);
    CHECK(reset->isVisible());

    // Reset puts it back as written, and keeps nothing.
    reset->click();
    settle();
    CHECK(card->code() == "play 60");
    CHECK_FALSE(reset->isVisible());
    deck.load();
    settle();
    CHECK(deck.firstCard()->code() == "play 60");
}

TEST_CASE("a quickstart card shows what its program puts, and its error", "[quickstart][card]")
{
    Deck deck;
    CodeCard* card = deck.firstCard();
    REQUIRE(card);

    button(card, "Play Your First Note")->click();
    deck.pane.runStarted(7, kWorkspace);
    settle();

    // Another run's output is not this card's.
    deck.pane.runOutput(99, "not mine");
    CHECK(shown(card, "qsCardOutput").isEmpty());

    // The last few lines show, the newest last.
    for (int i = 1; i <= CodeCard::kOutputLines + 1; i++)
        deck.pane.runOutput(7, QString("line %1").arg(i));
    settle();
    const QString out = shown(card, "qsCardOutput");
    CHECK_FALSE(out.contains("line 1<"));
    CHECK(out.contains("line 2"));
    CHECK(out.contains(QString("line %1").arg(CodeCard::kOutputLines + 1)));

    // The error names the card's own line: the run's first is the with_fx
    // :scope_out the code is wrapped in.
    deck.pane.runError(7, "Unknown function `plya`", 2);
    settle();
    CHECK(shown(card, "qsCardError") == "Unknown function `plya` (line 1)");
    CHECK(card->property("errored").toBool());

    // A new run from rest clears both.
    deck.pane.runEnded(7);
    button(card, "Play Your First Note")->click();
    settle();
    CHECK(shown(card, "qsCardError").isEmpty());
    CHECK(shown(card, "qsCardOutput").isEmpty());
}

TEST_CASE("a quickstart card lights under the pointer", "[quickstart][card]")
{
    Deck deck;
    CodeCard* card = deck.firstCard();
    REQUIRE(card);
    CHECK_FALSE(card->property("cardHover").toBool());
    card->hoverAt(card->mapToGlobal(card->rect().center()), true);
    CHECK(card->property("cardHover").toBool());
    card->hoverAt(QPoint(-10000, -10000), true);
    CHECK_FALSE(card->property("cardHover").toBool());
}

TEST_CASE("a quickstart card lights the line that sounded", "[quickstart][card]")
{
    Deck deck;
    CodeCard* card = deck.firstCard();
    REQUIRE(card);
    TutProseText* code = card->codeText();
    REQUIRE(code);
    CHECK(code->washedLine() == -1);
    // A run's lines count from 1, and the first is the with_fx :scope_out
    // the card's code is wrapped in: its own code starts at line 2.
    deck.pane.flashLine(kWorkspace, 1);
    CHECK(code->washedLine() == -1);
    deck.pane.flashLine(kWorkspace, 2);
    CHECK(code->washedLine() == 0);
    // It clears, as the editor's does.
    CHECK(QTest::qWaitFor([code] { return code->washedLine() == -1; }, 2000));
}

