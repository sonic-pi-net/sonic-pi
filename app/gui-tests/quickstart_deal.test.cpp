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

// The QuickStart decks are dealt as the web's are (app/web/app/src/quickstart.js): a deck opens on its welcome card,
// Start deals its first card, the tile after the newest card deals the next, and the end of a deck deals the next
// deck. The rules (model/quickstartprogress.h), then the pane that shows them.

#include <catch2/catch_test_macros.hpp>

#include <QAbstractButton>
#include <QAccessible>
#include <QApplication>
#include <QLabel>
#include <QSignalSpy>
#include <QTest>
#include <QTextDocumentFragment>

#include "model/quickstartprogress.h"
#include "model/sonicpitheme.h"
#include "own_settings.h"
#include "utils/fontroles.h"
#include "widgets/quickstartpane.h"
#include "widgets/tutorialwidgets.h"

using SonicPi::QuickstartProgress;
using Tile = QuickstartProgress::Tile;
using Start = QuickstartProgress::Start;
using Moved = QuickstartProgress::Moved;

namespace {

// Basics and Synths, as etc/quickstart/cards.txt has them
QuickstartProgress twoDecks()
{
    QuickstartProgress p;
    p.setDecks({ 10, 5 });
    return p;
}

void dealAll(QuickstartProgress& p, int perPage)
{
    p.startDeck(perPage);
    while (p.dealt() < p.cards())
        p.deal(perPage);
}

} // namespace

TEST_CASE("a deck opens on its welcome card alone, with Start", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    CHECK(!p.inDeck());
    CHECK(p.start() == Start::Start);
    CHECK(p.pages(3) == 1);
    const QuickstartProgress::Page s = p.shown(3);
    CHECK(s.welcome);
    CHECK(s.from == s.to);
    CHECK(s.tile == Tile::None);
    CHECK(!p.decksShown());   // one deck reached: nothing to choose between
}

TEST_CASE("Start deals the first card, the next card's tile beside it", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    p.startDeck(3);
    CHECK(p.inDeck());
    CHECK(p.dealt() == 1);
    const QuickstartProgress::Page s = p.shown(3);
    CHECK(s.welcome);
    CHECK(s.from == 0);
    CHECK(s.to == 1);
    CHECK(s.tile == Tile::NextCard);
    CHECK(p.start() == Start::CarryOn);
}

TEST_CASE("a card dealt into a full page moves the view along by one", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    p.startDeck(3);
    p.deal(3);
    p.deal(3);
    CHECK(p.dealt() == 3);
    CHECK(p.pages(3) == 2);
    CHECK(p.page() == 1);
    const QuickstartProgress::Page last = p.shown(3);
    CHECK(!last.welcome);
    CHECK(last.from == 1);
    CHECK(last.to == 3);
    CHECK(last.tile == Tile::NextCard);

    CHECK(p.turn(-1, 3));
    const QuickstartProgress::Page before = p.shown(3);
    CHECK(before.welcome);
    CHECK(before.from == 0);
    CHECK(before.to == 2);
    CHECK(before.tile == Tile::None);   // the tile ends the last page only
}

TEST_CASE("a finished deck offers the next deck, which joins the decks to choose from", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    dealAll(p, 3);
    CHECK(p.dealt() == 10);
    CHECK(p.shown(3).tile == Tile::NextDeck);
    CHECK(p.start() == Start::Again);
    CHECK(p.reached() == 0);

    p.nextDeck();
    CHECK(p.deck() == 1);
    CHECK(p.reached() == 1);
    CHECK(!p.inDeck());
    CHECK(p.decksShown());
    CHECK(p.deckShown(1));
    CHECK(p.shown(3).welcome);
    CHECK(p.start() == Start::Start);
}

TEST_CASE("the last deck finished has nothing after it", "[quickstart][deal]")
{
    QuickstartProgress p;
    p.setDecks({ 2 });
    dealAll(p, 3);
    CHECK(p.shown(3).tile == Tile::None);
    CHECK(!p.canForward(3));
    CHECK(p.forward(3) == Moved::None);
}

TEST_CASE("Carry on goes to the newest card, Play through again to the first", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    p.startDeck(3);
    for (int i = 0; i < 3; ++i)
        p.deal(3);
    p.leave();
    CHECK(!p.inDeck());
    CHECK(p.shown(3).welcome);
    CHECK(p.start() == Start::CarryOn);

    p.startDeck(3);
    CHECK(p.inDeck());
    CHECK(p.dealt() == 4);   // nothing dealt: it goes to the newest
    const QuickstartProgress::Page newest = p.shown(3);
    CHECK(newest.to == 4);
    CHECK(newest.tile == Tile::NextCard);

    while (p.dealt() < p.cards())
        p.deal(3);
    p.leave();
    CHECK(p.start() == Start::Again);
    p.startDeck(3);
    const QuickstartProgress::Page firstCard = p.shown(3);
    CHECK(firstCard.welcome);
    CHECK(firstCard.from == 0);
    CHECK(firstCard.to == 2);
}

TEST_CASE("Next turns the page, deals at the end, then moves on to the next deck", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    CHECK(p.forward(3) == Moved::Dealt);   // a fresh deck: Next deals, as Start does
    CHECK(p.dealt() == 1);
    p.deal(3);
    p.deal(3);
    p.turn(-1, 3);
    CHECK(p.forward(3) == Moved::Page);
    CHECK(p.page() == 1);
    CHECK(p.dealt() == 3);
    CHECK(p.forward(3) == Moved::Dealt);
    CHECK(p.dealt() == 4);
    while (p.dealt() < p.cards())
        p.deal(3);
    CHECK(p.forward(3) == Moved::NewDeck);
    CHECK(p.deck() == 1);
}

TEST_CASE("Back stops at the first page", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    CHECK(!p.canBack());
    CHECK(!p.turn(-1, 3));
    p.startDeck(3);
    for (int i = 0; i < 4; ++i)
        p.deal(3);
    CHECK(p.canBack());
    while (p.turn(-1, 3)) {}
    CHECK(p.page() == 0);
    CHECK(!p.canBack());
}

TEST_CASE("a page chosen by its dot goes into the deck", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    p.startDeck(3);
    for (int i = 0; i < 4; ++i)
        p.deal(3);
    p.leave();
    CHECK(p.goTo(1, 3));
    CHECK(p.inDeck());
    CHECK(p.page() == 1);
}

TEST_CASE("a deck chosen opens on its welcome card", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    p.restore(0, { 10, 2 }, 1);
    p.startDeck(3);
    p.leave();
    p.choose(1);
    CHECK(p.deck() == 1);
    CHECK(!p.inDeck());
    CHECK(p.page() == 0);
    CHECK(p.start() == Start::CarryOn);
}

TEST_CASE("a row a card wide has no tile: Next deals from the newest card", "[quickstart][deal]")
{
    QuickstartProgress p = twoDecks();
    p.startDeck(1);
    CHECK(p.pages(1) == 2);
    const QuickstartProgress::Page s = p.shown(1);
    CHECK(!s.welcome);
    CHECK(s.from == 0);
    CHECK(s.to == 1);
    CHECK(s.tile == Tile::None);
    CHECK(p.forward(1) == Moved::Dealt);
    CHECK(p.shown(1).from == 1);
}

TEST_CASE("how far each deck is dealt and the decks reached are kept, within the set", "[quickstart][deal]")
{
    QuickstartProgress p;
    p.setDecks({ 10, 5, 4 });
    p.restore(1, { 3, 2, 0 }, 0);   // a deck under way counts as reached
    CHECK(p.deck() == 1);
    CHECK(p.reached() == 1);
    CHECK(p.dealt(0) == 3);
    CHECK(p.dealtAll() == QVector<int>{ 3, 2, 0 });

    p.restore(7, { 30, -1 }, 9);   // kept for a bigger set than this one
    CHECK(p.deck() == 2);
    CHECK(p.dealt(0) == 10);
    CHECK(p.dealt(1) == 0);
    CHECK(p.reached() == 2);
    CHECK(p.dealtAll() == QVector<int>{ 10, 0, 0 });
}

// ── the pane ──────────────────────────────────────────────────────────────────────────────────────────────────────

namespace {

void settle()
{
    for (int i = 0; i < 5; i++)
    {
        QApplication::processEvents();
        QTest::qWait(10);
    }
}

struct Pane
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    QuickstartPane* pane = new QuickstartPane(theme);

    Pane()
    {
        QApplication::setFont(RoleFont(FontRole::Base));
        qApp->setStyleSheet(theme->getAppStylesheet());
        pane->setCardsFile(QStringLiteral(SP_ROOT) + "/etc/quickstart/cards.txt");
        pane->resize(1400, 520);
        pane->show();
        settle();
    }
    ~Pane()
    {
        delete pane;
        delete theme;
    }

    QWidget* welcome() const { return shown<QWidget>("qsWelcome"); }
    QAbstractButton* start() const { return shown<QAbstractButton>("qsStart"); }
    QAbstractButton* tile() const { return shown<QAbstractButton>("qsNextTile"); }
    QAbstractButton* leave() const { return shown<QAbstractButton>("qsExit"); }

    template <typename T> T* shown(const char* name) const
    {
        for (T* w : pane->findChildren<T*>(QString::fromLatin1(name)))
            if (w->isVisible())
                return w;
        return nullptr;
    }
    QString text(const char* name) const
    {
        QLabel* l = shown<QLabel>(name);
        return l ? l->text() : QString();
    }
    QStringList cards() const
    {
        QStringList titles;
        for (QWidget* c : pane->findChildren<QWidget*>(QStringLiteral("qsCard")))
            if (c->isVisible())
                if (QLabel* t = c->findChild<QLabel*>(QStringLiteral("qsCardTitle")))
                    titles << t->text();
        return titles;
    }
    QStringList decks() const
    {
        QStringList names;
        for (QAbstractButton* b : pane->findChildren<QAbstractButton*>(QStringLiteral("qsDeckPill")))
            if (b->isVisible())
                names << b->text();
        return names;
    }
    void click(QAbstractButton* b)
    {
        REQUIRE(b);
        b->click();
        settle();
    }
};

} // namespace

TEST_CASE("the pane opens on the deck's welcome card: its name, its first sentence, then the rest", "[quickstart][deal]")
{
    OwnSettings settings;
    Pane p;
    REQUIRE(p.welcome());
    CHECK(p.text("qsWelcomeDeck") == "Basics");
    CHECK(p.text("qsWelcomeLead") == "Welcome friend :-)");
    CHECK(QTextDocumentFragment::fromHtml(p.text("qsWelcomeBody")).toPlainText().startsWith("This is a deck of QuickStart cards."));
    CHECK(p.text("qsStartSay") == "Start deck");
    CHECK(p.text("qsStartSub") == "10 cards");
    CHECK(p.cards().isEmpty());
    CHECK(!p.tile());
    CHECK(!p.leave());
    CHECK(p.decks().isEmpty());   // one deck reached: nothing to choose between
}

TEST_CASE("Start deals the first card, with focus on it and the next card's tile beside it", "[quickstart][deal]")
{
    OwnSettings settings;
    Pane p;
    REQUIRE(p.start());
    p.start()->setFocus();
    p.click(p.start());
    CHECK(p.cards() == QStringList{ "Your First Note" });
    REQUIRE(p.tile());
    CHECK(p.text("qsTileTitle") == "Change Synth");
    CHECK(p.text("qsTileCount") == "2 of 10");
    QWidget* focused = p.pane->window()->focusWidget();
    REQUIRE(focused);
    CHECK(focused->objectName() == "qsCard");
    REQUIRE(p.leave());
    CHECK(p.text("qsBarTitle") == "Basics");
}

TEST_CASE("the tile deals the next card, and Decks goes back to the welcome card", "[quickstart][deal]")
{
    OwnSettings settings;
    Pane p;
    p.click(p.start());
    p.click(p.tile());
    CHECK(p.cards() == QStringList{ "Your First Note", "Change Synth" });
    p.click(p.leave());
    CHECK(p.welcome());
    CHECK(p.cards().isEmpty());
    CHECK(p.text("qsStartSay") == "Carry on");
    CHECK(p.text("qsStartSub") == "2 of 10 cards dealt");
}

TEST_CASE("the decks reached are the ones to choose between", "[quickstart][deal]")
{
    OwnSettings settings;
    Pane p;
    p.click(p.start());
    for (int i = 1; i < 10; ++i)
        p.click(p.tile());
    REQUIRE(p.tile());
    CHECK(p.text("qsTileLabel") == "Next deck");
    CHECK(p.text("qsTileTitle") == "Synths");
    p.click(p.tile());
    CHECK(p.text("qsWelcomeDeck") == "Synths");
    CHECK(p.decks() == QStringList{ "Basics", "Synths" });
}

TEST_CASE("how far the decks are dealt is kept for next time", "[quickstart][deal]")
{
    OwnSettings settings;
    {
        Pane p;
        p.click(p.start());
        p.click(p.tile());
    }
    Pane again;
    CHECK(again.text("qsStartSay") == "Carry on");
    CHECK(again.text("qsStartSub") == "2 of 10 cards dealt");
}

// ── what a screen reader meets ───────────────────────────────────────────────────────────────────────────────────

namespace {

QAccessibleInterface* a11y(QWidget* w)
{
    return w ? QAccessible::queryAccessibleInterface(w) : nullptr;
}

QString spoken(QWidget* w)
{
    QAccessibleInterface* i = a11y(w);
    return i ? i->text(QAccessible::Name) : QString();
}

// What a screen reader is told beside the focus: the pane's announcements
QStringList said(const QSignalSpy& spy)
{
    QStringList out;
    for (const QList<QVariant>& args : spy)
        out << args.at(0).toString();
    return out;
}

} // namespace

TEST_CASE("the welcome card reads as a group named for its deck, the deck's name its heading", "[quickstart][deal][a11y]")
{
    OwnSettings settings;
    Pane p;
    QAccessibleInterface* card = a11y(p.welcome());
    REQUIRE(card);
    CHECK(card->role() == QAccessible::Grouping);
    CHECK(card->text(QAccessible::Name) == "Basics: the deck's introduction");
    TutHeading* heading = qobject_cast<TutHeading*>(p.shown<QLabel>("qsWelcomeDeck"));
    REQUIRE(heading);
    CHECK(a11y(heading)->role() == QAccessible::Heading);
    CHECK(spoken(heading) == "Basics");   // drawn in capitals, read as the word
}

TEST_CASE("Start, the tile and Decks each say what they do, once", "[quickstart][deal][a11y]")
{
    OwnSettings settings;
    Pane p;
    REQUIRE(p.start());
    CHECK(spoken(p.start()) == "Start deck, 10 cards");
    CHECK(a11y(p.start())->text(QAccessible::Description) == "Deal the first card: Your First Note");
    // the words drawn inside are the name already: not read again
    for (QLabel* l : p.start()->findChildren<QLabel*>())
        CHECK(a11y(l)->state().invisible);

    p.click(p.start());
    REQUIRE(p.tile());
    CHECK(spoken(p.tile()) == "Next card: Change Synth, 2 of 10");
    CHECK(a11y(p.tile())->text(QAccessible::Description) == "Show the next card: Change Synth");
    for (QLabel* l : p.tile()->findChildren<QLabel*>())
        CHECK(a11y(l)->state().invisible);
    REQUIRE(p.leave());
    CHECK(spoken(p.leave()) == "Back to the decks");
}

TEST_CASE("Return and Space work Start and the tile, as they do every button", "[quickstart][deal][a11y]")
{
    OwnSettings settings;
    Pane p;
    REQUIRE(p.start());
    QTest::keyClick(p.start(), Qt::Key_Return);
    settle();
    CHECK(p.cards() == QStringList{ "Your First Note" });
    REQUIRE(p.tile());
    QTest::keyClick(p.tile(), Qt::Key_Space);
    settle();
    CHECK(p.cards() == QStringList{ "Your First Note", "Change Synth" });
    REQUIRE(p.tile());
    QTest::keyClick(p.tile(), Qt::Key_Enter);
    settle();
    CHECK(p.cards().contains("Lower and Longer Synth"));
}

TEST_CASE("focus stays in the pane: on the card dealt, then on Start back at the decks and in a new deck", "[quickstart][deal][a11y]")
{
    OwnSettings settings;
    Pane p;
    const auto focused = [&] { return p.pane->window()->focusWidget(); };
    REQUIRE(p.start());
    p.start()->setFocus();
    p.click(p.start());
    REQUIRE(focused());
    CHECK(focused()->objectName() == "qsCard");
    CHECK(spoken(focused()).startsWith("Your First Note card, 1 of 10."));

    REQUIRE(p.tile());

    p.tile()->setFocus();
    p.click(p.tile());
    REQUIRE(focused());
    CHECK(spoken(focused()).startsWith("Change Synth card, 2 of 10."));

    REQUIRE(p.leave());

    p.leave()->setFocus();
    p.click(p.leave());
    CHECK(focused() == p.start());

    p.click(p.start());
    while (p.text("qsTileLabel") != "Next deck")
    {
        REQUIRE(p.tile());
        REQUIRE(p.tile());
        p.tile()->setFocus();
        p.click(p.tile());
    }
    REQUIRE(p.tile());
    p.tile()->setFocus();
    p.click(p.tile());
    CHECK(focused() == p.start());
}

TEST_CASE("a screen reader is told where it is whenever the focus does not say it", "[quickstart][deal][a11y]")
{
    OwnSettings settings;
    Pane p;
    QSignalSpy spy(p.pane, &QuickstartPane::announceRequested);

    // a card dealt takes the focus, and its name says which card it is: nothing more is said
    REQUIRE(p.start());
    p.start()->setFocus();
    p.click(p.start());
    INFO(said(spy).join(" | ").toStdString());
    CHECK(said(spy).isEmpty());

    REQUIRE(p.leave());

    p.leave()->setFocus();
    p.click(p.leave());
    CHECK(said(spy) == QStringList{ "Decks. Basics: the deck's introduction" });
    spy.clear();

    p.click(p.start());
    while (p.text("qsTileLabel") != "Next deck")
    {
        REQUIRE(p.tile());
        p.tile()->setFocus();
        p.click(p.tile());
    }
    spy.clear();
    REQUIRE(p.tile());
    p.tile()->setFocus();
    p.click(p.tile());
    CHECK(said(spy) == QStringList{ "New deck: Synths, 5 cards" });
}

TEST_CASE("the arrows and the page dots name what they do and say which page is showing", "[quickstart][deal][a11y]")
{
    OwnSettings settings;
    Pane p;
    p.click(p.start());
    for (int i = 0; i < 3; ++i)
        p.click(p.tile());   // four dealt: the welcome card and the cards over two pages
    REQUIRE(p.shown<QAbstractButton>("qsPrev"));
    REQUIRE(p.shown<QAbstractButton>("qsNext"));
    CHECK(spoken(p.shown<QAbstractButton>("qsPrev")) == "Previous cards");
    CHECK(spoken(p.shown<QAbstractButton>("qsNext")) == "Next cards");
    QList<QAbstractButton*> dots;
    for (QAbstractButton* d : p.pane->findChildren<QAbstractButton*>(QStringLiteral("qsDot")))
        if (d->isVisible())
            dots << d;
    REQUIRE(dots.size() == 2);
    CHECK(spoken(dots[0]) == "Page 1 of 2");
    CHECK(spoken(dots[1]) == "Page 2 of 2");
    CHECK(!a11y(dots[0])->state().checked);
    CHECK(a11y(dots[1])->state().checked);

    QSignalSpy spy(p.pane, &QuickstartPane::announceRequested);
    p.click(p.shown<QAbstractButton>("qsPrev"));
    CHECK(said(spy) == QStringList{ "Basics: the deck's introduction" });
    CHECK(a11y(dots[0])->state().checked);
    REQUIRE(p.shown<QAbstractButton>("qsPrev"));
    CHECK(!p.shown<QAbstractButton>("qsPrev")->isEnabled());
    spy.clear();
    REQUIRE(p.start());
    QTest::keyClick(p.start(), Qt::Key_Left);   // already at the first page: it says so
    settle();
    CHECK(said(spy) == QStringList{ "First card." });
}
