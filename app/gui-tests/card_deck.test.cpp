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

// A deck: the runs a host's code cards start, as the web's deck.js keeps
// them. Each card's Play and Stop are the deck's; the session's run events
// come back to the card whose run they are.

#include <catch2/catch_test_macros.hpp>

#include <QAbstractButton>
#include <QApplication>
#include <QLabel>
#include <QPushButton>
#include <QSignalSpy>
#include <QTest>
#include <QVBoxLayout>

#include <memory>

#include "model/sonicpitheme.h"
#include "widgets/carddeck.h"
#include "widgets/codecard.h"
#include "widgets/tutorialwidgets.h"

namespace {

struct Host
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    QWidget page;
    QVBoxLayout* column = new QVBoxLayout(&page);

    Host() { page.resize(800, 900); }
    ~Host() { delete theme; }

    CodeCard* card(const QString& title, const QString& code, unsigned int scopeSlot = 1)
    {
        CodeCard::Spec spec;
        spec.title = title;
        spec.code = code;
        spec.scopeSlot = scopeSlot;
        CodeCard::Metrics m;
        m.codeFontPx = 14;
        m.titlePx = 16;
        CodeCard* c = new CodeCard(spec, m, theme, &page);
        column->addWidget(c);
        return c;
    }
};

void play(CodeCard* c) { c->playButton()->click(); }
void stop(CodeCard* c) { c->stopButton()->click(); }

QSet<int> stopped(const QSignalSpy& spy)
{
    QSet<int> jobs;
    for (const QList<QVariant>& call : spy)
        jobs.insert(call[0].toInt());
    return jobs;
}

QString shown(QWidget* card, const char* name)
{
    QLabel* l = card->findChild<QLabel*>(QString::fromLatin1(name));
    return l && !l->isHidden() ? l->text() : QString();
}

} // namespace

TEST_CASE("a deck runs a card's code and stops its runs", "[carddeck]")
{
    Host host;
    CardDeck deck(&host.page, CardDeck::Playing::Layered);
    CodeCard* a = host.card("A", "play 60", 3);
    deck.add(a, "ws-a");
    QSignalSpy runs(&deck, &CardDeck::runRequested);
    QSignalSpy stops(&deck, &CardDeck::stopJobRequested);

    play(a);
    REQUIRE(runs.count() == 1);
    CHECK(runs[0][0].toString() == "A");
    CHECK(runs[0][1].toString() == "play 60");
    CHECK(runs[0][2].toString() == "ws-a");
    CHECK(runs[0][3].toInt() == 3);
    CHECK_FALSE(a->isPlaying());

    deck.runStarted(7, "ws-a");
    CHECK(a->isPlaying());
    deck.runStarted(99, "somebody-else");   // not the deck's
    stop(a);
    CHECK(stopped(stops) == QSet<int>{ 7 });
    deck.runEnded(7);
    CHECK_FALSE(a->isPlaying());
}

TEST_CASE("a layered deck plays its cards together", "[carddeck]")
{
    Host host;
    CardDeck deck(&host.page, CardDeck::Playing::Layered);
    CodeCard* drums = host.card("Drums", "live_loop :drums do\n  sample :bd_haus\n  sleep 1\nend");
    CodeCard* bass = host.card("Bass", "live_loop :bass do\n  play :e1\n  sleep 1\nend");
    deck.add(drums, "ws-d");
    deck.add(bass, "ws-b");
    QSignalSpy stops(&deck, &CardDeck::stopJobRequested);

    play(drums);
    deck.runStarted(7, "ws-d");
    play(bass);
    deck.runStarted(8, "ws-b");
    CHECK(drums->isPlaying());
    CHECK(bass->isPlaying());
    CHECK(stops.isEmpty());

    stop(bass);
    CHECK(stopped(stops) == QSet<int>{ 8 });
}

TEST_CASE("a card that takes over another's live_loops takes its runs too", "[carddeck]")
{
    Host host;
    CardDeck deck(&host.page, CardDeck::Playing::Layered);
    CodeCard* first = host.card("Beat", "live_loop :beat do\n  sample :bd_haus\n  sleep 1\nend");
    CodeCard* second = host.card("Beat, faster", "live_loop :beat do\n  sample :bd_haus\n  sleep 0.5\nend");
    deck.add(first, "ws-1");
    deck.add(second, "ws-2");
    QSignalSpy stops(&deck, &CardDeck::stopJobRequested);

    play(first);
    deck.runStarted(7, "ws-1");
    play(second);
    deck.runStarted(8, "ws-2");
    // live_loop redefines the running thread, which stays the first run's:
    // the second card plays it now, so its Stop is the one that ends it.
    CHECK_FALSE(first->isPlaying());
    CHECK(second->isPlaying());
    deck.runEnded(8);   // the redefinition lands and its own run ends
    CHECK(second->isPlaying());
    stop(second);
    CHECK(stopped(stops) == QSet<int>{ 7 });
}

TEST_CASE("a deck that plays one card at a time stops the last for the next", "[carddeck]")
{
    Host host;
    CardDeck deck(&host.page, CardDeck::Playing::OneAtATime);
    CodeCard* a = host.card("A", "live_loop :a do\n  play 60\n  sleep 1\nend");
    CodeCard* b = host.card("B", "live_loop :b do\n  play 64\n  sleep 1\nend");
    deck.add(a, "ws-a");
    deck.add(b, "ws-b");
    QSignalSpy stops(&deck, &CardDeck::stopJobRequested);

    play(a);
    deck.runStarted(7, "ws-a");
    play(b);
    CHECK(stopped(stops) == QSet<int>{ 7 });
    CHECK_FALSE(a->isPlaying());

    // Play again on the card that plays is a run again, not a stop.
    deck.runStarted(8, "ws-b");
    stops.clear();
    play(b);
    CHECK(stops.isEmpty());
}

TEST_CASE("a card made again while its runs play is still playing", "[carddeck]")
{
    Host host;
    CardDeck deck(&host.page, CardDeck::Playing::Layered);
    CodeCard* a = host.card("A", "play 60");
    deck.add(a, "ws-a");
    play(a);
    deck.runStarted(7, "ws-a");

    // A rebuild (a zoom, a theme) makes the cards again.
    deck.clear();
    delete a;
    CodeCard* again = host.card("A", "play 60");
    deck.add(again, "ws-a");
    CHECK(again->isPlaying());
    QSignalSpy stops(&deck, &CardDeck::stopJobRequested);
    stop(again);
    CHECK(stopped(stops) == QSet<int>{ 7 });
}

TEST_CASE("a run that starts after its card went is still the deck's", "[carddeck]")
{
    Host host;
    CardDeck deck(&host.page, CardDeck::Playing::Layered);
    CodeCard* a = host.card("A", "play 60");
    deck.add(a, "ws-a");
    play(a);
    // The host moves on (another deck) before the engine starts the run.
    deck.clear();
    delete a;
    deck.runStarted(7, "ws-a");
    // Back again: the card made again plays it, and stops it.
    CodeCard* again = host.card("A", "play 60");
    deck.add(again, "ws-a");
    CHECK(again->isPlaying());
    QSignalSpy stops(&deck, &CardDeck::stopJobRequested);
    stop(again);
    CHECK(stopped(stops) == QSet<int>{ 7 });
}

TEST_CASE("a deck shows a run's output, error and sounding line on its card", "[carddeck]")
{
    Host host;
    host.page.show();
    CardDeck deck(&host.page, CardDeck::Playing::Layered);
    CodeCard* a = host.card("A", "puts :hi\nplay 60");
    CodeCard* b = host.card("B", "play 72");
    deck.add(a, "ws-a");
    deck.add(b, "ws-b");
    play(a);
    deck.runStarted(7, "ws-a");

    deck.runOutput(7, "hi");
    deck.runOutput(99, "not the deck's");
    CHECK(shown(a, "qsCardOutput").contains("hi"));
    CHECK(shown(b, "qsCardOutput").isEmpty());

    // A run's lines count the with_fx :scope_out the code is wrapped in.
    deck.flashLine("ws-a", 3);
    CHECK(a->codeText()->washedLine() == 1);
    deck.runError(7, "Unknown function `plya`", 3);
    CHECK(shown(a, "qsCardError") == "Unknown function `plya` (line 2)");
}
