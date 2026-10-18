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

// A run's flashes, as every native renderer shows them: the editor's and a
// card's alike light every line that sounded, each held from its own latest
// flash, behind just its code, as bright as Flash Brightness says. A card lit
// one line at a time, so a loop's header, its end and its sound, flashing
// together, showed only the last (#3587). With Reduce animations on, none
// flashes, as on the web.

#include <catch2/catch_test_macros.hpp>

#include <algorithm>
#include <utility>

#include <QAbstractButton>
#include <QApplication>
#include <QImage>
#include <QPlainTextEdit>
#include <QScrollArea>
#include <QScrollBar>
#include <QTest>
#include <QVBoxLayout>

#include "pacer_by_hand.h"
#include "reduced_motion_on.h"
#include "model/sonicpitheme.h"
#include "utils/flash_style.h"
#include "utils/lineflashes.h"
#include "widgets/carddeck.h"
#include "widgets/codecard.h"
#include "widgets/tutorialwidgets.h"

namespace {

// A clock the test moves.
struct FakeClock
{
    qint64 ms = 1000;
    SonicPi::LineFlashes::Clock clock()
    {
        return [this] { return ms; };
    }
};

const QString kLoop = QStringLiteral("live_loop :a do\n  play 60  \n  sleep 1\nend");   // a space or two after a line's code, as typing leaves

// Code a card shows only part of at once, in a short budget.
QString longCode()
{
    QStringList lines;
    for (int i = 0; i < 20; ++i)
        lines << QStringLiteral("play %1").arg(60 + i);
    return lines.join('\n');
}

// A deck of one card, shown, its flashes timed by the test's clock. Its
// code is kLoop, as tall as it needs, unless the test says otherwise.
struct Card
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    QWidget page;
    QVBoxLayout* column = new QVBoxLayout(&page);
    CardDeck deck{ &page, CardDeck::Playing::Layered };
    FakeClock t;
    QString code;
    int budget;   // the code's height, 0 for as tall as it is
    CodeCard* card = nullptr;

    explicit Card(QString code = kLoop, int budget = 0) : code(std::move(code)), budget(budget)
    {
        page.resize(800, 600);
        card = make();
        page.show();
        REQUIRE(QTest::qWaitForWindowExposed(&page));
    }
    ~Card() { delete theme; }

    CodeCard* make()
    {
        CodeCard::Spec spec;
        spec.title = QStringLiteral("Loop");
        spec.code = code;
        CodeCard::Metrics m;
        m.codeBodyHeight = budget;
        m.codeFontPx = 14;
        m.titlePx = 16;
        CodeCard* c = new CodeCard(spec, m, theme, &page);
        column->addWidget(c);
        deck.add(c, QStringLiteral("ws"));
        c->lineFlashes()->setClock(t.clock());
        return c;
    }

    TutProseText* text() const { return card->codeText(); }

    QImage shot() const { return text()->grab().toImage(); }

    // The pixel at the middle of character `pos` of the code (a position in
    // its plain text), in the code's own grab.
    static QColor at(const QImage& img, TutProseText* text, int pos)
    {
        const QPoint p = text->mapFromGlobal(text->characterRectGlobal(pos).center());
        return img.pixelColor(qRound(p.x() * img.devicePixelRatio()), qRound(p.y() * img.devicePixelRatio()));
    }

    // The code as the card shows it, a line at a time: what the positions
    // below count in.
    QStringList shown() const { return text()->plainText().split('\n'); }

    // The position where `line` starts.
    int start(int line) const
    {
        const QStringList lines = shown();
        int pos = 0;
        for (int i = 0; i < line; ++i)
            pos += lines[i].size() + 1;
        return pos;
    }

    // The position of the gap after the first word of `line`: inside the
    // line's code, and on no glyph's ink.
    int gap(int line) const
    {
        const QString s = shown()[line];
        int i = 0;
        while (s[i].isSpace())
            ++i;
        return start(line) + s.indexOf(' ', i);
    }

    // The position of the last character of `line`'s code.
    int last(int line) const { return start(line) + shown()[line].trimmed().size() + SonicPi::LineFlashes::codeSpan(shown()[line]).first - 1; }

    // Whether any of the pixels along the middle of `line` differ from the
    // code as it read unlit: lit, on its ink or round it.
    bool lit(const QImage& img, const QImage& unlit, int line) const
    {
        const QPoint mid = text()->mapFromGlobal(text()->characterRectGlobal(start(line)).center());
        const int y = qRound(mid.y() * img.devicePixelRatio());
        for (int x = 0; x < img.width(); ++x)
            if (img.pixelColor(x, y) != unlit.pixelColor(x, y))
                return true;
        return false;
    }

    // The position of the line's indent, before its code: -1 when it has none.
    int indent(int line) const { return shown()[line].at(0).isSpace() ? start(line) : -1; }
};

// One colour laid over another, as a wash lies over the code's background.
QColor over(const QColor& under, const QColor& wash)
{
    const double aw = wash.alphaF(), au = under.alphaF();
    const double a = aw + au * (1 - aw);
    if (a <= 0)
        return QColor(0, 0, 0, 0);
    auto mix = [&](double w, double u) { return qRound((w * aw + u * au * (1 - aw)) / a * 255); };
    return QColor(mix(wash.redF(), under.redF()), mix(wash.greenF(), under.greenF()),
                  mix(wash.blueF(), under.blueF()), qRound(a * 255));
}

bool near(const QColor& a, const QColor& b)
{
    return qAbs(a.red() - b.red()) <= 3 && qAbs(a.green() - b.green()) <= 3 && qAbs(a.blue() - b.blue()) <= 3
           && qAbs(a.alpha() - b.alpha()) <= 3;
}

} // namespace

TEST_CASE("a line's code is its first character to its last, as every renderer washes it", "[flash]")
{
    using SonicPi::LineFlashes;
    CHECK(LineFlashes::codeSpan("play 60") == std::pair{ 0, 7 });
    CHECK(LineFlashes::codeSpan("  play 60  ") == std::pair{ 2, 9 });
    CHECK(LineFlashes::codeSpan("\tsleep 1\t") == std::pair{ 1, 8 });
    const auto [from, to] = LineFlashes::codeSpan("    ");
    CHECK(from == to);   // none
}

TEST_CASE("flashes light every line flashed, on the tick", "[flash]")
{
    HandDriven hand;
    QObject owner;
    FakeClock t;
    SonicPi::LineFlashes flashes(&owner);
    flashes.setClock(t.clock());
    QList<int> cleared;
    QObject::connect(&flashes, &SonicPi::LineFlashes::changed, &owner, [&](int line, bool lit) {
        if (!lit)
            cleared << line;
    });

    flashes.flash(0);
    flashes.flash(3);
    flashes.flash(1);
    CHECK(flashes.litLines().isEmpty());   // nothing before the tick
    hand.tick();
    CHECK(flashes.litLines() == QList<int>{ 0, 1, 3 });

    t.ms += SonicPi::kFlashHoldMs - 1;
    hand.tick();
    CHECK(flashes.litLines() == QList<int>{ 0, 1, 3 });
    t.ms += 1;
    hand.tick();
    CHECK(flashes.litLines().isEmpty());
    std::sort(cleared.begin(), cleared.end());
    CHECK(cleared == QList<int>{ 0, 1, 3 });   // each cleared once
}

TEST_CASE("a line flashed again while lit holds from its latest flash", "[flash]")
{
    HandDriven hand;
    QObject owner;
    FakeClock t;
    SonicPi::LineFlashes flashes(&owner);
    flashes.setClock(t.clock());

    flashes.flash(2);
    hand.tick();
    t.ms += SonicPi::kFlashHoldMs - 50;
    flashes.flash(2);
    hand.tick();
    t.ms += 50;   // the hold after the first flash is up
    hand.tick();
    CHECK(flashes.isLit(2));
    t.ms += SonicPi::kFlashHoldMs - 50;   // and the hold after the latest
    hand.tick();
    CHECK_FALSE(flashes.isLit(2));
}

TEST_CASE("with Reduce animations on, a flash lights no line", "[flash][motion]")
{
    HandDriven hand;
    ReducedMotionOn still;
    QObject owner;
    SonicPi::LineFlashes flashes(&owner);
    flashes.flash(0);
    flashes.flash(3);
    hand.tick();
    CHECK(flashes.litLines().isEmpty());
    CHECK(hand.pacer->clients() == 0);   // nor holds the pacer for one
}

TEST_CASE("a card lights every line that sounds together, as the editor does", "[flash][card]")
{
    HandDriven hand;
    Card c;
    const QImage unlit = c.shot();

    // A run's lines count from 1, and the first is the with_fx :scope_out the
    // card's code is wrapped in: its own lines start at 2. The loop goes
    // round: its header, its sound and its end flash at once.
    c.deck.flashLine("ws", 2);
    c.deck.flashLine("ws", 3);
    c.deck.flashLine("ws", 5);
    hand.tick();
    const QImage lit = c.shot();
    for (int line : { 0, 1, 3 })
    {
        INFO("line " << line);
        CHECK(c.lit(lit, unlit, line));
    }
    CHECK_FALSE(c.lit(lit, unlit, 2));   // sleep never sounded

    c.t.ms += SonicPi::kFlashHoldMs;
    hand.tick();
    CHECK(c.shot() == unlit);
}

TEST_CASE("a card too short for its code scrolls to the line that lights", "[flash][card]")
{
    HandDriven hand;
    Card c(longCode(), 60);
    QScrollBar* bar = c.card->body()->verticalScrollBar();
    REQUIRE(bar->maximum() > 0);
    REQUIRE(bar->value() == 0);

    c.deck.flashLine("ws", 2 + 19);   // the last line
    hand.tick();
    CHECK(bar->value() > 0);

    c.t.ms += SonicPi::kFlashHoldMs;
    hand.tick();
}

TEST_CASE("with Reduce animations on, a card's code stays still as it plays", "[flash][card][motion]")
{
    HandDriven hand;
    ReducedMotionOn still;
    Card c(longCode(), 60);
    QScrollBar* bar = c.card->body()->verticalScrollBar();
    REQUIRE(bar->maximum() > 0);
    const QImage unlit = c.shot();

    c.deck.flashLine("ws", 2);
    c.deck.flashLine("ws", 2 + 19);
    hand.tick();
    CHECK(c.card->lineFlashes()->isEmpty());
    CHECK(c.shot() == unlit);
    CHECK(bar->value() == 0);   // nor scrolls to follow the run
}

TEST_CASE("a card washes just a line's code, as the editor does", "[flash][card]")
{
    HandDriven hand;
    Card c;
    const QImage unlit = c.shot();
    c.deck.flashLine("ws", 3);   // "  play 60"
    hand.tick();
    const QImage lit = c.shot();

    CHECK(Card::at(lit, c.text(), c.gap(1)) != Card::at(unlit, c.text(), c.gap(1)));
    // Not its indent, nor just past its last character, nor out at the edge.
    CHECK(Card::at(lit, c.text(), c.indent(1)) == Card::at(unlit, c.text(), c.indent(1)));
    const QRect last = c.text()->characterRectGlobal(c.last(1));
    const QPoint past = c.text()->mapFromGlobal(QPoint(last.right() + 4, last.center().y()));
    const qreal dpr = lit.devicePixelRatio();
    for (const int x : { qRound(past.x() * dpr), lit.width() - 2 })
    {
        const int y = qRound(past.y() * dpr);
        INFO("x " << x);
        CHECK(lit.pixelColor(x, y) == unlit.pixelColor(x, y));
    }

    c.t.ms += SonicPi::kFlashHoldMs;
    hand.tick();
}

TEST_CASE("a card's wash is as bright as Flash Brightness says", "[flash][card]")
{
    HandDriven hand;
    Card c;
    const QImage unlit = c.shot();
    const QColor under = Card::at(unlit, c.text(), c.gap(1));

    for (int percent : { 10, 80 })
    {
        INFO(percent << "%");
        c.deck.setFlashBrightness(percent);
        c.deck.flashLine("ws", 3);
        hand.tick();
        QColor wash = c.theme->color("HighlightedBackground");
        wash.setAlpha(SonicPi::flashAlpha(percent));
        CHECK(near(Card::at(c.shot(), c.text(), c.gap(1)), over(under, wash)));
        c.t.ms += SonicPi::kFlashHoldMs;
        hand.tick();
    }

    // A card made after (as a zoom or a theme makes them again) washes as
    // brightly too.
    c.deck.setFlashBrightness(80);
    c.deck.clear();
    delete c.card;
    c.card = c.make();
    QApplication::processEvents();
    const QImage fresh = c.shot();
    const QColor freshUnder = Card::at(fresh, c.text(), c.gap(1));
    c.deck.flashLine("ws", 3);
    hand.tick();
    QColor wash = c.theme->color("HighlightedBackground");
    wash.setAlpha(SonicPi::flashAlpha(80));
    CHECK(near(Card::at(c.shot(), c.text(), c.gap(1)), over(freshUnder, wash)));
    c.t.ms += SonicPi::kFlashHoldMs;
    hand.tick();
}

TEST_CASE("a card open to edit lights the lines in its editor", "[flash][card]")
{
    HandDriven hand;
    Card c;
    QAbstractButton* edit = nullptr;
    for (QAbstractButton* b : c.card->findChildren<QAbstractButton*>())
        if (b->accessibleName().startsWith("Edit"))
            edit = b;
    REQUIRE(edit);
    edit->click();
    QPlainTextEdit* editor = c.card->findChild<QPlainTextEdit*>(QStringLiteral("qsCardEditor"));
    REQUIRE(editor);

    c.deck.flashLine("ws", 2);
    c.deck.flashLine("ws", 3);
    hand.tick();
    QStringList washed;
    for (const QTextEdit::ExtraSelection& s : editor->extraSelections())
        washed << s.cursor.selectedText();
    washed.sort();
    // Each line's code, its indent left out, as the editor washes it.
    CHECK(washed == QStringList{ "live_loop :a do", "play 60" });

    c.t.ms += SonicPi::kFlashHoldMs;
    hand.tick();
    CHECK(editor->extraSelections().isEmpty());
}
