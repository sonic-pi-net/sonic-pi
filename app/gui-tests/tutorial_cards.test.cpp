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

// The tutorial's examples, as the pane draws them: cards, as the web's
// tutorial has (app/web/scripts/build-site.mjs tutorialBlocks, site.js
// mountSnippets); and the lead lines and boxes in the words around them. With SP_GUI_SHOT_DIR set the [.shots] case renders a part
// offscreen to PNGs, to set beside the web's tutorial page.

#include <catch2/catch_test_macros.hpp>

#include "pacer_by_hand.h"
#include "utils/flash_style.h"
#include "utils/lineflashes.h"

#include <QAbstractButton>
#include <QApplication>
#include <QDir>
#include <QFile>
#include <QFrame>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QLabel>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollArea>
#include <QScrollBar>
#include <QSignalSpy>
#include <QGuiApplication>
#include <QMouseEvent>
#include <QTest>
#include <QTimer>
#include <QWindow>

#include <algorithm>

#include "model/sonicpitheme.h"
#include "own_settings.h"
#include "utils/fontroles.h"
#include "utils/tutorialdocs.h"
#include "widgets/codecard.h"
#include "widgets/tutorialpane.h"
#include "widgets/tutorialwidgets.h"

namespace {

void settle()
{
    for (int i = 0; i < 6; i++) { QApplication::processEvents(); QTest::qWait(10); }
}

SonicPi::TutorialChapter chapter(const QString& file)
{
    QFile f(QStringLiteral(SP_ROOT) + "/etc/doc/generated/native/en/tutorial/" + file);
    REQUIRE(f.open(QIODevice::ReadOnly));
    return SonicPi::TutorialDocs::chapterFromJson(f.readAll());
}

void shoot(QWidget& w, const QString& name)
{
    const QString dir = qEnvironmentVariable("SP_GUI_SHOT_DIR");
    if (dir.isEmpty()) return;
    QDir().mkpath(dir);
    w.grab().save(dir + QStringLiteral("/") + name + QStringLiteral(".png"));
}

SonicPi::LangPage langPage(const QString& key)
{
    QFile f(QStringLiteral(SP_ROOT) + "/etc/doc/generated/native/reference/lang.json");
    REQUIRE(f.open(QIODevice::ReadOnly));
    for (const SonicPi::LangPage& page : SonicPi::TutorialDocs::langPagesFromJson(f.readAll()))
        if (page.key == key)
            return page;
    FAIL("no lang page " << key.toStdString());
    return {};
}

struct Docs
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    TutorialPane* pane = nullptr;

    Docs()
    {
        QApplication::setFont(RoleFont(FontRole::Base));
        qApp->setStyleSheet(theme->getAppStylesheet());
        pane = new TutorialPane(theme);
        pane->resize(1000, 900);
    }
    ~Docs()
    {
        delete pane;
        delete theme;
    }

    void load(const QString& file)
    {
        pane->loadChapter(chapter(file), QStringLiteral(SP_ROOT) + "/etc/doc/images", "Before", "After");
        pane->show();
        settle();
    }

    // The page's cards, top to bottom.
    QList<CodeCard*> cards() const
    {
        QList<CodeCard*> all;
        for (CodeCard* c : pane->findChildren<CodeCard*>())
            if (!c->isHidden())
                all << c;
        std::sort(all.begin(), all.end(), [this](CodeCard* a, CodeCard* b) {
            return a->mapTo(pane, QPoint()).y() < b->mapTo(pane, QPoint()).y();
        });
        return all;
    }
};

QAbstractButton* button(QWidget* card, const QString& accessibleStart)
{
    for (QAbstractButton* b : card->findChildren<QAbstractButton*>())
        if (b->accessibleName().startsWith(accessibleStart)) return b;
    return nullptr;
}

QString shown(QWidget* card, const char* name)
{
    QLabel* l = card->findChild<QLabel*>(QString::fromLatin1(name));
    return l && !l->isHidden() ? l->text() : QString();
}

} // namespace

TEST_CASE("a tutorial part's code is cards, named for the heading over them", "[tutorial][cards]")
{
    Docs docs;
    docs.load("02.1-Your-First-Beeps.json");
    QStringList titles;
    for (CodeCard* c : docs.cards())
        titles << c->title();
    CHECK(titles == QStringList{ "Your First Beeps", "Beep!", "Beep! · 2", "Chords", "Melody",
                                 "Melody · 2", "Traditional Note Names",
                                 "Traditional Note Names · 2" });

    // Each plays, with the web tutorial's Edit, Reset once edited, Copy and
    // Open, and the Cards tab's drag handle: picked up and dropped on the
    // editor.
    CodeCard* first = docs.cards().value(0);
    REQUIRE(first);
    CHECK(first->code() == "play 70");
    CHECK(first->playButton());
    CHECK(button(first, "Edit Your First Beeps"));
    CHECK(button(first, "Copy Your First Beeps"));
    CHECK(button(first, "Open Your First Beeps"));
    CHECK_FALSE(button(first, "Add Your First Beeps"));
    CHECK(first->spec().actions & CodeCard::Drag);
    REQUIRE(button(first, "Reset Your First Beeps"));
    CHECK(button(first, "Reset Your First Beeps")->isHidden());
}

// The page's headings, top to bottom.
static QList<TutHeading*> headings(QWidget* pane)
{
    QList<TutHeading*> all = pane->findChildren<TutHeading*>();
    std::sort(all.begin(), all.end(), [pane](TutHeading* a, TutHeading* b) {
        return a->mapTo(pane, QPoint()).y() < b->mapTo(pane, QPoint()).y();
    });
    return all;
}

TEST_CASE("a tutorial part opens with its title, numbered, as the web's does", "[tutorial][headings]")
{
    // the title in place of the part's own # (its name unnumbered, here
    // worded otherwise): the page's one top heading
    {
        Docs docs;
        docs.load("01-Welcome-to-Sonic-Pi.json");
        const QList<TutHeading*> hs = headings(docs.pane);
        REQUIRE(hs.size() == 1);
        CHECK(hs[0]->text() == "1 Welcome to Sonic Pi");
        CHECK(hs[0]->headingLevel() == 1);
        CHECK(hs[0]->objectName() == "tutH1");
    }
    // the cards above the part's first ## named for the title less its
    // number, as the web names them (the part's own # says "Sliding Opts")
    {
        Docs docs;
        docs.load("07.3-Sliding-Parameters.json");
        REQUIRE(docs.cards().size() >= 2);
        CHECK(docs.cards()[0]->title() == "Sliding Options");
        CHECK(docs.cards()[1]->title() == "Sliding Options · 2");
    }
    // a # further on reads a level under the title, as the web's <h2>
    {
        Docs docs;
        docs.load("A.20-creative-coding-in-the-classroom.json");
        const QList<TutHeading*> hs = headings(docs.pane);
        auto later = std::find_if(hs.begin(), hs.end(), [](TutHeading* h) { return h->text() == "Some fun capabilities"; });
        REQUIRE(later != hs.end());
        CHECK((*later)->headingLevel() == 2);
        CHECK(std::count_if(hs.begin(), hs.end(), [](TutHeading* h) { return h->headingLevel() == 1; }) == 1);
    }
}

TEST_CASE("a fragment of a tutorial part to read is a still card", "[tutorial][cards]")
{
    Docs docs;
    docs.load("03.6-External-Samples.json");
    CodeCard* still = nullptr;
    for (CodeCard* c : docs.cards())
        if (c->code().startsWith("# Raspberry Pi, Mac, Linux"))
            still = c;
    REQUIRE(still);
    CHECK_FALSE(still->playButton());
    CHECK(still->property("still").toBool());
}

TEST_CASE("a tutorial card's code reads on with the page", "[tutorial][cards][a11y]")
{
    Docs docs;
    docs.load("02.1-Your-First-Beeps.json");
    CodeCard* first = docs.cards().value(0);
    REQUIRE(first);
    TutProseText* code = first->codeText();
    REQUIRE(code);
    CHECK(code->focusPolicy() == Qt::StrongFocus);

    // The prose just above the card: stepping off its end lands in the code,
    // and off the code's end in the prose below.
    TutProseText* above = nullptr;
    const int cardY = first->mapTo(docs.pane, QPoint()).y();
    for (TutProseText* t : docs.pane->findChildren<TutProseText*>())
    {
        if (first->isAncestorOf(t) || t->mapTo(docs.pane, QPoint()).y() >= cardY)
            continue;
        if (!above || t->mapTo(docs.pane, QPoint()).y() > above->mapTo(docs.pane, QPoint()).y())
            above = t;
    }
    REQUIRE(above);
    above->setFocus();
    above->setCaretPosition(above->endPosition());
    QTest::keyClick(above, Qt::Key_Right);
    CHECK(QApplication::focusWidget() == code);
    CHECK(code->caretPosition() == 0);
}

namespace {
// The page's words that read top to bottom, in that order.
QList<TutProseText*> texts(QWidget* pane)
{
    QList<TutProseText*> all = pane->findChildren<TutProseText*>();
    std::sort(all.begin(), all.end(), [pane](TutProseText* a, TutProseText* b) {
        return a->mapTo(pane, QPoint()).y() < b->mapTo(pane, QPoint()).y();
    });
    return all;
}

QString plain(TutProseText* t) { return t->property("mdtext").toString().remove(QRegularExpression("<[^>]*>")).trimmed(); }

// Off the end of `from`, as an arrow key steps: where the caret lands.
QWidget* stepOn(TutProseText* from)
{
    from->setFocus();
    from->setCaretPosition(from->endPosition());
    QTest::keyClick(from, Qt::Key_Right);
    return QApplication::focusWidget();
}
} // namespace

TEST_CASE("a tutorial part's lead line is its words set larger, read first", "[tutorial][blocks][a11y]")
{
    Docs docs;
    docs.load("01-Welcome-to-Sonic-Pi.json");
    const QList<TutProseText*> all = texts(docs.pane);
    REQUIRE(all.size() >= 2);
    TutProseText* lead = all[0];
    CHECK(lead->objectName() == "tutLead");
    CHECK(plain(lead) == "Welcome friend. Today is the perfect day to learn how to live code!");
    TutProseText* next = all[1];
    CHECK(next->objectName() == "tutProse");
    CHECK(lead->font().pointSizeF() > next->font().pointSizeF());
    CHECK(stepOn(lead) == next);
}

TEST_CASE("a tutorial part's box is its label over its words, read straight through", "[tutorial][blocks][a11y]")
{
    Docs docs;
    docs.load("01-Welcome-to-Sonic-Pi.json");
    const QList<QFrame*> boxes = docs.pane->findChildren<QFrame*>("tutBox");
    REQUIRE(boxes.size() == 1);
    QFrame* box = boxes[0];

    // Its label: drawn in capitals, said as written, and said once (the box
    // itself has no name of its own to say over it).
    QLabel* label = box->findChild<QLabel*>("tutBoxLabel");
    REQUIRE(label);
    CHECK(label->text() == "How this tutorial works");
    CHECK(label->font().capitalization() == QFont::AllUppercase);
    CHECK(box->accessibleName().isEmpty());
    CHECK(box->maximumWidth() == docs.pane->findChild<TutProseText*>("tutProse")->maximumWidth());

    // Its words, inside it, read on from the words before it and on into the
    // words after it.
    QList<TutProseText*> inside, all = texts(docs.pane);
    for (TutProseText* t : all)
        if (box->isAncestorOf(t))
            inside << t;
    REQUIRE(inside.size() == 2);
    CHECK(plain(inside[0]).startsWith("This tutorial is divided up into sections"));
    CHECK(plain(inside[1]).startsWith("Also, if you have any thoughts"));
    const int first = all.indexOf(inside[0]);
    REQUIRE(first > 0);
    CHECK(plain(all[first - 1]).startsWith("Watching others live code"));
    CHECK(stepOn(all[first - 1]) == inside[0]);
    CHECK(stepOn(inside[0]) == inside[1]);
    REQUIRE(first + 2 < all.size());
    CHECK(stepOn(inside[1]) == all[first + 2]);
    CHECK(plain(all[first + 2]).startsWith("OK, let"));
}

TEST_CASE("a box with no label is its words alone", "[tutorial][blocks]")
{
    SonicPi::TutorialChapter ch = SonicPi::TutorialDocs::chapterFromJson(R"({"title":"t","blocks":[
        {"type":"prose","html":"<p>Before.</p>"},
        {"type":"box","label":"","blocks":[{"type":"prose","html":"<p>Inside.</p>"},
          {"type":"list","ordered":true,"items":["one","two"]}]}]})");
    Docs docs;
    docs.pane->loadChapter(ch, QString(), "Before", "After");
    docs.pane->show();
    settle();
    QFrame* box = docs.pane->findChild<QFrame*>("tutBox");
    REQUIRE(box);
    CHECK_FALSE(box->findChild<QLabel*>("tutBoxLabel"));
    QList<TutProseText*> inside;
    for (TutProseText* t : texts(docs.pane))
        if (box->isAncestorOf(t))
            inside << t;
    REQUIRE(inside.size() == 2);
    CHECK(plain(inside[0]) == "Inside.");
    CHECK(plain(inside[1]).contains("one"));
}

TEST_CASE("a box's words show whole at any width of the pane", "[tutorial][blocks]")
{
    // A box is capped at the prose's measure, and its words sit inside its
    // edge and padding. Wider than the cap, the column must still measure
    // them at the width they're drawn at: measured at the cap, a paragraph
    // a few words over a line is given a line too few and its end is cut
    // off. Paragraphs of every length from 1 word to 150 put some in that
    // window whatever the fonts.
    QJsonArray paragraphs;
    QStringList words;
    for (int n = 1; n <= 150; n++)
    {
        words << QStringLiteral("word%1").arg(n % 10);
        paragraphs.append(QJsonObject{ { "type", "prose" }, { "html", "<p>" + words.join(' ') + "</p>" } });
    }
    const QJsonObject json{ { "title", "1 Boxes" },
                            { "blocks", QJsonArray{ QJsonObject{ { "type", "box" }, { "label", "A box" }, { "blocks", paragraphs } } } } };
    Docs docs;
    docs.pane->loadChapter(SonicPi::TutorialDocs::chapterFromJson(QJsonDocument(json).toJson()), QString(), "Before", "After");
    docs.pane->show();
    settle();
    QFrame* box = docs.pane->findChild<QFrame*>("tutBox");
    REQUIRE(box);
    QStringList cut;
    bool capped = false;
    for (int w = 600; w <= 2 * box->maximumWidth(); w += 7)
    {
        docs.pane->resize(w, 900);
        QApplication::processEvents();
        capped = capped || box->width() == box->maximumWidth();
        for (TutProseText* t : texts(docs.pane))
            if (t->height() < t->heightForWidth(t->width()))
                cut << QStringLiteral("%1 wide: %2 words, %3 high of %4")
                           .arg(w).arg(plain(t).count(' ') + 1).arg(t->height()).arg(t->heightForWidth(t->width()));
    }
    CHECK(capped); // the sweep reached past the cap
    QStringList widths;
    for (const QString& c : cut)
        if (!widths.contains(c.section(' ', 0, 0)))
            widths << c.section(' ', 0, 0);
    INFO("cut at " << widths.join(' ').toStdString() << " wide; first:\n" << cut.mid(0, 4).join(QLatin1Char('\n')).toStdString());
    CHECK(cut.isEmpty());
}

TEST_CASE("a box goes with its page", "[tutorial][blocks]")
{
    Docs docs;
    docs.load("01-Welcome-to-Sonic-Pi.json");
    REQUIRE(docs.pane->findChildren<QFrame*>("tutBox").size() == 1);
    docs.load("02.1-Your-First-Beeps.json");
    CHECK(docs.pane->findChildren<QFrame*>("tutBox").isEmpty());
    for (TutProseText* t : texts(docs.pane))
        CHECK_FALSE(plain(t).startsWith("This tutorial is divided"));
}

namespace {
// Press on `at` in `w` and move as a hand does: whether the card it is on went
// up into a drag. QDrag::exec runs a loop of its own until the button comes up,
// so the release is sent from inside it.
bool picksUp(CodeCard* card, QWidget* w, QPoint at)
{
    QSignalSpy ended(card, &CodeCard::dragEnded);
    QTimer::singleShot(300, [] {
        if (QWindow* win = QGuiApplication::focusWindow())
            QTest::mouseRelease(win, Qt::LeftButton);
    });
    QTest::mousePress(w, Qt::LeftButton, Qt::NoModifier, at);
    const QPoint to = at + QPoint(30, 30);
    QMouseEvent move(QEvent::MouseMove, QPointF(to), w->mapToGlobal(QPointF(to)), Qt::NoButton,
                     Qt::LeftButton, Qt::NoModifier);
    QApplication::sendEvent(w, &move);
    QTest::qWait(500);
    QTest::mouseRelease(w, Qt::LeftButton, Qt::NoModifier, at);
    return ended.count() > 0;
}

} // namespace

TEST_CASE("a tutorial card is picked up by its handle and its face, as the Cards tab's are", "[tutorial][cards]")
{
    Docs docs;
    docs.load("02.1-Your-First-Beeps.json");
    CodeCard* first = docs.cards().value(0);
    REQUIRE(first);
    QWidget* handle = nullptr;   // pointer-only: no accessible name, the open hand
    for (QPushButton* b : first->findChildren<QPushButton*>(QStringLiteral("qsCardBtn")))
        if (b->cursor().shape() == Qt::OpenHandCursor)
            handle = b;
    REQUIRE(handle);
    CHECK(picksUp(first, handle, QPoint(5, 5)));
    QWidget* header = first->findChild<QWidget*>(QStringLiteral("qsCardHeader"));
    REQUIRE(header);
    CHECK(picksUp(first, header, QPoint(8, 8)));

    // Its keyboard equivalent: I puts the code in the editor at the cursor.
    QSignalSpy inserts(docs.pane, &TutorialPane::insertRequested);
    first->setFocus();
    QTest::keyClick(first, Qt::Key_I);
    REQUIRE(inserts.count() == 1);
    CHECK(inserts[0][1].toString() == "\nplay 70\n\n");
}

TEST_CASE("a tutorial part plays one card at a time", "[tutorial][cards]")
{
    HandDriven hand;
    Docs docs;
    docs.load("02.1-Your-First-Beeps.json");
    QSignalSpy runs(docs.pane, &TutorialPane::runRequested);
    QSignalSpy stops(docs.pane, &TutorialPane::stopJobRequested);
    CodeCard* first = docs.cards().value(0);
    CodeCard* second = docs.cards().value(1);
    REQUIRE(first);
    REQUIRE(second);

    first->playButton()->click();
    REQUIRE(runs.count() == 1);
    CHECK(runs[0][0].toString() == "play 70");
    CHECK_FALSE(runs[0][2].toBool());   // heard: not silent
    CHECK(runs[0][3].toBool());         // through its own scope tap, for the rings
    const QString ws = runs[0][1].toString();
    docs.pane->runStarted(7, ws);
    CHECK(first->isPlaying());

    // What it puts, its error, the line that sounded: on the card.
    docs.pane->runOutput(7, "hello");
    CHECK(shown(first, "qsCardOutput").contains("hello"));
    docs.pane->flashLine(ws, 2);
    hand.tick();
    CHECK(first->codeText()->isWashed(0));
    first->lineFlashes()->clear();
    docs.pane->runError(7, "Something broke", 2);
    CHECK(shown(first, "qsCardError") == "Something broke (line 1)");

    second->playButton()->click();
    REQUIRE(stops.count() == 1);
    CHECK(stops[0][0].toInt() == 7);
    CHECK_FALSE(first->isPlaying());
}

TEST_CASE("a playing tutorial card plays on through a zoom", "[tutorial][cards]")
{
    Docs docs;
    docs.load("02.1-Your-First-Beeps.json");
    QSignalSpy runs(docs.pane, &TutorialPane::runRequested);
    docs.cards().value(1)->playButton()->click();
    REQUIRE(runs.count() == 1);
    docs.pane->runStarted(7, runs[0][1].toString());

    docs.pane->setUserZoom(docs.pane->userZoom() + 1);
    settle();
    CodeCard* again = docs.cards().value(1);
    REQUIRE(again);
    CHECK(again->title() == "Beep!");
    CHECK(again->isPlaying());
}

TEST_CASE("Open puts a tutorial card's code in the editor", "[tutorial][cards]")
{
    Docs docs;
    docs.load("02.1-Your-First-Beeps.json");
    QSignalSpy loads(docs.pane, &TutorialPane::loadRequested);
    QAbstractButton* open = button(docs.cards().value(0), "Open Your First Beeps");
    REQUIRE(open);
    open->click();
    REQUIRE(loads.count() == 1);
    CHECK(loads[0][0].toString() == "play 70");
}

TEST_CASE("a Lang page's examples are cards", "[tutorial][cards]")
{
    Docs docs;
    const SonicPi::LangPage play = langPage("play");
    docs.pane->showLangPage(play);
    docs.pane->show();
    settle();
    const QList<CodeCard*> cards = docs.cards();
    REQUIRE(cards.size() == play.examples.size());
    CHECK(cards[0]->title() == "Examples");
    CHECK(cards[1]->title() == "Examples · 2");
    CHECK(cards[0]->code() == play.examples[0].code);
    CHECK(bool(cards[0]->playButton()) == play.examples[0].runnable);
    // As the web's docs pane has them: into the editor by Add or a drag.
    CHECK(button(cards[0], "Add Examples"));
    CHECK_FALSE(button(cards[0], "Open Examples"));
}

namespace {
QString exampleCode(const QString& path)
{
    QFile f(QStringLiteral(SP_ROOT) + "/etc/examples/" + path);
    REQUIRE(f.open(QIODevice::ReadOnly | QIODevice::Text));
    return QString::fromUtf8(f.readAll());
}
} // namespace

TEST_CASE("an example is one card, fitted to the pane", "[examples][cards]")
{
    OwnSettings settings;
    Docs docs;
    docs.pane->show();
    const QString acid = exampleCode("magician/acid.rb");
    docs.pane->showExamplePage("acid", "Acid Walk", acid, "Start producing longer tracks...");
    settle();

    const QList<CodeCard*> cards = docs.cards();
    REQUIRE(cards.size() == 1);
    CodeCard* card = cards[0];
    CHECK(card->title() == "Acid Walk");
    CHECK(card->code() == acid.chopped(acid.endsWith('\n') ? 1 : 0));
    CHECK(shown(card, "qsCardBlurb") == "Start producing longer tracks...");
    // The web's Examples cards' Edit, Reset once edited, Copy and Open, and
    // the Cards tab's drag handle.
    CHECK(button(card, "Edit Acid Walk"));
    CHECK(button(card, "Copy Acid Walk"));
    CHECK(button(card, "Open Acid Walk"));
    CHECK_FALSE(button(card, "Add Acid Walk"));
    CHECK(card->spec().actions & CodeCard::Drag);
    // An edit is kept, as the web's Examples page keeps it.
    CHECK(card->spec().key == "examples/acid");

    // The whole card shows, Play and all; a long example scrolls inside it.
    const QRect inPane(card->mapTo(docs.pane, QPoint()), card->size());
    CHECK(inPane.bottom() <= docs.pane->height());
    REQUIRE(card->playButton());
    CHECK(card->playButton()->isVisible());
    CHECK(card->body()->verticalScrollBar()->maximum() > 0);

    // And it still fits once the pane is shorter.
    docs.pane->resize(docs.pane->width(), 600);
    settle();
    CHECK(QRect(card->mapTo(docs.pane, QPoint()), card->size()).bottom() <= docs.pane->height());
}

TEST_CASE("an example plays when opened, and plays on through a zoom", "[examples][cards]")
{
    OwnSettings settings;
    Docs docs;
    docs.pane->show();
    const QString haunted = exampleCode("apprentice/haunted.rb");
    docs.pane->showExamplePage("haunted", "Haunted Bells", haunted, "Listen to the coded bells...");
    settle();
    QSignalSpy runs(docs.pane, &TutorialPane::runRequested);

    // Examples > Play When Opened plays the page's card.
    REQUIRE(docs.pane->playFirstSnippet());
    REQUIRE(runs.count() == 1);
    CHECK(runs[0][0].toString().startsWith("# Coded by Sam Aaron"));
    CHECK(runs[0][3].toBool());   // through the scope tap, for the rings
    docs.pane->runStarted(7, runs[0][1].toString());
    CHECK(docs.cards().value(0)->isPlaying());

    docs.pane->setUserZoom(docs.pane->userZoom() + 1);
    settle();
    REQUIRE(docs.cards().size() == 1);
    CHECK(docs.cards()[0]->isPlaying());

    // Open puts it in the editor.
    QSignalSpy loads(docs.pane, &TutorialPane::loadRequested);
    button(docs.cards()[0], "Open Haunted Bells")->click();
    REQUIRE(loads.count() == 1);
    CHECK(loads[0][0].toString().startsWith("# Coded by Sam Aaron"));
}

TEST_CASE("the tutorial's examples, rendered for a look", "[tutorial][cards][.shots]")
{
    const QString root = QStringLiteral(SP_ROOT);
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", root);
    QApplication::setFont(RoleFont(FontRole::Base));
    qApp->setStyleSheet(theme->getAppStylesheet());
    TutorialPane pane(theme);
    pane.resize(1000, 900);
    pane.loadChapter(chapter("02.1-Your-First-Beeps.json"), root + "/etc/doc/images",
                     "Synths", "Synth Options");
    pane.show();
    settle();
    shoot(pane, "native-tutorial-02.1");
    // the opening parts, their lead lines and boxes: the whole page, as the web's is seen scrolled through
    for (const QString part : { QStringLiteral("01-Welcome-to-Sonic-Pi"), QStringLiteral("01.1-Live-Coding") })
    {
        pane.loadChapter(chapter(part + ".json"), root + "/etc/doc/images", "Before", "After");
        settle();
        QWidget* page = pane.findChild<QWidget*>("tutorialContent");
        REQUIRE(page);
        shoot(*page, "native-tutorial-" + part);
    }
    pane.showExamplePage("acid", "Acid Walk", exampleCode("magician/acid.rb"),
                         "Start producing longer tracks...");
    settle();
    shoot(pane, "native-example-acid");
    pane.showLangPage(langPage("play"));
    settle();
    shoot(pane, "native-lang-play");
    delete theme;
}
