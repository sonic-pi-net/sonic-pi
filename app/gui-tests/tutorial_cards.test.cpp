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
// mountSnippets). With SP_GUI_SHOT_DIR set the [.shots] case renders a part
// offscreen to PNGs, to set beside the web's tutorial page.

#include <catch2/catch_test_macros.hpp>

#include <QAbstractButton>
#include <QApplication>
#include <QDir>
#include <QFile>
#include <QLabel>
#include <QPushButton>
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
    CHECK(first->codeText()->washedLine() == 0);
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
    pane.showExamplePage("acid", "Acid Walk", exampleCode("magician/acid.rb"),
                         "Start producing longer tracks...");
    settle();
    shoot(pane, "native-example-acid");
    pane.showLangPage(langPage("play"));
    settle();
    shoot(pane, "native-lang-play");
    delete theme;
}
