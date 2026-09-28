// The editor itself: sync's cue paths. The paths seen (a MIDI controller's among them) are offered as the strings code
// writes, and typing the quote that begins one keeps them up, narrowed as the path is typed. It used to close them:
// the editor kept the popup shut inside any string but track_control's parameter. Headless, offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QSettings>
#include <QTest>

#include "model/sonicpitheme.h"
#include "utils/scintilla_api.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpiscintilla.h"

namespace {

struct Editor
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLexer* lexer = new SonicPiLexer(theme);
    ScintillaAPI* api = new ScintillaAPI(lexer);
    QSettings keys{ QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "gui-tests-keys" };
    SonicPiScintilla* ed = new SonicPiScintilla(lexer, theme, "", true, &keys);

    Editor()
    {
        // as the window gives them (MainWindow::addCuePath): each path quoted
        api->addCuePath("\"/midi:nanokey2_keyboard:1/note_on\"");
        api->addCuePath("\"/midi:nanokey2_keyboard:1/control_change\"");
        api->addCuePath("\"/cue/beat\"");
        ed->setText(QString());   // not the "loading" placeholder line
        ed->show();
        QApplication::processEvents();
    }

    // A window left open would count in a later test's last-window accounting.
    ~Editor()
    {
        delete ed;
        delete lexer;   // owns the api
        delete theme;
        QApplication::processEvents();
    }

    // Type the text a key at a time, as a person does; the popup follows.
    void type(const QString& s)
    {
        QTest::keyClicks(ed, s);
        QApplication::processEvents();
    }
};

} // namespace

TEST_CASE("the cue paths are up after sync", "[completion][cue][editor]")
{
    Editor e;
    e.type("sync ");
    CHECK(e.ed->completionActive());
}

TEST_CASE("the quote that begins a path keeps the cue paths up", "[completion][cue][editor]")
{
    Editor e;
    e.type("sync \"");
    CHECK(e.ed->completionActive());
}

TEST_CASE("the paths narrow as one is typed, and the one taken is a string", "[completion][cue][editor]")
{
    Editor e;
    e.type("sync \"/midi:nanokey2_keyboard:1/note");
    REQUIRE(e.ed->completionActive());
    e.ed->acceptCompletionPopup();
    CHECK(e.ed->text().trimmed() == "sync \"/midi:nanokey2_keyboard:1/note_on\"");
}

TEST_CASE("a path taken straight after sync goes in as a string", "[completion][cue][editor]")
{
    Editor e;
    e.type("sync /cue/be");
    REQUIRE(e.ed->completionActive());
    e.ed->acceptCompletionPopup();
    CHECK(e.ed->text().trimmed() == "sync \"/cue/beat\"");
}

TEST_CASE("a path taken inside a string already closed has one closing quote", "[completion][cue][editor]")
{
    Editor e;
    e.ed->setText("sync \"\"");
    e.ed->setCursorPosition(0, 6);   // between the quotes
    e.type("/cue/be");
    REQUIRE(e.ed->completionActive());
    e.ed->acceptCompletionPopup();
    CHECK(e.ed->text().trimmed() == "sync \"/cue/beat\"");
}

TEST_CASE("a string that is not a cue path still keeps the popup shut", "[completion][cue][editor]")
{
    Editor e;
    e.type("puts \"/cue");
    CHECK_FALSE(e.ed->completionActive());
}
