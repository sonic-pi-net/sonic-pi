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

// The editor's undo: a run of typing, or of deleting, goes back in one step, not a key at a time (#3562). A key
// pressed while the completion popup has a preview in the code makes one step with the preview it replaces, and
// that step is closed behind it, so what is typed afterwards is a step of its own. Headless, offscreen.

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
        // paths for sync's popup to preview, as the window gives them (MainWindow::addCuePath)
        api->addCuePath("\"/cue/beat\"");
        api->addCuePath("\"/cue/bar\"");
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

    void key(Qt::Key k)
    {
        QTest::keyClick(ed, k);
        QApplication::processEvents();
    }

    std::string code() const { return ed->text().toStdString(); }
};

} // namespace

TEST_CASE("a run of typing goes back in one step", "[editor][undo]")
{
    Editor e;
    e.type("play 60");
    e.key(Qt::Key_Escape);   // the popup down, the code as typed
    REQUIRE(e.code() == "play 60");

    e.ed->undo();
    CHECK(e.code() == "");
}

TEST_CASE("a run of deleting comes back in one step", "[editor][undo]")
{
    Editor e;
    e.type("play 60");
    e.key(Qt::Key_Escape);
    for (int i = 0; i < 3; i++)
        e.key(Qt::Key_Backspace);
    e.key(Qt::Key_Escape);
    REQUIRE(e.code() == "play");

    e.ed->undo();
    CHECK(e.code() == "play 60");
}

TEST_CASE("a key pressed over a preview closes its step behind it", "[editor][undo][completion]")
{
    Editor e;
    e.type("sync ");
    REQUIRE(e.ed->completionActive());
    e.key(Qt::Key_Down);   // a path previewed in the code
    REQUIRE(e.code() != "sync ");
    e.type("x");           // typed over the preview, which goes
    e.key(Qt::Key_Escape);
    REQUIRE(e.code() == "sync x");

    e.ed->setCursorPosition(0, 0);   // elsewhere, so what comes next is a run of its own
    e.type("ab");
    REQUIRE(e.code() == "absync x");

    e.ed->undo();
    CHECK(e.code() == "sync x");
}
