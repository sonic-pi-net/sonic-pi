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

// The edit toolbar over the editor's top right corner fades to a faint ghost while code runs beneath it, letting the
// mouse through to the code, and fades back once no code does. On a fresh start it stayed a ghost over nothing until
// the help panel was made full size and back (#3599): the loading line began the fade, the buffer's code took its
// place before the fade had moved, and the fade carried on. Headless, offscreen, the fades on the test's own clock.

#include <catch2/catch_test_macros.hpp>

#include <QAnimationDriver>
#include <QApplication>
#include <QImage>
#include <QSettings>

#include "model/sonicpitheme.h"
#include "utils/scintilla_api.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpiscintilla.h"

namespace {

const QString kShort = QStringLiteral("play 60");
const QString kLong = QStringLiteral("play 60, amp: 0.5, release: 2, cutoff: 100, pan: -0.5, attack: 0.1, sustain: 1");

// Qt's animations, the toolbar's fade among them, move only when the test moves this clock.
class HandClock : public QAnimationDriver
{
public:
    HandClock() { install(); }
    ~HandClock() override { uninstall(); }
    qint64 elapsed() const override { return m_ms; }

    void run(qint64 ms)
    {
        for (qint64 t = 0; t < ms; t += 16)
        {
            m_ms += 16;
            advance();
        }
        QApplication::processEvents();
    }

private:
    qint64 m_ms = 0;
};

struct Editor
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLexer* lexer = new SonicPiLexer(theme);
    ScintillaAPI* api = new ScintillaAPI(lexer);
    QSettings keys{ QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "gui-tests-keys" };
    SonicPiScintilla* ed = new SonicPiScintilla(lexer, theme, "", true, &keys);
    QWidget* bar = nullptr;

    // The editor as the window makes it, the toolbar switched on, with its code from the start (or, with none, the
    // loading line the editor shows until the buffer's code is read in).
    explicit Editor(const QString& code = {})
    {
        if (!code.isNull())
            ed->setText(code);
        ed->setEditorToolbarEnabled(true);
        ed->resize(420, 300);   // the loading line and kLong run under the toolbar, kShort does not
        ed->show();
        settle();
        bar = ed->findChild<QWidget*>(QStringLiteral("editorToolbar"));
    }

    ~Editor()
    {
        delete ed;
        delete lexer;   // owns the api
        delete theme;
        QApplication::processEvents();
    }

    static void settle()
    {
        for (int i = 0; i < 5; ++i)
            QApplication::processEvents();
    }

    void setCode(const QString& code)
    {
        ed->setText(code);
        settle();
    }

    // A ghost lets the mouse through to the code beneath (EditorToolbar::setOccluded).
    bool mouseThrough() const { return bar->testAttribute(Qt::WA_TransparentForMouseEvents); }

    QImage look() const { return bar->grab().toImage(); }
};

} // namespace

TEST_CASE("the edit toolbar lets the mouse through over code, and stops once there is none", "[editor][toolbar]")
{
    Editor e;   // the loading line under the toolbar
    REQUIRE(e.bar != nullptr);
    REQUIRE(e.bar->isVisible());
    CHECK(e.mouseThrough());

    e.setCode(kShort);
    CHECK_FALSE(e.mouseThrough());

    e.setCode(kLong);
    CHECK(e.mouseThrough());

    e.ed->resize(1200, 300);   // widened clear of the code
    Editor::settle();
    CHECK_FALSE(e.mouseThrough());
}

TEST_CASE("the edit toolbar looks as it always does once the code that began its fade has gone", "[editor][toolbar]")
{
    HandClock clock;
    Editor clear(kShort);   // never any code beneath
    clock.run(400);

    Editor e;   // the loading line under the toolbar begins the fade to a ghost...
    e.setCode(kShort);   // ...and the buffer's code takes its place before the fade has moved
    clock.run(400);

    REQUIRE(e.bar != nullptr);
    CHECK_FALSE(e.mouseThrough());
    CHECK(e.look() == clear.look());
}

TEST_CASE("the edit toolbar stays a ghost once code comes back before its fade back has moved", "[editor][toolbar]")
{
    HandClock clock;
    Editor ghost(kLong);   // code beneath all along
    clock.run(400);

    Editor e(kLong);
    clock.run(400);   // a ghost
    e.setCode(kShort);   // the fade back begins...
    e.setCode(kLong);    // ...and code is beneath again before it has moved
    clock.run(400);

    REQUIRE(e.bar != nullptr);
    CHECK(e.mouseThrough());
    CHECK(e.look() == ghost.look());
}
