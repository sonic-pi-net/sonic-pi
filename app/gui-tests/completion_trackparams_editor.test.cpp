// The editor itself, not just the API: track_control's parameter is a string,
// and the editor keeps the popup shut inside a string — except here, where
// the string is the name being typed. Headless, offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QSettings>
#include <QTest>

#include "model/sonicpitheme.h"
#include "utils/scintilla_api.h"
#include "utils/trackparam.h"
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
        // The window resolves use_track from the text; here the track is given.
        api->setTrackResolver([]() { return QStringLiteral("surge"); });
        api->updateTracks(QStringList{ "surge", "verb" });
        SonicPi::TrackParam cutoff, res;
        cutoff.name = "Cutoff"; cutoff.group = "Filter";
        res.name = "Filter 1 Resonance"; res.group = "Filter"; res.value = 0.5;
        api->updateTrackParams("surge", { cutoff, res });
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

TEST_CASE("the parameter list is up after the verb", "[completion][track][editor]")
{
    Editor e;
    e.type("track_control ");
    CHECK(e.ed->completionActive());
}

TEST_CASE("the parameter list stays up while the quoted name is typed", "[completion][track][editor]")
{
    Editor e;
    e.type("track_control \"Fil");
    CHECK(e.ed->completionActive());
    e.ed->acceptCompletionPopup();
    CHECK(e.ed->text().trimmed() == "track_control \"Filter 1 Resonance\"");
}

TEST_CASE("a string anywhere else still keeps the popup shut", "[completion][track][editor]")
{
    Editor e;
    e.type("puts \"pla");
    CHECK_FALSE(e.ed->completionActive());
}

TEST_CASE("a space inside the name is a space, not an accept", "[completion][track][editor]")
{
    Editor e;
    e.type("track_control \"Filter 1");
    CHECK(e.ed->completionActive());
    CHECK(e.ed->text() == "track_control \"Filter 1");
}

TEST_CASE("the opening quote alone shows every parameter", "[completion][track][editor]")
{
    Editor e;
    e.type("track_control \"");
    CHECK(e.ed->completionActive());
}

TEST_CASE("a parameter key completes on track_midi and its value gets a slider", "[completion][track][editor]")
{
    Editor e;
    e.type("track_midi :e3, res");
    REQUIRE(e.ed->completionActive());
    e.ed->acceptCompletionPopup();
    CHECK(e.ed->text().trimmed() == "track_midi :e3, filter_1_resonance:");
    e.type(" ");
    CHECK(e.ed->completionActive());   // the slider over 0 – 1
}
