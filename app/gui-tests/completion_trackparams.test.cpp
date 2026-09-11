// The parameters of the plugins on a track, as the Tracks panel reports them,
// complete two ways — as opt keys on the track verbs, with a slider over the
// plugin's own range, and as the quoted name in track_control's parameter
// slot. The track is the one use_track set (the resolver, as the window
// wires it), the one named first by live_track / with_send, or `track:`.
// Headless: the ScintillaAPI alone, fed as the window feeds it.

#include <catch2/catch_test_macros.hpp>

#include <Qsci/qscilexerruby.h>
#include <QStringList>

#include "utils/scintilla_api.h"
#include "utils/completion_context.h"
#include "utils/trackparam.h"

namespace {

SonicPi::TrackParam param(const QString& name, const QString& group, double lo, double hi,
                          double value, const QString& plugin = QString())
{
    SonicPi::TrackParam p;
    p.name = name; p.group = group; p.min = lo; p.max = hi; p.value = value; p.plugin = plugin;
    return p;
}

struct Api
{
    QsciLexerRuby lexer;
    ScintillaAPI* api = new ScintillaAPI(&lexer);
    QString current = "surge";   // what use_track set above the cursor

    Api()
    {
        api->setTrackResolver([this]() { return current; });
        api->updateTracks(QStringList{ "surge", "verb" });
        api->updateTrackParams("surge", { param("Cutoff", "Filter", 0, 1, 0.25),
                                          param("Filter 1 Resonance", "Filter", 0, 1, 0.5),
                                          param("A \"B\"", "", 0, 1, 0),
                                          param("Sustain", "Amp EG", 0, 1, 0.7) });
        api->updateTrackParams("verb", { param("Mix", "", 0, 100, 30, "ValhallaSupermassive"),
                                         param("Mix", "", 0, 1, 0, "Surge XT Effects") });
    }

    QList<CompletionItem> items(const QString& code) const
    {
        return api->completionsFor(SonicPi::lineToContext(code, code.length()));
    }

    QStringList complete(const QString& code) const
    {
        QStringList out;
        for (const CompletionItem& it : items(code)) out << it.text;
        return out;
    }

    QString kind(const QString& code) const
    {
        const auto its = items(code);
        return its.isEmpty() ? QString() : its.first().kind;
    }
};

} // namespace

TEST_CASE("the key rule: lower case, underscores between, a symbol Ruby can take", "[completion][track]")
{
    CHECK(SonicPi::trackParamKey("Filter 1 Cutoff") == "filter_1_cutoff");
    CHECK(SonicPi::trackParamKey("  A/B  Mix--Wet ") == "a_b_mix_wet");
    CHECK(SonicPi::trackParamKey("1 Osc") == "_1_osc");
    CHECK(SonicPi::trackParamKey("Über") == "ber");
    CHECK(SonicPi::trackParamKey("---") == "");
}

TEST_CASE("a track verb's opts are its own, then the plugins' parameters as keys", "[completion][track]")
{
    Api t;
    const QStringList note = t.complete("track_midi :e3, ");
    CHECK(t.kind("track_midi :e3, ") == "opt");
    CHECK(note.contains("sustain:"));           // the verb's own
    CHECK(note.contains("cutoff:"));            // the plugin's
    CHECK(note.contains("filter_1_resonance:"));
    CHECK(note.contains("a_b:"));
    // The plugin's "Sustain" is shadowed by track_midi's sustain: — offered once.
    CHECK(note.count("sustain:") == 1);
    // Own opts first, parameters after, in chain order.
    CHECK(note.indexOf("sustain:") < note.indexOf("cutoff:"));
    CHECK(note.indexOf("cutoff:") < note.indexOf("filter_1_resonance:"));

    CHECK(t.complete("with_send :verb, ").contains("mix:"));
    CHECK(t.complete("with_send :verb, ").contains("amp:"));
    CHECK(t.complete("live_track :surge, ").contains("cutoff:"));
    CHECK(t.complete("track_control ").contains("cutoff:"));
    CHECK(t.complete("track_control ").contains("on:"));
    CHECK(t.complete("track_midi_note_on :e3, ").contains("cutoff:"));
    CHECK(t.complete("track_midi_all_notes_off ").contains("track:"));
    CHECK(t.complete("track_midi_all_notes_off ").contains("cutoff:"));
}

TEST_CASE("the track is use_track's, unless track: names another", "[completion][track]")
{
    Api t;
    t.current = "verb";
    CHECK(t.complete("track_midi :e3, ").contains("mix:"));
    CHECK_FALSE(t.complete("track_midi :e3, ").contains("cutoff:"));
    // track: wins, before or after the cursor's slot.
    CHECK(t.complete("track_midi :e3, track: :surge, ").contains("cutoff:"));
    CHECK(t.complete("track_control track: :surge, ").contains("cutoff:"));
    CHECK(t.complete("live_track track: :surge, ").contains("cutoff:"));
    // No use_track and no track: — only the verb's own opts, no plugin's.
    t.current.clear();
    CHECK(t.complete("track_midi :e3, ").contains("sustain:"));
    CHECK_FALSE(t.complete("track_midi :e3, ").contains("cutoff:"));
    // The name in front still counts for the two verbs that take one.
    CHECK(t.complete("with_send :verb, ").contains("mix:"));
    CHECK(t.complete("live_track :surge, ").contains("cutoff:"));
}

TEST_CASE("track: takes a track name", "[completion][track]")
{
    Api t;
    CHECK(t.complete("track_midi :e3, track: ") == QStringList{ ":surge", ":verb" });
    CHECK(t.complete("track_control track: ") == QStringList{ ":surge", ":verb" });
    CHECK(t.complete("use_track ") == QStringList{ ":surge", ":verb" });
    CHECK(t.complete("with_track ") == QStringList{ ":surge", ":verb" });
}

TEST_CASE("a name two plugins share: the bare key is the first plugin's, the second has its own",
          "[completion][track]")
{
    Api t;
    const auto its = t.items("with_send :verb, ");
    int bare = 0, prefixed = 0;
    for (const CompletionItem& it : its)
    {
        if (it.text == "mix:")
        {
            ++bare;
            CHECK(it.summary == "ValhallaSupermassive · 0 – 100");
            CHECK(it.tag == "ValhallaSupermassive");
        }
        if (it.text == "surge_xt_effects_mix:")
        {
            ++prefixed;
            CHECK(it.summary == "Surge XT Effects · 0 – 1");
            CHECK(it.tag == "Surge XT Effects");
        }
    }
    CHECK(bare == 1);
    CHECK(prefixed == 1);
    // The bare key comes before the one that had to say whose it is.
    const QStringList list = t.complete("with_send :verb, ");
    CHECK(list.indexOf("mix:") < list.indexOf("surge_xt_effects_mix:"));
    // The prefixed key's value is that plugin's range, not the first's.
    const auto v = t.items("with_send :verb, surge_xt_effects_mix: ");
    REQUIRE(v.size() == 1);
    CHECK(v.first().rmax == 1);
}

TEST_CASE("a track with one plugin does not name it on every key", "[completion][track]")
{
    Api t;
    for (const CompletionItem& it : t.items("track_midi :e3, "))
        CHECK(it.tag.isEmpty());
}

TEST_CASE("the value after a parameter key is a slider over the plugin's range", "[completion][track]")
{
    Api t;
    const auto its = t.items("with_send :verb, mix: ");
    REQUIRE(its.size() == 1);
    CHECK(its.first().slider);
    CHECK(its.first().rmin == 0);
    CHECK(its.first().rmax == 100);
    CHECK(its.first().rdefault == 30);   // where the plugin is now
    // A typed value is where the slider starts.
    CHECK(t.items("with_send :verb, mix: 80").first().rdefault == 80);
    // The verb's own opt keeps its own range, not a plugin's.
    const auto amp = t.items("with_send :verb, amp: ");
    if (!amp.isEmpty() && amp.first().slider) CHECK(amp.first().rmax != 100);
}

TEST_CASE("an opening quote in track_control's parameter slot asks for the plugin's spelling",
          "[completion][track]")
{
    Api t;
    CHECK(t.complete("track_control \"")
          == QStringList{ "\"Cutoff\"", "\"Filter 1 Resonance\"", "\"A \\\"B\\\"\"", "\"Sustain\"" });
    CHECK(t.kind("track_control \"") == "param");
    CHECK(t.complete("track_control track: :verb, \"") == QStringList{ "\"Mix\"" });
    // Still the slot while the string is being typed, spaces included.
    CHECK(t.complete("track_control \"Filter 1 R").contains("\"Filter 1 Resonance\""));
}

TEST_CASE("the popup's summary for a parameter is its group and range", "[completion][track]")
{
    Api t;
    t.current = "verb";
    const auto items = t.items("track_control \"");
    REQUIRE(items.size() == 1);
    CHECK(items.first().summary == "ValhallaSupermassive · 0 – 100");
    t.current = "surge";
    for (const CompletionItem& it : t.items("track_midi :e3, "))
        if (it.text == "filter_1_resonance:") CHECK(it.summary == "Filter · 0 – 1");
}

TEST_CASE("after the name comes the value, which is not a list", "[completion][track]")
{
    Api t;
    CHECK_FALSE(t.complete("track_control \"Cutoff\", ").contains("\"Cutoff\""));
}

TEST_CASE("a track that goes takes its parameters with it", "[completion][track]")
{
    Api t;
    t.api->updateTracks(QStringList{ "verb" });
    // What is left is the documented opt (on:), not a parameter of a track gone.
    CHECK_FALSE(t.complete("track_control ").contains("cutoff:"));
    CHECK_FALSE(t.complete("track_control \"").contains("\"Cutoff\""));
    CHECK(t.complete("track_control track: :verb, \"") == QStringList{ "\"Mix\"" });
}
