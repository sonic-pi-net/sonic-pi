// Owner-scoped opt metadata. Sonic Pi reuses opt names across synths and FX and
// they don't mean the same thing there: depth: is flange depth on :flanger and
// carrier depth on :fm, wave: is 0..2 on :tb303 but 0..4 on :tremolo. The global
// per-opt-name tables can only hold one owner's version (the first registered),
// so anything that differs needs an owner-scoped entry generated alongside it.
//
// Two halves: the generated table pins what qt-doc.rb emits, and the ScintillaAPI
// cases drive the completion popup with the real (wrong-for-most-owners) global
// values loaded, so a regression puts the wrong docs back in front of the user.

#include <catch2/catch_test_macros.hpp>

#include <Qsci/qscilexerruby.h>

#include "utils/completion_optowners.gen.h"
#include "utils/scintilla_api.h"

using SonicPi::OptOwnerTable;

namespace {

// The values the global tables actually hold today: whichever owner qt-doc.rb
// reached first wins the opt name. Tests set these so a fix that only works
// because the global happens to be right would still fail.
const char* kGlobalWaveDoc = "Control waveform used to modulate the amplitude. 0=saw, 1=pulse, 2=tri, 3=sine";
const char* kGlobalDepthDoc = "<p>Default: <code>5</code></p><p>Flange depth - greater depths produce a more prominent effect.</p>";

// A ScintillaAPI wired like the GUI's: global opt metadata as ruby_help.h
// registers it, plus the synth/FX arg lists the owner lookup needs. The API
// parents itself to the lexer, which owns it from there.
struct Api
{
    QsciLexerRuby lexer;
    ScintillaAPI* api = new ScintillaAPI(&lexer);

    Api()
    {
        api->setOptOptions("wave:", QStringList{ "0", "1", "2", "3" });
        api->setDoc("wave:", QString::fromUtf8(kGlobalWaveDoc));
        api->setDoc("depth:", QString::fromUtf8(kGlobalDepthDoc));
        api->addSynthArgs(":tb303", QStringList{ "wave:", "cutoff:", "note:" });
        api->addSynthArgs(":fm", QStringList{ "depth:", "divisor:", "note:" });
        api->addFXArgs(":flanger", QStringList{ "wave:", "depth:", "phase:" });
        api->addFXArgs(":tremolo", QStringList{ "wave:", "depth:", "phase:" });
        api->addFXArgs(":slicer", QStringList{ "wave:", "phase:" });
    }

    // The completion candidates for a context split the way QScintilla hands it
    // to the API: each token, with the partial word last.
    QList<CompletionItem> completionsFor(const QStringList& context) const
    {
        return api->completionsFor(context);
    }
};

QStringList textsOf(const QList<CompletionItem>& items)
{
    QStringList out;
    for (const CompletionItem& it : items)
        out << it.text;
    return out;
}

const CompletionItem* itemNamed(const QList<CompletionItem>& items, const QString& text)
{
    for (const CompletionItem& it : items)
        if (it.text == text) return &it;
    return nullptr;
}

} // namespace

TEST_CASE("generated owner table pins enum opts whose values differ per owner", "[completion][optowner][gen]")
{
    const OptOwnerTable t = SonicPi::generatedOptOwners();

    // :tb303 accepts 0..2 (v_one_of [0,1,2]); the global list holds :slicer's 0..3.
    CHECK(t.options(":tb303", "wave:", QStringList{ "0", "1", "2", "3" })
          == QStringList{ "0", "1", "2" });
    // :flanger and :tremolo reach 4 (cubic), which the global list can't offer.
    CHECK(t.options(":flanger", "wave:", QStringList{}) == QStringList{ "0", "1", "2", "3", "4" });
    CHECK(t.options(":tremolo", "wave:", QStringList{}) == QStringList{ "0", "1", "2", "3", "4" });
    // An owner that agrees with the global keeps using it — no entry emitted.
    CHECK(t.options(":slicer", "wave:", QStringList{ "0", "1", "2", "3" })
          == QStringList{ "0", "1", "2", "3" });
}

TEST_CASE("generated owner table carries per-owner opt docs", "[completion][optowner][gen]")
{
    const OptOwnerTable t = SonicPi::generatedOptOwners();

    const QString fmDepth = t.doc(":fm", "depth:", QString());
    REQUIRE_FALSE(fmDepth.isEmpty());
    CHECK(fmDepth.contains("carrier"));
    CHECK_FALSE(fmDepth.contains("Flange"));
    CHECK(fmDepth.contains("<code>1</code>"));   // :fm's default, not :flanger's 5

    const QString tremoloDepth = t.doc(":tremolo", "depth:", QString());
    CHECK(tremoloDepth.contains("Tremolo depth"));
    CHECK(tremoloDepth.contains("<code>0.5</code>"));

    // room: is a 0..1 mix on :reverb (the global winner) but metres elsewhere.
    CHECK(t.doc(":gverb", "room:", QString()).contains("metres"));
    CHECK(t.doc(":dark_ambience", "room:", QString()).contains("metres"));

    // detune: is 0.1 MIDI notes on :dsaw (global) and -11 on :sc808_snare.
    CHECK(t.doc(":sc808_snare", "detune:", QString()).contains("<code>-11</code>"));

    // An owner that matches the global emits nothing and falls back.
    CHECK(t.doc(":reverb", "room:", QString::fromUtf8("global")) == QString::fromUtf8("global"));
}

TEST_CASE("generated owner table carries the per-owner slider ranges", "[completion][optowner][gen]")
{
    const OptOwnerTable t = SonicPi::generatedOptOwners();

    REQUIRE(t.hasRange(":gverb", "room:"));
    CHECK(t.range(":gverb", "room:").lo == 1.0);
    CHECK(t.range(":gverb", "room:").def == 10.0);
    REQUIRE(t.hasRange(":dark_ambience", "room:"));
    CHECK(t.range(":dark_ambience", "room:").hi == 300.0);
    CHECK_FALSE(t.hasRange(":reverb", "room:"));
}

TEST_CASE("owner lookups fall back to the global value", "[completion][optowner]")
{
    OptOwnerTable t;
    t.setDoc(":fm", "depth:", QString::fromUtf8("carrier depth"));
    t.setOptions(":tb303", "wave:", QStringList{ "0", "1", "2" });

    CHECK(t.doc(":fm", "depth:", QString::fromUtf8("global")) == QString::fromUtf8("carrier depth"));
    CHECK(t.doc(":tremolo", "depth:", QString::fromUtf8("global")) == QString::fromUtf8("global"));
    CHECK(t.doc(QString(), "depth:", QString::fromUtf8("global")) == QString::fromUtf8("global"));
    CHECK(t.options(":tb303", "wave:", QStringList{ "9" }) == QStringList{ "0", "1", "2" });
    CHECK(t.options(":slicer", "wave:", QStringList{ "9" }) == QStringList{ "9" });
}

TEST_CASE("the popup never offers an enum value the owner rejects", "[completion][optowner]")
{
    Api api;

    // synth :tb303, wave: <caret> — the engine rejects 3 (v_one_of [0,1,2]).
    const QStringList offered = textsOf(api.completionsFor({ "synth", ":tb303", "wave:", "" }));
    CHECK(offered == QStringList{ "0", "1", "2" });

    // with_fx :flanger, wave: <caret> — 4 (cubic) is valid and must be reachable.
    CHECK(textsOf(api.completionsFor({ "with_fx", ":flanger", "wave:", "" }))
          == QStringList{ "0", "1", "2", "3", "4" });

    // An owner that agrees with the global is unaffected.
    CHECK(textsOf(api.completionsFor({ "with_fx", ":slicer", "wave:", "" }))
          == QStringList{ "0", "1", "2", "3" });
}

TEST_CASE("enum value labels come from the owner's own doc", "[completion][optowner]")
{
    Api api;

    // :tb303 documents "2 triangle"; the global (:slicer) doc says "2=tri".
    const QList<CompletionItem> vals = api.completionsFor({ "synth", ":tb303", "wave:", "" });
    const CompletionItem* two = itemNamed(vals, "2");
    REQUIRE(two != nullptr);
    CHECK(two->summary == QString::fromUtf8("triangle"));
    CHECK(two->doc.contains("Wave type"));
}

TEST_CASE("completing an opt name shows that synth's docs, not another owner's", "[completion][optowner]")
{
    Api api;

    // synth :fm, dep<caret> — the reported RC-4 bug: :flanger's doc for :fm's opt.
    const QList<CompletionItem> synthOpts = api.completionsFor({ "synth", ":fm", "dep" });
    const CompletionItem* depth = itemNamed(synthOpts, "depth:");
    REQUIRE(depth != nullptr);
    CHECK(depth->doc.contains("carrier"));
    CHECK_FALSE(depth->doc.contains("Flange"));

    const QList<CompletionItem> fxOpts = api.completionsFor({ "with_fx", ":tremolo", "dep" });
    const CompletionItem* tremoloDepth = itemNamed(fxOpts, "depth:");
    REQUIRE(tremoloDepth != nullptr);
    CHECK(tremoloDepth->doc.contains("Tremolo depth"));

    // :flanger owns the global, so its entry reads the same either way.
    const QList<CompletionItem> flangerOpts = api.completionsFor({ "with_fx", ":flanger", "dep" });
    const CompletionItem* flangerDepth = itemNamed(flangerOpts, "depth:");
    REQUIRE(flangerDepth != nullptr);
    CHECK(flangerDepth->doc.contains("Flange depth"));
}
