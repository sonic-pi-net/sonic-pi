// Tests for documented-opt completion on plain lang functions (live_audio,
// live_loop, use_bpm, …) — the ones with no synth/fx/sample-specific branch.
// resolveFnOpts maps real editor text to the governing function's documented
// opt keys; argListHasOptKey marks opt-land so the engine can stay quiet there
// instead of offering the global function list. The generated-table test pins
// the qt-doc.rb emission (completion_fnopts.gen.h) to the lang docs.

#include <catch2/catch_test_macros.hpp>

#include "utils/completion_context.h"
#include "utils/completion_fnopts.gen.h"

using SonicPi::lineToContext;

static const SonicPi::FnOptsTable kTable = {
    { "live_audio", { "input:", "stereo:" } },
    { "live_loop",  { "init:", "sync:", "sync_bpm:", "seed:", "delay:" } },
};

// Opts offered for `code` with the caret at its end — the same reduction the
// editor uses (apiContext → lineToContext) feeding resolveFnOpts.
static QStringList optsAt(const char* code)
{
    const QString s = QString::fromUtf8(code);
    return SonicPi::resolveFnOpts(lineToContext(s, s.length()), kTable);
}

static bool optKeySlotAt(const char* code)
{
    const QString s = QString::fromUtf8(code);
    return SonicPi::atOptKeySlot(lineToContext(s, s.length()));
}

TEST_CASE("documented opts complete after the function's first argument", "[completion][fnopts]")
{
    const QStringList la = { "input:", "stereo:" };
    CHECK(optsAt("live_audio :hello, ")             == la);
    CHECK(optsAt("live_audio :hello, inp")          == la);   // partial rides along
    CHECK(optsAt("live_audio :hello, input: 2, st") == la);   // later opts still complete
    CHECK(optsAt("live_loop :foo, sy")              == kTable.value("live_loop"));
}

TEST_CASE("the name slot and value slots offer no opts", "[completion][fnopts]")
{
    CHECK(optsAt("live_audio ").isEmpty());                // first arg = the name
    CHECK(optsAt("live_audio inp").isEmpty());             // still the name slot
    CHECK(optsAt("live_audio :hello, input: ").isEmpty()); // opt value slot
}

TEST_CASE("unknown functions resolve no opts", "[completion][fnopts]")
{
    CHECK(optsAt("play 60, ").isEmpty());
    CHECK(optsAt("puts ").isEmpty());
}

TEST_CASE("resolution is robust to expression prefixes", "[completion][fnopts]")
{
    CHECK(optsAt("x = live_audio :mic, ") == QStringList({ "input:", "stereo:" }));
}

TEST_CASE("opt-key slots are detected so the engine can suppress the function fallback", "[completion][fnopts]")
{
    CHECK(optKeySlotAt("my_fn 1, foo: 1, bar"));
    CHECK(optKeySlotAt("my_fn foo: 1, "));
    CHECK_FALSE(optKeySlotAt("my_fn :sym, "));   // a symbol arg is not an opt key
    CHECK_FALSE(optKeySlotAt("play 60, "));
    CHECK_FALSE(optKeySlotAt("play "));
}

TEST_CASE("an opt's value slot still completes calls", "[completion][fnopts]")
{
    // `delay: rrand(1, 2)` — suppressing the whole arg list once any opt key
    // appeared would kill function completion in the value position.
    CHECK_FALSE(optKeySlotAt("my_fn 1, delay: rran"));
    CHECK_FALSE(optKeySlotAt("live_loop :a, delay: "));
    CHECK_FALSE(optKeySlotAt("my_fn foo: "));
}

// End-to-end over the REAL generated table: editor text -> offered opt keys.
static QStringList realOptsAt(const char* code)
{
    const QString s = QString::fromUtf8(code);
    return SonicPi::resolveFnOpts(lineToContext(s, s.length()), SonicPi::generatedFnOpts());
}

TEST_CASE("real lang functions complete their documented opts", "[completion][fnopts][gen]")
{
    CHECK(realOptsAt("live_audio :hello, ")      == QStringList({ "input:", "stereo:" }));
    CHECK(realOptsAt("live_loop :foo, ").contains("sync:"));
    CHECK(realOptsAt("use_bpm :midi, ")          == QStringList({ "quantum:" }));
    CHECK(realOptsAt("sync :foo, ")              == QStringList({ "bpm_sync:" }));
    CHECK(realOptsAt("use_sample_bpm :loop_amen, ") == QStringList({ "num_beats:" }));
}

// Opts only start after the function's first argument, so a function documenting
// NO positional args (in_thread, with_swing — opts and a block only) never
// reaches them. Pinned as the known limit of the current rule, not as desired
// behaviour.
TEST_CASE("zero-positional-arg functions do not reach their opts", "[completion][fnopts][gen]")
{
    REQUIRE(SonicPi::generatedFnOpts().value("in_thread").contains("name:"));
    CHECK(realOptsAt("in_thread ").isEmpty());
    CHECK(realOptsAt("with_swing ").isEmpty());
}

TEST_CASE("generated fn-opts table matches the lang docs", "[completion][fnopts][gen]")
{
    const SonicPi::FnOptsTable t = SonicPi::generatedFnOpts();
    CHECK(t.value("live_audio") == QStringList({ "input:", "stereo:" }));
    CHECK(t.value("live_loop").contains("sync:"));
    CHECK(t.value("in_thread").contains("name:"));
    CHECK(t.value("use_bpm").contains("quantum:"));
    // cue's documented opts are placeholders (your_key: …) — arbitrary user
    // keys, not completable vocabulary.
    CHECK_FALSE(t.contains("cue"));
    CHECK_FALSE(t.contains("puts"));
}
