// Integration test for completion detection: hardcoded editor buffer text +
// cursor run through the REAL pipeline the GUI uses — lineToContext (the editor's
// token reduction) then resolveArgKind over the generated metadata table. Both are
// pure, so the detection the user sees is exercised headlessly, no widget/typing.

#include <catch2/catch_test_macros.hpp>

#include "utils/completion_context.h"
#include "utils/completion_argkinds.gen.h"

using SonicPi::ArgKind;

// Detect the completion kind for `code` with the caret at its end — the same path
// apiContext + updateAutoCompletionList take in the editor.
static ArgKind detect(const char* code)
{
    const QString s = QString::fromUtf8(code);
    return SonicPi::resolveArgKind(SonicPi::lineToContext(s, s.length()),
                                   SonicPi::generatedArgKinds());
}

TEST_CASE("detects value/name slots from real editor text", "[completion][detect]")
{
    CHECK(detect("sample ")    == ArgKind::Sample);
    CHECK(detect("cue ")       == ArgKind::CuePath);
    CHECK(detect("with_fx ")   == ArgKind::Fx);
    CHECK(detect("use_synth ") == ArgKind::Synth);
    CHECK(detect("scale 60, ") == ArgKind::Scale);
    CHECK(detect("chord 60, ") == ArgKind::Chord);
}

TEST_CASE("detection is robust to assignment/expression prefixes", "[completion][detect]")
{
    // `dur = sample_duration :loop_amen` — the function isn't token 0.
    CHECK(detect("dur = sample_duration ") == ArgKind::Sample);
    CHECK(detect("x = scale 60, ")         == ArgKind::Scale);
    CHECK(detect("foo = with_fx ")         == ArgKind::Fx);
}

TEST_CASE("detection resolves nested calls to the innermost", "[completion][detect]")
{
    CHECK(detect("play (scale 60, ") == ArgKind::Scale);
    CHECK(detect("puts (sample ")    == ArgKind::Sample);
}

TEST_CASE("opt positions and arbitrary calls are not value slots", "[completion][detect]")
{
    CHECK(detect("sample :bd, amp: ") == ArgKind::None);   // in opts, not a sample
    CHECK(detect("play 60, ")         == ArgKind::None);   // not a value-slot fn
    CHECK(detect("puts ")             == ArgKind::None);
}

TEST_CASE("track functions detect the track slot and the note slot", "[completion][detect]")
{
    CHECK(detect("live_track ")             == ArgKind::Track);
    CHECK(detect("with_send ")              == ArgKind::Track);
    CHECK(detect("use_track ")              == ArgKind::Track);
    CHECK(detect("with_track ")             == ArgKind::Track);
    CHECK(detect("track_midi ")             == ArgKind::Note);
    CHECK(detect("track_midi_note_on ")     == ArgKind::Note);
    CHECK(detect("track_midi :e3, ")        == ArgKind::None);    // velocity, a number
    CHECK(detect("track_control ")          == ArgKind::None);    // a parameter name
    CHECK(detect("live_track :surge, amp: ") == ArgKind::None);   // in opts
}
