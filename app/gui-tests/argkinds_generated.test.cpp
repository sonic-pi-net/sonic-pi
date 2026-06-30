// Contract test for the arg-kinds table generated from the Sonic Pi function
// metadata (qt-doc.rb -> completion_argkinds.gen.h). Pins the generation so the
// Ruby `arg_kinds:` tags and the codegen can't silently drift from what the
// completion engine expects.

#include <catch2/catch_test_macros.hpp>

#include "utils/completion_argkinds.gen.h"

using SonicPi::ArgKind;

TEST_CASE("generated arg-kinds classify the value/name-slot functions", "[completion][argkind][gen]")
{
    const SonicPi::ArgKindTable t = SonicPi::generatedArgKinds();
    CHECK(t.value("sample")     == QVector<ArgKind>{ ArgKind::Sample });
    CHECK(t.value("cue")        == QVector<ArgKind>{ ArgKind::CuePath });
    CHECK(t.value("sync")       == QVector<ArgKind>{ ArgKind::CuePath });
    CHECK(t.value("with_fx")    == QVector<ArgKind>{ ArgKind::Fx });
    CHECK(t.value("use_synth")  == QVector<ArgKind>{ ArgKind::Synth });
    CHECK(t.value("with_synth") == QVector<ArgKind>{ ArgKind::Synth });
    CHECK(t.value("synth")      == QVector<ArgKind>{ ArgKind::Synth });
    CHECK(t.value("get")        == QVector<ArgKind>{ ArgKind::CuePath });
    CHECK(t.value("set")        == QVector<ArgKind>{ ArgKind::CuePath });
    CHECK(t.value("scale")      == QVector<ArgKind>{ ArgKind::None, ArgKind::Scale });
    CHECK(t.value("chord")      == QVector<ArgKind>{ ArgKind::None, ArgKind::Chord });
    CHECK(t.value("link_audio") == QVector<ArgKind>{ ArgKind::LinkAudioPeer, ArgKind::LinkAudioChannel });

    // a plain function takes no value/name-slot args
    CHECK_FALSE(t.contains("puts"));
}

TEST_CASE("the whole sample family completes sample names", "[completion][argkind][gen]")
{
    const SonicPi::ArgKindTable t = SonicPi::generatedArgKinds();
    for (const char* fn : { "sample", "sample_info", "sample_duration", "sample_buffer",
                            "sample_loaded?", "load_sample", "load_samples",
                            "use_sample_bpm", "with_sample_bpm" })
        CHECK(t.value(QString::fromUtf8(fn)) == QVector<ArgKind>{ ArgKind::Sample });
}
