// Tests for the arg-position-aware completion resolver. Given the reduced tokens
// (last element = the partial) and an arg-kinds table (function -> kind per
// positional argument, which qt-doc.rb will generate from the language metadata),
// it returns the kind expected at the caret's argument slot. This generalises the
// old last/first heuristics into proper (function, argIndex) resolution and is the
// single consumer the generated metadata feeds.

#include <catch2/catch_test_macros.hpp>

#include "utils/completion_context.h"

using SonicPi::ArgKind;
using SonicPi::ArgKindTable;
using SonicPi::resolveArgKind;

static QStringList ctx(std::initializer_list<const char*> xs)
{
    QStringList l;
    for (auto* x : xs) l << QString::fromUtf8(x);
    return l;
}

static ArgKindTable fixture()
{
    return {
        { "sample",     { ArgKind::Sample } },
        { "cue",        { ArgKind::CuePath } },
        { "play",       { ArgKind::Note } },
        { "scale",      { ArgKind::Note, ArgKind::Scale } },
        { "with_fx",    { ArgKind::Fx } },
        { "link_audio", { ArgKind::LinkAudioPeer, ArgKind::LinkAudioChannel } },
    };
}

TEST_CASE("first-argument positions resolve to that arg's kind", "[completion][argkind]")
{
    const ArgKindTable t = fixture();
    CHECK(resolveArgKind(ctx({"sample", ""}),  t) == ArgKind::Sample);
    CHECK(resolveArgKind(ctx({"cue", ""}),     t) == ArgKind::CuePath);
    CHECK(resolveArgKind(ctx({"play", ""}),    t) == ArgKind::Note);
    CHECK(resolveArgKind(ctx({"with_fx", ""}), t) == ArgKind::Fx);
    CHECK(resolveArgKind(ctx({"scale", ""}),   t) == ArgKind::Note);   // arg0 = tonic
}

TEST_CASE("later positional args resolve by index", "[completion][argkind]")
{
    const ArgKindTable t = fixture();
    CHECK(resolveArgKind(ctx({"scale", "60", ""}), t)        == ArgKind::Scale);          // arg1
    CHECK(resolveArgKind(ctx({"link_audio", ""}), t)         == ArgKind::LinkAudioPeer);  // arg0
    CHECK(resolveArgKind(ctx({"link_audio", "peerA", ""}), t) == ArgKind::LinkAudioChannel); // arg1
}

TEST_CASE("unknown function is None (false-positive guard)", "[completion][argkind]")
{
    const ArgKindTable t = fixture();
    CHECK(resolveArgKind(ctx({"puts", ""}), t)  == ArgKind::None);
    CHECK(resolveArgKind(ctx({""}), t)          == ArgKind::None);
    CHECK(resolveArgKind(QStringList(), t)      == ArgKind::None);
}

TEST_CASE("a position past the function's positional args is None", "[completion][argkind]")
{
    const ArgKindTable t = fixture();
    // sample takes one value arg; a second positional slot is None (opts handled
    // elsewhere).
    CHECK(resolveArgKind(ctx({"sample", ":bd", ""}), t) == ArgKind::None);
}

TEST_CASE("once an opt key appears the caret is in opt-land, not a value slot", "[completion][argkind]")
{
    const ArgKindTable t = fixture();
    CHECK(resolveArgKind(ctx({"play", "60", "amp:", ""}), t) == ArgKind::None);
    // an opt before the would-be value arg also disqualifies it
    CHECK(resolveArgKind(ctx({"scale", "amp:", ""}), t)      == ArgKind::None);
}

TEST_CASE("intermediate empty tokens are ignored", "[completion][argkind]")
{
    const ArgKindTable t = fixture();
    CHECK(resolveArgKind(ctx({"scale", "60", "", ""}), t) == ArgKind::Scale);
}
