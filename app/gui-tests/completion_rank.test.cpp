// Ranking tests for the completion fuzzy matcher. These pin the *relative* order
// the user sees (what ranks above what) for the common typing patterns, which is
// what matters — not absolute scores.

#include <catch2/catch_test_macros.hpp>

#include "utils/completion_context.h"

// score, or -1 if `pat` isn't a fuzzy match of `text`
static int sc(const char* pat, const char* text)
{
    int s = 0;
    return SonicPi::fuzzyMatch(QString::fromUtf8(pat), QString::fromUtf8(text), s) ? s : -1;
}

TEST_CASE("matching basics", "[completion][rank]")
{
    CHECK(sc("", "play") == 0);          // empty matches everything
    CHECK(sc("ply", "play") >= 0);       // subsequence
    CHECK(sc("empo", "tempo") >= 0);     // mid-word subsequence still matches
    CHECK(sc("xyz", "play") == -1);      // not a subsequence
    CHECK(sc("zq", "play") == -1);
}

TEST_CASE("exact match ranks above a longer prefix match", "[completion][rank]")
{
    // typing "play" must put `play` itself above `play_pattern_timed`
    CHECK(sc("play", "play") > sc("play", "play_pattern_timed"));
}

TEST_CASE("shorter candidate wins among prefix matches", "[completion][rank]")
{
    CHECK(sc("pl", "play") > sc("pl", "play_pattern_timed"));
}

TEST_CASE("prefix beats interior substring", "[completion][rank]")
{
    CHECK(sc("loop", "loop_amen") > sc("loop", "ambient_loop"));
}

TEST_CASE("substring beats a scattered subsequence", "[completion][rank]")
{
    CHECK(sc("amen", "loop_amen") > sc("amen", "ambient_noise"));
}

TEST_CASE("word-boundary substring beats a mid-word one", "[completion][rank]")
{
    CHECK(sc("loop", "bass_loop") > sc("loop", "blooper"));
}
