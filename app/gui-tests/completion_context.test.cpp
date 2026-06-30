// Tests for the shared completion line scanner — the single source of truth for
// "is the caret inside a string / comment, and how deep in brackets". It replaces
// the two divergent ad-hoc scanners (the suppression scan in updateCompletion and
// the modifier-strip scan in apiContext) and fixes escaped-quote tracking.

#include <catch2/catch_test_macros.hpp>

#include "utils/completion_context.h"

using SonicPi::LineScan;
using SonicPi::scanLineToCaret;

TEST_CASE("plain code is neither string nor comment", "[completion][scan]")
{
    const LineScan s = scanLineToCaret("play 60", 7);
    CHECK_FALSE(s.inString);
    CHECK_FALSE(s.inComment);
    CHECK(s.bracketDepth == 0);
    CHECK(s.openQuoteCol == -1);
}

TEST_CASE("caret inside an unterminated double-quoted string", "[completion][scan]")
{
    const QString line = "cue \"/link";   // cue "/link  (caret at end)
    const LineScan s = scanLineToCaret(line, line.length());
    CHECK(s.inString);
    CHECK(s.quote == QChar('"'));
    CHECK(s.openQuoteCol == 4);            // the opening quote position
    CHECK_FALSE(s.inComment);
}

TEST_CASE("single-quoted string is tracked too", "[completion][scan]")
{
    const QString line = "sample 'loop";
    const LineScan s = scanLineToCaret(line, line.length());
    CHECK(s.inString);
    CHECK(s.quote == QChar('\''));
}

TEST_CASE("a closed string leaves the caret back in code", "[completion][scan]")
{
    const QString line = "cue \"x\" ";       // cue "x"  (caret after the space)
    const LineScan s = scanLineToCaret(line, line.length());
    CHECK_FALSE(s.inString);
    CHECK(s.openQuoteCol == -1);
}

TEST_CASE("escaped quote does NOT close the string (the bug fix)", "[completion][scan]")
{
    const QString line = "cue \"a\\\"b";     // cue "a\"b  (caret at end, still open)
    const LineScan s = scanLineToCaret(line, line.length());
    CHECK(s.inString);
    CHECK(s.quote == QChar('"'));
    CHECK(s.openQuoteCol == 4);
}

TEST_CASE("a '#' outside a string starts a comment", "[completion][scan]")
{
    const QString line = "play 60 # foo";
    const LineScan s = scanLineToCaret(line, line.length());
    CHECK(s.inComment);
    CHECK_FALSE(s.inString);
}

TEST_CASE("a '#' INSIDE a string is not a comment", "[completion][scan]")
{
    const QString line = "cue \"a#b";        // cue "a#b
    const LineScan s = scanLineToCaret(line, line.length());
    CHECK(s.inString);
    CHECK_FALSE(s.inComment);
}

TEST_CASE("unclosed bracket increases depth; balanced returns to zero", "[completion][scan]")
{
    CHECK(scanLineToCaret("play (scale 60,", 15).bracketDepth == 1);
    CHECK(scanLineToCaret("foo(bar)", 8).bracketDepth == 0);
    CHECK(scanLineToCaret("a([{", 4).bracketDepth == 3);
}

TEST_CASE("brackets inside a string do not count", "[completion][scan]")
{
    const QString line = "cue \"a(b[c";
    const LineScan s = scanLineToCaret(line, line.length());
    CHECK(s.inString);
    CHECK(s.bracketDepth == 0);
}

TEST_CASE("scan stops at the caret column, ignoring text after it", "[completion][scan]")
{
    const QString line = "cue \"x\" play";   // caret placed INSIDE the first string
    const LineScan s = scanLineToCaret(line, 6); // between the x and the closing quote
    CHECK(s.inString);
}
