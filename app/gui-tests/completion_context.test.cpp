// Tests for the shared completion line scanner — the single source of truth for
// "is the caret inside a string / comment, and how deep in brackets". It replaces
// the two divergent ad-hoc scanners (the suppression scan in updateCompletion and
// the modifier-strip scan in apiContext) and fixes escaped-quote tracking.

#include <catch2/catch_test_macros.hpp>

#include "utils/completion_context.h"

using SonicPi::LineScan;
using SonicPi::scanLineToCaret;
using SonicPi::tokenEndAtCaret;
using SonicPi::lineToContext;

// The partial being completed = the last token lineToContext produces. text(line)
// from the editor arrives WITH its trailing newline, so these cases include it.
static QString partialAt(const QString& line, int caret)
{
    const QStringList ctx = lineToContext(line, caret);
    return ctx.isEmpty() ? QString() : ctx.last();
}

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

TEST_CASE("tokenEndAtCaret extends over the token around the caret", "[completion][token]")
{
    // Caret in the middle of a number/word sees the whole token, not just what's
    // before it: `lpf: 7|0` completes 70, not 7.
    CHECK(tokenEndAtCaret("lpf: 70", 6) == 7);   // between 7 and 0
    CHECK(tokenEndAtCaret("lpf: 70", 5) == 7);   // just after the space
    CHECK(tokenEndAtCaret("sample :ambi_choir", 11) == 18);  // inside the symbol
}

TEST_CASE("tokenEndAtCaret stops at separators, including the newline", "[completion][token]")
{
    // The regression: text(linenum) carries a trailing newline. A caret at end of
    // line must NOT extend across it (that made the partial "\n" and shoved the
    // completion span back onto the preceding space, gluing `pan:0.5`).
    CHECK(tokenEndAtCaret("pan: \n", 5) == 5);       // caret after space, before \n
    CHECK(tokenEndAtCaret("pan: 0.5\n", 8) == 8);    // caret after value, before \n
    CHECK(tokenEndAtCaret("pan: \r\n", 5) == 5);     // CRLF too
    CHECK(tokenEndAtCaret("lpf: 70,pan", 7) == 7);   // stops at the comma
}

TEST_CASE("partial is the whole token around a mid-token caret", "[completion][token]")
{
    CHECK(partialAt("lpf: 70", 6) == "70");
    CHECK(partialAt("sample :ambi_choir", 11) == ":ambi_choir");
}

TEST_CASE("end-of-line partial is empty despite the trailing newline", "[completion][token]")
{
    // With the caret right after `pan: ` the partial is empty (a fresh value slot),
    // NOT "\n" — the bug that ate the space after the opt colon.
    CHECK(partialAt("sample :ambi_choir, pan: \n", 25) == "");
    CHECK(partialAt("pan: \n", 5) == "");
    CHECK(partialAt("sample :ambi_choir, pan: 0.5\n", 28) == "0.5");
}

TEST_CASE("caret directly after a closing bracket/quote suppresses completion", "[completion][closed]")
{
    using SonicPi::caretAfterClosedValue;
    // The regression: typing the trailing ')' of `control s, phase_offset: rand(1)`
    // popped the opts list, so Return inserted an opt instead of a newline.
    const QString line = "control s, phase_offset: rand(1)";
    CHECK(caretAfterClosedValue(line, line.length()));

    CHECK(caretAfterClosedValue("play [60, 64]", 13));
    CHECK(caretAfterClosedValue("puts({a: 1})", 12));
    CHECK(caretAfterClosedValue("play \"foo\"", 10));
    CHECK(caretAfterClosedValue("play 'foo'", 10));
}

TEST_CASE("ordinary positions do not read as a closed value", "[completion][closed]")
{
    using SonicPi::caretAfterClosedValue;
    CHECK_FALSE(caretAfterClosedValue("", 0));
    CHECK_FALSE(caretAfterClosedValue("synth :sine, ", 13));   // after ", " - opt slot
    CHECK_FALSE(caretAfterClosedValue("control s, phase_offset: rand(1) ", 33)); // separator typed
    CHECK_FALSE(caretAfterClosedValue("play (scale ", 12));    // inside an open call
    CHECK_FALSE(caretAfterClosedValue("play 60", 7));          // after a plain value
}
