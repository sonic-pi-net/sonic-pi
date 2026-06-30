#ifndef SONICPI_COMPLETION_CONTEXT_H
#define SONICPI_COMPLETION_CONTEXT_H

#include <QString>
#include <QChar>
#include <QStringList>
#include <QHash>
#include <QVector>

namespace SonicPi {

// Result of scanning a line up to the caret. The single source of truth for
// where the caret sits relative to strings/comments/brackets, used by the
// completion engine (replaces the two divergent ad-hoc scanners and fixes
// escaped-quote handling in one place).
struct LineScan {
    bool  inComment = false;   // caret follows an unquoted '#'
    bool  inString  = false;   // caret is inside an unterminated string literal
    QChar quote;               // the opening quote char while inString (else null)
    int   openQuoteCol = -1;   // column of the active string's opening quote, else -1
    int   bracketDepth = 0;    // unclosed (,[,{ depth at the caret
};

// Scan `line` over [0, caretCol), Ruby-style: a backslash in a string escapes the
// next char; '#' outside a string begins a comment to end of line.
LineScan scanLineToCaret(const QString& line, int caretCol);

// Reduce a line (considered up to caretCol) to the completion context token list
// — the same reduction the editor uses: drop up to the last top-level statement
// modifier/operator, resolve to the innermost call, split into tokens (the last
// element is the partial). Pure, so completion detection can be exercised from
// hardcoded editor text + cursor without the GUI.
QStringList lineToContext(const QString& line, int caretCol);

// The semantic kind of a completion position. Derived from the language metadata
// (see ArgKindTable); the completion engine maps these to its keyword lists.
enum class ArgKind {
    None, Note, Sample, CuePath, MidiPort, LinkAudioPeer, LinkAudioChannel,
    Fx, Synth, Scale, Chord, Tuning,
};

// Per-function positional argument kinds: function name -> kind of each positional
// arg. Generated from the Sonic Pi function metadata (qt-doc.rb).
using ArgKindTable = QHash<QString, QVector<ArgKind>>;

// Resolve the kind expected at the caret's positional-argument slot. Returns None
// when the function is absent from the table, the caret is in opt-land (an opt key
// like `amp:` precedes it), or the position is past the function's positional args.
ArgKind resolveArgKind(const QStringList& context, const ArgKindTable& table);

// Fuzzy subsequence match + rank score for completion. Every char of `pat` must
// appear in `text` in order (case-insensitive); returns false otherwise. Higher
// `score` = better match. Used to filter + rank the completion list.
bool fuzzyMatch(const QString& pat, const QString& text, int& score);

} // namespace SonicPi

#endif // SONICPI_COMPLETION_CONTEXT_H
