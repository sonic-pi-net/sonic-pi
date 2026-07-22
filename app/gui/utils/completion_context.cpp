#include "completion_context.h"

#include <QRegularExpression>
#include <QList>

namespace SonicPi {

LineScan scanLineToCaret(const QString& line, int caretCol)
{
    LineScan s;
    int end = caretCol < 0 ? 0 : caretCol;
    if (end > line.length()) end = line.length();
    for (int i = 0; i < end; ++i)
    {
        const QChar c = line[i];
        if (s.inString)
        {
            if (c == '\\') { ++i; continue; }        // escape: skip the next char
            if (c == s.quote) { s.inString = false; s.quote = QChar(); s.openQuoteCol = -1; }
            continue;
        }
        if (c == '#') { s.inComment = true; break; } // comment to end of line
        if (c == '"' || c == '\'') { s.inString = true; s.quote = c; s.openQuoteCol = i; continue; }
        if (c == '(' || c == '[' || c == '{') { ++s.bracketDepth; continue; }
        if (c == ')' || c == ']' || c == '}') { if (s.bracketDepth > 0) --s.bracketDepth; }
    }
    return s;
}

static bool isTokenSeparator(QChar c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' ||
           c == '(' || c == ')' || c == '{' || c == '}' ||
           c == '[' || c == ']' || c == '"' || c == '\'' || c == '#';
}

int tokenEndAtCaret(const QString& line, int caretCol)
{
    int end = caretCol < 0 ? 0 : caretCol;
    if (end > line.length()) end = line.length();
    while (end < line.length() && !isTokenSeparator(line[end])) ++end;
    return end;
}

QStringList lineToContext(const QString& fullLine, int caretCol)
{
    QString line = fullLine.left(tokenEndAtCaret(fullLine, caretCol));

    // A trailing statement modifier or operator (`... if cond`, `unless`, `while`,
    // `until`, `and`, `or`, `then`, `do`, `;`, `&&`, `||`) ends the call's argument
    // list — text after it is a fresh expression. Drop up to the last such
    // top-level token (ignoring ones inside strings or brackets).
    {
        static const QStringList mods = { "if", "unless", "while", "until",
                                          "and", "or", "then", "do" };
        int depth = 0, cut = -1;
        QChar quote;
        for (int i = 0; i < line.length(); ++i)
        {
            const QChar c = line[i];
            if (!quote.isNull()) { if (c == quote) quote = QChar(); continue; }
            if (c == '"' || c == '\'') { quote = c; continue; }
            if (c == '(' || c == '[' || c == '{') { ++depth; continue; }
            if (c == ')' || c == ']' || c == '}') { if (depth > 0) --depth; continue; }
            if (depth != 0) continue;
            if (c == ';') { cut = i + 1; continue; }
            if ((c == '&' && i + 1 < line.length() && line[i + 1] == '&') ||
                (c == '|' && i + 1 < line.length() && line[i + 1] == '|'))
            { cut = i + 2; ++i; continue; }
            const bool startsWord = (c.isLetter() || c == '_') &&
                (i == 0 || !(line[i - 1].isLetterOrNumber() || line[i - 1] == '_'));
            if (!startsWord) continue;
            int j = i;
            while (j < line.length() && (line[j].isLetterOrNumber() || line[j] == '_')) ++j;
            if (mods.contains(line.mid(i, j - i))) cut = j;
            i = j - 1;
        }
        if (cut >= 0)
        {
            while (cut < line.length() && line[cut] == ' ') ++cut;
            line = line.mid(cut);
        }
    }

    // Resolve nested calls to the innermost one: `play (scale ` should complete
    // scale's args, not play's. Drop everything up to the last unclosed bracket.
    QList<int> open;
    for (int i = 0; i < line.length(); ++i) {
        const QChar c = line[i];
        if (c == '(' || c == '[' || c == '{') open.append(i);
        else if ((c == ')' || c == ']' || c == '}') && !open.isEmpty()) open.removeLast();
    }
    if (!open.isEmpty()) {
        // Keep the function name when '(' is its call paren (`scale(60,`); a space
        // before '(' marks a grouping paren — drop it.
        const int innermost = open.last();
        int s = innermost;
        while (s > 0 && (line[s - 1].isLetterOrNumber() || line[s - 1] == '_')) --s;
        const QString fn = line.mid(s, innermost - s);
        line = line.mid(innermost + 1);
        if (!fn.isEmpty()) line = fn + " " + line;
    }

    static const QRegularExpression splitRe("[ ,(){}]+");
    return line.split(splitRe);
}

ArgKind resolveArgKind(const QStringList& context, const ArgKindTable& table)
{
    if (context.isEmpty()) return ArgKind::None;
    // Governing words = non-empty tokens before the partial (the last element).
    QStringList words;
    for (int i = 0; i < context.length() - 1; ++i)
        if (!context[i].isEmpty()) words << context[i];

    // Scan back from the partial: the governing function is the nearest preceding
    // word in the table, and the positional args between it and the partial give
    // the index. An opt key (ends ':') before the partial means we're in opt-land.
    // Scanning back (rather than assuming words[0]) stays robust to prefixes like
    // `dur = sample_duration ...`, where the function isn't the first token.
    int argIndex = 0;
    for (int i = words.length() - 1; i >= 0; --i) {
        if (words[i].endsWith(':')) return ArgKind::None;
        const auto it = table.constFind(words[i]);
        if (it != table.constEnd()) {
            const QVector<ArgKind>& kinds = it.value();
            return argIndex < kinds.size() ? kinds[argIndex] : ArgKind::None;
        }
        ++argIndex;
    }
    return ArgKind::None;
}

bool caretAfterClosedValue(const QString& line, int caretCol)
{
    int i = caretCol < 0 ? 0 : caretCol;
    if (i > line.length()) i = line.length();
    if (i == 0) return false;
    const QChar prev = line[i - 1];
    return prev == ')' || prev == ']' || prev == '}' || prev == '"' || prev == '\'';
}

// Fuzzy subsequence match + rank (fzf / VS Code style). Every char of `pat` must
// appear in `text` in order, case-insensitive — so "empo" matches "tempo". Scoring
// is tiered so the common typing patterns sort intuitively:
//   exact > prefix > substring (word-boundary > mid-word) > boundary subsequence >
//   scattered, with shorter candidates winning ties. The tier dominates a base
//   score that rewards boundary (`: _ - / space . ! ?`) and contiguous-run hits.
bool fuzzyMatch(const QString& pat, const QString& text, int& score)
{
    if (pat.isEmpty()) { score = 0; return true; }

    // Case-insensitive throughout, but without allocating lowercased copies of
    // every candidate on each keystroke: compare chars via QChar::toLower and use
    // Qt::CaseInsensitive for the tier checks. Separators are case-agnostic.
    auto isBoundary = [&](int i) {
        if (i == 0) return true;
        const QChar c = text[i - 1];
        return c == ':' || c == '_' || c == ' ' || c == '-' ||
               c == '/' || c == '.' || c == '!' || c == '?';
    };

    // base: must be a subsequence; reward boundary + contiguous-run hits.
    int ti = 0, pi = 0, run = 0, base = 0, gaps = 0;
    while (ti < text.size() && pi < pat.size()) {
        if (text[ti].toLower() == pat[pi].toLower()) {
            if (isBoundary(ti) && ti > 0) base += 8;
            ++run;
            base += 1 + run * 3;
            ++pi;
        } else {
            if (pi > 0) ++gaps;
            run = 0;
        }
        ++ti;
    }
    if (pi != pat.size()) return false;          // not a subsequence → no match
    base -= gaps * 2;

    // tier dominates the base so the buckets order correctly regardless of length.
    int tier = 0;
    if (QString::compare(text, pat, Qt::CaseInsensitive) == 0) tier = 1000; // exact
    else if (text.startsWith(pat, Qt::CaseInsensitive))        tier = 600;  // prefix
    else {
        const int idx = text.indexOf(pat, 0, Qt::CaseInsensitive); // substring
        if (idx >= 0) tier = isBoundary(idx) ? 400 : 250;
    }

    score = tier + base - text.size();           // shorter wins ties
    return true;
}

} // namespace SonicPi
