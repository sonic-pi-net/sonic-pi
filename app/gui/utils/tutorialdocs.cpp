//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "tutorialdocs.h"

#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QSet>

namespace SonicPi
{

namespace
{

QJsonArray rootArray(const QByteArray& json, const QString& key)
{
    return QJsonDocument::fromJson(json).object().value(key).toArray();
}

InstrumentOpt optFromJson(const QJsonObject& o)
{
    InstrumentOpt opt;
    opt.name = o.value("name").toString();
    opt.doc = o.value("doc").toString();
    opt.slidable = o.value("slidable").toBool();
    const QJsonValue def = o.value("default");
    opt.numeric = def.isDouble();
    if (opt.numeric)
    {
        opt.defaultNum = def.toDouble();
        opt.defaultText = QString::number(opt.defaultNum);
    }
    else
    {
        opt.defaultText = def.toString();
    }
    opt.hasRange = o.contains("min") && o.contains("max");
    opt.min = o.value("min").toDouble();
    opt.max = o.value("max").toDouble();
    opt.minExcl = o.value("min_excl").toBool();
    opt.maxExcl = o.value("max_excl").toBool();
    return opt;
}

} // namespace

TutorialChapter TutorialDocs::chapterFromJson(const QByteArray& json)
{
    TutorialChapter chapter;
    const QJsonObject root = QJsonDocument::fromJson(json).object();
    chapter.title = root.value("title").toString();
    const QJsonArray blocks = root.value("blocks").toArray();
    for (const QJsonValue& v : blocks)
    {
        const QJsonObject b = v.toObject();
        const QString type = b.value("type").toString();
        TutorialBlock block;
        if (type == "heading")
        {
            block.type = TutorialBlock::Heading;
            block.level = b.value("level").toInt(1);
            block.text = b.value("text").toString();
        }
        else if (type == "prose")
        {
            block.type = TutorialBlock::Prose;
            block.text = b.value("html").toString();
        }
        else if (type == "list")
        {
            block.type = TutorialBlock::List;
            block.ordered = b.value("ordered").toBool();
            for (const QJsonValue& item : b.value("items").toArray())
                block.items << item.toString();
        }
        else if (type == "code")
        {
            block.type = TutorialBlock::Code;
            block.source = b.value("source").toString();
            block.runnable = b.value("runnable").toBool();
        }
        else if (type == "image")
        {
            block.type = TutorialBlock::Image;
            block.path = b.value("path").toString();
            block.text = b.value("alt").toString();
        }
        else
        {
            continue;
        }
        chapter.blocks.append(block);
    }
    return chapter;
}

QVector<InstrumentPage> TutorialDocs::instrumentsFromJson(const QByteArray& json)
{
    QVector<InstrumentPage> pages;
    const QJsonArray arr = rootArray(json, "pages");
    for (const QJsonValue& v : arr)
    {
        const QJsonObject p = v.toObject();
        InstrumentPage page;
        page.key = p.value("key").toString();
        page.title = p.value("title").toString();
        page.docHtml = p.value("doc_html").toString();
        for (const QJsonValue& o : p.value("opts").toArray())
            page.opts.append(optFromJson(o.toObject()));
        pages.append(page);
    }
    return pages;
}

QVector<SampleGroup> TutorialDocs::sampleGroupsFromJson(const QByteArray& json)
{
    QVector<SampleGroup> groups;
    const QJsonArray arr = rootArray(json, "groups");
    for (const QJsonValue& v : arr)
    {
        const QJsonObject g = v.toObject();
        SampleGroup group;
        group.title = g.value("title").toString();
        // Entries are objects ({name, duration}) in current generator output,
        // bare strings in older files — take the name either way.
        for (const QJsonValue& s : g.value("samples").toArray())
            group.samples << (s.isObject() ? s.toObject().value("name").toString()
                                           : s.toString());
        groups.append(group);
    }
    return groups;
}

QVector<LangPage> TutorialDocs::langPagesFromJson(const QByteArray& json)
{
    QVector<LangPage> pages;
    const QJsonArray arr = rootArray(json, "pages");
    for (const QJsonValue& v : arr)
    {
        const QJsonObject p = v.toObject();
        LangPage page;
        page.key = p.value("key").toString();
        page.summary = p.value("summary").toString();
        page.usage = p.value("usage").toString();
        page.docHtml = p.value("doc_html").toString();
        page.introduced = p.value("introduced").toString();
        page.needs = p.value("needs").toString();
        for (const QJsonValue& e : p.value("examples").toArray())
        {
            const QJsonObject ex = e.toObject();
            page.examples.append({ ex.value("code").toString(), ex.value("runnable").toBool() });
        }
        pages.append(page);
    }
    return pages;
}

// ---- code highlighting: a hand-rolled scanner, no regular expressions ----

namespace
{

bool isIdentStart(QChar c) { return c.isLetter() || c == '_'; }
bool isIdentChar(QChar c) { return c.isLetterOrNumber() || c == '_'; }

const QSet<QString>& rubyKeywords()
{
    static const QSet<QString> kw = {
        "do", "end", "if", "else", "elsif", "then", "while", "until", "loop",
        "begin", "rescue", "def", "case", "when", "and", "or", "not",
        "return", "true", "false", "nil"
    };
    return kw;
}

QString escaped(const QString& text)
{
    return text.toHtmlEscaped();
}

// Where a `/` begins a regex rather than dividing: where a value cannot be,
// after nothing, an operator, an opening bracket or a keyword such as `if`.
bool regexMayStart(const QString& line, int i, bool lastWasValue)
{
    if (lastWasValue)
        return false;
    // `/ ` after nothing at all is still a regex only if it closes on the line
    return line.indexOf('/', i + 1) > i;
}

} // namespace

QVector<CodeToken> TutorialDocs::tokenizeLine(const QString& line)
{
    QVector<CodeToken> out;
    const int n = line.size();
    int i = 0;
    bool lastWasValue = false;   // a value just ended: `/` divides, it does not open a regex
    bool afterDef = false;       // the next word is a method being defined
    auto push = [&](int start, int end, CodeTokenKind kind) {
        out.push_back(CodeToken{ start, end - start, kind });
    };
    while (i < n)
    {
        const QChar ch = line[i];
        if (ch.isSpace())
        {
            i++;
            continue;
        }
        if (ch == '#') // comment to end of line
        {
            push(i, n, CodeTokenKind::Comment);
            break;
        }
        if (ch == '"' || ch == '\'') // string literal
        {
            int end = i + 1;
            while (end < n && line[end] != ch)
                end += (line[end] == '\\' && end + 1 < n) ? 2 : 1; // skip escapes
            if (end < n)
                end++;
            push(i, end, CodeTokenKind::String);
            i = end;
            lastWasValue = true;
            continue;
        }
        if (ch == ':' && i + 1 < n && isIdentStart(line[i + 1])) // :symbol
        {
            int end = i + 1;
            while (end < n && isIdentChar(line[end]))
                end++;
            push(i, end, CodeTokenKind::Symbol);
            i = end;
            lastWasValue = true;
            continue;
        }
        if (ch == '@' && i + 1 < n && (isIdentStart(line[i + 1]) || line[i + 1] == '@')) // @ivar
        {
            int end = i + 1;
            while (end < n && (isIdentChar(line[end]) || line[end] == '@'))
                end++;
            push(i, end, CodeTokenKind::Ivar);
            i = end;
            lastWasValue = true;
            continue;
        }
        if (ch == '/' && regexMayStart(line, i, lastWasValue)) // /regex/flags
        {
            int end = i + 1;
            while (end < n && line[end] != '/')
                end += (line[end] == '\\' && end + 1 < n) ? 2 : 1;
            if (end < n)
                end++;
            while (end < n && line[end].isLetter())
                end++;
            push(i, end, CodeTokenKind::Regex);
            i = end;
            lastWasValue = true;
            continue;
        }
        if (ch.isDigit() && (i == 0 || !isIdentChar(line[i - 1]))) // number (int or float)
        {
            int end = i;
            while (end < n && line[end].isDigit())
                end++;
            if (end < n && line[end] == '.' && end + 1 < n && line[end + 1].isDigit())
            {
                end++;
                while (end < n && line[end].isDigit())
                    end++;
            }
            push(i, end, CodeTokenKind::Number);
            i = end;
            lastWasValue = true;
            continue;
        }
        if (isIdentStart(ch) && (i == 0 || !isIdentChar(line[i - 1]))) // word
        {
            int end = i;
            while (end < n && isIdentChar(line[end]))
                end++;
            const QString word = line.mid(i, end - i);
            if (end < n && line[end] == ':' && !(end + 1 < n && line[end + 1] == ':')) // opt key `name:`
            {
                push(i, end, CodeTokenKind::Symbol);
                lastWasValue = false;
            }
            else if (afterDef)
            {
                push(i, end, CodeTokenKind::Def);
                lastWasValue = true;
            }
            else if (rubyKeywords().contains(word))
            {
                push(i, end, CodeTokenKind::Keyword);
                // true/false/nil are values; if/when/and are where one goes
                lastWasValue = word == "true" || word == "false" || word == "nil" || word == "end";
            }
            else
            {
                if (word[0].isUpper())
                    push(i, end, CodeTokenKind::Constant);
                lastWasValue = true;
            }
            afterDef = word == "def";
            i = end;
            continue;
        }
        // An operator or a bracket: a value may come next, unless one closed.
        lastWasValue = ch == ')' || ch == ']' || ch == '}';
        afterDef = afterDef && ch == '.'; // def self.name
        i++;
    }
    return out;
}

namespace
{

QString highlightLine(const QString& line, const CodeColours& c)
{
    auto colourOf = [&c](CodeTokenKind k) -> const QString& {
        switch (k)
        {
        case CodeTokenKind::Keyword:  return c.keyword;
        case CodeTokenKind::Symbol:   return c.symbol;
        case CodeTokenKind::Number:   return c.number;
        case CodeTokenKind::String:   return c.string;
        case CodeTokenKind::Regex:    return c.regex;
        case CodeTokenKind::Comment:  return c.comment;
        case CodeTokenKind::Def:      return c.def;
        case CodeTokenKind::Ivar:     return c.ivar;
        case CodeTokenKind::Constant: return c.constant;
        }
        return c.keyword;
    };
    QString out;
    int at = 0;
    for (const CodeToken& t : TutorialDocs::tokenizeLine(line))
    {
        out += escaped(line.mid(at, t.start - at));
        const QString text = line.mid(t.start, t.length);
        const QString& colour = colourOf(t.kind);
        if (colour.isEmpty())
            out += escaped(text);
        else
            out += "<span style=\"color:" + colour + ";"
                + (t.kind == CodeTokenKind::Comment ? QStringLiteral("font-style:italic;") : QString())
                + "\">" + escaped(text) + "</span>";
        at = t.start + t.length;
    }
    out += escaped(line.mid(at));
    return out;
}

} // namespace

QHash<QString, ExampleCard> TutorialDocs::exampleCardsFromJson(const QByteArray& json)
{
    QHash<QString, ExampleCard> cards;
    const QJsonObject root = QJsonDocument::fromJson(json).object();
    for (auto it = root.constBegin(); it != root.constEnd(); ++it)
    {
        const QJsonObject card = it.value().toObject();
        cards.insert(it.key(), { card.value("title").toString(), card.value("blurb").toString() });
    }
    return cards;
}

QString TutorialDocs::exampleTitle(const QString& key, const QHash<QString, ExampleCard>& cards)
{
    const QString named = cards.value(key).title;
    if (!named.isEmpty())
        return named;
    QStringList words = key.split(QLatin1Char('_'), Qt::SkipEmptyParts);
    for (QString& word : words)
        word[0] = word[0].toUpper();
    return words.join(QLatin1Char(' '));
}

QString TutorialDocs::highlightCode(const QString& source, const CodeColours& colours)
{
    QStringList htmlLines;
    const QStringList lines = source.split('\n');
    for (const QString& line : lines)
    {
        QString html = highlightLine(line, colours);
        int indent = 0;
        while (indent < line.size() && line[indent] == ' ')
            indent++;
        if (indent > 0)
            html = QString("&nbsp;").repeated(indent) + html.mid(indent);
        htmlLines << html;
    }
    return htmlLines.join("<br>");
}

} // namespace SonicPi
