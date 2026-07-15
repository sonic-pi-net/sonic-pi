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

QString span(const QString& colour, const QString& text)
{
    return "<span style=\"color:" + colour + ";\">" + escaped(text) + "</span>";
}

QString highlightLine(const QString& line, const CodeColours& c)
{
    QString out;
    int i = 0;
    const int n = line.size();
    int plainStart = 0;

    auto flushPlain = [&](int upTo) {
        if (upTo > plainStart)
            out += escaped(line.mid(plainStart, upTo - plainStart));
    };

    while (i < n)
    {
        const QChar ch = line[i];

        if (ch == '#') // comment to end of line
        {
            flushPlain(i);
            out += span(c.comment, line.mid(i));
            plainStart = n;
            break;
        }
        if (ch == '"' || ch == '\'') // string literal
        {
            int end = i + 1;
            while (end < n && line[end] != ch)
                end += (line[end] == '\\' && end + 1 < n) ? 2 : 1; // skip escapes
            if (end < n)
                end++;
            flushPlain(i);
            out += span(c.string, line.mid(i, end - i));
            i = end;
            plainStart = i;
            continue;
        }
        if (ch == ':' && i + 1 < n && isIdentStart(line[i + 1])) // :symbol
        {
            int end = i + 1;
            while (end < n && isIdentChar(line[end]))
                end++;
            flushPlain(i);
            out += span(c.symbol, line.mid(i, end - i));
            i = end;
            plainStart = i;
            continue;
        }
        if (ch.isDigit()
            && (i == 0 || !isIdentChar(line[i - 1]))) // number (int or float)
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
            flushPlain(i);
            out += span(c.number, line.mid(i, end - i));
            i = end;
            plainStart = i;
            continue;
        }
        if (isIdentStart(ch) && (i == 0 || !isIdentChar(line[i - 1]))) // word
        {
            int end = i;
            while (end < n && isIdentChar(line[end]))
                end++;
            const QString word = line.mid(i, end - i);
            if (end < n && line[end] == ':'
                && !(end + 1 < n && line[end + 1] == ':')) // opt key `name:`
            {
                flushPlain(i);
                out += span(c.symbol, word);
                i = end;
                plainStart = i;
                continue;
            }
            if (rubyKeywords().contains(word))
            {
                flushPlain(i);
                out += span(c.keyword, word);
                i = end;
                plainStart = i;
                continue;
            }
            i = end;
            continue;
        }
        i++;
    }
    flushPlain(n);
    return out;
}

} // namespace

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
