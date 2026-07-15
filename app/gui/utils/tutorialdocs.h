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

#pragma once

#include <QColor>
#include <QString>
#include <QStringList>
#include <QVector>

namespace SonicPi
{

// Typed doc content, deserialised from the JSON emitted by qt-doc.rb
// (etc/doc/generated/native/...). The generator owns all parsing and
// runnability analysis; this layer only loads and renders.

struct TutorialBlock
{
    enum Type
    {
        Heading, // level 1-3, plain text
        Prose,   // html: inline-formatted rich text
        List,    // items: per-item rich text; ordered flag
        Code,    // source verbatim; runnable decided by the generator
        Image    // path relative to etc/doc/images; alt text
    };

    Type type = Prose;
    int level = 1;
    QString text;       // Heading text / Prose+Image: html / alt
    QString source;     // Code
    QStringList items;  // List
    bool ordered = false;
    bool runnable = false;
    QString path;       // Image
};

struct TutorialChapter
{
    QString title;
    QVector<TutorialBlock> blocks;
};

struct InstrumentOpt
{
    QString name;
    QString defaultText; // as shown in generated code ("110", ":sustain_level")
    double defaultNum = 0;
    bool numeric = false;
    QString doc;
    bool hasRange = false;
    double min = 0;
    double max = 0;
    bool minExcl = false; // open bound: valid values are strictly inside
    bool maxExcl = false;
    bool slidable = false;
};

struct InstrumentPage
{
    QString key;   // "prophet", "reverb"
    QString title; // "The Prophet"
    QString docHtml;
    QVector<InstrumentOpt> opts;
};

struct SampleGroup
{
    QString title;
    QStringList samples;
};

struct CodeExample
{
    QString code;
    bool runnable = false;
};

struct LangPage
{
    QString key;
    QString summary;
    QString usage;
    QString docHtml;
    QString introduced;
    QVector<CodeExample> examples;
};

// One quickstart cheat-sheet card: a friendly one-liner plus a small
// runnable snippet
struct QuickstartCard
{
    QString title;
    QString blurb;
    QString code;
};

struct QuickstartGroup
{
    QString title;
    QVector<QuickstartCard> cards;
};

// Colour names for the code token classes, mirroring the editor theme
struct CodeColours
{
    QString keyword;
    QString symbol;
    QString number;
    QString string;
    QString comment;
};

class TutorialDocs
{
public:
    static TutorialChapter chapterFromJson(const QByteArray& json);
    static QVector<InstrumentPage> instrumentsFromJson(const QByteArray& json);
    static QVector<SampleGroup> sampleGroupsFromJson(const QByteArray& json);
    static QVector<LangPage> langPagesFromJson(const QByteArray& json);

    // Syntax-highlight code as label rich text (spans + <br>, leading spaces
    // as &nbsp;). A hand-rolled character scanner — no regular expressions.
    static QString highlightCode(const QString& source, const CodeColours& colours);
};

} // namespace SonicPi
