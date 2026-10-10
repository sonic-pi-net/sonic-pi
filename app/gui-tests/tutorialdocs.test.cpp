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

#include <catch2/catch_test_macros.hpp>

#include <QFile>

#include "utils/tutorialdocs.h"

using namespace SonicPi;

TEST_CASE("chapter json loads typed blocks in order", "[tutorialdocs]")
{
    const QByteArray json = R"({
      "title": "9.2 Live Loops",
      "blocks": [
        {"type":"heading","level":1,"text":"Live Loops"},
        {"type":"prose","html":"<p>the real <i>gem</i></p>"},
        {"type":"code","source":"live_loop :foo do\n  play 60\nend","runnable":true},
        {"type":"code","source":"do\n  play 50\nend","runnable":false},
        {"type":"list","ordered":true,"items":["one","two"]},
        {"type":"image","path":"tutorial/GUI.png","alt":"Interface"}
      ]})";
    TutorialChapter ch = TutorialDocs::chapterFromJson(json);
    REQUIRE(ch.title == "9.2 Live Loops");
    REQUIRE(ch.blocks.size() == 6);
    CHECK(ch.blocks[0].type == TutorialBlock::Heading);
    CHECK(ch.blocks[0].level == 1);
    CHECK(ch.blocks[1].text == "<p>the real <i>gem</i></p>");
    CHECK(ch.blocks[2].runnable);
    CHECK(ch.blocks[2].source == "live_loop :foo do\n  play 60\nend");
    CHECK_FALSE(ch.blocks[3].runnable);
    CHECK(ch.blocks[4].ordered);
    CHECK(ch.blocks[4].items == QStringList({ "one", "two" }));
    CHECK(ch.blocks[5].path == "tutorial/GUI.png");
    CHECK(ch.blocks[5].text == "Interface");
}

TEST_CASE("instrument pages load true defaults and ranges", "[tutorialdocs]")
{
    const QByteArray json = R"({"pages":[
      {"key":"prophet","title":"The Prophet","doc_html":"<p>swirly</p>",
       "opts":[
         {"name":"cutoff","default":110,"doc":"brightness","min":30,"max":130,"slidable":true},
         {"name":"env_curve","default":2,"doc":"shape"},
         {"name":"sustain_level","default":":sustain_level","doc":"sym"}
       ]}]})";
    QVector<InstrumentPage> pages = TutorialDocs::instrumentsFromJson(json);
    REQUIRE(pages.size() == 1);
    const InstrumentPage& p = pages[0];
    CHECK(p.key == "prophet");
    REQUIRE(p.opts.size() == 3);
    CHECK(p.opts[0].numeric);
    CHECK(p.opts[0].defaultNum == 110.0);
    CHECK(p.opts[0].hasRange);
    CHECK(p.opts[0].min == 30.0);
    CHECK(p.opts[0].slidable);
    CHECK_FALSE(p.opts[1].hasRange);
    CHECK_FALSE(p.opts[2].numeric);
    CHECK(p.opts[2].defaultText == ":sustain_level");
}

TEST_CASE("sample groups and lang pages load", "[tutorialdocs]")
{
    QVector<SampleGroup> groups = TutorialDocs::sampleGroupsFromJson(
        R"({"groups":[{"title":"Bass Drums","samples":["bd_ada","bd_pure"]}]})");
    REQUIRE(groups.size() == 1);
    CHECK(groups[0].title == "Bass Drums");
    CHECK(groups[0].samples == QStringList({ "bd_ada", "bd_pure" }));

    QVector<LangPage> pages = TutorialDocs::langPagesFromJson(
        R"({"pages":[{"key":"play","summary":"Play a note","usage":"play note",
            "doc_html":"<p>d</p>","introduced":"v2.0",
            "examples":[{"code":"play 50","runnable":true}]}]})");
    REQUIRE(pages.size() == 1);
    CHECK(pages[0].key == "play");
    CHECK(pages[0].introduced == "v2.0");
    REQUIRE(pages[0].examples.size() == 1);
    CHECK(pages[0].examples[0].runnable);
}

TEST_CASE("code highlighting matches the editor's token colours", "[tutorialdocs]")
{
    CodeColours c;
    c.keyword = "#gold";
    c.symbol = "#pink";
    c.number = "#blue";
    c.string = "#green";
    c.comment = "#grey";

    // Function calls stay in the default colour; only Ruby keywords go gold
    CHECK(TutorialDocs::highlightCode("play 60", c)
          == "play <span style=\"color:#blue;\">60</span>");
    CHECK(TutorialDocs::highlightCode("foo :bd_haus", c)
          == "foo <span style=\"color:#pink;\">:bd_haus</span>");
    CHECK(TutorialDocs::highlightCode("rate: 0.5", c)
          == "<span style=\"color:#pink;\">rate</span>: <span style=\"color:#blue;\">0.5</span>");
    CHECK(TutorialDocs::highlightCode("# a < comment", c)
          == "<span style=\"color:#grey;font-style:italic;\"># a &lt; comment</span>");
    CHECK(TutorialDocs::highlightCode("sample \"hi # there\"", c)
          == "sample <span style=\"color:#green;\">&quot;hi # there&quot;</span>");
    CHECK(TutorialDocs::highlightCode("live_loop :foo do\n  sleep 1\nend", c)
          == "live_loop <span style=\"color:#pink;\">:foo</span>"
             " <span style=\"color:#gold;\">do</span><br>&nbsp;&nbsp;"
             "sleep <span style=\"color:#blue;\">1</span>"
             "<br><span style=\"color:#gold;\">end</span>");
    // Numbers inside identifiers stay plain; floats scan as one token
    CHECK(TutorialDocs::highlightCode("tb303", c) == "tb303");
    CHECK(TutorialDocs::highlightCode("0.25", c)
          == "<span style=\"color:#blue;\">0.25</span>");
}

// The web's other kinds (app/web/app/src/highlight.js), coloured from the
// same theme entries: a method being defined, an instance variable, a
// constant, a regex — and division, which is not one.
TEST_CASE("code highlighting has the web's token kinds", "[tutorialdocs]")
{
    CodeColours c;
    c.keyword = "#gold";
    c.number = "#blue";
    c.def = "#pink";
    c.ivar = "#teal";
    c.constant = "#ink";
    c.regex = "#lime";

    CHECK(TutorialDocs::highlightCode("def beep", c)
          == "<span style=\"color:#gold;\">def</span> <span style=\"color:#pink;\">beep</span>");
    CHECK(TutorialDocs::highlightCode("@count", c)
          == "<span style=\"color:#teal;\">@count</span>");
    CHECK(TutorialDocs::highlightCode("Ring.new", c)
          == "<span style=\"color:#ink;\">Ring</span>.new");
    CHECK(TutorialDocs::highlightCode("if s =~ /be+p/", c)
          == "<span style=\"color:#gold;\">if</span> s =~ <span style=\"color:#lime;\">/be+p/</span>");
    CHECK(TutorialDocs::highlightCode("x = 10 / 2", c)
          == "x = <span style=\"color:#blue;\">10</span> / <span style=\"color:#blue;\">2</span>");

    // One tokenizer for every rendering: the card's lines, its editor, and
    // inline code in prose.
    const auto t = TutorialDocs::tokenizeLine("def beep # hi");
    REQUIRE(t.size() == 3);
    CHECK(t[0].kind == CodeTokenKind::Keyword);
    CHECK((t[1].start == 4 && t[1].length == 4 && t[1].kind == CodeTokenKind::Def));
    CHECK(t[2].kind == CodeTokenKind::Comment);
}

TEST_CASE("an example's card is named as the web names it", "[tutorialdocs][examples]")
{
    QFile f(QStringLiteral(SP_ROOT) + "/etc/examples/cards.json");
    REQUIRE(f.open(QIODevice::ReadOnly));
    const QHash<QString, SonicPi::ExampleCard> cards = SonicPi::TutorialDocs::exampleCardsFromJson(f.readAll());

    // The ones the web's Examples page has its own words for.
    REQUIRE(cards.contains("haunted"));
    CHECK(cards["haunted"].title == "Haunted Bells");
    CHECK(cards["haunted"].blurb == "Listen to the coded bells...");
    CHECK(cards["acid"].title == "Acid Walk");
    CHECK(cards["idm_breakbeat"].title == "IDM Breakbeat");

    // Any other is named for its file, a word for each part.
    CHECK(SonicPi::TutorialDocs::exampleTitle("ambient_experiment", cards) == "Ambient Experiment");
    CHECK(SonicPi::TutorialDocs::exampleTitle("haunted", cards) == "Haunted Bells");
    CHECK(SonicPi::TutorialDocs::exampleTitle("tilburg_2", cards) == "Tilburg 2");
}
