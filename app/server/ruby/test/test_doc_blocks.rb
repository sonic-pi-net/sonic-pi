#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/doc_blocks"
require "tmpdir"

module SonicPi
  class DocBlocksTester < Minitest::Test
    def blocks(md)
      DocBlocks.tutorial(md, "test.md")
    end

    def test_the_blocks_a_chapter_has_always_had
      md = <<~MD
        # Live Coding

        Some *words* and `code`.

        ```
        play 60
        ```

        * one
        * two

        ![A picture](../../../etc/doc/images/tutorial/a.png)
      MD
      assert_equal([
        { "type" => "heading", "level" => 1, "text" => "Live Coding" },
        { "type" => "prose", "html" => "<p>Some <em>words</em> and <code>code</code>.</p>" },
        { "type" => "code", "source" => "play 60", "runnable" => true },
        { "type" => "list", "ordered" => false, "items" => ["one", "two"] },
        { "type" => "image", "path" => "tutorial/a.png", "alt" => "A picture" },
      ], blocks(md))
    end

    def test_a_lead_line_is_a_paragraph_marked_lead
      md = <<~MD
        Welcome *friend*. Today is the perfect day!
        {: .lead}

        Hopefully you're as excited as I am.
      MD
      assert_equal([
        { "type" => "lead", "html" => "Welcome <em>friend</em>. Today is the perfect day!" },
        { "type" => "prose", "html" => "<p>Hopefully you’re as excited as I am.</p>" },
      ], blocks(md))
    end

    def test_a_box_is_a_quoted_block_its_bold_first_line_its_label
      md = <<~MD
        > **How this `tutorial` works**
        >
        > Dip in and out.
        >
        > * one
        > * two
        >
        > Also, tell me.
      MD
      assert_equal([
        { "type" => "box", "label" => "How this <code>tutorial</code> works", "blocks" => [
          { "type" => "prose", "html" => "<p>Dip in and out.</p>" },
          { "type" => "list", "ordered" => false, "items" => ["one", "two"] },
          { "type" => "prose", "html" => "<p>Also, tell me.</p>" },
        ] },
      ], blocks(md))
    end

    def test_a_box_need_not_have_a_label
      md = <<~MD
        > Remember you're **free** to experiment.
      MD
      assert_equal([
        { "type" => "box", "label" => "", "blocks" => [
          { "type" => "prose", "html" => "<p>Remember you’re <strong>free</strong> to experiment.</p>" },
        ] },
      ], blocks(md))
    end

    def test_what_a_box_cannot_hold_is_refused_with_where_it_is
      # at the line of what is refused: the box's own (line 3) when it is empty
      { "code" => ["> **A box**\n>\n> ```\n> play 60\n> ```\n", 5],
        "a heading" => ["> **A box**\n>\n> ## Heading\n", 5],
        "a box" => ["> **A box**\n>\n> > inner\n", 5],
        "a lead line" => ["> **A box**\n>\n> Lead\n> {: .lead}\n", 5],
        "nothing" => ["> **A box**\n", 3] }.each do |what, (md, line)|
        e = assert_raises(RuntimeError, what) { blocks("Intro.\n\n" + md) }
        assert_match(/\Atest\.md:#{line}: .*#{what}/, e.message, what)
      end
    end

    def test_an_unknown_marker_is_refused_not_dropped
      e = assert_raises(RuntimeError) { blocks("Words.\n{: .laed}\n") }
      assert_match(/\Atest\.md:1: .*\.laed/, e.message)
    end

    def chapter(text)
      Dir.mktmpdir do |dir|
        path = File.join(dir, "01.1-Live-Coding.md")
        File.write(path, text)
        [DocBlocks.chapter(path), path]
      end
    end

    def test_a_chapter_is_its_title_and_its_blocks_less_its_opening_heading
      # the page's heading is the title, numbered; the # under it is the same name, unnumbered (or worded otherwise)
      page, = chapter("1.1 Live Coding\n\n# Live Coding\n\nWords.\n\n## Later\n\n# A later one stays\n")
      assert_equal({ "title" => "1.1 Live Coding", "blocks" => [
        { "type" => "prose", "html" => "<p>Words.</p>" },
        { "type" => "heading", "level" => 2, "text" => "Later" },
        { "type" => "heading", "level" => 1, "text" => "A later one stays" },
      ] }, page)
    end

    def test_a_chapter_that_opens_with_words_keeps_them
      page, = chapter("1.1 Live Coding\n\nWords.\n\n## Later\n")
      assert_equal([{ "type" => "prose", "html" => "<p>Words.</p>" },
                    { "type" => "heading", "level" => 2, "text" => "Later" }], page["blocks"])
    end

    def test_a_chapter_refusal_names_the_files_own_line
      e = assert_raises(RuntimeError) { chapter("1.1 Live Coding\n\n# Live Coding\n\nWords.\n{: .laed}\n") }
      assert_match(%r{01\.1-Live-Coding\.md:5: .*\.laed}, e.message)
    end

    def test_runnable_code
      assert DocBlocks.runnable?("play 60")
      refute DocBlocks.runnable?("# just a comment")
      refute DocBlocks.runnable?("puts 1 #=> 1")
      refute DocBlocks.runnable?("sample \"/Users/sam/a.wav\"")
      refute DocBlocks.runnable?("sample \"C:/a.wav\"")
      refute DocBlocks.runnable?("(ring 1, 2)")
      refute DocBlocks.runnable?("play 60 do")
    end
  end
end
