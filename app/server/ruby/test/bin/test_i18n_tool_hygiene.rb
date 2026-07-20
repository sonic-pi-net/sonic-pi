# frozen_string_literal: true

#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative '../setup_test'
require_relative '../../bin/i18n-tool-hygiene'

class I18nToolHygieneTester < Minitest::Test

  # ---- clean_invisible_chars ----------------------------------------------

  def test_narrow_no_break_space_becomes_a_plain_space
    # The original report: the French tutorial's `live_loop :flibble` used
    # U+202F, giving "undefined method 'live_loop '" when pasted.
    assert_equal("live_loop :flibble",
                 I18nHygiene.clean_invisible_chars("live_loop :flibble"))
  end

  def test_non_breaking_space_becomes_a_plain_space
    assert_equal("sleep 1", I18nHygiene.clean_invisible_chars("sleep 1"))
  end

  def test_zero_width_and_direction_marks_are_removed
    "​‌‍﻿‎‏".each_char do |c|
      assert_equal("play 60", I18nHygiene.clean_invisible_chars("play#{c} 60"))
    end
  end

  # ---- typographic_quotes_in_code ------------------------------------------

  def test_typographic_quote_in_a_fenced_code_block_is_reported
    md = <<~MD
      Some prose.

      ```
      samps = "/pfad/zu/meinen/samples/“
      ```
    MD

    offences = I18nHygiene.typographic_quotes_in_code(md)

    assert_equal(1, offences.count)
    assert_equal(4, offences.first[:line])
    assert_equal("“", offences.first[:char])
  end

  def test_typographic_quotes_in_prose_are_not_reported
    md = <<~MD
      Er sagte „Hallo“ und l'ami’s café «salut».

      ```
      play 60
      ```
    MD

    assert_empty(I18nHygiene.typographic_quotes_in_code(md))
  end

  def test_typographic_quote_in_an_inline_code_span_is_reported
    md = "Use `sample “foo”` to play it.\n"

    offences = I18nHygiene.typographic_quotes_in_code(md)

    assert_equal(2, offences.count)
    assert_equal([1, 1], offences.map { |o| o[:line] })
  end

  def test_typographic_quotes_inside_a_code_comment_are_not_reported
    # Harmless: Ruby doesn't parse comment text, so a translator's curly
    # quotes there can't break a paste.
    md = <<~MD
      ```
      # Er sagte „Hallo“
      play 60
      ```
    MD

    assert_empty(I18nHygiene.typographic_quotes_in_code(md))
  end

  def test_a_hash_inside_a_string_does_not_start_a_comment
    md = <<~MD
      ```
      sample "/pfad/#zu/samples/“
      ```
    MD

    assert_equal(1, I18nHygiene.typographic_quotes_in_code(md).count)
  end

  def test_guillemets_and_fullwidth_quotes_in_code_are_reported
    md = <<~MD
      ```
      sample «foo»
      sample ＂bar＂
      ```
    MD

    assert_equal(4, I18nHygiene.typographic_quotes_in_code(md).count)
  end

  # ---- assert_no_typographic_quotes_in_code! -------------------------------

  def test_clean_content_does_not_raise
    md = "```\nsample \"/pfad/zu/samples/\"\n```\n"

    I18nHygiene.assert_no_typographic_quotes_in_code!("03.7-Sample-Packs.md", md)
  end

  def test_offending_content_raises_naming_the_file_and_line
    md = "```\nsample \"/pfad/“\n```\n"

    err = assert_raises(RuntimeError) do
      I18nHygiene.assert_no_typographic_quotes_in_code!("03.7-Sample-Packs.md", md)
    end

    assert_includes(err.message, "03.7-Sample-Packs.md:2")
    assert_includes(err.message, "U+201C")
  end

  # ---- the shipped tutorials ------------------------------------------------

  def test_generated_tutorials_are_free_of_typographic_quotes_in_code
    generated = File.expand_path("../../../../../etc/doc/generated", __dir__)
    offenders = Dir["#{generated}/*/tutorial/*.md"].sort.flat_map do |path|
      I18nHygiene.typographic_quotes_in_code(File.read(path, encoding: 'utf-8')).
        map { |o| "#{path.split('/')[-3..].join('/')}:#{o[:line]} #{o[:name]}" }
    end

    assert_empty(offenders)
  end
end
