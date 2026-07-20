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

# Character hygiene for the translated tutorials.
#
# Translators write into Weblate, and their editors happily substitute
# typographically correct characters for the ASCII ones Ruby needs. Where the
# substitution is invisible (space-like and zero-width characters) it is
# repaired here; where it is visible (quote marks) the offending line is
# reported instead, because only a translator can tell a broken string
# delimiter apart from quoted text.
module I18nHygiene

  # Space-like and zero-width characters, repaired on sight. They are
  # indistinguishable from correct code on screen, so a reader has no way to
  # diagnose the resulting error themselves.
  INVISIBLE_REPLACEMENTS = {
    " " => ' ',  # Narrow No-Break Space
    " " => ' ',  # Non-Breaking Space
    "​" => '',   # Zero Width Space
    "‌" => '',   # Zero Width Non-Joiner
    "‍" => '',   # Zero Width Joiner
    "﻿" => '',   # Byte Order Mark
    "‎" => '',   # Left-to-Right Mark
    "‏" => ''    # Right-to-Left Mark
  }.freeze

  # Quote marks that Ruby will not accept as a string delimiter. Substituting
  # ASCII would silently rewrite quoted text as often as it fixed a delimiter,
  # so these are reported rather than repaired.
  TYPOGRAPHIC_QUOTES = {
    "‘" => 'LEFT SINGLE QUOTATION MARK',
    "’" => 'RIGHT SINGLE QUOTATION MARK',
    "‚" => 'SINGLE LOW-9 QUOTATION MARK',
    "‛" => 'SINGLE HIGH-REVERSED-9 QUOTATION MARK',
    "“" => 'LEFT DOUBLE QUOTATION MARK',
    "”" => 'RIGHT DOUBLE QUOTATION MARK',
    "„" => 'DOUBLE LOW-9 QUOTATION MARK',
    "‟" => 'DOUBLE HIGH-REVERSED-9 QUOTATION MARK',
    "«" => 'LEFT-POINTING DOUBLE ANGLE QUOTATION MARK',
    "»" => 'RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK',
    "‹" => 'SINGLE LEFT-POINTING ANGLE QUOTATION MARK',
    "›" => 'SINGLE RIGHT-POINTING ANGLE QUOTATION MARK',
    "＂" => 'FULLWIDTH QUOTATION MARK',
    "＇" => 'FULLWIDTH APOSTROPHE'
  }.freeze


  def self.clean_invisible_chars(text)
    text.gsub(Regexp.union(INVISIBLE_REPLACEMENTS.keys), INVISIBLE_REPLACEMENTS)
  end


  # Every typographic quote mark in executable parts of the markdown, as
  # {:line, :char, :name, :source} hashes. Prose is skipped, as are code
  # comments — Ruby never parses either, so a curly quote there is harmless.
  def self.typographic_quotes_in_code(markdown)
    offences = []

    code_segments(markdown).each do |line, source|
      strip_comment(source).each_char do |c|
        next unless TYPOGRAPHIC_QUOTES.key?(c)
        offences << { line: line,
                      char: c,
                      name: "U+#{format('%04X', c.ord)} #{TYPOGRAPHIC_QUOTES[c]}",
                      source: source.strip }
      end
    end

    offences
  end


  def self.assert_no_typographic_quotes_in_code!(filename, markdown)
    offences = typographic_quotes_in_code(markdown)
    return if offences.empty?

    report = offences.map do |o|
      "  #{filename}:#{o[:line]} #{o[:name]} in: #{o[:source]}"
    end

    raise <<~MSG
      Typographic quote marks found in tutorial code examples. Ruby cannot
      parse these, so the example will fail when pasted into the editor.
      Fix the translation in Weblate, then re-run this tool.

      #{report.join("\n")}
    MSG
  end


  # [line number, source] for each fenced code block line and each inline code
  # span in the markdown.
  def self.code_segments(markdown)
    segments = []
    in_fence = false

    markdown.split("\n").each_with_index do |line, i|
      if line.strip.start_with?('```')
        in_fence = !in_fence
      elsif in_fence
        segments << [i + 1, line]
      else
        line.scan(/`([^`]+)`/) { segments << [i + 1, Regexp.last_match(1)] }
      end
    end

    segments
  end


  # Drop any trailing `#` comment, leaving `#` characters that sit inside a
  # string literal alone.
  def self.strip_comment(source)
    delimiter = nil

    source.each_char.with_index do |c, i|
      if delimiter
        delimiter = nil if c == delimiter
      elsif c == '"' || c == "'"
        delimiter = c
      elsif c == '#'
        return source[0...i]
      end
    end

    source
  end
end
