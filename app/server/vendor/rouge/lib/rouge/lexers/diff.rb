# -*- coding: utf-8 -*- #

module Rouge
  module Lexers
    class Diff < RegexLexer
      desc "Lexes unified diffs or patches"

      tag 'diff'
      aliases 'patch', 'udiff'
      filenames '*.diff', '*.patch'
      mimetypes 'text/x-diff', 'text/x-patch'

      def self.analyze_text(text)
        return 1   if text.start_with?('Index: ')
        return 1   if text.start_with?('diff ')

        return 0.9 if text =~ /\A---.*?\n\+\+\+/m
      end

      state :header do
        rule /^diff .*?\n(?=---|\+\+\+)/m, Generic::Heading
        rule /^--- .*?\n/, Generic::Deleted
        rule /^\+\+\+ .*?\n/, Generic::Inserted
      end

      state :diff do
        rule /@@ -\d+,\d+ \+\d+,\d+ @@.*?\n/, Generic::Heading
        rule /^\+.*?\n/, Generic::Inserted
        rule /^-.*?\n/,  Generic::Deleted
        rule /^ .*?\n/,  Text
        rule /^.*?\n/,   Error
      end

      state :root do
        mixin :header
        mixin :diff
      end
    end
  end
end
