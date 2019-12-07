# -*- coding: utf-8 -*-
#
#--
# Copyright (C) 2009-2014 Thomas Leitner <t_leitner@gmx.at>
#
# This file is part of kramdown which is licensed under the MIT.
#++
#

require 'kramdown/parser/kramdown/extensions'
require 'kramdown/parser/kramdown/blank_line'
require 'kramdown/parser/kramdown/codeblock'

module Kramdown
  module Parser
    class Kramdown

      FOOTNOTE_DEFINITION_START = /^#{OPT_SPACE}\[\^(#{ALD_ID_NAME})\]:\s*?(.*?\n#{CODEBLOCK_MATCH})/

      # Parse the foot note definition at the current location.
      def parse_footnote_definition
        start_line_number = @src.current_line_number
        @src.pos += @src.matched_size

        el = Element.new(:footnote_def, nil, nil, :location => start_line_number)
        parse_blocks(el, @src[2].gsub(INDENT, ''))
        warning("Duplicate footnote name '#{@src[1]}' - overwriting") if @footnotes[@src[1]]
        (@footnotes[@src[1]] = {})[:content] = el
        @tree.children << Element.new(:eob, :footnote_def, nil, :location => start_line_number)
        true
      end
      define_parser(:footnote_definition, FOOTNOTE_DEFINITION_START)


      FOOTNOTE_MARKER_START = /\[\^(#{ALD_ID_NAME})\]/

      # Parse the footnote marker at the current location.
      def parse_footnote_marker
        start_line_number = @src.current_line_number
        @src.pos += @src.matched_size
        fn_def = @footnotes[@src[1]]
        if fn_def
          fn_def[:marker] ||= []
          fn_def[:marker].push(Element.new(:footnote, fn_def[:content], nil, :name => @src[1], :location => start_line_number))
          fn_def[:stack] = [@stack.map {|s| s.first}, @tree, fn_def[:marker]].flatten.compact
          @tree.children << fn_def[:marker].last
        else
          warning("Footnote definition for '#{@src[1]}' not found on line #{start_line_number}")
          add_text(@src.matched)
        end
      end
      define_parser(:footnote_marker, FOOTNOTE_MARKER_START, '\[')

    end
  end
end
