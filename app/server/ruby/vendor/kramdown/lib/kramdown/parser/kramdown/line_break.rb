# -*- coding: utf-8 -*-
#
#--
# Copyright (C) 2009-2014 Thomas Leitner <t_leitner@gmx.at>
#
# This file is part of kramdown which is licensed under the MIT.
#++
#

module Kramdown
  module Parser
    class Kramdown

      LINE_BREAK = /(  |\\\\)(?=\n)/

      # Parse the line break at the current location.
      def parse_line_break
        @src.pos += @src.matched_size
        @tree.children << Element.new(:br)
      end
      define_parser(:line_break, LINE_BREAK, '(  |\\\\)(?=\n)')

    end
  end
end
