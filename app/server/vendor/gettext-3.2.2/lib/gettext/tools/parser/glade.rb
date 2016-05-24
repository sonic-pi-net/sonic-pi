# -*- coding: utf-8 -*-

=begin
  parser/glade.rb - parser for Glade-2

  Copyright (C) 2013       Kouhei Sutou <kou@clear-code.com>
  Copyright (C) 2004,2005  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby or LGPL.
=end

require 'cgi'
require 'gettext'

module GetText
  class GladeParser
    extend GetText

    bindtextdomain("gettext")

    class << self
      XML_RE = /<\?xml/
      GLADE_RE = /glade-2.0.dtd/

      def target?(file) # :nodoc:
        data = IO.readlines(file)
        if XML_RE =~ data[0] and GLADE_RE =~ data[1]
          true
        else
          if File.extname(file) == '.glade'
            raise _("`%{file}' is not glade-2.0 format.") % {:file => file}
          end
          false
        end
      end

      def parse(path, options={})
        parser = new(path, options)
        parser.parse
      end
    end

    TARGET1 = /<property.*translatable="yes">(.*)/
    TARGET2 = /(.*)<\/property>/

    def initialize(path, options={})
      @path = path
      @options = options
    end

    def parse # :nodoc:
      File.open(@path) do |file|
        parse_source(file)
      end
    end

    private
    def parse_source(input) # :nodoc:
      targets = []
      target = false
      start_line_no = nil
      val = nil

      input.each_line.with_index do |line, i|
        if TARGET1 =~ line
          start_line_no = i + 1
          val = $1 + "\n"
          target = true
          if TARGET2 =~ $1
            val = $1
            add_target(val, start_line_no, targets)
            val = nil
            target = false
          end
        elsif target
          if TARGET2 =~ line
            val << $1
            add_target(val, start_line_no, targets)
            val = nil
            target = false
          else
            val << line
          end
        end
      end
      targets
    end

    def add_target(val, line_no, targets) # :nodoc:
      return unless val.size > 0
      assoc_data = targets.assoc(val)
      val = CGI.unescapeHTML(val)
      if assoc_data
        targets[targets.index(assoc_data)] = assoc_data << "#{@path}:#{line_no}"
      else
        targets << [val.gsub(/\n/, '\n'), "#{@path}:#{line_no}"]
      end
      targets
    end
  end
end

if __FILE__ == $0
  # ex) ruby glade.rb foo.glade  bar.glade
  ARGV.each do |file|
    p GetText::GladeParser.parse(file)
  end
end
