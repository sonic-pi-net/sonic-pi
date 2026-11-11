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
require 'kramdown'
require 'ripper'

module SonicPi
  # The docs as typed blocks, read once for every place that shows them: the
  # desktop's docs pane (bin/qt-doc.rb) and the web's tutorial pages
  # (app/web/scripts/gen-editor-data.rb).
  module DocBlocks
    # Whether a piece of code plays as it stands: it parses, and it is not a
    # comment alone, a printed result (#=>), a path on disk or a bare
    # expression shown for its value.
    def self.runnable?(src)
      s = src.to_s
      lines = s.lines.map(&:strip)
      return false unless lines.any? { |l| !l.empty? && !l.start_with?("#") }
      return false if s.include?("#=>")
      return false if s.include?("\"/")
      return false if (("A".."Z").to_a + ("a".."z").to_a).any? { |d| s.include?("\"#{d}:/") }
      first = lines.find { |l| !l.empty? }
      return false if first.start_with?("(") || first.start_with?("[")
      begin
        !Ripper.sexp(s).nil?
      rescue StandardError
        false
      end
    end

    # A tutorial chapter's Markdown (less its title line) as blocks:
    #   heading  level, text
    #   prose    html
    #   list     ordered, items (an item's html each)
    #   code     source, runnable
    #   image    path (under etc/doc/images), alt
    #   lead     html: a line set larger than the text, as a chapter's
    #            opening. A paragraph with {: .lead} on the line under it
    #   box      label (html, or empty), blocks (prose and lists): words set
    #            apart from the text. A quoted block (> ...), its label its
    #            first line when that line is bold throughout
    # A construct the docs have no block for fails, naming the file (source)
    # and line, rather than going missing from a page.
    def self.tutorial(markdown, source)
      # GitHub's code fences, as kramdown has them (MarkdownConverter.convert)
      md = markdown.to_s.gsub(/\`\`\`\`*/, '~~~~')
      Kramdown::Document.new(md).root.children.filter_map { |el| block(el, source, false) }
    end

    # A tutorial chapter's file as its page: its title (the file's first line,
    # numbered: "1.1 Live Coding") and its blocks. A page's heading is its
    # title, so the chapter's opening # heading, its name as the text has it
    # (unnumbered, sometimes worded otherwise), is left out.
    def self.chapter(path)
      lines = File.read(path, encoding: "utf-8").lines
      # the title line kept as a blank one, so a refusal names the file's own line
      blocks = tutorial("\n" + lines.drop(1).join, path)
      blocks = blocks.drop(1) if blocks.first && blocks.first["type"] == "heading" && blocks.first["level"] == 1
      { "title" => lines.first.to_s.strip, "blocks" => blocks }
    end

    def self.block(el, source, in_box)
      where = "#{source}:#{el.options[:location]}"
      classes = el.attr["class"].to_s.split
      unless classes.empty? || (el.type == :p && classes == ["lead"])
        raise "#{where}: {: .#{classes.join(' .')}} is not a mark the docs have: {: .lead} under a paragraph is the one there is"
      end
      if in_box && !(%i[blank ul ol].include?(el.type) || (el.type == :p && classes.empty?))
        what = { p: "a lead line", codeblock: "code", header: "a heading", blockquote: "a box" }[el.type] || el.type
        raise "#{where}: a box (> ...) holds paragraphs and lists, not #{what}"
      end

      case el.type
      when :blank
        nil
      when :header
        { "type" => "heading", "level" => el.options[:level], "text" => el.options[:raw_text].to_s.strip }
      when :codeblock
        source_code = el.value.to_s
        source_code = source_code[0...-1] while source_code.end_with?("\n")
        { "type" => "code", "source" => source_code, "runnable" => runnable?(source_code) }
      when :ul, :ol
        items = el.children.select { |c| c.type == :li }.map { |li| item_html(li) }
        { "type" => "list", "ordered" => el.type == :ol, "items" => items }
      when :p
        if classes == ["lead"]
          { "type" => "lead", "html" => inline_html(el) }
        elsif (img = only_image(el))
          { "type" => "image", "path" => image_path(img.attr["src"].to_s), "alt" => img.attr["alt"].to_s }
        else
          prose(el)
        end
      when :blockquote
        box(el, source, where)
      else
        # tables and any other block construct: their content as prose
        prose(el)
      end
    end

    def self.box(el, source, where)
      children = el.children.reject { |c| c.type == :blank }
      first = children.first
      label = ""
      if first && first.type == :p && first.attr.empty? && bold_throughout?(first)
        label = inline_html(first.children.find { |c| c.type == :strong })
        children = children.drop(1)
      end
      raise "#{where}: a box (> ...) with nothing in it" if children.empty?
      { "type" => "box", "label" => label, "blocks" => children.filter_map { |c| block(c, source, true) } }
    end

    def self.bold_throughout?(para)
      strong = para.children.select { |c| c.type == :strong }
      strong.length == 1 && (para.children - strong).all? { |c| c.type == :text && c.value.to_s.strip.empty? }
    end

    def self.prose(el)
      html = html([el]).strip
      html.empty? ? nil : { "type" => "prose", "html" => html }
    end

    def self.html(els)
      root = Kramdown::Element.new(:root, nil, nil, encoding: "UTF-8", location: 1, options: {}, abbrev_defs: {}, abbrev_attr: {})
      root.children = els
      Kramdown::Converter::Html.convert(root)[0].to_s
    end

    # an element's content alone, as html: a paragraph's words without the <p>
    def self.inline_html(el)
      para = Kramdown::Element.new(:p)
      para.children = el.children
      html([para]).strip.delete_prefix("<p>").delete_suffix("</p>").strip
    end

    def self.item_html(li)
      html = html(li.children).strip
      if li.children.length == 1 && li.children[0].type == :p && html.start_with?("<p>") && html.end_with?("</p>")
        html = html[3...-4].strip
      end
      html
    end

    # the paragraph's picture when it is a picture alone (blank space aside)
    def self.only_image(el)
      imgs = el.children.select { |c| c.type == :img }
      return nil unless imgs.length == 1
      rest = el.children - imgs
      return nil unless rest.all? { |c| c.type == :text && c.value.to_s.strip.empty? }
      imgs.first
    end

    def self.image_path(src)
      if src.include?("etc/doc/images/") then src.split("etc/doc/images/").last
      elsif src.include?("images/") then src.split("images/").last
      else src
      end
    end
  end
end
