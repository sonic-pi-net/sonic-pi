#!/usr/bin/env ruby
# Convert LICENSE.md (Markdown) into LICENSE.rtf for the WiX EULA dialog.
# Run from any cwd; writes wix/LICENSE.rtf next to this script.

require "pathname"

here    = Pathname.new(__FILE__).realpath.dirname
md_path = here.parent.parent.parent + "LICENSE.md"
rtf_path = here + "LICENSE.rtf"

def rtf_escape(text)
  out = +""
  text.each_char do |c|
    case c
    when "\\" then out << "\\\\"
    when "{"  then out << "\\{"
    when "}"  then out << "\\}"
    when /[\x00-\x7f]/ then out << c
    else
      # \u<signed16>? — RTF requires signed 16-bit for \u
      cp = c.ord
      cp -= 0x10000 if cp > 32767
      out << "\\u#{cp}?"
    end
  end
  out
end

def strip_links(line)
  # [text](url) -> text  (drop URLs since this is a static EULA)
  line.gsub(/\[([^\]]*)\]\(([^)]+)\)/) { $1 }
end

def emphasize(line)
  # `code` -> code (no special font in this minimal styling)
  line.gsub(/`([^`]+)`/) { $1 }
end

lines = md_path.read.split("\n")

rtf = []
rtf << '{\rtf1\ansi\ansicpg1252\deff0\nouicompat'
rtf << '{\fonttbl{\f0\fswiss\fcharset0 Helvetica-Bold;}{\f1\fswiss\fcharset0 Helvetica;}}'
rtf << '{\colortbl ;\red0\green0\blue0;}'
rtf << '\viewkind4\uc1'

in_list = false

lines.each do |raw|
  line = strip_links(emphasize(raw))

  if line =~ /^# (.+)$/
    rtf << "\\pard\\sa180\\cf1\\b\\f0\\fs28 #{rtf_escape($1)}\\par"
    rtf << "\\b0\\f1\\fs18"
    in_list = false
  elsif line =~ /^## (.+)$/
    rtf << "\\pard\\sa120\\cf1\\b\\f0\\fs22 #{rtf_escape($1)}\\par"
    rtf << "\\b0\\f1\\fs18"
    in_list = false
  elsif line =~ /^### (.+)$/
    rtf << "\\pard\\sa80\\cf1\\b\\f0\\fs20 #{rtf_escape($1)}\\par"
    rtf << "\\b0\\f1\\fs18"
    in_list = false
  elsif line =~ /^- (.+)$/
    rtf << "\\pard\\fi-200\\li200\\sa40\\f1\\fs18\\bullet  #{rtf_escape($1)}\\par"
    in_list = true
  elsif line.strip.empty?
    rtf << "\\par" if !in_list
  else
    rtf << "\\pard\\sa80\\f1\\fs18 #{rtf_escape(line)}\\par"
    in_list = false
  end
end

rtf << "}"

rtf_path.write(rtf.join("\n"))
puts "Wrote #{rtf_path} (#{rtf_path.size} bytes)"
