#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# This script creates translated versions of the English tutorial.
require_relative "../core.rb"

require 'kramdown'
require 'gettext'
require 'gettext/po'
require 'gettext/po_parser'
require 'gettext/tools/msgmerge'
require 'optparse'
require 'fileutils'


class KramdownToOurMarkdown < Kramdown::Converter::Kramdown
  # slightly alters the behaviour of ruby kramdown gem's converter
  # TODO: send these as config options to upstream devs

  def convert_a(el, opts)
    # ruby kramdown wants to use document-wide link list footnotes,
    # but we prefer inline links instead
    if el.attr['href'].empty? then
      "[#{inner(el, opts)}]()"
    elsif el.attr['href'] =~ /^(?:http|ftp)/ || el.attr['href'].count("()") > 0
      "[#{inner(el, opts)}](#{el.attr['href']})"
    else
      title = parse_title(el.attr['title'])
      "[#{inner(el, opts)}](#{el.attr['href']}#{title})"
    end
  end

end


def is_number? string
  true if Float(string) rescue false
end


def handle_entry(msgid, filename, line, flags = [])
  reference = "#{filename}:#{line}"
  msgid.gsub!(/\\([.:_])/, '\1')

  if is_number?msgid then
    return msgid
  end

  if $pot.has_key?msgid then
    entry = $pot[msgid]
  else
    entry = GetText::POEntry.new(:normal)
    entry.msgid = msgid
  end

  entry.flags |= flags
  entry.references << reference

  $pot[msgid] = entry
  $count_msgid += 1

  if $po.has_key?msgid then
    $count_msgstr += 1 unless ($po[msgid].msgstr == nil) || ($po[msgid].msgstr == "")
    $count_fuzzy += 1 if $po[msgid].flags.include?("fuzzy")
    return $po[msgid].msgstr || msgid
  else
    return msgid
  end
end


def convert_element(filename, el, bullet = nil) case el.type

  when :root, :li, :ul, :ol
    i = 0
    while i < el.children.count do
      case el.type
      when :ul
        b = '*'
      when :ol
        b = "#{i+1}."
      else
        b = nil
      end
      convert_element(filename, el.children[i], b)
      i += 1
    end

  when :blank
    if $task == :translate then
      $translated[filename] += el.value.gsub(/' '/, '')
    end

  when :p
    root = Kramdown::Element.new(
      :root, nil, nil,
      :encoding => "UTF-8",
      :location => 1,
      :options => {},
      :abbrev_defs => {}, :abbrev_attr => {}
    )
    root.children = [el]
    output, warnings = KramdownToOurMarkdown.convert(root)
    output.gsub!(/\n/, ' ').strip!

    t = handle_entry(output, filename, el.options[:location])

    if $task == :translate then
      if bullet then
        $translated[filename] += bullet + " "
      end
      $translated[filename] += t + "\n"
    end

  when :codeblock
    t = handle_entry(el.value.gsub(/\n+$/, ""), filename, el.options[:location], ["no-wrap"])

    if $task == :translate then
      $translated[filename] += "```\n" + t + "\n" + "```\n"
    end

  when :header
    t = handle_entry(el.options[:raw_text].strip, filename, el.options[:location])

    if $task == :translate then
      $translated[filename] += ("#" * el.options[:level]) + " " + t + "\n"
    end

  else
    raise "Error #{filename}: Please implement conversion for unknown Kramdown element type :#{el.type} in line #{el.options[:location]}"
  end
end


# ------------ main ------------

$task = nil

OptionParser.new do |opts|
  opts.banner = "Usage: i18n-tool.rb [options]"
  opts.on('-x', '--extract', 'creates .pot file from English tutorial (similar to xgettext)') do
    $task = :extract
  end
  opts.on('-t', '--translate', 'translate English tutorial to all languages') do
    $task = :translate
  end
  opts.on('-u', '--update', 'update translation files with current English tutorial (similar to msgmerge)') do
    $task = :update
  end
end.parse!

if $task == :extract then
  lang = ['en']
else
  lang =
    Dir[File.expand_path("../../../../etc/doc/lang/sonic-pi-tutorial-*.po", __dir__)].
    map { |p| File.basename(p).gsub(/sonic-pi-tutorial-(.*?).po/, '\1') }.sort
end

lang.each do |l|

  po_filename  = File.expand_path("../../../../etc/doc/lang/sonic-pi-tutorial-#{l}.po", __dir__)
  pot_filename = File.expand_path("../../../../etc/doc/lang/sonic-pi-tutorial.pot", __dir__)

  if $task == :update then

    raise "no .pot file, run 'i18n-tool.rb --extract' first" unless File.exist?pot_filename
    cmdline = ['--update', '--no-obsolete-entries', po_filename, pot_filename]
    GetText::Tools::MsgMerge.run(*cmdline)
    $stderr.puts "Merged tutorial translation #{l}"

  else

    $pot = GetText::PO.new
    $po = GetText::PO.new
    $translated = {}
    $count_msgid = 0
    $count_msgstr = 0
    $count_fuzzy = 0

    if $task == :translate then
      parser = GetText::POParser.new
      parser.ignore_fuzzy = false
      parser.report_warning = false
      parser.parse_file(File.expand_path("../../../../etc/doc/lang/sonic-pi-tutorial-#{l}.po", __dir__), $po)
    end

    Dir[File.expand_path("../../../../etc/doc/tutorial/*.md", __dir__)].sort.each do |path|
      $stderr.puts "Parsing #{path}" if $task == :extract
      basename = File.basename(path)
      $translated[basename] = ""

      content = IO.read(path, :encoding => 'utf-8')
      content = content.to_s
      # GitHub markdown syntax uses ```` to mark code blocks Kramdown uses ~~~~
      # Therefore, let's fix-point on GitHub syntax, and fudge it
      # into Kramdown syntax where necessary
      content.gsub!(/\`\`\`\`*/, '~~~~')
      k = Kramdown::Document.new(content)
      convert_element(basename, k.root)
    end

    case $task
    when :translate
      FileUtils::rm_rf File.expand_path("../../../../etc/doc/generated/#{l}/tutorial", __dir__)
      FileUtils::mkdir_p File.expand_path("../../../../etc/doc/generated/#{l}/tutorial", __dir__)
      $translated.each do |filename, newcontent|
        File.open(File.expand_path("../../../../etc/doc/generated/#{l}/tutorial/#{filename}", __dir__), 'w') do |f|
          f << newcontent
        end
      end
      if ($count_msgid > 0) then
        pt = ($count_msgstr * 100.0) / $count_msgid
        pf = ($count_fuzzy * 100.0) / $count_msgid
      else
        pt = 0
        pf = 0
      end
      $stderr.puts "Translated tutorial #{l}: #{format("%.1f", pt)}% ready, #{format("%.1f", pf)}% fuzzy."

    when :extract
      File.open(File.expand_path("../../../../etc/doc/lang/sonic-pi-tutorial.pot", __dir__), 'w') do |f|
        $stderr.puts "Writing ../../../../etc/doc/lang/sonic-pi-tutorial.pot"
        f << <<-HEADER
# This file is distributed under the same license as the Sonic Pi package.
# Do not edit this file, use Weblate instead.
# Read TRANSLATION.md for more information.

msgid ""
msgstr ""
"Project-Id-Version: Sonic Pi\\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF-8\\n"
"Content-Transfer-Encoding: 8bit\\n"

HEADER
        f << $pot.to_s
      end
    end

  end

end
