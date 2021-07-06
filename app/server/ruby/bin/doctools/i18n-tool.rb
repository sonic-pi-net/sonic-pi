#!/usr/bin/env ruby
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

# This script creates translated versions of the English tutorial.
require_relative "../../core.rb"

require 'kramdown'
require 'gettext'
require 'gettext/po'
require 'gettext/po_parser'
require 'gettext/tools/msgmerge'
require 'optparse'
require 'fileutils'
require 'json'

@etc_path = File.expand_path("../../../../../etc/", File.dirname(__FILE__))

# List of all languages with GUI translation files
@lang_names = Hash[
  "bg" => "български", # Bulgarian
  "bn" => "বাংলা", # Bengali/Bangla
  "bs" => "Bosanski", # Bosnian
  "ca" => "Català", # Catalan
  "ca@valencia" => "Valencià", # Valencian
  "cs" => "Čeština", # Czech
  "da" => "Dansk", # Danish
  "de" => "Deutsch", # German
  "el" => "ελληνικά", # Greek
  "en_AU" => "English (Australian)", # English (Australian)
  "en_GB" => "English (UK)", # English (UK) - default language
  "en_US" => "English (US)", # English (US)
  "eo" => "Esperanto", # Esperanto
  "es" => "Español", # Spanish
  "et" => "Eesti keel", # Estonian
  "fa" => "فارسی", # Persian
  "fi" => "Suomi", # Finnish
  "fr" => "Français", # French
  "ga" => "Gaeilge", # Irish
  "gl" => "Galego", # Galician
  "he" => "עברית", # Hebrew
  "hi" => "हिन्दी", # Hindi
  "hu" => "Magyar", # Hungarian
  "hy" => "Հայերեն", # Armenian
  "id" => "Bahasa Indonesia", # Indonesian
  "is" => "Íslenska", # Icelandic
  "it" => "Italiano", # Italian
  "ja" => "日本語", # Japanese
  "ka" => "ქართული", # Georgian
  "ko" => "한국어", # Korean
  "nb" => "Norsk Bokmål", # Norwegian Bokmål
  "nl" => "Nederlands", # Dutch (Netherlands)
  "pl" => "Polski", # Polish
  "pt" => "Português", # Portuguese
  "pt_BR" => "Português do Brasil", # Brazilian Portuguese
  "ro" => "Română", # Romanian
  "ru" => "Pусский", # Russian
  "si" => "සිංහල", # Sinhala/Sinhalese
  "sk" => "Slovenčina",#/Slovenský Jazyk", # Slovak/Slovakian
  "sl" => "Slovenščina",#/Slovenski Jezik", # Slovenian
  "sv" => "Svenska", # Swedish
  "sw" => "Kiswahili", # Swahili
  "th" => "ไทย", # Thai
  "tr" => "Türkçe", # Turkish
  "ug" => "ئۇيغۇر تىلى", # Uyghur
  "uk" => "Українська", # Ukranian
  "vi" => "Tiếng Việt", # Vietnamese
  "zh" => "中文", # Chinese
  "zh-Hans" => "简体中文", # Chinese (Simplified)
  "zh_HK" => "廣東話", # Chinese (Traditional, Hong Kong)
  "zh_TW" => "臺灣華語" # Chinese (Traditional, Taiwan)
]

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
  return "" if msgid == nil

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

def convert_element(filename, el, bullet = nil)
  case el.type
  when :root, :li, :ul, :ol
    i = 0
    while i < el.children.count
      b = case el.type
          when :ul
            '*'
          when :ol
            "#{i + 1}."
          when :li
            bullet
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

  when :html_element, :table
    # do nothing
  else
    raise "Error #{filename}: Please implement conversion for unknown Kramdown element type :#{el.type} in line #{el.options[:location]}"
  end
end


# ------------ main ------------

$task = nil

OptionParser.new do |opts|
  opts.banner = "Usage: translate-tutorial.rb [options]\nTranslates the Sonic Pi tutorial."
  opts.on('-x', '--extract', 'creates .pot file from English tutorial (similar to xgettext)') do
    $task = :extract
  end
  opts.on('-t', '--translate', 'translate English tutorial to all languages') do
    $task = :translate
  end
  opts.on('-u', '--update', 'update translation files with current English tutorial (similar to msgmerge)') do
    $task = :update
  end

  #opts.on('--add-lang', 'add a .po file for the specified language if it doesn\'t exist') do
  #  $task = :add_lang
  #end
end.parse!

if $task == nil then
  puts "Usage: translate-tutorial.rb [options]\nTranslates the Sonic Pi tutorial."
  exit
end

if $task == :extract then
  lang = ['en']
else
  lang =
    Dir[File.expand_path("#{@etc_path}/doc/translations/tutorial/sonic-pi-tutorial-*.po", __dir__)].
    map { |p| File.basename(p).gsub(/sonic-pi-tutorial-(.*?).po/, '\1') }.sort
  puts(@etc_path)
  puts(lang.to_s())
end

def process_section(section, sources, translation_dir, out_dir, lang)
  lang.each do |l|

    po_filename  = File.expand_path("#{translation_dir}/sonic-pi-#{section}-#{l}.po", __dir__)
    pot_filename = File.expand_path("#{translation_dir}/sonic-pi-#{section}.pot", __dir__)

    output_dir = File.expand_path(out_dir.sub("%lang%", l), __dir__)

    if $task == :update then

      raise "no .pot file, run 'i18n-tool.rb --extract' first" unless File.exist?pot_filename
      FileUtils.touch po_filename
      cmdline = ['--update', '--no-obsolete-entries', po_filename, pot_filename]
      GetText::Tools::MsgMerge.run(*cmdline)
      $stderr.puts "Merged #{section} translation #{l}"

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
        parser.parse_file(po_filename, $po)
      end

      Dir[File.expand_path(sources, __dir__)].sort.each do |path|
        puts "Parsing #{path}" if $task == :extract
        basename = File.basename(path)
        $translated[basename] = ""

        content = IO.read(path, :encoding => 'utf-8')
        content = content.to_s

        if (section == "tutorial")
          # GitHub markdown syntax uses ```` to mark code blocks Kramdown uses ~~~~
          # Therefore, let's fix-point on GitHub syntax, and fudge it
          # into Kramdown syntax where necessary
          content.gsub!(/\`\`\`\`*/, '~~~~')
          k = Kramdown::Document.new(content)
          convert_element(basename, k.root)
        elsif (section == "reference")
          reference_hash = JSON.parse(content)

          # Translate translatable parts...
          case basename
          when "lang.json"
            reference_hash.each do |k, v|
              reference_hash[k]["description"] = handle_entry(v["description"], basename, "#{k}:description")
              reference_hash[k]["summary"] = handle_entry(v["summary"], basename, "#{k}:summary")
              v["examples"].each_with_index do |e, idx|
                reference_hash[k]["examples"][idx]["comments"] = handle_entry(e["comments"], basename, "#{k}:examples:#{idx}:comments")
              end
              reference_hash[k]["usage"]["args"].each do |arg, type|
                reference_hash[k]["usage"]["args"][arg] = handle_entry(type, basename, "_types:#{type}")
              end
              reference_hash[k]["opts"].each do |opt_name, opt_value|
                reference_hash[k]["opts"][opt_name]["description"] = handle_entry(opt_value["description"], basename, "#{k}:opts:#{opt_name}:description")
              end
            end
          when "synths.json"
            reference_hash.each do |k, v|
              reference_hash[k]["description"] = handle_entry(v["description"], basename, "#{k}:description")
              #reference_hash[k]["summary"] = handle_entry(v["summary"], basename, "#{k}:summary")
              #v["examples"].each_with_index do |e, idx|
              #  reference_hash[k]["examples"][idx]["comments"] = handle_entry(e["comments"], basename, "#{k}:examples:#{idx}:comments")
              #end
              #reference_hash[k]["usage"]["args"].each do |arg, type|
              #  reference_hash[k]["usage"]["args"] = handle_entry(type, basename, "_types:#{type}")
              #end
              reference_hash[k]["opts"].each do |opt_name, opt_value|
                reference_hash[k]["opts"][opt_name]["description"] = handle_entry(opt_value["description"], basename, "#{k}:opts:#{opt_name}:description")
                #opt_value["constraints"].each_with_index do |constraint, idx|
                #  puts "constraint: " + constraint
                #  puts idx
                #  reference_hash[k]["opts"][opt_name]["constraints"][idx] = handle_entry(constraint, basename, "_arg_constraints:#{constraint.sub(" ", "_")}")
                #end
              end
            end
          when "fx.json"
            reference_hash.each do |k, v|
              reference_hash[k]["description"] = handle_entry(v["description"], basename, "#{k}:description")
              #reference_hash[k]["summary"] = handle_entry(v["summary"], basename, "#{k}:summary")
              #v["examples"].each_with_index do |e, idx|
              #  reference_hash[k]["examples"][idx]["comments"] = handle_entry(e["comments"], basename, "#{k}:examples:#{idx}:comments")
              #end
              #reference_hash[k]["usage"]["args"].each do |arg, type|
              #  reference_hash[k]["usage"]["args"] = handle_entry(type, basename, "_types:#{type}")
              #end
              reference_hash[k]["opts"].each do |opt_name, opt_value|
                reference_hash[k]["opts"][opt_name]["description"] = handle_entry(opt_value["description"], basename, "#{k}:opts:#{opt_name}:description")
                #opt_value["constraints"].each_with_index do |constraint, idx|
                #  reference_hash[k]["opts"][opt_name]["constraints"][idx] = handle_entry(constraint, basename, "_arg_constraints:#{constraint.sub(" ", "_")}")
                #end
              end
            end
          when "samples.json"
            reference_hash.each do |k, v|
              reference_hash[k]["description"] = handle_entry(v["description"], basename, "#{k}:description")
            end
          end

          $translated[basename] = JSON.pretty_generate(reference_hash)
        end


      end

      case $task
      when :translate
        FileUtils::rm_rf output_dir
        FileUtils::mkdir_p output_dir
        $translated.each do |filename, newcontent|
          File.open(File.expand_path("#{output_dir}/#{filename}", __dir__), 'w') do |f|
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
        $stderr.puts "Translated #{section} #{l}: #{format("%.1f", pt)}% ready, #{format("%.1f", pf)}% fuzzy."

      when :extract
        File.open(pot_filename, 'w') do |f|
          $stderr.puts "Writing #{pot_filename}"
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
end

puts "tutorial"
process_section("tutorial", File.expand_path("../../../../../etc/doc/tutorial/*.md", __dir__), File.expand_path("../../../../../etc/doc/translations/tutorial/", __dir__), "../../../../../etc/doc/generated/%lang%/tutorial/", lang)
puts "reference"
process_section("reference", File.expand_path("../../../../../etc/doc/reference/*.json", __dir__), File.expand_path("../../../../../etc/doc/translations/reference/", __dir__), "../../../../../etc/doc/generated/%lang%/reference/", lang)
