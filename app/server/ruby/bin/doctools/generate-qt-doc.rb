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

require 'cgi'
require 'optparse'
require 'fileutils'
require 'json'

require_relative "../../core.rb"
require_relative "../../lib/sonicpi/synths/synthinfo"
require_relative "../../lib/sonicpi/util"
require_relative "../../lib/sonicpi/runtime"
require_relative "../../lib/sonicpi/lang/core"
require_relative "../../lib/sonicpi/lang/sound"
require_relative "../../lib/sonicpi/lang/minecraftpi"
require_relative "../../lib/sonicpi/lang/midi"

require 'active_support/inflector'

require_relative "./lang-names"


include SonicPi::Util

FileUtils::rm_rf "#{qt_gui_path}/resources/help/"
FileUtils::mkdir_p "#{qt_gui_path}/resources/help/"

FileUtils::rm_rf "#{qt_gui_path}/resources/info/"
FileUtils::mkdir_p "#{qt_gui_path}/resources/info/"

FileUtils::rm_rf "#{qt_gui_path}/book/"
FileUtils::mkdir "#{qt_gui_path}/book/"

docs = []
@filenames = {
    "tutorial" => [],
    "lang" => [],
    "synths" => [],
    "fx" => [],
    "samples" => [],
    "examples" => []
}
count = 0

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: qt-doc.rb [options]"

  opts.on('-o', '--output NAME', 'Output filename') { |v| options[:output_name] = v }
end.parse!

def parse_toc(lang)
  puts("Generating pages for #{lang}...")

  help_index = ""

  toc = {}
  json_toc = ""
  File.open("#{etc_path}/doc/generated_html/#{lang}/toc.json", 'r:UTF-8') do |f|
    json_toc = f.read()
  end

  toc = JSON.parse(json_toc)

  toc.each do |section, sec_list|
    section_filenames = []

    help_index << "    struct help_page #{section}HelpPages[] = {\n"
    sec_list["pages"].each do |page_info|
      page_index, page_filenames = generate_page_and_subpages(page_info)

      help_index += page_index
      section_filenames += page_filenames
    end
    help_index << "};\n"

    help_index << "addHelpPage(createHelpTab(tr(\"#{section.capitalize}\")), #{section}HelpPages, #{section_filenames.length});\n\n"
    @filenames[section] += section_filenames
  end

  return help_index
end

def generate_page_and_subpages(page_info)
  help_index = ""
  filenames = ["help/#{page_info["path"]}"]

  # Name
  title = page_info["name"]
  help_index << "    {"
  help_index << "QString::fromUtf8(" unless title.ascii_only?
  help_index << "\"#{title.gsub(/"/, '\\"')}\""
  help_index << ")" unless title.ascii_only?
  help_index << ", "

  # Keyword (reference docs)
  if page_info.has_key?("keyword") then
    help_index << "\"#{page_info["keyword"].downcase}\""
  else
    help_index << "NULL"
  end

  # File path
  help_index << ", "
  help_index << "\"qrc:///help/#{page_info["path"]}\""
  help_index << "},\n"

  # Copy file to help folder
  content = IO.readlines("#{etc_path}/doc/generated_html/#{page_info["path"]}")

  html =  "<!doctype html>\n"
  html << "<html>\n"
  html << "<head>\n"
  html << "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n"
  html << "</head>\n"
  html << "<body class=\"manual\">\n"
  html << content.join("") << "\n"
  html << "</body>\n"
  html << "</html>\n"

  FileUtils::mkdir_p(File.dirname("#{qt_gui_path}/resources/help/#{page_info["path"]}"))

  File.open("#{qt_gui_path}/resources/help/#{page_info["path"]}", 'w') do |f|
    f << "#{html}"
  end

  if page_info.has_key?("subpages")
    page_info["subpages"].each do |subpage_info|
      subpage_index, subpage_filenames = generate_page_and_subpages(subpage_info)

      help_index += subpage_index
      filenames += subpage_filenames
    end
  end

  return help_index, filenames
end

# ruby_html_map = {
# #  "n.times" => "Loop n times",
# #  "loop" => "Loop forever",
# }


def synth_fx_autocomplete
  docs = "  // FX arguments for autocompletion\n"
  docs << "  QStringList fxtmp;\n"
  SonicPi::Synths::SynthInfo.get_all.each do |k, v|
    next unless v.is_a? SonicPi::Synths::FXInfo
    next if (k.to_s.include? 'replace_')
    safe_k = k.to_s[3..-1]
    docs << "  // fx :#{safe_k}\n"
    docs << "  fxtmp.clear(); fxtmp "
    v.arg_info.each do |ak, av|
      docs << "<< \"#{ak}:\" ";
    end
    docs << ";\n"
    docs << "  autocomplete->addFXArgs(\":#{safe_k}\", fxtmp);\n\n"
  end


  SonicPi::Synths::SynthInfo.get_all.each do |k, v|
    next unless v.is_a? SonicPi::Synths::SynthInfo
    docs << "  // synth :#{k}\n"
    docs << "  fxtmp.clear(); fxtmp "
    v.arg_info.each do |ak, av|
      docs << "<< \"#{ak}:\" ";
    end
    docs << ";\n"
    docs << "  autocomplete->addSynthArgs(\":#{k}\", fxtmp);\n\n"
  end
  return docs
end

def examples_index
  puts("Generating example pages...")
  help_index = ""

  toc = {}
  json_toc = ""
  File.open("#{etc_path}/doc/generated_html/examples/toc.json", 'r:UTF-8') do |f|
    json_toc = f.read()
  end

  toc = JSON.parse(json_toc)

  toc.each do |section, sec_list|
    page_count = 0

    help_index << "    struct help_page #{section}HelpPages[] = {\n"
    sec_list.each do |page_info|
      # Name
      title = page_info["name"]
      help_index << "    {"
      help_index << "QString::fromUtf8(" unless title.ascii_only?
      help_index << "\"#{title.gsub(/"/, '\\"')}\""
      help_index << ")" unless title.ascii_only?
      help_index << ", "

      # Keyword (reference docs)
      if page_info.has_key?("reference") then
        help_index << "\"#{n.downcase}\""
      else
        help_index << "NULL"
      end

      # File path
      help_index << ", "
      help_index << "\"qrc:///help/#{page_info["path"]}\""
      help_index << "},\n"

      # Copy file to help folder
      content = IO.readlines("#{etc_path}/doc/generated_html/#{page_info["path"]}")

      html =  "<!doctype html>\n"
      html << "<html>\n"
      html << "<head>\n"
      html << "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n"
      html << "</head>\n"
      html << "<body class=\"example\">\n"
      html << content.join("\n") << "\n"
      html << "</body>\n"
      html << "</html>\n"

      FileUtils::mkdir_p(File.dirname("#{qt_gui_path}/resources/help/#{page_info["path"]}"))

      File.open("#{qt_gui_path}/resources/help/#{page_info["path"]}", 'w') do |f|
        f << "#{html}"
      end

      @filenames["examples"] << "help/#{page_info["path"]}"
      page_count += 1
    end
    help_index << "};\n"

    help_index << "addHelpPage(createHelpTab(tr(\"Examples\")), examplesHelpPages, #{page_count});\n\n"
  end

  return help_index
end

def generate_ui_lang_names()
  # Define the language list map -----
  ui_languages = @lang_names.keys
  ui_languages = ui_languages.sort_by {|l| l.downcase}
  locale_arrays = []
  locale_arrays << "std::map<QString, QString> SonicPii18n::native_language_names = {\n"

  # # Add each language
  for i in 0..(ui_languages.length()-1) do
    lang = ui_languages[i]
    locale_arrays << ",\n" if i != 0
    locale_arrays << "{\"#{lang}\", \"#{@lang_names[lang]}\"}"
  end

  # End the map
  locale_arrays << "\n};\n"

  # Write the map to lang_list.h
  content = File.readlines("#{qt_gui_path}/utils/lang_list.tmpl")
  lang_names_generated = content.take_while { |line| !line.start_with?("// AUTO-GENERATED")}
  lang_names_generated << "// AUTO-GENERATED HEADER FILE\n"
  lang_names_generated << "// Do not add any code to this file\n"
  lang_names_generated << "// as it will be removed/overwritten\n"
  lang_names_generated << "\n"
  lang_names_generated << "#ifndef LANG_LIST_H\n"
  lang_names_generated << "#define LANG_LIST_H\n"
  lang_names_generated << "#include <map>\n"
  lang_names_generated << locale_arrays.join()
  lang_names_generated << "#endif\n"

  File.open("#{qt_gui_path}/utils/lang_list.h", 'w') do |f|
    f << lang_names_generated.join()
  end
end

# this will sort locale code names by reverse length
# to make sure that a more specific locale is handled
# before the generic language code,
# e.g., "de_CH" should be handled before "de"

languages =
Dir[File.expand_path("../translations/tutorial/sonic-pi-tutorial-*.po", tutorial_path)].
map { |p| File.basename(p).gsub(/sonic-pi-tutorial-(.*?).po/, '\1') }.
sort_by {|n| -n.length}

docs << "\n"
languages.each do |lang|
  docs << "if (this->ui_language.startsWith(\"#{lang}\")) {\n"
  docs << parse_toc(lang)
  docs << "} else "
end
docs << "{\n" unless (languages.empty?)
docs << parse_toc("en")
docs << "}\n" unless (languages.empty?)

docs << examples_index() << "\n"
docs << synth_fx_autocomplete() << "\n"

# update ruby_help.h
if options[:output_name]
  cpp = options[:output_name]
else
  cpp = "#{qt_gui_path}/utils/ruby_help.h"
end

content = File.readlines(cpp)
new_content = content.take_while { |line| !line.start_with?("// AUTO-GENERATED-DOCS")}
new_content << "// AUTO-GENERATED-DOCS\n"
new_content << "// Do not manually add any code below this comment\n"
new_content << "// otherwise it may be removed\n"
new_content << "\n"
new_content << "void MainWindow::initDocsWindow() {\n"
new_content << docs << "\n"
new_content << "}\n"

File.open(cpp, 'w') do |f|
  f << new_content.join
end

# Write .qrc files (split into sections)
puts("Writing .qrc files...")
FileUtils.mkdir_p("#{qt_gui_path}/resources/")
@filenames.each { |section,pages|
  #puts section
  #puts pages.to_s
  File.open("#{qt_gui_path}/resources/help_files_#{section.to_s}.qrc", 'w') do |f|
    f << "<RCC>\n  <qresource prefix=\"/\">\n"
    f << pages.map{|n| "    <file>#{n}</file>\n"}.join
    f << "  </qresource>\n</RCC>\n"
  end
}

###
# Generate info pages
###

info_sources = ["CHANGELOG.md", "CONTRIBUTORS.md", "COMMUNITY.md", "CORETEAM.html", "LICENSE.md"]
outputdir = "#{qt_gui_path}/resources/info"

info_sources.each do |src|

  input_path = "#{root_path}/#{src}"
  base = File.basename(input_path)
  m = base.match /(.*)\.(.*)/
  bn = m[1]
  ext = m[2]

  input = IO.read(input_path, :encoding => 'utf-8')
  if ext == "md"
    html = SonicPi::MarkdownConverter.convert(input)
  else
    html = SonicPi::MarkdownConverter.massage!(input)
  end

  output_path = "#{outputdir}/#{bn}.html"

  File.open(output_path, 'w') do |f|
    f << html
  end

end

File.open("#{qt_gui_path}/resources/info_files.qrc", 'w') do |f|
  f << "<RCC>\n  <qresource prefix=\"/\">\n"
  f << info_sources.map{|n| "    <file>info/#{n.gsub(".md", ".html")}</file>\n"}.join
  f << "  </qresource>\n</RCC>\n"
end

generate_ui_lang_names()
