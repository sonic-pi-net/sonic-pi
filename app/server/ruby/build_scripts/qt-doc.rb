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

require 'cgi'
require 'optparse'
require 'fileutils'

require_relative "../core.rb"
require_relative "../lib/sonicpi/synths/synthinfo"
require_relative "../lib/sonicpi/util"
require_relative "../lib/sonicpi/runtime"
require_relative "../lib/sonicpi/lang/core"
require_relative "../lib/sonicpi/lang/sound"
require_relative "../lib/sonicpi/lang/minecraftpi"
require_relative "../lib/sonicpi/lang/midi"

require 'active_support/inflector'


include SonicPi::Util

FileUtils::rm_rf "#{qt_gui_path}/help/"
FileUtils::mkdir "#{qt_gui_path}/help/"

FileUtils::rm_rf "#{qt_gui_path}/info/"
FileUtils::mkdir "#{qt_gui_path}/info/"

FileUtils::rm_rf "#{qt_gui_path}/book/"
FileUtils::mkdir "#{qt_gui_path}/book/"

docs = []
filenames = []
count = 0

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: qt-doc.rb [options]"

  opts.on('-o', '--output NAME', 'Output filename') { |v| options[:output_name] = v }

end.parse!

# valid names: lang, synths, fx, samples, examples
make_tab = lambda do |name, doc_items, titleize=false, should_sort=true, with_keyword=false, page_break=false, chapters=false, lang="en"|
  return if doc_items.empty?
  list_widget = "#{name}NameList"
  layout = "#{name}Layout"
  tab_widget = "#{name}TabWidget"
  help_pages = "#{name}HelpPages"

  docs << "\n"
  docs << "  // #{name} info\n"

  docs << "  struct help_page #{help_pages}[] = {\n"
  doc_items = doc_items.sort if should_sort

  book = ""
  toc = "<ul class=\"toc\">\n"
  toc_level = 0

  doc_items.each do |n, doc|
    title = n
    if titleize == :titleize then
      title = ActiveSupport::Inflector.titleize(title)
      # HPF et al get capitalized
      if name == 'fx' and title =~ /pf$/ then
        title = title.upcase
      end
    end

    item_var = "#{name}_item_#{count+=1}"
    filename = "help/#{item_var}.html"

    if title.start_with?("   ") then
      if toc_level == 0 then
        toc << "<ul class=\"toc\">\n"
        toc_level += 1
      end
    else
      if toc_level == 1 then
        toc << "</ul>\n"
        toc_level -= 1
      end
    end
    toc << "<li><a href=\"\##{item_var}\">#{title.gsub(/"/, '&quot;')}</a></li>\n"

    docs << "    { "

    docs << "QString::fromUtf8(" unless title.ascii_only?
    docs << "\"#{title.gsub(/"/, '\\"')}\""
    docs << ")" unless title.ascii_only?

    docs << ", "

    if with_keyword then
      docs << "\"#{n.downcase}\""
    else
      docs << "NULL"
    end

    docs << ", "
    docs << "\"qrc:///#{filename}\""
    docs << "},\n"

    filenames << filename

    File.open("#{qt_gui_path}/#{filename}", 'w') do |f|
      f << "#{doc}"
    end

    if chapters then
      c = title[/\A\s*[0-9]+(\.[0-9]+)?/]
      doc.gsub!(/(<h1.*?>)/, "\\1#{c} - ")
    end
    if page_break then
      doc.gsub!(/<h1.*?>/, "<h1 id=\"#{item_var}\" style=\"page-break-before: always;\">")
    else
      doc.gsub!(/<h1.*?>/, "<h1 id=\"#{item_var}\">")
    end
    book << doc
    book << "<hr/>\n"
  end

  while toc_level >= 0 do
    toc << "</ul>\n"
    toc_level -= 1
  end

  book_body = book[/<body.*?>/]
  book.gsub!(/<\/?body.*?>/, '')
  book.gsub!(/<meta http-equiv.*?>/, '')
  File.open("#{qt_gui_path}/book/Sonic Pi - #{name.capitalize}" + (lang != "en" ? " (#{lang})" : "") + ".html", 'w') do |f|
    f << "<link rel=\"stylesheet\" href=\"../theme/light/doc-styles.css\" type=\"text/css\"/>\n"
    f << "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n"
    f << book_body << "\n"
    f << toc << "\n"
    f << book << "\n"
    f << "</body>\n"
  end

  docs << "  };\n\n"
  docs << "  addHelpPage(createHelpTab(tr(\"#{name.capitalize}\")), #{help_pages}, #{doc_items.length});\n\n"

  docs
end


make_tutorial = lambda do |lang|

  docs << "\n  // language #{lang}\n"
  tutorial_html_map = {}
  if lang == "en" then
    markdown_path = tutorial_path
  else
    markdown_path = File.expand_path("../generated/#{lang}/tutorial", tutorial_path)
  end
  Dir["#{markdown_path}/*.md"].sort.each do |path|
    f = File.open(path, 'r:UTF-8')
    # read first line (title) of the markdown, use as title
    name = f.readline.strip
    # indent subchapters
    name = "   #{name}" if name.match(/\A[A-Z0-9]+\.[0-9]+ /)
    # read remaining content of markdown
    markdown = f.read
    html = SonicPi::MarkdownConverter.convert markdown
    tutorial_html_map[name] = html
  end

  make_tab.call("tutorial", tutorial_html_map, false, false, false, true, true, lang)
end


example_html_map = {}
example_dirs = ["Apprentice", "Illusionist", "Magician", "Sorcerer", "Wizard", "Algomancer"]
example_dirs.each do |ex_dir|
  Dir["#{examples_path}/#{ex_dir.downcase}/*.rb"].each do |path|
    bname = File.basename(path, ".rb")
    bname = ActiveSupport::Inflector.titleize(bname)
    name = "[#{ex_dir}] #{bname}"
    lines = IO.readlines(path).map(&:chop).map{|s| CGI.escapeHTML(s)}
    html = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n"
    html << "<body class=\"example\">\n"
    html << '<h1>'
    html << "# #{bname}"
    html << '</h1>'
    html << "<p><pre><code>\n"

    html << "#{lines.join("\n")}\n\n</code></pre></p>\n"
    html << "</body>\n"
    example_html_map[name] = html
  end
end

ruby_html_map = {
#  "n.times" => "Loop n times",
#  "loop" => "Loop forever",
}

# this will sort locale code names by reverse length
# to make sure that a more specific locale is handled
# before the generic language code,
# e.g., "de_CH" should be handled before "de"
languages =
  Dir[File.expand_path("../lang/sonic-pi-tutorial-*.po", tutorial_path)].
  map { |p| File.basename(p).gsub(/sonic-pi-tutorial-(.*?).po/, '\1') }.
  sort_by {|n| -n.length}

docs << "\n  QString systemLocale = QLocale::system().name();\n\n" unless languages.empty?

# first, try to match all non-default languages (those that aren't "en")
languages.each do |lang|
  docs << "if (systemLocale.startsWith(\"#{lang}\")) {\n"
  make_tutorial.call(lang)
  docs << "} else "
end

# finally, add the default language ("en")
docs << "{\n" unless (languages.empty?)
make_tutorial.call("en")
docs << "}\n" unless (languages.empty?)

make_tab.call("examples", example_html_map, false, false, false, true)
make_tab.call("synths", SonicPi::Synths::SynthInfo.synth_doc_html_map, :titleize, true, true, true)
make_tab.call("fx", SonicPi::Synths::SynthInfo.fx_doc_html_map, :titleize, true, true, true)
make_tab.call("samples", SonicPi::Synths::SynthInfo.samples_doc_html_map, false, true, false, true)
make_tab.call("lang", SonicPi::Lang::Core.docs_html_map.merge(SonicPi::Lang::Sound.docs_html_map).merge(ruby_html_map), false, true, true, false)

docs << "  // FX arguments for autocompletion\n"
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


# update ruby_help.h
if options[:output_name] then
   cpp = options[:output_name]
else
   cpp = "#{qt_gui_path}/ruby_help.h"
end

content = File.readlines(cpp)
new_content = content.take_while { |line| !line.start_with?("// AUTO-GENERATED-DOCS")}
new_content << "// AUTO-GENERATED-DOCS\n"
new_content << "// Do not manually add any code below this comment\n"
new_content << "// otherwise it may be removed\n"
new_content << "\n"
new_content << "void MainWindow::initDocsWindow() {\n"
new_content += docs
new_content << "}\n"

File.open(cpp, 'w') do |f|
  f << new_content.join
end

File.open("#{qt_gui_path}/help_files.qrc", 'w') do |f|
  f << "<RCC>\n  <qresource prefix=\"/\">\n"
  f << filenames.map{|n| "    <file>#{n}</file>\n"}.join
  f << "  </qresource>\n</RCC>\n"
end


###
# Generate info pages
###

info_sources = ["CHANGELOG.md", "CONTRIBUTORS.md", "COMMUNITY.md", "CORETEAM.html", "LICENSE.md"]
outputdir = "#{qt_gui_path}/info"

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
