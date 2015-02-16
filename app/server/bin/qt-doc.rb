#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

require 'cgi'
require 'optparse'
require 'fileutils'

require_relative "../core.rb"
require_relative "../sonicpi/lib/sonicpi/synthinfo"
require_relative "../sonicpi/lib/sonicpi/util"
require_relative "../sonicpi/lib/sonicpi/spiderapi"
require_relative "../sonicpi/lib/sonicpi/mods/sound"

require 'kramdown'
require 'active_support/inflector'

# i18n not enabled until translations are ready
enable_i18n = false

class MarkdownConverter
  def self.convert(contents)
    # GitHub markdown syntax uses ```` to mark code blocks Kramdown uses ~~~~
    # Therefore, let's fix-point on GitHub syntax, and fudge it
    # into Kramdown syntax where necessary
    contents.gsub!(/\`\`\`\`*/, '~~~~')

    # todo: CSS
    contents_html = Kramdown::Document.new(contents).to_html
    massage!(contents_html)
  end

  def self.massage!(html)
    html.gsub!(/<h1.*?>/, '<p> <span style="font-size:25px; color:white;background-color:deeppink;">')
    html.gsub!(/<h2.*?>/, '<br><p><span style="font-size:20px; color:white; background-color:dodgerblue;">')
    html.gsub!(/<\/h1>/, '</span></p>')
    html.gsub!(/<\/h2>/, '</span></p>')
    html.gsub!(/<p>/, '<p style="font-size:15px;color:#5e5e5e;">')
    html.gsub!(/<em>/, '<em style="font-size:15px;color:darkorange;">')
    html.gsub!(/<ol>/, '<ol style="font-size:15px;color:#5e5e5e;">')
    html.gsub!(/<ul>/, '<ul style="font-size:15px;color:#5e5e5e;">')

    html.gsub!(/<code>/, '<code style="font-size:15px; color:deeppink; background-color:white">')
    html.gsub!(/<a href/, '<a style="text-decoration: none; color:dodgerblue;" href')
    "<font face=\"HelveticaNeue-Light,Helvetica Neue Light,Helvetica Neue\">\n\n" + html + "</font>"
  end
end

include SonicPi::Util

FileUtils::rm_rf "#{qt_gui_path}/help/"
FileUtils::mkdir "#{qt_gui_path}/help/"

FileUtils::rm_rf "#{qt_gui_path}/info/"
FileUtils::mkdir "#{qt_gui_path}/info/"

docs = []
filenames = []
count = 0

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: qt-doc.rb [options]"

  opts.on('-o', '--output NAME', 'Output filename') { |v| options[:output_name] = v }

end.parse!

# valid names: lang, synths, fx, samples, examples
make_tab = lambda do |name, doc_items, titleize=false, should_sort=true, with_keyword=false|
  return if doc_items.empty?
  list_widget = "#{name}NameList"
  layout = "#{name}Layout"
  tab_widget = "#{name}TabWidget"
  help_pages = "#{name}HelpPages"

  docs << "\n"
  docs << "  // #{name} info\n"

  docs << "  struct help_page #{help_pages}[] = {\n"
  doc_items = doc_items.sort if should_sort
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

    docs << "    { "

    docs << "QString::fromUtf8(" unless title.ascii_only?
    docs << "\"#{title}\""
    docs << ")" unless title.ascii_only?

    docs << ", "
    
    if with_keyword then
      docs << "\"#{n.downcase}\""
    else
      docs << "NULL"
    end

    docs << ", "
    docs << "\":/#{filename}\""
    docs << "},\n"

    filenames << filename

    File.open("#{qt_gui_path}/#{filename}", 'w') do |f|
      f << "#{doc}"
    end

  end

  docs << "  };\n\n"
  docs << "  addHelpPage(createHelpTab(tr(\"#{name.capitalize}\")), #{help_pages}, #{doc_items.length});\n\n"

  docs
end


make_tutorial = lambda do |lang|

  docs << "\n  // language #{lang}\n"
  tutorial_html_map = {}
  Dir["#{tutorial_path}/#{lang}/*.md"].sort.each do |path|
    f = File.open(path, 'r:UTF-8')
    # read first line (title) of the markdown, use as title
    name = f.readline.strip
    # indent subchapters
    name = "   #{name}" if name.match(/\A[0-9]+\.[0-9]+/)
    # read remaining content of markdown
    markdown = f.read
    html = MarkdownConverter.convert markdown
    tutorial_html_map[name] = html
  end

  make_tab.call("tutorial", tutorial_html_map, false, false)
end


example_html_map = {}
example_dirs = ["Apprentice", "Illusionist", "Magician", "Sorcerer", "Wizard", "Algomancer"]
example_dirs.each do |ex_dir|
  Dir["#{examples_path}/#{ex_dir.downcase}/*.rb"].each do |path|
    bname = File.basename(path, ".rb")
    bname = ActiveSupport::Inflector.titleize(bname)
    name = "[#{ex_dir}] #{bname}"
    lines = IO.readlines(path).map(&:chop).map{|s| CGI.escapeHTML(s)}
    html = '<p> <span style="font-size:25px; color:white;background-color:deeppink;">'
    html << "# #{bname}"
    html << '</span></p>'
    html << "<pre style=\"font-size:18px;color:dodgerblue;\">\n\n"

    html << "#{lines.join("\n")}\n\n</pre>\n"
    example_html_map[name] = html
  end
end

ruby_html_map = {
#  "n.times" => "Loop n times",
#  "loop" => "Loop forever",
}

if enable_i18n then
  # this will sort locale code names by reverse length
  # to make sure that a more specific locale is handled
  # before the generic language code,
  # e.g., "de_CH" should be handled before "de"
  languages = Dir.
    glob("#{tutorial_path}/*").
    select {|f| File.directory? f}.
    map {|f| File.basename f}.
    select {|n| n != "en"}.
    sort_by {|n| -n.length}
else
  languages = []
end

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

make_tab.call("examples", example_html_map, false, false)
make_tab.call("synths", SonicPi::SynthInfo.synth_doc_html_map, :titleize, true, true)
make_tab.call("fx", SonicPi::SynthInfo.fx_doc_html_map, :titleize, true, true)
make_tab.call("samples", SonicPi::SynthInfo.samples_doc_html_map)
make_tab.call("lang", SonicPi::SpiderAPI.docs_html_map.merge(SonicPi::Mods::Sound.docs_html_map).merge(ruby_html_map), false, true, true)

docs << "  // FX arguments for autocompletion\n"
docs << "  QStringList fxtmp;\n"
SonicPi::SynthInfo.get_all.each do |k, v|
  next unless v.is_a? SonicPi::FXInfo
  next if (k.to_s.include? 'replace_')
  safe_k = k.to_s[3..-1]
  docs << "  // fx :#{safe_k}\n"
  docs << "  fxtmp.clear(); fxtmp "
  v.arg_info.each do |ak, av|
    docs << "<< \"#{ak}:\" ";
  end
  docs <<";\n"
  docs << "  autocomplete->addFXArgs(\":#{safe_k}\", fxtmp);\n\n"
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
    html = MarkdownConverter.convert(input)
  else
    html = MarkdownConverter.massage!(input)
  end

  output_path = "#{outputdir}/#{bn}.html"

  File.open(output_path, 'w') do |f|
    f << html
  end

end
