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

require_relative "../core.rb"
require_relative "../sonicpi/synthinfo"
require_relative "../sonicpi/util"
require_relative "../sonicpi/spiderapi"
require_relative "../sonicpi/mods/sound"

include SonicPi::Util

docs = []
filenames = []
count = 0

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: qt-doc.rb [options]"

  opts.on('-o', '--output NAME', 'Output filename') { |v| options[:output_name] = v }

end.parse!

# valid names: lang, synths, fx, samples, examples
make_tab = lambda do |name, doc_items|

  list_widget = "#{name}NameList"
  layout = "#{name}Layout"
  tab_widget = "#{name}TabWidget"
  help_pages = "#{name}HelpPages"

  docs << "  // #{name} info\n"
  docs << "\n"

  docs << "  QListWidget *#{list_widget} = "
  docs << "createHelpTab(#{name}DocPane, \"#{name.capitalize}\");\n"
  docs << "\n"
  docs << "\n  struct help_page #{help_pages}[] = {\n"

  doc_items.each do |n, doc|

    item_var = "#{name}_item_#{count+=1}"
    filename = "help/#{item_var}.html"

    docs << "    { \"#{n}\", \":/#{filename}\" },\n"

    filenames << "    <file>#{filename}</file>\n"

    File.open("#{qt_gui_path}/#{filename}", 'w') do |f|
      f << "#{doc}"
    end

  end

  docs << "  };\n\n"
  docs << "  helpPagesCount = sizeof(#{help_pages}) / sizeof(struct help_page);\n"
  docs << "  addHelpPage(#{list_widget}, #{help_pages}, helpPagesCount);\n\n"

  docs
end

example_html_map = {}
Dir["#{examples_path}/*.rb"].each do |path|
  name = File.basename(path, ".rb")
  lines = IO.readlines(path).map(&:chop).map{|s| CGI.escapeHTML(s)}
  html = "<pre>\n\n#{lines.join("\n")}\n\n</pre>\n"
  example_html_map[name] = html
end

ruby_html_map = {
  "n.times" => "Loop n times",
  "loop" => "Loop forever",
}

make_tab.call("synths", SonicPi::SynthInfo.synth_doc_html_map)
make_tab.call("fx", SonicPi::SynthInfo.fx_doc_html_map)
make_tab.call("samples", SonicPi::SynthInfo.samples_doc_html_map)
make_tab.call("lang", SonicPi::SpiderAPI.docs_html_map.merge(SonicPi::Mods::Sound.docs_html_map).merge(ruby_html_map))
make_tab.call("examples", example_html_map)

# update mainwindow.cpp
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
new_content << "void MainWindow::initDocsWindow() {\n  int helpPagesCount;\n"
new_content += docs
new_content << "}\n"

File.open(cpp, 'w') do |f|
  f << new_content.join
end

File.open("#{qt_gui_path}/help_files.qrc", 'w') do |f|
  f << "<RCC>\n  <qresource prefix=\"/\">\n"
  f << filenames.join
  f << "  </qresource>\n</RCC>\n"
end
