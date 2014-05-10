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

  docs << "// #{name} info\n"
  docs << "\n"

  docs << "QListWidget *#{list_widget} = new QListWidget;\n"
  docs << "#{list_widget}->setSortingEnabled(true);\n"

  docs << "connect(#{list_widget}, SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)), this, SLOT(updateDocPane(QListWidgetItem*, QListWidgetItem*)));\n"

  docs << "QBoxLayout *#{layout} = new QBoxLayout(QBoxLayout::LeftToRight);\n"
  docs << "#{layout}->addWidget(#{list_widget});\n"
  docs << "#{layout}->addWidget(#{name}DocPane);\n"
  docs << "#{layout}->setStretch(1, 1);\n"
  docs << "QWidget *#{tab_widget} = new QWidget;\n"
  docs << "#{tab_widget}->setLayout(#{layout});\n"
  docs << "docsCentral->addTab(#{tab_widget}, \"#{name.capitalize}\");\n"
  docs << "\n"

  doc_items.each do |n, doc|

    item_var = "#{name}_item_#{count+=1}"
    docs << "QListWidgetItem *#{item_var} = new QListWidgetItem(\"#{n}\");\n"
    docs << "#{item_var}->setData(32, QVariant(\"#{doc}\"));\n"
    docs << "#{list_widget}->addItem(#{item_var});\n"
    docs << "\n"

  end

  docs
end

example_html_map = {}
Dir["#{examples_path}/*.rb"].each do |path|
  name = File.basename(path, ".rb")
  lines = IO.readlines(path).map(&:chop).map{|s| CGI.escapeHTML(s)}
  html = "<pre>#{lines.join('<br/>')} </pre>"
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
   cpp = "#{qt_gui_path}/mainwindow.cpp"
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
