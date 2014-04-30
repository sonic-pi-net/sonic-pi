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

require_relative "../core.rb"
require_relative "../sonicpi/synthinfo"
require_relative "../sonicpi/util"

include SonicPi::Util

docs = []
count = 0

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

make_tab.call("lang", {:foo => "this is foo", :bar => "this is bar"})
make_tab.call("synths", SonicPi::SynthInfo.synth_doc_html_map)
make_tab.call("fx", SonicPi::SynthInfo.fx_doc_html_map)
make_tab.call("samples", SonicPi::SynthInfo.samples_doc_html_map)

# update mainwindow.cpp
cpp = "#{qt_gui_path}/mainwindow.cpp"
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
