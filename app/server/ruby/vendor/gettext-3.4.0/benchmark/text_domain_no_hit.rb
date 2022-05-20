# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
#
# You may redistribute it and/or modify it under the same
# license terms as Ruby or LGPL.

require "benchmark"
require "gettext"

text_domain = GetText::TextDomain.new(nil)
texts = {}
string = "a"
10000.times do
  texts[string] = string
  string = string.succ
end
text_domain.mofiles["ja"] = texts

Benchmark.bmbm do |benchmark|
  benchmark.report("singular: no hit") do
    10.times do
      text_domain.translate_singular_message("ja", " never hit")
    end
  end
end
