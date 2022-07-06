#!/usr/bin/ruby
# hello2.po - sample for _() and module
#
# Copyright (C) 2002-2004 Masao Mutoh
# This file is distributed under the same license as gettext.

require 'rubygems'
require 'gettext'

module Hello
  include GetText

  base_dir = File.dirname(__FILE__)
  bindtextdomain("hello2", :path => File.join(base_dir, "locale"))

  module_function
  def hello
    num = 1
    puts _("One is %{num}\n") % {:num => num}
    puts _("Hello %{world}\n") % {:world => _("World")}
  end
end

Hello.hello
