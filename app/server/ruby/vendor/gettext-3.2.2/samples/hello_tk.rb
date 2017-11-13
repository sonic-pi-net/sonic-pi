#!/usr/bin/ruby
# hello_tk.rb - sample for Ruby/TK
#
# Copyright (C) 2004 Masao Mutoh
# This file is distributed under the same license as gettext.

require 'rubygems'
require 'gettext'
require 'tk'

include GetText
base_dir = File.dirname(__FILE__)
bindtextdomain("hello_tk", :path => File.join(base_dir, "locale"))

TkLabel.new {
  text _("hello, tk world")
  pack
}

Tk.mainloop
