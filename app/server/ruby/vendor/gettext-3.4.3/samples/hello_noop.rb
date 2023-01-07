#!/usr/bin/ruby
# hello_noop.rb - sample for N_() and class.
#
# Copyright (C) 2002-2006 Masao Mutoh
# This file is distributed under the same license as gettext.

require 'rubygems'
require 'gettext'

class HelloNoop
  include GetText

  MSGS = [N_("Hello World"), N_("Hello World2")]

  def initialize
    # You can call bindtextdomain as instance methods.
    # In this case, it initializes(decided the locale lazily)
    # in a instance.
    base_dir = File.dirname(__FILE__)
    bindtextdomain("hello_noop", :path => File.join(base_dir, "locale"))
  end

  def hello
    MSGS.each do |msg|
      print _(msg), "\n"
    end
  end
end

hello = HelloNoop.new

hello.hello
