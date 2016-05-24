#!/usr/bin/ruby
# hellolib.rb
#
# Copyright (C) 2005-2009  Masao Mutoh
#
# This file is distributed under the same
# license as gettext.
#

require 'gettext'

class HelloLib
  include GetText
  bindtextdomain("hellolib", "locale")
  def hello
    _("This message is from hellolib.")
  end
end
