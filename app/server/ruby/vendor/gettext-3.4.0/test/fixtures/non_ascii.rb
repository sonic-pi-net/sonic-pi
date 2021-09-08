# -*- coding: utf-8 -*-

require 'gettext'

class NonAscii
  include GetText
  bindtextdomain("non_ascii", :path => "locale")

  def hello_in_japanese
    _("こんにちは")
  end
end
