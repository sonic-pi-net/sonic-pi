# -*- coding: utf-8 -*-

require 'gettext'

class UnTranslated
  include GetText
  bindtextdomain("untranslated", :path => "locale")

  def untranslated
    _("untranslated")
  end
end
