# -*- coding: utf-8 -*-

include GetText

bindtextdomain("test1", :path => "locale")
module TopLevelModule
  module_function
  def module_function
    _("language")
  end
end

class TopLevelClass
  def instance_method
    _("language")
  end
  def self.class_method
    _("language")
  end
end

def toplevel_method
  _("language")
end

class TestTextDomainToplevel < Test::Unit::TestCase
  include GetText

  def teardown
    GetText.locale = nil
  end

  def test_toplevel
    GetText.locale = "ja"
    assert_equal("japanese", toplevel_method)
    assert_equal("japanese", TopLevelModule.module_function)
    assert_equal("japanese", TopLevelClass.class_method)
    assert_equal("japanese", TopLevelClass.new.instance_method)

    GetText.bindtextdomain("test1", :path => "locale")
    assert_equal("japanese", toplevel_method)
    assert_equal("japanese", TopLevelModule.module_function)
    assert_equal("japanese", TopLevelClass.class_method)
    assert_equal("japanese", TopLevelClass.new.instance_method)
  end
end
