# -*- coding: utf-8 -*-

require 'fixtures/multi_text_domain'

class TestGetTextMulti < Test::Unit::TestCase
  include MultiTextDomain

  def setup
    GetText.locale = "ja"
  end

  def teardown
    GetText.locale = nil
  end

  def test_two_domains_in_a_class
    test = C11.new
    assert_equal("japanese", test.test)  # Use test1.po
    assert_equal("JAPANESE", test.test2) # Use test2.po

    test = C12.new
    assert_equal("japanese", test.test)  # Use test1.po
    assert_equal("JAPANESE", test.test2) # Use test2.po
  end

  def test_inheritance
    # inheritance. only parent has a text domain and it's methods
    test = C21.new
    assert_equal("japanese", test.test)   # Use C11's po(test1.po)
    assert_equal("JAPANESE", test.test2)  # Use C11's po(test2.po)

    test = C22.new
    assert_equal("japanese", test.test)   # Use C11's po(test1.po)
    assert_equal("JAPANESE", test.test2)  # Use C11's po(test2.po)
  end

  def test_module_and_sub_modules
    # module
    assert_equal("japanese", M1.test)

    # sub-module. only an included module has a text domain and it's methods
    assert_equal("japanese", M1::M1M1.test)   # Same method with M1.
    assert_equal("LANGUAGE", M1::M1M1.test2)  # No influence from ancestors.

     # sub-class (class bindtextdomain).
    test = M1::M1C1.new
    assert_equal("japanese", test.test)   # Use test1.po
    assert_equal("JAPANESE", test.test2)  # Use test2.po

   # sub-class (instance bindtextdomain).
    test = M1::M1C2.new
    assert_equal("japanese", test.test)   # Use test1.po
    assert_equal("JAPANESE", test.test2)  # Use test2.po
  end

  def test_eval
    test = C2.new
    assert_equal("japanese", test.test)   # Use test1.po
  end

  def test_as_class_methods
    test = C3.new
    assert_equal("japanese", test.test)   # Use test1.po
    assert_equal("japanese", C3.test)     # Use test1.po
  end

  def test_simple_inheritance
    test = C4.new
    assert_equal("japanese", test.test)   # Use C3's test1.po
    assert_equal("japanese", C4.test)     # Use C3's test1.po
    assert_equal("JAPANESE", test.test2)  # Use C4's test2.po
    assert_equal("no data", test.test3)   # No po file.
  end

  def test_same_msgid_but_different_text_domain
    test1 = C12.new  # test1 domain
    test2 = C51.new  # test3 domain
    test3 = C52.new  # test3 domain but inherited C11.

    assert_equal("japanese", test1.test)  # Use text1 message
    assert_equal("JAPANESE", test2.test)  # Use text3 message
    assert_equal("JAPANESE", test3.test)  # Use text3 message
  end

end
