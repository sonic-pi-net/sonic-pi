# -*- coding: utf-8 -*-

class Foo
end

class TestGetTextBind < Test::Unit::TestCase
  def setup
    GetText.locale = "ja_JP.EUC-JP"
    @dumped_all_text_domains = GetText::TextDomainManager.dump_all_text_domains
    GetText::TextDomainManager.clear_all_text_domains
  end

  def teardown
    GetText::TextDomainManager.restore_all_text_domains(@dumped_all_text_domains)
  end

  def test_bindtextdomain
    domain = GetText.bindtextdomain("foo")
    assert_equal domain, GetText::TextDomainManager.create_or_find_text_domain_group(Object).text_domains[0]
    assert_equal domain, GetText::TextDomainManager.text_domain_pool("foo")
  end

  def test_textdomain
    domain1 = GetText.bindtextdomain("foo")

    assert_equal domain1, GetText.textdomain("foo")

    assert_raise(GetText::NoboundTextDomainError) {
      GetText.textdomain_to(Foo, "bar")
    }
  end

  def test_textdomain_to
    domain1 = GetText.bindtextdomain("foo")

    assert_equal domain1, GetText.textdomain_to(Foo, "foo")

    assert_raise(GetText::NoboundTextDomainError) {
      GetText.textdomain_to(Foo, "bar")
    }
  end
end
