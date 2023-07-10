require 'test_helper'

class I18nUtilsTest < I18n::TestCase

  test ".deep_symbolize_keys" do
    hash = { 'foo' => { 'bar' => { 'baz' => 'bar' } } }
    expected = { :foo => { :bar => { :baz => 'bar' } } }
    assert_equal expected, I18n::Utils.deep_symbolize_keys(hash)
  end

  test "#deep_symbolize_keys with numeric keys" do
    hash = { 1 => { 2 => { 3 => 'bar' } } }
    expected = { 1 => { 2 => { 3 => 'bar' } } }
    assert_equal expected, I18n::Utils.deep_symbolize_keys(hash)
  end

  test "#except" do
    hash = { :foo => 'bar',  :baz => 'bar' }
    expected = { :foo => 'bar' }
    assert_equal expected, I18n::Utils.except(hash, :baz)
  end

  test "#deep_merge!" do
    hash = { :foo => { :bar => { :baz => 'bar' } }, :baz => 'bar' }
    I18n::Utils.deep_merge!(hash, :foo => { :bar => { :baz => 'foo' } })

    expected = { :foo => { :bar => { :baz => 'foo' } }, :baz => 'bar' }
    assert_equal expected, hash
  end
end
