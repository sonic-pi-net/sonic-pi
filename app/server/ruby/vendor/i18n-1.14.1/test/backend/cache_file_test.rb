require 'test_helper'
require 'fileutils'
require 'tempfile'

module CountWrites
  attr_reader :writes

  def initialize(*args)
    super
    @writes = []
  end

  def store_translations(*args)
    super.tap { @writes << args }
  end
end

module CacheFileTest
  test 'load_translations caches loaded file contents' do
    setup_backend!
    I18n.load_path = [locales_dir + '/en.yml']
    assert_equal 0, @backend.writes.count

    @backend.load_translations unless @backend.is_a?(I18n::Backend::Simple)
    assert_equal('baz', I18n.t('foo.bar'))
    assert_equal 2, @backend.writes.count

    @backend.load_translations
    assert_equal('baz', I18n.t('foo.bar'))
    assert_equal 2, @backend.writes.count
  end

  test 'setting path_roots normalizes write key' do
    setup_backend!
    I18n.load_path = [locales_dir + '/en.yml']
    @backend.path_roots = [locales_dir]
    @backend.load_translations
    refute_nil I18n.t("0/en\x01yml", scope: :load_file, locale: :i18n, default: nil)
  end

  test 'load_translations caches file through updated modification time' do
    setup_backend!
    Tempfile.open(['test', '.yml']) do |file|
      I18n.load_path = [file.path]

      File.write(file, { :en => { :foo => { :bar => 'baz' } } }.to_yaml)
      assert_equal 0, @backend.writes.count

      @backend.load_translations unless @backend.is_a?(I18n::Backend::Simple)
      assert_equal('baz', I18n.t('foo.bar'))
      assert_equal 2, @backend.writes.count

      FileUtils.touch(file, :mtime => Time.now + 1)
      @backend.load_translations
      assert_equal('baz', I18n.t('foo.bar'))
      assert_equal 2, @backend.writes.count

      File.write(file, { :en => { :foo => { :bar => 'baa' } } }.to_yaml)
      FileUtils.touch(file, :mtime => Time.now + 1)
      @backend.load_translations
      assert_equal('baa', I18n.t('foo.bar'))
      assert_equal 4, @backend.writes.count
    end
  end
end

class SimpleCacheFileTest < I18n::TestCase
  include CacheFileTest

  class Backend < I18n::Backend::Simple
    include CountWrites
    include I18n::Backend::CacheFile
  end

  def setup_backend!
    @backend = I18n.backend = Backend.new
  end
end

class KeyValueCacheFileTest < I18n::TestCase
  include CacheFileTest

  class Backend < I18n::Backend::KeyValue
    include CountWrites
    include I18n::Backend::CacheFile
    def initialize
      super({})
    end
  end

  def setup_backend!
    @backend = I18n.backend = Backend.new
  end
end if I18n::TestCase.key_value?
