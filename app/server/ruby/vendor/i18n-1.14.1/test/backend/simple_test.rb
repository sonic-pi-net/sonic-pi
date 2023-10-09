require 'test_helper'

class I18nBackendSimpleTest < I18n::TestCase
  def setup
    super
    I18n.backend = I18n::Backend::Simple.new
    I18n.load_path = [locales_dir + '/en.yml']
  end

  # useful because this way we can use the backend with no key for interpolation/pluralization
  test "simple backend translate: given nil as a key it still interpolations the default value" do
    assert_equal "Hi David", I18n.t(nil, :default => "Hi %{name}", :name => "David")
  end

  test "simple backend translate: given true as a key" do
    store_translations :en, available: { true => "Yes", false => "No" }
    assert_equal "Yes", I18n.t(:available)[true]
    assert_equal "No", I18n.t(:available)[false]
  end

  test "simple backend translate: given integer as a key" do
    store_translations :en, available: { -1 => "Possibly", 0 => "Maybe", 1 => "Yes", 2 => "No", 3 => "Never"  }
    assert_equal "Possibly", I18n.t(:available)[-1]
    assert_equal "Possibly", I18n.t('available.-1')
    assert_equal "Maybe", I18n.t(:available)[0]
    assert_equal "Maybe", I18n.t('available.0')
    assert_equal "Yes", I18n.t(:available)[1]
    assert_equal "Yes", I18n.t('available.1')
    assert_equal "No", I18n.t(:available)[2]
    assert_equal "No", I18n.t('available.2')
    assert_equal "Never", I18n.t(:available)[3]
    assert_equal "Never", I18n.t('available.+3')
  end

  test 'simple backend translate: given integer with a leading positive/negative sign' do
    store_translations :en, available: { -1 => "No", 0 => "Maybe", 1 => "Yes" }
    assert_equal 'No', I18n.t(:available)[-1]
    assert_equal 'No', I18n.t('available.-1')
    assert_equal 'Maybe', I18n.t(:available)[+0]
    assert_equal 'Maybe', I18n.t(:available)[-0]
    assert_equal 'Maybe', I18n.t('available.-0')
    assert_equal 'Maybe', I18n.t('available.+0')
    assert_equal 'Yes', I18n.t(:available)[+1]
    assert_equal 'Yes', I18n.t('available.+1')
  end

  test 'simple backend translate: given integer with a lead zero as a key' do
    store_translations :en, available: { '01' => 'foo' }
    assert_equal 'foo', I18n.t(:available)[:'01']
    assert_equal 'foo', I18n.t('available.01')
  end

  test "simple backend translate: symbolize keys in hash" do
    store_translations :en, nested_hashes_in_array: { hello: "world" }
    assert_equal "world", I18n.t('nested_hashes_in_array.hello')
    assert_equal "world", I18n.t('nested_hashes_in_array')[:hello]
  end

  test "simple backend translate: symbolize keys in array" do
    store_translations :en, nested_hashes_in_array: [ { hello: "world" } ]
    I18n.t('nested_hashes_in_array').each do |val|
      assert_equal "world", val[:hello]
    end
  end

  # loading translations
  test "simple load_translations: given an unknown file type it raises I18n::UnknownFileType" do
    assert_raises(I18n::UnknownFileType) { I18n.backend.load_translations("#{locales_dir}/en.xml") }
  end

  test "simple load_translations: given a YAML file name with yaml extension does not raise anything" do
    assert_nothing_raised { I18n.backend.load_translations("#{locales_dir}/en.yaml") }
  end

  test "simple load_translations: given a JSON file name with yaml extension does not raise anything" do
    assert_nothing_raised { I18n.backend.load_translations("#{locales_dir}/en.json") }
  end

  test "simple load_translations: given a Ruby file name it does not raise anything" do
    assert_nothing_raised { I18n.backend.load_translations("#{locales_dir}/en.rb") }
  end

  test "simple load_translations: given no argument, it uses I18n.load_path" do
    I18n.backend.load_translations
    assert_equal({ :en => { :foo => { :bar => 'baz' } } }, I18n.backend.send(:translations))
  end

  test "simple load_rb: loads data from a Ruby file" do
    data, _ = I18n.backend.send(:load_rb, "#{locales_dir}/en.rb")
    assert_equal({ :en => { :fuh => { :bah => 'bas' } } }, data)
  end

  test "simple load_yml: loads data from a YAML file" do
    data, _ = I18n.backend.send(:load_yml, "#{locales_dir}/en.yml")
    if ::YAML.respond_to?(:unsafe_load_file)
      assert_equal({ :en => { :foo => { :bar => 'baz' } } }, data)
      assert_predicate data.dig(:en, :foo, :bar), :frozen?
    else
      assert_equal({ 'en' => { 'foo' => { 'bar' => 'baz' } } }, data)
    end
  end

  test "simple load_json: loads data from a JSON file" do
    data, _ = I18n.backend.send(:load_json, "#{locales_dir}/en.json")

    if JSON.respond_to?(:load_file)
      assert_equal({ :en => { :foo => { :bar => 'baz' } } }, data)
      assert_predicate data.dig(:en, :foo, :bar), :frozen?
    else
      assert_equal({ 'en' => { 'foo' => { 'bar' => 'baz' } } }, data)
    end
  end

  test "simple load_translations: loads data from known file formats" do
    I18n.backend = I18n::Backend::Simple.new
    I18n.backend.load_translations("#{locales_dir}/en.rb", "#{locales_dir}/en.yml")
    expected = { :en => { :fuh => { :bah => "bas" }, :foo => { :bar => "baz" } } }
    assert_equal expected, translations
  end

  test "simple load_translations: given file names as array it does not raise anything" do
    assert_nothing_raised { I18n.backend.load_translations(["#{locales_dir}/en.rb", "#{locales_dir}/en.yml"]) }
  end

  # storing translations

  test "simple store_translations: stores translations, ... no, really :-)" do
    store_translations :'en', :foo => 'bar'
    assert_equal Hash[:'en', {:foo => 'bar'}], translations
  end

  test "simple store_translations: deep_merges with existing translations" do
    store_translations :'en', :foo => {:bar => 'bar'}
    store_translations :'en', :foo => {:baz => 'baz'}
    assert_equal Hash[:'en', {:foo => {:bar => 'bar', :baz => 'baz'}}], translations
  end

  test "simple store_translations: converts the given locale to a Symbol" do
    store_translations 'en', :foo => 'bar'
    assert_equal Hash[:'en', {:foo => 'bar'}], translations
  end

  test "simple store_translations: converts keys to Symbols" do
    store_translations 'en', 'foo' => {'bar' => 'bar', 'baz' => 'baz'}
    assert_equal Hash[:'en', {:foo => {:bar => 'bar', :baz => 'baz'}}], translations
  end

  test "simple store_translations: do not store translations unavailable locales if enforce_available_locales is true" do
    begin
      I18n.enforce_available_locales = true
      I18n.available_locales = [:en, :es]
      store_translations(:fr, :foo => {:bar => 'barfr', :baz => 'bazfr'})
      store_translations(:es, :foo => {:bar => 'bares', :baz => 'bazes'})
      assert_equal translations[:fr], {}
      assert_equal Hash[:foo, {:bar => 'bares', :baz => 'bazes'}], translations[:es]
    ensure
      I18n.config.enforce_available_locales = false
    end
  end

  test "simple store_translations: store translations for unavailable locales if enforce_available_locales is false" do
    I18n.available_locales = [:en, :es]
    store_translations(:fr, :foo => {:bar => 'barfr', :baz => 'bazfr'})
    assert_equal Hash[:foo, {:bar => 'barfr', :baz => 'bazfr'}], translations[:fr]
  end

  test "simple store_translations: supports numeric keys" do
    store_translations(:en, 1 => 'foo')
    assert_equal 'foo', I18n.t('1')
    assert_equal 'foo', I18n.t(1)
    assert_equal 'foo', I18n.t(:'1')
  end

  test "simple store_translations: store translations doesn't deep symbolize keys if skip_symbolize_keys is true" do
    data = { :foo => {'bar' => 'barfr', 'baz' => 'bazfr'} }

    # symbolized by default
    store_translations(:fr, data)
    assert_equal Hash[:foo, {:bar => 'barfr', :baz => 'bazfr'}], translations[:fr]

    I18n.backend.reload!

    # not deep symbolized when configured
    store_translations(:fr, data, skip_symbolize_keys: true)
    assert_equal Hash[:foo, {'bar' => 'barfr', 'baz' => 'bazfr'}], translations[:fr]
  end

  # reloading translations

  test "simple reload_translations: unloads translations" do
    I18n.backend.reload!
    assert_nil translations
  end

  test "simple reload_translations: uninitializes the backend" do
    I18n.backend.reload!
    assert_equal false, I18n.backend.initialized?
  end

  test "simple eager_load!: loads the translations" do
    assert_equal false, I18n.backend.initialized?
    I18n.backend.eager_load!
    assert_equal true, I18n.backend.initialized?
  end

  test "simple reload!: reinitialize the backend if it was previously eager loaded" do
    I18n.backend.eager_load!
    I18n.backend.reload!
    assert_equal true, I18n.backend.initialized?
  end

  test "Nested keys within pluralization context" do
    store_translations(:en,
      :stars => {
        one: "%{count} star",
        other: "%{count} stars",
        special: {
          one: "%{count} special star",
          other: "%{count} special stars",
        }
      }
    )
    assert_equal "1 star", I18n.t('stars', count: 1, :locale => :en)
    assert_equal "20 stars", I18n.t('stars', count: 20, :locale => :en)
    assert_equal "1 special star", I18n.t('stars.special', count: 1, :locale => :en)
    assert_equal "20 special stars", I18n.t('stars.special', count: 20, :locale => :en)
  end

  test "returns localized string given missing pluralization data" do
    assert_equal 'baz', I18n.t('foo.bar', count: 1)
  end
end
