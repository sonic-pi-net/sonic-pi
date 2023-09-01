require 'test_helper'

class I18nBackendFallbacksTranslateTest < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.backend = Backend.new
    store_translations(:en, :foo => 'Foo in :en', :bar => 'Bar in :en', :buz => 'Buz in :en', :interpolate => 'Interpolate %{value}', :interpolate_count => 'Interpolate %{value} %{count}')
    store_translations(:de, :bar => 'Bar in :de', :baz => 'Baz in :de')
    store_translations(:'de-DE', :baz => 'Baz in :de-DE')
    store_translations(:'pt-BR', :baz => 'Baz in :pt-BR')
  end

  test "still returns an existing translation as usual" do
    assert_equal 'Foo in :en', I18n.t(:foo, :locale => :en)
    assert_equal 'Bar in :de', I18n.t(:bar, :locale => :de)
    assert_equal 'Baz in :de-DE', I18n.t(:baz, :locale => :'de-DE')
  end

  test "returns interpolated value if no key provided" do
    assert_equal 'Interpolate %{value}', I18n.t(:interpolate)
  end

  test "returns the :de translation for a missing :'de-DE' translation" do
    assert_equal 'Bar in :de', I18n.t(:bar, :locale => :'de-DE')
  end

  test "keeps the count option when defaulting to a different key" do
    assert_equal 'Interpolate 5 10', I18n.t(:non_existent, default: :interpolate_count, count: 10, value: 5)
  end

  test "returns the :de translation for a missing :'de-DE' when :default is a String" do
    assert_equal 'Bar in :de', I18n.t(:bar, :locale => :'de-DE', :default => "Default Bar")
    assert_equal "Default Bar", I18n.t(:missing_bar, :locale => :'de-DE', :default => "Default Bar")
  end

  test "returns the :de translation for a missing :'de-DE' when defaults is a Symbol (which exists in :en)" do
    assert_equal "Bar in :de", I18n.t(:bar, :locale => :'de-DE', :default => [:buz])
  end

  test "returns the :'de-DE' default :baz translation for a missing :'de-DE' (which exists in :de)" do
    assert_equal "Baz in :de-DE", I18n.t(:bar, :locale => :'de-DE', :default => [:baz])
  end

  test "returns the :de translation for a missing :'de-DE' when :default is a Proc" do
    assert_equal 'Bar in :de', I18n.t(:bar, :locale => :'de-DE', :default => Proc.new { "Default Bar" })
    assert_equal "Default Bar", I18n.t(:missing_bar, :locale => :'de-DE', :default => Proc.new { "Default Bar" })
  end

  test "returns the :de translation for a missing :'de-DE' when :default is a Hash" do
    assert_equal 'Bar in :de', I18n.t(:bar, :locale => :'de-DE', :default => {})
    assert_equal({}, I18n.t(:missing_bar, :locale => :'de-DE', :default => {}))
  end

  test "returns the :de translation for a missing :'de-DE' when :default is nil" do
    assert_equal 'Bar in :de', I18n.t(:bar, :locale => :'de-DE', :default => nil)
    assert_nil I18n.t(:missing_bar, :locale => :'de-DE', :default => nil)
  end

  test "returns the Translation missing: message if the default is also missing" do
    translation_missing_message = <<~MSG
      Translation missing. Options considered were:
      - de-DE.missing_bar
      - de-DE.missing_baz
    MSG

    assert_equal translation_missing_message.chomp, I18n.t(:missing_bar, :locale => :'de-DE', :default => [:missing_baz])
  end

  test "returns the simple Translation missing: message when default is an empty Array" do
    assert_equal "Translation missing: de-DE.missing_bar", I18n.t(:missing_bar, :locale => :'de-DE', :default => [])
  end

  test "returns the :'de-DE' default :baz translation for a missing :'de-DE' when defaults contains Symbol" do
    assert_equal 'Baz in :de-DE', I18n.t(:missing_foo, :locale => :'de-DE', :default => [:baz, "Default Bar"])
  end

  test "returns the defaults translation for a missing :'de-DE' when defaults contains a String or Proc before Symbol" do
    assert_equal "Default Bar", I18n.t(:missing_foo, :locale => :'de-DE', :default => [:missing_bar, "Default Bar", :baz])
    assert_equal "Default Bar", I18n.t(:missing_foo, :locale => :'de-DE', :default => [:missing_bar, Proc.new { "Default Bar" }, :baz])
  end

  test "returns the default translation for a missing :'de-DE' and existing :de when default is a Hash" do
    assert_equal 'Default 6 Bars', I18n.t(:missing_foo, :locale => :'de-DE', :default => [:missing_bar, {:other => "Default %{count} Bars"}, "Default Bar"], :count => 6)
  end

  test "returns the default translation for a missing :de translation even when default is a String when fallback is disabled" do
    assert_equal 'Default String', I18n.t(:foo, :locale => :de, :default => 'Default String', :fallback => false)
  end

  test "raises I18n::MissingTranslationData exception when fallback is disabled even when fallback translation exists" do
    assert_raises(I18n::MissingTranslationData) { I18n.t(:foo, :locale => :de, :fallback => false, :raise => true) }
  end

  test "raises I18n::MissingTranslationData exception when no translation was found" do
    assert_raises(I18n::MissingTranslationData) { I18n.t(:faa, :locale => :en, :raise => true) }
    assert_raises(I18n::MissingTranslationData) { I18n.t(:faa, :locale => :de, :raise => true) }
  end

  test "should ensure that default is not splitted on new line char" do
    assert_equal "Default \n Bar", I18n.t(:missing_bar, :default => "Default \n Bar")
  end

  test "should not raise error when enforce_available_locales is true, :'pt' is missing and default is a Symbol" do
    I18n.enforce_available_locales = true
    begin
      assert_equal 'Foo', I18n.t(:'model.attrs.foo', :locale => :'pt-BR', :default => [:'attrs.foo', "Foo"])
    ensure
      I18n.enforce_available_locales = false
    end
  end

  test "returns fallback default given missing pluralization data" do
    assert_equal 'default', I18n.t(:missing_bar, count: 1, default: 'default')
    assert_equal 'default', I18n.t(:missing_bar, count: 0, default: 'default')
  end

  test "multi-threaded fallbacks" do
    I18n.fallbacks = [:en]

    thread = Thread.new do
      I18n.fallbacks = [:de]
    end

    begin
      thread.join
      assert_equal 'Bar in :en', I18n.t(:bar, :locale => :'pt-BR')
    ensure
      thread.exit
      I18n.fallbacks = I18n::Locale::Fallbacks.new
    end
  end
end

# See Issue #534
class I18nBackendFallbacksLocalizeTestWithDefaultLocale < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.backend = Backend.new
    I18n.enforce_available_locales = false
    I18n.fallbacks = [I18n.default_locale]
    store_translations(:en, time: { formats: { fallback: 'en fallback' } })
  end

  test "falls back to default locale - Issue #534" do
    assert_equal 'en fallback', I18n.l(Time.now, format: :fallback, locale: "un-supported")
  end
end

# See Issue #536
class I18nBackendFallbacksWithCustomClass < I18n::TestCase
  class BackendWithFallbacks < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  # Quacks like a fallback class
  class MyDefaultFallback
    def [](key)
     [:my_language]
    end
  end

  def setup
    super
    I18n.backend = BackendWithFallbacks.new
    I18n.enforce_available_locales = false
    I18n.fallbacks = MyDefaultFallback.new
    store_translations(:my_language, foo: 'customer foo')
    store_translations(:en, foo: 'english foo')
  end

  test "can use a default fallback object that doesn't inherit from I18n::Locale::Fallbacks" do
    assert_equal 'customer foo', I18n.t(:foo, locale: :en)
    assert_equal 'customer foo', I18n.t(:foo, locale: :nothing)
  end
end

# See Issue #546
class I18nBackendFallbacksLocalizeTestWithMultipleThreads < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.backend = Backend.new
    I18n.enforce_available_locales = false
    I18n.fallbacks = [I18n.default_locale]
    store_translations(:en, time: { formats: { fallback: 'en fallback' } })
  end

  test "falls back to default locale - Issue #546" do
    Thread.new { assert_equal 'en fallback', I18n.l(Time.now, format: :fallback, locale: "un-supported") }.join
  end
end

# See Issue #590
class I18nBackendFallbacksSymbolResolveRestartsLookupAtOriginalLocale < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.backend = Backend.new
    I18n.enforce_available_locales = false
    I18n.fallbacks = [:root]
    store_translations(:ak,
                       'calendars' => {
                         'gregorian' => {
                           'months' => {
                             'format' => {
                               'abbreviated' => {
                                 1 => 'S-Ɔ'
                                 # Other months omitted for brevity
                               }
                             }
                           }
                         }
                       })
    store_translations(:root,
                       'calendars' => {
                         'gregorian' => {
                           'months' => {
                             'format' => {
                               'abbreviated' => :"calendars.gregorian.months.format.wide",
                               'wide' => {
                                 1 => 'M01'
                                 # Other months omitted for brevity
                               }
                             },
                             'stand-alone' => {
                               'abbreviated' => :"calendars.gregorian.months.format.abbreviated"
                             }
                           }
                         }
                       })
  end

  test 'falls back to original locale when symbol resolved at fallback locale' do
    assert_equal({ 1 => 'S-Ɔ' }, I18n.t('calendars.gregorian.months.stand-alone.abbreviated', locale: :"ak-GH"))
  end
end

# See Issue #617
class RegressionTestFor617 < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.backend = Backend.new
    I18n.enforce_available_locales = false
    I18n.fallbacks = {:en=>[:en], :"en-US"=>[:"en-US", :en]}
    I18n.locale = :'en-US'
    store_translations(:"en-US", {})
    store_translations(:en, :activerecord=>{:models=>{:product=>{:one=>"Product", :other=>"Products"}, :"product/ticket"=>{:one=>"Ticket", :other=>"Tickets"}}})
  end

  test 'model scope resolution' do
    defaults = [:product, "Ticket"]
    options = {:scope=>[:activerecord, :models], :count=>1, :default=> defaults}
    assert_equal("Ticket", I18n.t(:"product/ticket", **options))
  end
end

class I18nBackendFallbacksLocalizeTest < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.backend = Backend.new
    store_translations(:en, :date => { :formats => { :en => 'en' }, :day_names => %w(Sunday) })
    store_translations(:de, :date => { :formats => { :de => 'de' }, :day_names => %w(Sunday) })
  end

  test "still uses an existing format as usual" do
    assert_equal 'en', I18n.l(Date.today, :format => :en, :locale => :en)
  end
  test "looks up and uses a fallback locale's format for a key missing in the given locale" do
    assert_equal 'de', I18n.l(Date.today, :format => :de, :locale => :'de-DE')
  end

  test "still uses an existing day name translation as usual" do
    assert_equal 'Sunday', I18n.l(Date.new(2010, 1, 3), :format => '%A', :locale => :en)
  end

  test "uses a fallback locale's translation for a key missing in the given locale" do
    assert_equal 'Sunday', I18n.l(Date.new(2010, 1, 3), :format => '%A', :locale => :'de-DE')
  end
end

class I18nBackendFallbacksWithChainTest < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  class Chain < I18n::Backend::Chain
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    backend = Backend.new
    backend.store_translations(:de, :foo => 'FOO')
    backend.store_translations(:'pt-BR', :foo => 'Baz in :pt-BR')
    I18n.backend = Chain.new(I18n::Backend::Simple.new, backend)
  end

  test "falls back from de-DE to de when there is no translation for de-DE available" do
    assert_equal 'FOO', I18n.t(:foo, :locale => :'de-DE')
  end

  test "exists? falls back from de-DE to de given a key missing from the given locale" do
    assert_equal true, I18n.exists?(:foo, :locale => :'de-DE')
  end

  test "exists? should return false when fallback disabled given a key missing from the given locale" do
    assert_equal false, I18n.exists?(:foo, :locale => :'de-DE', fallback: false)
  end

  test "falls back from de-DE to de when there is no translation for de-DE available when using arrays, too" do
    assert_equal ['FOO', 'FOO'], I18n.t([:foo, :foo], :locale => :'de-DE')
  end

  test "should not raise error when enforce_available_locales is true, :'pt' is missing and default is a Symbol" do
    I18n.enforce_available_locales = true
    begin
      assert_equal 'Foo', I18n.t(:'model.attrs.foo', :locale => :'pt-BR', :default => [:'attrs.foo', "Foo"])
    ensure
      I18n.enforce_available_locales = false
    end
  end
end

class I18nBackendFallbacksExistsTest < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.backend = Backend.new
    store_translations(:en, :foo => 'Foo in :en', :bar => 'Bar in :en')
    store_translations(:de, :bar => 'Bar in :de')
    store_translations(:'de-DE', :baz => 'Baz in :de-DE')
  end

  test "exists? given an existing key will return true" do
    assert_equal true, I18n.exists?(:foo)
  end

  test "exists? given a non-existing key will return false" do
    assert_equal false, I18n.exists?(:bogus)
  end

  test "exists? given an existing key and an existing locale will return true" do
    assert_equal true, I18n.exists?(:foo, :en)
    assert_equal true, I18n.exists?(:bar, :de)
  end

  test "exists? given a non-existing key and an existing locale will return false" do
    assert_equal false, I18n.exists?(:bogus, :en)
    assert_equal false, I18n.exists?(:bogus, :de)
  end

  test "exists? should return true given a key which is missing from the given locale and exists in a fallback locale" do
    assert_equal true, I18n.exists?(:bar, :de)
    assert_equal true, I18n.exists?(:bar, :'de-DE')
  end

  test "exists? should return false given a key which is missing from the given locale and all its fallback locales" do
    assert_equal false, I18n.exists?(:baz, :de)
    assert_equal false, I18n.exists?(:bogus, :'de-DE')
  end

  test "exists? should return false when fallback is disabled given a key which is missing from the given locale" do
    assert_equal true, I18n.exists?(:bar, :'de-DE')
    assert_equal false, I18n.exists?(:bar, :'de-DE', fallback: false)
    assert_equal false, I18n.exists?(:bar, :'de-DE-XX', fallback: false)
  end
end

class I18nBackendOnFallbackHookTest < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Fallbacks

    attr :fallback_collector

    private

      def on_fallback(*args)
        @fallback_collector ||= []
        @fallback_collector << args
      end
  end

  def setup
    super
    I18n.backend = Backend.new
    I18n.fallbacks = I18n::Locale::Fallbacks.new(de: :en)
    store_translations(:en, :foo => 'Foo in :en', :bar => 'Bar in :en')
    store_translations(:de, :bar => 'Bar in :de')
    store_translations(:"de-DE", :baz => 'Baz in :"de-DE"')
  end

  test "on_fallback should be called when fallback happens" do
    assert_equal [:"de-DE", :de, :en], I18n.fallbacks[:"de-DE"]
    assert_equal 'Baz in :"de-DE"', I18n.t(:baz, locale: :'de-DE')
    assert_equal 'Bar in :de', I18n.t(:bar, locale: :'de-DE')
    assert_equal 'Foo in :en', I18n.t(:foo, locale: :'de-DE')
    assert_equal [:'de-DE', :de, :bar, {}], I18n.backend.fallback_collector[0]
    assert_equal [:'de-DE', :en, :foo, {}], I18n.backend.fallback_collector[1]
  end

  test "on_fallback should not be called when use a String locale" do
    assert_equal 'Bar in :de', I18n.t("bar", locale: "de")
    assert I18n.backend.fallback_collector.nil?
  end
end
