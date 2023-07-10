# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'
require 'tmpdir'

class TCDataSource < Minitest::Test
  include TZInfo

  class InitDataSource < DataSource
  end

  class DummyDataSource < DataSource
  end

  class TestDataSource < DataSource
    attr_reader :called

    def initialize
      super
      @called = 0
    end
  end

  class GetTimezoneInfoTestDataSource < TestDataSource
    include TZInfo

    protected

    def load_timezone_info(identifier)
      @called += 1
      raise InvalidTimezoneIdentifier, identifier if identifier == 'Test/Invalid'
      DataSources::TimezoneInfo.new(identifier)
    end
  end

  class GetCountryInfoTestDataSource < TestDataSource
    include TZInfo

    protected

    def load_country_info(code)
      @called += 1
      raise InvalidCountryCode, code if code == 'XX'
      DataSources::CountryInfo.new(code, "Country #{code}", [])
    end
  end

  class GetTimezoneIdentifiersTestDataSource < DataSource
    attr_reader :data_timezone_identifiers_called
    attr_reader :linked_timezone_identifiers_called

    def initialize(data_timezone_identifiers, linked_timezone_identifiers)
      @data_timezone_identifiers = data_timezone_identifiers.each(&:freeze).freeze
      @linked_timezone_identifiers = linked_timezone_identifiers.each(&:freeze).freeze
      @data_timezone_identifiers_called = 0
      @linked_timezone_identifiers_called = 0
    end

    def data_timezone_identifiers
      @data_timezone_identifiers_called += 1
      @data_timezone_identifiers
    end

    def linked_timezone_identifiers
      @linked_timezone_identifiers_called += 1
      @linked_timezone_identifiers
    end
  end

  class ValidateTimezoneIdentifierTestDataSource < DataSource
    attr_reader :data_timezone_identifiers
    attr_reader :linked_timezone_identifiers
    attr_reader :timezone_identifier_encoding

    def initialize(data_timezone_identifiers, linked_timezone_identifiers, timezone_identifier_encoding = Encoding::UTF_8)
      super()
      @data_timezone_identifiers = data_timezone_identifiers.sort.freeze
      @linked_timezone_identifiers = linked_timezone_identifiers.sort.freeze
      @timezone_identifier_encoding = timezone_identifier_encoding
    end

    def call_validate_timezone_identifier(identifier)
      validate_timezone_identifier(identifier)
    end
  end

  class LookupCountryInfoTestDataSource < DataSource
    def call_lookup_country_info(hash, code, encoding = Encoding::UTF_8)
      lookup_country_info(hash, code, encoding)
    end
  end

  class EagerLoadTestDataSource < GetTimezoneIdentifiersTestDataSource
    include TZInfo

    attr_reader :country_codes_called
    attr_reader :loaded_timezones
    attr_reader :loaded_countries

    def initialize(data_timezone_identifiers, linked_timezone_identifiers, country_codes)
      super(data_timezone_identifiers, linked_timezone_identifiers)
      @country_codes = country_codes
      @country_codes_called = 0
      @loaded_timezones = []
      @loaded_countries = []
    end

    protected

    def country_codes
      @country_codes_called += 1
      @country_codes
    end

    def load_timezone_info(identifier)
      @loaded_timezones << identifier
      DataSources::TimezoneInfo.new(identifier)
    end

    def load_country_info(code)
      @loaded_countries << code
      DataSources::CountryInfo.new(code, "Country #{code}", [])
    end
  end

  def setup
    @orig_data_source = DataSource.get
    DataSource.set(InitDataSource.new)
    @orig_search_path = DataSources::ZoneinfoDataSource.search_path.clone
  end

  def teardown
    DataSource.set(@orig_data_source)
    DataSources::ZoneinfoDataSource.search_path = @orig_search_path
  end

  def test_get
    data_source = DataSource.get
    assert_kind_of(InitDataSource, data_source)
  end

  def test_get_default_ruby_only
    code = <<-EOF
      require 'tmpdir'

      begin
        Dir.mktmpdir('tzinfo_test_dir') do |dir|
          TZInfo::DataSources::ZoneinfoDataSource.search_path = [dir]

          puts TZInfo::DataSource.get.class
        end
      rescue Exception => e
        puts "Unexpected exception: \#{e}"
      end
    EOF

    assert_sub_process_returns(['TZInfo::DataSources::RubyDataSource'], code, [TZINFO_TEST_DATA_DIR])
  end

  def test_get_default_zoneinfo_only
    code = <<-EOF
      require 'tmpdir'

      begin
        Dir.mktmpdir('tzinfo_test_dir') do |dir|
          TZInfo::DataSources::ZoneinfoDataSource.search_path = [dir, '#{TZINFO_TEST_ZONEINFO_DIR}']

          puts TZInfo::DataSource.get.class
          puts TZInfo::DataSource.get.zoneinfo_dir
        end
      rescue Exception => e
        puts "Unexpected exception: \#{e}"
      end
    EOF

    assert_sub_process_returns(
      ['TZInfo::DataSources::ZoneinfoDataSource', TZINFO_TEST_ZONEINFO_DIR],
      code)
  end

  def test_get_default_ruby_and_zoneinfo
    code = <<-EOF
      begin
        TZInfo::DataSources::ZoneinfoDataSource.search_path = ['#{TZINFO_TEST_ZONEINFO_DIR}']

        puts TZInfo::DataSource.get.class
      rescue Exception => e
        puts "Unexpected exception: \#{e}"
      end
    EOF

    assert_sub_process_returns(['TZInfo::DataSources::RubyDataSource'], code, [TZINFO_TEST_DATA_DIR])
  end

  def test_get_default_no_data
    code = <<-EOF
      require 'tmpdir'

      begin
        Dir.mktmpdir('tzinfo_test_dir') do |dir|
          TZInfo::DataSources::ZoneinfoDataSource.search_path = [dir]

          begin
            data_source = TZInfo::DataSource.get
            puts "No exception raised, returned \#{data_source} instead"
          rescue Exception => e
            puts e.class
          end
        end
      rescue Exception => e
        puts "Unexpected exception: \#{e}"
      end
    EOF

    assert_sub_process_returns(['TZInfo::DataSourceNotFound'], code)
  end

  def test_set_instance
    DataSource.set(DummyDataSource.new)
    data_source = DataSource.get
    assert_kind_of(DummyDataSource, data_source)
  end

  def test_set_standard_ruby
    DataSource.set(:ruby)
    data_source = DataSource.get
    assert_kind_of(DataSources::RubyDataSource, data_source)
  end

  def test_set_standard_ruby_not_found
    code = <<-EOF
      begin
        TZInfo::DataSources::RubyDataSource.set(:ruby)
        puts 'No exception raised'
      rescue Exception => e
        puts e.class
        puts e.message
      end
    EOF

    assert_sub_process_returns([
      'TZInfo::DataSources::TZInfoDataNotFound',
      'The tzinfo-data gem could not be found (require \'tzinfo/data\' failed).'], code)
  end

  def test_set_standard_zoneinfo_search
    Dir.mktmpdir('tzinfo_test_dir') do |dir|
      FileUtils.touch(File.join(dir, 'iso3166.tab'))
      FileUtils.touch(File.join(dir, 'zone.tab'))

      DataSources::ZoneinfoDataSource.search_path = [dir]

      DataSource.set(:zoneinfo)
      data_source = DataSource.get
      assert_kind_of(DataSources::ZoneinfoDataSource, data_source)
      assert_equal(dir, data_source.zoneinfo_dir)
    end
  end

  def test_set_standard_zoneinfo_search_zone1970
    Dir.mktmpdir('tzinfo_test_dir') do |dir|
      FileUtils.touch(File.join(dir, 'iso3166.tab'))
      FileUtils.touch(File.join(dir, 'zone1970.tab'))

      DataSources::ZoneinfoDataSource.search_path = [dir]

      DataSource.set(:zoneinfo)
      data_source = DataSource.get
      assert_kind_of(DataSources::ZoneinfoDataSource, data_source)
      assert_equal(dir, data_source.zoneinfo_dir)
    end
  end

  def test_set_standard_zoneinfo_explicit
    Dir.mktmpdir('tzinfo_test_dir') do |dir|
      FileUtils.touch(File.join(dir, 'iso3166.tab'))
      FileUtils.touch(File.join(dir, 'zone.tab'))

      DataSource.set(:zoneinfo, dir)
      data_source = DataSource.get
      assert_kind_of(DataSources::ZoneinfoDataSource, data_source)
      assert_equal(dir, data_source.zoneinfo_dir)
    end
  end

  def test_set_standard_zoneinfo_explicit_zone1970
    Dir.mktmpdir('tzinfo_test_dir') do |dir|
      FileUtils.touch(File.join(dir, 'iso3166.tab'))
      FileUtils.touch(File.join(dir, 'zone.tab'))

      DataSource.set(:zoneinfo, dir)
      data_source = DataSource.get
      assert_kind_of(DataSources::ZoneinfoDataSource, data_source)
      assert_equal(dir, data_source.zoneinfo_dir)
    end
  end

  def test_set_standard_zoneinfo_explicit_alternate_iso3166
    Dir.mktmpdir('tzinfo_test_dir') do |dir|
      zoneinfo_dir = File.join(dir, 'zoneinfo')
      tab_dir = File.join(dir, 'tab')

      FileUtils.mkdir(zoneinfo_dir)
      FileUtils.mkdir(tab_dir)

      FileUtils.touch(File.join(zoneinfo_dir, 'zone.tab'))

      iso3166_file = File.join(tab_dir, 'iso3166.tab')
      FileUtils.touch(iso3166_file)

      DataSource.set(:zoneinfo, zoneinfo_dir, iso3166_file)
      data_source = DataSource.get
      assert_kind_of(DataSources::ZoneinfoDataSource, data_source)
      assert_equal(zoneinfo_dir, data_source.zoneinfo_dir)
    end
  end

  def test_set_standard_zoneinfo_search_not_found
    Dir.mktmpdir('tzinfo_test_dir') do |dir|
      DataSources::ZoneinfoDataSource.search_path = [dir]

      error = assert_raises(DataSources::ZoneinfoDirectoryNotFound) do
        DataSource.set(:zoneinfo)
      end
      assert_equal('None of the paths included in TZInfo::DataSources::ZoneinfoDataSource.search_path are valid zoneinfo directories.', error.message)

      assert_kind_of(InitDataSource, DataSource.get)
    end
  end

  def test_set_standard_zoneinfo_explicit_invalid
    Dir.mktmpdir('tzinfo_test_dir') do |dir|
      error = assert_raises(DataSources::InvalidZoneinfoDirectory) do
        DataSource.set(:zoneinfo, dir)
      end
      assert_equal("#{dir} is not a directory or doesn't contain a iso3166.tab file and a zone1970.tab or zone.tab file.", error.message)

      assert_kind_of(InitDataSource, DataSource.get)
    end
  end

  def test_set_standard_zoneinfo_wrong_arg_count
    assert_raises(ArgumentError) do
      DataSource.set(:zoneinfo, 1, 2, 3)
    end

    assert_kind_of(InitDataSource, DataSource.get)
  end

  def test_set_invalid_datasource_type
    error = assert_raises(ArgumentError) { DataSource.set(:other) }
    assert_equal('data_source_or_type must be a DataSource instance or a data source type (:ruby or :zoneinfo)', error.message)
  end

  def test_set_not_a_datasource
    error = assert_raises(ArgumentError) { DataSource.set(Object.new) }
    assert_equal('data_source_or_type must be a DataSource instance or a data source type (:ruby or :zoneinfo)', error.message)
  end

  def test_get_timezone_info
    ds = GetTimezoneInfoTestDataSource.new
    info = ds.get_timezone_info('Test/Simple')
    assert_equal('Test/Simple', info.identifier)
    assert_equal(1, ds.called)
  end

  def test_get_timezone_info_caches_result
    ds = GetTimezoneInfoTestDataSource.new
    info = ds.get_timezone_info('Test/Cache')
    assert_equal('Test/Cache', info.identifier)
    assert_same(info, ds.get_timezone_info('Test/Cache'))
    assert_equal(1, ds.called)
  end

  def test_get_timezone_info_invalid_identifier
    ds = GetTimezoneInfoTestDataSource.new
    error = assert_raises(InvalidTimezoneIdentifier) { ds.get_timezone_info('Test/Invalid') }
    assert_equal('Test/Invalid', error.message)
    assert(1, ds.called)
  end

  def test_timezone_identifiers
    data_identifiers = ['Test/Aaa', 'Test/Ccc']
    linked_identifiers = ['Test/Bbb']
    ds = GetTimezoneIdentifiersTestDataSource.new(data_identifiers, linked_identifiers)
    result = ds.timezone_identifiers
    assert_kind_of(Array, result)
    assert_equal(['Test/Aaa', 'Test/Bbb', 'Test/Ccc'], result)
    assert_same(data_identifiers[0], result[0])
    assert_same(linked_identifiers[0], result[1])
    assert_same(data_identifiers[1], result[2])
    assert(result.frozen?)
    assert(result.all?(&:frozen?))
    assert_equal(1, ds.data_timezone_identifiers_called)
    assert_equal(1, ds.linked_timezone_identifiers_called)
  end

  def test_timezone_identifiers_caches_result
    ds = GetTimezoneIdentifiersTestDataSource.new(['Test/Aaa', 'Test/Ccc'], ['Test/Bbb'])
    result = ds.timezone_identifiers
    assert_same(result, ds.timezone_identifiers)
    assert_equal(1, ds.data_timezone_identifiers_called)
    assert_equal(1, ds.linked_timezone_identifiers_called)
  end

  def test_timezone_identifiers_returns_data_array_if_linked_is_empty
    data_identifiers = ['Test/Aaa', 'Test/Ccc']
    ds = GetTimezoneIdentifiersTestDataSource.new(data_identifiers, [])
    result = ds.timezone_identifiers
    assert_same(data_identifiers, result)
    assert_equal(1, ds.data_timezone_identifiers_called)
    assert_equal(1, ds.linked_timezone_identifiers_called)
  end

  def test_timezone_identifiers_caches_result_if_linked_is_empty
    data_identifiers = ['Test/Aaa', 'Test/Ccc']
    ds = GetTimezoneIdentifiersTestDataSource.new(data_identifiers, [])
    result = ds.timezone_identifiers
    assert_same(data_identifiers, result)
    assert_same(data_identifiers, ds.timezone_identifiers)
    assert_equal(1, ds.data_timezone_identifiers_called)
    assert_equal(1, ds.linked_timezone_identifiers_called)
  end

  def abstract_test(method, public, *args)
    ds = DataSource.new
    error = assert_raises(InvalidDataSource) { ds.public_send(*([public ? :public_send : :send, method] + args)) }
    assert_equal("#{method} not defined", error.message)
  end

  def test_data_timezone_identifiers
    abstract_test(:data_timezone_identifiers, true)
  end

  def test_linked_timezone_identifiers
    abstract_test(:linked_timezone_identifiers, true)
  end

  def test_get_country_info
    ds = GetCountryInfoTestDataSource.new
    info = ds.get_country_info('CC')
    assert_equal('CC', info.code)
    assert_equal(1, ds.called)
  end

  def test_get_country_info_invalid_identifier
    ds = GetCountryInfoTestDataSource.new
    error = assert_raises(InvalidCountryCode) { ds.get_country_info('XX') }
    assert_equal('XX', error.message)
    assert_equal(1, ds.called)
  end

  def test_country_codes
    abstract_test(:country_codes, true)
  end

  def test_to_s
    assert_equal('Default DataSource', DataSource.new.to_s)
  end

  def test_inspect
    assert_equal('#<TCDataSource::TestDataSource>', TestDataSource.new.inspect)
  end

  def test_load_timezone_info
    abstract_test(:load_timezone_info, false, 'Test/Identifier')
  end

  def test_load_country_info
    abstract_test(:load_country_info, false, 'CC')
  end

  def test_validate_timezone_identifier
    data_identifiers = ['Test/One', 'Test/Two', 'Test/Three']
    linked_identifiers = ['Test/Four', 'Test/Five']
    ds = ValidateTimezoneIdentifierTestDataSource.new(data_identifiers, linked_identifiers)

    [data_identifiers, linked_identifiers].each do |identifiers|
      identifiers.each do |identifier|
        assert_same(identifier, ds.call_validate_timezone_identifier(identifier.dup))
      end
    end

    ['Test/Invalid', 'Invalid/Test', 'TST'].each do |identifier|
      error = assert_raises(InvalidTimezoneIdentifier) { ds.call_validate_timezone_identifier(identifier) }
      assert_match(Regexp.new("\\b#{Regexp.escape(identifier)}\\b"), error.message)
    end

    error = assert_raises(InvalidTimezoneIdentifier) { ds.call_validate_timezone_identifier(nil) }
    assert_match(/\bnil\b/, error.message)

    error = assert_raises(InvalidTimezoneIdentifier) { ds.call_validate_timezone_identifier(false) }
    assert_match(/\bfalse\b/, error.message)
  end

  test_encodings('UTF-8', 'UTF-16', 'ISO-8859-1') do |identifier_encoding|
    test_encodings('UTF-8', 'UTF-16', 'ISO-8859-1') do |index_encoding|
      define_method("test_validate_timezone_identifier_with_#{index_encoding.to_method}_encoded_index_and_#{identifier_encoding.to_method}_encoded_identifier") do
        data_identifiers = ['Test/One'.encode(index_encoding.name).freeze]
        ds = ValidateTimezoneIdentifierTestDataSource.new(data_identifiers, [], index_encoding.find)
        assert_same(data_identifiers[0], ds.call_validate_timezone_identifier('Test/One'.encode(identifier_encoding.name).freeze))
      end
    end


    define_method("test_validate_timezone_identifier_with_invalid_#{identifier_encoding.to_method}_encoded_identifier") do
      data_identifiers = ['Test/One'.freeze]
      linked_identifiers = ['Test/Two'.freeze]
      ds = ValidateTimezoneIdentifierTestDataSource.new(data_identifiers, linked_identifiers)

      encoded_identifier = 'Test/Invalid'.encode(identifier_encoding.name).freeze
      error = assert_raises(InvalidTimezoneIdentifier) { ds.call_validate_timezone_identifier(encoded_identifier) }
      assert_match(/\bTest\/Invalid\b/, error.message)
      assert_equal(Encoding::UTF_8, error.message.encoding)
    end
  end

  def test_lookup_country_info
    ds = LookupCountryInfoTestDataSource.new
    hash = {'GB' => Object.new}
    info = ds.call_lookup_country_info(hash, 'GB')
    assert_same(hash['GB'], info)
  end

  def test_lookup_country_info_not_exist
    ds = LookupCountryInfoTestDataSource.new
    hash = {'GB' => Object.new}

    error = assert_raises(InvalidCountryCode) do
      ds.call_lookup_country_info(hash, 'ZZ')
    end

    assert_match(/\bZZ\b/, error.message)
  end

  def test_lookup_country_info_nil
    ds = LookupCountryInfoTestDataSource.new
    hash = {'GB' => Object.new}

    error = assert_raises(InvalidCountryCode) do
      ds.call_lookup_country_info(hash, nil)
    end

    assert_match(/\bnil\b/, error.message)
  end

  def test_lookup_country_info_false
    ds = LookupCountryInfoTestDataSource.new
    hash = {'GB' => Object.new}

    error = assert_raises(InvalidCountryCode) do
      ds.call_lookup_country_info(hash, false)
    end

    assert_match(/\bfalse\b/, error.message)
  end

  def test_lookup_country_info_case
    ds = LookupCountryInfoTestDataSource.new
    hash = {'GB' => Object.new}

    error = assert_raises(InvalidCountryCode) do
      ds.call_lookup_country_info(hash, 'gb')
    end

    assert_match(/\bgb\b/, error.message)
  end

  test_encodings('UTF-8', 'UTF-16', 'ISO-8859-1') do |code_encoding|
    test_encodings('UTF-8', 'UTF-16', 'ISO-8859-1') do |index_encoding|
      define_method("test_lookup_country_info_with_#{index_encoding.to_method}_encoded_index_and_#{code_encoding.to_method}_encoded_identifier") do
        ds = LookupCountryInfoTestDataSource.new
        info = Object.new
        hash = {'GB'.encode(index_encoding).freeze => info}
        assert_same(info, ds.call_lookup_country_info(hash, 'GB'.encode(code_encoding.name).freeze, index_encoding.find))
      end
    end

    define_method("test_lookup_country_info_with_invalid_#{code_encoding.to_method}_encoded_identifier") do
      ds = LookupCountryInfoTestDataSource.new
      hash = {}

      error = assert_raises(InvalidCountryCode) { ds.call_lookup_country_info(hash, 'ZZ'.encode(code_encoding.name).freeze) }
      assert_match(/\bZZ\b/, error.message)
      assert_equal(Encoding::UTF_8, error.message.encoding)
    end
  end

  def test_eager_load
    data_timezone_identifiers = ['Data/Zone1', 'Data/Zone2']
    linked_timezone_identifiers = ['Linked/Zone1', 'Linked/Zone2']
    all_timezone_identifiers = data_timezone_identifiers + linked_timezone_identifiers
    country_codes = ['AA', 'BB', 'CC']
    ds = EagerLoadTestDataSource.new(data_timezone_identifiers, linked_timezone_identifiers, country_codes)
    assert_nil(ds.eager_load!)
    assert_equal(1, ds.data_timezone_identifiers_called)
    assert_equal(1, ds.linked_timezone_identifiers_called)
    assert_equal(all_timezone_identifiers, ds.loaded_timezones)
    assert_equal(all_timezone_identifiers, ds.instance_variable_get(:@timezone_identifiers))
    assert_equal(1, ds.country_codes_called)
    assert_equal(country_codes, ds.loaded_countries)
  end
end
