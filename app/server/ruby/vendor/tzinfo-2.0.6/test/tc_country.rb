# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCCountry < Minitest::Test
  include TZInfo

  def setup
    @orig_data_source = DataSource.get
  end

  def teardown
    DataSource.set(@orig_data_source)
  end

  test_encodings('ISO-8859-1', 'UTF-8', 'UTF-16').each do |encoding|
    define_method("test_get_valid_with_#{encoding.to_method}_encoded_code") do
      c = Country.get('GB'.encode(encoding.name))

      refute_nil(c)
      assert_equal('GB', c.code)
    end

    define_method("test_get_not_exist_with_#{encoding.to_method}_encoded_code") do
      error = assert_raises(InvalidCountryCode) { Country.get('ZZ'.encode(encoding.name)) }
      assert_match(/\bZZ\b/, error.message)
    end

    define_method("test_get_invalid_with_#{encoding.to_method}_encoded_code") do
      error = assert_raises(InvalidCountryCode) { Country.get('../Countries/GB'.encode(encoding.name)) }
      assert_match(/\W\.\.\/Countries\/GB\b/, error.message)
    end

    define_method("test_get_case_with_#{encoding.to_method}_encoded_code") do
      error = assert_raises(InvalidCountryCode) { Country.get('gb'.encode(encoding.name)) }
      assert_match(/\bgb\b/, error.message)
    end
  end

  def test_get_nil
    error = assert_raises(InvalidCountryCode) { Country.get(nil) }
    assert_match(/\bnil\b/, error.message)
  end

  def test_get_tainted_loaded
    skip_if_taint_is_undefined_or_no_op
    Country.get('GB')

    safe_test(unavailable: :skip) do
      code = 'GB'.dup.taint
      assert(code.tainted?)
      country = Country.get(code)
      assert_equal('GB', country.code)
      assert(code.tainted?)
    end
  end

  def test_get_tainted_and_frozen_loaded
    skip_if_taint_is_undefined_or_no_op
    Country.get('GB')

    safe_test do
      country = Country.get('GB'.dup.taint.freeze)
      assert_equal('GB', country.code)
    end
  end

  def test_get_tainted_not_previously_loaded
    skip_if_taint_is_undefined_or_no_op

    safe_test(unavailable: :skip) do
      code = 'GB'.dup.taint
      assert(code.tainted?)
      country = Country.get(code)
      assert_equal('GB', country.code)
      assert(code.tainted?)
    end
  end

  def test_get_tainted_and_frozen_not_previously_loaded
    skip_if_taint_is_undefined_or_no_op

    safe_test do
      country = Country.get('GB'.dup.taint.freeze)
      assert_equal('GB', country.code)
    end
  end

  def test_all_codes
    assert_equal(DataSource.get.country_codes, Country.all_codes)
  end

  def test_all
    assert_equal(Country.all_codes, Country.all.collect(&:code))
  end

  def test_initialize
    info = DataSources::CountryInfo.new('TT', 'Test', [])
    c = Country.new(info)
    assert_equal('TT', c.code)
    assert_equal('Test', c.name)
    assert_equal([], c.zones)
  end

  def test_code
    assert_equal('US', Country.get('US').code)
  end

  def test_name
    assert_equal('United States', Country.get('US').name)
  end

  def test_to_s
    assert_equal(Country.get('US').name, Country.get('US').to_s)
    assert_equal(Country.get('GB').name, Country.get('GB').to_s)
  end

  def test_inspect
    assert_equal('#<TZInfo::Country: GB>', Country.get('GB').inspect)
  end

  def test_zone_identifiers
    zone_identifiers = Country.get('US').zone_identifiers
    assert_equal(DataSource.get.get_country_info('US').zones.map(&:identifier), zone_identifiers)
  end

  def test_zone_names
    assert_equal(Country.get('US').zone_identifiers, Country.get('US').zone_names)
  end

  def test_zones
    zones = Country.get('US').zones
    assert_kind_of(Array, zones)
    assert_equal(Country.get('US').zone_identifiers, zones.collect(&:identifier))

    zones.each {|z| assert_kind_of(TimezoneProxy, z)}
  end

  def test_zone_info
    zone_info = Country.get('US').zone_info
    assert_equal(DataSource.get.get_country_info('US').zones, zone_info)
    assert_equal(true, zone_info.frozen?)
  end

  def test_compare
    assert_equal(0, Country.get('GB') <=> Country.get('GB'))
    assert_equal(-1, Country.get('GB') <=> Country.get('US'))
    assert_equal(1, Country.get('US') <=> Country.get('GB'))
    assert_equal(-1, Country.get('FR') <=> Country.get('US'))
    assert_equal(1, Country.get('US') <=> Country.get('FR'))
  end

  def test_compare_non_comparable
    assert_nil(Country.get('GB') <=> Object.new)
  end

  def test_equality
    assert_equal(true, Country.get('GB') == Country.get('GB'))
    assert_equal(false, Country.get('GB') == Country.get('US'))
    assert(!(Country.get('GB') == Object.new))
  end

  def test_eql
    assert_equal(true, Country.get('GB').eql?(Country.get('GB')))
    assert_equal(false, Country.get('GB').eql?(Country.get('US')))
    assert(!Country.get('GB').eql?(Object.new))
  end

  def test_hash
    assert_equal('GB'.hash, Country.get('GB').hash)
    assert_equal('US'.hash, Country.get('US').hash)
  end

  define_method("test_=~_operator_matches_against_code") do
    c = Country.get('GB')
    assert_equal(0, c =~ /\AGB\z/)
    assert_equal(1, c =~ /B/)
  end

  define_method("test_=~_operator_returns_nil_when_regexp_does_not_match") do
    c = Country.get('GB')
    assert_nil(c =~ /US/)
  end

  def test_marshal
    c = Country.get('US')
    marshalled_c = Marshal.load(Marshal.dump(c))
    assert_kind_of(Country, marshalled_c)
    assert_equal('US', marshalled_c.code)
  end

  def test_get_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Country.get('GB')
    end
    assert_equal('load_country_info not defined', error.message)
  end

  def test_all_codes_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Country.all_codes
    end
    assert_equal('country_codes not defined', error.message)
  end

  def test_all_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Country.all
    end
    assert_equal('country_codes not defined', error.message)
  end
end
