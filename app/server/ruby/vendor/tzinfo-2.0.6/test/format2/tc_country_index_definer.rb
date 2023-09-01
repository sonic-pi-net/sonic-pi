# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module Format2
  class TCCountryIndexDefiner < Minitest::Test
    include TZInfo
    include TZInfo.const_get(:Format2)

    def setup
      @identifier_deduper = StringDeduper.new
      @description_deduper = StringDeduper.new
      @definer = CountryIndexDefiner.new(@identifier_deduper, @description_deduper)
    end


    def test_none
      countries = @definer.countries
      assert_equal({}, countries)
    end

    def test_multiple
      @definer.timezone(:t1, 'Test/Zone/Shared1', -1, -2, -3, -4)
      @definer.timezone(:t2, 'Test/Zone/Shared2', 1, 2, 3, 4, 'Shared 2')

      @definer.country('ZZ', 'Country One') do |c|
        assert_kind_of(CountryDefiner, c)
        c.timezone :t1
        c.timezone 'Test/Zone/1', 3, 2, 41, 20
      end

      @definer.country('AA', 'Aland') do |c|
        assert_kind_of(CountryDefiner, c)
        c.timezone 'Test/Zone/3', 71, 30, 358, 15, 'Zone 3'
        c.timezone 'Test/Zone/2', 41, 20, 211, 30
        c.timezone :t1
        c.timezone :t2
      end

      @definer.country('TE', 'Three')

      @definer.country('FR', 'Four') do |c|
      end

      countries = @definer.countries
      assert_equal(%w(ZZ AA TE FR), countries.keys)

      country_zz = countries['ZZ']
      assert_kind_of(DataSources::CountryInfo, country_zz)
      assert_equal('ZZ', country_zz.code)
      assert_equal('Country One', country_zz.name)
      assert_equal([
        CountryTimezone.new('Test/Zone/Shared1', Rational(-1, -2), Rational(-3, -4)),
        CountryTimezone.new('Test/Zone/1', Rational(3, 2), Rational(41, 20))], country_zz.zones)

      country_aa = countries['AA']
      assert_kind_of(DataSources::CountryInfo, country_aa)
      assert_equal('AA', country_aa.code)
      assert_equal('Aland', country_aa.name)
      assert_equal([
        CountryTimezone.new('Test/Zone/3', Rational(71, 30), Rational(358, 15), 'Zone 3'),
        CountryTimezone.new('Test/Zone/2', Rational(41, 20), Rational(211, 30)),
        CountryTimezone.new('Test/Zone/Shared1', Rational(-1, -2), Rational(-3, -4)),
        CountryTimezone.new('Test/Zone/Shared2', Rational(1, 2), Rational(3, 4), 'Shared 2')], country_aa.zones)

      assert_same(country_zz.zones[0], country_aa.zones[2])

      country_te = countries['TE']
      assert_kind_of(DataSources::CountryInfo, country_te)
      assert_equal('TE', country_te.code)
      assert_equal('Three', country_te.name)
      assert_equal([], country_te.zones)

      country_fr = countries['FR']
      assert_kind_of(DataSources::CountryInfo, country_fr)
      assert_equal('FR', country_fr.code)
      assert_equal('Four', country_fr.name)
      assert_equal([], country_fr.zones)
    end

    def test_redefined_country
      @definer.country('TT', 'Test1') do |c|
        assert_kind_of(CountryDefiner, c)
        c.timezone 'Test/Zone/1', 1, 2, 3, 4, 'Zone 1'
      end

      @definer.country('TT', 'Test2') do |c|
        assert_kind_of(CountryDefiner, c)
        c.timezone 'Test/Zone/2', 5, 6, 7, 8, 'Zone 2'
      end

      countries = @definer.countries
      assert_equal(%w(TT), countries.keys)

      country_tt = countries['TT']
      assert_kind_of(DataSources::CountryInfo, country_tt)
      assert_equal('TT', country_tt.code)
      assert_equal('Test2', country_tt.name)
      assert_equal([CountryTimezone.new('Test/Zone/2', Rational(5, 6), Rational(7, 8), 'Zone 2')], country_tt.zones)
    end

    def test_redefined_shared_timezone
      @definer.timezone(:t1, 'Test/Zone/Shared1', -1, -2, -3, -4)
      @definer.timezone(:t1, 'Test/Zone/Shared2', 1, 2, 3, 4, 'Shared 2')

      @definer.country('TT', 'Test1') do |c|
        assert_kind_of(CountryDefiner, c)
        c.timezone(:t1)
      end

      countries = @definer.countries
      assert_equal(%w(TT), countries.keys)

      country_tt = countries['TT']
      assert_kind_of(DataSources::CountryInfo, country_tt)
      assert_equal('TT', country_tt.code)
      assert_equal('Test1', country_tt.name)
      assert_equal([CountryTimezone.new('Test/Zone/Shared2', Rational(1, 2), Rational(3, 4), 'Shared 2')], country_tt.zones)
    end

    def test_strings_frozen
      @definer.timezone(:t1, 'Test/Zone/Shared1', 1, 2, 3, 4, 'Shared 1')

      @definer.country('TT', 'Test') do |c|
        assert_kind_of(CountryDefiner, c)
        c.timezone 'Test/Zone/1', 1, 2, 3, 4, 'Zone One'
        c.timezone :t1
      end

      countries = @definer.countries
      assert(countries.keys.all?(&:frozen?))
      country_tt = countries['TT']
      assert(country_tt.code.frozen?)
      assert(country_tt.name.frozen?)
      zone0 = country_tt.zones[0]
      assert(zone0.identifier.frozen?)
      assert(zone0.description.frozen?)
      zone1 = country_tt.zones[1]
      assert(zone1.identifier.frozen?)
      assert(zone1.description.frozen?)
    end

    def test_strings_deduped
      @definer.timezone(:t1, 'Test/Zone/Shared1', 1, 2, 3, 4, 'Shared 1')
      @definer.timezone(:t2, 'Test/Zone/Shared2', 1, 2, 3, 4, 'Shared 2')

      # There will never be a country defined with either a duplicate code or
      # a duplicate name.

      @definer.country('TT', 'Test') do |c|
        c.timezone :t1
        c.timezone 'Test/Zone/Shared2', 5, 6, 7, 8, 'Shared 2'
      end

      @definer.country('TU', 'Test 2') do |c|
        c.timezone 'Test/Zone/Shared1', 9, 10, 11, 12, 'Shared 1'
        c.timezone :t2
      end

      countries = @definer.countries
      country_tt = countries['TT']
      country_tu = countries['TU']

      assert_same(country_tt.zones[0].identifier, country_tu.zones[0].identifier)
      assert_same(country_tt.zones[0].description, country_tu.zones[0].description)

      assert_same(country_tt.zones[1].identifier, country_tu.zones[1].identifier)
      assert_same(country_tt.zones[1].description, country_tu.zones[1].description)

      assert_same(@identifier_deduper.dedupe('Test/Zone/Shared1'), country_tt.zones[0].identifier)
      assert_same(@identifier_deduper.dedupe('Test/Zone/Shared2'), country_tt.zones[1].identifier)

      assert_same(@description_deduper.dedupe('Shared 1'), country_tt.zones[0].description)
      assert_same(@description_deduper.dedupe('Shared 2'), country_tt.zones[1].description)
    end

    def test_identifier_and_description_string_dedupers_used_for_country_definer
      block_called = 0

      @definer.country('TT', 'Test') do |c|
        block_called += 1
        assert_kind_of(CountryDefiner, c)
        assert_same(@identifier_deduper, c.instance_variable_get(:@identifier_deduper))
        assert_same(@description_deduper, c.instance_variable_get(:@description_deduper))
        c.timezone 'Test/Zone/1', 1, 2, 3, 4, 'One'
      end

      assert_equal(1, block_called)
    end
  end
end
