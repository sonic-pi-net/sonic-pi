# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module Format2
  class TCCountryIndexDefinition < Minitest::Test
    include TZInfo
    include TZInfo.const_get(:Format2)

    def test_not_defined
      m = Module.new
      m.send(:include, CountryIndexDefinition)

      countries = m.countries
      assert_equal({}, countries)
      assert(countries.frozen?)
    end

    def test_none
      m = Module.new
      m.send(:include, CountryIndexDefinition)

      m.send(:country_index) do |i|
        assert_kind_of(CountryIndexDefiner, i)
      end

      countries = m.countries
      assert_equal({}, countries)
      assert(countries.frozen?)
    end

    def test_multiple
      m = Module.new
      m.send(:include, CountryIndexDefinition)

      m.send(:country_index) do |i|
        assert_kind_of(CountryIndexDefiner, i)
        i.timezone(:t1, 'Test/Zone/Shared1', -1, -2, -3, -4)
        i.timezone(:t2, 'Test/Zone/Shared2', 1, 2, 3, 4, 'Shared 2')

        i.country('ZZ', 'Country One') do |c|
          c.timezone :t1
          c.timezone 'Test/Zone/1', 3, 2, 41, 20
        end

        i.country('AA', 'Aland') do |c|
          c.timezone 'Test/Zone/3', 71, 30, 358, 15, 'Zone 3'
          c.timezone 'Test/Zone/2', 41, 20, 211, 30
          c.timezone :t1
          c.timezone :t2
        end

        i.country('TE', 'Three')

        i.country('FR', 'Four') do |c|
        end
      end

      countries = m.countries
      assert_equal(%w(ZZ AA TE FR), countries.keys)
      assert(countries.frozen?)

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

    def test_global_and_local_string_dedupers_used_for_country_index_definer
      block_called = 0

      m = Module.new
      m.send(:include, CountryIndexDefinition)

      m.send(:country_index) do |i|
        assert_kind_of(CountryIndexDefiner, i)
        assert_same(StringDeduper.global, i.instance_variable_get(:@identifier_deduper))
        assert_same(StringDeduper, i.instance_variable_get(:@description_deduper).class)
        block_called += 1
      end

      assert_equal(1, block_called)
    end
  end
end
