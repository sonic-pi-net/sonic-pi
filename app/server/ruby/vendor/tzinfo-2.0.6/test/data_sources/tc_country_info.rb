# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module DataSources
  class TCCountryInfo < Minitest::Test
    include TZInfo
    include TZInfo::DataSources

    def test_initialize_nil_code
      error = assert_raises(ArgumentError) { CountryInfo.new(nil, 'Zzz', []) }
      assert_match(/\bcode\b/, error.message)
    end

    def test_initialize_nil_name
      error = assert_raises(ArgumentError) { CountryInfo.new('ZZ', nil, []) }
      assert_match(/\bname\b/, error.message)
    end

    def test_initialize_nil_zones
      error = assert_raises(ArgumentError) { CountryInfo.new('ZZ', 'Zzz', nil) }
      assert_match(/\bzones\b/, error.message)
    end

    def test_code
      ci = CountryInfo.new('ZZ', 'Zzz', [])
      assert_equal('ZZ', ci.code)
    end

    def test_code_frozen
      code = 'ZZ'.dup
      refute(code.frozen?)
      ci = CountryInfo.new(code, 'Zzz', [])
      assert_same(code, ci.code)
      assert(ci.code.frozen?)
    end

    def test_name
      ci = CountryInfo.new('ZZ', 'Zzz', [])
      assert_equal('Zzz', ci.name)
    end

    def test_name_frozen
      name = 'Zzz'.dup
      refute(name.frozen?)
      ci = CountryInfo.new('ZZ', name, [])
      assert_same(name, ci.name)
      assert(ci.name.frozen?)
    end

    def test_zones_empty
      zones = []
      ci = CountryInfo.new('ZZ', 'Zzz', zones)
      value = ci.zones
      assert_equal([], value)
      assert_same(zones, value)
      assert(value.frozen?)
    end

    def test_zones
      zones = [
        CountryTimezone.new('ZZ/TimezoneB', Rational(1, 2), Rational(1, 2), 'Timezone B'),
        CountryTimezone.new('ZZ/TimezoneA', Rational(1, 4), Rational(1, 4), 'Timezone A'),
        CountryTimezone.new('ZZ/TimezoneC', Rational(-10, 3), Rational(-20, 7), 'C'),
        CountryTimezone.new('ZZ/TimezoneD', Rational(-10, 3), Rational(-20, 7))
      ]

      ci = CountryInfo.new('ZZ', 'Zzz', zones)
      value = ci.zones

      assert_equal([CountryTimezone.new('ZZ/TimezoneB', Rational(1, 2), Rational(1, 2), 'Timezone B'),
        CountryTimezone.new('ZZ/TimezoneA', Rational(1, 4), Rational(1, 4), 'Timezone A'),
        CountryTimezone.new('ZZ/TimezoneC', Rational(-10, 3), Rational(-20, 7), 'C'),
        CountryTimezone.new('ZZ/TimezoneD', Rational(-10, 3), Rational(-20, 7))],
        value)
      assert_same(zones, value)
      assert(value.frozen?)
    end

    def test_inspect
      ci = CountryInfo.new('ZZ', 'Zzz', [])
      assert_equal('#<TZInfo::DataSources::CountryInfo: ZZ>', ci.inspect)
    end
  end
end
