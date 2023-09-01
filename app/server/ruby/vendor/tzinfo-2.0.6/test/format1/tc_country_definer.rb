# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module Format1
  class TCCountryDefiner < Minitest::Test
    include TZInfo
    include TZInfo.const_get(:Format1)

    def setup
      @identifier_deduper = StringDeduper.new
      @description_deduper = StringDeduper.new
    end

    def test_no_timezones
      cd = create_country_definer
      assert_equal([], cd.timezones)
    end

    def test_timezones
      cd = create_country_definer
      cd.timezone('Test/One',   1, 2,  3, 4)
      cd.timezone('Test/Two',  -1, 2, -3, 4, nil)
      cd.timezone('Test/Three', 5, 6, -7, 8, 'Test Three')
      assert_equal([
        CountryTimezone.new('Test/One',   Rational( 1, 2), Rational( 3, 4), nil),
        CountryTimezone.new('Test/Two',   Rational(-1, 2), Rational(-3, 4), nil),
        CountryTimezone.new('Test/Three', Rational( 5, 6), Rational(-7, 8), 'Test Three')], cd.timezones)
    end

    def test_strings_frozen
      cd = create_country_definer
      cd.timezone('Test/One'.dup, 1, 2, 3, 4, 'Test 1'.dup)
      timezone = cd.timezones.first
      assert(timezone.identifier.frozen?)
      assert(timezone.description.frozen?)
    end

    def test_strings_deduped
      tz = 2.times.collect do
        cd = create_country_definer
        cd.timezone('Test/One'.dup, 1, 2, 3, 4, 'Test 1'.dup)
        cd.timezones.first
      end

      assert_same(tz[0].identifier, tz[1].identifier)
      assert_same(tz[0].description, tz[1].description)

      assert_same(@identifier_deduper.dedupe('Test/One'.dup), tz[0].identifier)
      assert_same(@description_deduper.dedupe('Test 1'.dup), tz[0].description)
    end

    private

    def create_country_definer
      CountryDefiner.new(@identifier_deduper, @description_deduper)
    end
  end
end
