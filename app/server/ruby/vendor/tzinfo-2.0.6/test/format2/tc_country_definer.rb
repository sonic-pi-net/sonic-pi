# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module Format2
  class TCCountryDefiner < Minitest::Test
    include TZInfo
    include TZInfo.const_get(:Format2)

    def setup
      @identifier_deduper = StringDeduper.new
      @description_deduper = StringDeduper.new
    end

    def test_no_timezones
      cd = create_country_definer({})
      assert_equal([], cd.timezones)
    end

    def test_defined_timezones
      cd = create_country_definer({})
      cd.timezone('Test/One',   1, 2,  3, 4)
      cd.timezone('Test/Two',  -1, 2, -3, 4, nil)
      cd.timezone('Test/Three', 5, 6, -7, 8, 'Test Three')
      assert_equal([
        CountryTimezone.new('Test/One',   Rational( 1, 2), Rational( 3, 4), nil),
        CountryTimezone.new('Test/Two',   Rational(-1, 2), Rational(-3, 4), nil),
        CountryTimezone.new('Test/Three', Rational( 5, 6), Rational(-7, 8), 'Test Three')], cd.timezones)
    end

    def test_shared_timezones
      shared_timezones = {
        t1: CountryTimezone.new('Test/One',   Rational( 1, 2), Rational( 3, 4), nil),
        t2: CountryTimezone.new('Test/Two',   Rational(-1, 2), Rational(-3, 4), nil),
        t3: CountryTimezone.new('Test/Three', Rational( 5, 6), Rational(-7, 8), 'Test Three')
      }
      cd = create_country_definer(shared_timezones)
      cd.timezone(:t1)
      cd.timezone(:t2)
      cd.timezone(:t3)
      assert_equal(3, cd.timezones.length)
      assert_same(shared_timezones[:t1], cd.timezones[0])
      assert_same(shared_timezones[:t2], cd.timezones[1])
      assert_same(shared_timezones[:t3], cd.timezones[2])
    end

    def test_shared_timezone_not_found
      cd = create_country_definer({})
      error = assert_raises(ArgumentError) { cd.timezone(:t1) }
      assert_equal("Unknown shared timezone: t1", error.message)
    end

    def test_missing_arguments
      cd = create_country_definer(
        t1: CountryTimezone.new('Test/One',   Rational( 1, 2), Rational( 3, 4), nil)
      )

      1.upto(3) do |count|
        args = [:t1] + 1.upto(count).to_a
        error = assert_raises(ArgumentError) { cd.timezone(*args) }
        assert_equal('Either just a reference should be supplied, or the identifier, latitude and longitude must all be specified', error.message)
      end
    end

    def test_strings_frozen
      cd = create_country_definer({})
      cd.timezone('Test/One', 1, 2, 3, 4, 'Test 1')
      timezone = cd.timezones.first
      assert(timezone.identifier.frozen?)
      assert(timezone.description.frozen?)
    end

    def test_strings_deduped
      tz = 2.times.collect do
        cd = create_country_definer({})
        cd.timezone('Test/One', 1, 2, 3, 4, 'Test 1')
        cd.timezones.first
      end

      assert_same(tz[0].identifier, tz[1].identifier)
      assert_same(tz[0].description, tz[1].description)

      assert_same(@identifier_deduper.dedupe('Test/One'), tz[0].identifier)
      assert_same(@description_deduper.dedupe('Test 1'), tz[0].description)
    end

    private

    def create_country_definer(shared_timezones)
      CountryDefiner.new(shared_timezones, @identifier_deduper, @description_deduper)
    end
  end
end
