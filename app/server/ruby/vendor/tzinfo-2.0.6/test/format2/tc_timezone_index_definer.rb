# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module Format2
  class TCTimezoneIndexDefiner < Minitest::Test
    include TZInfo
    include TZInfo.const_get(:Format2)

    def setup
      @string_deduper = StringDeduper.new
      @definer = TimezoneIndexDefiner.new(@string_deduper)
    end

    def test_mixed
      @definer.data_timezone 'Test/One'
      @definer.data_timezone 'Test/Two'
      @definer.linked_timezone 'Test/Three'
      @definer.data_timezone 'Another/Zone'
      @definer.linked_timezone 'And/Yet/Another'

      assert_array_same_items(['Another/Zone', 'Test/One', 'Test/Two'], @definer.data_timezones)
      assert_array_same_items(['And/Yet/Another', 'Test/Three'], @definer.linked_timezones)
      assert(@definer.data_timezones.all?(&:frozen?))
      assert(@definer.linked_timezones.all?(&:frozen?))
    end

    def test_data_only
      @definer.data_timezone 'Test/A/One'
      @definer.data_timezone 'Test/A/Two'
      @definer.data_timezone 'Test/A/Three'

      assert_array_same_items(['Test/A/One', 'Test/A/Two', 'Test/A/Three'], @definer.data_timezones)
      assert_equal([], @definer.linked_timezones)
      assert(@definer.data_timezones.all?(&:frozen?))
    end

    def test_linked_only
      @definer.linked_timezone 'Test/B/One'
      @definer.linked_timezone 'Test/B/Two'
      @definer.linked_timezone 'Test/B/Three'

      assert_equal([], @definer.data_timezones)
      assert_array_same_items(['Test/B/One', 'Test/B/Three', 'Test/B/Two'], @definer.linked_timezones)
      assert(@definer.linked_timezones.all?(&:frozen?))
    end

    def test_none
      assert_equal([], @definer.data_timezones)
      assert_equal([], @definer.linked_timezones)
    end

    def test_strings_deduped
      identifier = @string_deduper.dedupe('Test/A/One')
      linked_identifier = @string_deduper.dedupe('Test/B/One')

      @definer.data_timezone 'Test/A/One'
      @definer.linked_timezone 'Test/B/One'

      assert_same(identifier, @definer.data_timezones.first)
      assert_same(linked_identifier, @definer.linked_timezones.first)
    end
  end
end
