# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module Format2
  class TCTimezoneIndexDefinition < Minitest::Test
    include TZInfo
    include TZInfo.const_get(:Format2)

    def test_mixed
      m = Module.new
      m.send(:include, TimezoneIndexDefinition)

      m.send(:timezone_index) do |i|
        assert_kind_of(TimezoneIndexDefiner, i)
        i.data_timezone 'Test/One'
        i.data_timezone 'Test/Two'
        i.linked_timezone 'Test/Three'
        i.data_timezone 'Another/Zone'
        i.linked_timezone 'And/Yet/Another'
      end

      data_timezones = m.data_timezones
      assert_equal(['Another/Zone', 'Test/One', 'Test/Two'], data_timezones)
      assert(data_timezones.frozen?)
      assert(data_timezones.all?(&:frozen?))
      assert_same(data_timezones, m.data_timezones)

      linked_timezones = m.linked_timezones
      assert_equal(['And/Yet/Another', 'Test/Three'], linked_timezones)
      assert(linked_timezones.frozen?)
      assert(linked_timezones.all?(&:frozen?))
      assert_same(linked_timezones, m.linked_timezones)
    end

    def test_data_only
      m = Module.new
      m.send(:include, TimezoneIndexDefinition)

      m.send(:timezone_index) do |i|
        assert_kind_of(TimezoneIndexDefiner, i)
        i.data_timezone 'Test/A/One'
        i.data_timezone 'Test/A/Two'
        i.data_timezone 'Test/A/Three'
      end

      data_timezones = m.data_timezones
      assert_equal(['Test/A/One', 'Test/A/Three', 'Test/A/Two'], data_timezones)
      assert(data_timezones.frozen?)
      assert(data_timezones.all?(&:frozen?))
      assert_same(data_timezones, m.data_timezones)

      linked_timezones = m.linked_timezones
      assert_equal([], linked_timezones)
      assert(linked_timezones.frozen?)
      assert_same(linked_timezones, m.linked_timezones)
    end

    def test_linked_only
      m = Module.new
      m.send(:include, TimezoneIndexDefinition)

      m.send(:timezone_index) do |i|
        assert_kind_of(TimezoneIndexDefiner, i)
        i.linked_timezone 'Test/B/One'
        i.linked_timezone 'Test/B/Two'
        i.linked_timezone 'Test/B/Three'
      end

      data_timezones = m.data_timezones
      assert_equal([], data_timezones)
      assert(data_timezones.frozen?)
      assert_same(data_timezones, m.data_timezones)

      linked_timezones = m.linked_timezones
      assert_equal(['Test/B/One', 'Test/B/Three', 'Test/B/Two'], linked_timezones)
      assert(linked_timezones.frozen?)
      assert(linked_timezones.all?(&:frozen?))
      assert_same(linked_timezones, m.linked_timezones)
    end

    def test_none
      m = Module.new
      m.send(:include, TimezoneIndexDefinition)

      m.send(:timezone_index) do |i|
        assert_kind_of(TimezoneIndexDefiner, i)
      end

      data_timezones = m.data_timezones
      assert_equal([], data_timezones)
      assert(data_timezones.frozen?)
      assert_same(data_timezones, m.data_timezones)

      linked_timezones = m.linked_timezones
      assert_equal([], linked_timezones)
      assert(linked_timezones.frozen?)
      assert_same(linked_timezones, m.linked_timezones)
    end

    def test_not_defined
      m = Module.new
      m.send(:include, TimezoneIndexDefinition)

      data_timezones = m.data_timezones
      assert_equal([], data_timezones)
      assert(data_timezones.frozen?)
      assert_same(data_timezones, m.data_timezones)

      linked_timezones = m.linked_timezones
      assert_equal([], linked_timezones)
      assert(linked_timezones.frozen?)
      assert_same(linked_timezones, m.linked_timezones)
    end

    def test_global_string_deduper_used_for_definer
      m = Module.new
      m.send(:include, TimezoneIndexDefinition)

      m.send(:timezone_index) do |i|
        assert_kind_of(TimezoneIndexDefiner, i)
        assert_same(StringDeduper.global, i.instance_variable_get(:@string_deduper))
      end
    end
  end
end
