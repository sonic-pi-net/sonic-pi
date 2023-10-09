# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTimezoneLondon < Minitest::Test
  include TZInfo

  def test_2004
    #Europe/London  Sun Mar 28 00:59:59 2004 UTC = Sun Mar 28 00:59:59 2004 GMT isdst=0 gmtoff=0
    #Europe/London  Sun Mar 28 01:00:00 2004 UTC = Sun Mar 28 02:00:00 2004 BST isdst=1 gmtoff=3600
    #Europe/London  Sun Oct 31 00:59:59 2004 UTC = Sun Oct 31 01:59:59 2004 BST isdst=1 gmtoff=3600
    #Europe/London  Sun Oct 31 01:00:00 2004 UTC = Sun Oct 31 01:00:00 2004 GMT isdst=0 gmtoff=0

    tz = Timezone.get('Europe/London')

    time_types_test(:offset) do |h|
      assert_equal_with_offset(h.output_time(2004, 3,28,0,59,59,0,   0), tz.to_local(h.time(2004, 3,28,0,59,59,0,0)))
      assert_equal_with_offset(h.output_time(2004, 3,28,2, 0, 0,0,3600), tz.to_local(h.time(2004, 3,28,1, 0, 0,0,0)))
      assert_equal_with_offset(h.output_time(2004,10,31,1,59,59,0,3600), tz.to_local(h.time(2004,10,31,0,59,59,0,0)))
      assert_equal_with_offset(h.output_time(2004,10,31,1, 0, 0,0,   0), tz.to_local(h.time(2004,10,31,1, 0, 0,0,0)))

      assert_equal_with_offset(h.output_time(2004, 3,28,0,59,59,0,   0), tz.utc_to_local(h.time(2004, 3,28,0,59,59)))
      assert_equal_with_offset(h.output_time(2004, 3,28,2, 0, 0,0,3600), tz.utc_to_local(h.time(2004, 3,28,1, 0, 0)))
      assert_equal_with_offset(h.output_time(2004,10,31,1,59,59,0,3600), tz.utc_to_local(h.time(2004,10,31,0,59,59)))
      assert_equal_with_offset(h.output_time(2004,10,31,1, 0, 0,0,   0), tz.utc_to_local(h.time(2004,10,31,1, 0, 0)))

      assert_equal_with_offset(h.output_time(2004, 3,28,0,59,59,0,:utc), tz.local_to_utc(h.time(2004, 3,28,0,59,59)))
      assert_equal_with_offset(h.output_time(2004, 3,28,1, 0, 0,0,:utc), tz.local_to_utc(h.time(2004, 3,28,2, 0, 0)))
      assert_equal_with_offset(h.output_time(2004,10,31,0,59,59,0,:utc), tz.local_to_utc(h.time(2004,10,31,1,59,59),  true))
      assert_equal_with_offset(h.output_time(2004,10,31,1,59,59,0,:utc), tz.local_to_utc(h.time(2004,10,31,1,59,59), false))
      assert_equal_with_offset(h.output_time(2004,10,31,0, 0, 0,0,:utc), tz.local_to_utc(h.time(2004,10,31,1, 0, 0),  true))
      assert_equal_with_offset(h.output_time(2004,10,31,1, 0, 0,0,:utc), tz.local_to_utc(h.time(2004,10,31,1, 0, 0), false))

      assert_raises(PeriodNotFound) { tz.local_to_utc(h.time(2004, 3,28,1,0,0)) }
      assert_raises(AmbiguousTime)  { tz.local_to_utc(h.time(2004,10,31,1,0,0)) }

      assert_equal('GMT', tz.period_for(h.time(2004, 3,28,0,59,59,0,0)).zone_identifier)
      assert_equal('BST', tz.period_for(h.time(2004, 3,28,1, 0, 0,0,0)).zone_identifier)
      assert_equal('BST', tz.period_for(h.time(2004,10,31,0,59,59,0,0)).zone_identifier)
      assert_equal('GMT', tz.period_for(h.time(2004,10,31,1, 0, 0,0,0)).zone_identifier)

      assert_equal('GMT', tz.period_for_utc(h.time(2004, 3,28,0,59,59)).zone_identifier)
      assert_equal('BST', tz.period_for_utc(h.time(2004, 3,28,1, 0, 0)).zone_identifier)
      assert_equal('BST', tz.period_for_utc(h.time(2004,10,31,0,59,59)).zone_identifier)
      assert_equal('GMT', tz.period_for_utc(h.time(2004,10,31,1, 0, 0)).zone_identifier)

      assert_equal('GMT', tz.period_for_local(h.time(2004, 3,28,0,59,59)).zone_identifier)
      assert_equal('BST', tz.period_for_local(h.time(2004, 3,28,2, 0, 0)).zone_identifier)
      assert_equal('BST', tz.period_for_local(h.time(2004,10,31,1,59,59),  true).zone_identifier)
      assert_equal('GMT', tz.period_for_local(h.time(2004,10,31,1,59,59), false).zone_identifier)
      assert_equal('BST', tz.period_for_local(h.time(2004,10,31,1, 0, 0),  true).zone_identifier)
      assert_equal('GMT', tz.period_for_local(h.time(2004,10,31,1, 0, 0), false).zone_identifier)

      assert_equal(   0, tz.period_for(h.time(2004, 3,28,0,59,59,0,0)).observed_utc_offset)
      assert_equal(3600, tz.period_for(h.time(2004, 3,28,1, 0, 0,0,0)).observed_utc_offset)
      assert_equal(3600, tz.period_for(h.time(2004,10,31,0,59,59,0,0)).observed_utc_offset)
      assert_equal(   0, tz.period_for(h.time(2004,10,31,1, 0, 0,0,0)).observed_utc_offset)

      assert_equal(   0, tz.period_for_utc(h.time(2004, 3,28,0,59,59)).observed_utc_offset)
      assert_equal(3600, tz.period_for_utc(h.time(2004, 3,28,1, 0, 0)).observed_utc_offset)
      assert_equal(3600, tz.period_for_utc(h.time(2004,10,31,0,59,59)).observed_utc_offset)
      assert_equal(   0, tz.period_for_utc(h.time(2004,10,31,1, 0, 0)).observed_utc_offset)

      assert_equal(   0, tz.period_for_local(h.time(2004, 3,28,0,59,59)).observed_utc_offset)
      assert_equal(3600, tz.period_for_local(h.time(2004, 3,28,2, 0, 0)).observed_utc_offset)
      assert_equal(3600, tz.period_for_local(h.time(2004,10,31,1,59,59),  true).observed_utc_offset)
      assert_equal(   0, tz.period_for_local(h.time(2004,10,31,1,59,59), false).observed_utc_offset)
      assert_equal(3600, tz.period_for_local(h.time(2004,10,31,1, 0, 0),  true).observed_utc_offset)
      assert_equal(   0, tz.period_for_local(h.time(2004,10,31,1, 0, 0), false).observed_utc_offset)

      transitions = tz.transitions_up_to(h.time(2005,1,1,0,0,0,0,0), h.time(2004,1,1,0,0,0,0,0))
      assert_equal(2, transitions.length)
      assert_equal_with_offset(Timestamp.for(Time.utc(2004,3,28,1,0,0)), transitions[0].at)
      assert_equal(TimezoneOffset.new(0,    0, 'GMT'), transitions[0].previous_offset)
      assert_equal(TimezoneOffset.new(0, 3600, 'BST'), transitions[0].offset)
      assert_equal_with_offset(Timestamp.for(Time.utc(2004,10,31,1,0,0)), transitions[1].at)
      assert_equal(TimezoneOffset.new(0, 3600, 'BST'), transitions[1].previous_offset)
      assert_equal(TimezoneOffset.new(0,    0, 'GMT'), transitions[1].offset)

      offsets = tz.offsets_up_to(h.time(2005,1,1,0,0,0,0,0), h.time(2004,1,1,0,0,0,0,0))
      assert_array_same_items([TimezoneOffset.new(0, 0, 'GMT'), TimezoneOffset.new(0, 3600, 'BST')], offsets)
    end
  end

  def test_1961
    #Europe/London  Sun Mar 26 01:59:59 1961 UTC = Sun Mar 26 01:59:59 1961 GMT isdst=0 gmtoff=0
    #Europe/London  Sun Mar 26 02:00:00 1961 UTC = Sun Mar 26 03:00:00 1961 BST isdst=1 gmtoff=3600
    #Europe/London  Sun Oct 29 01:59:59 1961 UTC = Sun Oct 29 02:59:59 1961 BST isdst=1 gmtoff=3600
    #Europe/London  Sun Oct 29 02:00:00 1961 UTC = Sun Oct 29 02:00:00 1961 GMT isdst=0 gmtoff=0

    tz = Timezone.get('Europe/London')

    time_types_test do |h|
      assert_equal_with_offset(h.output_time(1961, 3,26,1,59,59,0,   0), tz.to_local(h.time(1961, 3,26,1,59,59,0,0)))
      assert_equal_with_offset(h.output_time(1961, 3,26,3, 0, 0,0,3600), tz.to_local(h.time(1961, 3,26,2, 0, 0,0,0)))
      assert_equal_with_offset(h.output_time(1961,10,29,2,59,59,0,3600), tz.to_local(h.time(1961,10,29,1,59,59,0,0)))
      assert_equal_with_offset(h.output_time(1961,10,29,2, 0, 0,0,   0), tz.to_local(h.time(1961,10,29,2, 0, 0,0,0)))

      assert_equal_with_offset(h.output_time(1961, 3,26,1,59,59,0,   0), tz.utc_to_local(h.time(1961, 3,26,1,59,59)))
      assert_equal_with_offset(h.output_time(1961, 3,26,3, 0, 0,0,3600), tz.utc_to_local(h.time(1961, 3,26,2, 0, 0)))
      assert_equal_with_offset(h.output_time(1961,10,29,2,59,59,0,3600), tz.utc_to_local(h.time(1961,10,29,1,59,59)))
      assert_equal_with_offset(h.output_time(1961,10,29,2, 0, 0,0,   0), tz.utc_to_local(h.time(1961,10,29,2, 0, 0)))

      assert_equal_with_offset(h.output_time(1961, 3,26,1,59,59,0,:utc), tz.local_to_utc(h.time(1961, 3,26,1,59,59)))
      assert_equal_with_offset(h.output_time(1961, 3,26,2, 0, 0,0,:utc), tz.local_to_utc(h.time(1961, 3,26,3, 0, 0)))
      assert_equal_with_offset(h.output_time(1961,10,29,1,59,59,0,:utc), tz.local_to_utc(h.time(1961,10,29,2,59,59), true))
      assert_equal_with_offset(h.output_time(1961,10,29,2,59,59,0,:utc), tz.local_to_utc(h.time(1961,10,29,2,59,59), false))
      assert_equal_with_offset(h.output_time(1961,10,29,1, 0, 0,0,:utc), tz.local_to_utc(h.time(1961,10,29,2, 0, 0), true))
      assert_equal_with_offset(h.output_time(1961,10,29,2, 0, 0,0,:utc), tz.local_to_utc(h.time(1961,10,29,2, 0, 0), false))

      assert_raises(PeriodNotFound) { tz.local_to_utc(h.time(1961, 3,26,2,0,0)) }
      assert_raises(AmbiguousTime)  { tz.local_to_utc(h.time(1961,10,29,2,0,0)) }

      assert_equal('GMT', tz.period_for(h.time(1961, 3,26,1,59,59,0,0)).zone_identifier)
      assert_equal('BST', tz.period_for(h.time(1961, 3,26,2, 0, 0,0,0)).zone_identifier)
      assert_equal('BST', tz.period_for(h.time(1961,10,29,1,59,59,0,0)).zone_identifier)
      assert_equal('GMT', tz.period_for(h.time(1961,10,29,2, 0, 0,0,0)).zone_identifier)

      assert_equal('GMT', tz.period_for_utc(h.time(1961, 3,26,1,59,59)).zone_identifier)
      assert_equal('BST', tz.period_for_utc(h.time(1961, 3,26,2, 0, 0)).zone_identifier)
      assert_equal('BST', tz.period_for_utc(h.time(1961,10,29,1,59,59)).zone_identifier)
      assert_equal('GMT', tz.period_for_utc(h.time(1961,10,29,2, 0, 0)).zone_identifier)

      assert_equal('GMT', tz.period_for_local(h.time(1961, 3,26,1,59,59)).zone_identifier)
      assert_equal('BST', tz.period_for_local(h.time(1961, 3,26,3, 0, 0)).zone_identifier)
      assert_equal('BST', tz.period_for_local(h.time(1961,10,29,2,59,59),  true).zone_identifier)
      assert_equal('GMT', tz.period_for_local(h.time(1961,10,29,2,59,59), false).zone_identifier)
      assert_equal('BST', tz.period_for_local(h.time(1961,10,29,2, 0, 0),  true).zone_identifier)
      assert_equal('GMT', tz.period_for_local(h.time(1961,10,29,2, 0, 0), false).zone_identifier)

      assert_equal(   0, tz.period_for(h.time(1961, 3,26,1,59,59,0,0)).observed_utc_offset)
      assert_equal(3600, tz.period_for(h.time(1961, 3,26,2, 0, 0,0,0)).observed_utc_offset)
      assert_equal(3600, tz.period_for(h.time(1961,10,29,1,59,59,0,0)).observed_utc_offset)
      assert_equal(   0, tz.period_for(h.time(1961,10,29,2, 0, 0,0,0)).observed_utc_offset)

      assert_equal(   0, tz.period_for_utc(h.time(1961, 3,26,1,59,59)).observed_utc_offset)
      assert_equal(3600, tz.period_for_utc(h.time(1961, 3,26,2, 0, 0)).observed_utc_offset)
      assert_equal(3600, tz.period_for_utc(h.time(1961,10,29,1,59,59)).observed_utc_offset)
      assert_equal(   0, tz.period_for_utc(h.time(1961,10,29,2, 0, 0)).observed_utc_offset)

      assert_equal(   0, tz.period_for_local(h.time(1961, 3,26,1,59,59)).observed_utc_offset)
      assert_equal(3600, tz.period_for_local(h.time(1961, 3,26,3, 0, 0)).observed_utc_offset)
      assert_equal(3600, tz.period_for_local(h.time(1961,10,29,2,59,59),  true).observed_utc_offset)
      assert_equal(   0, tz.period_for_local(h.time(1961,10,29,2,59,59), false).observed_utc_offset)
      assert_equal(3600, tz.period_for_local(h.time(1961,10,29,2, 0, 0),  true).observed_utc_offset)
      assert_equal(   0, tz.period_for_local(h.time(1961,10,29,2, 0, 0), false).observed_utc_offset)


      transitions = tz.transitions_up_to(h.time(1962,1,1,0,0,0,0,0), h.time(1961,1,1,0,0,0,0,0))
      assert_equal(2, transitions.length)
      assert_equal_with_offset(Timestamp.for(Time.utc(1961,3,26,2,0,0)), transitions[0].at)
      assert_equal(TimezoneOffset.new(0,    0, 'GMT'), transitions[0].previous_offset)
      assert_equal(TimezoneOffset.new(0, 3600, 'BST'), transitions[0].offset)
      assert_equal_with_offset(Timestamp.for(Time.utc(1961,10,29,2,0,0)), transitions[1].at)
      assert_equal(TimezoneOffset.new(0, 3600, 'BST'), transitions[1].previous_offset)
      assert_equal(TimezoneOffset.new(0,    0, 'GMT'), transitions[1].offset)

      offsets = tz.offsets_up_to(h.time(1962,1,1,0,0,0,0,0), h.time(1961,1,1,0,0,0,0,0))
      assert_array_same_items([TimezoneOffset.new(0, 0, 'GMT'), TimezoneOffset.new(0, 3600, 'BST')], offsets)
    end
  end

  def test_time_boundary
    #Europe/London  Sat Oct 26 23:00:00 1968 UTC = Sun Oct 27 00:00:00 1968 GMT isdst=0 gmtoff=3600
    #Europe/London  Sun Oct 31 01:59:59 1971 UTC = Sun Oct 31 02:59:59 1971 GMT isdst=0 gmtoff=3600

    tz = Timezone.get('Europe/London')

    time_types_test(:offset) do |h|
      assert_equal_with_offset(h.output_time(1970,1,1,1,0,0,0,3600),     tz.to_local(h.time(1970,1,1,0,0,0,0,0)))
      assert_equal_with_offset(h.output_time(1970,1,1,1,0,0,0,3600), tz.utc_to_local(h.time(1970,1,1,0,0,0)))
      assert_equal_with_offset(h.output_time(1970,1,1,0,0,0,0,:utc), tz.local_to_utc(h.time(1970,1,1,1,0,0)))
    end
  end
end
