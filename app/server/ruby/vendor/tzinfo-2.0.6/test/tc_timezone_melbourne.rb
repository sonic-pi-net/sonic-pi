# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTimezoneMelbourne < Minitest::Test
  include TZInfo

  def test_2004
    #Australia/Melbourne  Sat Mar 27 15:59:59 2004 UTC = Sun Mar 28 02:59:59 2004 AEDT isdst=1 gmtoff=39600
    #Australia/Melbourne  Sat Mar 27 16:00:00 2004 UTC = Sun Mar 28 02:00:00 2004 AEST isdst=0 gmtoff=36000
    #Australia/Melbourne  Sat Oct 30 15:59:59 2004 UTC = Sun Oct 31 01:59:59 2004 AEST isdst=0 gmtoff=36000
    #Australia/Melbourne  Sat Oct 30 16:00:00 2004 UTC = Sun Oct 31 03:00:00 2004 AEDT isdst=1 gmtoff=39600

    tz = Timezone.get('Australia/Melbourne')

    time_types_test(:offset) do |h|
      assert_equal_with_offset(h.output_time(2004, 3,28,2,59,59,0,39600), tz.to_local(h.time(2004, 3,27,15,59,59,0,0)))
      assert_equal_with_offset(h.output_time(2004, 3,28,2, 0, 0,0,36000), tz.to_local(h.time(2004, 3,27,16, 0, 0,0,0)))
      assert_equal_with_offset(h.output_time(2004,10,31,1,59,59,0,36000), tz.to_local(h.time(2004,10,30,15,59,59,0,0)))
      assert_equal_with_offset(h.output_time(2004,10,31,3, 0, 0,0,39600), tz.to_local(h.time(2004,10,30,16, 0, 0,0,0)))

      assert_equal_with_offset(h.output_time(2004, 3,28,2,59,59,0,39600), tz.utc_to_local(h.time(2004, 3,27,15,59,59)))
      assert_equal_with_offset(h.output_time(2004, 3,28,2, 0, 0,0,36000), tz.utc_to_local(h.time(2004, 3,27,16, 0, 0)))
      assert_equal_with_offset(h.output_time(2004,10,31,1,59,59,0,36000), tz.utc_to_local(h.time(2004,10,30,15,59,59)))
      assert_equal_with_offset(h.output_time(2004,10,31,3, 0, 0,0,39600), tz.utc_to_local(h.time(2004,10,30,16, 0, 0)))

      assert_equal_with_offset(h.output_time(2004, 3,27,15,59,59,0,:utc), tz.local_to_utc(h.time(2004, 3,28,2,59,59),  true))
      assert_equal_with_offset(h.output_time(2004, 3,27,16,59,59,0,:utc), tz.local_to_utc(h.time(2004, 3,28,2,59,59), false))
      assert_equal_with_offset(h.output_time(2004, 3,27,15, 0, 0,0,:utc), tz.local_to_utc(h.time(2004, 3,28,2, 0, 0),  true))
      assert_equal_with_offset(h.output_time(2004, 3,27,16, 0, 0,0,:utc), tz.local_to_utc(h.time(2004, 3,28,2, 0, 0), false))
      assert_equal_with_offset(h.output_time(2004,10,30,15,59,59,0,:utc), tz.local_to_utc(h.time(2004,10,31,1,59,59)))
      assert_equal_with_offset(h.output_time(2004,10,30,16, 0, 0,0,:utc), tz.local_to_utc(h.time(2004,10,31,3, 0, 0)))

      assert_raises(PeriodNotFound) { tz.local_to_utc(h.time(2004,10,31,2,0,0)) }
      assert_raises(AmbiguousTime)  { tz.local_to_utc(h.time(2004, 3,28,2,0,0)) }

      assert_equal('AEDT', tz.period_for(h.time(2004, 3,27,15,59,59,0,0)).zone_identifier)
      assert_equal('AEST', tz.period_for(h.time(2004, 3,27,16, 0, 0,0,0)).zone_identifier)
      assert_equal('AEST', tz.period_for(h.time(2004,10,30,15,59,59,0,0)).zone_identifier)
      assert_equal('AEDT', tz.period_for(h.time(2004,10,30,16, 0, 0,0,0)).zone_identifier)

      assert_equal('AEDT', tz.period_for_utc(h.time(2004, 3,27,15,59,59)).zone_identifier)
      assert_equal('AEST', tz.period_for_utc(h.time(2004, 3,27,16, 0, 0)).zone_identifier)
      assert_equal('AEST', tz.period_for_utc(h.time(2004,10,30,15,59,59)).zone_identifier)
      assert_equal('AEDT', tz.period_for_utc(h.time(2004,10,30,16, 0, 0)).zone_identifier)

      assert_equal('AEDT', tz.period_for_local(h.time(2004, 3,28,2,59,59),  true).zone_identifier)
      assert_equal('AEST', tz.period_for_local(h.time(2004, 3,28,2,59,59), false).zone_identifier)
      assert_equal('AEDT', tz.period_for_local(h.time(2004, 3,28,2, 0, 0),  true).zone_identifier)
      assert_equal('AEST', tz.period_for_local(h.time(2004, 3,28,2, 0, 0), false).zone_identifier)
      assert_equal('AEST', tz.period_for_local(h.time(2004,10,31,1,59,59)).zone_identifier)
      assert_equal('AEDT', tz.period_for_local(h.time(2004,10,31,3, 0, 0)).zone_identifier)

      assert_equal(39600, tz.period_for(h.time(2004, 3,27,15,59,59,0,0)).observed_utc_offset)
      assert_equal(36000, tz.period_for(h.time(2004, 3,27,16, 0, 0,0,0)).observed_utc_offset)
      assert_equal(36000, tz.period_for(h.time(2004,10,30,15,59,59,0,0)).observed_utc_offset)
      assert_equal(39600, tz.period_for(h.time(2004,10,30,16, 0, 0,0,0)).observed_utc_offset)

      assert_equal(39600, tz.period_for_utc(h.time(2004, 3,27,15,59,59)).observed_utc_offset)
      assert_equal(36000, tz.period_for_utc(h.time(2004, 3,27,16, 0, 0)).observed_utc_offset)
      assert_equal(36000, tz.period_for_utc(h.time(2004,10,30,15,59,59)).observed_utc_offset)
      assert_equal(39600, tz.period_for_utc(h.time(2004,10,30,16, 0, 0)).observed_utc_offset)

      assert_equal(39600, tz.period_for_local(h.time(2004, 3,28,2,59,59),  true).observed_utc_offset)
      assert_equal(36000, tz.period_for_local(h.time(2004, 3,28,2,59,59), false).observed_utc_offset)
      assert_equal(39600, tz.period_for_local(h.time(2004, 3,28,2, 0, 0),  true).observed_utc_offset)
      assert_equal(36000, tz.period_for_local(h.time(2004, 3,28,2, 0, 0), false).observed_utc_offset)
      assert_equal(36000, tz.period_for_local(h.time(2004,10,31,1,59,59)).observed_utc_offset)
      assert_equal(39600, tz.period_for_local(h.time(2004,10,31,3, 0, 0)).observed_utc_offset)

      transitions = tz.transitions_up_to(h.time(2005,1,1,0,0,0,0,0), h.time(2004,1,1,0,0,0,0,0))
      assert_equal(2, transitions.length)
      assert_equal_with_offset(Timestamp.for(Time.utc(2004,3,27,16,0,0)), transitions[0].at)
      assert_equal(TimezoneOffset.new(36000, 3600, 'AEDT'), transitions[0].previous_offset)
      assert_equal(TimezoneOffset.new(36000,    0, 'AEST'), transitions[0].offset)
      assert_equal_with_offset(Timestamp.for(Time.utc(2004,10,30,16,0,0)), transitions[1].at)
      assert_equal(TimezoneOffset.new(36000,    0, 'AEST'), transitions[1].previous_offset)
      assert_equal(TimezoneOffset.new(36000, 3600, 'AEDT'), transitions[1].offset)

      offsets = tz.offsets_up_to(h.time(2005,1,1,0,0,0,0,0), h.time(2004,1,1,0,0,0,0,0))
      assert_array_same_items([TimezoneOffset.new(36000, 0, 'AEST'), TimezoneOffset.new(36000, 3600, 'AEDT')], offsets)
    end
  end

  def test_1942
    #Australia/Melbourne  Sat Mar 28 14:59:59 1942 UTC = Sun Mar 29 01:59:59 1942 AEDT isdst=1 gmtoff=39600
    #Australia/Melbourne  Sat Mar 28 15:00:00 1942 UTC = Sun Mar 29 01:00:00 1942 AEST isdst=0 gmtoff=36000
    #Australia/Melbourne  Sat Sep 26 15:59:59 1942 UTC = Sun Sep 27 01:59:59 1942 AEST isdst=0 gmtoff=36000
    #Australia/Melbourne  Sat Sep 26 16:00:00 1942 UTC = Sun Sep 27 03:00:00 1942 AEDT isdst=1 gmtoff=39600

    tz = Timezone.get('Australia/Melbourne')

    time_types_test(:offset) do |h|
      assert_equal_with_offset(h.output_time(1942,3,29,1,59,59,0,39600), tz.to_local(h.time(1942,3,28,14,59,59,0,0)))
      assert_equal_with_offset(h.output_time(1942,3,29,1, 0, 0,0,36000), tz.to_local(h.time(1942,3,28,15, 0, 0,0,0)))
      assert_equal_with_offset(h.output_time(1942,9,27,1,59,59,0,36000), tz.to_local(h.time(1942,9,26,15,59,59,0,0)))
      assert_equal_with_offset(h.output_time(1942,9,27,3, 0, 0,0,39600), tz.to_local(h.time(1942,9,26,16, 0, 0,0,0)))

      assert_equal_with_offset(h.output_time(1942,3,29,1,59,59,0,39600), tz.utc_to_local(h.time(1942,3,28,14,59,59)))
      assert_equal_with_offset(h.output_time(1942,3,29,1, 0, 0,0,36000), tz.utc_to_local(h.time(1942,3,28,15, 0, 0)))
      assert_equal_with_offset(h.output_time(1942,9,27,1,59,59,0,36000), tz.utc_to_local(h.time(1942,9,26,15,59,59)))
      assert_equal_with_offset(h.output_time(1942,9,27,3, 0, 0,0,39600), tz.utc_to_local(h.time(1942,9,26,16, 0, 0)))

      assert_equal_with_offset(h.output_time(1942,3,28,14,59,59,0,:utc), tz.local_to_utc(h.time(1942,3,29,1,59,59),  true))
      assert_equal_with_offset(h.output_time(1942,3,28,15,59,59,0,:utc), tz.local_to_utc(h.time(1942,3,29,1,59,59), false))
      assert_equal_with_offset(h.output_time(1942,3,28,14, 0, 0,0,:utc), tz.local_to_utc(h.time(1942,3,29,1, 0, 0),  true))
      assert_equal_with_offset(h.output_time(1942,3,28,15, 0, 0,0,:utc), tz.local_to_utc(h.time(1942,3,29,1, 0, 0), false))
      assert_equal_with_offset(h.output_time(1942,9,26,15,59,59,0,:utc), tz.local_to_utc(h.time(1942,9,27,1,59,59)))
      assert_equal_with_offset(h.output_time(1942,9,26,16, 0, 0,0,:utc), tz.local_to_utc(h.time(1942,9,27,3, 0, 0)))

      assert_raises(PeriodNotFound) { tz.local_to_utc(h.time(1942,9,27,2,0,0)) }
      assert_raises(AmbiguousTime)  { tz.local_to_utc(h.time(1942,3,29,1,0,0)) }

      assert_equal('AEDT', tz.period_for(h.time(1942,3,28,14,59,59,0,0)).zone_identifier)
      assert_equal('AEST', tz.period_for(h.time(1942,3,28,15, 0, 0,0,0)).zone_identifier)
      assert_equal('AEST', tz.period_for(h.time(1942,9,26,15,59,59,0,0)).zone_identifier)
      assert_equal('AEDT', tz.period_for(h.time(1942,9,26,16, 0, 0,0,0)).zone_identifier)

      assert_equal('AEDT', tz.period_for_utc(h.time(1942,3,28,14,59,59)).zone_identifier)
      assert_equal('AEST', tz.period_for_utc(h.time(1942,3,28,15, 0, 0)).zone_identifier)
      assert_equal('AEST', tz.period_for_utc(h.time(1942,9,26,15,59,59)).zone_identifier)
      assert_equal('AEDT', tz.period_for_utc(h.time(1942,9,26,16, 0, 0)).zone_identifier)

      assert_equal('AEDT', tz.period_for_local(h.time(1942,3,29,1,59,59),  true).zone_identifier)
      assert_equal('AEST', tz.period_for_local(h.time(1942,3,29,1,59,59), false).zone_identifier)
      assert_equal('AEDT', tz.period_for_local(h.time(1942,3,29,1, 0, 0),  true).zone_identifier)
      assert_equal('AEST', tz.period_for_local(h.time(1942,3,29,1, 0, 0), false).zone_identifier)
      assert_equal('AEST', tz.period_for_local(h.time(1942,9,27,1,59,59)).zone_identifier)
      assert_equal('AEDT', tz.period_for_local(h.time(1942,9,27,3, 0, 0)).zone_identifier)

      assert_equal(39600, tz.period_for(h.time(1942,3,28,14,59,59,0,0)).observed_utc_offset)
      assert_equal(36000, tz.period_for(h.time(1942,3,28,15, 0, 0,0,0)).observed_utc_offset)
      assert_equal(36000, tz.period_for(h.time(1942,9,26,15,59,59,0,0)).observed_utc_offset)
      assert_equal(39600, tz.period_for(h.time(1942,9,26,16, 0, 0,0,0)).observed_utc_offset)

      assert_equal(39600, tz.period_for_utc(h.time(1942,3,28,14,59,59)).observed_utc_offset)
      assert_equal(36000, tz.period_for_utc(h.time(1942,3,28,15, 0, 0)).observed_utc_offset)
      assert_equal(36000, tz.period_for_utc(h.time(1942,9,26,15,59,59)).observed_utc_offset)
      assert_equal(39600, tz.period_for_utc(h.time(1942,9,26,16, 0, 0)).observed_utc_offset)

      assert_equal(39600, tz.period_for_local(h.time(1942,3,29,1,59,59),  true).observed_utc_offset)
      assert_equal(36000, tz.period_for_local(h.time(1942,3,29,1,59,59), false).observed_utc_offset)
      assert_equal(39600, tz.period_for_local(h.time(1942,3,29,1, 0, 0),  true).observed_utc_offset)
      assert_equal(36000, tz.period_for_local(h.time(1942,3,29,1, 0, 0), false).observed_utc_offset)
      assert_equal(36000, tz.period_for_local(h.time(1942,9,27,1,59,59)).observed_utc_offset)
      assert_equal(39600, tz.period_for_local(h.time(1942,9,27,3, 0, 0)).observed_utc_offset)

      transitions = tz.transitions_up_to(h.time(1943,1,1,0,0,0,0,0), h.time(1942,1,1,0,0,0,0,0))
      assert_equal(2, transitions.length)
      assert_equal_with_offset(Timestamp.for(Time.utc(1942,3,28,15,0,0)), transitions[0].at)
      assert_equal(TimezoneOffset.new(36000, 3600, 'AEDT'), transitions[0].previous_offset)
      assert_equal(TimezoneOffset.new(36000,    0, 'AEST'), transitions[0].offset)
      assert_equal_with_offset(Timestamp.for(Time.utc(1942,9,26,16,0,0)), transitions[1].at)
      assert_equal(TimezoneOffset.new(36000,    0, 'AEST'), transitions[1].previous_offset)
      assert_equal(TimezoneOffset.new(36000, 3600, 'AEDT'), transitions[1].offset)

      offsets = tz.offsets_up_to(h.time(1943,1,1,0,0,0,0,0), h.time(1942,1,1,0,0,0,0,0))
      assert_array_same_items([TimezoneOffset.new(36000, 0, 'AEST'), TimezoneOffset.new(36000, 3600, 'AEDT')], offsets)
    end
  end

  def test_time_boundary
    #Australia/Melbourne  Sat Mar 25 15:00:00 1944 UTC = Sun Mar 26 01:00:00 1944 AEST isdst=0 gmtoff=36000
    #Australia/Melbourne  Sat Oct 30 15:59:59 1971 UTC = Sun Oct 31 01:59:59 1971 AEST isdst=0 gmtoff=36000

    tz = Timezone.get('Australia/Melbourne')

    time_types_test(:offset) do |h|
      assert_equal_with_offset(h.output_time(1970,1,1,10,0,0,0,36000),     tz.to_local(h.time(1970,1,1, 0,0,0,0,0)))
      assert_equal_with_offset(h.output_time(1970,1,1,10,0,0,0,36000), tz.utc_to_local(h.time(1970,1,1, 0,0,0)))
      assert_equal_with_offset(h.output_time(1970,1,1, 0,0,0,0, :utc), tz.local_to_utc(h.time(1970,1,1,10,0,0)))
    end
  end
end
