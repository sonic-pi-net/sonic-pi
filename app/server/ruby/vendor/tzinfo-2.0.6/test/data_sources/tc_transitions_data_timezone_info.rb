# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module DataSources
  class TCTransitionsDataTimezoneInfo < Minitest::Test
    include TZInfo
    include TZInfo::DataSources

    def test_initialize_transitions
      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TESTD')
      o3 = TimezoneOffset.new(-18000,    0, 'TESTS')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000, 4,1,1,0,0).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2000,10,1,1,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o3, Time.utc(2001, 3,1,1,0,0).to_i)

      transitions = [t1, t2, t3]
      identifier = 'Test/Zone'.dup
      refute(identifier.frozen?)

      i = TransitionsDataTimezoneInfo.new(identifier, transitions)

      assert_same(identifier, i.identifier)
      assert_same(transitions, i.transitions)
      assert_equal(true, i.identifier.frozen?)
      assert_equal(true, i.transitions.frozen?)
    end

    def test_initialize_empty_transitions
      error = assert_raises(ArgumentError) { TransitionsDataTimezoneInfo.new('Test/Zone', []) }
      assert_match(/\btransitions\b/, error.message)
    end

    def test_initialize_nil_identifier
      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000,    0, 'TESTS')
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,10,1,1,0,0).to_i)
      transitions = [t1]

      error = assert_raises(ArgumentError) { TransitionsDataTimezoneInfo.new(nil, transitions) }
      assert_match(/\bidentifier\b/, error.message)
    end

    def test_initialize_nil_transitions
      error = assert_raises(ArgumentError) { TransitionsDataTimezoneInfo.new('Test/Zone', nil) }
      assert_match(/\btransitions\b/, error.message)
    end

    def test_period_for
      o1 = TimezoneOffset.new(-17900, 0,    'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TESTD')
      o3 = TimezoneOffset.new(-18000, 0,    'TESTS')
      o4 = TimezoneOffset.new(-21600, 3600, 'TESTD')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000, 4,1,1,0,0).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2000,10,1,1,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o3, Time.utc(2001, 3,1,1,0,0).to_i)
      t4 = TimezoneTransition.new(o4, o2, Time.utc(2001, 4,1,1,0,0).to_i)
      t5 = TimezoneTransition.new(o3, o4, Time.utc(2001,10,1,1,0,0).to_i)
      t6 = TimezoneTransition.new(o3, o3, Time.utc(2002,10,1,1,0,0).to_i)
      t7 = TimezoneTransition.new(o2, o3, Time.utc(2003, 2,1,1,0,0).to_i)
      t8 = TimezoneTransition.new(o3, o2, Time.utc(2003, 3,1,1,0,0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Zone', [t1,t2,t3,t4,t5,t6,t7,t8])

      assert_equal(TransitionsTimezonePeriod.new(nil, t1), i.period_for(Timestamp.for(Time.utc(1960, 1,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(nil, t1), i.period_for(Timestamp.for(Time.utc(1999,12,1,0, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(nil, t1), i.period_for(Timestamp.for(Time.utc(2000, 4,1,0,59,59))))
      assert_equal(TransitionsTimezonePeriod.new(t1, t2),  i.period_for(Timestamp.for(Time.utc(2000, 4,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t1, t2),  i.period_for(Timestamp.for(Time.utc(2000,10,1,0,59,59))))
      assert_equal(TransitionsTimezonePeriod.new(t2, t3),  i.period_for(Timestamp.for(Time.utc(2000,10,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t2, t3),  i.period_for(Timestamp.for(Time.utc(2001, 3,1,0,59,59))))
      assert_equal(TransitionsTimezonePeriod.new(t3, t4),  i.period_for(Timestamp.for(Time.utc(2001, 3,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t3, t4),  i.period_for(Timestamp.for(Time.utc(2001, 4,1,0,59,59))))
      assert_equal(TransitionsTimezonePeriod.new(t4, t5),  i.period_for(Timestamp.for(Time.utc(2001, 4,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t4, t5),  i.period_for(Timestamp.for(Time.utc(2001,10,1,0,59,59))))
      assert_equal(TransitionsTimezonePeriod.new(t5, t6),  i.period_for(Timestamp.for(Time.utc(2001,10,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t5, t6),  i.period_for(Timestamp.for(Time.utc(2002, 2,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t5, t6),  i.period_for(Timestamp.for(Time.utc(2002,10,1,0,59,59))))
      assert_equal(TransitionsTimezonePeriod.new(t6, t7),  i.period_for(Timestamp.for(Time.utc(2002,10,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t6, t7),  i.period_for(Timestamp.for(Time.utc(2003, 2,1,0,59,59))))
      assert_equal(TransitionsTimezonePeriod.new(t7, t8),  i.period_for(Timestamp.for(Time.utc(2003, 2,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t7, t8),  i.period_for(Timestamp.for(Time.utc(2003, 3,1,0,59,59))))
      assert_equal(TransitionsTimezonePeriod.new(t8, nil), i.period_for(Timestamp.for(Time.utc(2003, 3,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t8, nil), i.period_for(Timestamp.for(Time.utc(2004, 1,1,1, 0, 0))))
      assert_equal(TransitionsTimezonePeriod.new(t8, nil), i.period_for(Timestamp.for(Time.utc(2050, 1,1,1, 0, 0))))
    end

    def test_period_for_timestamp_with_zero_utc_offset
      o1 = TimezoneOffset.new(-17900, 0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 0, 'TEST')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,7,1,0,0,0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Zone', [t1])

      assert_equal(TransitionsTimezonePeriod.new(t1, nil), i.period_for(Timestamp.for(Time.new(2000,7,1,0,0,0,0))))
    end

    def test_period_for_timestamp_with_non_zero_utc_offset
      o1 = TimezoneOffset.new(-17900, 0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 0, 'TEST')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,7,1,0,0,0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Zone', [t1])

      assert_equal(TransitionsTimezonePeriod.new(t1, nil), i.period_for(Timestamp.for(Time.new(2000,6,30,23, 0, 0,-3600))))
      assert_equal(TransitionsTimezonePeriod.new(nil, t1), i.period_for(Timestamp.for(Time.new(2000,7, 1, 0,59,59, 3600))))
    end

    def create_basic_info
      o1 = TimezoneOffset.new(-17900, 0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 0, 'TEST')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,7,1,0,0,0).to_i)

      TransitionsDataTimezoneInfo.new('Test/Zone', [t1])
    end

    def test_period_for_timestamp_with_unspecified_offset
      i = create_basic_info
      error = assert_raises(ArgumentError) { i.period_for(Timestamp.for(Time.utc(2005,1,1,0,0,0), :ignore)) }
      assert_equal('timestamp must have a specified utc_offset', error.message)
    end

    def test_period_for_nil
      i = create_basic_info
      error = assert_raises(ArgumentError) { i.period_for(nil) }
      assert_equal('timestamp must be specified', error.message)
    end

    def test_periods_for_local
      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TESTD')
      o3 = TimezoneOffset.new(-18000,    0, 'TESTS')
      o4 = TimezoneOffset.new(-21600, 3600, 'TESTD')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000, 4,2,1,0,0).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2000,10,2,1,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o3, Time.utc(2001, 3,2,1,0,0).to_i)
      t4 = TimezoneTransition.new(o4, o2, Time.utc(2001, 4,2,1,0,0).to_i)
      t5 = TimezoneTransition.new(o3, o4, Time.utc(2001,10,2,1,0,0).to_i)
      t6 = TimezoneTransition.new(o3, o3, Time.utc(2002,10,2,1,0,0).to_i)
      t7 = TimezoneTransition.new(o2, o3, Time.utc(2003, 2,2,1,0,0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Zone', [t1,t2,t3,t4,t5,t6,t7])

      assert_equal([TransitionsTimezonePeriod.new(nil, t1)], i.periods_for_local(Timestamp.for(Time.utc(1960, 1, 1, 1, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(nil, t1)], i.periods_for_local(Timestamp.for(Time.utc(1999,12, 1, 0, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(nil, t1)], i.periods_for_local(Timestamp.for(Time.utc(2000, 1, 1,10, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(nil, t1)], i.periods_for_local(Timestamp.for(Time.utc(2000, 4, 1,20, 1,39), :ignore)))
      assert_equal([],                            i.periods_for_local(Timestamp.for(Time.utc(2000, 4, 1,20, 1,40), :ignore)))
      assert_equal([],                            i.periods_for_local(Timestamp.for(Time.utc(2000, 4, 1,20,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t1,  t2)], i.periods_for_local(Timestamp.for(Time.utc(2000, 4, 1,21, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t1,  t2)], i.periods_for_local(Timestamp.for(Time.utc(2000,10, 1,19,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t1,  t2),
                    TransitionsTimezonePeriod.new(t2,  t3)], i.periods_for_local(Timestamp.for(Time.utc(2000,10, 1,20, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t1,  t2),
                    TransitionsTimezonePeriod.new(t2,  t3)], i.periods_for_local(Timestamp.for(Time.utc(2000,10, 1,20,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t2,  t3)], i.periods_for_local(Timestamp.for(Time.utc(2000,10, 1,21, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t2,  t3)], i.periods_for_local(Timestamp.for(Time.utc(2001, 3, 1,19,59,59), :ignore)))
      assert_equal([],                            i.periods_for_local(Timestamp.for(Time.utc(2001, 3, 1,20, 0, 0), :ignore)))
      assert_equal([],                            i.periods_for_local(Timestamp.for(Time.utc(2001, 3, 1,20,30, 0), :ignore)))
      assert_equal([],                            i.periods_for_local(Timestamp.for(Time.utc(2001, 3, 1,20,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t3,  t4)], i.periods_for_local(Timestamp.for(Time.utc(2001, 3, 1,21, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t3,  t4)], i.periods_for_local(Timestamp.for(Time.utc(2001, 4, 1,19,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t3,  t4),
                    TransitionsTimezonePeriod.new(t4,  t5)], i.periods_for_local(Timestamp.for(Time.utc(2001, 4, 1,20, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t3,  t4),
                    TransitionsTimezonePeriod.new(t4,  t5)], i.periods_for_local(Timestamp.for(Time.utc(2001, 4, 1,20,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t4,  t5)], i.periods_for_local(Timestamp.for(Time.utc(2001, 4, 1,21, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t4,  t5)], i.periods_for_local(Timestamp.for(Time.utc(2001,10, 1,19,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t5,  t6)], i.periods_for_local(Timestamp.for(Time.utc(2001,10, 1,20, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t5,  t6)], i.periods_for_local(Timestamp.for(Time.utc(2002, 2, 1,20, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t5,  t6)], i.periods_for_local(Timestamp.for(Time.utc(2002,10, 1,19,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t6,  t7)], i.periods_for_local(Timestamp.for(Time.utc(2002,10, 1,20, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t6,  t7)], i.periods_for_local(Timestamp.for(Time.utc(2003, 2, 1,19,59,59), :ignore)))
      assert_equal([],                            i.periods_for_local(Timestamp.for(Time.utc(2003, 2, 1,20, 0, 0), :ignore)))
      assert_equal([],                            i.periods_for_local(Timestamp.for(Time.utc(2003, 2, 1,20,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t7, nil)], i.periods_for_local(Timestamp.for(Time.utc(2003, 2, 1,21, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t7, nil)], i.periods_for_local(Timestamp.for(Time.utc(2004, 2, 1,20, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t7, nil)], i.periods_for_local(Timestamp.for(Time.utc(2040, 2, 1,20, 0, 0), :ignore)))
    end

    def test_periods_for_local_warsaw
      o1 = TimezoneOffset.new(5040,    0, 'LMT')
      o2 = TimezoneOffset.new(5040,    0, 'WMT')
      o3 = TimezoneOffset.new(3600,    0, 'CET')
      o4 = TimezoneOffset.new(3600, 3600, 'CEST')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(1879,12,31,22,36,0).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(1915, 8, 4,22,36,0).to_i)
      t3 = TimezoneTransition.new(o4, o3, Time.utc(1916, 4,30,22, 0,0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Europe/Warsaw', [t1,t2,t3])

      assert_equal([TransitionsTimezonePeriod.new(t1, t2),
                    TransitionsTimezonePeriod.new(t2, t3)], i.periods_for_local(Timestamp.for(Time.utc(1915,8,4,23,40,0), :ignore)))
    end

    def test_periods_for_local_single_transition
      o1 = TimezoneOffset.new(-3600, 0, 'TESTD')
      o2 = TimezoneOffset.new(-3600, 0, 'TESTS')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,7,1,0,0,0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Zone', [t1])

      assert_equal([TransitionsTimezonePeriod.new(nil, t1)], i.periods_for_local(Timestamp.for(Time.utc(2000,6,30,22,59,59), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t1, nil)], i.periods_for_local(Timestamp.for(Time.utc(2000,6,30,23, 0, 0), :ignore)))
      assert_equal([TransitionsTimezonePeriod.new(t1, nil)], i.periods_for_local(Timestamp.for(Time.utc(2000,7, 1, 0, 0, 0), :ignore)))
    end

    def test_periods_for_local_timestamp_with_specified_offset
      i = create_basic_info
      error = assert_raises(ArgumentError) { i.periods_for_local(Timestamp.for(Time.utc(2005,1,1,0,0,0))) }
      assert_equal('local_timestamp must have an unspecified utc_offset', error.message)
    end

    def test_periods_for_local_nil
      i = create_basic_info
      error = assert_raises(ArgumentError) { i.periods_for_local(nil) }
      assert_equal('local_timestamp must be specified', error.message)
    end

    def test_transitions_up_to
      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TESTD')
      o3 = TimezoneOffset.new(-18000,    0, 'TESTS')
      o4 = TimezoneOffset.new(-21600, 3600, 'TESTD')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2010, 4,1,1,0,0).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2010,10,1,1,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o3, Time.utc(2011, 3,1,1,0,0).to_i)
      t4 = TimezoneTransition.new(o4, o2, Time.utc(2011, 4,1,1,0,0).to_i)
      t5 = TimezoneTransition.new(o3, o4, Time.utc(2011,10,1,1,0,0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Zone', [t1,t2,t3,t4,t5])

      assert_equal([],               i.transitions_up_to(Timestamp.for(Time.utc(2010, 4,1,1,0,0))))
      assert_equal([],               i.transitions_up_to(Timestamp.for(Time.utc(2010, 4,1,1,0,0)), Timestamp.for(Time.utc(2000, 1,1,0,0,0))))
      assert_equal([t1],             i.transitions_up_to(Timestamp.for(Time.utc(2010, 4,1,1,0,1))))
      assert_equal([t1],             i.transitions_up_to(Timestamp.for(Time.utc(2010, 4,1,1,0,1)), Timestamp.for(Time.utc(2000, 1,1,0,0,0))))
      assert_equal([t2,t3,t4],       i.transitions_up_to(Timestamp.for(Time.utc(2011, 4,1,1,0,1)), Timestamp.for(Time.utc(2010,10,1,1,0,0))))
      assert_equal([t2,t3,t4],       i.transitions_up_to(Timestamp.for(Time.utc(2011,10,1,1,0,0)), Timestamp.for(Time.utc(2010, 4,1,1,0,1))))
      assert_equal([t3],             i.transitions_up_to(Timestamp.for(Time.utc(2011, 4,1,1,0,0)), Timestamp.for(Time.utc(2010,10,1,1,0,1))))
      assert_equal([],               i.transitions_up_to(Timestamp.for(Time.utc(2011, 3,1,1,0,0)), Timestamp.for(Time.utc(2010,10,1,1,0,1))))
      assert_equal([t1,t2,t3,t4],    i.transitions_up_to(Timestamp.for(Time.utc(2011,10,1,1,0,0))))
      assert_equal([t1,t2,t3,t4,t5], i.transitions_up_to(Timestamp.for(Time.utc(2011,10,1,1,0,1))))
      assert_equal([t1,t2,t3,t4,t5], i.transitions_up_to(Timestamp.for(Time.utc(2011,10,1,1,0,0,1))))
      assert_equal([t1,t2,t3,t4,t5], i.transitions_up_to(Timestamp.for(Time.utc(2011,10,1,1,0,1)), Timestamp.for(Time.utc(2010, 4,1,1,0,0))))
      assert_equal([t2,t3,t4,t5],    i.transitions_up_to(Timestamp.for(Time.utc(2011,10,1,1,0,1)), Timestamp.for(Time.utc(2010, 4,1,1,0,1))))
      assert_equal([t2,t3,t4,t5],    i.transitions_up_to(Timestamp.for(Time.utc(2011,10,1,1,0,1)), Timestamp.for(Time.utc(2010, 4,1,1,0,0,1))))
      assert_equal([t5],             i.transitions_up_to(Timestamp.for(Time.utc(2015, 1,1,0,0,0)), Timestamp.for(Time.utc(2011,10,1,1,0,0))))
      assert_equal([],               i.transitions_up_to(Timestamp.for(Time.utc(2015, 1,1,0,0,0)), Timestamp.for(Time.utc(2011,10,1,1,0,1))))
    end

    def test_transitions_up_to_timestamp_with_zero_utc_offset
      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000,    0, 'TESTS')
      o3 = TimezoneOffset.new(-18000, 3600, 'TESTD')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2009,12,31,23,59,59).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2010, 7, 1, 0, 0, 0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Zone', [t1,t2])

      assert_equal([t1,t2], i.transitions_up_to(Timestamp.for(Time.new(2010,7,1,0,0,1,0))))
      assert_equal([t1,t2], i.transitions_up_to(Timestamp.for(Time.new(2011,1,1,0,0,0,0)), Timestamp.for(Time.new(2009,12,31,23,59,59,0))))
    end

    def test_transitions_up_to_timestamp_with_non_zero_utc_offset
      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000,    0, 'TESTS')
      o3 = TimezoneOffset.new(-18000, 3600, 'TESTD')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2009,12,31,23,59,59).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2010, 7, 1, 0, 0, 0).to_i)

      i = TransitionsDataTimezoneInfo.new('Test/Zone', [t1,t2])

      assert_equal([t1,t2], i.transitions_up_to(Timestamp.for(Time.new(2010,6,30,23,0,1,-3600))))
      assert_equal([t1],    i.transitions_up_to(Timestamp.for(Time.new(2010,7, 1, 1,0,0, 3600))))
      assert_equal([t1,t2], i.transitions_up_to(Timestamp.for(Time.new(2011,1,1,0,0,0,0)), Timestamp.for(Time.new(2010, 1, 1, 0,59,59, 3600))))
      assert_equal([t2],    i.transitions_up_to(Timestamp.for(Time.new(2011,1,1,0,0,0,0)), Timestamp.for(Time.new(2009,12,31,23, 0, 0,-3600))))
    end

    def test_transitions_up_to_to_not_greater_than_from
      i = create_basic_info

      to = Timestamp.for(Time.utc(2012,8,1,0,0,0))
      from = Timestamp.for(Time.utc(2013,8,1,0,0,0))

      error = assert_raises(ArgumentError) { i.transitions_up_to(to, from) }
      assert_equal('to_timestamp must be greater than from_timestamp', error.message)
    end

    def test_transitions_up_to_to_not_greater_than_from_subsecond
      i = create_basic_info

      to = Timestamp.for(Time.utc(2012,8,1,0,0,0))
      from = Timestamp.for(Time.utc(2012,8,1,0,0,0,1))

      error = assert_raises(ArgumentError) { i.transitions_up_to(to, from) }
      assert_equal('to_timestamp must be greater than from_timestamp', error.message)
    end

    def test_transitions_up_to_to_timestamp_with_unspecified_offset
      i = create_basic_info

      to = Timestamp.for(Time.utc(2015,1,1,0,0,0), :ignore)

      error = assert_raises(ArgumentError) { i.transitions_up_to(to) }
      assert_equal('to_timestamp must have a specified utc_offset', error.message)
    end

    def test_transitions_up_to_from_timestamp_with_unspecified_offset
      i = create_basic_info

      to = Timestamp.for(Time.utc(2015,1,1,0,0,0))
      from = Timestamp.for(Time.utc(2014,1,1,0,0,0), :ignore)

      error = assert_raises(ArgumentError) { i.transitions_up_to(to, from) }
      assert_equal('from_timestamp must have a specified utc_offset', error.message)
    end

    def test_transitions_up_to_to_timestamp_nil
      i = create_basic_info
      error = assert_raises(ArgumentError) { i.transitions_up_to(nil) }
      assert_equal('to_timestamp must be specified', error.message)
    end

    def test_inspect
      i = create_basic_info
      assert_equal('#<TZInfo::DataSources::TransitionsDataTimezoneInfo: Test/Zone>', i.inspect)
    end
  end
end
