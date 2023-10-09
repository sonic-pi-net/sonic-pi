# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module Format1
  class TCTimezoneDefiner < Minitest::Test
    include TZInfo
    include TZInfo.const_get(:Format1)

    def setup
      @string_deduper = StringDeduper.new
      @definer = TimezoneDefiner.new(@string_deduper)
    end

    def test_transitions_with_timestamps
      @definer.offset :o1, -17900,    0, :TESTLMT
      @definer.offset :o2, -18000, 3600, :TEST
      @definer.offset :o3, -18000,    0, :TESTD
      @definer.transition 2016, 3, :o2, 1456790400
      @definer.transition 2016, 9, :o3, 1472688000
      @definer.transition 2016, 3, :o2, 1488326400

      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TEST')
      o3 = TimezoneOffset.new(-18000,    0, 'TESTD')
      t1 = TimezoneTransition.new(o2, o1, 1456790400)
      t2 = TimezoneTransition.new(o3, o2, 1472688000)
      t3 = TimezoneTransition.new(o2, o3, 1488326400)

      assert_equal(o1, @definer.first_offset)
      assert_equal([t1,t2,t3], @definer.transitions)
    end

    def test_transitions_with_timestamps_and_datetimes
      @definer.offset :o1, -17900,    0, :TESTLMT
      @definer.offset :o2, -18000, 3600, :TEST
      @definer.offset :o3, -18000,    0, :TESTD

      # DateTimes are defined 1 second after the timestamps. The timestamp values
      # will be used to construct TimezoneTransitions.
      @definer.transition 2016, 3, :o2, 1456790400, 212323550401, 86400
      @definer.transition 2016, 9, :o3, 1472688000, 212339448001, 86400
      @definer.transition 2016, 3, :o2, 1488326400, 212355086401, 86400

      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TEST')
      o3 = TimezoneOffset.new(-18000,    0, 'TESTD')
      t1 = TimezoneTransition.new(o2, o1, 1456790400)
      t2 = TimezoneTransition.new(o3, o2, 1472688000)
      t3 = TimezoneTransition.new(o2, o3, 1488326400)

      assert_equal(o1, @definer.first_offset)
      assert_equal([t1,t2,t3], @definer.transitions)
    end

    def test_transition_with_datetime
      # The ability to specify a transition solely as a DateTime has not been used
      # in any released version of TZInfo::Data. This is now not supporte@definer.
      @definer.offset :o1, -17900, 0, :TESTLMT

      error = assert_raises(ArgumentError) { @definer.transition 2016, 3, :o1, 4914897, 2 }
      assert_match(/\bDateTime\b/, error.message)
    end

    def test_strings_deduped
      abbreviation = @string_deduper.dedupe(:SYMDDLMT.to_s)

      @definer.offset :o1, 3600, 0, :SYMDDLMT

      assert_same(abbreviation, @definer.first_offset.abbreviation)
    end
  end
end
