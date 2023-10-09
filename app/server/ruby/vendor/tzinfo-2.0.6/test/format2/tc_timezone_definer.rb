# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module Format2
  class TCTimezoneDefiner < Minitest::Test
    include TZInfo
    include TZInfo.const_get(:Format2)

    def setup
      @string_deduper = StringDeduper.new
      @definer = TimezoneDefiner.new(@string_deduper)
    end

    def test_no_offsets_or_transitions
      assert_nil(@definer.first_offset)
      assert_equal([], @definer.transitions)
    end

    def test_single_offset_no_transitions
      @definer.offset :o1, -17900, 0, 'TESTLMT'

      o1 = TimezoneOffset.new(-17900, 0, 'TESTLMT')

      assert_equal(o1, @definer.first_offset)
      assert_equal([], @definer.transitions)
    end

    def test_single_offset_single_transition
      @definer.offset :o1, -17900, 0, 'TESTLMT'
      @definer.transition :o1, 1456790400

      o1 = TimezoneOffset.new(-17900, 0, 'TESTLMT')
      t1 = TimezoneTransition.new(o1, o1, 1456790400)

      assert_equal(o1, @definer.first_offset)
      assert_equal([t1], @definer.transitions)
    end

    def test_multiple_offsets_single_transition
      @definer.offset :o1, -17900,    0, 'TESTLMT'
      @definer.offset :o2, -18000, 3600, 'TEST'
      @definer.transition :o2, 1456790400

      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TEST')
      t1 = TimezoneTransition.new(o2, o1, 1456790400)

      assert_equal(o1, @definer.first_offset)
      assert_equal([t1], @definer.transitions)
    end

    def test_multiple_offsets_multiple_transitions
      @definer.offset :o1, -17900,    0, 'TESTLMT'
      @definer.offset :o2, -18000, 3600, 'TEST'
      @definer.offset :o3, -18000,    0, 'TESTD'
      @definer.transition :o2, 1456790400
      @definer.transition :o3, 1472688000
      @definer.transition :o2, 1488326400

      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TEST')
      o3 = TimezoneOffset.new(-18000,    0, 'TESTD')
      t1 = TimezoneTransition.new(o2, o1, 1456790400)
      t2 = TimezoneTransition.new(o3, o2, 1472688000)
      t3 = TimezoneTransition.new(o2, o3, 1488326400)

      assert_equal(o1, @definer.first_offset)
      assert_equal([t1,t2,t3], @definer.transitions)
    end

    def test_offset_already_defined
      @definer.offset :o1, -17900, 0, 'TESTLMT'
      @definer.offset :o2,  18000, 0, 'TEST'

      error = assert_raises(ArgumentError) { @definer.offset :o2, 1800, 3600, :TESTD }
      assert_match(/\bid\b/, error.message)
    end

    def test_transition_with_no_offsets
      error = assert_raises(ArgumentError) { @definer.transition :o1, 1456790400 }
      assert_match(/\boffset_id\b/, error.message)
    end

    def test_transition_with_undefined_offset
      @definer.offset :o1, -17900, 0, 'TESTLMT'
      @definer.transition :o1, 1456790400

      error = assert_raises(ArgumentError) { @definer.transition :o2, 1472688000 }
      assert_match(/\boffset_id\b/, error.message)
    end

    def test_transition_not_increased
      @definer.offset :o1, -17900, 0, 'TESTLMT'
      @definer.transition :o1, 1456790400

      error = assert_raises(ArgumentError) { @definer.transition :o1, 1456790400 }
      assert_match(/\btimestamp\b/, error.message)
    end

    def test_transition_decreased
      @definer.offset :o1, -17900, 0, 'TESTLMT'
      @definer.transition :o1, 1456790400

      error = assert_raises(ArgumentError) { @definer.transition :o1, 1456790399 }
      assert_match(/\btimestamp\b/, error.message)
    end

    def test_strings_deduped
      abbreviation = @string_deduper.dedupe('DEFDDSMT')

      @definer.offset :o1, 3600, 0, 'DEFDDSMT'
      assert_same(abbreviation, @definer.first_offset.abbreviation)
    end

    # subsequent_rules is just a placeholder for forward compatibility,
    # accepting an arbitrary number of arguments and ignoring them.
    0.upto(5) do |n|
      define_method("test_subsequent_rules_#{n}_args") do
        assert_nil(@definer.subsequent_rules(*(0..n).to_a))
      end
    end
  end
end
