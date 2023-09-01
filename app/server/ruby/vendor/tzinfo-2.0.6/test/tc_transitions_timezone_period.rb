# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTransitionsTimezonePeriod < Minitest::Test
  include TZInfo

  def test_initialize_start_end
    std = TimezoneOffset.new(-7200, 0, 'TEST')
    dst = TimezoneOffset.new(-7200, 3600, 'TEST')
    start_t = TimezoneTransition.new(dst, std, 1136073600)
    end_t = TimezoneTransition.new(std, dst, 1136160000)

    p = TransitionsTimezonePeriod.new(start_t, end_t)

    assert_same(start_t, p.start_transition)
    assert_same(end_t, p.end_transition)
    assert_same(dst, p.offset)
  end

  def test_initialize_start
    std = TimezoneOffset.new(-7200, 0, 'TEST')
    dst = TimezoneOffset.new(-7200, 3600, 'TEST')
    start_t = TimezoneTransition.new(dst, std, 1136073600)

    p = TransitionsTimezonePeriod.new(start_t, nil)

    assert_same(start_t, p.start_transition)
    assert_nil(p.end_transition)
    assert_same(dst, p.offset)
  end

  def test_initialize_end
    std = TimezoneOffset.new(-7200, 0, 'TEST')
    dst = TimezoneOffset.new(-7200, 3600, 'TEST')
    end_t = TimezoneTransition.new(std, dst, 1136160000)

    p = TransitionsTimezonePeriod.new(nil, end_t)

    assert_nil(p.start_transition)
    assert_same(end_t, p.end_transition)
    assert_same(dst, p.offset)
  end

  def test_initialize_start_and_end_nil
    error = assert_raises(ArgumentError) { TransitionsTimezonePeriod.new(nil, nil) }
    assert_match(/\bstart_transition\b/, error.message)
    assert_match(/\bend_transition\b/, error.message)
  end

  def test_equality
    o1 = TimezoneOffset.new(0, 3600, 'TEST')
    o2 = TimezoneOffset.new(0, 0, 'TEST')
    t1 = TimezoneTransition.new(o1, o2, 1149368400)
    t2 = TimezoneTransition.new(o1, o2, 1149454800)
    t3 = TimezoneTransition.new(o1, o2, 1149541200)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t1, t2)
    p3 = TransitionsTimezonePeriod.new(t2, nil)
    p4 = TransitionsTimezonePeriod.new(t2, nil)
    p5 = TransitionsTimezonePeriod.new(t3, nil)
    p6 = TransitionsTimezonePeriod.new(nil, t2)
    p7 = TransitionsTimezonePeriod.new(nil, t2)
    p8 = TransitionsTimezonePeriod.new(nil, t3)
    p9 = TimezonePeriod.new(o1)

    assert_equal(true, p1 == p1)
    assert_equal(true, p1 == p2)
    assert_equal(false, p1 == p3)
    assert_equal(false, p1 == p4)
    assert_equal(false, p1 == p5)
    assert_equal(false, p1 == p6)
    assert_equal(false, p1 == p7)
    assert_equal(false, p1 == p8)
    assert_equal(false, p1 == p9)
    assert_equal(false, p1 == Object.new)

    assert_equal(true, p3 == p3)
    assert_equal(true, p3 == p4)
    assert_equal(false, p3 == p5)
    assert_equal(false, p3 == p9)
    assert_equal(false, p3 == Object.new)

    assert_equal(true, p6 == p6)
    assert_equal(true, p6 == p7)
    assert_equal(false, p6 == p8)
    assert_equal(false, p6 == p9)
    assert_equal(false, p6 == Object.new)
  end

  def test_eql
    o1 = TimezoneOffset.new(0, 3600, 'TEST')
    o2 = TimezoneOffset.new(0, 0, 'TEST')
    t1 = TimezoneTransition.new(o1, o2, 1149368400)
    t2 = TimezoneTransition.new(o1, o2, 1149454800)
    t3 = TimezoneTransition.new(o1, o2, 1149541200)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t1, t2)
    p3 = TransitionsTimezonePeriod.new(t2, nil)
    p4 = TransitionsTimezonePeriod.new(t2, nil)
    p5 = TransitionsTimezonePeriod.new(t3, nil)
    p6 = TransitionsTimezonePeriod.new(nil, t2)
    p7 = TransitionsTimezonePeriod.new(nil, t2)
    p8 = TransitionsTimezonePeriod.new(nil, t3)
    p9 = TimezonePeriod.new(o1)

    assert_equal(true, p1.eql?(p1))
    assert_equal(true, p1.eql?(p2))
    assert_equal(false, p1.eql?(p3))
    assert_equal(false, p1.eql?(p4))
    assert_equal(false, p1.eql?(p5))
    assert_equal(false, p1.eql?(p6))
    assert_equal(false, p1.eql?(p7))
    assert_equal(false, p1.eql?(p8))
    assert_equal(false, p1.eql?(p9))
    assert_equal(false, p1.eql?(Object.new))

    assert_equal(true, p3.eql?(p3))
    assert_equal(true, p3.eql?(p4))
    assert_equal(false, p3.eql?(p5))
    assert_equal(false, p3.eql?(p9))
    assert_equal(false, p3.eql?(Object.new))

    assert_equal(true, p6.eql?(p6))
    assert_equal(true, p6.eql?(p7))
    assert_equal(false, p6.eql?(p8))
    assert_equal(false, p6.eql?(p9))
    assert_equal(false, p6.eql?(Object.new))
  end

  def test_hash
    o1 = TimezoneOffset.new(0, 3600, 'TEST')
    o2 = TimezoneOffset.new(0, 0, 'TEST')
    t1 = TimezoneTransition.new(o1, o2, 1149368400)
    t2 = TimezoneTransition.new(o1, o2, 1149454800)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t1, nil)
    p3 = TransitionsTimezonePeriod.new(nil, t2)

    assert_equal([t1, t2].hash, p1.hash)
    assert_equal([t1, nil].hash, p2.hash)
    assert_equal([nil, t2].hash, p3.hash)
  end

  def test_inspect
    o1 = TimezoneOffset.new(0, 3600, 'TESTD')
    o2 = TimezoneOffset.new(0, 0, 'TEST')
    t1 = TimezoneTransition.new(o1, o2, 1149368400)
    t2 = TimezoneTransition.new(o2, o1, 1149454800)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t1, nil)
    p3 = TransitionsTimezonePeriod.new(nil, t2)

    assert_equal("#<TZInfo::TransitionsTimezonePeriod: @start_transition=#{t1.inspect}, @end_transition=#{t2.inspect}>", p1.inspect)
    assert_equal("#<TZInfo::TransitionsTimezonePeriod: @start_transition=#{t1.inspect}, @end_transition=nil>", p2.inspect)
    assert_equal("#<TZInfo::TransitionsTimezonePeriod: @start_transition=nil, @end_transition=#{t2.inspect}>", p3.inspect)
  end
end
