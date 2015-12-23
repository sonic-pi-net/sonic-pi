require 'test/unit'
require 'wavefile.rb'

include WaveFile

class DurationTest < Test::Unit::TestCase
  SECONDS_IN_MINUTE = 60
  SECONDS_IN_HOUR = SECONDS_IN_MINUTE * 60

  def test_constructor
    # Test common sample rates (22050 and 44100), and some crazy arbitrary sample rate (12346)
    [22050, 44100, 12346].each do |sample_rate|
      duration = Duration.new(0, sample_rate)
      assert_equal(0, duration.hours)
      assert_equal(0, duration.minutes)
      assert_equal(0, duration.seconds)
      assert_equal(0, duration.milliseconds)
      assert_equal(0, duration.sample_frame_count)
      assert_equal(sample_rate, duration.sample_rate)

      duration = Duration.new(sample_rate / 2, sample_rate)
      assert_equal(0, duration.hours)
      assert_equal(0, duration.minutes)
      assert_equal(0, duration.seconds)
      assert_equal(500, duration.milliseconds)
      assert_equal(sample_rate / 2, duration.sample_frame_count)
      assert_equal(sample_rate, duration.sample_rate)

      duration = Duration.new(sample_rate, sample_rate)
      assert_equal(0, duration.hours)
      assert_equal(0, duration.minutes)
      assert_equal(1, duration.seconds)
      assert_equal(0, duration.milliseconds)
      assert_equal(sample_rate, duration.sample_frame_count)
      assert_equal(sample_rate, duration.sample_rate)

      duration = Duration.new(sample_rate * SECONDS_IN_MINUTE, sample_rate)
      assert_equal(0, duration.hours)
      assert_equal(1, duration.minutes)
      assert_equal(0, duration.seconds)
      assert_equal(0, duration.milliseconds)
      assert_equal(sample_rate * SECONDS_IN_MINUTE, duration.sample_frame_count)
      assert_equal(sample_rate, duration.sample_rate)

      duration = Duration.new(sample_rate * SECONDS_IN_HOUR, sample_rate)
      assert_equal(1, duration.hours)
      assert_equal(0, duration.minutes)
      assert_equal(0, duration.seconds)
      assert_equal(0, duration.milliseconds)
      assert_equal(sample_rate * SECONDS_IN_HOUR, duration.sample_frame_count)
      assert_equal(sample_rate, duration.sample_rate)

      sample_frame_count = (sample_rate * SECONDS_IN_MINUTE) + sample_rate + (sample_rate / 2)
      duration = Duration.new(sample_frame_count, sample_rate)
      assert_equal(0, duration.hours)
      assert_equal(1, duration.minutes)
      assert_equal(1, duration.seconds)
      assert_equal(500, duration.milliseconds)
      assert_equal(sample_frame_count, duration.sample_frame_count)
      assert_equal(sample_rate, duration.sample_rate)
    end

    # Test for when the number of hours is more than a day.
    samples_per_hour = 44100 * 60 * 60
    duration = Duration.new(samples_per_hour * 25, 44100)
    assert_equal(25, duration.hours)
    assert_equal(0, duration.minutes)
    assert_equal(0, duration.seconds)
    assert_equal(0, duration.milliseconds)
    assert_equal(samples_per_hour * 25, duration.sample_frame_count)
    assert_equal(44100, duration.sample_rate)
  end
end
