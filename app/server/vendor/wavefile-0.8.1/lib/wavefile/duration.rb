module WaveFile
  # Public: Calculates playback time given the number of sample frames and the sample rate.
  # For example, you can use this to calculate how long a given Wave file is.
  #
  # The hours, minutes, seconds, and milliseconds fields return values like you would
  # see on a stopwatch, and not the total amount of time in that unit. For example, a
  # stopwatch running for exactly 2 hours would show something like "2:00:00.000".
  # Accordingly, if the given sample frame count and sample rate add up to exactly
  # 2 hours, then hours will be 2, and minutes, seconds, and milliseconds will all be 0.
  #
  # This class is immutable - once a new Duration is constructed, it can't be modified.
  class Duration
    # Public: Constructs a new immutable Duration.
    #
    # sample_frame_count - The number of sample frames, i.e. the number
    #                      samples in each channel.
    # sample_rate - The number of samples per second, such as 44100
    #
    # Examples:
    #
    #   duration = Duration.new(400_000_000, 44100)
    #   duration.hours         # => 2
    #   duration.minutes       # => 31
    #   duration.seconds       # => 10
    #   duration.milliseconds  # => 294
    #
    # The hours, minutes, seconds, and milliseconds fields return values like you would
    # see on a stopwatch, and not the total amount of time in that unit. For example, a
    # stopwatch running for exactly 2 hours would show something like "2:00:00.000".
    # Accordingly, if the given sample frame count and sample rate add up to exactly
    # 2 hours, then hours will be 2, and minutes, seconds, and milliseconds will all be 0.
    def initialize(sample_frame_count, sample_rate)
      @sample_frame_count = sample_frame_count
      @sample_rate = sample_rate

      sample_frames_per_millisecond = sample_rate / 1000.0
      sample_frames_per_second = sample_rate
      sample_frames_per_minute = sample_frames_per_second * 60
      sample_frames_per_hour = sample_frames_per_minute * 60
      @hours, @minutes, @seconds, @milliseconds = 0, 0, 0, 0

      if(sample_frame_count >= sample_frames_per_hour)
        @hours = sample_frame_count / sample_frames_per_hour
        sample_frame_count -= sample_frames_per_hour * @hours
      end

      if(sample_frame_count >= sample_frames_per_minute)
        @minutes = sample_frame_count / sample_frames_per_minute
        sample_frame_count -= sample_frames_per_minute * @minutes
      end

      if(sample_frame_count >= sample_frames_per_second)
        @seconds = sample_frame_count / sample_frames_per_second
        sample_frame_count -= sample_frames_per_second * @seconds
      end

      @milliseconds = (sample_frame_count / sample_frames_per_millisecond).floor
    end

    # Public: Returns true if this Duration represents that same amount of time as
    # other_duration.
    #
    # Two Duration instances will evaluate as == if they correspond
    # to the same "stopwatch time". This means that two Durations constructed
    # from a different number of sample frames or different sample rates can be
    # considered equal if they correspond to the same amount
    # of time. For example, a Duration from 44,100 sample frames
    # at 44,100 samples/sec will be considered equal to a Duration
    # from 22,050 sample frames at 22,050 samples/sec, because
    # both correspond to 1 second of audio.
    #
    # Since the finest resolution of a duration is 1 millisecond,
    # two Durations that represent different amounts of time but
    # differ by less than 1 millisecond will be considered equal.
    def ==(other_duration)
      @hours == other_duration.hours &&
      @minutes == other_duration.minutes &&
      @seconds == other_duration.seconds &&
      @milliseconds == other_duration.milliseconds
    end
 
    # Public
    attr_reader :sample_frame_count

    # Public
    attr_reader :sample_rate

    # Public
    attr_reader :hours

    # Public
    attr_reader :minutes

    # Public
    attr_reader :seconds

    # Public
    attr_reader :milliseconds
  end
end
