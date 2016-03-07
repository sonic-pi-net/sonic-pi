module WaveFile
  # Calculates playback time given the number of sample frames and the sample rate. For 
  # example, you can use this to calculate how long a given Wave file is. 
  #
  # The hours, minutes, seconds, and milliseconds fields return values like you would 
  # see on a stopwatch, and not the total amount of time in that unit. For example, a 
  # stopwatch running for exactly 2 hours would show something like "2:00:00.000". 
  # Accordingly, if the given sample frame count and sample rate add up to exactly 
  # 2 hours, then hours will be 2, and minutes, seconds, and milliseconds will all be 0.
  #
  # This class is immutable - once a new Duration is constructed, it can't be modified.
  class Duration
    # Constructs a new immutable Duration.
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
    # Note that the hours, minutes, seconds, and milliseconds fields do not return 
    # the total of the respective unit in the entire duration. For example, if a 
    # duration is exactly 2 hours, then minutes will be 0, not 120.
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
 
    attr_reader :sample_frame_count
    attr_reader :sample_rate 
    attr_reader :hours
    attr_reader :minutes
    attr_reader :seconds
    attr_reader :milliseconds
  end
end
