require_relative "aubio/version"
require_relative "aubio/api"
require_relative "aubio/onsets"
require_relative "aubio/pitches"
require_relative "aubio/beats"

module Aubio
  class AubioException < Exception; end
  class FileNotFound < AubioException; end
  class AlreadyClosed < AubioException; end
  class InvalidAudioInput < AubioException; end

  class Base
    def initialize(path, params)
      raise FileNotFound unless File.file?(path)

			sample_rate = params[:sample_rate] || 44100
			hop_size    = params[:hop_size]    || 512

			@source = Api.new_aubio_source(path, sample_rate, hop_size)
      @params = params

      check_for_valid_audio_source(path)
    end

    def close
      Api.del_aubio_source(@source)
      @is_closed = true
    end

    def onsets
      check_for_closed

      Onsets.new(@source, @params).each
    end

    def pitches
      check_for_closed

      Pitches.new(@source, @params).each
    end

    def beats
      check_for_closed

      Beats.new(@source, @params).each
    end

    def bpm
      check_for_closed

      beat_locations = Beats.new(@source, @params).each.to_a
      beat_periods = beat_locations.each_cons(2).map {|a,b| b[:s] - a[:s] }

      return 60.0 if beat_locations.length == 1

      # use interquartile median to discourage outliers
      s = beat_periods.length
      qrt_lower_idx = (s/4.0).floor
      qrt_upper_idx = qrt_lower_idx * 3
      interquartile_beat_periods = beat_periods[qrt_lower_idx..qrt_upper_idx]

      # Calculate median
      iqs = interquartile_beat_periods.length

      iq_median_beat_period = interquartile_beat_periods.sort[(iqs/2.0).floor() - 1]
      60.0 / iq_median_beat_period
    end

    private
    def check_for_closed
      raise AlreadyClosed if @is_closed
    end

    def check_for_valid_audio_source(path)
      begin
        @source.read_pointer
      rescue FFI::NullPointerError
        raise InvalidAudioInput.new(%Q{

            Couldn't read file at #{path}
            Did you install aubio with libsndfile support?
          })
      end
    end
  end
end

module Aubio
  def self.open(path, params = {})
    Base.new(path, params)
  end
end
