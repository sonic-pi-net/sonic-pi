require_relative "aubio/version"
require_relative "aubio/aubio-ffi"
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

      @is_closed = false
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

    def enum_beats
      check_for_closed

      Beats.new(@source, @params).each
    end

    def beats
      check_for_closed

      beats = Beats.new(@source, @params).each.to_a

      # fill in the zero beat
      beats = beats.unshift(
        beats.first.merge({
          confidence: 1,
          s: 0.0,
          ms: 0.0,
          sample_no: 0,
          rel_start: 0.0
        })
      )

      # fetch the rel_end from the next beat
      # using 1.0 for the last beat
      beats = beats.each_cons(2).map {|a,b|
        a.merge({
          rel_end: (b[:rel_start] || 1.0)
        })
      }

      # set minimum inter-onset interval in seconds
      # allows for 4/4 at 400bpm (faster than most music)
      # filters beats detected too closely together
      minioi = @params[:minioi] || 0.15
      filtered_beats = [beats.first]
      beats.each do |b|
        if (b[:s] - filtered_beats.last[:s]) > minioi
          filtered_beats << b
        end
      end

      # TODO: are there other smoothing methods that would be useful here?
      filtered_beats
    end

    def bpm
      check_for_closed

      beat_locations = self.beats
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
