require_relative "bar"
require_relative "distribution"

module SonicPi
  class Metre
    
    TIME_SIGNATURE_LOOKUP = {
      '2/4' => [2,2],
      '3/4' => [2,2,2],
      '4/4' => [2,2,2,2],
      '6/8' => [3,3],
      '9/8' => [3,3,3],
      '12/8' => [3,3,3,3]
    }.freeze
    
    attr_reader :beat_groupings, :total_beats, :total_pulse_units
    
    def initialize(metre)
      if is_list_like?(metre)
        @beat_groupings = metre
      else
        @beat_groupings = TIME_SIGNATURE_LOOKUP[metre]
      end
      @total_beats = @beat_groupings.length
      @total_pulse_units = @beat_groupings.sum
    end

    def note_to_pulse_units(current_beat, level, duration)
      if level == 0
        # Lookup number of pulse units in current beat
        @beat_groupings[current_beat]
      else
        # Assume pulse units are further divisible by 2
        (2 ** (level + 1)) * duration
      end
    end

    def to_beats(pulse_units)
      pulse_units.to_f / beat_groupings[0]
    end
  end


  class SynchronisedMetre < Metre
    attr_reader :style, :timings
    
    def initialize(metre, style=nil)
      super(metre)
      @style = style
      @timings = Array.new(@total_pulse_units, 0)
      @current_bar_number = __thread_locals.get(:sonic_pi_bar_number)
      @current_bar_number = 0 unless @current_bar_number
      @mutex = Mutex.new
    end

    def sp_thread_safe?
      true
    end

    def get_timing(pulse_unit)
      @timings[pulse_unit]
    end
    
    def request_bar(requested_bar_number)
      @mutex.synchronize do
        if requested_bar_number > @current_bar_number
          recalculate_timings
          @current_bar_number = requested_bar_number
        end
      end
      return @current_bar_number
    end
    
    private
    def recalculate_timings
      @timings[0] = @timings[0] + 1
    end
  end

end
