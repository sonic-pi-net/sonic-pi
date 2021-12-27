require_relative "bar"
require_relative "style"

module SonicPi

  # Class for describing a particular metre
  class Metre
    
    TIME_SIGNATURE_LOOKUP = {
      '2/4' => [2,2],
      '3/4' => [2,2,2],
      '4/4' => [2,2,2,2],
      '6/8' => [3,3],
      '9/8' => [3,3,3],
      '12/8' => [3,3,3,3]
    }.freeze
    
    attr_reader :beat_divisions, :total_beats, :total_pulse_units
    
    # Metre can be a list of integers representing the number of pulse units each beat is divided into, or a time signature string (e.g. '4/4')
    def initialize(metre)
      if is_list_like?(metre)
        @beat_divisions = metre
      else
        @beat_divisions = TIME_SIGNATURE_LOOKUP[metre]
      end
      @total_beats = @beat_divisions.length
      @total_pulse_units = @beat_divisions.sum
    end

    # Calculates the duration of a note in pulse units
    def note_to_pulse_units(current_beat, level, duration)
      if level == 0
        # Lookup number of pulse units in current beat
        @beat_divisions[current_beat] * duration
      else
        # Assume pulse units are further divisible by 2
        (2 ** (level + 1)) * duration
      end
    end

    # Get the index of the current note at each metrical level (up to the lowest specified in the style)
    # current_beat is the integer index of the current beat
    # total_elapsed_pulse_units is a float describing how many pulse units have elapsed since the start of the bar
    def metrical_level_indices(current_beat, total_elapsed_pulse_units, lowest_metrical_level)
      indices = []
      indices[0] = current_beat
    
      level = -1
      position = total_elapsed_pulse_units
      indices[-level] = position.floor
    
      (2..-lowest_metrical_level).each do
        level -= 1
        position *= 2
        indices[-level] = position.floor
      end
      return indices
    end

    # Calculates the number of Sonic Pi beats to sleep for for a note/rest of duration pulse_units
    def sleep_time(pulse_units)
      pulse_units.to_f / beat_divisions[0]
    end
  end


  # Class for controlling the synchronisation of all notes contained by this metre
  class SynchronisedMetre < Metre
    attr_reader :style, :timings
    
    # Metre can be a list of integers representing the number of pulse units each beat is divided into, or a time signature string (e.g. '4/4')
    # Style can be the symbol of a style preset to be looked up (see Style class), or a Style object
    # The chosen/given style must have the same beat divisions as the metre
    def initialize(metre, style=nil)
      super(metre)

      if style
        if style.is_a?(Style)
          @style = style
        else
          @style = Style.lookup(style)
        end
        raise "Style #{@style.name} requires beat divisions #{@style.beat_divisions} but metre has #{@beat_divisions}" unless @beat_divisions == @style.beat_divisions
        @timings = {}
        recalculate_timings
      end

      # Initialise current_bar_number with the thread's most recent one so bar numbers continue to increment if a different metre came before
      @current_bar_number = __thread_locals.get(:sonic_pi_bar_number)
      @current_bar_number = 0 unless @current_bar_number
      @mutex = Mutex.new
    end

    def sp_thread_safe?
      true
    end

    # Calculates the timing shift (in pulse units) to be applied to a note occurring total_elapsed_pulse_units into the cycle
    # Includes contributions of all metrical levels down to the lowest_level specified by the Style
    def get_timing(current_beat, total_elapsed_pulse_units)
      return 0 unless @style
      lowest_level = @style.lowest_metrical_level
      indices = metrical_level_indices(current_beat, total_elapsed_pulse_units, lowest_level)
      timing_shift = 0
      (lowest_level..0).each do |level|
        timing_shift += @timings[level][indices[-level]] if @timings[level]
      end
      return timing_shift
    end
    
    # Request the next bar in sequence
    # If this is the first time this bar number has been requested, the currently active timings (sampled from the style's distributions) are updated
    # Synchronised on a mutex to avoid race conditions so all bars will have the same random micro-timing
    def request_bar(requested_bar_number)
      @mutex.synchronize do
        if requested_bar_number > @current_bar_number
          recalculate_timings if @style
          @current_bar_number = requested_bar_number
        end
      end
      return @current_bar_number
    end
    
    # Samples values for timing offsets for each note at each metrical level from the style's distributions
    private
    def recalculate_timings
      @timings = @style.sample_distributions
    end
  end

end
