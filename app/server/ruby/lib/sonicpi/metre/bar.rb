module SonicPi
  class Bar
    
    attr_reader :current_beat, :current_pulse_unit, :metre
    
    def initialize
      @metre = __thread_locals.get(:sonic_pi_metre)
      @current_beat = 0
      @current_pulse_unit = 0

      previous_bar_number = __thread_locals.get(:sonic_pi_bar_number)
      if previous_bar_number
        current_bar_number = @metre.request_bar(previous_bar_number + 1)
      else
        current_bar_number = 0
      end
      __thread_locals.set(:sonic_pi_bar_number, current_bar_number)
    end
    
    def sp_thread_safe?
      true
    end
    
    def total_elapsed_pulse_units
      pulse_units = 0
      (0...@current_beat).each do |i|
        pulse_units += @metre.beat_groupings[i]
      end
      pulse_units += @current_pulse_unit
      pulse_units
    end
    
    def total_remaining_pulse_units
      @metre.total_pulse_units - total_elapsed_pulse_units
    end
    
    def beat_remaining_pulse_units
      @metre.beat_groupings[@current_beat] - @current_pulse_unit
    end

    def note_to_pulse_units(level, duration)
      @metre.note_to_pulse_units(@current_beat, level, duration)
    end
    
    def fit_note?(level, duration)
      total_remaining_pulse_units >= note_to_pulse_units(level, duration)
    end
    
    def add_note(level, duration)
      raise "Cannot fit a note of this length into the bar" unless fit_note?(level, duration)
      pulse_units_to_add = note_to_pulse_units(level, duration)
      while pulse_units_to_add > 0 and pulse_units_to_add >= beat_remaining_pulse_units do
        pulse_units_to_add -= beat_remaining_pulse_units
        @current_beat += 1
        @current_pulse_unit = 0
      end
      @current_pulse_unit += pulse_units_to_add
    end
  end
end
