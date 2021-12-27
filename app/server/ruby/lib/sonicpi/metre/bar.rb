module SonicPi

  # Class for tracking notes being added to a bar
  class Bar
    
    attr_reader :current_beat, :current_pulse_unit, :metre
    
    def initialize
      @metre = __thread_locals.get(:sonic_pi_metre)
      @current_beat = 0
      @current_pulse_unit = 0

      # Calculate the next bar number and request it from the current metre
      # If an old number is requested, metre.request_bar will return the current one
      previous_bar_number = __thread_locals.get(:sonic_pi_bar_number)
      if previous_bar_number
        current_bar_number = @metre.request_bar(previous_bar_number + 1)
      else
        current_bar_number = 0
      end
      # Let other and future Bar objects know what the current bar number is
      __thread_locals.set(:sonic_pi_bar_number, current_bar_number)
    end
    
    def sp_thread_safe?
      true
    end
    
    # Calculate how many pulse units have elapsed so far in this bar
    def total_elapsed_pulse_units
      pulse_units = 0
      (0...@current_beat).each do |i|
        pulse_units += @metre.beat_divisions[i]
      end
      pulse_units += @current_pulse_unit
      return pulse_units
    end
    
    def total_remaining_pulse_units
      @metre.total_pulse_units - total_elapsed_pulse_units
    end
    
    # Calculate how many pulse units remain in the current bar
    def beat_remaining_pulse_units
      @metre.beat_divisions[@current_beat] - @current_pulse_unit
    end

    # Calculate a note's duration in pulse units from its metrical level and its duration at that level
    def note_to_pulse_units(level, duration)
      @metre.note_to_pulse_units(@current_beat, level, duration)
    end
    
    # Determines whether a note with the given metrical level and duration will fit in the remainder of the bar
    def fit_note?(level, duration)
      total_remaining_pulse_units >= note_to_pulse_units(level, duration)
    end
    
    # Tries to add a note to the bar
    # Increments the current_beat and current_pulse_unit (within the current beat) accordingly
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
