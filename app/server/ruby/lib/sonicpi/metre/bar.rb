module SonicPi

  # Class for tracking notes being added to a bar
  class Bar
    
    attr_reader :current_offset, :metre
    
    def initialize
      @metre = __thread_locals.get(:sonic_pi_metre)
      @current_offset = 0

      # Calculate the next bar number and request it from the current metre
      # If an old number is requested, metre.request_bar will return the current one
      previous_bar_number = __thread_locals.get(:sonic_pi_bar_number)
      if previous_bar_number
        current_bar_number = @metre.request_bar(previous_bar_number + 1)
      else
        current_bar_number = @metre.request_bar(0)
      end
      # Let other and future Bar objects know what the current bar number is
      __thread_locals.set(:sonic_pi_bar_number, current_bar_number)
    end
    
    def sp_thread_safe?
      true
    end

    def remaining_quarter_lengths
      return @metre.quarter_length - @current_offset
    end

    # Tries to add a note to the bar. If successful, returns the length of the note in quarter lengths
    # Increments the current_offset accordingly
    def add_note(level, duration)
      new_offset = @current_offset
      metre_at_level = @metre.get_level(level)
      duration.times do
        raise "Cannot fit a note of this length into the bar" if new_offset >= @metre.quarter_length
        new_offset += metre_at_level.offset_to_quarter_length(new_offset)
      end
      quarter_length_duration = new_offset - @current_offset
      @current_offset = new_offset
      return quarter_length_duration
    end

  end
end
