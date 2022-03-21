require_relative "bar"
require_relative "style"

module SonicPi

  # The leaf of the metrical tree structure
  class MetreTerminal

    attr_reader :fraction, :quarter_length

    # <fraction> is a Rational representing the duration of the object
    def initialize(fraction)
      @fraction = fraction
      @quarter_length = 4 * @fraction.to_f
    end

    def depth
      return 0
    end

    def to_s
      return @fraction.to_s
    end

    def ==(other)
      @fraction == other.fraction
    end

    # Divides the MetreTerminal by two <subdivisions> times
    # Returns a MetreSequence with 2**subdivisions MetreTerminals
    def subdivide(subdivisions)
      new_sequence = []
      count = 2 ** subdivisions
      new_frac = @fraction / count
      count.times do
        new_sequence.append(MetreTerminal.new(new_frac))
      end
      return MetreSequence.new(new_sequence)
    end

    def sp_thread_safe?
      true
    end
  end

  # A tree structure representing a metrical hierarchy
  class MetreSequence
    
    attr_reader :sequence, :length, :depth

    # Sequence is a tree of metrical levels represented as a list of MetreSequences or MetreTerminals
    def initialize(sequence)
      @sequence = sequence.freeze
      @length = self._length
      @depth = self._depth
      @level_cache = {}
      @offset_indices_cache = {}
    end

    # Calculates the total duration of the metre as a single fraction
    # Uses a cached value if it exists to reduce expensive .flat computations
    def fraction
      @fraction_cache = self._fraction unless @fraction_cache
      return @fraction_cache
    end

    # Calculates the total duration of the metre in quarter lengths
    # Uses a cached value if it exists to reduce expensive .flat computations
    def quarter_length
      @quarter_length_cache = self._quarter_length unless @quarter_length_cache
      return @quarter_length_cache
    end
    
    # Returns a string representation of the metrical hierarchy
    def to_s
      return '{' + @sequence.join('+') + '}'
    end
  
    # Returns a new MetreSequence with a flat representation of the hierarchy
    def flat
      new_sequence = []
      for m in @sequence
        if m.is_a?(MetreTerminal)
          new_sequence.append(m)
        else
          new_sequence += m.flat.sequence
        end
      end
      return MetreSequence.new(new_sequence)
    end

    # Returns a flat MetreSequence at a given metrical level
    # Uses a cached value if it exists to reduce expensive recomputations
    def get_level(level)
      @level_cache[level] = self._get_level(level) unless @level_cache[level]
      return @level_cache[level]
    end

    # Performs a very basic partition on the sequence
    # Returns a new MetreSequence with the same duration/fraction but split into <count> equally-sized MetreTerminals
    # Can only perform uniform partitioning
    def partition(count)
      fraction = self.fraction
      if count == 1
        return MetreSequence.new([fraction])
      end
      new_denom = fraction.denominator * (count.to_f / fraction.numerator)
      new_frac = Rational(1, new_denom)
      if new_denom % 1 == 0
        # Can partition into <count> elements
        new_sequence = []
        count.times do
          new_sequence.append(MetreTerminal.new(new_frac))
        end
        return MetreSequence.new(new_sequence)
      else
        # Can't make partition
        return nil
      end
    end
    
    # Equality for MetreSequences is defined by its @sequence list
    def ==(other)
      return @sequence == other.sequence
    end

    # Returns the index of the active element in @sequence at an <offset> given in quarter lengths
    # Uses a cached value if it exists to reduce expensive recomputations
    def offset_to_index(offset)
      @offset_indices_cache[offset] = self._offset_to_index(offset) unless @offset_indices_cache[offset]
      return @offset_indices_cache[offset]
    end

    # Returns the duration (in quarter lengths) of the active element in @sequence at a given <offset>
    def offset_to_quarter_length(offset)
      @sequence[self.offset_to_index(offset)].quarter_length
    end

    def sp_thread_safe?
      true
    end

    # Returns a Hash of metrical levels to the index of any event at that position which lies exactly on <offset>
    # Gets all metrical events at <offset> up to a depth of <deepest_level>
    def metrical_level_indices(offset, deepest_level)
      raise "Offset #{offset} out of bounds for duration #{self.quarter_length}" if offset >= self.quarter_length or offset < 0
      indices = {}
      # For each metrical level used in the style
      (0..deepest_level).each do |level|
        seq = self.get_level(level)
        position = 0
        # Search through sequence for a MetreTerminal at exactly <offset>
        seq.length.times do |i|
          if position == offset
            indices[level] = i
            break
          else
            position += seq.sequence[i].quarter_length
          end
        end
      end
      return indices
    end

    private

    # Length of a MetreSequence is the number of elements in its list
    def _length
      return @sequence.length
    end

    # Depth of a MetreSequence is the number of metrical levels defined in its hierarchy
    # Recursively calls .depth on each MetreSequence or MetreTerminal in the list
    def _depth
      return @sequence.map{ |m| m.depth}.max + 1
    end

    # Calculates the total duration of the metre as a single fraction
    def _fraction
      return self.flat.sequence.map{ |terminal| terminal.fraction }.sum
    end

    # Calculates the total duration of the metre in quarter lengths
    def _quarter_length
      return self.flat.sequence.map{ |terminal| terminal.quarter_length }.sum
    end

    # Returns a flat MetreSequence at a given metrical level
    def _get_level(level)
      new_sequence = []
      for m in @sequence
        if m.is_a?(MetreTerminal)
          if level > 0
            new_sequence += m.subdivide(level).sequence
          else
            new_sequence.append(m)
          end
        else
          if level > 0
            # Continue recursing
            new_sequence += m.get_level(level - 1).sequence
          else
            # Base of recursion, combine all children of m into one MetreTerminal
            new_sequence.append(MetreTerminal.new(m.fraction))
          end
        end
      end
      return MetreSequence.new(new_sequence)
    end

    # Returns the index of the active element in @sequence at an <offset> given in quarter lengths
    # Raises exception if the offset is outside the range of the MetreSequence
    def _offset_to_index(offset)
      raise "Offset #{offset} out of bounds for duration #{self.quarter_length}" if offset >= self.quarter_length or offset < 0
      position = 0
      @sequence.length.times do |i|
        if position <= offset and offset < (position + @sequence[i].quarter_length)
          return i
        else
          position += @sequence[i].quarter_length
        end
      end
    end
  end




  # Class for describing a particular metre
  class Metre < MetreSequence

    TIME_SIGNATURE_LOOKUP = {
      '4/4' => [MetreTerminal.new(1/4r)] * 2,
      '3/4' => [MetreTerminal.new(1/4r)] * 3,
      '4/4' => [MetreTerminal.new(1/4r)] * 4,
      '6/8' => [MetreSequence.new([MetreTerminal.new(1/8r)] * 3)] * 2,
      '9/8' => [MetreSequence.new([MetreTerminal.new(1/8r)] * 3)] * 3,
      '12/8' => [MetreSequence.new([MetreTerminal.new(1/8r)] * 3)] * 4
    }.freeze

    attr_reader :beat_duration

    # Sequence can either be a time signature string (e.g. '4/4'),
    # a multi-dimensional list of Rationals representing the metrical hierarchy (e.g. [[1/8r,1/8r,1/8r], [1/8r,1/8r,1/8r]] is 6/8),
    # or a MetreSequence or MetreTerminal object
    def initialize(sequence)
      if sequence.is_a?(MetreSequence)
        super(sequence.sequence)
      elsif sequence.is_a?(MetreTerminal)
        super([sequence])
      elsif is_list_like?(sequence)
        super(Metre.parse_rational_list(sequence).sequence)
      else
        super(TIME_SIGNATURE_LOOKUP[sequence])
      end
      # Beat duration is set to the duration of the first beat of the metre. Used for tempo purposes
      @beat_duration = self.get_level(0).sequence[0].quarter_length
    end

    # Converts a duration in quarter lengths to be in Sonic Pi beats
    # For simple time, @beat_duration = 1
    def quarter_length_to_sonic_pi_beat(duration)
      return duration / @beat_duration
    end

    def self.parse_rational_list(list)
      new_sequence = []
      for element in list
        if is_list_like?(element)
          new_sequence.append(Metre.parse_rational_list(element))
        else
          new_sequence.append(MetreTerminal.new(element))
        end
      end
      return MetreSequence.new(new_sequence)
    end
  end


  # Class for controlling the synchronisation of all notes contained by this metre
  class SynchronisedMetre < Metre
    attr_reader :style, :timings
    
    # Metre can either be a time signature string (e.g. '4/4'), or a list of MetreSequences and MetreTerminals
    # Style can be the symbol of a style preset to be looked up (see Style.lookup()), or a Style object
    # The chosen/given style must be compatible with the metre (see Style.compatible_with?())
    def initialize(metre, style=nil)
      super(metre)

      if style
        if style.is_a?(Style)
          @style = style
        else
          @style = Style.lookup(style)
        end
        raise "Style #{@style.name} incompatible with metre #{self.to_s}" unless @style.compatible_with?(self)
        @timings = {}
        recalculate_timings
      end

      # Initialise <current_bar_number> with the thread's most recent one so bar numbers continue to increment if a different metre came before
      @current_bar_number = __thread_locals.get(:sonic_pi_bar_number)
      @current_bar_number = 0 unless @current_bar_number
      @mutex = Mutex.new
    end

    # Calculates the timing shift (in quarter lengths) to be applied to a note occurring <current_offset> into the cycle
    # Includes contributions of all metrical levels down to the <deepest_level> specified by the Style
    def get_timing(current_offset)
      return 0 unless @style
      deepest_level = @style.deepest_metrical_level
      indices = self.metrical_level_indices(current_offset, deepest_level)
      timing_shift = 0
      (0..deepest_level).each do |level|
        timing_shift += @timings[level][indices[level]] if @timings[level] and indices[level]
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
