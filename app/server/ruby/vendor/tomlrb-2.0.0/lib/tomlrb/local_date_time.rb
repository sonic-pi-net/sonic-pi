require 'forwardable'

module Tomlrb
  class LocalDateTime
    extend Forwardable

    def_delegators :@time, :year, :month, :day, :hour, :min, :sec, :usec, :nsec

    def initialize(year, month, day, hour, min, sec) # rubocop:disable Metrics/ParameterLists
      @time = Time.new(year, month, day, hour, min, sec, '-00:00')
      @sec = sec
    end

    # @param offset [String, Symbol, Numeric, nil] time zone offset.
    #   * when +String+, must be '+HH:MM' format, '-HH:MM' format, 'UTC', 'A'..'I' or 'K'..'Z'. Arguments excluding '+-HH:MM' are supporeted at Ruby >= 2.7.0
    #   * when +Symbol+, must be +:dst+(for summar time for local) or +:std+(for standard time).
    #   * when +Numeric+, it is time zone offset in second.
    #   * when +nil+, local time zone offset is used.
    # @return [Time]
    def to_time(offset='-00:00')
      return @time if offset == '-00:00'
      Time.new(year, month, day, hour, min, @sec, offset)
    end

    def to_s
      frac = (@sec - sec)
      frac_str = frac == 0 ? '' : "#{frac.to_s[1..-1]}"
      @time.strftime("%FT%T") << frac_str
    end

    def ==(other)
      other.respond_to?(:to_time) &&
        to_time == other.to_time
    end

    def inspect
      "#<#{self.class}: #{to_s}>"
    end
  end
end
