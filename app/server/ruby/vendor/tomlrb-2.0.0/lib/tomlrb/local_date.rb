require 'forwardable'

module Tomlrb
  class LocalDate
    extend Forwardable

    def_delegators :@time, :year, :month, :day

    def initialize(year, month, day)
      @time = Time.new(year, month, day, 0, 0, 0, '-00:00')
    end

    # @param offset see {LocalDateTime#to_time}
    # @return [Time] 00:00:00 of the date
    def to_time(offset='-00:00')
      return @time if offset == '-00:00'
      Time.new(year, month, day, 0, 0, 0, offset)
    end

    def to_s
      @time.strftime('%F')
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
