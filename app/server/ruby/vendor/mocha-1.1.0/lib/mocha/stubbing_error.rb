require 'mocha/backtrace_filter'

module Mocha

  # Exception raised when stubbing a particular method is not allowed.
  #
  # @see Configuration.prevent
  class StubbingError < StandardError

    # @private
    def initialize(message = nil, backtrace = [])
      super(message)
      filter = BacktraceFilter.new
      set_backtrace(filter.filtered(backtrace))
    end

  end

end
