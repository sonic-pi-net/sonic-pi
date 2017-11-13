module Mocha

  class ExceptionRaiser

    def initialize(exception, message)
      @exception, @message = exception, message
    end

    def evaluate
      raise @exception, @exception.to_s if @exception.is_a?(Module) && (@exception < Interrupt)
      raise @exception, @message if @message
      raise @exception
    end

  end

end
