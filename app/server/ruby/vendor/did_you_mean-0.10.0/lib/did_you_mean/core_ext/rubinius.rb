if defined?(Rubinius)
  class << Rubinius
    alias raise_with_no_receiver_capturer raise_exception

    def raise_exception(exc)
      if exc.is_a?(NoMethodError)
        bt = Rubinius::VM.backtrace(0, true).detect do |x|
          x.method.name == :method_missing
        end
        exc.instance_variable_set(:@receiver, bt.variables.self) if bt
      end

      raise_with_no_receiver_capturer(exc)
    end
  end
end
