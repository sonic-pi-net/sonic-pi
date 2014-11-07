module UniMIDI

  # Populate UniMIDI devices using the underlying device objects from the platform-specific gems
  module Loader

    extend self

    # Use the given platform-specific adapter to load devices
    # @param [UniMIDI::Adapter::Loader] loader
    def use(loader)
      @loader = loader
    end

    # @param [Hash] options
    # @option options [Symbol] :direction Return only a particular direction of device eg :input, :output
    # @return [Array<Input>, Array<Output>]
    def devices(options = {})
      if @devices.nil?
        inputs = @loader.inputs.map { |device| ::UniMIDI::Input.new(device) }
        outputs = @loader.outputs.map { |device| ::UniMIDI::Output.new(device) }
        @devices = {
          :input => inputs,
          :output => outputs
        }
      end
      options[:direction].nil? ? @devices.values.flatten : @devices[options[:direction]]
    end

  end
end
