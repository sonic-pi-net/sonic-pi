require "alsa-rawmidi"

module UniMIDI

  module Adapter

    # Load underlying devices using the alsa-rawmidi gem
    module AlsaRawMIDI

      module Loader

        extend self

        # @return [Array<AlsaRawMIDI::Input>]
        def inputs
          ::AlsaRawMIDI::Device.all_by_type[:input]
        end

        # @return [Array<AlsaRawMIDI::Output>]
        def outputs
          ::AlsaRawMIDI::Device.all_by_type[:output]
        end

      end

    end

  end

end
