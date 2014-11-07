require "coremidi"

module UniMIDI

  module Adapter

    # Load underlying devices using the coremidi gem
    module CoreMIDI

      module Loader

        extend self

        # @return [Array<CoreMIDI::Source>]
        def inputs
          ::CoreMIDI::Endpoint.all_by_type[:source]
        end

        # @return [Array<CoreMIDI::Destination>]
        def outputs
          ::CoreMIDI::Endpoint.all_by_type[:destination]
        end

      end

    end

  end

end
