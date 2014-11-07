module UniMIDI

  # Module for command-line use of UniMIDI.  Used by the bin/unimidi script
  module Command

    extend self

    # Execute a command
    # @param [Symbol] command
    # @param [Hash] options
    # @return [Boolean]
    def exec(command, options = {})
      if [:l, :list, :list_devices].include?(command)
        puts "input:"
        Input.list
        puts "output:"
        Output.list
        true
      else
        raise "Command #{command.to_s} not found"
      end   
    end

  end

end
