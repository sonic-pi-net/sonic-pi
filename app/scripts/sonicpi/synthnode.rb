require File.absolute_path("#{File.dirname(__FILE__)}/node")

module SonicPi
  class SynthNode < Node
    attr_reader :name
    def initialize(id, comms, name)
      super(id, comms)
      @name = name
    end
  end
end
