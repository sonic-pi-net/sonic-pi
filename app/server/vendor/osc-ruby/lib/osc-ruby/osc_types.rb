require File.join( File.dirname( __FILE__ ), "osc_argument" )

module OSC
  class OSCInt32 < OSCArgument
    def tag() 'i' end
    def encode() [@val].pack('N').force_encoding("BINARY") end
  end

  class OSCFloat32 < OSCArgument
    def tag() 'f' end
    def encode() [@val].pack('g').force_encoding("BINARY") end 
  end

  class OSCString < OSCArgument
    def tag() 's' end
    def encode() padding(@val.sub(/\000.*\z/, '') + "\000").force_encoding("BINARY") end
  end

  class OSCBlob < OSCArgument
    def tag() 'b' end
    def encode() padding([@val.size].pack('N') + @val).force_encoding("BINARY") end
  end
end